##
##
##30min涡度和气象数据自动处理程序
##基于REddyProc包
##main.r数据整理为标准的输入数据
##黑河气象数据为10min-涡度数据为30min

##提取需要的数据
dataprocessing <- function(
inpath_awc, ##输入awc文件的路径
inpath_ec, ##输入ec文件的路径
outpath, #存储文件的路径
year, ##年份
LAT, ##纬度
LON, ##经度
timezone, ##时区
storage, ##是否计算存储量
height, ##冠层高度
Fcunit, ##如果单位是mg那么*22.73如果是umol直接用
window_size ##窗口长度
){
library(REddyProc)
library(dplyr)
library(zoo)
library(stringr)
##开始处理
##
##awc和ec数据
awc <- inputs_awc(inpath_awc,2013)
ec <- inputs_ec(inpath_ec,2013)
ec$NEE <- Fstorage(ec$NEE,storage,height)
#合并开始处理
dat <- data.frame(awc,ec[,2:8])
##FC值需要*22.73
if(Fcunit=='mg'){
	dat$NEE <- dat$NEE*22.73
}else{
	dat$NEE <- dat$NEE
}
##Fc改名为NEE
#colnames(dat)[11] <- 'NEE'
dat$TIMESTAMP=as.POSIXct(dat$TIMESTAMP,tz='UTC')
variable.names(dat)
dat_origin <- dat
##raw_eddy_data.complTime.MADProc
##数据的异常值检测和剔除 (MAD 算法)
Sample_length=nrow(dat) ;window_size=window_size ;z=4
##进行昼夜时间划分,给样本量赋名，以及设定窗口和z值
Day_Night=numeric(Sample_length);Day_Night[which(Day_Night==0)]=NA
Day_Night[dat$Rg>20]<-1
Day_Night[dat$Rg<=20]<-0
##设定昼夜标记
window_ID=numeric(Sample_length)
for (ii in 1:Sample_length) {
window_ID[ii]=ceiling(ii/window_size)}
###设定每个样本所在的窗口位置
##计算"di"####
di=numeric(Sample_length)
di[1]=NA ;di[Sample_length]=NA
for (iii in 2:(Sample_length-1)) {
	di[iii]=(dat$NEE[iii]-dat$NEE[iii-1])-(dat$NEE[iii+1]-dat$NEE[iii])
}
###将Day_Night、window_ID、di合并到数据集
dat=data.frame(dat,Day_Night, window_ID, di)
dat=arrange(dat, TIMESTAMP)
rm(Day_Night,window_ID,di);variable.names(dat)
##计算 Md (即分别计算每个窗口中，以及窗口中白天和夜间，di的中位数)####
##分组整合最后合并
Md=aggregate(dat$di,by=list(window_ID=dat$window_ID,Day_Night=dat$Day_Night),FUN=median,na.rm=TRUE)
names(Md)[3]='Md'
dat=merge(dat, Md, by=c('window_ID','Day_Night'), all.x =TRUE)
variable.names(dat)
##计算|di-Md|####
dat=dat%>%mutate(di.Md=abs(di-Md))%>%arrange(TIMESTAMP)
##计算 MAD ####
##整合+合并
MAD=aggregate(dat$di.Md,by=list(window_ID=dat$window_ID,Day_Night=dat$Day_Night),FUN=median,na.rm=TRUE)
names(MAD)[3]='MAD'
dat=merge(dat, MAD, by=c('window_ID','Day_Night'), all.x =TRUE)
variable.names(dat)
##计算每个窗口内的样本阈值 ####
dat=dat%>%mutate(Ceiling=Md+((z*MAD)/0.6745),Floor=Md-((z*MAD)/0.6745))%>%arrange(TIMESTAMP)
variable.names(dat)
##根据阈值筛选数据整合到原始完整时间序列的数据 ####
dat=dat%>%subset(di>Floor & di<Ceiling,select=c(TIMESTAMP,NEE))%>%arrange(TIMESTAMP) 
names(dat)[which(names(dat)=='NEE')]='NEE_MAD'
dat.FinishMAD=merge(dat_origin,dat,by=c('TIMESTAMP'),all=TRUE)
dat.FinishMAD=arrange(dat.FinishMAD,TIMESTAMP)
rm(MAD,Md)
variable.names(dat.FinishMAD)
##选择阈值范围内的数据，合并到原始数据
###查看异常值剔除后的效果
#plot(dat.FinishMAD$NEE_MAD,col=c("gray55"))
variable.names(dat.FinishMAD)
##计算VPD
dat.FinishMAD$VPD <- 6.1078*exp((17.27*dat.FinishMAD$Tair)/(dat.FinishMAD$Tair+237.3))*(1-(dat.FinishMAD$rH/100))
##初始化REddyProc程序包，输入相关通量站点信息
dat.FinishMAD.REddyProc=dat.FinishMAD%>%
subset(select=c(TIMESTAMP,Rg,Ustar,Tair,Tsoil,rH,VPD,NEE_MAD,Hs,LE))%>%
  #mutate(VPD=6.1078*exp((17.27*Tair)/(Tair+237.3))*(1-(rH/100)))%>%
dplyr::rename(DateTime='TIMESTAMP',NEE='NEE_MAD')%>%arrange(DateTime)
str(dat.FinishMAD.REddyProc)
##调整REddyProc程序包识别的数据格式
##删除第一列
dat.FinishMAD.REddyProc <- dat.FinishMAD.REddyProc[-1,]
dat_origin <- dat_origin[-1,]
##把年月日加上

EddyProc.C <- sEddyProc$new('Heihe River Basin',
                          dat.FinishMAD.REddyProc,
                          ColPOSIXTime = "DateTime",
                          LatDeg=LAT,LongDeg=LON,TimeZoneHour=timezone,
                          names(dat.FinishMAD.REddyProc)[-1])
t(data.frame(EddyProc.C$sID,EddyProc.C$sINFO,EddyProc.C$sLOCATION))
####计算Ustar阈值
uStarTh <- EddyProc.C$sEstimateUstarScenarios(nSample = 100L, probs = c(0.05, 0.5, 0.95))
EddyProc.C$sGetEstimatedUstarThresholdDistribution()
str(uStarTh)
signif(unlist(uStarTh$sUSTAR[1,5:7]),2)
##阈值外赋值为NA
EddyProc.C$sDATA$NEE_low <- EddyProc.C$sDATA$NEE_median <- EddyProc.C$sDATA$NEE_orig <- 
EddyProc.C$sDATA$NEE_upper  <- EddyProc.C$sDATA$NEE
EddyProc.C$sDATA$NEE_orig[EddyProc.C$sDATA$Ustar < unlist(uStarTh$sUSTAR[1,4])] <- NA
EddyProc.C$sDATA$NEE_low[EddyProc.C$sDATA$Ustar < unlist(uStarTh$sUSTAR[1,5])] <- NA
EddyProc.C$sDATA$NEE_median[EddyProc.C$sDATA$Ustar < unlist(uStarTh$sUSTAR[1,6])] <- NA
EddyProc.C$sDATA$NEE_upper[EddyProc.C$sDATA$Ustar < unlist(uStarTh$sUSTAR[1,7])] <- NA
EddyProc.C$sDATA$NEE_median <- EddyProc.C$sDATA$NEE_orig <- 
EddyProc.C$sDATA$NEE_low <- EddyProc.C$sDATA$NEE_upper <- NULL
EddyProc.C$sGetUstarScenarios()
##插补数据
EddyProc.C$sMDSGapFillUStarScens('H',FillAll = TRUE,minNWarnRunLength = NA)
EddyProc.C$sMDSGapFillUStarScens('LE',FillAll = TRUE,minNWarnRunLength = NA)
EddyProc.C$sMDSGapFillUStarScens('NEE',FillAll = TRUE,minNWarnRunLength = NA)
EddyProc.C$sMDSGapFillUStarScens('Tair',FillAll = TRUE,minNWarnRunLength = NA)
EddyProc.C$sMDSGapFillUStarScens('Tsoil',FillAll = TRUE,minNWarnRunLength = NA)
EddyProc.C$sMDSGapFillUStarScens('rH',FillAll = TRUE,minNWarnRunLength = NA)
EddyProc.C$sMDSGapFillUStarScens('VPD',FillAll = TRUE,minNWarnRunLength = NA)
EddyProc.C$sMDSGapFillUStarScens('Rg',FillAll = TRUE,minNWarnRunLength = NA)
##对于每个不同的 $u_*$ 阈值估计值 填充 NEE 的一组单独的输出列及其 产生不确定性
grep("NEE_.*_f$",names(EddyProc.C$sExportResults()), value = TRUE)
grep("NEE_.*_fsd$",names(EddyProc.C$sExportResults()), value = TRUE)
##合并数据
dat_all <- cbind(dat_origin, EddyProc.C$sExportResults())
dat_sub <- EddyProc.C$sTEMP
##剔除插补
dat_sub$NEE_uStar_f[which(dat_sub$NEE_uStar_f<(-50))]<-NA
dat_sub$NEE_uStar_f[which(dat_sub$NEE_uStar_f>50)]<-NA
dat_sub$NEE_uStar_f <- na.approx(dat_sub$NEE_uStar_f)
# for(j in 1:length(dat_sub)){
# if(length(grep("NEE",names(dat_sub)[j],value = TRUE))=="0"){
# colnames(dat_sub)[j] <- gsub("_uStar","",colnames(dat_sub)[j])
# }}
colnames(dat_sub) <- gsub("_uStar","",colnames(dat_sub))
EddyProc.C$sTEMP <- dat_sub
EddyProc.C$sMRFluxPartitionUStarScens(EddyProc.C$sMRFluxPartition())
EddyProc.C$sGLFluxPartitionUStarScens(EddyProc.C$sGLFluxPartition())
grep("GPP.*_f$|Reco",names(EddyProc.C$sExportResults()), value = TRUE)
##不确定性分析
##不同u*阈值情景的结果可用于估计由于不知道阈值而导致的不确定性
##首先，计算每个\（u_*\）情景的GPP平均值
##将umol转为gC
FilledEddyData <- EddyProc.C$sExportResults()
uStarSuffixes <- colnames(EddyProc.C$sGetUstarScenarios())[-1]
GPPAggCO2 <- sapply( uStarSuffixes, function(suffix) {
    GPPHalfHour <- FilledEddyData[[paste0("GPP_",suffix,"_f")]]
    mean(GPPHalfHour, na.rm = TRUE)
})
molarMass <- 12.011
GPPAgg <- GPPAggCO2 * 1e-6 * molarMass * 3600*24*365.25
##由于u*阈值的不确定性而导致的GPP不确定性范围的估计
print(GPPAgg)
##推断出相对误差的大小
error <- (max(GPPAgg) - min(GPPAgg)) / median(GPPAgg) 
EddyProc.C$sEstimateUstarScenarios( 
  nSample = 200, probs = seq(0.025,0.975,length.out = 39))
FilledEddyData <- EddyProc.C$sExportResults()
CombinedData <- cbind(dat_origin, FilledEddyData)
year(CombinedData$TIMESTAMP)<-year[i]
write.csv(CombinedData,outpath,row.names=F)
}	
	
	