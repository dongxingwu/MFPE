##
##
##上师大通量气象数据处理

##方程1
library(Rssa)
library(dplyr)
outpath <- 'C:/users/win/desktop/out.csv'
path <- 'C:/users/win/desktop/eddypro_10_full_output_2021-10-18T095722_adv.csv'
Y <- 'LE'
funstep6_sp <- function(x,y){
	x <- as.numeric(x)
	if(y %in% x){
	x[which(x==y)]<-NA}else{x<-x}
	return(x)
}
##方程2
funstep6_sn <- function(x,y){
	x <- as.numeric(x)
	y <- as.numeric(y)
	z <- array()
	for(i in 1:length(x)){
		if(is.na(x[i])){z[i]<-y[i]}else{z[i]<-x[i]}
	}
	return(as.numeric(z))
}

##主函数
dataproc <- function(
path, ##path是读取的路径
Y, ##需要插补的数据
method, ##使用的方法
outpath
){
	##读取文件,表头处理
	dat <- read.csv(path,header=T)
	dats <- dat[-c(1:2),]
	colnames(dats) <- dat[1,]
	datsub <- dats%>%select(c(
						'air_temperature','air_pressure','air_density','air_heat_capacity',
						'es','specific_humidity','RH','VPD','Tdew',
						'wind_speed','max_wind_speed','wind_dir',
						'u*','TKE','L','(z-d)/L','T*',
						Y)) %>% apply(2,funstep6_sp,-9999)
	if(method=='XGB'){					
		##整理数据使用XGB进行训练
		colnames(datsub)[dim(datsub)[2]] <- 'Y'
		datsub_train <- as.data.frame(na.omit(datsub))
		datsub <- as.data.frame(datsub)	
		dat_xgb <- xgboosti(datsub_train,datsub)
		dat_pre <- funstep6_sn(dat_xgb$data_predict$Y,dat_xgb$data_predict$predict)
	}else if(method=='SSA'){
		##使用奇异谱分析插补	
		dat <- 	datsub[,dim(datsub)[2]]
		s <- ssa(dat, L = 72)
		dat_p <- gapfill(s, groups = list(1:6))
		dat_pre <- funstep6_sn(dat,dat_p)
		cat('不重要的信息：')
	}
	write.csv(dat_pre,outpath,row.names=F)
}









