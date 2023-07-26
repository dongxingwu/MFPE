##
##
##30min涡度和气象数据自动处理程序
##基于REddyProc包
##STEP1数据整理为标准的输入数据
##黑河气象数据为10min-涡度数据为30min
##使用的方程
mean_na <- function(x){
	if(length(na.omit(x))=="0"){
	r<-NA}else{
	r<-mean(na.omit(as.numeric(x)))}
	return(r)}
sum_na <- function(x){
	if(length(na.omit(x))=="0"){
	r<-NA}else{
	r<-sum(na.omit(as.numeric(x)))}
	return(r)}
max_na <- function(x){
	if(length(na.omit(x))=="0"){
	r<-NA}else{
	r<-max(na.omit(as.numeric(x)))}
	return(r)}	
min_na <- function(x){
	if(length(na.omit(x))=="0"){
	r<-NA}else{
	r<-min(na.omit(as.numeric(x)))}
	return(r)}	

funstep6_sp <- function(x,y){
	x <- as.numeric(x)
	if(y %in% x){
	x[which(x==y)]<-NA}else{x<-x}
	return(x)
}

##x是，y=1是求平均y=2是求和
fun30 <- function(x,y){
	r <- array(data=0,dim=c(17521,1))
	x <- as.data.frame(x)
	##第一个值不需要修改
	r[1,] <- x[1,1]
	rr <- as.matrix(x[-1,1])
	if(y=="1"){
		dim(rr)<-c(3,17520)
		r[2:17521,] <- apply(rr,2,mean_na)
		r <- as.data.frame(r)
		}else if(y=="2"){
		dim(rr)<-c(3,17520)
		r[2:17521] <- apply(rr,2,sum_na)
		r <- as.data.frame(r)
		}
	return(r)
}


leap<-function(x){
        if(x%%4==0){
                if(((x%%100==0)&(x%%400==0))|(x%%100!=0))
                        print(TRUE) else
                                print(FALSE)
        } else
                print(FALSE)
}


##提取需要的涡度数据
inputs_ec <- function(path,year){
	library(dplyr)
	##数据缺失情况并插补
	##这里的path是'Arou2013.csv'
	dat_ec <- datagl_s(path,2,year) %>% select(TIMESTAMP,Ustar,Hs,LE,NEE,QA_Hs,QA_LE,QA_NEE)
	dat_ec[,2:8]<-apply(dat_ec[,2:8],2,funstep6_sp,-6999)											
	return(dat_ec)
}


##提取需要的气象数据
inputs_awc <- function(path,year){
	library(stringr)
	##这里的path是'Arou2013.csv'
	##设定每个站点的选取变量
	#id <- substr(path,22,str_length(path)-8)
	##检查数据的时间是否缺失
	dat_awc <- datagl_s(path,2,year)%>%select(TIMESTAMP,Tair,rH,Tsoil,Rg)
	dat_awc[,2:6]<-apply(dat_awc[,2:6],2,funstep6_sp,-6999)	
	##
	return(dat_awc)
}

##是否计算存储量
Fstorage <- function(x,storage,height){
##y==1的话需要计算存储量其他值不计算
##height是冠层高度
	y <- array()
	if(storage=="1"){
		for(i in 1:length(x)){
			if(i=="1"){
				y[i]<-x[i]
			}else{
				y[i] <- ((x[i]-x[i-1])*as.numeric(height))/30+x[i]
			}
		}
	}else{
		y[i]<-x[i]
	}
	return(y)
}


##数据时间插补，处理存在时间段不连续的情况
##针对10min气象数据和30min通量数据
datagl <- function(path,y,year){
	##首先根据年份计算数据长度
	if(leap(year)){
		len30 <- 17569 ##闰年
	}else{
		len30 <- 17521 ##平年
	}
	if(leap(year)){
		len10 <- 52609 ##闰年
	}else{
		len10 <- 52561 ##平年
	}
	library(lubridate)
	x <- read.csv(path,header=T)
	r1 <- array(data=NA,dim=c(len10,dim(x)[2]-1))
	r2 <- array(data=NA,dim=c(len30,dim(x)[2]-1))
	##如果时间戳名字不是TIMESTAMP那么将[1,1]改为TIMESTAMP
	colnames(x)[1] <- 'TIMESTAMP'
	ts <- as.POSIXct(x$TIMESTAMP,tz = "UTC")
	##开始时间和结束时间
	start_time <- ymd_hm(paste0(year,"-01-01 00:00"),tz = "UTC")
	end_time <- ymd_hm(paste0(year+1,"-01-01 00:00"),tz = "UTC")
	t10 <- seq(start_time, end_time, by = "10 mins")
	t30 <- seq(start_time, end_time, by = "30 mins")
	##首先识别数据的长度如果存在缺失就插补
	if(y=="1"){##按照10min进行插补
		if(length(x[,1])==len10){
			r1=x[,-1]
		}else{
			for(i in 1:length(ts)){
				print(paste0("我正在检查第",i,"个数据"))
				r1[which(t10==ts[i]),]<- as.matrix(x[i,-1])
			}
		##补充上第一列
		rr <-data.frame(t10,r1)
		}
	}else if(y=="2"){
		##按照30min进行插补
		if(length(x[,1])==len30){
			r2=x[,-1]
		}else{
			for(i in 1:length(ts)){
				print(paste0("我正在检查第",i,"个数据"))
				r2[which(t30==ts[i]),]<-as.matrix(x[i,-1])
			}
		rr <-data.frame(t30,r2)
		}
	}
	rr <- as.data.frame(rr)
	colnames(rr)<-colnames(x)
	return(rr)
}



##处理气象数据10min变为30min
awc30 <- function(path,##气象数据路径
	y, ##固定为1
	year##处理的年份
){
	data <- datagl(path,1,year)
	if(leap(year)){
		len30 <- 17569 ##闰年
	}else{
		len30 <- 17521 ##平年
	}
	if(leap(year)){
		len10 <- 52609 ##闰年
	}else{
		len10 <- 52561 ##平年
	}
	start_time <- ymd_hm(paste0(year,"-01-01 00:00"),tz = "UTC")
	end_time <- ymd_hm(paste0(year+1,"-01-01 00:00"),tz = "UTC")
	time_seq30 <- seq(start_time, end_time, by = "30 mins")
	#t30 <- read.csv("h:/data/all/test/ts30.csv",header=T)[,1]
	##输入10分钟输出30min
	#data <- read.csv(path,header=T)
	#data <- as.data.frame(x)
	##异常值转换为NA
	data1 <- apply(data[,2:length(data[1,])],2,funstep6_sp,-6999)
	##构建一个17521*(data-1)的矩阵
	rs <- array(data=0,dim=c(len30,length(data1[1,])))
	for(ii in 1:length(data1[1,])){
		print(paste0("我正在处理第",ii,"个数据"))
		##判断Rain是否在名字中
		if("Rain" %in% colnames(data1)){ 
			j <- which(colnames(data)=="Rain")
			##如果不是Rain那么求平均值如果是Rain求和
			if(ii!=j){
				rs[,ii] <- as.matrix(fun30(data1[,ii],1))
			}else{
				rs[,ii] <- as.matrix(fun30(data1[,ii],2))
			}
		}else{
			rs[,ii] <- as.matrix(fun30(data1[,ii],1))
		}
	}
	dat <- data.frame(time_seq30,rs)
	dat <- as.data.frame(dat)
	colnames(dat) <- colnames(data)
	return(dat)
}






##下面是不考虑闰年的情况全部是365天为了网站处理的格式
##数据时间插补，处理存在时间段不连续的情况
##针对10min气象数据和30min通量数据
datagl_s <- function(path,y,year){
	##首先根据年份计算数据长度
	len30 <- 17521;len10 <- 52561
	library(lubridate)
	x <- read.csv(path,header=T)
	r1 <- array(data=NA,dim=c(len10,dim(x)[2]-1))
	r2 <- array(data=NA,dim=c(len30,dim(x)[2]-1))
	##如果时间戳名字不是TIMESTAMP那么将[1,1]改为TIMESTAMP
	colnames(x)[1] <- 'TIMESTAMP'
	##如果时间戳存在NA就先补充
	ts <- as.POSIXct(x$TIMESTAMP,tz = "UTC")
	##将时间改为2013年，如果是通用处理那么不改
	year(ts)<-2013
	if(leap(year)){
		start_time <- ymd_hm(paste0(year,"-01-01 00:00"),tz = "UTC")
		end_time <- ymd_hm(paste0(year+1,"-01-01 00:00"),tz = "UTC")
		t10 <- seq(start_time, end_time, by = "10 mins")[-c(8497:8640)]
		t30 <- seq(start_time, end_time, by = "30 mins")[-c(2833:2880)]
	}else{
		start_time <- ymd_hm(paste0(year,"-01-01 00:00"),tz = "UTC")
		end_time <- ymd_hm(paste0(year+1,"-01-01 00:00"),tz = "UTC")
		t10 <- seq(start_time, end_time, by = "10 mins")
		t30 <- seq(start_time, end_time, by = "30 mins")
	}
	##首先识别数据的长度如果存在缺失就插补
	if(y=="1"){##按照10min进行插补
		if(length(x[,1])==len10){
			r1=x[,-1]
		}else{
			for(i in 1:length(ts)){
				print(paste0("我正在检查第",i,"个数据"))
				r1[which(t10==ts[i]),]<- as.matrix(x[i,-1])
			}
			##补充上第一列
			rr <-data.frame(t10,r1)
		}
	}else if(y=="2"){##按照30min进行插补
		if(length(x[,1])==len30){
			r2=x[,-1]
		}else{
			for(i in 1:length(ts)){
				print(paste0("我正在检查第",i,"个数据"))
				r2[which(t30==ts[i]),]<-as.matrix(x[i,-1])
			}
		}
		rr <-data.frame(t30,r2)
	}
	rr <- as.data.frame(rr)
	colnames(rr)<-colnames(x)
	return(rr)
}





##处理气象数据10min变为30min
awc30_s <- function(path,##气象数据路径
	y, ##固定为1
	year##处理的年份
){
	data <- datagl_s(path,1,year)
	len30 <- 17521;len10 <- 52561
	if(leap(year)){
		start_time <- ymd_hm(paste0(year,"-01-01 00:00"),tz = "UTC")
		end_time <- ymd_hm(paste0(year+1,"-01-01 00:00"),tz = "UTC")
		time_seq30 <- seq(start_time, end_time, by = "30 mins")[-c(2833:2880)]
	}else{
		start_time <- ymd_hm(paste0(year,"-01-01 00:00"),tz = "UTC")
		end_time <- ymd_hm(paste0(year+1,"-01-01 00:00"),tz = "UTC")
		time_seq30 <- seq(start_time, end_time, by = "30 mins")
	}
	#t30 <- read.csv("h:/data/all/test/ts30.csv",header=T)[,1]
	##输入10分钟输出30min
	#data <- read.csv(path,header=T)
	#data <- as.data.frame(x)
	##异常值转换为NA
	data1 <- as.data.frame(apply(data[,2:length(data[1,])],2,funstep6_sp,-6999))
	##构建一个17521*(data-1)的矩阵
	rs <- array(data=0,dim=c(len30,length(data1[1,])))
	for(ii in 1:length(data1[1,])){
		print(paste0("我正在处理第",ii,"个数据"))
		rs[,ii] <- as.matrix(fun30(data1[,ii],1))
	}
	dat <- data.frame(time_seq30,rs)
	dat <- as.data.frame(dat)
	colnames(dat) <- colnames(data)
	if('Rain' %in% colnames(data)){
		dat$Rain <- dat$Rain*3
	}
	return(dat)
}





























