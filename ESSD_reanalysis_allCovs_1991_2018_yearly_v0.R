###PREPARED BY MARIO GUEVARA, 2019
###UNIVERSITY OF DELAWARE
###GLOBAL SOIL MOISTURE PREDICTIONS
###https://www.hydroshare.org/resource/b940b704429244a99f902ff7cb30a31f/
###https://www.earth-syst-sci-data-discuss.net/essd-2019-191/


#time counter function
CronometroON<- function(){
      tic<-proc.time()[3]
      assign(".tic", tic, envir=baseenv())
      invisible(tic)
      }

 CronometroOFF<- function(){
      tic <- get(".tic", envir=baseenv())
      toc<-proc.time()[3]-tic
      hrs<-as.integer(toc/3600)
      minu<- as.integer(((toc/3600)-hrs)*60)
      seg<- ((((((toc/3600)-hrs)*60)))-minu)*60
      time<-paste(as.character(hrs),"hrs ",as.character(minu),"min ",as.character(round(seg,digit=2)),"seg",sep="")
      return(time)
      }
CronometroON()

#predict using all covariates see list above
vars <- c(1:44)
#start time (0 if starting montly in January or any year for annual basis)
yearmo_init <- 1990
#model results output name
res_out <- 'ESSD_sm_prediction_kknn_1991_2018_mon.csv'
#list to paths and names of ESA raster files
lis <- list.files('/home/mguevara/Downloads/annual/', full.names=TRUE)
#e.g., select only 1 year in a monthly basis
#here selecting the mean of 2017 and 2018
#lis <- lis[c(27:52)]
#problem definition (all covariates, monthly predictions year 2017-2018)
probdef <-'essdd_ALL_COVS_15km_'
#libraries
library(rgdal)
library(raster)
library(kknn)
library(vip)
#load covariates
x <- readRDS('ESSDD_covariates_v2.rds')
#function to use a median value for NAs, at the borders 
#NA2median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
#replace with median
#x@data[] <- lapply(x@data, NA2median)
#empty data frame for storing results
results<-data.frame(yearmo=numeric(), cor=numeric(),rmse=numeric(), n=numeric(),kernel=as.character(),stringsAsFactors=FALSE,k=numeric(), time=character())
#initialize the loop
###time counter
 
###generate data bases
#1=1
for (i in 1:length(lis)){
	#ras <- raster('/home/mguevara/Downloads/ESACCI-SM-COMBINED_2017-2018-v045_metrics/World_sm_mean_2017_version_45.tif')
	#read raster i
	ras <- raster(lis[i])

	#define proyection
	#proj4string(ras) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
	#convert to data frame
	df=as.data.frame(ras, xy=T)
	#remove NAs
	df=na.omit(df)
	#define coordinates
	coordinates(df)=~x+y
	#define proyection
	proj4string(df) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
	#extract values to points, only the first 5 PCAs
	#change here if needed (names(df))
	#ov=over(df, x[c(1:15, 39:44)])
	ov=over(df, x[vars])
	#to data frame
	d=as.data.frame(df)
	#combine extraction with data
	X=cbind(d,  ov)
	#copy data frame (only columns of interes)
	df=data.frame(y=X[,3], X[,4:dim(X)[2]])
	#only oblique coordinates
	#remove NAs again
	df=na.omit(df)
	#df=data.frame(y=X[,3], X[,4:9])
	names(df)
	print(dim(df))
	##you must select the best parameters by tunning them with CV,  the parameter K and the parameter kernel
	#test a max value of 25 for k
	kmax=25
	#tune the model
	#df <- df[1:100,]
	knnTuning <- train.kknn(y~., data=df, kmax = kmax, distance = 2,
	kernel = c("rectangular", "triangular", "epanechnikov","gaussian", "rank", "optimal"),
		   ykernel = NULL, scale = TRUE,kcv=10)
	#extract best paramters
	n<-which(knnTuning$best.parameters$kernel==c("rectangular", "triangular", "epanechnikov","gaussian", "rank", "optimal"))
	#save best paramters
	mejoresresultados <- data.matrix(unlist(knnTuning$fitted.values[[(kmax*(n-1))+knnTuning$best.parameters$k]]))
	#calculate RMSE
	rmse <- sqrt(knnTuning$MEAN.SQU[knnTuning$best.parameters$k,n])
	#calculate correlation obs pred
	(cd <- cor(df[,1], mejoresresultados))
	#run the best model (with the best parameters) and make predictions (maps)
	print(CronometroON())
	#x <- x[vars][1:100,]
	mejorKKNN <- kknn(y~.,train=df,test=x,kernel=unlist(knnTuning$best.parameters[1]),
		                scale=TRUE,k=as.numeric(knnTuning$best.parameters[2]))
	print(CronometroOFF())
	#get the fitted values for all the area (x)
	x$kknn=mejorKKNN$fitted.values
	#coordinates(kknn)=~x+y
	#gridded(kknn) = TRUE
	#kknn=as(kknn,'SpatialPixelsDataFrame')
	r <- raster(x['kknn'])
	#save the prediction raster (check year/month )
	writeRaster(r, file=#index
	out <- paste0(probdef, unlist(strsplit(lis[i], '//'))[2])
	, overwrite=TRUE) ### 
	#save month, replace by year if using yearly averages
	yearmo=yearmo_init+i
	#best k value
	k=as.numeric(knnTuning$best.parameters[2])
	#store in the results data frame (month), replace by year if using yearly averages
	results[i,1]<-yearmo
	#correlation obs pred
	results[i,2]<-cd
	#root mean squared error
	results[i,3]<-rmse
	#data available for that month/year
	results[i,4]<-dim(df)[1]
	#best kernel
	results[i,5]<-unlist(knnTuning$best.parameters[1])
	#k value
	results[i,6]<-k
	#print results for model i
	print(i)
	print(cd)
	print(rmse)
}
#modeling results 
print(results)
#save csv with modeling results
write.csv(results, file=res_out )
#total time 
CronometroOFF()
#end


