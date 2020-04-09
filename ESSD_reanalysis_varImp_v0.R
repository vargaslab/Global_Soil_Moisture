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
yearmo_init <- 0
#model results output name
res_out <- 'ESSD_sm_prediction_kknn_2017_2018_mon.csv'
#list to paths and names of ESA raster files
lis <- list.files('/home/mguevara/Downloads/ESACCI-SM-COMBINED_2017-2018-v045_metrics/', full.names=TRUE)
#e.g., select only 1 year in a monthly basis
#here selecting the mean of 2017 and 2018
lis <- lis[c(27:52)]
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
results<-data.frame(yearmo=numeric(), cor=numeric(),rmse=numeric(), n=numeric(),kernel=as.character(),stringsAsFactors=FALSE,k=numeric())
#save the plots in a list 
plots <- list()
#initialize the loop
###time counter
 
CronometroON()
###generate data bases
i=1



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


library(rethinking)
df1 <- df[sample(nrow(df), 1000), ]
df1 <- na.omit(df1)

df1 <- data.frame(y=df1$y, scale(df1[-1]))

DAT <- df1[c(1, 16, 17:39 )][-7]
m1 <- glimmer( y~., DAT)
ms1 <- map2stan( m1$f , data=m1$d )

DAT <- df1[c(1, 16, 17:39 )][-7]
m1 <- glimmer( y~., DAT)
ms1 <- map2stan( m1$f , data=m1$d )

DAT <- df1[c(1, 40:45)]
m1 <- glimmer( y~., DAT)
ms1 <- map2stan( m1$f , data=m1$d )


prot

m1 <- glimmer( y~., df[c(1, 2)])
ms1 <- map2stan( m1$f , data=m1$d )
m2 <- glimmer( y~., df[c(1, 3)])
ms2 <- map2stan( m2$f , data=m2$d )

m2 <- glimmer( y~., df[c(1, 3)])
ms2 <- map2stan( m2$f , data=m2$d )

m2 <- glimmer( y~., df[c(1, 3)])
ms2 <- map2stan( m2$f , data=m2$d )

m2 <- glimmer( y~., df[c(1, 3)])
ms2 <- map2stan( m2$f , data=m2$d )
e <- ensemble(ms1, ms2)
for(i in 3:dim(df)[2]) {
m <- glimmer( y~., df[c(1, i)])
ms <- map2stan( m$f , data=m$d )
e <- ensemble(e, ms)


