library(raster)
library(dplyr)
library(openair)
library(Metrics)

library(scales)

valset <- readRDS('ALL_ismn.rds')
valset[valset$sm<=0,] <- NA
valset <- na.omit(valset)
valset[valset$sm>=1,] <- NA
valset <- na.omit(valset)
summary(valset)

all <- list.files(pattern='tif')[2:27]
all <- stack(all)
library(spatialEco)
#all <- raster.transformation(all, trans = "stretch", smin = min(valset$sm), smax = max(valset$sm))
res <- data.frame()
for (i in 1991:2016){
val <- valset[grep(i,valset$date),]
val <-data.frame(val)
val$e <- extract(all[[i-1990]], val[c('x', 'y')])
res <- rbind(res, val)
}
all_covs <- na.omit(res)
#all_covs$e <- rescale(all_covs$e, c(min(all_covs$sm), max(all_covs$sm)))

topo <- stack(list.files('/home/mguevara/Downloads/predicted-2001-2016-esa-sm-topo-GLOBALmean/', full.names=TRUE
, pattern='tif'))
#topo[topo<0] <- NA
#topo[topo>1] <- NA
library(spatialEco)
#topo <- raster.transformation(topo, trans = "stretch", smin = min(valset$sm), smax = max(valset$sm))
res <- data.frame()
for (i in 1991:2016){
val <- valset[grep(i,valset$date),]
val <-data.frame(val)
val$e <- extract(topo[[i-1990]], val[c('x', 'y')])
res <- rbind(res, val)
}
topo <- na.omit(res)
topo$e[topo$e < 0] <- NA
topo <- na.omit(topo)
#topo$e <- rescale(topo$e, c(min(topo$sm), max(topo$sm)))

res <- data.frame()
esa <- stack(list.files('/home/mguevara/Downloads/annual/', full.names=TRUE))
library(spatialEco)
#esa <- raster.transformation(esa, trans = "stretch", smin = min(valset$sm), smax = max(valset$sm))
res <- data.frame()
for (i in 1991:2016){
val <- valset[grep(i,valset$date),]
val <-data.frame(val)
val$e <- extract(esa[[i-1990]], val[c('x', 'y')])
res <- rbind(res, val)
}

esa <- na.omit(res)
#esa$e <- rescale(esa$e, c(min(esa$sm), max(esa$sm)))

all_covs$yr <-  as.numeric(substr(all_covs$date, 1, 4))
ALL=all_covs %>% group_by(yr) %>% summarise_each(list(mean = mean))
ALL$e <- rescale(ALL$e_mean, c(min(ALL$sm_mean), max(ALL$sm_mean)))
cor(ALL$sm_mean, ALL$e)
rmse(ALL$sm_mean, ALL$e)

ISMN <- data.frame(date=as.Date(paste0(ALL$yr, '/01/01/')), sm = ALL$sm_mean)
TheilSen(ISMN, 'sm', slope.percent = TRUE, alpha = 0.01, ylim=c(0.1, 0.4),  dec.place=3, deseason=TRUE, shade = "white", lab.cex=1.5, data.col='black', text.col='black',  scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)))

scatterPlot(ALL, x = "sm_mean", y = "e",  mod.line=TRUE, smooth=TRUE)
ALL <- data.frame(date=as.Date(paste0(ALL$yr, '/01/01/')), sm = ALL$e)
TheilSen(ALL, 'sm', slope.percent = TRUE, alpha = 0.01, ylim=c(0.1, 0.4),  dec.place=3, deseason=TRUE, shade = "white", lab.cex=1.5, data.col='black', text.col='black',  scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)))

topo$yr <-  as.numeric(substr(topo$date, 1, 4))
TOPO=topo %>% group_by(yr) %>% summarise_each(list(mean = mean))
TOPO$e <- rescale(TOPO$e_mean, c(min(TOPO$sm_mean), max(TOPO$sm_mean)))
 cor(TOPO$sm_mean, TOPO$e)
 rmse(TOPO$sm_mean, TOPO$e)
scatterPlot(TOPO, x = "sm_mean", y = "e",  mod.line=TRUE, linear=TRUE)


TOPO <- data.frame(date=as.Date(paste0(TOPO$yr, '/01/01/')), sm = TOPO$e)
TheilSen(TOPO, 'sm', slope.percent = TRUE, alpha = 0.01, ylim=c(0.1, 0.4),  dec.place=3, deseason=TRUE, shade = "white", lab.cex=1.5, data.col='black', text.col='black',  scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)))

esa$yr <-  as.numeric(substr(esa$date, 1, 4))
ESA=esa %>% group_by(yr) %>% summarise_each(list(mean = mean))
ESA$e <- rescale(ESA$e_mean, c(min(ESA$sm_mean), max(ESA$sm_mean)))
cor(ESA$sm_mean, ESA$e)
rmse(ESA$sm_mean, ESA$e)
scatterPlot(ESA, x = "sm_mean", y = "e",  mod.line=TRUE, linear=TRUE)
ESA <- data.frame(date=as.Date(paste0(ESA$yr, '/01/01/')), sm = ESA$e)
TheilSen(ESA, 'sm', slope.percent = TRUE, alpha = 0.01, ylim=c(0.1, 0.4),  dec.place=3, deseason=TRUE, shade = "white", lab.cex=1.5, data.col='black', text.col='black',  scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)))










ben <- read.csv('/home/mguevara/Downloads/SRDB_V4_1578/data/srdb-data-V4.csv')
prec <- ben[c(2,13:14, 34, 21)]
res <- data.frame()
#ras <- stack(list.files('/home/mguevara/Downloads/annual/', full.names=TRUE))
#ras<- stack(list.files('/home/mguevara/Downloads/predicted-2001-2016-esa-sm-topo-GLOBALmean/', full.names=TRUE, pattern='tif'))
ras <- stack(list.files(pattern='tif')[2:27])
res <- data.frame()
for (i in 1991:2016){
val <- prec[grep(i,prec$Entry_date),]
val <-data.frame(val)
val$e <- extract(ras[[i-1990]], val[c('Longitude', 'Latitude')])
res <- rbind(res, val)
}

all <- res
all$e[all$e < 0] <- NA
all <- na.omit(all)
all$sm <- log1p(all$e)
cor(all$MAP, all$e)

 scatterPlot(all, x = "MAP", y = "sm", smooth = TRUE)












cor(log1p(all_covs$e), log1p(all_covs$sm))
cor(log1p(topo$e), log1p(topo$sm))
cor(log1p(esa$e), log1p(esa$sm))

library(Metrics)

rmse(all_covs$e, all_covs$sm)
rmse(topo$e,topo$sm)
rmse(esa$e, esa$sm)


esa$yr <-  as.numeric(substr(esa$date, 1, 4))
esa$date <-as.Date(paste0(esa$yr, '/01/01/'))

esa %>% 
  group_by(year = as.numeric(yr)) %>% 
  summarise(sm = mean(sm),  
		x = mean (x), y=mean(y))   ->  ESA

		ESA <- ESA[ESA$year>1990,]
		ESA <- ESA[ESA$year<2017,]

ESA$date <-as.Date(paste0(ESA$year, '/01/01/'))

topo$yr <-  as.numeric(substr(topo$date, 1, 4))
topo$date <-as.Date(paste0(topo$yr, '/01/01/'))

topo %>% 
  group_by(year = as.numeric(yr)) %>% 
  summarise(sm = mean(sm),  
		x = mean (x), y=mean(y))   ->  TOPO

		TOPO <- TOPO[TOPO$year>1990,]
		TOPO <- TOPO[TOPO$year<2017,]

TOPO$date <-as.Date(paste0(TOPO$year, '/01/01/'))

all_covs$yr <-  as.numeric(substr(all_covs$date, 1, 4))
all_covs$date <-as.Date(paste0(all_covs$yr, '/01/01/'))

all_covs %>% 
  group_by(year = as.numeric(yr)) %>% 
  summarise(sm = mean(sm),  
		x = mean (x), y=mean(y))   ->  ALL

		ALL <- ALL[ALL$year>1990,]
		ALL <- ALL[ALL$year<2017,]

ALL$date <-as.Date(paste0(ALL$year, '/01/01/'))

valset$yr <-  as.numeric(substr(valset$date, 1, 4))
valset$date <-as.Date(paste0(valset$yr, '/01/01/'))

valset %>% 
  group_by(year = as.numeric(yr)) %>% 
  summarise(sm = mean(sm),  
		x = mean (x), y=mean(y))   ->  VAL

		VAL <- VAL[VAL$year>1990,]
		VAL <- VAL[VAL$year<2017,]

VAL$date <-as.Date(paste0(VAL$year, '/01/01/'))


TheilSen(VAL, 'sm', slope.percent = TRUE, alpha = 0.01, ylim=c(0.1, 0.4),  dec.place=3, deseason=TRUE, shade = "white", lab.cex=1.5, data.col='black', text.col='black',  scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)))



TheilSen(ESA, 'sm', slope.percent = TRUE, alpha = 0.01, ylim=c(0.1, 0.4),  dec.place=3, deseason=TRUE, shade = "white", lab.cex=1.5, data.col='black', text.col='black',  scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)))


TheilSen(ALL, 'sm', slope.percent = TRUE, alpha = 0.01, ylim=c(0.1, 0.4),  dec.place=3, deseason=TRUE, shade = "white", lab.cex=1.5, data.col='black', text.col='black',  scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)))
	

TheilSen(TOPO, 'sm', slope.percent = TRUE, alpha = 0.01, ylim=c(0.1, 0.4),  dec.place=3, deseason=TRUE, shade = "white", lab.cex=1.5, data.col='black', text.col='black',  scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)))
























topo$yr <-  as.numeric(substr(topo$date, 1, 4))
topo$date <-as.Date(paste0(topo$yr, '/01/01/'))
TheilSen(topo, 'e', slope.percent = TRUE, alpha = 0.01, ylim=c(0.1, 0.4),  dec.place=3, deseason=TRUE, shade = "white", lab.cex=1.5, data.col='black', text.col='black',  scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)))

all_covs$yr <-  as.numeric(substr(all_covs$date, 1, 4))
all_covs$date <-as.Date(paste0(all_covs$yr, '/01/01/'))
TheilSen(all_covs, 'e', slope.percent = TRUE, alpha = 0.01, ylim=c(0.1, 0.4),  dec.place=3, deseason=TRUE, shade = "white", lab.cex=1.5, data.col='black', text.col='black',  scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)))


all_covs$yr <-  as.numeric(substr(all_covs$date, 1, 4))
all_covs$date <-as.Date(paste0(all_covs$yr, '/01/01/'))
TheilSen(all_covs, 'sm', slope.percent = TRUE, alpha = 0.01, ylim=c(0.1, 0.4),  dec.place=3, deseason=TRUE, shade = "white", lab.cex=1.5, data.col='black', text.col='black',  scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)))




library(dplyr)
esa %>% 
 group_by(year = as.factor(yr)) %>% 
 summarise(sm = mean(e), 
x = mean (x), y=mean(y))   ->  DAT
DAT$date <-as.Date(paste0(DAT$year, '/01/01/'))
TheilSen(DAT, 'sm', slope.percent = TRUE, alpha = 0.01, ylim=c(0.1, 0.4),  dec.place=3, deseason=TRUE, shade = "white", lab.cex=1.5, data.col='black', text.col='black',  scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)))

topo$yr <-  as.numeric(substr(topo$date, 1, 4))
topo %>% 
 group_by(year = as.factor(yr)) %>% 
 summarise(sm = mean(e), 
x = mean (x), y=mean(y))   ->  DAT
DAT$date <-as.Date(paste0(DAT$year, '/01/01/'))
TheilSen(DAT, 'sm', slope.percent = TRUE, alpha = 0.01, ylim=c(0.1, 0.4),  dec.place=3, deseason=TRUE, shade = "white", lab.cex=1.5, data.col='black', text.col='black',  scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)))

all_covs$yr <-  as.numeric(substr(all_covs$date, 1, 4))
all_covs %>% 
group_by(year = as.factor(yr)) %>% 
summarise(sm = mean(e), 
x = mean (x), y=mean(y))   ->  DAT
DAT$date <-as.Date(paste0(DAT$year, '/01/01/'))
TheilSen(DAT, 'sm', slope.percent = TRUE, alpha = 0.01, ylim=c(0.1, 0.4),  dec.place=3, deseason=TRUE, shade = "white", lab.cex=1.5, data.col='black', text.col='black',  scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5)))



#6570 datos 

#75%

###
###Rodrigo Vargas data
library(dplyr)
y <- 21.216667	
x <- -87.183333
library(maps)
#d <- data.frame(x=x, y=y, z=1)
dat<- read.csv('8309567/CO2_Efflux_Eden.csv')
dat$x <- x
dat$y <- y
proms <- dat %>% group_by(Year) %>% summarise_each(funs(mean))
library(raster)
s <- stack(list.files()[40:42])
library(RStoolbox)
s <- raster.transformation(s, trans = "norm", smin = 0, smax = 1)
model <- as.numeric(extract(s, data.frame(x, y)))
data <- data.frame(mod=model, obs=proms$SWC, yr=proms$Year, src='Vargas2012')
#https://iopscience.iop.org/article/10.1088/1748-9326/7/3/035704

###
###DAAC fluxnet BRA
###https://daac.ornl.gov/LBA/guides/CD32_Brazil_Flux_Network.html
lis <- list.files('CD32_BRAZIL_FLUX_NETWORK_1174/data/', full.names=TRUE)[-10]
data1 <- data.frame()
lista <- list.files(lis, pattern='csv',full.names=TRUE)
#lista <- lista[grep('Avg_month.csv', lista)]
for (i in 1:length(lista)){
#i=1
data <- read.csv(lista[i])
yr <- data[1][-c(1:3),]
SWC <- data[76][-c(1:3),]
src=unlist(strsplit(lista[i], '/'))[4]
data <- data.frame(obs=SWC, yr=yr, src=src)
data1 <- rbind(data1, data)
print(i)
}

src <- c('STM_K67', 'STM_K77', 'STM_K83', 'MAN_K34', 'PA_CAX', 'RON_FNS', 'RON_RJA', 'TOC_BAN', 'SP_PDG' )
x <- c(-54.95900, -54.88850,-54.97070, -60.00000, -51.45360, -62.35720, -61.93310, -50.1591111,	-47.6498889 )
y <- c(-2.85700,  -3.02020, -3.01700, -2.50000, -1.74830, -10.76180, -10.07800, -9.824416667, -21.61947222)
data2 <- data.frame(src=src, x=x, y=y)
d <- merge(data1, data2)
d$obs <- as.numeric(as.character(as.factor(as.character(d$obs))))
d <- na.omit(d)
d[d$obs==-9999,] <- NA
d <- na.omit(d)
d[is.infinite(d$obs)==TRUE,] <- NA
d <- na.omit(d)
d$yr <- as.numeric(as.character(as.factor(as.character(d$yr))))
#d$src_id<- as.numeric(d$src)
d$src <- as.factor(as.character(d$src))
summary(d)

#proms <- d[-1] %>% group_by(yr) %>% summarise_each(funs(mean))
library(raster)
s <- stack(list.files()[34:40])
library(spatialEco)
s <- raster.transformation(s, trans = "norm", smin = 0, smax = 1)
model <-data.frame(extract(s, d[4:5]))
names(model) <- c(2000:2006)
model <- t(model)
proms <- cbind(d, model=colMeans(model))
proms <- proms[-1] %>% group_by(yr) %>% summarise_each(funs(mean))



s2 <- stack(list.files('annual', full.names=TRUE)[10:16])
s2 <- raster.transformation(s2, trans = "norm", smin = 0, smax = 1)
sat <-data.frame(extract(s2, d[4:5]))
names(sat) <- c(2000:2006)
sat <- t(sat)
promsat <- cbind(d, sat=colMeans(model))
promsat <- promsat[-1] %>% group_by(yr) %>% summarise_each(funs(mean))
 


###
###DAAC Ben Bond Lamberty
###



#coordinates(dat) <- ~x+y
#proj4string(dat) <- CRS("+init=epsg:4326")
#library(plotKML)
#plotKML(dat['SWC'])


cor(X$SWC, c(0.2637043,0.2654383, 0.2510874))

