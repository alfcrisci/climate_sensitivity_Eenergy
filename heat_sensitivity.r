##################################################################################################
# Please before run the code install the packages :

###########################################################################################################################################################################################

options(java.parameters = "-Xmx4g" )

library(XLConnect)
library(raster)
library(rgdal)
library(greenbrown)
library(RColorBrewer)
library(lubridate)
library(doBy)

##################################################################################################


month2season=function(x) {
  res=NA
  if ( (x==1) || (x==2) ||(x==12)) {res="inv"};
  if ( (x==3) || (x==4) || (x==5) ) {res="prim"};
  if ( (x==6) || (x==7) ||(x==8)) {res="est"};
  if ( (x==9) || (x==10) ||(x==11)) {res="aut"};
  return(res);
  
}

##################################################################################################################

italia_bound_region=readRDS("italia_bound_region.rds")
regione_stats=read.csv("regione_stats.csv",stringsAsFactors = F)
regione_stats=regione_stats[order(regione_stats[,1]),]
italia_bound_region@data[,5:10]=regione_stats
names(italia_bound_region@data)[11]="Cun0414"
saveRDS(regione_stats,"regione_stats_01_2016.rds")
saveRDS(italia_bound_region,"italia_bound_region.rds")

##################################################################################################
# Read italian data 

tg_ita=brick("tg_ita_eobs_1950_2015.nc",lvar=1)


mean_tg=t(raster::extract(tg_ita,italia_bound_region,fun=mean,na.rm = T))
names(mean_tg)=italia_bound_region@data$NAME_1

mean_tg_2004_2014=head(tail(mean_tg,138),132)
mean_tg_2004_2014_date=as.Date(ymd(gsub("^X","",row.names(mean_tg_2004_2014))))

tm_monthly_2004_2014=data.frame(year=year(mean_tg_2004_2014_date),month=month(mean_tg_2004_2014_date),mean_tg_2004_2014)
row.names(tm_monthly_2004_2014)=NULL
names(tm_monthly_2004_2014)[21]="Valle.d.Aosta"

tm_monthly_2004_2014$stag=sapply(tm_monthly_2004_2014$month,month2season)

names(tm_monthly_2004_2014)[3:(ncol(tm_monthly_2004_2014)-1)]=gsub(" ",".",gsub("-",".",italia_bound_region@data$NAME_1))

tm_monthly_2004_2014$year=as.factor(tm_monthly_2004_2014$year)
tm_monthly_2004_2014$month=as.factor(tm_monthly_2004_2014$month)

saveRDS(tm_monthly_2004_2014,"tm_monthly_2004_2014.rds")
writeWorksheetToFile(paste0("tm_monthly_2004_2014",".xls"),tm_monthly_2004_2014,sheet="Regione")

###########################################################################################################################

tm_monthly_2004_2014_inv=tm_monthly_2004_2014[grep("inv",tm_monthly_2004_2014$stag),]
tm_monthly_2004_2014_est=tm_monthly_2004_2014[grep("est",tm_monthly_2004_2014$stag),]

names(tm_monthly_2004_2014_inv)[21]="Valle.d.Aosta"
names(tm_monthly_2004_2014_est)[21]="Valle.d.Aosta"

writeWorksheetToFile(paste0("tm_monthly_2004_2014_est",".xls"),tm_monthly_2004_2014_est,sheet="Regione")
writeWorksheetToFile(paste0("tm_monthly_2004_2014_inv",".xls"),tm_monthly_2004_2014_inv,sheet="Regione")

series_est_ITA_max=summaryBy(Abruzzo+Basilicata+Calabria+Campania+Emilia.Romagna+Friuli.Venezia.Giulia+Lazio+Liguria+Lombardia+Marche+Molise+Piemonte+Puglia+Sardegna+Sicilia+Toscana+Trentino.Alto.Adige+Umbria+Valle.d.Aosta+Veneto~year, data=tm_monthly_2004_2014_est,FUN=c(max))
series_est_ITA_med=summaryBy(Abruzzo+Basilicata+Calabria+Campania+Emilia.Romagna+Friuli.Venezia.Giulia+Lazio+Liguria+Lombardia+Marche+Molise+Piemonte+Puglia+Sardegna+Sicilia+Toscana+Trentino.Alto.Adige+Umbria+Valle.d.Aosta+Veneto~year, data=tm_monthly_2004_2014_est,FUN=c(mean))




# kwh/ab

cons_unitario_reg=read.csv("consumo_elettrico_reg.csv",stringsAsFactors = F)[,1:22]
temp=order(names(cons_unitario_reg)[2:21])+1
cons_unitario_reg=cons_unitario_reg[,c(1,temp,22)]
saveRDS(cons_unitario_reg,"cons_unitario_reg.rds")

cons_unitario_2004_2014=tail(cons_unitario_reg,11)

saveRDS(cons_unitario_2004_2014,"cons_unitario_reg_2004_2014.rds")

writeWorksheetToFile(paste0("cons_unitario_reg_serie",".xls"),cons_unitario_reg,sheet="Regione")
writeWorksheetToFile(paste0("cons_unitario_2004_2014",".xls"),cons_unitario_2004_2014,sheet="Regione")

##########################################################################################################################

cons_unitario_2004_2014_m=apply(cons_unitario_2004_2014[-1],2,mean)[1:20]

italia_bound_region@data$Cun0414=as.vector(cons_unitario_2004_2014_m)

##########################################################################################################################
# Analisi di regressione senza intercetta fra consumo unitario (tmed) tmedia estiva e (tmax) massima di media mensile estiva

mat_cons_unitario=cons_unitario_2004_2014[,2:21]

res_max=list()
for ( i in 1:20) {
                 res_max[[i]]=summary(lm(mat_cons_unitario[,i]~series_est_ITA_max[-1][,i]-1))$coefficients[1]
}

names(res_max)=names(mat_cons_unitario)

italia_bound_region@data$kwhabmax=as.numeric(unlist(res_max))

res=list()
for ( i in 1:20) {
                 res[[i]]=summary(lm(mat_cons_unitario[,i]~series_est_ITA_med[-1][,i]-1))$coefficients[1]
}

names(res)=names(mat_cons_unitario)

italia_bound_region@data$kwhabmed=as.numeric(unlist(res))

################################################################################################################

italia_bound_region@data$costomed=(italia_bound_region@data$kwhabmed*italia_bound_region@data$Popolazione)*0.18
italia_bound_region@data$costomax=(italia_bound_region@data$kwhabmax*italia_bound_region@data$Popolazione)*0.18
italia_bound_region@data$costomed=italia_bound_region@data$costomed/1000000
italia_bound_region@data$costomax=italia_bound_region@data$costomax/1000000

saveRDS(italia_bound_region,"italia_bound_region.rds")

rgdal::writeOGR(italia_bound_region, ".", "italia_heat_sensibility", driver="ESRI Shapefile")

###################################################################################################################
