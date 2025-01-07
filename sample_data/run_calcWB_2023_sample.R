### 0.1. empty environment ---
rm(list=ls())
try(dev.off())

### 0.2 configuration ---
mypath="C:/Users/username/Desktop/WaterBalanceMod_Git/waterbalancemodel/sample_data" #replace example path with your local directory
shape_site = sf::st_read(paste(mypath,"/Shapefile/sample_2023.shp",sep="")) #shapefile of AOI
target_res=5 #spatial resolution of results, must be >=5
modeltype="poly" #poly or linear, leave at poly
last_NDVI_0=132 #last day with NDVI==0, so with no visible vegetation
ET_ref=read.csv(paste(mypath,"/DWD_ET0_2023.csv",sep=""),sep=",") #csv file with with daily reference ET or leave at NA. If NA, then...
ET_ref_dl="DWD" #...choose from "DWD" or "Arable" to download reference ET
path_WR_precip=paste(mypath,"/Radolan_2023_processed_daily",sep="") #folder with shapefiles of daily precipitation for AOI, either from furuno or radolan or leave at "NA".
precip_source="radolan" #choose "radolan" or "furuno". if line above is NA, then choose "radolan" to automatically download und process radolan data
irrig_sf=sf::st_read(paste(mypath,"/Shapefile/Buffer_36m_all_interp.shp",sep="")) #Shapefile with irrigation amounts from raindancer
irrigation_efficiency=1 #0.775 #choose irrigation efficiency. If unsure, leave at 1
my_year=2023 #Year of interest "YYYY"
save_shape=TRUE #save results as shapefile?
save_geotiff=TRUE #save results as geotiff?
save_RDATA=TRUE #save results as RDATA?
arable_user="username" #user name for arable account, if you choose to download reference ET from Arable
arable_pass="password" #password for arable account, if you choose to download reference ET from Arable

### 1. run model ----
test_wb=WaterBalanceMod::calcWB(mypath=mypath,
               shape_site=shape_site,
               target_res=target_res,
               modeltype=modeltype,
               last_NDVI_0=last_NDVI_0,
               ET_ref=ET_ref,
               ET_ref_dl=ET_ref_dl,
               output_year=my_year,
               path_WR_precip=path_WR_precip,
               precip_source=precip_source,
               irrig_sf=irrig_sf,
               irrigation_efficiency=irrigation_efficiency,
               save_shape=save_shape,
               save_geotiff=save_geotiff,
               save_RDATA=save_RDATA,
               arable_user=arable_user,
               arable_pass=arable_pass)

############# plot procedure ----
buffer20 = sf::st_read(paste(mypath,"/Shapefile/Buffer_5_WB.shp",sep=""))
WaterBalanceMod::calcWBplots(source_path=paste(mypath,"/",modeltype,"_",precip_source,"_",irrigation_efficiency,"_",last_NDVI_0,"_",target_res,"/",
                                      "WBM_",precip_source,"_",modeltype,"_",irrigation_efficiency,"_",last_NDVI_0,"_",target_res,".RData",sep=""),
                    plant_doy=109,
                    buffer20=buffer20,
                    shape_site=shape_site)
