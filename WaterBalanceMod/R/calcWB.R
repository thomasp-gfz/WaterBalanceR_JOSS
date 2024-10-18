#' Calculate Water Balance from UAV or PlanateScope NDVI Data
#'
#' Calculate Water Balance using DJI Phantom 4 Multispectral or PlanetScope NDVI data.
#' Reference Evapotranspiration can be used either from German Weather Service (DWD) or Arable Mark 2 ground stations from your site.
#' Precipitation is gathered from either the German Weather Service (DWD) product "RADOLAN" or FURUNO WR 2120, if available.
#' @param mypath Path to your project main folder (string). The main folder needs to contain the subfolders "NDVI_Files" containing your NDVI-files for your AOI.
#' @param shape_site shapefile of AOI (string)
#' @param target_res Resolution of product (integer). Default is 5 m, but can be turned down to at least 3 m.
#' @param method_NDVI Method of processing NDVI values: "uwdw" or "direct" (string). Direct is actually using a higher accuracy but is flagged in an unknown way. That is why "uwdw" is set as default and should be kept as this.
#' @param modeltype Method of modelling NDVI values: "poly" or "linear" (string). Should always be kept as "poly" (default).
#' @param last_NDVI_0 Number of day with day (DOI, integer) with NDVI = 0, i.e. last day before germination.
#' @param ET_ref Either csv-file with reference ET for every day of vegetation period or recent date (read.csv(paste(mypath,"/ET0_Arable_2021.csv",sep=""),sep=",")) or leave at NA. When using the list, the first column needs to be ascending numerized (integer) from one on with empty header. The second column contains the reference ET value for the certain DOY (float) with header "V1". The third column needs to be the date (format "YYYY-MM-DD", e.g. "2021-05-01). When left NA (default), the reference ET is automatically downloaded from either German Weather Servcice (DWD, default) or Arable, if you have an account. This decision needs to be made in the next step.
#' @param ET_ref_dl If you do not have any reference ET data, leave "ET_ref" as "NA" and choose here between "DWD" to download from German Weather Service ("DWD") or "Arable" to download from your Arable account ("string"). If you choose to download from your Arable Account, you need to put in your Arable login data.
#' @param output_year Number of year, you are processing (format: "YYYY", e.g. 2021, integer).
#' @param precip_source Choose either "RADOLAN" (default, string) or "FURUNO" (string) depending on the source you would like to use.
#' @param path_WR_precip Choose the path to your precipitation data (string). This should be a folder containing shapefiles with precipitation data for every day during the vegetation period you are interested in. If you leave it an NA (default), precipitation data is downloaded from German Weather Service (DWD).
#' @param irrig_sf Path to shapefile containing the irrigation data (string), e.g. st_read(paste(mypath,"/Shapefile/Buffer_36m_all_interp.shp",sep="")). The shapefile needs to contain the following coloumns: Drck_mn (water pressure, float), Dtm_Uh_ (Date and time, string, format: "YY-MM-DD hh:mm:ss"), timedif (time difference between steps in hours, float), dst_gps (spatial distance between in m the logs of sprinkler, float), gschwn_ (speed of sprinkler in m/s, float), Brg_GPS (irrigation amount, mm, float), Dstnz_k (cumulated spatial distance between logs in m, float), DOY (day of year, integer), geometry (geometric geometry). You can also generate this shapefile by 1st using the function "DownloadRaindancer" to download all of your irrigation data that was logged by raindancer. Take note, that irrigation data can only be downloaded from the last 12 days. So you should downoad regularly. In the 2nd step you can use the function "DownloadRaindancerCombineCharts" to combine the downloaded charts and process them to the needed shapefile. The resulting shapefile is being updated witht every iteration of download.
#' @param irrigation_efficiency Choose irrigation efficiency, float between 0 and 1 (default). Here, irrigation efficiency is meant to be as the fraction of water that was infiltrated in the soil from the amount that was applied.
#' @param save_shape Save results as shapefile? (TRUE or FALSE, default: TRUE)
#' @param save_geotiff Save results as geotiff? (TRUE or FALSE, default: TRUE)
#' @param save_RDATA Save results as RDATA? (TRUE or FALSE, default: TRUE)
#' @param arable_user Your user name for your Arable account (string). Only necessary, if you chose "ET_ref_dl" with "Arable". Else: leave at NA.
#' @param arable_pass Your password for your Arable account (string). Only necessary, if you chose "ET_ref_dl" with "Arable". Else: leave at NA.
#' @return Shapefiles, Geotiffs and/or RDATA-files with maps showing the water balance
#' @importFrom RCurl getURL
#' @export

calcWB=function(mypath,
                       shape_site=NA,
                       target_res=5,
                       method_NDVI="uwdw",
                       modeltype="poly",
                       last_NDVI_0=NA,
                       ET_ref=NA,
                       ET_ref_dl="DWD",
                       output_year=NA,
                       precip_source=precip_source,
                       path_WR_precip=NA,
                       irrig_sf=NA,
                       irrigation_efficiency=1,
                       save_shape=TRUE,
                       save_geotiff=TRUE,
                       save_RDATA=TRUE,
                       arable_user=NA,
                       arable_pass=NA){

  start.time = Sys.time()

  target_res=target_res
  NDVI_List=list.files(paste(mypath,"/NDVI_Files/",sep=""),pattern = "\\.tif$")
  DOY=lubridate::yday(as.POSIXct(strptime(substr(NDVI_List,1,8),"%Y%m%d" ))) #fly over dates for Planet stellites

  #### 1.create empty lists ----
  originals=list(NA)
  plot_list=list(NA)
  cropped=vector(mode="list",length=max(DOY))
  subsetted=vector(mode="list",length=max(DOY))
  subsetted_resampled=vector(mode="list",length=max(DOY))
  aggregated=vector(mode="list",length=max(DOY))
  aggregated_cropped=vector(mode="list",length=max(DOY))
  aggregated_cropped_subsetted=vector(mode="list",length=max(DOY))
  subsetted_aggregated=list(NA)

  print("1. create empty lists done - loading tiffs")

  #### 2. Load Tiffs as raster and resample to given resolution ----
  setwd(paste(mypath,"/NDVI_Files/",sep=""))
  for (i in 1:length(NDVI_List)){#Tiffs als Raster abspeichern
    if(substr(NDVI_List[[i]],10,13)=="P4M1"){
      originals[[DOY[i]]]=raster::raster(NDVI_List[[i]])
      originals[[DOY[i]]]@data@names=as.character(DOY[i])
    } else if (substr(NDVI_List[[i]],10,13)=="Plan"){
      raster_help_red=raster::raster(NDVI_List[[i]],band = 6)
      raster_help_nir=raster::raster(NDVI_List[[i]],band = 8)
      raster_help_NDVI=(raster_help_nir-raster_help_red)/(raster_help_nir+raster_help_red)
      originals[[DOY[i]]]=raster_help_NDVI
      originals[[DOY[i]]]@data@names=as.character(DOY[i])
    }
  }

  print("2. Load tiff files as raster and resample to given resolution done - resampling raster...")

  #### 3. resample raster to site outlines, crop and mask ----
  for (i in 1:length(DOY)){#Croppen und maskieren der resampled Originale auf das Beregnungstransekt
    cropped[[DOY[i]]] = terra::crop(originals[[DOY[i]]],shape_site)
    subsetted[[DOY[i]]] = terra::mask(cropped[[DOY[i]]], shape_site)#NDVI aus Dateien, gecropt auf Beregnungsspur
  }

  subsetted_resampled=subsetted
  for(i in 1:(length(DOY)-1)){
    subsetted_resampled[[DOY[i+1]]]=terra::resample(subsetted_resampled[[DOY[i+1]]], subsetted_resampled[[DOY[i]]], method='bilinear')
  }

  print("3. resample raster to site outlines, crop and mask done - aggregating tiff files...")

  #### 4. rescale and aggregate Tiffs to first given Tiff ----
  for (i in 1:length(DOY)){
    aggregated[[DOY[i]]]=terra::aggregate(subsetted_resampled[[DOY[i]]],fact=target_res/raster::res(subsetted_resampled[[DOY[i]]]))
    aggregated_cropped[[DOY[i]]] = terra::crop(aggregated[[DOY[i]]],shape_site)
    aggregated_cropped_subsetted[[DOY[i]]] = terra::mask(aggregated_cropped[[DOY[i]]], shape_site)#NDVI aus Dateien, gecropt auf Beregnungsspur
    aggregated_cropped[[DOY[i]]]=NA
  }

  aggregated_cropped_subsetted_2=aggregated_cropped_subsetted

  print("4. rescale and aggregate Tiffs to first given Tiff done - correction of NDVI from UAV/satellite to Arable standard...")

  ### 5. Correction of NDVI from UAV or Planet to Arable standard ----
  for (i in 1:length(NDVI_List)){
    if(substr(NDVI_List[[i]],10,13)=="P4M1"){
      NDVI_source="UAV"

      #UAV
      if(method_NDVI=="uwdw" & modeltype=="linear"){
        aggregated_cropped_subsetted_2[[DOY[i]]]=0.72137*aggregated_cropped_subsetted[[DOY[i]]]+0.17137#Korrektur NDVI auf Arable-Standard, UAV und Rohwerte
      } else if (method_NDVI=="direct" & modeltype=="linear"){
        aggregated_cropped_subsetted_2[[DOY[i]]]=0.77585*aggregated_cropped_subsetted[[DOY[i]]]+0.14805#Korrektur NDVI auf Arable-Standard, UAV und direkte NDVI-Werte
      } else if(method_NDVI=="uwdw" & modeltype=="poly"){
        aggregated_cropped_subsetted_2[[DOY[i]]]=
          (2.065*aggregated_cropped_subsetted[[DOY[i]]]^3)-
          (3.7264*aggregated_cropped_subsetted[[DOY[i]]]^2)+
          (2.7397*aggregated_cropped_subsetted[[DOY[i]]])-
          0.1144#Korrektur NDVI auf Arable-Standard, UAV und direkte NDVI-Werte
      } else if (method_NDVI=="direct" & modeltype=="poly"){
        aggregated_cropped_subsetted_2[[DOY[i]]]=
          (1.76528*aggregated_cropped_subsetted[[DOY[i]]]^3)-
          (3.21885*aggregated_cropped_subsetted[[DOY[i]]]^2)+
          (2.48910*aggregated_cropped_subsetted[[DOY[i]]])-
          0.07796#Korrektur NDVI auf Arable-Standard, UAV und direkte NDVI-Werte
      }

    } else{
      NDVI_source="Planet"

      #Planet
      if (method_NDVI=="uwdw" & modeltype=="linear"){
        aggregated_cropped_subsetted_2[[DOY[i]]]=0.83632*aggregated_cropped_subsetted[[DOY[i]]]+0.10890#Korrektur NDVI auf Arable-Standard
      } else if (method_NDVI=="direct" & modeltype=="linear"){
        aggregated_cropped_subsetted_2[[DOY[i]]]=0.97736*aggregated_cropped_subsetted[[DOY[i]]]+0.01248#Korrektur NDVI auf Arable-Standard, UAV und direkte NDVI-Werte
      } else if(method_NDVI=="uwdw" & modeltype=="poly"){
        aggregated_cropped_subsetted_2[[DOY[i]]]=
          (2.8082*aggregated_cropped_subsetted[[DOY[i]]]^3)-
          (5.2500*aggregated_cropped_subsetted[[DOY[i]]]^2)+
          (3.9685*aggregated_cropped_subsetted[[DOY[i]]])-
          0.4851#Korrektur NDVI auf Arable-Standard, Planet und direkte NDVI-Werte
      } else if (method_NDVI=="direct" & modeltype=="poly"){
        aggregated_cropped_subsetted_2[[DOY[i]]]=
          (2.11710*aggregated_cropped_subsetted[[DOY[i]]]^3)-
          (4.15536*aggregated_cropped_subsetted[[DOY[i]]]^2)+
          (3.48386*aggregated_cropped_subsetted[[DOY[i]]])-
          0.42862#Korrektur NDVI auf Arable-Standard, Planet und direkte NDVI-Werte
      }
    }
  }

  print("5. Correction of NDVI from UAV or Planet to Arable standard done - interpolating cropped NDVI-sets and saving daily values...")

  #### 6. Interpolation of cropped NDVI-sets and saving of daily values ----
  aggregated_cropped_subsetted_3=unlist(aggregated_cropped_subsetted_2[[DOY[1]]])
  raster::values(aggregated_cropped_subsetted_3)=NA
  aggregated_cropped_subsetted_4 = sapply(1:max(DOY), function(...) aggregated_cropped_subsetted_3)

  for(i in 1:length(aggregated_cropped_subsetted_4)){
    if(is.element(i,DOY)==T){
      aggregated_cropped_subsetted_4[[i]]=raster::setValues(aggregated_cropped_subsetted_3,raster::values(aggregated_cropped_subsetted_2[[i]]))
    }
  }

  aggregated_cropped_subsetted_4[[last_NDVI_0]]=aggregated_cropped_subsetted_4[[DOY[1]]]
  for(i in 1:length(aggregated_cropped_subsetted_4[[last_NDVI_0]]@data@values)){
    if(is.na(aggregated_cropped_subsetted_4[[last_NDVI_0]]@data@values[i])==F){
      aggregated_cropped_subsetted_4[[last_NDVI_0]]@data@values[i]=0
    }
  }

  for(i in 1:(last_NDVI_0-1)){
    aggregated_cropped_subsetted_4[[i]]=aggregated_cropped_subsetted_4[[last_NDVI_0]]
  }

  for(i in 1:length(aggregated_cropped_subsetted_4)){
    aggregated_cropped_subsetted_4[[i]]@data@values[1]=1
    aggregated_cropped_subsetted_4[[i]]@data@values[2]=0
  }

  aggregated_cropped_subsetted_5 <- raster::stack(aggregated_cropped_subsetted_4)
  NDVI <- raster::approxNA(aggregated_cropped_subsetted_5)#NDVI Subset

  for(i in 1:DOY[length(DOY)]){
    NDVI[[i]]@data@values[1]=NA
    NDVI[[i]]@data@values[2]=NA
  }

  print("6. Interpolation of cropped NDVI-sets and saving of daily values done - derive reference ET...")

  #### 7. derive ET_ref ----
  #ET0 aus DWD-Daten

  if(length(ET_ref)!=1){
    ET_ref=ET_ref[,-1]
    ET_ref[,2]=lubridate::yday(ET_ref[,2])
    colnames(ET_ref)=c("ET0","DOY")
    ET0_3=ET_ref
  } else{

    if(ET_ref_dl=="DWD"){
      ET_ref=DownloadET0fromDWD(target_path=mypath,
                              test_site_shp=shape_site,
                              target_year=output_year)
      ET_ref[,2]=lubridate::yday(ET_ref[,2])
      colnames(ET_ref)=c("ET0","DOY")
      ET0_3=ET_ref
    } else if (ET_ref_dl=="Arable"){
      ET0_3=DownloadET0fromArable(user_name=arable_user,
                           pwd=arable_pass,
                           start_date=as.Date(paste(my_year,"-",min(DOY,na.rm=T),sep=""), '%Y-%j'),
                           end_date=as.Date(paste(my_year,"-",max(DOY,na.rm=T),sep=""), '%Y-%j'),
                           shape_site=shape_site)
    }
  }

  if (all(is.na(ET0_3[,1])) == T){
    stop("No reference ET data available from Arable for your desired timespan. Please choose ET_ref_dl=DWD for configuration.")
  }

  ET0_3[ET0_3[,1]=="NaN",1]=NA
  ET0_3[,1]=zoo::na.approx(ET0_3[,1],na.rm=F)

  print("7. derive ET_ref done - calculate precipitation...")

    #### 8.1 calculation of precipitation from FURUNO ----
    if(precip_source=="furuno"){
      WR_precip_List=list.files(path=path_WR_precip,pattern = "\\.shp$")
      WR_precip=vector(mode='list', length=366)#Liste mit Werten je Tag
      precipitation_daily=vector(mode='list', length=366)#Liste mit Werten je Tag
      for (i in 1:length(WR_precip)){
        WR_precip[[i]]=NA
        precipitation_daily[[i]]=NA
      }

      for (i in min(substr(WR_precip_List,23,25)):max(substr(WR_precip_List,23,25))){#i=137:237
        WR_precip[[i]]=try(raster::shapefile(paste(path_WR_precip,"/",WR_precip_List[substr(WR_precip_List,23,25)==formatC(i, width = 3, format = "d", flag = "0")],sep="")))
        if (class(WR_precip[[i]])!="SpatialPolygonsDataFrame"){
          WR_precip[[i]]=try(raster::shapefile(paste(path_WR_precip,"/",WR_precip_List[substr(WR_precip_List,23,25)==formatC(min(substr(WR_precip_List,23,25)), width = 3, format = "d", flag = "0")],sep="")))
          WR_precip[[i]]@data=data.frame(rep(0,nrow(WR_precip[[i]]@data)))
          names(WR_precip[[i]]@data)="layer"
        }
      }
    }

    #### 8.2 calculation of precipitation from Radolan ----

    if(precip_source=="radolan"){

      if(is.na(path_WR_precip)==T){
        DownloadRadolanFromDWD(target_path=paste(mypath,"/Radolan_",output_year,"_processed_daily/",sep=""),
                         start_date=min(as.POSIXct(strptime(substr(NDVI_List,1,8),"%Y%m%d" )),na.rm=T),
                         end_date=max(as.POSIXct(strptime(substr(NDVI_List,1,8),"%Y%m%d" )),na.rm=T),
                         target_site=shape_site)
        path_WR_precip=paste(mypath,"/Radolan_",output_year,"_processed_daily/",sep="")
      }

      WR_precip_List=list.files(path=path_WR_precip,pattern = "\\.shp$")
      WR_precip=vector(mode='list', length=366)#Liste mit Werten je Tag
      precipitation_daily=vector(mode='list', length=366)#Liste mit Werten je Tag
      for (i in 1:length(WR_precip)){
        WR_precip[[i]]=NA
        precipitation_daily[[i]]=NA
      }

      for (i in min(substr(WR_precip_List,14,16)):max(substr(WR_precip_List,14,16))){#i=137:237
        WR_precip[[i]]=try(raster::shapefile(paste(path_WR_precip,"/",WR_precip_List[substr(WR_precip_List,14,16)==formatC(i, width = 3, format = "d", flag = "0")],sep="")))
        if (class(WR_precip[[i]])!="SpatialPolygonsDataFrame"){
          WR_precip[[i]]=try(raster::shapefile(paste(path_WR_precip,"/",WR_precip_List[substr(WR_precip_List,14,16)==formatC(min(substr(WR_precip_List,14,16)), width = 3, format = "d", flag = "0")],sep="")))
          WR_precip[[i]]@data=data.frame(rep(0,nrow(WR_precip[[i]]@data)))
          names(WR_precip[[i]]@data)="layer"
        }
      }
    }

  print("8. calculation of precipitation done.")

  #### 9. start WB Model ----

  print("9. start WB Model for doy..")

  if (method_NDVI=="direct"){
    aspect_KC=1.4580841
    y_Kc=-0.1733250
  }
  if (method_NDVI=="uwdw"){
    #aspect_KC=1.5514
    #y_Kc=-0.2216
    aspect_KC=1.4535593
    y_Kc=-0.1686968
  }

  KC=vector(mode='list', length=max(DOY,na.rm=T))#Kc-Wert
  WB_daily=list(NA)
  ETC_ND_daily=list(NA)
  irrigation_daily=list(NA)#Beregnung
  ETC_daily=list(NA)
  irrigation=list(NA)
  aggregated_cropped_subsetted_13_BoFeu_rel=list(NA)
  aggregated_cropped_subsetted_13_BoFeu_abs=list(NA)

  for (i in min(DOY,na.rm=T):max(DOY,na.rm=T)){
    subs_help_DOY=subset(ET0_3,ET0_3$DOY==i)
    subs_beregnung=raster::subset(irrig_sf,irrig_sf$DOY==i)
    subs_beregnung2=raster::subset(irrig_sf,irrig_sf$DOY==(i+1))
    subs_beregnung0=raster::subset(irrig_sf,irrig_sf$DOY==(i-1))
    aggregated_cropped_subsetted_10=sf::st_as_sf(methods::as(NDVI[[i]],'SpatialPolygonsDataFrame'))
    aggregated_cropped_subsetted_10_ETC=sf::st_as_sf(methods::as(NDVI[[i]],'SpatialPolygonsDataFrame'))

    print(i)

    r_hr <- raster::raster(nrow=nrow(NDVI[[i]]), ncol=ncol(NDVI[[i]]))
    terra::crs(r_hr) <- terra::crs(NDVI[[i]])# utm
    r_hr@extent <- raster::extent(NDVI[[i]])
    rast_res <- terra::rasterize(WR_precip[[i]],r_hr,field="layer")
    if(precip_source=="furuno"){
      crop <- terra::crop(rast_res, NDVI[[i]],snap="out")
    }
    if(precip_source=="radolan"){
      crop <- terra::crop(rast_res, NDVI[[i]])
    }
    precipitation_daily[[i]] <- terra::mask(crop, NDVI[[i]])

    if(mean(WR_precip[[i]]@data$layer,na.rm=T)==0){#Wenn an diesem Tag kein Niederschlag erfolgt...
      aggregated_cropped_subsetted_9_help=((aspect_KC*(NDVI[[i]])+y_Kc)*subs_help_DOY[1,1])*(-1)#+BoFeu_Abs#ETC ohne Niederschlag
    } else{ aggregated_cropped_subsetted_9_help=precipitation_daily[[i]]+(((aspect_KC*(NDVI[[i]])+y_Kc)*subs_help_DOY[1,1])*(-1))#+BoFeu_Abs
    }

    aggregated_cropped_subsetted_10=sf::st_as_sf(methods::as(aggregated_cropped_subsetted_9_help,'SpatialPolygonsDataFrame'))
    colnames(aggregated_cropped_subsetted_10)[1]=paste("X139.",i,sep="")

    aggregated_cropped_subsetted_10_ETC[[1]]=((aspect_KC*(aggregated_cropped_subsetted_10_ETC[[1]])+y_Kc)*subs_help_DOY[1,1])*(-1)#nur ETC
    KC[[i]]=sf::st_as_sf(methods::as(NDVI[[i]],'SpatialPolygonsDataFrame'))
    KC[[i]][[1]]=aspect_KC*KC[[i]][[1]]+y_Kc
    WB_daily[[i]]=aggregated_cropped_subsetted_10#ETC + ND + (spaeter) Beregnung
    ETC_ND_daily[[i]]=aggregated_cropped_subsetted_10#ETC + ND (ohne Beregnung)
    ETC_daily[[i]]=aggregated_cropped_subsetted_10_ETC#ETC ohne Nd und Beregnung
    if(length(subs_beregnung$DOY)!=0){#Wenn an diesem Tag beregnet wird,...
      for (j in 1:nrow(aggregated_cropped_subsetted_10)){#...pr?fe f?r jedes Pixel, ob...
        if(length(sf::st_intersects(aggregated_cropped_subsetted_10[j,],sf::st_as_sf(subs_beregnung))[[1]])!=0){#...an diesem Tag f?r dieses Pixel eine Beregnung stattfindet
          if(length(sf::st_intersects(aggregated_cropped_subsetted_10[j,],sf::st_as_sf(subs_beregnung2))[[1]])!=0){#...und ob am darauffolgenden Tag eine Beregnung stattfindet.
            intersec=sf::st_intersects(aggregated_cropped_subsetted_10[j,],sf::st_as_sf(subs_beregnung))#Schnittpunkte des Pixels mit Beregnungsradien f?r diesen Tag
            intersec2=sf::st_intersects(aggregated_cropped_subsetted_10[j,],sf::st_as_sf(subs_beregnung2))#Schnittpunkte des Pixels mit den Beregnungsradien des darauffolgenden Tages
            aggregated_cropped_subsetted_10[j,][[1]]=aggregated_cropped_subsetted_10[j,][[1]]+mean(c(sf::st_as_sf(subs_beregnung)[intersec[[1]],]$Brg_GPS,sf::st_as_sf(subs_beregnung2)[intersec2[[1]],]$Brg_GPS),na.rm=T)*irrigation_efficiency#Berechne Mittlwert der Beregnung auf dieses Pixel von diesem und n?chsten Tag
          }else{#Wenn aber nur heute eine Beregnung stattfindet (nicht morgen)
            if(length(sf::st_intersects(aggregated_cropped_subsetted_10[j,],sf::st_as_sf(subs_beregnung0))[[1]])==0){#Pr?fe, ob gestern eine Beregnung stattfand
              intersec=sf::st_intersects(aggregated_cropped_subsetted_10[j,],sf::st_as_sf(subs_beregnung))#Schnittpunkte des Pixels mit Beregnungsradien von heute
              aggregated_cropped_subsetted_10[j,][[1]]=aggregated_cropped_subsetted_10[j,][[1]]+mean(sf::st_as_sf(subs_beregnung)[intersec[[1]],]$Brg_GPS,na.rm=T)*irrigation_efficiency#Berechne Beregnung auf dieses Pixel von heute
            }
          }
        }
      }

      WB_daily[[i]]=aggregated_cropped_subsetted_10#ETC + ND + Beregnung
      irrigation_daily[[i]]=WB_daily[[i]]
      irrigation_daily[[i]][[1]]=irrigation_daily[[i]][[1]] - ETC_ND_daily[[i]][[1]]#Beregnung
      ETC_daily[[i]]=aggregated_cropped_subsetted_10_ETC#ETC
    }
  }
  #return(ETC_daily)

  print("9 WB modelling done.")

  #Liste nur mit Beregnung erstellen
  irrigation_daily=WB_daily
  for (i in min(DOY,na.rm=T):max(DOY,na.rm=T)){
    irrigation_daily[[i]][[1]]=WB_daily[[i]][[1]] - ETC_ND_daily[[i]][[1]]#Beregnung
  }



  #### 10. cumulating daily values ----

  print("10. cumulating daily values to latest date...")

  WB_cumulated=WB_daily#ETC + ND + Beregnung
  ETC_ND_cumulated=ETC_ND_daily#ETC + ND
  irrigation_cumulated=irrigation_daily#Beregnung
  ETC_cumulated=ETC_daily#Nur ETC
  precipitation_cumulated=precipitation_daily
  for (i in (1+min(DOY,na.rm=T)):max(DOY,na.rm=T)){
    WB_cumulated[[i]][[1]]=WB_cumulated[[i]][[1]]+WB_cumulated[[i-1]][[1]]
    WB_cumulated[[i]]$doy=i
    ETC_ND_cumulated[[i]][[1]]=ETC_ND_cumulated[[i]][[1]]+ETC_ND_cumulated[[i-1]][[1]]
    ETC_ND_cumulated[[i]]$doy=i
    irrigation_cumulated[[i]][[1]]=irrigation_cumulated[[i]][[1]]+irrigation_cumulated[[i-1]][[1]]
    irrigation_cumulated[[i]]$doy=i
    ETC_cumulated[[i]][[1]]=ETC_cumulated[[i]][[1]]+ETC_cumulated[[i-1]][[1]]
    ETC_cumulated[[i]]$doy=i

    if(class(precipitation_cumulated[[i]])[1]!="RasterLayer"){
      precipitation_cumulated[[i]]=precipitation_cumulated[[min(DOY,na.rm=T)]]
    }

    precipitation_cumulated[[i]][[1]]=sum(precipitation_cumulated[[i]][[1]],precipitation_cumulated[[i-1]][[1]],na.rm=T)
    precipitation_cumulated[[i]][[1]]$doy=i
  }

  colnames(WB_daily[[i]])[1]="values"
  colnames(ETC_ND_daily[[i]])[1]="values"
  colnames(irrigation_daily[[i]])[1]="values"
  colnames(ETC_daily[[i]])[1]="values"
  colnames(WB_cumulated[[i]])[1]="values"
  colnames(ETC_ND_cumulated[[i]])[1]="values"
  colnames(irrigation_cumulated[[i]])[1]="values"
  colnames(ETC_cumulated[[i]])[1]="values"
  precipitation_cumulated[[i]][[1]]@data@names="values"
  colnames(KC[[i]])[1]="values"
  NDVI[[i]]@data@names="values"
  precipitation_daily[[i]]@data@names="values"

  print("10 cumulating daily values to latest date - done")

  #### 11. save as GeoTIFF ----

  print("11. saving as GeoTIFF...")

  if(save_geotiff==TRUE){
    #results_list_shp=list(NA)
    for (i in min(DOY,na.rm=T):max(DOY,na.rm=T)){

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_waterbalance"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_waterbalance"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_waterbalance"))
      stars::write_stars(stars::st_rasterize(WB_daily[[i]]), paste("Wasserbilanz_Subplot_DOY_",i,".tif",sep=""))

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_ETc_precip"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_ETc_precip"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_ETc_precip"))
      stars::write_stars(stars::st_rasterize(ETC_ND_daily[[i]]), paste("ETC_ND_Subplot_DOY_",i,".tif",sep=""))

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_Irrigation"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_Irrigation"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_Irrigation"))
      stars::write_stars(stars::st_rasterize(irrigation_daily[[i]]), paste("Irrigation_Subplot_DOY_",i,".tif",sep=""))

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_ETc"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_ETc"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_ETc"))
      stars::write_stars(stars::st_rasterize(ETC_daily[[i]]), paste("ETC_Subplot_DOY_",i,".tif",sep=""))

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_waterbalance_cumulated"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_waterbalance_cumulated"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_waterbalance_cumulated"))
      stars::write_stars(stars::st_rasterize(WB_cumulated[[i]]), paste("Wasserbilanz_Subplot_cumulated_DOY_",i,".tif",sep=""))

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_ETc_ND_cumulated"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_ETc_ND_cumulated"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_ETc_ND_cumulated"))
      stars::write_stars(stars::st_rasterize(ETC_ND_cumulated[[i]]), paste("ETC_ND_Subplot_cumulated_DOY_",i,".tif",sep=""))

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_Irrigation_cumulated"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_Irrigation_cumulated"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_Irrigation_cumulated"))
      stars::write_stars(stars::st_rasterize(irrigation_cumulated[[i]]), paste("Irrigation_Subplot_cumulated_DOY_",i,".tif",sep=""))

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_ETc_cumulated"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_ETc_cumulated"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_ETc_cumulated"))
      stars::write_stars(stars::st_rasterize(ETC_cumulated[[i]]), paste("ETC_Subplot_cumulated_DOY_",i,".tif",sep=""))

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_Kc"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_Kc"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_Kc"))
      stars::write_stars(stars::st_rasterize(KC[[i]]), paste("Kc_DOY_",i,".tif",sep=""))

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_precipitation_cumulated"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_precipitation_cumulated"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_precipitation_cumulated"))
      stars::write_stars(stars::st_rasterize(sf::st_as_sf(methods::as(precipitation_cumulated[[i]],"SpatialPolygonsDataFrame"))), paste("precipitation_cumulated_DOY_",i,".tif",sep=""))

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_NDVI"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_NDVI"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_NDVI"))
      stars::write_stars(stars::st_rasterize(sf::st_as_sf(methods::as(NDVI[[i]],"SpatialPolygonsDataFrame"))), paste("NDVI_DOY_",i,".tif",sep=""))

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_precipitation_daily"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_precipitation_daily"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "geotiff_precipitation_daily"))
      stars::write_stars(stars::st_rasterize(sf::st_as_sf(methods::as(precipitation_daily[[i]],"SpatialPolygonsDataFrame"))), paste("precipitation_DOY_",i,".tif",sep=""))
    }
  }

  print("11 saving as GeoTIFF - done")

  #### 12. Saving as Shapefile ----

  print("12. saving as Shapefile...")

  if(save_shape==TRUE){
    #results_list_shp=list(NA)

    for (i in min(DOY,na.rm=T):max(DOY,na.rm=T)){

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_waterbalance"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_waterbalance"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_waterbalance"))
      sf::st_write(WB_daily[[i]],paste("Wasserbilanz_Subplot_DOY_",i,".shp",sep=""),delete_layer=T,quiet = T)

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_ETc_precip"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_ETc_precip"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_ETc_precip"))
      sf::st_write(ETC_ND_daily[[i]],paste("ETC_ND_Subplot_DOY_",i,".shp",sep=""),delete_layer=T,quiet = T)

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_Irrigation"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_Irrigation"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_Irrigation"))
      sf::st_write(irrigation_daily[[i]],paste("Irrigation_Subplot_DOY_",i,".shp",sep=""),delete_layer=T,quiet = T)

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_ETc"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_ETc"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_ETc"))
      sf::st_write(ETC_daily[[i]],paste("ETC_Subplot_DOY_",i,".shp",sep=""),delete_layer=T,quiet = T)

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_waterbalance_cumulated"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_waterbalance_cumulated"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_waterbalance_cumulated"))
      sf::st_write(WB_cumulated[[i]],paste("Wasserbilanz_Subplot_cumulated_DOY_",i,".shp",sep=""),delete_layer=T,quiet = T)

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_ETc_ND_cumulated"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_ETc_ND_cumulated"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_ETc_ND_cumulated"))
      sf::st_write(ETC_ND_cumulated[[i]],paste("ETC_ND_Subplot_cumulated_DOY_",i,".shp",sep=""),delete_layer=T,quiet = T)

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_Irrigation_cumulated"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_Irrigation_cumulated"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_Irrigation_cumulated"))
      sf::st_write(irrigation_cumulated[[i]],paste("Irrigation_Subplot_cumulated_DOY_",i,".shp",sep=""),delete_layer=T,quiet = T)

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_ETc_cumulated"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_ETc_cumulated"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_ETc_cumulated"))
      sf::st_write(ETC_cumulated[[i]],paste("ETC_Subplot_cumulated_DOY_",i,".shp",sep=""),delete_layer=T,quiet = T)

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_Kc"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_Kc"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_Kc"))
      sf::st_write(KC[[i]],paste("Kc_DOY_",i,".shp",sep=""),delete_layer=T,quiet = T)

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_precipitation_cumulated"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_precipitation_cumulated"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_precipitation_cumulated"))
      sf::st_write(sf::st_as_sf(methods::as(precipitation_cumulated[[i]][[1]],'SpatialPolygonsDataFrame')),paste("precipitation_cumulated_DOY_",i,".shp",sep=""),delete_layer=T,quiet = T)

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_NDVI"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_NDVI"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_NDVI"))
      sf::st_write(sf::st_as_sf(methods::as(NDVI[[i]],'SpatialPolygonsDataFrame')),paste("NDVI_DOY_",i,".shp",sep=""),delete_layer=T,quiet = T)

      if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_precipitation_daily"))==F){
        dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_precipitation_daily"), showWarnings = FALSE, recursive=TRUE)
      }
      setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_"), "shp_precipitation_daily"))
      sf::st_write(sf::st_as_sf(methods::as(precipitation_daily[[i]],'SpatialPolygonsDataFrame')),paste("precipitation_DOY_",i,".shp",sep=""),delete_layer=T,quiet = T)
    }
  }

  print("12. saving as Shapefile done - saving .RDATA-file")

  if(save_RDATA==TRUE){

    if(dir.exists(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_")))==F){
      dir.create(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_")), showWarnings = FALSE, recursive=TRUE)
    }

    setwd(file.path(mypath,paste(method_NDVI,modeltype,precip_source,irrigation_efficiency,last_NDVI_0,target_res,sep="_")))

    save(DOY,
         NDVI,
         KC,
         ETC_daily,
         ETC_cumulated,
         precipitation_daily,
         precipitation_cumulated,
         ETC_ND_daily,
         ETC_ND_cumulated,
         irrigation_daily,
         irrigation_cumulated,
         WB_daily,
         WB_cumulated,
         ET0_3,
         file=paste("WBM_",method_NDVI,"_",precip_source,"_",modeltype,"_",as.character(irrigation_efficiency),"_",as.character(last_NDVI_0),"_",as.character(target_res),".RData",sep=""))
  }

  print("13. Saving .RData-file - done")

  end.time = Sys.time()
  print(paste("All results successfully created. Start Time: ",start.time,", End Time: ",end.time,sep=""))

}
