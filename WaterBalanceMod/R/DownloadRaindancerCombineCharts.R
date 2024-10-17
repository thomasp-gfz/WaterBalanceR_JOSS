#' Processes downloaded csv-files from using DownloadRaindancer() to a shapefile. The resulting shapefile is being updated every time, this script is being run.
#' @param sourcepath Path (string) to Firefox download folder. Look it up in your Firefox browser.
#' @param targetpath Path (string) to destination folder for downloaded csv-files from Raindancer.
#' @param start_date You need to define a start date (default: 1st Jan of recent year)
#' @param buffer_dist spray radius of sprinkler in meter (integer), default is 36.
#' @return A shapefile, that contains all irrigation events, that were download. The shapefile is being opdated every time this script is being run, as long as all configuration parameter stay the same.
#' @export

### 1. Einlesen der Korrdinatenprotokolle ----
DownloadRaindancerCombineCharts=function(sourcepath=NA,
                                         targetpath=NA,
                                         start_date=paste(substr(Sys.Date(),1,4),"-01-01",sep=""),
                                         buffer_dist=36){

date_folders=dir(sourcepath)
irrigation_chart_file_path=system.file("extdata", "irrigation_tab.csv", package="WaterBalanceMod", mustWork=TRUE)
irrigation_chart=utils::read.csv(irrigation_chart_file_path,header=T,sep=",")
rownames(irrigation_chart)=irrigation_chart[,1]
irrigation_chart=irrigation_chart[,-1]
colnames(irrigation_chart)=as.character(c(10:60))

my_files_application_protocol=dir(paste(sourcepath,max(date_folders),sep=""))
application_protocol_file=my_files_application_protocol[grepl("Einsatzprotokoll",my_files_application_protocol)]

coord_tab_all=list(NA)
for (j in 1:length(date_folders)){
  my_files=dir(paste(sourcepath,date_folders[j],sep=""))

  coord_protocols=my_files[grepl("Koordinatenprotokoll",my_files)]
  coord_protocols_tab=as.data.frame(matrix(NA,length(coord_protocols),3))
  names(coord_protocols_tab)=c("Protocol","Sprinkler Name","Date_Time")

  coord_protocols_tab[,1]=substr(coord_protocols,1,20)
  coord_protocols_tab[,2]=substr(coord_protocols,22,nchar(coord_protocols)-23)
  coord_protocols_tab[,3]=as.POSIXct(strptime(substr(coord_protocols,nchar(coord_protocols)-21,nchar(coord_protocols)-5),"%Y-%m-%d_%H%M%S" ))

  coord_tab=list(NA)
  print("Loading Excel Files...")
  for(i in 1:nrow(coord_protocols_tab)){ #i = sprinkler
    coord_tab_temp=readxl::read_excel(paste(sourcepath,date_folders[j],"/",coord_protocols[i],sep=""),.name_repair = "unique_quiet")
    coord_tab_temp=readxl::read_excel(paste(sourcepath,date_folders[j],"/",coord_protocols[i],sep=""),.name_repair = "unique_quiet")
    coord_tab_temp$sprinkler=coord_protocols_tab[i,2]
    coord_tab_temp$date_time=coord_protocols_tab[i,3]
    coord_tab_temp$site=NA
    coord_tab_temp=subset(coord_tab_temp,coord_tab_temp$Status!="Regner hat keinen Einsatz")
    coord_tab_temp=subset(coord_tab_temp,coord_tab_temp$Status!="Regner sendet nicht mehr")
    coord_tab_temp=subset(coord_tab_temp,coord_tab_temp$bar>=1)
    coord_tab_temp=subset(coord_tab_temp,coord_tab_temp$Zeitpunkt>=start_date)

    application_protocol=readxl::read_excel(paste(sourcepath,max(date_folders),"/",application_protocol_file[i],sep=""),.name_repair = "unique_quiet")

    for(k in 1:nrow(coord_tab_temp)){#k = coordinate table
      for(l in 1:nrow(application_protocol)){#l = application protocol
        if(nrow(application_protocol)>0){#only calculate, when there is at least one irrigation event
          if(is.na(application_protocol$Endzeit[l])==T){#If latest irrigation has not finished yet, complete chart with recent time
            application_protocol$Endzeit[l]=Sys.time()
          }
          if(nrow(coord_tab_temp)>0){
            if(coord_tab_temp$Zeitpunkt[k]<=(application_protocol$Endzeit[l]+480) & coord_tab_temp$Zeitpunkt[k]>=(application_protocol$Startzeit[l]-480)){
              coord_tab_temp$site[k]=application_protocol$`Schlag/Gasse`[l]
            }
          }
        }
      }
    }
    coord_tab[[i]]=coord_tab_temp
  }
  coord_tab_all[[j]]=coord_tab
}

coord_tab_all_bound=as.data.frame(do.call(rbind.data.frame,do.call(c, coord_tab_all)))
coord_tab_all_bound_distinct=dplyr::distinct(coord_tab_all_bound,
                                      coord_tab_all_bound$Breitengrad,
                                      coord_tab_all_bound$Längengrad,
                                      coord_tab_all_bound$Zeitpunkt,
                                      coord_tab_all_bound$sprinkler,
                                      coord_tab_all_bound$site,
                                      coord_tab_all_bound$bar,
                                      coord_tab_all_bound$Status)

colnames(coord_tab_all_bound_distinct)=c("Breitengrad",
                                         "Laengengrad",
                                         "Zeitpunkt",
                                         "Sprinkler",
                                         "Site",
                                         "Bar",
                                         "Status")

coord_tab_all_bound_distinct=na.omit(coord_tab_all_bound_distinct)

unique_sites=unique(coord_tab_all_bound_distinct$Site)

site_irrigation=list(NA)
for (i in 1:length(unique_sites)){
  site_irrigation[[i]]=subset(coord_tab_all_bound_distinct,coord_tab_all_bound_distinct$Site==unique_sites[i])
}
names(site_irrigation)=unique_sites
save(site_irrigation,file=paste(targetpath,substr(Sys.time(),1,10),"_irrigation_by_site_",start_date,".RData",sep=""))

##################################
site_irrigation_2=site_irrigation
site_irrigation_2_new=list(NA)
site_irrigation_2_orig=list(NA)
site_irrigation_2_new_interp=list(NA)
site_irrigation_2_orig_interp=list(NA)
site_irrigation_2_orig_na_omit=list(NA)
site_irrigation_2_new_interp_na_omit=list(NA)

csv_list=list(NA)
csv_list_new=list(NA)
csv_list_new_orig=list(NA)
csv_list_new_interp=list(NA)
csv_list_new_interp_na_omit=list(NA)
csv_list_new_interp_na_omit_shapes_32633=list(NA)
csv_list_new_orig_shapes_32633=list(NA)

Buffer_36m=list(NA)
Buffer_36m_orig=list(NA)
Buffer_36m_all_shp=list(NA)
Buffer_36m_orig_all_shp=list(NA)

for (i in 1:length(site_irrigation)){
  site_irrigation_2[[i]]$entfernung=NA
  site_irrigation_2[[i]]$entfernung=NA
  site_irrigation_2[[i]]$entfernung=NA

  #turn sequence in chart upside-down
  site_irrigation_2[[i]]=site_irrigation_2[[i]][nrow(site_irrigation_2[[i]]):1,]

  #replace Comma by point
  site_irrigation_2[[i]]$Laengengrad=gsub(",", ".", site_irrigation_2[[i]]$Laengengrad)#Komma durch Punkt ersetzen
  site_irrigation_2[[i]]$Breitengrad=gsub(",", ".", site_irrigation_2[[i]]$Breitengrad)#Komma durch Punkt ersetzen

  #floating average of coordinates, pressure, date and time (filter)
  site_irrigation_2[[i]]$Laengengrad_mean=NA
  site_irrigation_2[[i]]$Breitengrad_mean=NA
  site_irrigation_2[[i]]$Druck_mean=NA
  site_irrigation_2[[i]]$Datum_Uhrzeit_mean=NA

  if (nrow(site_irrigation_2[[i]])>=9){
  j=5
  j_min=j-4
  j_max=j+4#5
    for (k in 1:floor(nrow(site_irrigation_2[[i]])/5)){
      site_irrigation_2[[i]]$Laengengrad_mean[j]=mean(as.numeric(site_irrigation_2[[i]]$Laengengrad[j_min:j_max]),na.rm=T)
      site_irrigation_2[[i]]$Breitengrad_mean[j]=mean(as.numeric(site_irrigation_2[[i]]$Breitengrad[j_min:j_max]),na.rm=T)
      site_irrigation_2[[i]]$Druck_mean[j]=mean(as.numeric(site_irrigation_2[[i]]$Bar[j_min:j_max]),na.rm=T)
      site_irrigation_2[[i]]$Datum_Uhrzeit_mean[j]=as.character(mean(site_irrigation_2[[i]]$Zeitpunkt[j_min:j_max],na.rm=T))
      site_irrigation_2[[i]]$Datum_Uhrzeit_mean=as.POSIXct(strptime(site_irrigation_2[[i]]$Datum_Uhrzeit_mean,"%Y-%m-%d %H:%M:%S" ))
      j=j+5
      j_min=j-4
      j_max=j+4#5
  }
  site_irrigation_2[[i]]$Datum_Uhrzeit_mean=as.POSIXct(strptime(site_irrigation_2[[i]]$Datum_Uhrzeit_mean,"%Y-%m-%d %H:%M:%S" ))

  #write filtered values in new matrix
  site_irrigation_2_new[[i]]=na.omit(site_irrigation_2[[i]][,9:12])
  site_irrigation_2_orig[[i]]=site_irrigation_2[[i]][,1:7]

  #calculate time difference in hours
  site_irrigation_2_new[[i]]$timedif=NA
  for (j in 1:(nrow(site_irrigation_2_new[[i]])-1)){
    site_irrigation_2_new[[i]]$timedif[j+1]=(site_irrigation_2_new[[i]]$Datum_Uhrzeit_mean[j+1]-site_irrigation_2_new[[i]]$Datum_Uhrzeit_mean[j])/60
  }

  site_irrigation_2_orig[[i]]$timedif=NA
  for (j in 1:(nrow(site_irrigation_2_orig[[i]])-1)){
    site_irrigation_2_orig[[i]]$timedif[j+1]=(site_irrigation_2_orig[[i]]$Zeitpunkt[j+1]-site_irrigation_2_orig[[i]]$Zeitpunkt[j])/60
  }

  site_irrigation_2_new[[i]]$dist_gps=NA
  for (j in 1:(nrow(site_irrigation_2_new[[i]])-1)){
    site_irrigation_2_new[[i]]$dist_gps[j+1]=geosphere::distm(c(site_irrigation_2_new[[i]]$Laengengrad[j], site_irrigation_2_new[[i]]$Breitengrad[j]),c(site_irrigation_2_new[[i]]$Laengengrad[j+1], site_irrigation_2_new[[i]]$Breitengrad[j+1]), fun = distGeo)
  }
  site_irrigation_2_new[[i]]$geschwindigkeit_gps=site_irrigation_2_new[[i]]$dist_gps/site_irrigation_2_new[[i]]$timedif

  site_irrigation_2_orig[[i]]$dist_gps=NA #for original Data
  for (j in 1:(nrow(site_irrigation_2_orig[[i]])-1)){
    site_irrigation_2_orig[[i]]$dist_gps[j+1]=geosphere::distm(c(site_irrigation_2_orig[[i]]$Laengengrad[j], site_irrigation_2_orig[[i]]$Breitengrad[j]),c(site_irrigation_2_orig[[i]]$Laengengrad[j+1], site_irrigation_2_orig[[i]]$Breitengrad[j+1]), fun = distGeo)
  }
  site_irrigation_2_orig[[i]]$geschwindigkeit_gps=site_irrigation_2_orig[[i]]$dist_gps/site_irrigation_2_orig[[i]]$timedif

  #Calculate irrigation amount from pressure and speed
  site_irrigation_2_new[[i]]$Beregnungshoehe_GPS=NA #for interpolated data
  for (j in 1:nrow(site_irrigation_2_new[[i]])){
    if (is.na(site_irrigation_2_new[[i]]$geschwindigkeit_gps[j])==F & site_irrigation_2_new[[i]]$geschwindigkeit_gps[j]>=10 & as.numeric(site_irrigation_2_new[[i]]$Druck_mean[j])>=3.5){
      geschwindigkeit_gerundet=round(site_irrigation_2_new[[i]]$geschwindigkeit_gps[j])
      druck=round(as.numeric(site_irrigation_2_new[[i]]$Druck_mean[j]),1)
      help_beregnung=as.numeric(irrigation_chart[druck==as.numeric(rownames(irrigation_chart)),geschwindigkeit_gerundet==as.numeric(colnames(irrigation_chart))])
      site_irrigation_2_new[[i]]$Beregnungshoehe_GPS[j]=ifelse(length(help_beregnung)==1,help_beregnung,NA)
    }}

  site_irrigation_2_orig[[i]]$Beregnungshoehe_GPS=NA #for original Data
  for (j in 1:nrow(site_irrigation_2_orig[[i]])){
    if (is.na(site_irrigation_2_orig[[i]]$geschwindigkeit_gps[j])==F){
      if(site_irrigation_2_orig[[i]]$geschwindigkeit_gps[j]>=10 &
         site_irrigation_2_orig[[i]]$geschwindigkeit_gps[j]<=60 &
         site_irrigation_2_orig[[i]]$geschwindigkeit_gps[j]!=Inf &
         as.numeric(site_irrigation_2_orig[[i]]$Bar[j])>=3.5){
        geschwindigkeit_gerundet=round(site_irrigation_2_orig[[i]]$geschwindigkeit_gps[j])
        druck=round(as.numeric(site_irrigation_2_orig[[i]]$Bar[j]),1)
        help_beregnung=as.numeric(irrigation_chart[druck==as.numeric(rownames(irrigation_chart)),geschwindigkeit_gerundet==as.numeric(colnames(irrigation_chart))])
        site_irrigation_2_orig[[i]]$Beregnungshoehe_GPS[j]=ifelse(length(help_beregnung)==1,help_beregnung,NA)
        }
      }
  }

  #cumulate distance
  site_irrigation_2_new[[i]]$Distanz_kum=NA #for interpolated data
  site_irrigation_2_new[[i]]$Distanz_kum[2]=site_irrigation_2_new[[i]]$dist_gps[2]
  for (j in 3:nrow(site_irrigation_2_new[[i]])){
    site_irrigation_2_new[[i]]$Distanz_kum[j]=site_irrigation_2_new[[i]]$Distanz_kum[j-1]+site_irrigation_2_new[[i]]$dist_gps[j]
  }

  site_irrigation_2_orig[[i]]$Distanz_kum=NA #for original Data
  site_irrigation_2_orig[[i]]$Distanz_kum[2]=site_irrigation_2_orig[[i]]$dist_gps[2]
  for (j in 3:nrow(site_irrigation_2_orig[[i]])){
    site_irrigation_2_orig[[i]]$Distanz_kum[j]=site_irrigation_2_orig[[i]]$Distanz_kum[j-1]+site_irrigation_2_orig[[i]]$dist_gps[j]
  }

  #Interpolation
  site_irrigation_2_new_interp[[i]]=as.data.frame(matrix(NA,nrow(site_irrigation_2_new[[i]])*5,ncol(site_irrigation_2_new[[i]])))
  colnames(site_irrigation_2_new_interp[[i]])=colnames(site_irrigation_2_new[[i]])
  j=1
  for (k in 1:nrow(site_irrigation_2_new[[i]])){
    site_irrigation_2_new_interp[[i]][j,]=site_irrigation_2_new[[i]][k,]
    j=j+5
  }

  #no-NA-Method
  m=6
  n=11
  for (k in 1:nrow(site_irrigation_2_new_interp[[i]])){
    for (j in 1:ncol(site_irrigation_2_new_interp[[i]])){
      if (is.na(site_irrigation_2_new_interp[[i]][m,j])==F & is.na(site_irrigation_2_new_interp[[i]][n,j])==F){
        site_irrigation_2_new_interp[[i]][m:n,j]=approx(site_irrigation_2_new_interp[[i]][m:n,j],n=6)$y
      }}
    m=m+5
    n=n+5
  }

  #write date and time as vector
  site_irrigation_2_new_interp[[i]]$Datum_Uhrzeit_mean=as.POSIXct(site_irrigation_2_new_interp[[i]]$Datum_Uhrzeit_mean,origin = '1970-01-01')

  #discard NAs from interpolated data
  site_irrigation_2_new_interp_na_omit[[i]]=na.omit(site_irrigation_2_new_interp[[i]])

  #reproject shapefiles and save as list - for interpolated data
  csv_list_new_interp_na_omit_shapes_32633[[i]]=sf::st_as_sf(site_irrigation_2_new_interp_na_omit[[i]], coords = c("Laengengrad_mean", "Breitengrad_mean"), crs = 4326)
  sf::st_crs(csv_list_new_interp_na_omit_shapes_32633[[i]])=4326
  csv_list_new_interp_na_omit_shapes_32633[[i]]=sf::st_transform(csv_list_new_interp_na_omit_shapes_32633[[i]],32633)

  #for original data
  site_irrigation_2_orig_na_omit[[i]]=subset(site_irrigation_2_orig[[i]],is.na(site_irrigation_2_orig[[i]]$Laengengrad)==F & is.na(site_irrigation_2_orig[[i]]$Breitengrad)==F &site_irrigation_2_orig[[i]]$Laengengrad != "" & site_irrigation_2_orig[[i]]$Breitengrad != "")
  csv_list_new_orig_shapes_32633[[i]]=sf::st_as_sf(site_irrigation_2_orig_na_omit[[i]], coords = c("Laengengrad", "Breitengrad"), crs = 4326)
  sf::st_crs(csv_list_new_orig_shapes_32633[[i]])=4326
  csv_list_new_orig_shapes_32633[[i]]=sf::st_transform(csv_list_new_orig_shapes_32633[[i]],32633)

  #creat buffer - interpolated data
  Buffer_36m[[i]]=csv_list_new_interp_na_omit_shapes_32633[[i]]
  Buffer_36m[[i]]=sf::st_buffer(Buffer_36m[[i]], dist = buffer_dist)
  Buffer_36m[[i]]$DOY=lubridate::yday(as.POSIXct(strptime(Buffer_36m[[i]]$Datum_Uhrzeit_mean,"%Y-%m-%d %H:%M:%S" )))

  for (j in 1:length(Buffer_36m)){
    if(is.null(Buffer_36m[[j]])==F){
      if(nrow(Buffer_36m[[j]])>0){
      Buffer_36m[[j]]$Schlag=names(site_irrigation_2)[[i]]
      }
    }
  }

  #create buffer - original data
  Buffer_36m_orig[[i]]=csv_list_new_orig_shapes_32633[[i]]
  Buffer_36m_orig[[i]]=sf::st_buffer(Buffer_36m_orig[[i]], dist = buffer_dist)
  Buffer_36m_orig[[i]]$DOY=lubridate::yday(as.POSIXct(strptime(Buffer_36m_orig[[i]]$Zeitpunkt,"%Y-%m-%d %H:%M:%S" )))

  for (j in 1:length(Buffer_36m_orig)){
    if(is.null(Buffer_36m_orig[[j]])==F){
      if(nrow(Buffer_36m_orig[[j]])>0){
        Buffer_36m_orig[[j]]$Schlag=names(site_irrigation_2)[[i]]
      }
    }
  }

    if(nrow(Buffer_36m[[i]])>0){
    Buffer_36m_all_shp[[i]]=as(Buffer_36m[[i]], 'Spatial')
    }

    if(nrow(Buffer_36m_orig[[i]])>0){
    Buffer_36m_orig_all_shp[[i]]=as(Buffer_36m_orig[[i]], 'Spatial')
    }
  }
}

#merge all single list entries of plots to one SPDF
Buffer_36m_all_shp_2=Buffer_36m_all_shp
k=1
for (i in 1:length(Buffer_36m_all_shp_2)){
  if(is.null(Buffer_36m_all_shp_2[[k]])){
    Buffer_36m_all_shp_2=Buffer_36m_all_shp_2[-k]
    k=k-1
  }
  k=k+1
}
Buffer_36m_all_shp_2=do.call(rbind,Buffer_36m_all_shp_2)

Buffer_36m_orig_all_shp_2=Buffer_36m_orig_all_shp
k=1
for (i in 1:length(Buffer_36m_orig_all_shp_2)){
  if(is.null(Buffer_36m_orig_all_shp_2[[k]])){
    Buffer_36m_orig_all_shp_2=Buffer_36m_orig_all_shp_2[-k]
    k=k-1
  }
  k=k+1
}
Buffer_36m_orig_all_shp_2=do.call(rbind,Buffer_36m_orig_all_shp_2)

#export as shapefiles
if(dir.exists(file.path(paste(targetpath,sep="")))==F){
  dir.create(file.path(paste(targetpath,sep="")), showWarnings = FALSE, recursive=TRUE)
}

setwd(paste(targetpath,sep=""))
suppressWarnings(sf::st_write(st_as_sf(Buffer_36m_orig_all_shp_2), paste(targetpath,"Buffer_36m_all_orig.shp",sep=""), driver = 'ESRI Shapefile', layer = 'Buffer_36m_all_orig', overwrite_layer = T,append=FALSE))
suppressWarnings(sf::st_write(st_as_sf(Buffer_36m_all_shp_2), paste(targetpath,"Buffer_36m_all_interp.shp",sep=""), driver = 'ESRI Shapefile', layer = 'Buffer_36m_all_interp', overwrite_layer = T,append=FALSE))

}






