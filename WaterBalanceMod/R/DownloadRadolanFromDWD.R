#' Downloads and processes precipitation data from German Weather Service (DWD)
#' @param target_path Path to download and save shapefile for every DOY within timespan of interest
#' @param target_site Path to shapefile containing your AOI (string).
#' @param start_date start date of interest (e.g.: "2021-01-01"). If empty, default is 1st Jan of recent year.
#' @param end_date end date of interest (e.g.: "2021-12-31"). If empty, default is yesterday.
#' @return Shapefiles for every DOY containing precipitation data for your AOI.
#' @export

DownloadRadolanFromDWD=function(target_path = NA,
                          target_site=NA,
                          start_date=NA,
                          end_date=NA){
  options(rdwdquiet=TRUE)
  options(timeout = 3000)

  if (is.na(start_date)==T){
  start_date=paste(substr(Sys.Date(),1,4),"-01","-01",sep="")
  }

  if (is.na(end_date)==T){
  end_date=Sys.Date()-1
  }

  if(dir.exists(file.path(target_path))==F){
    dir.create(file.path(target_path), showWarnings = FALSE, recursive=TRUE)
  }

  if(substr(start_date,1,4)==substr(Sys.Date(),1,4) & substr(end_date,1,4)==substr(Sys.Date(),1,4)){
    m=50
    for (i in as.numeric(substr(start_date,3,4)):as.numeric(substr(end_date,3,4))){
      for (j in as.numeric(substr(start_date,6,7)):as.numeric(substr(end_date,6,7))){
        for(k in 1:as.numeric(lubridate::days_in_month(as.Date(paste(as.numeric(substr(start_date,1,2)),"-",formatC(j, width = 2, format = "d", flag = "0"),"-01",sep=""))))){
          for (l in 23){
            filename=paste("raa01-sf_10000-",
                           i,
                           formatC(j, width = 2, format = "d", flag = "0"),
                           formatC(k, width = 2, format = "d", flag = "0"),
                           formatC(l, width = 2, format = "d", flag = "0"),
                           m,
                           "-dwd---bin.gz",
                           sep="")
            tf=paste(target_path,filename,sep="")
            try(utils::download.file(paste("https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/radolan/recent/bin/",filename,sep=""), tf))
            #read_dwd_raster=rdwd::projectRasterDWD(rdwd::readDWD(tf)$dat,targetproj=terra::crs(target_site))
            read_dwd_raster=rdwd::projectRasterDWD(rdwd::readDWD(tf)$dat,targetproj=terra::crs(paste("epsg:",substr(sf::st_crs(target_site)[[2]],nchar(sf::st_crs(target_site)[[2]])-6,nchar(sf::st_crs(target_site)[[2]])-2),sep=""), describe=FALSE))
            read_dwd_raster=terra::crop(read_dwd_raster,target_site,snap="out")

            if(sum(dim(read_dwd_raster))!=3){
              read_dwd_raster=terra::aggregate(read_dwd_raster)
            }

            doy=formatC(lubridate::yday(paste(substr(start_date,1,4),"-",
                                              formatC(j, width = 2, format = "d", flag = "0"),"-",
                                              formatC(k, width = 2, format = "d", flag = "0"),sep="")), width = 3, format = "d", flag = "0")
            sf::st_write(sf::st_as_sf(methods::as(raster::raster(read_dwd_raster),'SpatialPolygonsDataFrame')),paste(target_path,"Radolan_",substr(start_date,1,4),"_",doy,".shp",sep=""),delete_layer=T)
            file.remove(tf)
            file.remove(substr(tf,1,(nchar(tf)-3)))
          }
        }
      }
    }

    } else if(substr(start_date,1,4)<=substr(Sys.Date(),1,4) & substr(end_date,1,4)<=substr(Sys.Date(),1,4) & substr(start_date,1,4)==substr(end_date,1,4) & RCurl::url.exists(paste("https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/radolan/historical/bin/",as.numeric(substr(start_date,1,4)),"/",sep=""))==TRUE){
        for (i in as.numeric(substr(start_date,1,4))){
          for (j in as.numeric(substr(start_date,6,7)):as.numeric(substr(end_date,6,7))){
                filename=paste("SF",
                               i,
                               formatC(j, width = 2, format = "d", flag = "0"),
                               ".tar.gz",
                               sep="")
                tf=paste(target_path,filename,sep="")
                try(utils::download.file(paste("https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/radolan/historical/bin/",as.numeric(substr(start_date,1,4)),"/",filename,sep=""), tf))
                #read_dwd_raster=rdwd::projectRasterDWD(rdwd::readDWD(tf)$dat,targetproj=terra::crs(target_site))
                read_dwd_raster=rdwd::projectRasterDWD(rdwd::readDWD(tf)$dat,targetproj=terra::crs(paste("epsg:",substr(sf::st_crs(target_site)[[2]],nchar(sf::st_crs(target_site)[[2]])-6,nchar(sf::st_crs(target_site)[[2]])-2),sep=""), describe=FALSE))
                read_dwd_raster=terra::crop(read_dwd_raster,target_site,snap="out")

                if(sum(dim(read_dwd_raster))!=3){
                  read_dwd_raster=terra::aggregate(read_dwd_raster)
                }
                for (k in 1:terra::nlyr(read_dwd_raster)){
                  ref_date=paste(substr(names(read_dwd_raster)[k],1,4),"-",
                                 substr(names(read_dwd_raster)[k],6,7),"-",
                                 substr(names(read_dwd_raster)[k],9,10),sep="")

                  if(substr(names(read_dwd_raster)[k],12,13)==23 & start_date <= ref_date & end_date >= ref_date){
                    doy=formatC(lubridate::yday(ref_date), width = 3, format = "d", flag = "0")
                    daily_values=sf::st_as_sf(methods::as(raster::raster(read_dwd_raster[[k]]),'SpatialPolygonsDataFrame'))
                    colnames(daily_values)[1]="layer"
                    sf::st_write(daily_values,paste(target_path,"Radolan_",substr(start_date,1,4),"_",doy,".shp",sep=""),delete_layer=T)
                    file.remove(tf)
                    unlink(paste(target_path,substr(filename,1,8),sep=""), recursive=TRUE)
                }
          }
          }
      }

    } else if(substr(start_date,1,4)<=substr(Sys.Date(),1,4) & substr(end_date,1,4)<=substr(Sys.Date(),1,4) & substr(start_date,1,4)==substr(end_date,1,4) & RCurl::url.exists(paste("https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/radolan/historical/bin/",as.numeric(substr(start_date,1,4)),"/",sep=""))==FALSE){
      m=50
      for (i in as.numeric(substr(start_date,3,4)):as.numeric(substr(end_date,3,4))){
        for (j in as.numeric(substr(start_date,6,7)):as.numeric(substr(end_date,6,7))){
          for(k in 1:as.numeric(lubridate::days_in_month(as.Date(paste(as.numeric(substr(start_date,1,2)),"-",formatC(j, width = 2, format = "d", flag = "0"),"-01",sep=""))))){
            for (l in 23){
              filename=paste("raa01-sf_10000-",
                             i,
                             formatC(j, width = 2, format = "d", flag = "0"),
                             formatC(k, width = 2, format = "d", flag = "0"),
                             formatC(l, width = 2, format = "d", flag = "0"),
                             m,
                             "-dwd---bin.gz",
                             sep="")
              tf=paste(target_path,filename,sep="")
              try(utils::download.file(paste("https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/radolan/recent/bin/",filename,sep=""), tf))
              #read_dwd_raster=rdwd::projectRasterDWD(rdwd::readDWD(tf)$dat,targetproj=terra::crs(target_site))
              read_dwd_raster=rdwd::projectRasterDWD(rdwd::readDWD(tf)$dat,targetproj=terra::crs(paste("epsg:",substr(sf::st_crs(target_site)[[2]],nchar(sf::st_crs(target_site)[[2]])-6,nchar(sf::st_crs(target_site)[[2]])-2),sep=""), describe=FALSE))
              read_dwd_raster=terra::crop(read_dwd_raster,target_site,snap="out")

              if(sum(dim(read_dwd_raster))!=3){
                read_dwd_raster=terra::aggregate(read_dwd_raster)*10
              }

              doy=formatC(lubridate::yday(paste(substr(start_date,1,4),"-",
                                                formatC(j, width = 2, format = "d", flag = "0"),"-",
                                                formatC(k, width = 2, format = "d", flag = "0"),sep="")), width = 3, format = "d", flag = "0")
              sf::st_write(sf::st_as_sf(methods::as(raster::raster(read_dwd_raster),'SpatialPolygonsDataFrame')),paste(target_path,"Radolan_",substr(start_date,1,4),"_",doy,".shp",sep=""),delete_layer=T)
              file.remove(tf)
              file.remove(substr(tf,1,(nchar(tf)-3)))
            }
          }
        }
      }
      }else(print("Start year and end year have to be equal."))
}
