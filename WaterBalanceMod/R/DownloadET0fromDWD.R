#' Downloads and processes reference ET data from German Weather Service (DWD)
#' @param target_path Path to download and save csv-file with reference ET for your AOI and timespan of interest
#' @param test_site_shp Path to shapefile containing your AOI (string).
#' @param target_year year of interest (integer: 2021)
#' @param timeout time out span for downloading data (default: 10000, exceed, if your interconnection is slow)
#' @return chart containing reference evapotranspiration for every DOY during given timespan
#' @export

DownloadET0fromDWD=function(target_path=NA,
                          test_site_shp=NA,
                          target_year=NA,
                          timeout=1000){
  timeout_def=getOption('timeout')
  options(timeout=timeout)

  print("Download ET_ref data from German Weather Service (DWD)...")

  dir_ET_fao="https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/evaporation_fao/"
  if(dir.exists(file.path(target_path,"ET_fao"))==F){
    dir.create(file.path(target_path,"ET_fao"), showWarnings = FALSE, recursive=TRUE)
  }
  filename_fao=paste("grids_germany_daily_evaporation_fao_",target_year,"_v1.1.nc",sep="")
  tf_fao=paste(file.path(target_path,"ET_fao"),"/",filename_fao,sep="")
  try(utils::download.file(paste(dir_ET_fao,filename_fao,sep=""), tf_fao,mode = 'wb'))

  options(timeout=timeout_def)

  print("Download successfully finished. Transforming file format from *.nc to *.csv...")

  ### process downloaded ET0_fao to csv-file with daily values for certain site
  mypath=paste(target_path,"/ET_fao/",sep="")
  nc_List=list.files(path=mypath,pattern = "\\.nc$")
  nc_List=subset(nc_List,substr(nc_List,37,40)==target_year)

  rast_WR=list(NA)
  rast_WR_center=list(NA)
  rast_WR_center_mask=list(NA)
  cropped=list(NA)

  Varname_ET0=ifelse(class(try(as.data.frame(matrix(NA,raster::raster(paste(mypath,nc_List,sep=""),varname="et0")@file@nbands,2)),silent=T))!="try-error","et0","eta_fao")
  ET0=as.data.frame(matrix(NA,raster::raster(paste(mypath,nc_List,sep=""),varname=Varname_ET0)@file@nbands,2))

  ### daily ET0 FAO56
  for(i in 1:raster::raster(paste(mypath,nc_List,sep="")[1],varname=Varname_ET0)@file@nbands){
    rast_WR[[i]]=raster::raster(paste(mypath,nc_List,sep="")[1],varname=Varname_ET0,band=i)
    ET0[i,2]=as.character(rast_WR[[i]]@z[[1]])
    if(class(test_site_shp)[1]!="sf"){
      test_site_shp=sf::st_read(test_site_shp)
    }
    #rast_WR[[i]]=raster::projectRaster(rast_WR[[i]],crs=raster::crs(test_site_shp))
    #raster::crs(rast_WR[[i]])=raster::crs(test_site_shp)
    rast_WR[[i]]=raster::projectRaster(rast_WR[[i]],crs=terra::crs(paste("epsg:",substr(sf::st_crs(test_site_shp)[[2]],nchar(sf::st_crs(test_site_shp)[[2]])-6,nchar(sf::st_crs(test_site_shp)[[2]])-2),sep=""), describe=FALSE))
    terra::crs(rast_WR[[i]])=terra::crs(paste("epsg:",substr(sf::st_crs(test_site_shp)[[2]],nchar(sf::st_crs(test_site_shp)[[2]])-6,nchar(sf::st_crs(test_site_shp)[[2]])-2),sep=""), describe=FALSE)
    cropped[[i]]=terra::crop(rast_WR[[i]],test_site_shp)
    ET0[i,1]=mean(cropped[[i]]@data@values,na.rm=T)
    rast_WR[[i]]=NA
  }

  utils::write.csv(ET0,paste(file.path(target_path),"/DWD_ET0_",target_year,".csv",sep=""))
  unlink(mypath,recursive=TRUE)
  return(ET0)
  print("Fileformat successfully transformed and saved.")
}
