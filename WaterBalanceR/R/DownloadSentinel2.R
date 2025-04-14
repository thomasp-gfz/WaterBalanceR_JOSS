# SPDX-FileCopyrightText: 2025 GFZ Helmholtz Centre for Geosciences
# SPDX-FileCopyrightText: 2025 Thomas Piernicke <thomasp@gfz.de>
# SPDX-License-Identifier: AGPL-3.0-only

#' Processes downloaded csv-files from using DownloadRaindancer() to a shapefile. The resulting shapefile is being updated every time, this script is being run.
#' @param target_path Path (string) to destination folder for downloaded csv-files from Raindancer.
#' @param shape_site Path (string) to shapefile of AOI
#' @param start_date You need to define a start date
#' @param end_date You need to define an end date
#' @param limit Limiter (int) for number of entrys in resulting table
#' @param cloudcover Cloud cover (int) as percent of maximum cloud coverage above AOI
#' @return A shapefile, that contains all irrigation events, that were download. The shapefile is being opdated every time this script is being run, as long as all configuration parameter stay the same.
#' @export

DownloadSentinel2=function(target_path=NA,
                           shape_site=NA,
                           start_date=NA,
                           end_date=NA,
                           limit=1000,
                           cloudcover=10){
start_time=paste(start_date,"T00:00:01Z",sep="")
stop_time=paste(end_date,"T23:59:59Z",sep="")

shape_site=sf::st_transform(shape_site,crs= "+proj=longlat")#UTM2latlon

items=httr::GET(paste("https://catalogue.dataspace.copernicus.eu/stac/collections/SENTINEL-2/items?bbox=",
                      as.numeric(sf::st_bbox(shape_site)[1]),",",
                      as.numeric(sf::st_bbox(shape_site)[2]),",",
                      as.numeric(sf::st_bbox(shape_site)[3]),",",
                      as.numeric(sf::st_bbox(shape_site)[4]),
                      "&datetime=",
                      start_time,"/",stop_time,
                      "&limit=",limit,
                      "&sortby=datetime",
                      sep=""))
items=as.data.frame(jsonlite::fromJSON(httr::content(items,as="text",encoding="UTF-8")))
items=subset(items,items$features.properties$cloudCover<=cloudcover & items$features.properties$productType=="S2MSI2A")
dates_cloudcover=items$features.properties$datetime


################
#download all NDVI-data from startdate to enddate
connection = openeo::connect(host = "https://openeo.dataspace.copernicus.eu")
openeo::login()
processes_list=openeo::list_processes()
p = openeo::processes()
collections = openeo::list_collections()
formats = openeo::list_file_formats()
data = p$load_collection(id = collections$`SENTINEL2_L2A`,
                         spatial_extent = list(west=as.numeric(sf::st_bbox(shape_site)[1]),
                                               south=as.numeric(sf::st_bbox(shape_site)[2]),
                                               east=as.numeric(sf::st_bbox(shape_site)[3]),
                                               north=as.numeric(sf::st_bbox(shape_site)[4])),
                         temporal_extent = c(substr(start_time,1,10), substr(stop_time,1,10)),
                         bands=c("B04","B08","SCL"))

spectral_reduce = p$reduce_dimension(data = data, dimension = "bands",reducer = function(data,context) {
  B04 = data[1]
  B08 = data[2]
  return((B08-B04)/(B08+B04))
})

result = p$save_result(data = spectral_reduce,format = formats$output$GTiff, options = list(red="R",NIR="NIR"))

# create a job
job = openeo::create_job(graph = result, title = "S2A", description = "S2A-NDVI")

# then start the processing of the job and turn on logging (messages that are captured on the back-end during the process execution)
if(dir.exists(target_path)==F){
  dir.create(target_path)
}

openeo::start_job(job = job, log = TRUE)

while(1==1){
  Sys.sleep(10)
  if(invisible(class(openeo::list_results(job = job))=="ResultList")){
  invisible(openeo::download_results(job = job, folder = target_path))
    break
  }
}

##################
#remove files with cloudcover > threshold
delete_files=invisible(list.files(target_path)[!list.files(target_path) %in% unique(paste("openEO_",substr(dates_cloudcover,1,10),"Z.tif",sep=""))])
file.remove(paste(target_path,delete_files,sep="/"))

# rename files fitting the format needed to run the wb_calc.R
Sys.sleep(10)
if (length(list.files(target_path)>0)){
for(i in 1:length(list.files(target_path))){
  file.rename(list.files(target_path,full.names=T)[i],
              paste(target_path,substr(list.files(target_path)[i],8,11),
                    substr(list.files(target_path)[i],13,14),
                    substr(list.files(target_path)[i],16,17),
                    "_Sen2.tif",sep=""))
}
}
print(paste("Sentinel-2 Download successfully finished. Downloaded ",length(list.files(target_path)), " files.",sep=""))
}














