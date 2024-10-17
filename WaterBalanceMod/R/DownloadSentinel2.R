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
#setwd("Z:/GFZ/00R-Scripte/API_Sentinel_2/")
connection = openeo::connect(host = "https://openeo.dataspace.copernicus.eu")
openeo::login()
processes_list=list_processes()
p = openeo::processes()
collections = list_collections()
formats = list_file_formats()
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

start_job(job = job, log = TRUE)

while(1==1){
  Sys.sleep(10)
  if(invisible(class(list_results(job = job))=="ResultList")){
  invisible(download_results(job = job, folder = target_path))
    break
  }
}

##################
#remove files with cloudcover > threshold
delete_files=invisible(list.files(target_path)[!list.files(target_path) %in% unique(paste("openEO_",substr(dates_cloudcover,1,10),"Z.tif",sep=""))])
file.remove(paste(target_path,delete_files,sep="/"))

print("Sentinel-2 Download successfully finished.")
}














