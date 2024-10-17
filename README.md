
# WaterBalanceMod

R-Package to process maps showing daily, spatially distributed water balance für starch potatoes. NDVI data needs to be derived from either preprocessed DJI Phantom 4 Multispectral or PlanetScope satellite data.

## Aim of Model

As water is a usually limited source in agriculture, it needs to be saved as best as possible. Starch potatoes in NE Germany need to be irrigated with about 80 - 120 mm during a vegetation period. Here, agriculturists often overestimate the potatoes need of water, so the dispense can be optimized. Furthermore, the model Output can show a high spatial resolution, default 5 m, so an intraside specific irrigation can be applied, depending on the individual need of water in every pixel.

## Input data and configuration

The model needs at least NDVI data (GeoTIFF), a shapefile containing the AOI and a shapefile containing the irrigation amount. To show maps and detailed results for certain spots, you can also add a shapefile containting polygons for the spots the you are interested in. The folder "example_data" contains a set of data, that is the minimum required to run the model as well as as a script that is configured accordingly.

### NDVI data
The NDVI data needs to be either processed from DJI Phantom 4 Multispectral UAV using Pixer4Dmapper-othomosaics and/or PlanetScope images. The naming of the files is done by the date of recording [YYYYMMDD] followed by an underscore and the abbrivation of the source, i.e. for an image that was captured by PlanetScope ("Plan") on 15th July 2023, it should be "20230715_Plan.tiff", or for an orthomosaic from DJI Phantom 4 Multispectral ("P4M1"), it should be "20230715_P4M1.tiff". The timepan of your NDVI data also defines the timespan of dates for your results.

### AOI ("shape_site")
You need a shapefile containing a polygon from your AOI

### Precipitation
Precipitation data is gathered either from German Weather Service (default) or from given X-Band precipitation data, that was processed using WRaINfo (https://doi.org/10.5281/zenodo.7220832 and https://doi.org/10.5334/jors.453). Here, for every day of year (DOY) a shapefile with the daily precipitation distribution is needed.

#### Source of precipitation ("precip_source")
Choose either "RADOLAN" (default, string) or "FURUNO" (string) depending on the source you would like to use.

#### Path to precipitation data ("path_WR_precip")
Choose the path to your precipitation data (string). This should be a folder containing shapefiles with precipitation data for every day during the vegetation period you are interested in. If you leave it an NA (default), precipitation data is downloaded from German Weather Service (DWD).

### Reference Evapotranspiration
Reference ET Data is gathered from German Weather Service (default) or from on-site deployed Arable Mark 2 or Arable Mark 3 stations. Here, you need a working user account with log-in and password. 

#### "ET_ref"
Here you can enter a csv-file containing a chart of DOY (integer) and according values or you can leave it as "NA", which is the default. When using the csv-file, the first column needs to be ascending numerized (integer) from one on with empty header. The second column contains the reference ET value for the certain DOY (float) with header "V1". The third column needs to be the date (format "YYYY-MM-DD", e.g. "2021-05-01). When left ET_ref=NA (default), the reference ET is automatically downloaded from either German Weather Servcice (DWD, default) or Arable, if you have an account. This decision needs to be made in the next step ("ET_ref_dl").

#### "ET_ref_dl"
If you do not have any reference ET data chart, leave "ET_ref = NA" and choose here between "ET_ref_dl = DWD" to download from German Weather Service ("DWD") or "ET_ref_dl = Arable" to download from your Arable account ("string"). If you choose to download from your Arable Account, you need to put in your Arable login data.

### Irrigation
Irrigation is regareded as one shapefile containing irrigation distribution for the certain days, when an irrigation event was applied. Here, a script, that can scrape and process data from your Raindancer user account, is included ("DownloadRaindancer.R"). If you like to run it, you first have to install Java (https://www.java.com) and Firefox on your machine.

#### loading Irrigation data ("irrig_sf")
Path to shapefile containing the irrigation data (string), e.g. st_read(paste(mypath,"/Shapefile/Buffer_36m_all_interp.shp",sep="")). The shapefile needs to contain the following coloumns: Drck_mn (water pressure, float), Dtm_Uh_ (Date and time, string, format: "YY-MM-DD hh:mm:ss"), timedif (time difference between steps in hours, float), dst_gps (spatial distance between in m the logs of sprinkler, float), gschwn_ (speed of sprinkler in m/s, float), Brg_GPS (irrigation amount, mm, float), Dstnz_k (cumulated spatial distance between logs in m, float), DOY (day of year, integer), geometry (geometric geometry). You can also generate this shapefile by 1st using the function "DownloadRaindancer" to download all of your irrigation data that was logged by raindancer. Take note, that irrigation data can only be downloaded from the last 12 days. So you should downoad regularly. In the 2nd step you can use the function "DownloadRaindancerCombineCharts" to combine the downloaded charts and process them to the needed shapefile. The resulting shapefile is being updated witht every iteration of download.

#### Irrigation efficiency ("irrigation_efficiency")
Choose irrigation efficiency, float between 0 and 1 (default). Here, irrigation efficiency is meant to be as the fraction of water that was infiltrated in the soil from the amount that was applied.

### Target resolution ("target_res")
Here, you can name the the target resolution of your resulting map in meter. Default = 5 m.

### Method of calculating NDVI ("method_NDVI")
Here, you can choose a method, that is used to process the input NDVI data. Just leave at "uwdw" as default.

### Method of modeling ("modeltype")
Just leave at "poly" as deafult.

### Last day with NDVI = 0 ("last_NDVI_0")
Here you need to enter the DOY, when no plants were last visible.

### Processing year ("output_year")
Number of year, you like to process (format: "YYYY", e.g. 2021, integer).

### saving results
Choose, how to save your results:

#### save_shape
Results are saved as shapefiles, Default: TRUE.

#### save_geotiff
Results are saved as geotiffs, Default: TRUE.

#### save_RDATA
Results are saved as .RDATA, Default: TRUE.

### Arable user data ("arable_user" and "arable_pass")
Your user name for your Arable account (string). Only necessary, if you chose "ET_ref_dl" with "Arable". Else: leave at NA.
Your password for your Arable account (string). Only necessary, if you chose "ET_ref_dl" with "Arable". Else: leave at NA.
Here it is necessary, that either Arable Mark 2 or Arable Mark 3 Ground stations are deployed within your AOI during your desired timespan.

## Further results
Next to the given results from calWB.R, it is useful to run the script "calcWBplots.R" to get Maps containing Maps for every desired DOY. Here, it is necessary to load the .RDATA file resulting from calcWB.R next to some other input data:

### DOY of planting ("plant_doy")
DOY (integer) of planting potatoes.

### Buffer of the spots you are interested in ("buffer20")
Here, you Need to load a shapefile, that contains up to 5 polygons of spots, that you are especially interested in (see "example_data")

### Shapefile of AOI ("shape_site")




