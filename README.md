
# WaterBalanceMod

R-Package to process maps showing daily, spatially distributed water balance for starch potatoes. NDVI data needs to be derived from either preprocessed DJI Phantom 4 Multispectral or PlanetScope satellite data or Sentinel-2 satellite data.

## Aim of Model

As water is a usually limited source in agriculture, it needs to be saved as best as possible. Starch potatoes in NE Germany need to be irrigated with about 80 - 120 mm during a vegetation period depending on climate conditions. Here, agriculturists often overestimate the potatoes need of water, so the dispense can be optimized. Furthermore, the model output can show a high spatial resolution, default 5 m, so an intraside specific irrigation can be applied, depending on the individual need of water in every pixel.

## How to install the package?

The package includes the "install_WaterBalanceMod.R" file. Open it using R or RStudio and change `wd <-` to your local path, where there your downloaded version of the WaterBalanceMod is saved. Run the script. When asked, if depending packages shall be installed, confirm. When everything is installed, the model should be ready to be run. To run the model you can use the attached sample data, that you can find in the subfolder "sample_data" (How to: see below). Copy that folder to your preferred working Directory first.

## Using sample data

### What do we have here?

The "sample_data" folder consists of
- a set of NDVI-sample data (folder "NDVI-Files") including DJI Phantom 4 Multispectral data (nomenclature: "[YYYYMMDD]_P4M1.tif") and/or PlanetScope data (nomenclature: "[YYYYMMDD]_Plan.tif") and/or Sentinel-2 data (nomenclature: "[YYYYMMDD]_Sen2.tif"). 
- a set of shapefiles (folder "Shapefile") including "sample_2023.shp", which contains a polygon of a sample site, that spatially fits to the one from the NDVI files, and "Buffer_36m_all_interp.shp", which contains irrigation data according to the sample site, and "Buffer_5_WB.shp", which contains a set of sample spots within the AOI (polygon circles with a diameter of 5m), that we can use to get further details for the area, that these spots are covering.
- a "DWD_ET0_2023.csv" file containing reference ET from German Weather Service, so it does not have to be downloaded
- a Folder "Radolan_2023_processed_daily" containing daily precipitation data from German Weather Service, so it does not have to be downloaded
- an R-file "run_calcWB_2023_sample.R" including an example configuration for running the water balance model.

### How do we configure the "run_calcWB_2023_sample.R"?

As `mypath=`you should use your preferred working directory, where you copied your sample data. The rest of the scripts can stay unchanged. Further information about the configuration possibilies can be found below.
The script can be subdivided into three parts:
- 1st: empty Environment and configuration
- 2nd: run the actual model (`calcWB()`) to create maps as shapefiles and/or geotiffs showing the waterbalance and other results for your AOI.
- 3rd: create plots, that show additional information about the sample spots within the AOI, that you are interested in (shapefile: "Buffer_5_WB.shp").To get These resulting maps, you need to run the "calcWBplots()" after using the resulting .RData - file from "calcWB()".

As a result you get a Folder named after the given configuration, e.g. "uwdw_poly_radolan_1_132_5", which means "uwdw" = method of calculation of NDVI, "poly" = method of modeling, "radolan" = source of precipitation, "1" = irrigation Efficiency, "132" = last day of NDVI being 0, "5" = spatial Resolution in meter.


## Input data and configuration - further details

The model needs at least NDVI data (GeoTIFF), a shapefile containing the AOI and a shapefile containing the irrigation amount. To show maps and detailed results for certain spots, you can also add a shapefile containting polygons for the spots the you are interested in. The folder "example_data" contains a set of data, that is the minimum required to run the model as well as as a script that is configured accordingly.

### NDVI data
The NDVI data needs to be either processed from DJI Phantom 4 Multispectral UAV using Pixer4Dmapper-othomosaics and/or PlanetScope images and/or Sentinel-2 images. The naming of the files is done by the date of recording [YYYYMMDD] followed by an underscore and the abbrivation of the source, i.e. for an image that was captured by PlanetScope ("Plan") on 15th July 2023, it should be "20230715_Plan.tiff", or for an orthomosaic from DJI Phantom 4 Multispectral ("P4M1"), it should be "20230715_P4M1.tiff" or for an orthomosaic from Sentinel-2 ("Sen2"), it should be "20230715_Sen2.tiff". The timespan of your NDVI data also defines the timespan of dates for your results.

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

### Method of modeling ("modeltype")
Just leave at "poly" as default.

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




