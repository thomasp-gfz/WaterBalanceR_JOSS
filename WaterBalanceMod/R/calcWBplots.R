#' Create .png files from .RDATA file created by calcWB() for every DOY within given timespan (earliest till latest NDVI-capture) showing NDVI, crop coefficient, crop evapotranspiration, precipitation, irrigation and water balance. Besides, .RDATA file is created, that contains mean values for selected samples.
#' @param source_path Path to .RDATA file (string) resulting from calcWB() function.
#' @param plant_doy DOY (integer), when planted.
#' @param buffer20 Path to buffer (string) containing shapefile with buffers of interest within study site. Read with sf::read_st().
#' @param shape_site Path to shapefile containing your AOI (string).
#' @return .png files for every DOY within given timespan (earliest till latest NDVI-capture) showing NDVI, crop coefficient, crop evapotranspiration, precipitation, irrigation and water balance
#' @export

calcWBplots=function(source_path=NA,
                             plant_doy=NA,
                             buffer20=NA,
                             shape_site=NA){

  source_path_name=paste(strsplit(source_path,"/")[[1]][1:(length(strsplit(source_path,"/")[[1]])-1)],collapse="/")
  source_file_name=strsplit(source_path,"/")[[1]][length(strsplit(source_path,"/")[[1]])]
  load(source_path)

  if(dir.exists(file.path(source_path_name, "plots/"))==F){
    dir.create(file.path(source_path_name, "plots/"), showWarnings = FALSE)
  }

  setwd(source_path_name)

  DOY=c(min(DOY,na.rm=T),max(DOY,na.rm=T))

  if(is.na(terra::crs(NDVI))==T){
    terra::crs(NDVI)="+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"
  }

  print("Calculating mean NDVI values...")
  NDVI_mean=data.frame(matrix(NA,length(1:max(DOY,na.rm=T)),nrow(buffer20)+1))
  for(i in min(DOY,na.rm=T):max(DOY,na.rm=T)){
    NDVI_mean[i,1]=i
    help_vec=sf::st_as_sf(methods::as(NDVI[[i]],'SpatialPolygonsDataFrame'))
    for (j in 1:nrow(buffer20)){
      NDVI_mean[i,j+1]=mean(help_vec[[1]][as.data.frame(sf::st_intersects(help_vec,sf::st_as_sf(buffer20)[j,]))$row.id],na.rm=T)
    }
  }

  print("Calculating mean Crop Coefficient values...")
  kc_mean=data.frame(matrix(NA,length(1:max(DOY,na.rm=T)),nrow(buffer20)+1))
  for(i in min(DOY,na.rm=T):max(DOY,na.rm=T)){
    kc_mean[i,1]=i
    for (j in 1:nrow(buffer20)){
      kc_mean[i,j+1]=mean(KC[[i]][[1]][as.data.frame(sf::st_intersects(KC[[i]],sf::st_as_sf(buffer20)[j,]))$row.id],na.rm=T)
    }
  }

  print("Calculating mean ETC values...")
  etc_cumulated_mean=data.frame(matrix(NA,length(1:max(DOY,na.rm=T)),nrow(buffer20)+1))
  for(i in min(DOY,na.rm=T):max(DOY,na.rm=T)){
    etc_cumulated_mean[i,1]=i
    for (j in 1:nrow(buffer20)){
      etc_cumulated_mean[i,j+1]=mean(ETC_cumulated[[i]][[1]][as.data.frame(sf::st_intersects(ETC_cumulated[[i]],sf::st_as_sf(buffer20)[j,]))$row.id],na.rm=T)
    }
  }

  print("Calculating mean irrigation values...")
  irrigation_cumulated_mean=data.frame(matrix(NA,length(1:max(DOY,na.rm=T)),nrow(buffer20)+1))
  for(i in min(DOY,na.rm=T):max(DOY,na.rm=T)){
    irrigation_cumulated_mean[i,1]=i
    for (j in 1:nrow(buffer20)){
      irrigation_cumulated_mean[i,j+1]=mean(irrigation_cumulated[[i]][[1]][as.data.frame(sf::st_intersects(irrigation_cumulated[[i]],sf::st_as_sf(buffer20)[j,]))$row.id],na.rm=T)
    }
  }

  print("Calculating mean precipitation values...")
  precipitation_cumulated_mean=data.frame(matrix(NA,length(1:max(DOY,na.rm=T)),nrow(buffer20)+1))
  for(i in min(DOY,na.rm=T):max(DOY,na.rm=T)){
    precipitation_cumulated_mean[i,1]=i
    help_vec=sf::st_as_sf(methods::as(precipitation_cumulated[[i]],'SpatialPolygonsDataFrame'))

    for (j in 1:nrow(buffer20)){
      precipitation_cumulated_mean[i,j+1]=mean(help_vec[[1]][as.data.frame(sf::st_intersects(help_vec,sf::st_as_sf(buffer20)[j,]))$row.id],na.rm=T)
    }
  }

  print("Calculating mean cumulated precipitation values...")
  precipitation_cumulated_mean=data.frame(matrix(NA,length(1:max(DOY,na.rm=T)),nrow(buffer20)+1))
  for(i in min(DOY,na.rm=T):max(DOY,na.rm=T)){
    precipitation_cumulated_mean[i,1]=i
    help_vec=sf::st_as_sf(methods::as(precipitation_cumulated[[i]],'SpatialPolygonsDataFrame'))
    for (j in 1:nrow(buffer20)){
      precipitation_cumulated_mean[i,j+1]=mean(help_vec[[1]][as.data.frame(sf::st_intersects(help_vec,sf::st_as_sf(buffer20)[j,]))$row.id],na.rm=T)
    }
  }

  print("Calculating mean water balance values...")
  wb_cumulated_mean=data.frame(matrix(NA,length(1:max(DOY,na.rm=T)),nrow(buffer20)+1))
  for(i in min(DOY,na.rm=T):max(DOY,na.rm=T)){
    wb_cumulated_mean[i,1]=i
    for (j in 1:nrow(buffer20)){
      wb_cumulated_mean[i,j+1]=mean(WB_cumulated[[i]][[1]][as.data.frame(sf::st_intersects(WB_cumulated[[i]],sf::st_as_sf(buffer20)[j,]))$row.id],na.rm=T)
      }
  }

  save(DOY,
       NDVI_mean,
       kc_mean,
       etc_cumulated_mean,
       precipitation_cumulated_mean,
       irrigation_cumulated_mean,
       wb_cumulated_mean,
       file=paste(substr(source_path,1,nchar(source_path)-6),"_mean_data_charts.RData",sep=""))

  print(paste("Mean values successfully saved as .RData file: ",paste(substr(source_path,1,nchar(source_path)-6),"_mean_data_charts.RData",sep="")))

  brewer.pal_col=c("darkgreen","blue", "orange","red","cyan","lightgreen")

  for(i in min(DOY,na.rm=T):max(DOY,na.rm=T)){
    setwd(source_path_name)
    print(paste("Creating wallpaper for DOY: ",i,sep=""))

    #NDVI
    NDVI_help=sf::st_as_sf(methods::as(NDVI[[i]],'SpatialPolygonsDataFrame'))
    names(NDVI_help)[1]="values"
    if(stats::sd(NDVI_help$values,na.rm = T)!=0){
      gg_NDVI<-ggplot2::ggplot() + ggplot2::geom_sf(data = NDVI_help, ggplot2::aes(fill = values), colour=NA)+
        ggplot2::scale_fill_gradientn(colours = c("grey", "brown", "green"),name="NDVI",
                             values = scales::rescale(c(min(NDVI_help$values,na.rm=T),
                                                        mean(NDVI_help$values,na.rm=T) - stats::sd(NDVI_help$values,na.rm=T),
                                                        mean(NDVI_help$values,na.rm=T),
                                                        mean(NDVI_help$values,na.rm=T) + stats::sd(NDVI_help$values,na.rm=T),
                                                        max(NDVI_help$values,na.rm=T))))+
        ggplot2::geom_sf(data = sf::st_as_sf(buffer20), fill = NA, color = brewer.pal_col[1:nrow(buffer20)], lwd=1.5, shape = 21)+
        ggplot2::labs(title=paste("NDVI at DOY (DAP) = ",i,"(",i-plant_doy,")",sep=""))+
        ggplot2::geom_sf_text(data = sf::st_as_sf(buffer20),ggplot2::aes(label = c(as.character(1:nrow(buffer20)))),colour="black")+
        ggplot2::theme_bw()+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold", color = "black"),
              legend.title = ggplot2::element_text(size=14),legend.text = ggplot2::element_text(size=14))
    } else {
      gg_NDVI<-ggplot2::ggplot() + ggplot2::geom_sf(data = NDVI_help, ggplot2::aes(fill = values), colour=NA)+
        ggplot2::scale_colour_gradient2(low="grey",mid="brown", high="green", name="NDVI") +
        ggplot2::scale_fill_gradient2(low="grey", mid="brown", high="green", name="NDVI")+
        ggplot2::geom_sf(data = sf::st_as_sf(buffer20), fill = NA, color = brewer.pal_col[1:nrow(buffer20)], lwd=1.5, shape = 21)+
        ggplot2::labs(title=paste("NDVI at DOY (DAP) = ",i,"(",i-plant_doy,")",sep=""))+
        ggplot2::geom_sf_text(data = sf::st_as_sf(buffer20),ggplot2::aes(label = c(as.character(1:nrow(buffer20)))),colour="black")+
        ggplot2::theme_bw()+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold", color = "black"),
              legend.title = ggplot2::element_text(size=14),legend.text = ggplot2::element_text(size=14))
    }

      NDVI_mean_nona=stats::na.omit(NDVI_mean)
      colnames(NDVI_mean_nona)[1]="DOY"
      NDVI_mean_long_tidyr=tidyr::pivot_longer(NDVI_mean_nona, cols = tidyselect::starts_with("X"))

      gg_NDVI_mean=ggplot2::ggplot(data=NDVI_mean_long_tidyr,
             ggplot2::aes(x=DOY, y=value, colour=name)) +
        ggplot2::geom_line(lwd=1.5)+
        ggplot2::labs(title=paste("NDVI at DOY (DAP) = ",i,"(",i-plant_doy,")",sep=""), x="DOY",y="NDVI", color="Plot sample")+
        ggplot2::scale_color_manual(labels = c(as.character(1:nrow(buffer20))), values = brewer.pal_col[1:nrow(buffer20)])+
        ggplot2::geom_vline(xintercept = i)+
        ggplot2::theme_bw()+
        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), axis.title.x = ggplot2::element_text(size = 16),
              axis.text.y = ggplot2::element_text(size = 14), axis.title.y = ggplot2::element_text(size = 16),
              plot.title = ggplot2::element_text(size = 16, face = "bold", color = "black"),
              legend.title = ggplot2::element_text(size=14),legend.text = ggplot2::element_text(size=14))

    #precipitation
    precipitation_cumulated[[i]][[1]]=(terra::mask(raster::crop(precipitation_cumulated[[i]][[1]],shape_site),shape_site))
    names(precipitation_cumulated[[i]])[1]="values"
    if(stats::sd(precipitation_cumulated[[i]][[1]]@data@values,na.rm = T)!=0){
      gg_precip<-ggplot2::ggplot() + ggplot2::geom_sf(data = sf::st_as_sf(methods::as(precipitation_cumulated[[i]][[1]],"SpatialPolygonsDataFrame")), ggplot2::aes(fill = values), colour=NA)+
        ggplot2::scale_fill_gradientn(colours = c("red", "green", "blue"),name="prec",
                             values = scales::rescale(c(min(precipitation_cumulated[[i]][[1]]@data@values,na.rm=T),
                                                        mean(precipitation_cumulated[[i]][[1]]@data@values,na.rm=T) - stats::sd(precipitation_cumulated[[i]][[1]]@data@values,na.rm=T),
                                                        mean(precipitation_cumulated[[i]][[1]]@data@values,na.rm=T),
                                                        mean(precipitation_cumulated[[i]][[1]]@data@values,na.rm=T) + stats::sd(precipitation_cumulated[[i]][[1]]@data@values,na.rm=T),
                                                        max(precipitation_cumulated[[i]][[1]]@data@values,na.rm=T))))+
        ggplot2::geom_sf(data = sf::st_as_sf(buffer20), fill = NA, color = brewer.pal_col[1:nrow(buffer20)], lwd=1.5, shape = 21)+
        ggplot2::labs(title=paste("Cumulated Precipitation [mm] at DOY = ",i,"(",i-plant_doy,")",sep=""))+
        ggplot2::geom_sf_text(data = sf::st_as_sf(buffer20),ggplot2::aes(label = c(as.character(1:nrow(buffer20)))),colour="black")+
        ggplot2::theme_bw()+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold", color = "black"),
              legend.title = ggplot2::element_text(size=14),legend.text = ggplot2::element_text(size=14))
    } else {
      gg_precip<-ggplot2::ggplot() + ggplot2::geom_sf(data = sf::st_as_sf(methods::as(precipitation_cumulated[[i]][[1]],"SpatialPolygonsDataFrame")), ggplot2::aes(fill = values), colour=NA)+
        ggplot2::scale_colour_gradient2(low="red",mid="green", high="blue", name="Precipitation") +
        ggplot2::scale_fill_gradient2(low="red", mid="green", high="blue", name="prec")+
        ggplot2::geom_sf(data = sf::st_as_sf(buffer20), fill = NA, color = brewer.pal_col[1:nrow(buffer20)], lwd=1.5, shape = 21)+
        ggplot2::labs(title=paste("Cumulated Precipitation [mm] at DOY (DAP) = ",i,"(",i-plant_doy,")",sep=""))+
        ggplot2::geom_sf_text(data = sf::st_as_sf(buffer20),ggplot2::aes(label = c(as.character(1:nrow(buffer20)))),colour="black")+
        ggplot2::theme_bw()+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold", color = "black"),
            legend.title = ggplot2::element_text(size=14),legend.text = ggplot2::element_text(size=14))
    }

    precipitation_cumulated_mean_nona=stats::na.omit(precipitation_cumulated_mean)
    colnames(precipitation_cumulated_mean_nona)[1]="DOY"
    precip_mean_long_tidyr=tidyr::pivot_longer(precipitation_cumulated_mean_nona, cols = tidyselect::starts_with("X"))

    gg_precip_mean=ggplot2::ggplot(data=precip_mean_long_tidyr,
                        ggplot2::aes(x=DOY, y=value, colour=name)) +
      ggplot2::geom_line(lwd=1.5)+
      ggplot2::labs(title=paste("Cumulated Precipitation at DOY (DAP) = ",i,"(",i-plant_doy,")",sep=""), x="DOY",y="Precipitation [mm]", color="Plot sample")+
      ggplot2::scale_color_manual(labels = c(as.character(1:nrow(buffer20))), values = brewer.pal_col[1:nrow(buffer20)])+
      ggplot2::geom_vline(xintercept = i)+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), axis.title.x = ggplot2::element_text(size = 16),
            axis.text.y = ggplot2::element_text(size = 14), axis.title.y = ggplot2::element_text(size = 16),
            plot.title = ggplot2::element_text(size = 16, face = "bold", color = "black"),
            legend.title = ggplot2::element_text(size=14),legend.text = ggplot2::element_text(size=14))

    #KC
    names(KC[[i]])[1]="values"
    if(stats::sd(KC[[i]]$values,na.rm = T)!=0){
      gg_KC<-ggplot2::ggplot() + ggplot2::geom_sf(data = KC[[i]], ggplot2::aes(fill = values), colour=NA)+
        ggplot2::scale_fill_gradientn(colours = c("grey", "brown", "green"),name="Kc",
                             values = scales::rescale(c(min(KC[[i]]$values,na.rm=T),
                                                        mean(KC[[i]]$values,na.rm=T) - stats::sd(KC[[i]]$values,na.rm=T),
                                                        mean(KC[[i]]$values,na.rm=T),
                                                        mean(KC[[i]]$values,na.rm=T) + stats::sd(KC[[i]]$values,na.rm=T),
                                                        max(KC[[i]]$values,na.rm=T))))+
        ggplot2::geom_sf(data = sf::st_as_sf(buffer20), fill = NA, color = brewer.pal_col[1:nrow(buffer20)], lwd=1.5, shape = 21)+
        ggplot2::labs(title=paste("Crop Coefficent at DOY (DAP) = ",i,"(",i-plant_doy,")",sep=""))+
        ggplot2::geom_sf_text(data = sf::st_as_sf(buffer20),ggplot2::aes(label = c(as.character(1:nrow(buffer20)))),colour="black")+
        ggplot2::theme_bw()+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold", color = "black"),
              legend.title = ggplot2::element_text(size=14),legend.text = ggplot2::element_text(size=14))
    } else{
      gg_KC<-ggplot2::ggplot() + ggplot2::geom_sf(data = KC[[i]], ggplot2::aes(fill = values), colour=NA)+
        ggplot2::scale_colour_gradient2(low="grey",mid="brown", high="green", name="Crop Coefficient") +
        ggplot2::scale_fill_gradient2(low="grey", mid="brown", high="green", name="KC")+
        ggplot2::geom_sf(data = sf::st_as_sf(buffer20), fill = NA, color = brewer.pal_col[1:nrow(buffer20)], lwd=1.5, shape = 21)+
        ggplot2::labs(title=paste("Crop Coefficent at DOY (DAP) = ",i,"(",i-plant_doy,")",sep=""))+
        ggplot2::geom_sf_text(data = sf::st_as_sf(buffer20),ggplot2::aes(label = c(as.character(1:nrow(buffer20)))),colour="black")+
        ggplot2::theme_bw()+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold", color = "black"),
              legend.title = ggplot2::element_text(size=14),legend.text = ggplot2::element_text(size=14))
    }

    kc_mean_nona=stats::na.omit(kc_mean)
    colnames(kc_mean_nona)[1]="DOY"
    kc_mean_long_tidyr=tidyr::pivot_longer(kc_mean_nona, cols = tidyselect::starts_with("X"))

    gg_KC_mean=ggplot2::ggplot(data=kc_mean_long_tidyr,
                          ggplot2::aes(x=DOY, y=value, colour=name)) +
      ggplot2::geom_line(lwd=1.5)+
      ggplot2::labs(title=paste("Crop Coefficient at DOY (DAP) = ",i,"(",i-plant_doy,")",sep=""), x="DOY",y="Kc", color="Plot sample")+
      ggplot2::scale_color_manual(labels = c(as.character(1:nrow(buffer20))), values = brewer.pal_col[1:nrow(buffer20)])+
      ggplot2::geom_vline(xintercept = i)+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), axis.title.x = ggplot2::element_text(size = 16),
            axis.text.y = ggplot2::element_text(size = 14), axis.title.y = ggplot2::element_text(size = 16),
            plot.title = ggplot2::element_text(size = 16, face = "bold", color = "black"),
            legend.title = ggplot2::element_text(size=14),legend.text = ggplot2::element_text(size=14))



    #irrigation
    names(irrigation_cumulated[[i]])[1]="values"
    if(stats::sd(irrigation_cumulated[[i]]$values,na.rm = T)!=0){
      gg_irrigation<-ggplot2::ggplot() + ggplot2::geom_sf(data = irrigation_cumulated[[i]], ggplot2::aes(fill = values), colour=NA)+
        ggplot2::scale_fill_gradientn(colours = c("red", "green", "blue"),name="Irg",
                             values = scales::rescale(c(min(irrigation_cumulated[[i]]$values,na.rm=T),
                                                        mean(irrigation_cumulated[[i]]$values,na.rm=T) - stats::sd(irrigation_cumulated[[i]]$values,na.rm=T),
                                                        mean(irrigation_cumulated[[i]]$values,na.rm=T),
                                                        mean(irrigation_cumulated[[i]]$values,na.rm=T) + stats::sd(irrigation_cumulated[[i]]$values,na.rm=T),
                                                        max(irrigation_cumulated[[i]]$values,na.rm=T))))+
        ggplot2::geom_sf(data = sf::st_as_sf(buffer20), fill = NA, color = brewer.pal_col[1:nrow(buffer20)], lwd=1.5, shape = 21)+
        ggplot2::labs(title=paste("Cumulated Irrigation [mm] at DOY (DAP) = ",i,"(",i-plant_doy,")",sep=""))+
        ggplot2::geom_sf_text(data = sf::st_as_sf(buffer20),ggplot2::aes(label = c(as.character(1:nrow(buffer20)))),colour="black")+
        ggplot2::theme_bw()+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold", color = "black"),
              legend.title = ggplot2::element_text(size=14),legend.text = ggplot2::element_text(size=14))
    }else {
      gg_irrigation<-ggplot2::ggplot() + ggplot2::geom_sf(data = irrigation_cumulated[[i]], ggplot2::aes(fill = values), colour=NA)+
        ggplot2::scale_colour_gradient2(low="red",mid="green", high="blue", name="Irrigation") +
        ggplot2::scale_fill_gradient2(low="red", mid="green", high="blue", name="Irg")+
        ggplot2::geom_sf(data = sf::st_as_sf(buffer20), fill = NA, color = brewer.pal_col[1:nrow(buffer20)], lwd=1.5, shape = 21)+
        ggplot2::labs(title=paste("Cumulated Irrigation [mm] at DOY (DAP) = ",i,"(",i-plant_doy,")",sep=""))+
        ggplot2::geom_sf_text(data = sf::st_as_sf(buffer20),ggplot2::aes(label = c(as.character(1:nrow(buffer20)))),colour="black")+
        ggplot2::theme_bw()+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold", color = "black"),
              legend.title = ggplot2::element_text(size=14),legend.text = ggplot2::element_text(size=14))
    }

    irrigation_cumulated_mean_nona=stats::na.omit(irrigation_cumulated_mean)
    colnames(irrigation_cumulated_mean_nona)[1]="DOY"
    irrigation_cumulated_mean_nona_long_tidyr=tidyr::pivot_longer(irrigation_cumulated_mean_nona, cols = tidyselect::starts_with("X"))

    gg_irrigation_mean=ggplot2::ggplot(data=irrigation_cumulated_mean_nona_long_tidyr,
                      ggplot2::aes(x=DOY, y=value, colour=name)) +
      ggplot2::geom_line(lwd=1.5)+
      ggplot2::labs(title=paste("Cumulated Irrigation at DOY (DAP) = ",i,"(",i-plant_doy,")",sep=""), x="DOY",y="Irrigation [mm]", color="Plot sample")+
      ggplot2::scale_color_manual(labels = c(as.character(1:nrow(buffer20))), values = brewer.pal_col[1:nrow(buffer20)])+
      ggplot2::geom_vline(xintercept = i)+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), axis.title.x = ggplot2::element_text(size = 16),
            axis.text.y = ggplot2::element_text(size = 14), axis.title.y = ggplot2::element_text(size = 16),
            plot.title = ggplot2::element_text(size = 16, face = "bold", color = "black"),
            legend.title = ggplot2::element_text(size=14),legend.text = ggplot2::element_text(size=14))

    #ETC
    names(ETC_cumulated[[i]])[1]="values"
    if(stats::sd(ETC_cumulated[[i]]$values,na.rm = T)!=0){
      gg_ETC<-ggplot2::ggplot() + ggplot2::geom_sf(data = ETC_cumulated[[i]], ggplot2::aes(fill = values), colour=NA)+
        ggplot2::scale_fill_gradientn(colours = c("red", "green", "blue"),name="ETc",
                             values = scales::rescale(c(min(ETC_cumulated[[i]]$values,na.rm=T),
                                                        mean(ETC_cumulated[[i]]$values,na.rm=T) - stats::sd(ETC_cumulated[[i]]$values,na.rm=T),
                                                        mean(ETC_cumulated[[i]]$values,na.rm=T),
                                                        mean(ETC_cumulated[[i]]$values,na.rm=T) + stats::sd(ETC_cumulated[[i]]$values,na.rm=T),
                                                        max(ETC_cumulated[[i]]$values,na.rm=T))))+
        ggplot2::geom_sf(data = sf::st_as_sf(buffer20), fill = NA, color = brewer.pal_col[1:nrow(buffer20)], lwd=1.5, shape = 21)+
        ggplot2::labs(title=paste("Cumulated ETc [mm] at DOY (DAP) = ",i,"(",i-plant_doy,")",sep=""))+
        ggplot2::geom_sf_text(data = sf::st_as_sf(buffer20),ggplot2::aes(label = c(as.character(1:nrow(buffer20)))),colour="black")+
        ggplot2::theme_bw()+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold", color = "black"),
              legend.title = ggplot2::element_text(size=14),legend.text = ggplot2::element_text(size=14))
    } else {
      gg_ETC<-ggplot2::ggplot() + ggplot2::geom_sf(data = ETC_cumulated[[i]], ggplot2::aes(fill = values), colour=NA)+
        ggplot2::scale_colour_gradient2(low="grey",mid="brown", high="green", name="Crop Evapotranspiration") +
        ggplot2::scale_fill_gradient2(low="grey", mid="brown", high="green", name="ETc")+
        ggplot2::geom_sf(data = sf::st_as_sf(buffer20), fill = NA, color = brewer.pal_col[1:nrow(buffer20)], lwd=1.5, shape = 21)+
        ggplot2::labs(title=paste("Cumulated ETC [mm] at DOY (DAP) = ",i,"(",i-plant_doy,")",sep=""))+
        ggplot2::geom_sf_text(data = sf::st_as_sf(buffer20),ggplot2::aes(label = c(as.character(1:nrow(buffer20)))),colour="black")+
        ggplot2::theme_bw()+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold", color = "black"),
              legend.title = ggplot2::element_text(size=14),legend.text = ggplot2::element_text(size=14))
    }

    etc_cumulated_mean_nona=stats::na.omit(etc_cumulated_mean)
    colnames(etc_cumulated_mean_nona)[1]="DOY"
    etc_cumulated_mean_nona_long_tidyr=tidyr::pivot_longer(etc_cumulated_mean_nona, cols = tidyselect::starts_with("X"))

    gg_ETC_mean=ggplot2::ggplot(data=etc_cumulated_mean_nona_long_tidyr,
                              ggplot2::aes(x=DOY, y=value, colour=name)) +
      ggplot2::geom_line(lwd=1.5)+
      ggplot2::labs(title=paste("Cumulated ETC at DOY (DAP) = ",i,"(",i-plant_doy,")",sep=""), x="DOY",y="ETc [mm]", color="Plot sample")+
      ggplot2::scale_color_manual(labels = c(as.character(1:nrow(buffer20))), values = brewer.pal_col[1:nrow(buffer20)])+
      ggplot2::geom_vline(xintercept = i)+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), axis.title.x = ggplot2::element_text(size = 16),
            axis.text.y = ggplot2::element_text(size = 14), axis.title.y = ggplot2::element_text(size = 16),
            plot.title = ggplot2::element_text(size = 16, face = "bold", color = "black"),
            legend.title = ggplot2::element_text(size=14),legend.text = ggplot2::element_text(size=14))

    #water balance
    names(WB_cumulated[[i]])[1]="values"
    if(stats::sd(WB_cumulated[[i]]$values,na.rm = T)!=0){
      gg_wb<-ggplot2::ggplot() + ggplot2::geom_sf(data = WB_cumulated[[i]], ggplot2::aes(fill = values), colour=NA)+
        ggplot2::scale_fill_gradientn(colours = c("red", "green", "blue"),name="WB",
                             values = scales::rescale(c(min(WB_cumulated[[i]]$values,na.rm=T),
                                                        mean(WB_cumulated[[i]]$values,na.rm=T) - stats::sd(WB_cumulated[[i]]$values,na.rm=T),
                                                        mean(WB_cumulated[[i]]$values,na.rm=T),
                                                        mean(WB_cumulated[[i]]$values,na.rm=T) + stats::sd(WB_cumulated[[i]]$values,na.rm=T),
                                                        max(WB_cumulated[[i]]$values,na.rm=T))))+
        ggplot2::geom_sf(data = sf::st_as_sf(buffer20), fill = NA, color = brewer.pal_col[1:nrow(buffer20)], lwd=1.5, shape = 21)+
        ggplot2::labs(title=paste("Cumulated Water Balance [mm] at DOY (DAP) = ",i,"(",i-plant_doy,")",sep=""))+
        ggplot2::geom_sf_text(data = sf::st_as_sf(buffer20),ggplot2::aes(label = c(as.character(1:nrow(buffer20)))),colour="black")+
        ggplot2::theme_bw()+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold", color = "black"),
              legend.title = ggplot2::element_text(size=14),legend.text = ggplot2::element_text(size=14))
    } else{
      gg_wb<-ggplot2::ggplot() + ggplot2::geom_sf(data = WB_cumulated[[i]], ggplot2::aes(fill = values), colour=NA)+
        ggplot2::scale_colour_gradient2(low="red",mid="green", high="blue", name="Water Balance") +
        ggplot2::scale_fill_gradient2(low="red", mid="green", high="blue", name="WB")+
        ggplot2::geom_sf(data = sf::st_as_sf(buffer20), fill = NA, color = brewer.pal_col[1:nrow(buffer20)], lwd=1.5, shape = 21)+
        ggplot2::labs(title=paste("Cumulated Water Balance [mm] at DOY (DAP) = ",i,"(",i-plant_doy,")",sep=""))+
        ggplot2::geom_sf_text(data = sf::st_as_sf(buffer20),ggplot2::aes(label = c(as.character(1:nrow(buffer20)))),colour="black")+
        ggplot2::theme_bw()+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold", color = "black"),
              legend.title = ggplot2::element_text(size=14),legend.text = ggplot2::element_text(size=14))
    }

    wb_cumulated_mean_nona=stats::na.omit(wb_cumulated_mean)
    colnames(wb_cumulated_mean_nona)[1]="DOY"
    wb_cumulated_mean_nona_long_tidyr=tidyr::pivot_longer(wb_cumulated_mean_nona, cols = tidyselect::starts_with("X"))

    gg_wb_mean=ggplot2::ggplot(data=wb_cumulated_mean_nona_long_tidyr,
                       ggplot2::aes(x=DOY, y=value, colour=name)) +
      ggplot2::geom_line(lwd=1.5)+
      ggplot2::labs(title=paste("Cumulated Water Balance at DOY (DAP) = ",i,"(",i-plant_doy,")",sep=""), x="DOY",y="Water Balance [mm]", color="Plot sample")+
      ggplot2::scale_color_manual(labels = c(as.character(1:nrow(buffer20))), values = brewer.pal_col[1:nrow(buffer20)])+
      ggplot2::geom_vline(xintercept = i)+
      ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "grey30") +
      ggplot2::theme_bw()+
      ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "grey30") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), axis.title.x = ggplot2::element_text(size = 16),
            axis.text.y = ggplot2::element_text(size = 14), axis.title.y = ggplot2::element_text(size = 16),
            plot.title = ggplot2::element_text(size = 16, face = "bold", color = "black"),
            legend.title = ggplot2::element_text(size=14),legend.text = ggplot2::element_text(size=14))



    gg_ensemble=gridExtra::grid.arrange(gg_NDVI,
                                        gg_NDVI_mean,
                                        gg_precip,
                                        gg_precip_mean,
                                        gg_KC,
                                        gg_KC_mean,
                                        gg_irrigation,
                                        gg_irrigation_mean,
                                        gg_ETC,
                                        gg_ETC_mean,
                                        gg_wb,
                                        gg_wb_mean,
                                        nrow=3)

    if(dir.exists(file.path(source_path_name,"wallpapers"))==F){
      dir.create(file.path(source_path_name,"wallpapers"), showWarnings = FALSE, recursive=TRUE)
    }

    setwd(file.path(source_path_name,"wallpapers"))
    ggplot2::ggsave(paste("wallpaper at DOY ",i,".png",sep=""),
           plot=gg_ensemble,
           width = 1920,
           height = 1080,
           units = "px",
           dpi = 70)
    try(grDevices::dev.off())
  }
  print(paste("All wallpapers successfully saved: ",file.path(source_path_name,"wallpapers"),sep=""))
}














