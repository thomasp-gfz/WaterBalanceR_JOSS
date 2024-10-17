#' Scrapes data from Raindancer user account for all logged sprinklers. Beware: It only downloads the last 10.000 logs, i.e. the last ~12 days. Before using, you need to install Java on your machine. It only works, when Firefox is installed on your machine.
#' @param sourcepath Path (string) to Firefox download folder. Look it up in your Firefox browser.
#' @param targetpath Path (string) to destination folder for downloaded csv-files from Raindancer.
#' @param port You need to open a port to let R and Java scrape the website´s data (default: 4486L).
#' @param client Raindancer client number
#' @param user Raindancer user account
#' @param pass Raindancer password
#' @param waitfor time to wait for loading websites. The quicker your computer and internet connection, the less it can be (integer). Default is 3.
#' @param buffer_dist spray radius of sprinkler in meter (integer), default is 36.
#' @return csv file for all irrigation events of all sprinklers, that are logged in Raindancer Account.
#' @export

DownloadRaindancer=function(sourcepath,
               targetpath,
               port=4486L,
               client,
               user,
               pass,
               waitfor=3,
               buffer_dist=36){


  driver=RSelenium::rsDriver(browser="firefox",port=port,chromever = NULL, verbose=F, check=F)
  driver$client$closeWindow()#close session
  #rm(driver)

  remDr <- RSelenium::remoteDriver(browserName="firefox",port=port,remoteServerAddr="localhost")
  remDr$open(silent=T)

  remDr$navigate("https://portal.myraindancer.com/Login/Login.aspx")
  Sys.sleep(waitfor)

  #send clientnr
  clientnr <- remDr$findElement(using = "id", value = "clientnr_field")
  clientnr$clearElement()
  clientnr$sendKeysToElement(list(as.character(client)))

  #send username
  username <- remDr$findElement(using = "id", value = "username_field")
  username$clearElement()
  username$sendKeysToElement(list(as.character(user)))

  #send password and Enter
  passwd <- remDr$findElement(using = "id", value = "password_field")
  passwd$clearElement()
  passwd$sendKeysToElement(list(as.character(pass), "\uE007"))

  #get names and links for sprinklers and combine them to a table
  Sys.sleep(waitfor)
  get_name_rain=remDr$findElement(using="xpath", value="//*[@id='ctl00_CPH_MainContent_IrrigatorGrid_GridData']")
  get_name_rain_tab = rvest::read_html(get_name_rain$getElementAttribute('innerHTML')[[1]]) %>%
    rvest::html_table() %>% .[[1]]

  regner_tabelle=as.data.frame(matrix(NA,nrow(get_name_rain_tab),3))
  names(regner_tabelle)=c("Regner","ID","GUID")
  regner_tabelle[,1]=get_name_rain_tab[,2]

  for(j in 0:(nrow(regner_tabelle)-1)){
    get_link_rain=remDr$findElement(using="xpath", value=paste("//*[@id='ctl00_CPH_MainContent_IrrigatorGrid_ctl00__",j,"']",sep=""))
    get_link_rain_tab = rvest::read_html(get_link_rain$getElementAttribute('innerHTML')[[1]])%>%
      rvest::html_nodes("td") %>%
      rvest::html_nodes("p") %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href")
    regner_tabelle[(j+1),2]=paste("ctl00_CPH_MainContent_IrrigatorGrid_ctl00__",j,sep="")
    regner_tabelle[(j+1),3]=stringr::str_split(get_link_rain_tab,"=")[[1]][3]
  }

  for (i in 1:nrow(regner_tabelle)){
    #Navigiere zur Seite für Regner FAS49008
    remDr$navigate(paste("https://portal.myraindancer.com/Settings/RainerDetails.aspx?ai=settings&guid=",
                         regner_tabelle[i,3],sep=""))

    #Download Einsatzprotoll
    dwld_Einsatzprotokoll_Button = remDr$findElement("id", value="CPH_MainContent_ExportToExcelMission_btn")
    dwld_Einsatzprotokoll_Button$clickElement()

    #Switch zum Reiter Korrdinatenprotokoll
    switch_coordprotokoll_Button = remDr$findElement("xpath", "//*[@for='coordProtokoll']")
    switch_coordprotokoll_Button$clickElement()

    #Download Koordinatenprotokoll
    dwld_coordprotokoll_Button = remDr$findElement("id", value="CPH_MainContent_ExportToExcelCoordinates_btn")
    dwld_coordprotokoll_Button$clickElement()
    print(paste("Download",i,"/",nrow(regner_tabelle),"done",sep=" "))
  }
  #Logout
  logout_button = remDr$findElement("id", value="navbarLeftLogoutContainer")
  logout_button$clickElement()
  print("logged out")

  remDr$closeWindow()#close session
  remDr$closeServer()#close Server
  #driver$server$stop()#close Server
  #driver$client$closeServer()#close Server
  driver[["server"]]$stop()
  print("drivers closed")

  #copy files from download path to favourite path
  list_downloaded_files=list.files(sourcepath)
  list_downloaded_files_recent=list_downloaded_files[is.na(str_match(list_downloaded_files,"Einsatzprotokoll"))==F |
                                                       is.na(str_match(list_downloaded_files,"Koordinatenprotokoll"))==F &
                                                       (strptime(substr(list_downloaded_files,nchar(list_downloaded_files)-21,nchar(list_downloaded_files)-5),format='%Y-%m-%d_%H%M%S')>Sys.time()-60000)==T]

  folder_time=gsub("-","_",gsub(":","_",gsub(" ","_",substr(Sys.time(),1,19))))
  if(dir.exists(file.path(targetpath,"/downloaded_files",folder_time,sep="/"))==F){
    dir.create(file.path(targetpath,"/downloaded_files",folder_time,sep="/"), showWarnings = FALSE, recursive=TRUE)
  }

  file.copy(paste(sourcepath,list_downloaded_files_recent,sep="/"),
            paste(targetpath,"/downloaded_files",folder_time,list_downloaded_files_recent,sep="/"))

  file.remove(paste(sourcepath,list_downloaded_files_recent,sep="/"))
  print("files successfully copied")

  print("Done")

  ### start function to combine all single charts to one chart ---

  #export as shapefiles
  if(dir.exists(file.path(paste(targetpath,"/Beregnung_Shapefile/",sep="")))==F){
    dir.create(file.path(paste(targetpath,"/Beregnung_Shapefile/",sep="")), showWarnings = FALSE, recursive=TRUE)
  }

  WaterBalanceMod::DownloadRaindancerCombineCharts(sourcepath=paste(targetpath,"/downloaded_files/",sep=""),
                                           targetpath=paste(targetpath,"/Beregnung_Shapefile/",sep=""),
                                           start_date=paste(substr(Sys.Date(),1,4),"-01-01",sep=""),
                                           buffer_dist=buffer_dist)
}


