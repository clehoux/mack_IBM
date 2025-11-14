#2 choses à faire attention
##1) étant donné qu'un environnment Python est créer utiliser option ou R profile pour storer les password pourrait ne pas fonctionner
##2) il ne semble pas y avoir d'option d'overwrite. Donc, si un changement doit être apporté n'oubliez pas de supprimer le fichier avanty d'en créer un nouveau

##si output_filename et output_directory ne sont pas là, le fichier est enregistré dans le working directory avec un nom des limites de la requête.

#basé sur :
#https://help.marine.copernicus.eu/en/articles/8283072-copernicus-marine-toolbox-api-subset#h_ac2947af13
#et
#https://help.marine.copernicus.eu/en/articles/8638253-how-to-download-data-via-the-copernicus-marine-toolbox-in-r

source("user.R") # passwords for Glorys
#install_python()

#update pip# virtualenv_create(pip="25.1.1")
#virtualenv_create(envname = "CopernicusMarine")
#virtualenv_install("CopernicusMarine", packages = c("copernicusmarine"))
reticulate::use_virtualenv("CopernicusMarine", required = TRUE)
cm <- import("copernicusmarine")
#cm$login(CopernicusMarine_uid, CopernicusMarine_pwd) # only once

download.glorys_monthly = function (my.year){

for(y in my.year){#interim dataset not downloaded for the time being

result <- cm$subset(
  dataset_id = "cmems_mod_glo_phy_my_0.083deg_P1M-m",
  start_datetime=paste0(y,"-01-01T00:00:00"),
  end_datetime=paste0(y,"-12-31T00:00:00"),
  variables = list("thetao"),
  minimum_longitude = -78,
  maximum_longitude = -45,
  minimum_latitude = 35,
  maximum_latitude = 57.5,
  force_download = TRUE,
  minimum_depth=0,
  maximum_depth=50,
  output_filename = paste0("Glorys12V1_50m_",y,".nc"),
  output_directory = "data/glorys/monthly", 
  overwrite_output_data=T
)
}
}
#daily
download.glorys_daily = function (my.year){

for(y in my.year){#interim dataset not downloaded for the time being
 dates<- as.character(seq(ymd(paste0(y,"-01-01")), ymd(paste0(y,"12-31")), 1))

for(d in dates[1:365]){  
  
     result <- cm$subset(
    dataset_id = if_else(d <= "2021-05-31","cmems_mod_glo_phy_my_0.083deg_P1D-m" , "cmems_mod_glo_phy_myint_0.083deg_P1D-m"),
    start_datetime=paste0(d,"T00:00:00"),
    end_datetime=paste0(d,"T00:00:00"),
    variables = list("thetao", "so"),
    minimum_longitude = -78,
    maximum_longitude = -45,
    minimum_latitude = 35,
    maximum_latitude = 57.5,
    minimum_depth=9,
    maximum_depth=10.5,
    output_filename = paste0("Glorys12V1_10m_",d,".nc"),
    output_directory = "data/glorys/daily", 
    overwrite=T
  )
  cat(paste(d, "downloaded"))
  }
 }
}
  
  
  #satelite

#Dataset:
 # ESACCI-GLO-SST-L4-REP-OBS-SST contains: daily means to end 2016
#C3S-GLO-SST-L4-REP-OBS-SST contains: daily means from 2017 onwards

  download_sat = function(my.years, start_date="-01-01"){
    
    for(y in my.years){#interim dataset not downloaded for the time being
      if(y <2017) id= "ESACCI-GLO-SST-L4-REP-OBS-SST"
        if(y >= 2017) id="C3S-GLO-SST-L4-REP-OBS-SST"
        dates<- as.character(seq(ymd(paste0(y,start_date)), ymd(paste0(y,"12-31")), 1))
        
          for(d in dates[1:365]){  
          
      result <- cm$subset(
        dataset_id = id,
        start_datetime=paste0(d,"T00:00:00"),
        end_datetime=paste0(d,"T00:00:00"),
        variables = list("analysed_sst"),
        minimum_longitude = -78,
        maximum_longitude = -45,
        minimum_latitude = 35,
        maximum_latitude = 57.5,
        output_filename = paste0("GLO-SST-L4_",d,".nc"),
        output_directory = "data/sat/daily", 
        overwrite=T
      )
    }
  }
}


download.SEAPODYM_daily = function (my.year){
  
  for(y in my.year){#interim dataset not downloaded for the time being
    dates<- as.character(seq(ymd(paste0(y,"-01-01")), ymd(paste0(y,"12-31")), 1))
    
    for(d in dates[1:365]){  
      
      result <- cm$subset(
        dataset_id = "cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i",
        start_datetime=paste0(d,"T00:00:00"),
        end_datetime=paste0(d,"T00:00:00"),
        variables = list("npp", "zooc"),
        minimum_longitude = -78,
        maximum_longitude = -45,
        minimum_latitude = 35,
        maximum_latitude = 57.5,
        minimum_depth=9,
        maximum_depth=10.5,
        output_filename = paste0("SEAPODYM_",d,".nc"),
        output_directory = "data/seapodym/daily", 
        overwrite=T
      )
      cat(paste(d, "downloaded"))
    }
  }
}
