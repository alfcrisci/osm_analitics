
#########################################################################################################
# Authors : Alfonso Crisci 
# IBIMET CNR Institute of Biometeorology Firenze via Caproni 8,50145,Italia
# DCL FBK Trento : Cristian Consonni, Francesca De Chiara, Maurizio Napolitano                              
# mail: a.crisci@ibimet.cnr.it
# file: setup_geo.r
# github: https://github.com/alfcrisci/osm_analitics
# Esempi di cose che faccio con Cristian Consonni DCL Trento FBK
#########################################################################################################
################################################################################################## 
require(devtools)
# install_github('ramnathv/rCharts@dev')
# install_github('ramnathv/rMaps')
# install_github('chgrl/leafletR')
lib_geo=c("osmar","rgdal","raster","rasterVis","maptools","rjson","OpenStreetMap","reshape","reshape2","fractaldim","spatstat","splancs","ggmap","spatial.tools","bitops")
lib_lang=c("languageR","qdap","zipfR")
lib_utils=c("animation","data.table","lubridate","ggplot2","gridExtra","httr","bitops")
# lib_dev=c("rMaps","rCharts")

set_work_lib<-function(lib){
    if(lib %in% rownames(installed.packages())==FALSE){install.packages(lib)}}
	}
  
sapply(lib_geo,set_work_lib)
sapply(lib_lang,set_work_lib)
sapply(lib_utils,set_work_lib)
lapply(lib_geo,require, character.only=T)
lapply(lib_lang,require, character.only=T)
lapply(lib_utils,require, character.only=T)

