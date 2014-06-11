##################################################################################
# Authors : Alfonso Crisci 
# IBIMET CNR Institute of Biometeorology Firenze via Caproni 8,50145,Italia                              
# mail: a.crisci@ibimet.cnr.it
# file: create_gis.r
# github: https://github.com/alfcrisci/osm_analitics
# Esempi di cose che faccio con Cristian Consonni DCL Trento FBK
##################################################################################
# http://blog.revolutionanalytics.com/2012/07/making-beautiful-maps-in-r-with-ggmap.html
##################################################################################
# Set-up libraries  ( see Note in footer)

library(osmar)
library(sp)
library(rgdal)
library(raster)
library(maptools)
require(rjson)
require(ggmap)
##################################################################################

keylistosm_8_unique=readRDS("keylistosm_8_unique.rds")
citta_scelte=c("Matera","Trento","Firenze","Asti","Genova","Palermo","Milano","Lucca","Pistoia")
sel_admin_bbox=readRDS(file="selected_admin_bbox.rds")

##################################################################################
source("aux_function.r")
					  
##########################################################################################################################################################
setwd("D:\\lav_cantoro_presets\\lav_cantoro_osmar\\")

####################################################################################################################à
# Ricerca nodo admin_centre

capoluoghi=read.csv("capoluoghi_center.csv")

citta_scelte=c("Matera","Trento","Firenze","Asti","Genova","Palermo","Milano","Lucca","Pistoia")

res_coords=list()
for ( i in citta_scelte) { res_coords$ind[i]=grep(i,capoluoghi$location)
                         }

res_centri=list()
j=1
for (i in res_coords$ind){ 

res_centri[[j]]=find_admin_center_overpass(paste0(capoluoghi$location[i],"_boundary.osm"),capoluoghi$location[i],res_centri_box[[i]])
j=j+1
Sys.sleep(2)
}
mat_admin_centre=reshape::merge_all(res_centri)
write.csv(mat_admin_centre,"mat_admin_centre.csv",row.names=F)


mat_admin_centre=read.csv("mat_admin_centre.csv")

res_admin_box=list()
z=1
for (j in 12:16) { 
      	 bbox_temp=list()
         for (i in 1:nrow(mat_admin_centre)){ 
		                   bbox_centre=center_bbox_zoom(mat_admin_centre$lon[i],mat_admin_centre$lat[i],j)
                           bbox_temp$bbox[[i]]=recentering(bbox_centre,mat_admin_centre$lon[i],mat_admin_centre$lat[i])
					       bbox_temp$latcenter[i]=mat_admin_centre$lat[i]
						   bbox_temp$loncenter[i]=mat_admin_centre$lon[i]
						   bbox_temp$scala[i]=j
						   bbox_temp$nome[i]=as.character(mat_admin_centre$name[i])
						   #boxcentreToGIS(bbox_temp$nome[i],j,bbox_temp$bbox)	
					
                 }
				   	   
	    temp=data.frame(name=bbox_temp$nome,scales=bbox_temp$scala,loncenter=bbox_temp$loncenter,latcenter=bbox_temp$latcenter,
                   minlon=t(sapply(bbox_temp$bbox, function(x) c(x[1,1],x[2,1],x[1,2],x[2,2])))[,1],
				   minlat=t(sapply(bbox_temp$bbox, function(x) c(x[1,1],x[2,1],x[1,2],x[2,2])))[,3],
				   maxlon=t(sapply(bbox_temp$bbox, function(x) c(x[1,1],x[2,1],x[1,2],x[2,2])))[,2],
				   maxlat=t(sapply(bbox_temp$bbox, function(x) c(x[1,1],x[2,1],x[1,2],x[2,2])))[,4])
		  			   
				   
				   
res_admin_box[[z]]=temp
z=z+1
}
res_admin_box_df=reshape::merge_all(res_admin_box)

write.csv(res_admin_box_df,file="res_admin_box.csv",row.names=F)
saveRDS(res_admin_box,file="res_admin_box.rds")
##########################################################################################################################################################

  
