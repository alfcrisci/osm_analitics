#########################################################################################################
# Authors : Alfonso Crisci 
# IBIMET CNR Institute of Biometeorology Firenze via Caproni 8,50145,Italia
# DCL FBK Trento : Cristian Consonni, Francesca De Chiara, Maurizio Napolitano                              
# mail: a.crisci@ibimet.cnr.it
# file: auz_function.r
# github: https://github.com/alfcrisci/osm_analitics
# Esempi di cose che faccio con Cristian Consonni DCL Trento FBK
#########################################################################################################

find_nodecat_overpass=function(file,bbox,expr="place=*",remove=F){
                      require(osmar)
                      myURL <- paste0("http://overpass-api.de/api/xapi?node[bbox=",bbox[1,1],",",bbox[2,1],",",bbox[1,2],",",bbox[2,2],"][",expr,"]")
                      download.file(dest=file,myURL)
                      ciao_osm=get_osm(complete_file(), source = osmsource_file(file))
                      if (remove==TRUE){
                                       file.remove(file)
                                       }
                      return(ciao_osm)
}

find_admin_center_overpass=function(file,namecity,bbox,expr="boundary=administrative"){
                      require(osmar)
					  myURL <- paste0("http://overpass-api.de/api/xapi?relation[bbox=",bbox[1,1],",",bbox[2,1],",",bbox[1,2],",",bbox[2,2],"][",expr,"][@meta]")
                      download.file(dest=file,myURL)
                      ciao_osm=get_osm(complete_file(), source = osmsource_file(file))
					  node_admin=ciao_osm$relations$refs[which(ciao_osm$relations$refs$role=="admin_centre"),]$ref					  
				      res=data.frame(lat=NA,lon=NA,name=namecity,nodes=NA)
					  for (i in 1:length(node_admin)){  aa=get_osm(node(node_admin[i]))
					                                       res$nodes=node_admin[i]
					                                 if (as.character(aa$node$tags$v[which(aa$nodes$tags$k=="name")]) == namecity)
													    { res$lat[1]= aa$nodes$attrs$lat;res$lon[1]= aa$nodes$attrs$lon;
														} 														
					                                }					  
                      #file.remove(file)
					  return(res)
}



deg2num<-function(lat_deg, lon_deg, zoom){
                 # Reference: http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Tile_bounding_box
                 # http://www.r-bloggers.com/creating-a-zoomable-map-of-tweets-with-r/
                 lat_rad <- lat_deg * pi /180
                 n <- 2.0 ^ zoom
                 xtile <- floor((lon_deg + 180.0) / 360.0 * n)
                 ytile = floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)
                 return( c(xtile, ytile))
                 # return(paste(paste("http://a.tile.openstreetmap.org", zoom, xtile, ytile, sep="/"),".png",sep=""))

}
tile2lat <- function (y, zoom) {
                             n = 2 ^ zoom
                             rad = atan(sinh(pi * (1 - 2 * y / n)))
                             return (rad * 360 / (2.0 * pi));
}
 
tile2lon <- function(x, z) {
                    return ((x / (2.0 ^ z)) * 360.0 - 180);
}
 
tile2boundingBox <- function(z, x, y) {
                    #http://wiki.openstreetmap.org/wiki/Tiles@home/Zoom_levels
                    north = tile2lat(y, z);
                    south = tile2lat(y + 1, z);
                    west = tile2lon(x, z);
                    east = tile2lon(x + 1, z);
					bbox=matrix(NA,2,2)
					bbox[1,1]=west
					bbox[2,1]=south
					bbox[1,2]=east
					bbox[2,2]=north
					return (bbox);
}
 
center_bbox_zoom=function(lon,lat,zoom) {
                                 xytile=deg2num(lat,lon,zoom)
                                 return(tile2boundingBox(zoom,xytile[1],xytile[2]))
                          }
						  
spFromOSM_p <- function(source, index, type='points'){
  require(osmar)
  idx <- find_down(source, index)
  obj <- subset(source, ids=idx)
  objSP <- as_sp(obj, type)
  }
bboxToGIS<-function (citta,scala,minlon,minlat,maxlon,maxlat)
{   require('sp', quietly = TRUE)
    require('raster', quietly = TRUE)
	require('rgdal', quietly = TRUE)
    bb=data.frame(x=c(minlon,maxlon),y=c(minlat,maxlat))
    cc=rbind(c(bb$x[1],bb$y[1]),c(bb$x[2],bb$y[1]),c(bb$x[2],bb$y[2]),c(bb$x[1],bb$y[2]),c(bb$x[1],bb$y[1]))
	df_pol=data.frame(name=citta,scala=scala)
	rownames(df_pol)[1]=1
	pols=SpatialPolygons(list(Polygons(list(Polygon(cc)),ID=1)))
    box_sp=SpatialPolygonsDataFrame(pols,df_pol)
    
	proj4string(box_sp) = CRS("+init=epsg:4326")
    box_sp_3035<- spTransform(box_sp, CRS("+init=epsg:3035"))
	r <- raster(extent(box_sp_3035))
    res(r)=100
	values(r)=0
	proj4string(r) = CRS("+init=epsg:3035")
   
	rgeo=projectRaster(r, crs="+init=epsg:4326") 
	writeRaster(rgeo,filename=paste0(citta,"_",scala,".tif"),format="GTiff",overwrite=T)
    writeOGR(box_sp, paste0(citta,"_",scala,".json"), layer=".", driver="GeoJSON")
	return(1)
}

boxcentreToGIS<-function (citta,scala,bbox_centre)
{   require('sp', quietly = TRUE)
    require('raster', quietly = TRUE)
	require('rgdal', quietly = TRUE)
    bb=data.frame(x=bbox_centre[,1],y=bbox_centre[,2])
    cc=rbind(c(bb$x[1],bb$y[1]),c(bb$x[2],bb$y[1]),c(bb$x[2],bb$y[2]),c(bb$x[1],bb$y[2]),c(bb$x[1],bb$y[1]))
	df_pol=data.frame(name=citta,scala=scala)
	rownames(df_pol)[1]=1
	pols=SpatialPolygons(list(Polygons(list(Polygon(cc)),ID=1)))
    box_sp=SpatialPolygonsDataFrame(pols,df_pol)
    
	proj4string(box_sp) = CRS("+init=epsg:4326")
    box_sp_3035<- spTransform(box_sp, CRS("+init=epsg:3035"))
	r <- raster(extent(box_sp_3035))
    res(r)=100
	values(r)=0
	proj4string(r) = CRS("+init=epsg:3035")
   
	rgeo=projectRaster(r, crs="+init=epsg:4326") 
	writeRaster(rgeo,filename=paste0(citta,"_",scala,".tif"),format="GTiff",overwrite=T)
    writeOGR(box_sp, paste0(citta,"_",scala,".json"), layer=".", driver="GeoJSON")
	return(1)
}

recentering=function (bb,loncenter,latcenter) { wlon=diff(bb[1,])/2
                                                            wlat=diff(bb[2,])/2
															bbnew=bb
															bbnew[1,1]=loncenter-wlon
															bbnew[2,1]=loncenter+wlon
															bbnew[1,2]=latcenter-wlat
															bbnew[2,2]=latcenter+wlat
															return(bbnew)
                          }

						  
						  
######################################################################################
add.alpha <- function(col, alpha=1){
             if(missing(col))
             stop("Please provide a vector of colours.")
             apply(sapply(col, col2rgb)/255, 2,
             function(x)
             rgb(x[1], x[2], x[3], alpha=alpha))
}

###################################################################################################################################

ggplotZipf<- function (fit,citta,tag,scala=12) {
require(ggplot2)
significance="No Significant."
if ( signif(summary(fit)$coefficients[2,4], 5) < 0.05) {significance="Significant"}

note=paste("\n Zipf fitting: \n Inter. =",signif(fit$coef[[1]],5 ),
                                         "\nSlope  =",signif(fit$coef[[2]], 3),
										 "\n R2 = ",signif(summary(fit)$adj.r.squared, 2),
                                          "\n",significance);
placey=intercept=fit$coef[[1]]*0.2
placex=intercept=abs(fit$coef[[1]]*0.2)									  
ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + geom_point() + stat_smooth(method = "lm", col = "red") + xlab("Log Word Freq") + ylab("Log Word Rank")+geom_abline(slope=-1,intercept=fit$coef[[1]],color="blue",size = 2)+ annotate("text",x=placex,y=placey,label=note,color="red")+ ggtitle(paste(citta," Tag:",tag," OSM Scale:",scala))+coord_cartesian(xlim=c(-0.1, fit$coef[[1]]),ylim=c(-0.1, fit$coef[[1]]))
   
  }
  
DTM_zipfs_model=function(x) {
    require(tm)
    y <- log(sort(slam::col_sums(x), decreasing = TRUE))
    x <- log(seq_along(y))
    mod <- lm(y ~ x)
	return(mod)
}
###################################################################################################################################
# function to visualize raster using trellis object and RasterVis R library 

rastervismap <- function(p, map, ...){
bbMap <- attr(map, 'bb')
latCenter <- with(bbMap, ll.lat + ur.lat)/2
lonCenter <- with(bbMap, ll.lon + ur.lon)/2
height <- with(bbMap, ur.lat - ll.lat)
width <- with(bbMap, ur.lon - ll.lon)
p+
latticeExtra::layer({
grid.raster(map,
x=lonCenter, y=latCenter,
width=width, height=height,
default.units='native')
}, under=TRUE,
data=list(map=map,
lonCenter=lonCenter, latCenter=latCenter,
width=width, height=height))
}

osm2ggmap=function(map_longlat) {
          require(OpenStreetMap)
          map_raster=raster(map_longlat)
          mapcol=rgb(as.vector(map_raster[[1]]),as.vector(map_raster[[2]]),as.vector(map_raster[[3]]),maxColorValue=255)
          map=matrix(mapcol,map_raster@nrows,map_raster@ncols)
          class(map) <- c('ggmap','raster')
          # map spatial info
          attr(map, 'bb') <- data.frame(
          ll.lat = map_longlat$bbox$p2[2], ll.lon = map_longlat$bbox$p1[1],
          ur.lat = map_longlat$bbox$p1[2], ur.lon = map_longlat$bbox$p2[1])
          return(map)
    }
	
###################################################################################################################################

plot_custom = function(before, this, total_by_timeunit, timeunit_pretty, basemap) {
    if (nrow(this) < 20) 
        return()  # density plots don't make sense until you have 20 points or so
    this_timeunit = unique(this$timeunit)
    ## Lets plot! We'll make a heatmap of just the current (ie, 'this') data
    ## here.  I'll make a density2d-polygon. As long as you preserve the
    ## aes(x=lon, y=lat), you can play with different styles of plots
	
    plot <- basemap + geom_polygon(data = this, aes(x = lon, y = lat, fill = ..level..), 
        alpha = 0.5, stat = "density2d") + scale_fill_gradient(low = "yellow", 
        high = "red") + blank_theme() + labs(x = "Data © OpenStreetMap contributors", 
        title = paste(timeunit_pretty, this_timeunit, sep = ": "))
    print(plot)
}
	
###################################################################################################################################
# file per analisi lessicale

filter_file_key=function(name="Trento",scale=12,key,file="italy.osh.pbf") {

               system(paste0("osmfilter ",file, " --keep=  --keep-ways=\"",key,"=\" --keep-relations=\"",key,"=\" -o=",name,"_",scale,"_",key,".osm"))
                }
				#osmfilter Firenze_12_osh.osm --keep=  --keep-ways="natural=" --keep-relations="natural=" -o=Firenze_12_osh_natural.osm
                #osmfilter Firenze_12_osh.osm --keep=  --keep-ways="natural=" --keep-relations="natural=" -o=Firenze_12_osh_natural.osm					

create_stat_keyvalue=function(name="Trento",scale=12,key,file) {
               fileout=paste0(name,"_",scale,"_",key,"_f.csv")
			   sink("temp.bat")
               cat(paste0("osmfilter ",file, " --out-key=",key," > ",fileout))
			   sink()
			   system("temp.bat")
			   file.remove("temp.bat")
			   }
create_list_key=function(name="Trento",scale=12,file) {
               fileout=paste0(name,"_",scale,"_keys_f.csv")
			   sink("temp.bat")
               cat(paste0("osmfilter ",file, " --out-count > " ,fileout))
			   sink()
			   system("temp.bat")
			   file.remove("temp.bat")
			   }		
			   
list_file_key=function(name="Trento",scale=12,key) {
                 file=paste0(name,"_",scale,"_",key,"_f.csv")
			   
               filelines=read.csv(file,sep="\t",header=F)
			   res=list()
			   ind_names=grep("name",filelines$V2)
			   ind_names_tag=grep("^name:",filelines$V2)
			   ind_source=grep("source",filelines$V2)
			   ind_addr=grep("addr:",filelines$V2)
			   ind_ref=grep("ref:",filelines$V2)
			   ind_def=grep("def:",filelines$V2)
			   full_desc=c(ind_names,ind_addr,ind_names_tag,ind_ref,ind_def,ind_source)
			   full_names=c(ind_names,ind_names_tag)
			   full_refer=c(ind_ref,ind_source)
			   ind.na=filelines[full_desc,]=NA
			   corpora=filelines[!is.na(filelines$V2),]
			   corpora_names=filelines[full_names,]
			   corpora_ref=filelines[full_refer,]
			   res$words=rep(as.character(corpora$V2),corpora$V1)
			   res$uniquewords=unique(rep(as.character(corpora$V2),corpora$V1))
			   res$words_names=rep(as.character(corpora_names$V2),corpora_names$V1)
			   res$words_ref=rep(as.character(corpora_ref$V2),corpora_ref$V1)
			   return(res)			   
                }
list_file_key_simple=function(name="Trento",scale=12,key) {
                 file=paste0(name,"_",scale,"_",key,"_f.csv")
			   
               filelines=read.csv(file,sep="\t",header=F)
			   res=list()
			   ind_names=grep("name",filelines$V2)
			   ind_names_tag=grep("^name:",filelines$V2)
			   ind_source=grep("source",filelines$V2)
			   ind_addr=grep("addr:",filelines$V2)
			   ind_ref=grep("ref:",filelines$V2)
			   ind_def=grep("def:",filelines$V2)
			   full_desc=c(ind_names,ind_addr,ind_names_tag,ind_ref,ind_def,ind_source)
			   full_names=c(ind_names,ind_names_tag)
			   full_refer=c(ind_ref,ind_source)
			   ind.na=filelines[full_desc,]=NA
			   corpora=filelines[!is.na(filelines$V2),]
			    res$words=rep(as.character(corpora$V2),corpora$V1)
			   res$uniquewords=unique(rep(as.character(corpora$V2),corpora$V1))
			    return(res)			   
                }				
###################################################################################################################################

read_OSM_full <- function(osm_file, osmconvert = 'osmconvert', columns = '@lat @lon @timestamp @user @id @version') {
    ## Verify that osm_file exists
    osm_file = normalizePath(osm_file)
    if(!file.exists(osm_file)) { stop("Could not find file: ", osm_file)}
    ## If input file is osm (xml) or pbf, make csv file in the same directory / with the same name
    if(tools::file_ext(osm_file) %in% c('osm', 'pbf')) {
        ## First, verify that osmconvert can be run from the command line
        cmd.fun = if (.Platform$OS.type == 'windows') shell else system
        tryCatch(cmd.fun(sprintf('%s -h', osmconvert), intern = TRUE, ignore.stdout = TRUE), 
                 error = function(e) { stop("Could not find command: ", osmconvert )})
        ## Time to convert to csv. Pick the same basename and directory as the input file.
        osm_file_basename = tools::file_path_sans_ext(osm_file)
        cmd.fun(sprintf('%s %s --csv="%s" --csv-separator="," -o=%s.csv',
                        osmconvert, osm_file, columns, osm_file_basename))
        osm_file = sprintf("%s.csv", osm_file_basename)
        stopifnot(file.exists(osm_file)) # Verify that output happened correctly
    } else if (tools::file_ext(osm_file) != 'csv') {
        stop("File with that extension not support. Please report a bug if it should be.")
    }
    ## Finally, read the csv file, convert into a data.table, convert timestamp, and return
    dt = data.table(setNames(read.csv(osm_file, header = F), 
                             stringr::str_replace(unlist(strsplit(columns, " ")), "@", "")))
    dt$timestamp = ymd_hms(dt$timestamp) #dt[, timestamp := ymd_hms(timestamp)]
	dt$year=as.numeric(format(as.Date(dt$timestamp), format = "%Y"))
	dt$period=as.numeric(Sys.Date()-as.Date(dt$timestamp))
    dt
}
##############################################################################################################à
	
lacunarity <- function(file,winpixel=5) {
outfile=gsub(".tif",".txt",file)
sink("temp_lac.bat")
cat(paste0("r_lacunarity_old.exe -g ",winpixel," -i ",file," >",outfile))
sink()
system("temp_lac.bat")
file.remove("temp_lac.bat")
f=readLines(outfile)
file.remove(outfile)
res=read.table(textConnection(c(f[2],f[3])),header=T,sep="\t")
return(res$Lacunarity.index[1])
}

frac.dim.box <- function(file,winpixel=5) {
outfile=gsub(".tif",".txt",file)
sink("temp_frac.bat")
cat(paste0("r.fdim.boxcount.exe --minbox=",winpixel," ",file," >",outfile))
sink()
system("temp_frac.bat")
file.remove("temp_frac.bat")
f=readLines(outfile)
file.remove(outfile)
res=read.table(textConnection(c(f[32])),sep=":")
return(res$V2)
}