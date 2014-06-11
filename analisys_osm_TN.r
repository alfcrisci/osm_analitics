#########################################################################################################
# Authors : Alfonso Crisci 
# IBIMET CNR Institute of Biometeorology Firenze via Caproni 8,50145,Italia
# DCL FBK Trento : Cristian Consonni, Francesca De Chiara, Maurizio Napolitano                              
# mail: a.crisci@ibimet.cnr.it
# file: analisy_osm_MT.r
# github: https://github.com/alfcrisci/osm_analitics
# MATERA
#  A Map animation for feature
#  
#########################################################################################################
#########################################################################################################
# Set-up libraries  ( see Note in footer) check java dependencies on 64 bit and R packages availability

lib_geo=c("osmar","rgdal","raster","rasterVis","maptools","rjson","OpenStreetMap","reshape","reshape2","fractaldim","spatstat","splancs","ggmap")
lib_lang=c("languageR","qdap","zipfR")
lib_dev=c("rMaps","rCharts")
lib_utils=c("animation","data.table","lubridate","ggplot2","gridExtra","treemap")

lapply(lib_geo, require, character.only=T)
lapply(lib_lang, require, character.only=T)
lapply(lib_utils, require, character.only=T)


##################################################################################
# Set up working dir : to redefine if necessary

setwd("D:\\lav_cantoro_presets\\lav_cantoro_osmar")

# Load local supplementary functions

source("sem_functions.r")
source("aux_function.r")
source("TimeLapse.r")

##################################################################################
# define animation options

ani.options(ffmpeg = shQuote("C:\\Program Files\\ffmpeg\\bin\\ffmpeg.exe"),
            convert = shQuote("C:\\Program Files\\ImageMagick-6.7.4-Q16\\convert.exe"),
            ani.width = 800, ani.height = 800, interval = 0.05)  

##################################################################################
# defining working color palette adding trasparency

morab_pal=c("#00CC00","#FFFF00","#FF7519","#FF0000","#FF00FF")
cols <- colorRampPalette( c(morab_pal))(100)
morabcols <- add.alpha(cols,alpha=0.4)


######################################################################################
# Define keys ( and 1 tag useful) and load working data

key_sel=c("shop","amenity","tourism","man_made","natural","leisure","public_transport","wikipedia")


###################################################################################################################################
# Create year animation of feauture
trento_12_osh=read_OSM("trento_12_osh.osm")
basemap_type = "osm";
basemap_TN_12=get_ggbasemap(lat_range = range(trento_12_osh$lat, na.rm = T),lon_range = range(trento_12_osh$lon, na.rm = T), type = "osm", verbose = T,cache=F)
saveGIF({time_lapse(trento_12_osh, "year", basemap=basemap_TN_12,highlight="red",verbose = FALSE)}, movie.name = "trento_yearly.gif", interval = 2,extra.opts = "-dispose Background",clean = F, outdir = getwd())
gmap <- osm2ggmap(basemap_TN_12)

######################################################################################################################################à                               
# define structure extract data 

create_list_key(name="Trento",scale=12,file="Trento_12_osh.osm")
keys_TN=list_file_key_simple(name="Trento",scale=12,key="keys")	
keys_corpus_TN=generateCorpus_eng(paste(keys_TN$words,collapse=" "))
png("keys_corpus_TN_cloud.png",width = 1000, height = 800)
wordcloud.osm(keys_corpus_TN,nsat=5,palette=rainbow(100))
dev.off()
key_tagmatrix_TN=DocumentTermMatrix(keys_corpus_TN)
key_zipf_TN=DTM_zipfs_model(key_tagmatrix_TN)                                
ggplotZipf(key_zipf_TN,"Trento","keys")
ggsave("Keys_Zipf_TN.png",width = 8,height = 8,dpi = 600) 
dev.off()
#######################################################à

key_sel=c("shop","amenity","tourism","man_made","natural","leisure","landuse","wikipedia")

res_tags_associated_TN=list()
res_mat_keys_TN=list()
res_mat_keys_uniqueid_TN=list()
res_mat_users_unique_TN=list()
res_mat_tags_corpus_TN=list()
key_tagmatrix_TN=list()
res_mat_tags_zipf_TN=list()

for ( i in 1:length(key_sel)) {
                                
								filter_file_key(name="Trento",scale=12,key=key_sel[i],file="Trento_12_osh.osm")
                                create_stat_keyvalue(name="Trento",scale=12,key=key_sel[i],file=paste0("Trento_12_",key_sel[i],".osm"))
                                res_tags_associated_TN[[i]]=list_file_key_simple(name="Trento",scale=12,key=key_sel[i])$words	
                                columns = paste('@lat @lon @timestamp @user @id @version',key_sel[i])
								res_mat_keys_TN[[i]]=read_OSM_full(paste0("Trento_12_",key_sel[i],".osm"),columns=columns)
				                res_mat_keys_TN[[i]]$key=key_sel[i]
								res_mat_users_unique_TN[[i]]=length(unique(res_mat_keys_TN[[i]]$user))
                                res_mat_keys_uniqueid_TN[[i]]=as.data.frame(res_mat_keys_TN[[i]][which(!duplicated(res_mat_keys_TN[[i]]$id))],)
								tags=as.character(as.data.frame(res_mat_keys_uniqueid_TN[[i]])[,7])
								tagsn=tags[which(!tags=="")]
			                    res_mat_tags_corpus_TN[[i]]=generateCorpus_eng(paste(tagsn,collapse=" "))
                                key_tagmatrix_TN[[i]]=DocumentTermMatrix(res_mat_tags_corpus_TN[[i]])
                                res_mat_tags_zipf_TN[[i]]=DTM_zipfs_model(key_tagmatrix_TN[[i]])                                
                                ggplotZipf(res_mat_tags_zipf_TN[[i]],"Trento",key_sel[i])
                                ggsave(paste0(key_sel[i],"_TN_Zipf.png"),width = 8,height = 8,dpi = 600) 
								try(dev.off())
								names(res_mat_keys_uniqueid_TN[[i]])[7]="keyvalue"
                                png(paste0(key_sel[i],"_TN_wcloud.png"))
                                wordcloud.osm(res_mat_tags_corpus_TN[[i]],nsat=2,palette=rainbow(100))
                                dev.off()
								png(paste0(key_sel[i],"_TN_treemap_user_key.png"))
                                treemap(res_mat_keys_uniqueid_TN[[i]], index=c("key", "user"),vSize="version",type="index",title=paste("Trento",key_sel[i],"TreeMap Key - User"))
							    dev.off()
								png(paste0(key_sel[i],"_TN_treemap_key_value.png"))
                                treemap(res_mat_keys_uniqueid_TN[[i]][which(!tags==""),], index=c("key", "keyvalue"),vSize="version",type="index",title=paste("Trento",key_sel[i],"TreeMap Key with Value "))
							    dev.off()
								
								
}

# Merging data.table of each key

res_mat_keyvalues_df=reshape::merge_all(res_mat_keys_uniqueid_TN)


# Create data.frame of user engaged

keyset_users_TN=data.frame(Nuser=c(unlist(res_mat_users_unique_TN)),Keyset=factor(c(key_sel)))


# Read all features of area

res_mat_keys_All=read_OSM_full("Trento_12_osh.osm")

# Create a vector and identify the length
res_mat_users_unique_All=length(unique(res_mat_keys_All$user))
ggplot(data=keyset_users_TN, aes(x=Keyset, y=Nuser))+ geom_bar(aes(fill=Keyset),stat="identity")+geom_text(aes(label=Nuser), vjust=-0.5)+ ggtitle(paste("Trento Users Engaged in Keys (Ntot=",res_mat_users_unique_All,")"))
ggsave("Trento_12_users_engaged_keys.png", width = 8,height = 8,dpi = 600)                            

div_mod_TN <-diversity(res_mat_keyvalues_df$keyvalue, list(res_mat_keyvalues_df$key))
png("keys_diversity_TN_cloud.png",idth = 1500, height = 1300)
try(plot(div_mod_TN, high = "red", low = "yellow", grid = T, values = TRUE))
dev.off()

dis_mod_TN <-Dissimilarity(as.character(res_mat_keyvalues_df$keyvalue), list(as.character(res_mat_keyvalues_df$user)))
fit <- hclust(dis_mod_TN)
png("Cluster_user_TN_user_keyvalue.png",width = 1500, height = 1300)
plot(fit,main="Clustering Users by keyvalue wordset")
rect.hclust(fit, k=5, border=c("red", "purple", "seagreen","green","violet"))
dev.off()

dis_mod_TN <-Dissimilarity(as.character(res_mat_keyvalues_df$key), list(as.character(res_mat_keyvalues_df$user)))
fit <- hclust(dis_mod_TN)
png("Cluster_user_TN_user_key.png",width = 1500, height = 1300)
plot(fit,main="Clustering Users by key wordset")
rect.hclust(fit, k=5, border=c("red", "purple", "seagreen","green","violet"))
dev.off()

dis_mod_TN <-Dissimilarity(as.character(res_mat_keyvalues_df$user), grouping.var =res_mat_keyvalues_df$year)
fit <- hclust(dis_mod_TN)
png("Cluster_user_TN_user_period.png",width = 1500, height = 1300)
plot(fit,main="Clustering Year by User engaged")
rect.hclust(fit, k=2, border=c("red","seagreen"))
dev.off()

png("word_network_trento_keys_users.png",width = 1500, height = 1300)
word_network_plot(res_mat_keyvalues_df$keyvalue, grouping.var=res_mat_keyvalues_df$user,title.name = "MATERA KeyTagset network by users")
dev.off()  
                   
# word_network_plot(res_mat_keys_df$user, grouping.var=res_mat_keys_df$key )

png("Venn_diagram_trento_keys_users.png",width = 1000, height = 800)
trans_venn(res_mat_keyvalues_df$user, grouping.var=res_mat_keyvalues_df$key,title.name = "MATERA User and keys Venn  Diagram",legend.cex = 1.5, legend.location = "topright")
dev.off()

#####################################################################################
# Saving analisys

saveRDS(res_tags_associated_TN,"res_tags_associated_TN.rds")
saveRDS(res_mat_keys_TN,"res_mat_keys_TN.rds")
saveRDS(res_mat_keys_uniqueid_TN,"res_mat_keys_uniqueid_TN.rds")
saveRDS(res_mat_users_unique_TN,"res_mat_users_unique_TN.rds")
saveRDS(res_mat_users_unique_TN,"res_mat_users_unique_TN.rds")
saveRDS(res_mat_tags_corpus_TN,"res_mat_tags_corpus_TN.rds")
saveRDS(key_tagmatrix_TN,"key_tagmatrix_TN.rds")
saveRDS(res_mat_tags_zipf_TN,"res_mat_tags_zipf_TN.rds")
saveRDS(keyset_users_TN,"keyset_users_TN.rds")
saveRDS(res_mat_keyvalues_df,"res_mat_keyvalues_df_TN.rds")
###################################################################################################################################################àà
# Analisi per keys  analisi lessicale da fare il grafo di zipf

# Read raster 100 meter of city
r100=raster("Trento_12.tif")

r=raster("Trento_12.tif")
res(r)=res(r)/4

#######################################################################################################################################
# Read file complete with history
	
res_mat_keys_All=read_OSM_full("Trento_12_osh.osm")

# Create a vector and identify the length

res_mat_users_unique_All=length(unique(res_mat_keys_All$user))

# Eliminate version of feature

res_mat_keys_uniqueid_All=res_mat_keys_All[which(!duplicated(res_mat_keys_All$id)),]

# Create spatial objects

res_mat_keys_uniqueid_All_sp=na.omit(res_mat_keys_uniqueid_All)
coordinates(res_mat_keys_uniqueid_All_sp) = ~lon+lat

res_mat_keys_All_sp=na.omit(res_mat_keys_All)
coordinates(res_mat_keys_All_sp) = ~lon+lat

#######################################################################################################################################
# names(res_mat_keys_uniqueid_All)
# [1] "lat"       "lon"       "timestamp" "user"      "id"        "version"  
# [7] "year"      "period"   
#######################################################################################################################################
# Instance list and plotting

res_raster_full=list()
gmap <- osm2ggmap(basemap_TN_12)

# feature density #######################
res_raster_full$r_all_count <- rasterize(res_mat_keys_uniqueid_All_sp, r, 'id',fun=function(x,...)length(x))
png("feature_density_TN.png",width = 1000, height = 800)
p <- levelplot(res_raster_full$r_all_count,layers=1,margin=F,par.settings=RdBuTheme(region=terrain.colors(100)),alpha.regions=0.5)
rastervismap(p,gmap,margin=F,par.settings=RdBuTheme(region=terrain.colors(100)),alpha.regions=0.5)
dev.off()

res_raster_full$r_all_count100 <- rasterize(res_mat_keys_uniqueid_All_sp, r100, 'id',fun=function(x,...)length(x))
png("feature_density_TN_100m.png",width = 1000, height = 800)
p <- levelplot(res_raster_full$r_all_count100,layers=1,margin=F,par.settings=RdBuTheme(region=terrain.colors(100)),alpha.regions=0.5)
rastervismap(p,gmap,margin=F,par.settings=RdBuTheme(region=terrain.colors(100)),alpha.regions=0.5)
dev.off()




# user count #################################

res_raster_full$r_users_count <- rasterize(res_mat_keys_uniqueid_All_sp, r, 'user',fun=function(x,...)length(unique(x)))
png("r_users_count_TN.png",width = 1000, height = 800)

p <- levelplot(res_raster_full$r_users_count,layers=1,margin=F,par.settings=RdBuTheme(region=terrain.colors(100)),alpha.regions=0.5)
rastervismap(p,gmap,margin=F,par.settings=RdBuTheme(region=terrain.colors(100)),alpha.regions=0.5)
dev.off()

# version count #################################

res_raster_full$r_version_count <- rasterize(res_mat_keys_All_sp, r, 'version',fun=mean)
png("r_version_count_TN.png",width = 1000, height = 800)
p <- levelplot(res_raster_full$r_version_count,layers=1,margin=F,par.settings=RdBuTheme(region=terrain.colors(100)),alpha.regions=0.5)
rastervismap(p,gmap,margin=F,par.settings=RdBuTheme(region=terrain.colors(100)),alpha.regions=0.5)
dev.off()

# time_recent #################################

res_raster_full$r_time_recent <- rasterize(res_mat_keys_All_sp, r, 'period',fun=min)
png("r_time_recent_TN.png",width = 1000, height = 800)
p <- levelplot(res_raster_full$r_time_recent,layers=1,margin=F,par.settings=RdBuTheme(region=terrain.colors(100)),alpha.regions=0.5)
rastervismap(p,gmap,margin=F,par.settings=RdBuTheme(region=terrain.colors(100)),alpha.regions=0.5)
dev.off()

# time_mean #################################

res_raster_full$r_time_mean <- rasterize(res_mat_keys_All_sp, r, 'period',fun=mean)
png("r_time_mean_TN.png",width = 1000, height = 800)
p <- levelplot(res_raster_full$r_time_recent,layers=1,margin=F,par.settings=RdBuTheme(region=terrain.colors(100)),alpha.regions=0.5)
rastervismap(p,gmap,margin=F,par.settings=RdBuTheme(region=terrain.colors(100)),alpha.regions=0.5)
dev.off()


# time_far #################################


res_raster_full$r_time_far <- rasterize(res_mat_keys_All_sp, r, 'period',fun=max)
png("r_time_far_TN.png",width = 1000, height = 800)
p <- levelplot(res_raster_full$r_time_far ,layers=1,margin=F,par.settings=RdBuTheme(region=terrain.colors(100)),alpha.regions=0.5)
rastervismap(p,gmap,margin=F,par.settings=RdBuTheme(region=terrain.colors(100)),alpha.regions=0.5)
dev.off()

# ratio feauture users #################################

res_raster_full$r_node_users=res_raster_full$r_all_count/res_raster_full$r_users_count
png("r_node_users_TN.png",width = 1000, height = 800)
p <- levelplot(res_raster_full$r_node_users,layers=1,margin=F,par.settings=RdBuTheme(region=terrain.colors(100)),alpha.regions=0.5)
rastervismap(p,gmap,margin=F,par.settings=RdBuTheme(region=terrain.colors(100)),alpha.regions=0.5)
dev.off()


# save raster list

saveRDS(res_raster_full,"res_raster_full_TN.rds")


#######################################################################################################################################
# Lacunarity Spatial analisys

# define range of years

range_year=range(res_mat_keys_All_sp@data$year)

# define data structures
res_raster_year=list()
res_raster_year_frac=list()
res_raster_year_fd=list()
res_value_year_lac=list()
res_value_year_fd_count=list()
res_value_feature_count=list()

j=1

for ( i in as.numeric(range_year[1]+1):(as.numeric(range_year[2]))) {
                                         temp=res_mat_keys_All_sp[which(res_mat_keys_All_sp@data$year <= i),]
					                     temp=temp[!duplicated(temp@data$id),]
					                     r_temp<-rasterize(temp, r,background=0,'id',fun=function(x,...)length(x))
					                     res_value_feature_count[[j]]=sum(getValues(r_temp))
                                         r_temp[r_temp>1]=1
                                         fd2d <- fd.estimate(as.matrix(r_temp), methods="isotropic",window.size=5,step.size=2)
					                     fd=na.omit(data.frame(x=as.vector(fd2d$scale[,,1]),y=as.vector(fd2d$fd[,,1])))
					                     fit=lm(y ~ x,data=fd)
					                     slope=abs(signif(fit$coef[[1]], 5))-1
					                     res_raster_year_fd[[j]]=slope
                                         mask_data=matrix(data = as.matrix(fd2d$fd), nrow = nrow(fd2d$fd), ncol = ncol(fd2d$fd), byrow = FALSE)
                                         r_frac <- raster(mask_data)
                                         extent(r_frac ) <- extent(r_temp)
                                         proj4string(r_frac ) <- CRS(proj4string(r_temp))
                                         res_raster_year[[j]]=r_temp
					                     res_raster_year_frac[[j]]=r_frac
                                         writeRaster(r_temp, paste0("trento_node_12_",i,".tif"),format="GTiff", overwrite=TRUE)
                                   	     writeRaster(r_frac, paste0("trento_FDvoid_12_",i,".tif"),format="GTiff", overwrite=TRUE)
					                     res_value_year_lac[[j]]=lacunarity(paste0("trento_node_12_",i,".tif"))
					                     res_value_year_fd_count[[j]]=frac.dim.box(paste0("trento_node_12_",i,".tif"))
                                         j=j+1					 
                                        }

png("r_fractal_dim_TN.png")
p <- levelplot(res_raster_year_frac[[j-1]]-1,layers=1,margin=F,par.settings=RdBuTheme(region=terrain.colors(100)),alpha.regions=0.5)
rastervismap(p,gmap,margin=F,par.settings=RdBuTheme(region=terrain.colors(100)),alpha.regions=0.5)
dev.off()										
								
# create data.frame of variables of interest

time_fd=data.frame(Anno=as.Date(paste0(as.numeric(range_year[1]+1):(as.numeric(range_year[2])),"-1-1")),Fd=unlist(res_raster_year_fd))
time_lac=data.frame(Anno=as.Date(paste0(as.numeric(range_year[1]+1):(as.numeric(range_year[2])),"-1-1")),Lac=unlist(res_value_year_lac))
time_fdc=data.frame(Anno=as.Date(paste0(as.numeric(range_year[1]+1):(as.numeric(range_year[2])),"-1-1")),Fd=unlist(res_value_year_fd_count))
time_fea=data.frame(Anno=as.Date(paste0(as.numeric(range_year[1]):(as.numeric(range_year[2])),"-1-1")),Feature=unlist(res_value_feature_count))

# Produce graphs lacunarity, fractal dimension e year feature volume


ggplot(time_lac, aes(Anno, Lac),color="red") + geom_line(color="red") + xlab("") + ylab("Lacunarity")+coord_cartesian(ylim=c(1,12))+ggtitle("Lacunarity  Year - TRENTO S12 r 20m w 100m")
ggsave("Trento_12_year_lacunarity.png",width = 8,height = 8,dpi = 600) 
ggplot(time_fdc, aes(Anno, Fd)) + geom_line(color="red") + xlab("") + ylab("Fractal Dimension")+coord_cartesian(ylim=c(1,3))+ggtitle("Fractal Dimension  Year - TRENTO S12 r 20m w 100m")
ggsave("Trento_12_year_fractal_dim.png",width = 8,height = 8,dpi = 600) 
ggplot(time_fea, aes(Anno, Feature)) + geom_line(color="red") + xlab("") + ylab("Number feature")+coord_cartesian(ylim=c(0,200000))+ggtitle("Volume features  Year - TRENTO S12 r 20m ")
ggsave("Trento_12_year_volume_feature.png",width = 8,height = 8,dpi = 600) 

# Saving results

saveRDS(res_raster_year,"res_raster_year_TN.rds")
saveRDS(res_raster_year_frac,"res_raster_year_frac_TN.rds")
saveRDS(res_raster_year_fd,"res_raster_year_fd_TN.rds")
saveRDS(res_value_year_lac,"res_value_year_lac_TN.rds")
saveRDS(res_value_year_fd_count,"res_value_year_fd_count_TN.rds")
saveRDS(res_value_feature_count,"res_value_feature_count_TN.rds")
write.csv(time_lac,"year_lac_TN.csv",row.names=F)
write.csv(time_fdc,"year_fd_count_TN.csv",row.names=F)
write.csv(time_fea,"feature_count_TN.csv",row.names=F)

q()



####################################################################################################################################################
####################################################################################################################################################à





