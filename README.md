State of Map  Europe 2014 
================

This repository have the data and R code for reproducible examples presented 
Required  OSM history file 

Firenze 
Matera
Trento

available also at https://mapzen.com/metro-extracts

https://s3.amazonaws.com/metro-extracts.mapzen.com/matera.osm.pbf

htps://s3.amazonaws.com/metro-extracts.mapzen.com/florence.osm.pbf

https://s3.amazonaws.com/metro-extracts.mapzen.com/trento.osm.pbf

or using Osmconvert:

osmconvert italy.osh.pbf -b=16.55763969,40.63519687,16.64553031,40.70184913 -o=Matera_12_osh.

osmconvert italy.osh.pbf -b=11.08181479,46.03591941,11.16970541,46.09692619 -o=Trento_12_osh.osm

osmconvert italy.osh.pbf -b=11.21163039,43.73812104,11.29952101,43.80162136 -o=Firenze_12_osh.osm



Osmconvert http://wiki.openstreetmap.org/wiki/Osmconvert

Osmfilter  http://wiki.openstreetmap.org/wiki/Osmfilter


## Data Consistency in OpenStreetMap
=============================================

### Title :Monitoring Consistency using Spatial Features and Tag Semantics



Monitoring consistency and reliability of maps is an important task when Volunteered Geographic Information (VGI) is involved and OpenStreetMap provides a good testing ground to develop operative methodologies. The aim of this work is to present some procedures to be instanced as a web application. Two questions about data consistency are considered: for any given region (I) Is the level of spatial features density at a given scale enough for a suitable geographical description? (II) Is the semantics of the features (described by keys and tags) consistent and comprehensive?

To answer the first question we are developing a method based on the spatial scales derived from the existing OSM zoom levels; having selected a bounding box and a given zoom level a given area can be sampled for its geographical features (vector-point and polygons) on which it is possible to assess spatial indexes. Scale-dependent indices such as Lacunarity and K-ripley Index are applied on the sampled features in order to measure the spatial homogeneity of the map. This analysis can be repeated on historical data at different points in time to monitor its development.

For the second question, considering OSM tags as a dynamic text corpus we have applied text mining methodologies: after selecting a well-mapped reference region we can then apply Zipf�s Law to assess his semantic profile; thus for any other given region the statistical deviations from Zipf's theoretical fits obtained from the reference can be measured. Zipf's deviations can then be used as a tool to measure the degree of geographical description of an area.

This talk presents preliminary results from work done by Alfonso Crisci (IBIMET-CNR), Maurizio Napolitano (FBK-Trento), Francesca De Chiara (FBK-Trento), Valentina Grasso (IBIMET-CNR, LaMMA Consortium), and Cristian Consonni (FBK-Trento). 
