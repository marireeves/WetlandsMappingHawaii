#Mari K Reeves
#November 17, 2017
#Wetlands Project
#Working with Land Cover Data 

#This code will import shpfiles for the Hawaii National Wetlands Inventory and NOAA Coastal Change 
#The NOAA raster based file was processed in ArcGIS to convert it to polygons and merge the 
#data for all Hawaiian Islands
#Also in ArcGis, both files were assigned a field, "Island" that says what Island they are in
#The layers are the NOAA Coastal Change Program Raster Land Cover 1 m resolution and the 
#USFWS NWI layers

#The code will check file projections and standardize them to a single projection if needed

#It will examine percent overlap for the NWI and NOAA open water categories of interest (TBD)

#It will generate a training dataset for machine learning algorithms by randomly sampling
#a combined dataset of the NWI and the NOAA land cover polygons internally consistent categories
#e.g., open water, versus all other categories (impervious, forested, etc) according to proportion 
#of the landscape they cover on each island https://coast.noaa.gov/digitalcoast/training/ccap-land-cover-classifications.html
#so polygons will be stratefied by Island and Land cover category in proportion to area of each


#number of polygons to be determined by power analysis or preliminary data analysis.
set.seed(333)

rm(list = ls()) #remove all past worksheet variables

# Read in Base Packages ---------------------------------------------------

pckg <- c("dplyr","tidyverse", "tidyr","RColorBrewer", "ggplot2","curl", 
          "RCurl","parallel","gdalUtils","rgdal","rgeos", "spsurvey", 
          "spatial","sp", "raster", "maptools", "spatialEco", "RStoolbox", 
          "SpatialPack", "spatstat", "microbenchmark","fifer", 'stringr', 
          "snowfall", "tictoc") 


# READING IN PACKAGES
for(i in 1:length(pckg)){
  if ((!pckg[i] %in% installed.packages())==T) {
    install.packages(pckg[i], repos="http://cran.us.r-project.org", 
                     dependencies = T)
    print(pckg[i])
    do.call("library", list(pckg[i]))
  }else{
    print(pckg[i])
    do.call("library", list(pckg[i]))
  }
}

.libPaths()

#Define Directories
BaseDir <- "F:/1New/wetlands/"
WorkingDir <- paste0(BaseDir, "WetlandsAnalysis")
setwd(WorkingDir)
list.files(WorkingDir)
ImageDir<- "G:/Imagery/"
CIRDir<-paste0(ImageDir, "ImageTunedCIR/")
RadarDir<- paste0(BaseDir, "ifSAR")
TrainingDir<-paste0(WorkingDir, "/TrainingData")
ShapeDir<-paste0(WorkingDir, "/ShapeDir")
ValidationDir<-paste0(WorkingDir, "/TrainingValidated")
SoilsDir<-paste0(BaseDir, "/Soils/SoilShpFiles/SoilPolys")


sysCRS<-"+proj=utm +zone=4 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"


#Download and extract the Hawaii NWI zipped shapefile
library(RCurl)

#NWI_URL<-"http://128.104.224.198/State-Downloads/HI_shapefile_wetlands.zip"
#nwi<- getURLContent(NWI_URL, ssl.verifypeer = FALSE)

#so this works, because it has to be the file name, but it returns a whole folder with shpfile
#and metadata into the Working Dir. So I'm having trouble linking the download and unzip commands 
#unzip(paste0(WorkingDir,"HI_shapefile_wetlands.zip"))



#Download and extract the NOAA land cover layers:https://coast.noaa.gov/ccapftp/#/


#The NOAA files are by Island, so I found this cool function here and am using that:

#https://thebiobucket.blogspot.com/2013/09/batch-downloading-zipped-shapefiles.html

#####Doesn't quite work yet...returned an empty list. 
library(raster)
#example URL list:
#URLs <- c("myurl1.zip",
#         "myurl2.zip")

#NOAA_URLs<- c("https://coast.noaa.gov/htdata/CCAP/ccap_high_res/HI/hi_lanai_2011_ccap_hr_land_cover20141204.zip", 
#              "https://coast.noaa.gov/htdata/CCAP/ccap_high_res/HI/hi_oahu_2011_ccap_hr_land_cover20140619.zip",
#              "https://coast.noaa.gov/htdata/CCAP/ccap_high_res/HI/hi_hawaii_2010_ccap_hr_land_cover20150120.zip",
#              "https://coast.noaa.gov/htdata/CCAP/ccap_high_res/HI/hi_kauai_2010_ccap_hr_land_cover20140929.zip",
#              "https://coast.noaa.gov/htdata/CCAP/ccap_high_res/HI/hi_maui_2010_ccap_hr_land_cover_20150213.zip",
#              "https://coast.noaa.gov/htdata/CCAP/ccap_high_res/HI/hi_molokai_2010_ccap_hr_land_cover20150102.zip",
#              "https://coast.noaa.gov/htdata/CCAP/ccap_high_res/HI/hi_niihau_2010_ccap_hr_land_cover20140930.zip")

#~~~~~~~~~~~~~~~~~~~Downloaded and unzipped already so importing from disk to save data

#bring shapefiles into R 
#check the spatial projections and transform to WGS84 UTM zone 4 if needed

#~~~~~~~~~~~~~~~~~~~~~~Download and unzip NRCS Soils data from here: https://websoilsurvey.nrcs.usda.gov/app/WebSoilSurvey.aspx
soilfiles<-list.files(SoilsDir, pattern='\\.shp$')
#for (soil in soilfiles){
soilimport<-function(soil){
  lyr <- sub(".shp$", "", soil)#This gets rid of the .shp extension in the list of files
  g<-readOGR(dsn = SoilsDir, layer = lyr )#This reads in the point file for each island
  print(proj4string(g))#check the spatial projections
  g<-spTransform(g, CRS(sysCRS))#transform to WGS84 UTM zone 4 if needed
  print(proj4string(g))#recheck the spatial projections
  plot(g)
  assign(soil, g, envir = globalenv())
}
lapply(soilfiles, soilimport)

#merge the soil polygons to a statewide layer
soils<-list(soilmu_a_hi701.shp, soilmu_a_hi801.shp, soilmu_a_hi950.shp, 
            soilmu_a_hi960.shp, soilmu_a_hi970.shp, soilmu_a_hi980.shp, 
            soilmu_a_hi990.shp, soilmu_a_hi995.shp)
statesoils<-do.call("rbind", soils) 
plot(statesoils)


shapefiles<-list.files(ShapeDir, pattern='\\.shp$')#make a list of shp files

for (shape in shapefiles){
  lyr <- sub(".shp$", "", shape)#This gets rid of the .shp extension in the list of files
  g<-readOGR(dsn = ShapeDir, layer = lyr )#This reads in the point file for each island
  print(proj4string(g))#check the spatial projections
  g<-spTransform(g, CRS(sysCRS))#transform to WGS84 UTM zone 4 if needed
  print(proj4string(g))#recheck the spatial projections
  plot(g)
    assign(shape, g)
}

#read in the NWI layers of interest to subset polygons by
list.files(WorkingDir)
includenwi<-read.csv(paste0(WorkingDir,"/NWI2Include.csv"))
noaaDesc<-read.csv(paste0(WorkingDir, "/NOAA2KEEPDROP.csv"))
noaaDrop<-noaaDesc[noaaDesc$KEEP_DROP == "DROP",]
noaaKeep<-noaaDesc[noaaDesc$KEEP_DROP == "KEEP",]
mega<-read.csv(paste0(WorkingDir, "/MegalagrionRecords.csv"))
hydricsoils<-read.csv(paste0(WorkingDir, "/HydricSoils.csv"))

#give the Agriculture file a better name
ag2015<-`2015AgBaseline.shp`
rm(`2015AgBaseline.shp`)

#give the land use file a better name
landuse<-slud.shp
rm(slud.shp)

#give the coastline file a better name
coast<-state_coast.shp
rm(state_coast.shp)

#give waterbird file a better name
waterbirds<-`waterbirdwetlandsitesUpdated2017 FINAL.shp`
rm(`waterbirdwetlandsitesUpdated2017 FINAL.shp`)

#give geology layer a better name
geology<-Haw_St_geo_20070426_region.shp
rm(Haw_St_geo_20070426_region.shp)

#give National Wetlands Inventory a better name
nwi<-HI_Wetlands.shp
rm(HI_Wetlands.shp)

NOAADir<-paste0(BaseDir, "LandCoverNOAA/noaa_ccap_rasters/")

ccaprasters<-list.files(NOAADir, pattern='\\.img$')

#Example Code for stratified sample in raster
#sampleStratified(x, size, exp=10, na.rm=TRUE, xy=FALSE, ext=NULL, sp=FALSE, ...)
library(raster)

#generate a stratified random sample from NOAA CCAP raster layers
#~~~~~~~~~~~~~~`WARNING THIS TAKES ABOUT 20 HOURS TO RUN ON COMPUTER WITH 16 GB RAM~~~~~~~~~~~~~~~~~~~~~~~~```
for (imagename in ccaprasters){
  y<-raster(paste0(NOAADir,imagename))
  assign(paste0(imagename), y) 
  print(ncell(y))
  #size below is the number of samples per strata
  print(system.time(z<-sampleStratified(y, size = (ncell(y)/10000000), xy = T, sp = T)))
 assign(paste0(imagename, "_sample"), z)
  system.time(writeOGR(z, dsn = TrainingDir, layer = paste0(imagename, "_noaa_samples_fromR"), driver="ESRI Shapefile", overwrite_layer = T))
}   



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Process NWI~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Pull wetlands of interest out of NWI dataset 
nwiinclude<-semi_join(nwi@data, includenwi, by = "ATTRIBUTE")

#this works as a filtering join, but I lost the spatial aspects with the dplyr join step
#but could it be as simple as using this subset argument?..not that I can figure.
#convert factors to characters
#helpful resource: http://www.nickeubank.com/wp-content/uploads/2015/10/RGIS2_MergingSpatialData_part1_Joins.html
#make a copy of nwi
nwi2<-nwi

#Going to have to do it with a spatial merge, then delete the NAs
#example merge:
#worldCountries <- merge(worldCountries, countryData, by.x = "id-number", by.y = "countryID")
str(nwi@data)
str(includenwi)
nwiinclude<-merge(nwi, includenwi, by = "ATTRIBUTE")
#Check if this worked..it did, but added NAs
joinednwidata<-nwiinclude@data
#That worked, now we need to remove NAs from the nwiinclude layer
#helpful function from Jeff Evans: https://gis.stackexchange.com/questions/89512/r-dealing-with-missing-data-in-spatialpolygondataframes-for-moran-test
# DISPLAY NA ROWS IN nwi  
nwiinclude@data[!complete.cases(nwiinclude@data),] 

# FUNCTION TO REMOVE NA's IN sp DataFrame OBJECT
#   x           sp spatial DataFrame object
#   margin      Remove rows (1) or columns (2) 
sp.na.omit <- function(x, margin=1) {
  if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
    stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT") 
  na.index <- unique(as.data.frame(which(is.na(x@data),arr.ind=TRUE))[,margin])
  if(margin == 1) {  
    cat("DELETING ROWS: ", na.index, "\n") 
    return( x[-na.index,]  ) 
  }
  if(margin == 2) {  
    cat("DELETING COLUMNS: ", na.index, "\n") 
    return( x[,-na.index]  ) 
  }
}

# DELETE NA's AND SHOW CHANGE IN dim
nwiincludefinal <- sp.na.omit(nwiinclude)     
dim(nwiincludefinal)
dim(nwiincludefinal) 

#spit this to GIS to check it
system.time(writeOGR(nwiincludefinal, dsn = WorkingDir, layer = "nwiwetlandsfromr", driver="ESRI Shapefile", overwrite_layer = F))
#looks good. Pick training data from this layer
head(nwiincludefinal@data)
#This was too many polygons
#system.time(nwirandom<-stratified.random(x= nwiincludefinal, strata = 'Category', n=300, reps=2, replace = F))
#pick half
#system.time(nwirandom<-stratified.random(x= nwiincludefinal, strata = 'Category', n=150, reps=2, replace = F))
system.time(nwirandom<-stratified.random(x= nwiincludefinal, strata = 'Category', n=50, reps=2, replace = F))



#and export with a different name

#system.time(writeOGR(nwirandom, dsn = WorkingDir, layer = "nwirandomtest600poly", driver="ESRI Shapefile", overwrite_layer = T))
#

#system.time(writeOGR(nwirandom, dsn = WorkingDir, layer = "nwirandomtest300poly", driver="ESRI Shapefile", overwrite_layer = T))
system.time(writeOGR(nwirandom, dsn = WorkingDir, layer = "nwirandomtest100poly", driver="ESRI Shapefile", overwrite_layer = F))


#terrible function that uses too much memory
#system.time(nwipoints<-spsample(nwirandom, n = 2000, reps = 2, type = "stratified"))

#nice function that's fast and uses less memory
system.time(nwipoints<-sample.poly(nwirandom, n = 2, type = "random"))
plot(nwipoints)
# make a data frame with the polygon information for each point, using over 
proj4string(nwipoints)<-proj4string(nwirandom)
nwipointinfo <- over(nwipoints, nwirandom) 
#get the point coordinates
sp2 = SpatialPoints(nwipoints)

#combine this with the point coordinate data using SpatialPointsDataFrame
nwi_points_spdf <- SpatialPointsDataFrame(sp2,nwipointinfo)

proj4string(nwirandom)

proj4string(nwi_points_spdf)<-proj4string(nwirandom)


#now we can export it as a shpfile
#commenting this out just allows me to keep the same files but give shpfiles new names on output
#when I changed the sample size
#system.time(writeOGR(nwi_points_spdf, dsn = WorkingDir, layer = "nwi2000points", driver="ESRI Shapefile", overwrite_layer = T))
#system.time(writeOGR(nwi_points_spdf, dsn = WorkingDir, layer = "nwi1000points", driver="ESRI Shapefile", overwrite_layer = T))
system.time(writeOGR(nwi_points_spdf, dsn = WorkingDir, layer = "nwi250points", driver="ESRI Shapefile", overwrite_layer = F))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Now we have lots of stratefied, random samples. Too many.

#bring them into r, since the loop above just writes them to the WorkingDir

samplefiles<-list.files(TrainingDir, pattern='\\.shp$')#make a list of shp files

for (sample in samplefiles){
  lyr <- sub(".shp$", "", sample)#This gets rid of the .shp extension in the list of files
  g<-readOGR(dsn = TrainingDir, layer = lyr )#This reads in the point file for each island
  print(proj4string(g))#check the spatial projections
  g<-spTransform(g, CRS(sysCRS))#transform to WGS84 UTM zone 4 if needed
  print(proj4string(g))#recheck the spatial projections
  plot(g)
  names(g)<-c("cell","x","y","gridcode")
    assign(paste0(sample, "points"), g)
  }

#Let's do some cleaning, merging, and filtering of the sample datasets to keep only the NOAA land cover
#categories I plan to include in the training dataset = "noaaKeep" 
noaakeepers<-noaaKeep$NOAA_CAT
#~~~~~~~~~~~~~~~~make into a loop or function later
noaasub<- function(i) i[i$gridcode %in% noaakeepers,]#doesn't work yet i don't know how to assign
#names to the new files?
  

hawaii<-hi_hawaii_2010_ccap_hr_land_cover20150120.img_noaa_samples_fromR.shppoints[hi_hawaii_2010_ccap_hr_land_cover20150120.img_noaa_samples_fromR.shppoints$gridcode %in% noaakeepers,]
kahoolawe<-hi_kahoolawe_2005_ccap_hr_land_cover.img_noaa_samples_fromR.shppoints[hi_kahoolawe_2005_ccap_hr_land_cover.img_noaa_samples_fromR.shppoints$gridcode %in% noaakeepers,]
kauai<-hi_kauai_2010_ccap_hr_land_cover20140929.img_noaa_samples_fromR.shppoints[hi_kauai_2010_ccap_hr_land_cover20140929.img_noaa_samples_fromR.shppoints$gridcode %in% noaakeepers,]
lanai<-hi_lanai_2011_ccap_hr_land_cover_20141204.img_noaa_samples_fromR.shppoints[hi_lanai_2011_ccap_hr_land_cover_20141204.img_noaa_samples_fromR.shppoints$gridcode %in% noaakeepers,]
maui<-hi_maui_2010_ccap_hr_land_cover_20150213.img_noaa_samples_fromR.shppoints[hi_maui_2010_ccap_hr_land_cover_20150213.img_noaa_samples_fromR.shppoints$gridcode %in% noaakeepers,]
molokai<-hi_molokai_2010_ccap_hr_land_cover20150102.img_noaa_samples_fromR.shppoints[hi_molokai_2010_ccap_hr_land_cover20150102.img_noaa_samples_fromR.shppoints$gridcode %in% noaakeepers,]
niihau<-hi_niihau_2010_ccap_hr_land_cover20140930.img_noaa_samples_fromR.shppoints[hi_niihau_2010_ccap_hr_land_cover20140930.img_noaa_samples_fromR.shppoints$gridcode %in% noaakeepers,]
oahu<-hi_oahu_2011_ccap_hr_land_cover20140619.img_noaa_samples_fromR.shppoints[hi_oahu_2011_ccap_hr_land_cover20140619.img_noaa_samples_fromR.shppoints$gridcode %in% noaakeepers,]
plot(oahu)
plot(hi_kahoolawe_2005_ccap_hr_land_cover.img_noaa_samples_fromR.shppoints)
#make a list of the island point files
islands<-list(hawaii, kahoolawe, kauai, lanai, maui, molokai, oahu, niihau)

#Merge the Island Specific Point Files
statewide<-do.call("rbind", islands) 
plot(statewide)


#We can then use "over" with the coast layer to assign points to each polygon or island names to each point
island.info<-over(statewide, coast)
statewidenoaapts <- spCbind(statewide, island.info)

system.time(writeOGR(statewidenoaapts, dsn = WorkingDir, layer = "noaatrainingdata5000", driver="ESRI Shapefile", overwrite_layer = F))

#that was pretty big, 5K points where I wanted 500, so going to randomly subsample that. 
hawaiifiveooo<-statewidenoaapts@data
library(fifer)
hawaiifiveoo<-stratified(hawaiifiveooo, group = c("Island","gridcode"), size = 0.2)

summary889<-hawaiifiveoo %>% group_by(gridcode) %>% count(Island)

summary4949<-hawaiifiveooo %>% group_by(gridcode) %>% count(Island)

#this merges the subset data with the subset of randomly selected. Now they are all in the same shpfile
statewidenoaasubset<-merge(statewide, hawaiifiveoo)
stateside<- statewidenoaasubset@data 
system.time(writeOGR(statewidenoaasubset, dsn = WorkingDir, layer = "noaatrainingdata889", driver="ESRI Shapefile", overwrite_layer = F))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~Bring in validated NWI and NOAA datasets and merge

validationfiles<-list.files(ValidationDir, pattern='\\.shp$')#make a list of shp files

for (validata in validationfiles){
  lyr <- sub(".shp$", "", validata)#This gets rid of the .shp extension in the list of files
  g<-readOGR(dsn = ValidationDir, layer = lyr )#This reads in the point file for each island
  print(proj4string(g))#check the spatial projections
  g<-spTransform(g, CRS(sysCRS))#transform to WGS84 UTM zone 4 if needed
  print(proj4string(g))#recheck the spatial projections
  plot(g)
  assign(paste0(validata, "points"), g)
}

#make copies..originals in subdir below
noaafinal<-NOAA889TRAININGFINALEDITED.shppoints
 # F:\1New\wetlands\WetlandsAnalysis\TrainingValidated\NOAA889TRAININGFINALEDITED.shp 
nwifinal<-nwi522pointsFINISHED.shppoints
 # F:\1New\wetlands\WetlandsAnalysis\TrainingValidated\nwi522pointsFINISHED.shp

plot(nwifinal)
plot(noaafinal, add = T)

#assign x, y coordinates to these points
nwicoord<-coordinates(nwifinal)
noaacoord<-coordinates(noaafinal)
nwidata<-nwifinal@data
names(nwifinal@data)
noaadata<-noaafinal@data
names(noaafinal@data)
#I want to create a new column in nwi spatial points data frame called: state.


nwi_names<-over(nwifinal, coast[,7])
#add island labels to the nwi file
nwifinal@data<-cbind(nwifinal@data, nwi_names)

#add coordinates to nwi datafile
nwifinal@data<-cbind(nwifinal@data, nwicoord)
names(nwifinal@data)

#add coordinates to noaa datafile
noaafinal@data<-cbind(noaafinal@data, noaacoord)
names(noaafinal@data)

#drop fields not needed in merged analysis file and
#rename keeper fields so they match

nwifinal@data<-nwifinal@data[,c("Ctgry","WETLAND__1","Dan","Wet","Label","coords.x1","coords.x2")]
names(nwifinal@data)<-c("gridcode","KeepDropMove","DoubleChecked","Wet","IslandName","UTM_X","UTM_Y")
str(nwifinal)

noaafinal@data<-noaafinal@data[,c("gridcode","KeepDropMo","DAN","WET","Label","coords.x1","coords.x2")]
names(noaafinal@data)<-c("gridcode","KeepDropMove","DoubleChecked","Wet","IslandName","UTM_X","UTM_Y")
str(noaafinal)
noaafinal@data$gridcode<-as.factor(noaafinal@data$gridcode)

#merge the files with maptools package
trainingdata<-spRbind(nwifinal, noaafinal)

#Drop the points coded as "drop" from validation (done in ArcGIS)
trainingdatakeepers<-trainingdata[trainingdata@data$KeepDropMove != "D",]
#make a copy of gridcode to make sure next step works
trainingdatakeepers@data$gridcopy<-trainingdatakeepers@data$gridcode

plot(trainingdatakeepers)
#Reclassify the NWI wetland categories as gridcode 21 (open water)
levels(trainingdatakeepers@data$gridcode)
#BenBolker Example:https://stackoverflow.com/questions/11810605/replace-contents-of-factor-column-in-r-dataframe
#levels(iris$Species)[match("oldspecies",levels(iris$Species))] <- "newspecies"
levels(trainingdatakeepers@data$gridcode)[match(c("E","L","P"),levels(trainingdatakeepers@data$gridcode))] <-"21"

#assign a binomial response variable for wetland
trainingdatakeepers@data$response<-ifelse(trainingdatakeepers@data$gridcode == 21, 1,0 )

#export as a shp file
system.time(writeOGR(trainingdatakeepers, dsn = WorkingDir, layer = "FinalMergedTrainingData", driver="ESRI Shapefile", overwrite_layer = F))

traindatadata<-trainingdatakeepers@data
traindatasummary<-traindatadata %>% group_by(gridcode) %>% count(IslandName)

#export summary file. 
write.csv(traindatasummary, file = "traindatasummary.csv")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#extract info from these training data points for the geology, waterbirds, and other state layers

##for the random points
head(trainingdatakeepers)
names(trainingdatakeepers)

#the 'over' function is generating dataframes here. Need to fix this
#https://stackoverflow.com/questions/23665328/error-saving-new-shape-file-after-using-over-function-r 
#useful description:https://stackoverflow.com/questions/7070173/r-friendly-way-to-convert-r-data-frame-column-to-a-vector

#Pull in whether the point is in a waterbird survey location and reclassify
head(waterbirds)
names(waterbirds@data)
##Extract shapefile information at unbuffered training data point locations 

#trainingdatakeepers@data$waterbirds <- over(trainingdatakeepers, waterbirds[,"SurveyFreq"], returnList = FALSE) 


waterbirdsurveys<-over(trainingdatakeepers, waterbirds[,"SurveyFreq"], returnList = FALSE) 

#the double brackets convert this variable from a dataframe to a factor
waterbirdsurveys <- waterbirdsurveys[['SurveyFreq']]
class(waterbirdsurveys)
trainingdatakeepers@data$waterbirds<-waterbirdsurveys
trainingdatakeepers@data$waterbirds<-as.character(trainingdatakeepers@data$waterbirds)
trainingdatakeepers@data$waterbirds[is.na(trainingdatakeepers@data$waterbirds)]<-"No Waterbird Survey"
dim(trainingdatakeepers@data)


#Pull Geology Data
head(geology)
levels(geology$ROCK_TYPE)
#commented out code below attaches a dataframe within the data frame, so more convoluted code works better
#trainingdatakeepers@data$geology_rocktype<-over(trainingdatakeepers, geology[,"ROCK_TYPE"])
RockType<-over(trainingdatakeepers, geology[,"ROCK_TYPE"])
RockType<-RockType[["ROCK_TYPE"]]
class(RockType)
trainingdatakeepers@data$RockType<-RockType
trainingdatakeepers@data$RockType<-as.character(trainingdatakeepers@data$RockType)
trainingdatakeepers@data$RockType[is.na(trainingdatakeepers@data$RockType)]<-"No Rock Data"
dim(trainingdatakeepers@data)


levels(geology$LITHOLOGY)
#trainingdatakeepers@data$geology_lithology<-over(trainingdatakeepers, geology[,"LITHOLOGY"])
Lithology<-over(trainingdatakeepers, geology[,"LITHOLOGY"])
Lithology<-Lithology[["LITHOLOGY"]]
class(Lithology)
trainingdatakeepers@data$Lithology<-Lithology
trainingdatakeepers@data$Lithology<-as.character(trainingdatakeepers@data$Lithology)
trainingdatakeepers@data$Lithology[is.na(trainingdatakeepers@data$Lithology)]<-"No Lithology Data"
dim(trainingdatakeepers@data)

#Pull Agriculture Category
head(ag2015)
#trainingdatakeepers@data$ag2015<-over(trainingdatakeepers, ag2015[,"CropCatego"])
AgCategory<-over(trainingdatakeepers, ag2015[,"CropCatego"])
AgCategory<-AgCategory[["CropCatego"]]
class(AgCategory)
trainingdatakeepers@data$AgCat2015<-AgCategory
trainingdatakeepers@data$AgCat2015<-as.character(trainingdatakeepers@data$AgCat2015)
trainingdatakeepers@data$AgCat2015[is.na(trainingdatakeepers@data$AgCat2015)]<-"Not Cropland"
dim(trainingdatakeepers@data)

#Pull Land Use District Category
head(landuse)
LandUse<-over(trainingdatakeepers, landuse[,"LUDCODE"])
LandUse<-LandUse[["LUDCODE"]]
class(LandUse)
trainingdatakeepers@data$LandUse<-LandUse
trainingdatakeepers@data$LandUse<-as.character(trainingdatakeepers@data$LandUse)
trainingdatakeepers@data$LandUse[is.na(trainingdatakeepers@data$LandUse)]<-"No Land Use Data"
dim(trainingdatakeepers@data)

#Join Hydric Soils Map Unit info to Soils data and pull in Hydric Soil information
#This is basically a join field to the hydric soils table
names(statesoils@data)
statesoils@data$soil_id<-paste0(statesoils@data$AREASYMBOL, "_", statesoils@data$MUSYM)
head(statesoils@data)
head(hydricsoils)
hydricsoils$soil_id<-paste0(hydricsoils$Area_Symbol, "_", hydricsoils$Mapunit_SYM)
SoilMapUnit<-over(trainingdatakeepers, statesoils[,"soil_id"])
SoilMapUnit<-SoilMapUnit[["soil_id"]]
class(SoilMapUnit)
trainingdatakeepers@data$soil_id<-SoilMapUnit
trainingdatakeepers@data$soil_id<-as.character(trainingdatakeepers@data$soil_id)
class(trainingdatakeepers@data$soil_id)



trainingdataout<-as.data.frame(trainingdatakeepers@data)
str(trainingdataout)
head(trainingdatakeepers)
dim(trainingdataout)
write.csv(trainingdataout, file = "trainingdatawithpolygoninfo.csv")
#make a copy 
trainingset<-trainingdatakeepers
names(trainingset)
str(trainingset)
plot(trainingset)
dim(trainingdatakeepers)
dim(trainingset)

system.time(writeOGR(trainingset, dsn = WorkingDir, layer = "trainingdatawithpolygoninfo", driver="ESRI Shapefile", overwrite_layer = T))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
#EXTRACT THE POINT DATA FROM THE IMAGERY

#Define Local Image Dir - Image source Dir also defined above
ImageDir<- "G:/Imagery/"
BigCIRDir<-paste0(ImageDir, "ImageTunedCIR/BigIsland/")
OtherCIRDir<-paste0(ImageDir, "ImageTunedCIR/OtherIslands/")
BigIsland8BandDir<-paste0(ImageDir, "PanSharpened8Band/FoReal8Bands/Multispectral/BigIsland/")
OtherIsland8BandDir<-paste0(ImageDir, "PanSharpened8Band/FoReal8Bands/Multispectral/OtherIslands/")
BaseDir <- "F:/1New/wetlands/"
WorkingDir <- paste0(BaseDir, "WetlandsAnalysis")
ImageDataOut<-paste0(BaseDir,"Imagery/")
setwd(ImageDataOut)
getwd()

#assign the system crs
sysCRS<-"+proj=utm +zone=4 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"

#Assign the CRS of the incoming raster files - different for Big Island and Others

BigIslandCRS<- "+proj=utm +zone=5 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"

OtherIslandCRS<- "+proj=utm +zone=4 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"


#read in training dataaset shpfile (NOT NEEDED IF RUN STRAIGHT THROUGH)
trainingset<-readOGR(dsn = WorkingDir, layer = "trainingdatawithpolygoninfo")

#THIS JUST HELPS WITH MEMORY AND RASTER PROCESSING
tempDir <- paste0(getwd(), '/temp/')
if(file.exists(tempDir) == F){
  dir.create(tempDir)
}
rasterOptions(tmpdir=tempDir)


gc()#Run the garbage collector. this helps with memory

#Helpful Tutorial and function for bounding box...
#http://robinlovelace.net/r/2014/07/29/clipping-with-r.html

#FUNCTION TO CREATE A BOUNDING BOX AROUND THE IMAGE FILE
gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

#FUNCTION TO DESCRIBE THE RASTER STACKS
describestack<- function(imagefile)  {
  print(nlayers(imagefile))
  print(nrow(imagefile))
  print(ncol(imagefile))
  print(origin(imagefile))
  print(proj4string(imagefile))
  print(res(imagefile))
}

#CREATE LISTS OF INPUT IMAGERY FILES
OtherIslandEightBandFiles<-list.files(OtherIsland8BandDir, pattern='\\.img$', full.names=T)

BigIslandEightBandFiles<-list.files(BigIsland8BandDir, pattern='\\.img$', full.names=T)

OtherIslandCIRFiles<-list.files(OtherCIRDir, pattern='\\.img$', full.names=T)

BigIslandCIRFiles<-list.files(BigCIRDir, pattern='\\.img$', full.names=T)









#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CAUTION THESE ARE VERY LONG LOOPS TO RUN!
#ALSO, I AM NOT SURE THEY ARE RUNNING IN PARALLEL, SO RUN AS A FOR LOOP UNTIL FURTHER DIAGOSED. 
#
#detecting cores and esitimating CPU's
aC<- detectCores(all.tests = FALSE, logical = T)-1 # removing 1 so that I can view progress
sfInit(parallel=TRUE, cpus=aC)
try(for (i in 1:length(pckg)){
  eval(call("sfLibrary", (pckg[i]), character.only=TRUE))
}, F)


OtherIsland8BandRepro<-function(img){
    z<-stack(img)#This reads in the raster file as a stack
    c<-bbox(z)#create a bounding box based on extent of image file
    trainsetrepro<-spTransform(trainingset, CRSobj = OtherIslandCRS )
    trainsubset<-gClip(trainsetrepro, c) #subset the training data points by the bounding box (split step)
    trainreprobuffer<-gBuffer(trainsetrepro, byid=T, width=5) 
    trainmerge<-raster::intersect(trainsubset, trainreprobuffer)
    trainpts<-as.data.frame(raster::extract(z, trainmerge))#extract the point info from the image file
    names(trainpts)<-c("pt_band1", "pt_band2","pt_band3","pt_band4", "pt_band5", 
                       "pt_band6", "pt_band7", "pt_band8")
    #extract mean value from 5 m buffer circle around pts
    trainbuffermean<-as.data.frame(raster::extract(z, trainmerge, buffer = 5, fun = mean )) 
    names(trainbuffermean)<-c("mean5m_band1","mean5m_band2","mean5m_band3","mean5m_band4",
                              "mean5m_band5","mean5m_band6","mean5m_band7","mean5m_band8")
    trainptsout<-cbind(trainpts, trainbuffermean, trainmerge@data)
    pointfile<-as.character(paste0(img, ".csv"))
    #assign(pointfile, trainptsout, envir = globalenv())
    write.csv(trainptsout, file = pointfile)
}

sfExportAll()

tic("start OtherIslandEightBandFiles in parallel")
sfLapply(OtherIslandEightBandFiles, OtherIsland8BandRepro)
toc("stop OtherIslandEightBandFiles in parallel")

sfStop()


#start BigIslandEightBandFiles in parallel: 11896.4 sec elapsed - 3.5 hours

#detecting cores and esitimating CPU's
aC<- detectCores(all.tests = FALSE, logical = T)-1 # removing 1 so that I can view progress
sfInit(parallel=TRUE, cpus=aC)
try(for (i in 1:length(pckg)){
  eval(call("sfLibrary", (pckg[i]), character.only=TRUE))
}, F)


BigIsland8BandRepro<-function(img){
   z<-stack(img)#This reads in the raster file as a stack
  c<-bbox(z)#create a bounding box based on extent of image file
  trainsetrepro<-spTransform(trainingset, CRSobj = BigIslandCRS )
  trainsubset<-gClip(trainsetrepro, c) #subset the training data points by the bounding box (split step)
  trainreprobuffer<-gBuffer(trainsetrepro, byid=T, width=5) 
  trainmerge<-raster::intersect(trainsubset, trainreprobuffer)
  trainpts<-as.data.frame(raster::extract(z, trainmerge))#extract the point info from the image file
  names(trainpts)<-c("pt_band1", "pt_band2","pt_band3","pt_band4", "pt_band5", 
                     "pt_band6", "pt_band7", "pt_band8")
  #extract mean value from 5 m buffer circle around pts
  trainbuffermean<-as.data.frame(raster::extract(z, trainmerge, buffer = 5, fun = mean )) 
  names(trainbuffermean)<-c("mean5m_band1","mean5m_band2","mean5m_band3","mean5m_band4",
                            "mean5m_band5","mean5m_band6","mean5m_band7","mean5m_band8")
  trainptsout<-cbind(trainpts, trainbuffermean, trainmerge@data)
  pointfile<-as.character(paste0(img, ".csv"))
  #assign(pointfile, trainptsout, envir = globalenv())
  write.csv(trainptsout, file = pointfile)
}


sfExportAll()

tic("start BigIslandEightBandFiles in parallel")
sfLapply(BigIslandEightBandFiles, BigIsland8BandRepro)
toc("stop BigIslandEightBandFiles in parallel")

sfStop()

 aC<- detectCores(all.tests = FALSE, logical = T)-1 # removing 1 so that I can view progress
  sfInit(parallel=TRUE, cpus=aC)
  try(for (i in 1:length(pckg)){
    eval(call("sfLibrary", (pckg[i]), character.only=TRUE))
  }, F)
 
   #OtherIslandEightBandFiles in parallel: 11922.72 sec elapsed - 3.3 hrs
  
OtherIslandCIRRepro<-function(img){
    #for(img in OtherIslandEightBandFiles){
    z<-stack(img)#This reads in the raster file as a stack
    c<-bbox(z)#create a bounding box based on extent of image file
    trainsetrepro<-spTransform(trainingset, CRSobj = OtherIslandCRS )
    trainsubset<-gClip(trainsetrepro, c) #subset the training data points by the bounding box (split step)
    trainreprobuffer<-gBuffer(trainsetrepro, byid=T, width=5) 
    trainmerge<-raster::intersect(trainsubset, trainreprobuffer)
    trainpts<-as.data.frame(raster::extract(z, trainmerge))#extract the point info from the image file
    names(trainpts)<-c("pt_CIR_1", "pt_CIR_2","pt_CIR_3")
    #extract mean value from 5 m buffer circle around pts
    trainbuffermean<-as.data.frame(raster::extract(z, trainmerge, buffer = 5, fun = mean )) 
    names(trainbuffermean)<-c("mean5m_CIR_1","mean5m_CIR_2","mean5m_CIR_3")
    trainptsout<-cbind(trainpts, trainbuffermean, trainmerge@data)
    pointfile<-as.character(paste0(img, ".csv"))
    #assign(pointfile, trainptsout, envir = globalenv())
    write.csv(trainptsout, file = pointfile)
  }
 
  sfExportAll()
  tic("start OtherIslandCIRFiles in parallel")
  sfLapply(OtherIslandCIRFiles, OtherIslandCIRRepro)
  toc("stop OtherIslandCIRFiles in parallel")
  sfStop()
  #start OtherIslandCIRFiles in parallel: 4118.54 sec elapsed
  
  aC<- detectCores(all.tests = FALSE, logical = T)-1 # removing 1 so that I can view progress
  sfInit(parallel=TRUE, cpus=aC)
  try(for (i in 1:length(pckg)){
    eval(call("sfLibrary", (pckg[i]), character.only=TRUE))
  }, F)
  
  
  BigIslandCIRRepro<-function(img){
    z<-stack(img)#This reads in the raster file as a stack
    c<-bbox(z)#create a bounding box based on extent of image file
    trainsetrepro<-spTransform(trainingset, CRSobj = BigIslandCRS )
    trainsubset<-gClip(trainsetrepro, c) #subset the training data points by the bounding box (split step)
    trainreprobuffer<-gBuffer(trainsetrepro, byid=T, width=5) 
    trainmerge<-raster::intersect(trainsubset, trainreprobuffer)
    trainpts<-as.data.frame(raster::extract(z, trainmerge))#extract the point info from the image file
    names(trainpts)<-c("pt_CIR_1", "pt_CIR_2","pt_CIR_3")
    #extract mean value from 5 m buffer circle around pts
    trainbuffermean<-as.data.frame(raster::extract(z, trainmerge, buffer = 5, fun = mean )) 
    names(trainbuffermean)<-c("mean5m_CIR_1","mean5m_CIR_2","mean5m_CIR_3")
    trainptsout<-cbind(trainpts, trainbuffermean, trainmerge@data)
    pointfile<-as.character(paste0(img, ".csv"))
    #assign(pointfile, trainptsout, envir = globalenv())
    write.csv(trainptsout, file = pointfile)
  }

  
  sfExportAll()
  tic("start BigIslandCIRFiles in parallel")
  sfLapply(BigIslandCIRFiles, BigIslandCIRRepro)
  toc("stop BigIslandCIRFiles in parallel")
  sfStop()

  #start BigIslandCIRFiles in parallel: 7229.88 sec elapsed
 
#to my complete weekend consternation, this code DOES write the csv files, it just sends them 
#to the G: drive, (as they are labeled) not to the working directory on the F drive!!! ARGH
#because pointfile is a filename starting with G!
  
  
  #I moved them to a new directory and will import and bind them from there. 
  save.image("F:/1New/wetlands/WetlandsAnalysis/TrainingExtracted/18_02_14_PreBindEnvironment.RData") 
  #just saving to shut down/re-start really clear out memory
  load("F:/1New/wetlands/WetlandsAnalysis/TrainingExtracted/18_02_14_PreBindEnvironment.RData") 

#BIND THE IMAGE EXTRACTED DATA FILES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #example of using do.call to merge objectshttps://stackoverflow.com/questions/15876591/merging-multiple-rasters-in-r
  
  ValidatedExtractedDir<-paste0(WorkingDir, "/TrainingValidated/FinalTrainingExtracted")
  getwd()
  #https://stackoverflow.com/questions/23169645/r-3-0-3-rbind-multiple-csv-files
  
  extractedfiles<-list.files(ValidatedExtractedDir, pattern='\\.csv$', full.names=T)
  
  tables <- lapply(extractedfiles, read.csv, header = TRUE)
  
  TestSetNoCIR <- do.call(rbind , tables)
  
  #well that was easy. There are duplicates and NA values. Should probably look at those. IT went from 1292 - 1409 obs
  #but 100 misjoins is not too bad. 
  
  write.csv(TestSetNoCIR, file = "TestSetNoCIR.csv")
  
  #Make a directory for validated CIR files.
  
  
  extractedCIRDir<-paste0(WorkingDir, "/TrainingCIRExtracted")
  
  extractedCIRfiles<-list.files(extractedCIRDir, pattern='\\.csv$', full.names=T)
  
  tablesC <- lapply(extractedCIRfiles, read.csv, header = TRUE)
  
  TestSetCIR <- do.call(rbind , tablesC)
  
  TestDataNoRadar<-cbind(TestSetNoCIR, TestSetCIR)
  
  write.csv(TestSetCIR, file = "TestSetCIR.csv")
  
  
  #well that was easy. There are duplicates and NA values. Should probably look at those. IT went from 1292 - 1491 obs
  #but 200 misjoins is not too bad. 73 OF THESE ARE ZERO VALUES FOR THE 8 BAND DATA
  #The zero values are all on the big island or where island is NA, so assume overlapping photos
  
  write.csv(TestSetNoCIR, file = "TestSetNoCIR.csv")
  
  #TestSetCIR came in with 1303 observations and 22 variables. 
  #TestSetNoCIR came in with 1409 obs and 32 variables. 
  #training dataset was initially 1292, so this isn't bad.
  #Is it possible that the extractions pulled from more than one pixel value, 
  #so that's the source of the duplicates?
  #I think so, but this analysis should pull that out
  #I think X is the unique ID from the trainingset file, and then when a point covered 2 pixels, it replicated 
  #the sample (e.g., 1 and 1.1) became the nomenclature. 
  head(TestSetNoCIR)
  #Merge the files with a complete join, retaining all records
  lochness<-merge(TestSetCIR, TestSetNoCIR)
  #remove duplicates (there were none)
  nessy<-distinct(lochness)
  
  #Then work on the remaining 1411 messy values
  write.csv(lochness, file = "lochness.csv")
  
#Make a shpfile out of lochness so that I can look at problematic points in GIS
  names(lochness)
  lochcoords <- lochness[,c("UTM_X", "UTM_Y")]
  
  #overwriting nessy from above, there were no duplicates so file was redundant
  nessy <- SpatialPointsDataFrame(coords = lochcoords, data = lochness,
                                 proj4string = CRS(sysCRS))
  
  plot(nessy)
 
  #export to GIS to take a look and send to Adonia
  
  system.time(writeOGR(nessy, dsn = WorkingDir, layer = "messy_nessy", driver="ESRI Shapefile", overwrite_layer = F))
  
  save.image("F:/1New/wetlands/WetlandsAnalysis/18_02_16_MessyNessy.RData")
  
  load("F:/1New/wetlands/WetlandsAnalysis/18_02_16_MessyNessy.RData")

  names(nessy)
 dim(nessy)
  #drops the NA values
  nessy<-nessy[which(!is.na(nessy$pt_band1)),]
  #dim(nessy) 1468   38
  
  
  #drop the zero values
  nessy<-nessy[nessy$pt_band1 != 0,]
  #dim(nessy) 1396   38
  
  nessydata<-nessy@data
  #We still have 100 extra samples - these should all now be duplicates where the extract function drilled 
  #two different photos, so useful for examining intraphoto variance - these are one class of duplicates, where 
  # X does not have a decimal place after it, we'll get those next, by collapsing them into a mean, 
  #because I'm pretty sure that's where R sampled two pixels.
  
  
  
  #One more column to identify NRCS hydric soils versus not 
  #~~~~~~~~~~~~~~~~~~~~This doesn't run yet~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  names(hydricsoils)
  str(hydricshort)
  class(nessydata$mukey)
  hydricshort<-hydricsoils[,c("mukey", "Hydric_Rating")]
  hydricshort$Hydric_Rating<-as.character(hydricshort$Hydric_Rating)
 
  
  dim(nessydata)
 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~NRCS no unique id~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  nessydata<-left_join(nessydata, hydricshort)
  dim(nessydata)
  nessydata$mukey[is.na(nessydata$mukey)]<-"Not On Soils Map"
  nessydata$Hydric_Rating<-as.character(nessydata$Hydric_Rating)
  nessydata$Hydric_Rating[is.na(nessydata$Hydric_Rating)]<-"Not Hydric Soils"
  dim(nessydata)
  
  
  #https://stackoverflow.com/questions/6986657/find-duplicated-rows-based-on-2-columns-in-data-frame-in-r
  twoheadednessy<-nessy[which(duplicated(nessy@data[,"X"])),]
  dim(twoheadednessy) #239  38
  #This function only pulled the duplicates and left the original data in, so that's one way to pull the data from 
  #nessy, but I actually wanted a file with the duplicates in. For now, though I may just pull this out of 
  #nessy, see what it does to our sample size.
  
  table(twoheadednessy$X)#seems there were a few triplicates even
  nessytwohead<-twoheadednessy@data
  
  oneheadednessy<-nessy[which(!duplicated(nessy@data[,"X"])),]
  dim(oneheadednessy)# hmm, now we lose all the dups, which we don't want either!# 1157   38

  #resorting to dplyr here, hope it works on spdfs---nope
  nessyonehead<-
    nessydata %>%
    distinct(X, .keep_all = TRUE)
#this does the same as not duplicated. There has got to be a middle ground here. Both these remove the duplicates
  #completely. I guess I could do that, collapse nessytwohead by means, then rowbind it back together?
  #worth a try
  
#Alternatively...what if I just kept all these duplicate values IN? They are representing sampling variation for
  #the photos. The models should be able to handle this, and it might be kind of interesting. 
  #I can code X as a repeated measure. hmmmm. And this lets me run models NOW!! Bwahahaha!@
  
  
  #calculate NDVI: https://en.wikipedia.org/wiki/Normalized_difference_vegetation_index
  #This equals NIR-Red/NIR+Red, which should be Band 7-Band 5/Band 7 +Band 5
  #According to this https://blogs.esri.com/esri/arcgis/2011/08/23/what-is-the-band-ordering-for-worldview-2-imagery/
  
  nessy$NDVIpt<-(nessy$pt_band7-nessy$pt_band5)/(nessy$pt_band7+nessy$pt_band5)
  nessy$NDVImean<-(nessy$mean5m_band7-nessy$mean5m_band5)/(nessy$mean5m_band7+nessy$mean5m_band5)
  
  #calculate NDWI: https://en.wikipedia.org/wiki/Normalized_difference_water_index 
  #NDWI 1 is One is used to monitor changes in water content of leaves, 
  #using near-infrared (NIR) and short-wave infrared (SWIR) wavelengths, proposed by Gao in 1996
  nessy$NDWILeavespt<-(nessy$pt_band7-nessy$pt_band8)/(nessy$pt_band7+nessy$pt_band8)
  nessy$NDWILeavesmean<-(nessy$mean5m_band7 -nessy$mean5m_band8 )/(nessy$mean5m_band7+nessy$mean5m_band8)
 
  #Another is used to monitor changes related to water content in water bodies, using green and NIR wavelengths, 
  #defined by McFeeters (1996):
  nessy$NDWIWaterpt<-(nessy$pt_band3-nessy$pt_band7)/(nessy$pt_band3+nessy$pt_band7)
  nessy$NDWIWatermean<-(nessy$mean5m_band3 -nessy$mean5m_band7 )/(nessy$mean5m_band3+nessy$mean5m_band7)
  
 
  nessydata<-write.csv(nessydata, file = "nessydataformodels.csv")
  
  system.time(writeOGR(nessy, dsn = WorkingDir, layer = "clean_nessy_nozero_nona", driver="ESRI Shapefile", overwrite_layer = F))
  
 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~UNUSED CODE CHUNK - MAKE LIST OUT OF Items in Workspace
#make a list of all these dataframes..this is useful when they are assigned to the workspace 
  #instead of written to .csv, so I'm keeping it around, but not using it.

#dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

#names(dfs)
#subset dfs by the type of imagery (CIR or 8 Band)
#cirfiles<-grepl("CIR", names(dfs))
#cirlist<-dfs[cirfiles]

#or in one line of code, not 2..because grep calls it directly whereas grepl gives position index
#cirlist<-dfs[grep("CIR", names(dfs))]

#subset by 8band

#eightbandlist<-dfs[grep("8band", names(dfs))]


#column bind the CIR to the 8 band outputs


#then calculate NDVI and NDWI for both point value and average. 

#is it worth pulling a min/max or quartiles to get a range in variation or a CV?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Bring in the NOAA Radar (ifSAR) data. These were shared under liscence, so need to request 
#from NOAA to repeat the analysis...sorry!

#Code to Reproject ifSAR Rasters 
##Warning, even though this runs in parallel, it's SLOW!!!!!!!!!!!!!!!!

pckg2 <- c("dplyr","tidyverse", "tidyr","RColorBrewer", "ggplot2","curl", "RCurl",
           "parallel","rgdal","rgeos", "clhs",  "spatial",
           "sp", "raster", "maptools", "spatialEco", "RStoolbox", 
           "SpatialPack", "spatstat", "microbenchmark","fifer",
           'snowfall','parallel') 

library.path <- cat(.libPaths())

# READING IN PACKAGES
for(i in 1:length(pckg2)){
  if ((!pckg2[i] %in% installed.packages())==T) {
    install.packages(pckg2[i], repos="http://cran.us.r-project.org",
                     dependencies = T, INSTALL_opts = c('--no-lock'))#, lib=r_lib_loc
  }else{
    
    print(pckg2[i])
    do.call("library", list(pckg2[i])) #, lib=r_lib_loc
  }
}

#detecting cores and esitimating CPU's
aC<- detectCores(all.tests = FALSE, logical = T)-1 # removing 1 so that I can view progress
sfInit(parallel=TRUE, cpus=aC)
try(for (i in 1:length(pckg2)){
  eval(call("sfLibrary", (pckg2[i]), character.only=TRUE))
}, F)



#Define Directories---These are for the beast
#JBase <- "J:/PIOGIS12/APPS/SH_CC/Mari/MariWetlands/wetlands/"
#JWorking <- paste0(JBase, "WetlandsAnalysis")
#RadarJ<- paste0(JBase, "ifSAR")
getwd()
#setwd(RadarJ)

#Define Local Image Dir
ImageDataOut<-paste0(WorkingDir,"/Imagery/")
setwd(RadarJ)
list.files(RadarJ)
#assign the system crs
sysCRS<-"+proj=utm +zone=4 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"

radarfiles<-list.files(RadarJ, pattern='\\.tif$', full.names=T)
radarnms<-list.files(RadarJ, pattern='\\.tif$', full.names=F)
tempDir <- paste0(getwd(), '/temp/')
if(file.exists(tempDir) == F){
  dir.create(tempDir)
}
rasterOptions(tmpdir=tempDir)
gc()
beastrepro<-function(rad){
  jnkStff <- radarnms[which(rad == radarfiles)]
  locStff <- str_replace(rad, jnkStff, "")
  j<-raster(rad)#This reads in the raster file 
  j<-projectRaster(j, crs = sysCRS, res = c(4.33,4.33))#transform to WGS84 UTM zone 4 if needed
  try(wn <- str_replace(jnkStff, "_UTM4", '_UTM4_WGS84'), T)
  try(wn <- str_replace(jnkStff, "_UTM5", '_UTM4_WGS84'), T)
  writeRaster(j, filename = paste0(JBase, wn),format = "GTiff", overwrite = T , progress = T)
}

sfExportAll()
sfLapply(radarfiles, beastrepro)
sfStop()

##I moved these files off the J drive onto my local hard drive manually so the code below works

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#import the reprojected ifSAR files:
DSMReproDir<-paste0(RadarDir, "/WGS84/DSM/")
DTMReproDir<-paste0(RadarDir, "/WGS84/DTM/")
reprojectedDSMfiles<-list.files(DSMReproDir, pattern='\\.tif$', full.names=T)#make a list of image files
reprojectedDTMfiles<-list.files(DTMReproDir, pattern='\\.tif$', full.names=T)#make a list of image files

describestack<- function(imagefile)  {
  print(nlayers(imagefile))
  print(nrow(imagefile))
  print(ncol(imagefile))
  print(origin(imagefile))
}

importrepro<-function(reprotif){
    y<-stack(reprotif)
    describestack(y)
      print(proj4string(y))#check the spatial projections
            assign(paste0(gsub("F:/1New/wetlands/ifSAR/WGS84/","", reprotif)), y, envir = globalenv())
  }

lapply(reprojectedDSMfiles, importrepro)
lapply(reprojectedDTMfiles, importrepro)

gc()

#example of using do.call to merge objectshttps://stackoverflow.com/questions/15876591/merging-multiple-rasters-in-r

dsm<-list(`DSM/BigIsland_DSM_UTM4_WGS84.tif`,`DSM/Kahoolawe_DSM_UTM4.tif`,`DSM/Kauai_DSM_UTM4.tif`,`DSM/Lanai_DSM_UTM4.tif`,`DSM/Maui_DSM_UTM4.tif`,
          `DSM/Molokai_DSM_UTM4.tif`, `DSM/Niihau_DSM_UTM4.tif`, `DSM/Oahu_DSM_UTM4.tif`)

dsm$tolerance<-1
dsm$filename <- 'test.tif'
dsm$overwrite <- TRUE
m <- do.call(merge, dsm)
plot(m)
str(m)

writeRaster(m, filename = "HawaiiStateDSM_NOAAifSAR",format = "GTiff", overwrite = T )
str(dsm)

dtm<-list(`DTM/BigIsland_DTM_UTM4_WGS84.tif`, `DTM/Kahoolawe_DTM_UTM4.tif`, `DTM/Kauai_DTM_UTM4.tif`,
          `DTM/Lanai_DTM_UTM4.tif`, `DTM/Maui_DTM_UTM4.tif`, `DTM/Molokai_DTM_UTM4.tif`,
       `DTM/Niihau_DTM_UTM4.tif`, `DTM/Oahu_DTM_UTM4.tif` )




dtm$tolerance<-1
dtm$filename <- 'test.tif'
dtm$overwrite <- TRUE
HIStateDTM<-do.call(merge, dtm)
plot(HIStateDTM)

#make a 5 m grid and overlay points on islands

#https://stackoverflow.com/questions/41787313/how-to-create-a-grid-of-spatial-points
# check the CRS to know which map units are used
#proj4string(colorado)
# "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Create a grid of points within the bbox of the SpatialPolygonsDataFrame 
# colorado with decimal degrees as map units
#grid <- makegrid(colorado, cellsize = 0.1)
plot(trainingdatakeepers)
gc()
system.time(writeOGR(SpatialPoints(grid <- makegrid(trainingdatakeepers, cellsize = 25)), dsn = WorkingDir, layer = "TestGrid5m", driver = "ESRI Shapefile"))

# grid is a data.frame. To change it to a spatial data set we have to
grid <- SpatialPoints(grid, proj4string = sysCRS)

#plot(colorado)
#plot(grid, pch = ".", add = T)
#To extract only the points within the polygon, use `[` to subset the points based on the location like this:

# grid <- grid[colorado, ]

start.time <- proc.time() # start stopwatch

#extract the data for the training points
trainingdatakeepers$dsm <- extract(m, trainingdatakeepers)

# we aren't going to count plotting time!
end.time <- proc.time() # end stopwatch
time.par <- (end.time - start.time)["elapsed"]


#This calcTPI Function is from Jeff Tracey at USGS
#It calculates a topographic position index
#unused as yet. Need to figure how to set spatial scale pending discussions.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(raster)

calcTPI <- function(dem, radius) {
  cell.sz <- mean(res(dem)) # meters; should be same in both dims; CONSIDER - WHAT IS THIS FOR?
  #cf <- makeCircularFilter(radius, cell.sz)
  cf <- focalWeight(dem, radius, type="circle") # CONSIDER - COULD WE PASS THIS AS AN ARG?
  dem.smooth <- focal(dem, w=cf, fun=sum) # see help for focalWeight to see why this produces a mean
  tpi <- dem - dem.smooth
  # border cells should be NA if the default padValue=NA is used in focal()
  return(tpi)
}


