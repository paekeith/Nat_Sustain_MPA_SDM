####============ LOAD PACKAGES AND DATA ==============####
rm(list=ls())
gc()

library(terra)
library(Rcpp)
library(sf)
library(exactextractr)
library(tidyverse)

SDM_files <- list.files(path = "Data/Inputs/SDMs/", pattern = "current_th", full.names = TRUE)
study_region <- vect("Data/Inputs/Study_Region.shp")
MPA_data_shp <- st_read("Data/Inputs/MPA_layers/pan_european_MPA_classifications.shp")

### CURRENT DAY ------------------------------------------------
## Developing a raster mask for the study region
rast_template <- rast(SDM_files[1]) #getting a template for the raster 
study_region_rast <- rasterize(study_region,rast_template) #rasterising

#Extracting a dataframe from the MPA file to get structure for saving results
MPA_data_df <- st_drop_geometry(MPA_data_shp)
names(MPA_data_df) <- c("MPA_class")

## Running overlap analysis
start <- Sys.time()
for(i in 1:length(SDM_files)){
  print(i)
  
  #loading in the relevant SDM layer 
  SDM_rast <- rast(SDM_files[i])
  
  #masking to the study region
  rast_masked <- terra::crop(SDM_rast,study_region_rast,mask=TRUE)

  #matching the SDM to the CRS of the MPA files (EPSG:3035)
  rast_masked <- project(rast_masked,crs(MPA_data_shp)) 
  
  # Setting cells of 0 probability to NA, to ensure they are not counted in overlap
  rast_masked[rast_masked$current==0] <- NA
  
  #calculate % overlap of each raster cell with the MPA vectors
  overlap <- exact_extract(rast_masked, MPA_data_shp,include_cell=TRUE,include_xy=TRUE,coverage_area=TRUE)
  
  #convert resulting list to dataframe
  names(overlap) <- MPA_data_df$MPA_class
  overlap_frame <- bind_rows(overlap, .id = "MPA_class")
  
  #summarise the overall coverage of each MPA class, making sure to exclude cells with absences (NA values)
  overlap_MPA <- as.data.frame(overlap_frame%>%
                                 drop_na(value)%>%
                                 group_by(MPA_class)%>%
                                 summarise(area=sum(coverage_area))%>%
                                 ungroup())
  
  #setting up an if statement to catch situations where the species is predicted absent everywhere and therefore there is no data to use
  if(dim(overlap_MPA)[1]!=0){
    #if there are presences, identify the total area of presence cells for this species 
    species_area <- sum(as.data.frame(cellSize(rast_masked,mask=TRUE,transform=FALSE,unit="m"))) 
    
    overlap_MPA$sp_area <- species_area
    overlap_MPA$area_prop <- overlap_MPA$area/species_area #calculating proportion of species area covered by the MPA classes
    overlap_MPA$species_ID <- gsub(pattern = ".*taxonid=([0-9]+).*", replacement = "\\1", x = SDM_files[i]) #extracting the species ID from the SDM file to keep trackof results
    
    #setting up the data frame for saving
    if(i == 1){ #
      main_frame  <- overlap_MPA
    } else {
      main_frame <- rbind(main_frame,overlap_MPA)
    }
    
  } else if(dim(overlap_MPA)[1]==0){ #skips to next iteration if no presences in this SDM
    next
  }
  
}
# }
Sys.time()-start

overlap_data <- main_frame

# setting up a dataframe to save the species ID and number of cells to
cell_numbers_df <- as.data.frame(matrix(nrow=length(SDM_files),ncol=2))
names(cell_numbers_df) <- c("species_ID","cell_num")

for(i in 1:length(SDM_files)){
  cell_numbers_df$species_ID[i] <- gsub(pattern = ".*taxonid=([0-9]+).*", replacement = "\\1", x = SDM_files[i]) #extracting the species ID from the SDM file to keep trackof results
  
  SDM_rast <-   rast(SDM_files[i])
  
  SDM_rast <- terra::crop(SDM_rast,study_region_rast,mask=TRUE)
  
  # Setting areas of 0 probability to NA
  SDM_rast[SDM_rast$current==0] <- NA
  
  cell_numbers_df$cell_num[i] <- sum(!is.na(SDM_rast[]))
  print(i)
}

#identifying the species IDs which have fewer than 50 cells of predicted presences
to_exclude <- cell_numbers_df$species_ID[which(cell_numbers_df$cell_num<50)]
#cutting these out of the overlap results

overlap_data_new <- overlap_data[-c(which(overlap_data$species_ID%in%to_exclude)),]

table(overlap_data_new$MPA_class)

#filling in missing MPA categories 
overlap_data_new_complete <- overlap_data_new %>%
  tidyr::complete(species_ID, MPA_class, fill = list(value = 0))

table(overlap_data_new_complete$MPA_class)

overlap_data_new_complete$area_prop[which(is.na(overlap_data_new_complete$area_prop)==TRUE)] <- 0
overlap_data_new_complete$area[which(is.na(overlap_data_new_complete$area)==TRUE)] <- 0

overlap_data_new_complete <- overlap_data_new_complete %>%
  dplyr::group_by(species_ID) %>%
  tidyr::fill(sp_area, .direction = "downup") %>%
  dplyr::ungroup() 

saveRDS(overlap_data_new,"Outputs/SDM_MPA_overlap.rds")

#### RUNNING OVERLAP CODE FOR ssp2-4.5 2100--------------

## First, developing a raster mask from the study region shapefile
# SDM_files <- list.files(path = "Data/Raw_data/SDMs/MSS_constrained/", pattern = "ssp245_dec100", full.names = TRUE)
SDM_files <- list.files(path = "C:/Users/npk161/OneDrive - Newcastle University/MPA europe/Analyses/Data/Raw_data/SDMs/MSS_constrained_nobuffer/", pattern = "ssp245_dec100", full.names = TRUE)

#converting the MPA file to a dataframe to get structure for saving results
MPA_data_df <- st_drop_geometry(MPA_data_shp)
names(MPA_data_df) <- c("MPA_class")

start <- Sys.time()
for(i in 1:length(SDM_files)){

  print(i)
  
  #loading in one of the SDM outputs 
  SDM_rast <- rast(SDM_files[i])
  
  #masking to the study region
  rast_masked <- terra::crop(SDM_rast,study_region_rast,mask=TRUE)
  
  #matching the SDM to the CRS of the MPA files
  rast_masked <- project(rast_masked,crs(MPA_data_shp)) 
  
  # Setting areas of 0 probability to NA
  rast_masked[rast_masked$ssp245_dec100==0] <- NA
  
  #calculate %overlap of each raster cell with MPA vector
  overlap <- exact_extract(rast_masked, MPA_data_shp,include_cell=TRUE,include_xy=TRUE,coverage_area=TRUE)
  
  #convert resulting list to dataframe
  names(overlap) <- MPA_data_df$MPA_class
  overlap_frame <- bind_rows(overlap, .id = "MPA_class")
  
  #summarise the overall coverage of each MPA class, making sure to exclude cells with absences (NA values)
  overlap_MPA <- as.data.frame(overlap_frame%>%
                                 drop_na(value)%>%
                                 group_by(MPA_class)%>%
                                 summarise(area=sum(coverage_area))%>%
                                 ungroup())
  
  #setting up an if statement to catch situations where the species is predicted absent everywhere and therefore there is no data to use
  if(dim(overlap_MPA)[1]!=0){
    #if there are presences, identify the total area of presence cells for this species 
    species_area <- sum(as.data.frame(cellSize(rast_masked,mask=TRUE,transform=FALSE,unit="m"))) 
    
    overlap_MPA$sp_area <- species_area
    overlap_MPA$area_prop <- overlap_MPA$area/species_area #calculating proportion of species area covered by the MPA classes
    overlap_MPA$species_ID <- gsub(pattern = ".*taxonid=([0-9]+).*", replacement = "\\1", x = SDM_files[i]) #extracting the species ID from the SDM file to keep trackof results
    
    #setting up the data frame for saving
    if(i == 1){ #
      main_frame  <- overlap_MPA
    } else {
      main_frame <- rbind(main_frame,overlap_MPA)
    }
    
  } else if(dim(overlap_MPA)[1]==0){ #skips to next iteration if no presences in this SDM
    next
  }
  
}
Sys.time()-start

# saving to disk
saveRDS(main_frame,"Outputs/SDM_MPA_overlap_SSP245_2100.rds")

