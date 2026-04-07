rm(list=ls())
library(tidyverse)
library(terra)

## CALCULATING THE TOTAL PAN-EUROPEAN AREA COVERED BY EACH MPA CLASS ----
MPA_files <- vect("Data/Inputs/MPA_layers/pan_european_MPA_classifications.shp")
# is.valid(MPA_files)

#loading the study region file for clipping
study_region <- vect("Data/Inputs/Study_Region_EPSG3035.shp")

#Generate dataframe with the area of each MPA polygon 
areas <- terra::expanse(MPA_files,transform=FALSE,unit="km")
MPA_areas <- as.data.frame(MPA_files)
MPA_areas <- as.data.frame(MPA_areas[,c("MPA_class")])
MPA_areas$MPA_area <- areas
MPA_areas$MPA_area_prop <- (MPA_areas$MPA_area/sum(areas))*100
names(MPA_areas) <- c("MPA_class","MPA_area","MPA_area_prop")

#calculating the area of the full study region
EU_area <- expanse(study_region,unit="km",transform=FALSE)
MPA_areas$MPA_EU_area_prop <- (MPA_areas$MPA_area/EU_area)*100

## SEPARATELY CALCULATING THE TOTAL AREA COVERED BY UK, NOR, FAR AND ICE MPAS ----

#loading the UK, Norwegian, Faroese and Iceland files separately to get their values 
UK_MPAs <- terra::vect("Data/Inputs/MPA_layers/UK MPAs/UK_MPA_classes_3035.shp")
NOR_MPAs <- terra::vect("Data/Inputs/MPA_layers/NOR MPAs/NOR_MPA_classes_3035.shp")
FAR_MPAs <- terra::vect("Data/Inputs/MPA_layers/Faroes MPAs/Faroes_MPAs.shp")
ICE_MPAs <- terra::vect("Data/Inputs/MPA_layers/Iceland MPAs/Iceland_MPAs.shp")

NOR_MPAs <- NOR_MPAs["MPA_class"]
names(NOR_MPAs) <- "MPA_class"
UK_MPAs <- UK_MPAs["PL"]
names(UK_MPAs) <- "MPA_class"
names(FAR_MPAs) <- "MPA_class"
names(ICE_MPAs) <- "MPA_class"

#aligning the classification categories
value_map <- c(
  "unclassified" = "Unclassified",
  "incompatible" = "Incompatible",
  "minimally" = "Minimally",
  "lightly" = "Lightly",
  "highly" = "Highly",
  "fully" = "Fully")

UK_MPAs$MPA_class <- value_map[UK_MPAs$MPA_class]

#merging the layers
UK_NOR_ICE_FAR_MPAs <- rbind(UK_MPAs,NOR_MPAs,ICE_MPAs,FAR_MPAs)

#clipping the MPAs to the study region
UK_NOR_ICE_FAR_MPAs <- terra::intersect(UK_NOR_ICE_FAR_MPAs, study_region)

#calculating areas
MPA_areas_df <- as.data.frame(UK_NOR_ICE_FAR_MPAs)
areas <- expanse(UK_NOR_ICE_FAR_MPAs,unit="km")
MPA_areas_df$MPA_area <- areas

MPA_areas_df <- as.data.frame(MPA_areas_df%>%
                                 dplyr::group_by(MPA_class)%>%
                                 dplyr::summarise(total_area=sum(MPA_area)))

MPA_areas_df$proportion <- MPA_areas_df$total_area/sum(MPA_areas_df$total_area)*100

## CALCULATING THE % COVERAGE OF EACH EEZ ----
rm(list=ls())

MPA_files <- vect("Data/Inputs/MPA_layers/pan_european_MPA_classifications.shp")

study_region_EEZs <- vect("Data/Inputs/Study_Region_EEZs_EPSG3035.shp")

#calculate the area of each MPA polygon
areas <- terra::expanse(MPA_files,transform=FALSE,unit="km")

#getting EEZ areas
study_region_df <- as.data.frame(study_region_EEZs)
study_region_df$total_area <- terra::expanse(study_region_EEZs, unit = "km") # or 'km'
study_region_df <- study_region_df[,c("GEONAME","TERRITORY1","TERRITORY2","total_area")]

#intersecting the MPAs to the regions and getting area
intersection_vect <- terra::intersect(study_region_EEZs,MPA_files)
intersection_vect$overlap_area <- expanse(intersection_vect, unit = "km")
intersection_df <- as.data.frame(intersection_vect)

intersection_df$sov_both <- paste0(intersection_df$SOVEREIGN1_1,intersection_df$SOVEREIGN2_1)

intersection_df <- intersection_df[,c("GEONAME_1","sov_both","MPA_class","overlap_area")]
names(intersection_df) <- c("GEONAME","sov_both","MPA_class","overlap_area")

intersection_df <- intersection_df%>%
  group_by(GEONAME,sov_both,MPA_class)%>%
  summarise(overlap_area = sum(overlap_area))%>%
  ungroup()

#now removing shared/contested areas
study_region_df <- study_region_df[!study_region_df$GEONAME%in% c("Overlapping claim Gibraltar: United Kingdom / Spain","Joint regime area: Sweden / Norway","Joint regime area: Spain / France","Joint regime area: Croatia / Slovenia","Joint regime area: France / Italy"),]

#now getting the summed total area for each nation
#first, remove brackets from sub-territories:
study_region_df <- study_region_df %>%
  mutate(
    GEONAME = str_remove(GEONAME, "\\s*\\([^)]*\\)")
  )

#now summing total area by nation
study_region_df <- study_region_df%>%
  group_by(GEONAME)%>%
  summarise(total_area=sum(total_area))

study_region_df$MPA_class <- NA

all_classes <- c("Fully", "Highly","Incompatible", "Lightly", "Minimally", "Unclassified")

study_region_df_expanded <- study_region_df %>%
  tidyr::complete(GEONAME, MPA_class=all_classes)%>%
  group_by(GEONAME) %>%
  fill(total_area, .direction = "downup") %>%
  ungroup() %>%
  drop_na(MPA_class)

#Now for the interesction dataset, also sorting out the areas where there are multiple nations involved or disputed areas. Here, the area of the shared region, are simply added to each of the relevant nations totals. This means the overall total of areas across all of the EEZs will be slightly inflated.

multi_EEZs <- which(grepl("NA", intersection_df$sov_both)==FALSE)
intersection_df <- intersection_df[-multi_EEZs,]

#remove brackets from sub-territories:
intersection_df <- intersection_df %>%
  mutate(
    GEONAME = str_remove(GEONAME, "\\s*\\([^)]*\\)")
  )

#now sum the overlap areas for the same MPA classes
intersection_df <- intersection_df%>%
  group_by(GEONAME,sov_both,MPA_class)%>%
  summarise(overlap_area=sum(overlap_area))%>%
  ungroup()

#Removing the NA from end of country names
intersection_df$sov_both <- gsub("NA", "", intersection_df$sov_both)
names(intersection_df)[2] <- "Nation"

#joining both of the datasets together
study_region_df_expanded <- study_region_df_expanded%>%
  left_join(intersection_df, by = c("GEONAME", "MPA_class"))

study_region_df_expanded <- study_region_df_expanded%>%
  group_by(GEONAME) %>%
  fill(Nation, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    overlap_area = replace_na(overlap_area, 0)
  )

#calculating the overlap prop:
study_region_df_expanded$overlap_prop <- (study_region_df_expanded$overlap_area/study_region_df_expanded$total_area)*100

#reshaping
study_region_df_wide <- study_region_df_expanded %>%
  select(Nation,MPA_class,overlap_prop)%>%
  pivot_wider(
    names_from = MPA_class,
    values_from = c(overlap_prop)
  )

# now also getting the coverage summary stats across all MPA classes
study_region_df_all <- study_region_df_expanded%>%
  dplyr::group_by(Nation)%>%
  dplyr::summarise(overlap_area=sum(overlap_area))

study_region_df_all <- study_region_df_all%>%
  left_join(study_region_df_expanded%>%
              select(Nation,total_area)%>%
              distinct(Nation, total_area),
            by = c("Nation"))

study_region_df_all$overlap_prop_all <- (study_region_df_all$overlap_area/study_region_df_all$total_area)*100

# now also getting the coverage summary stats for strong and weak MPA classes
#Strong
study_region_df_strong <- study_region_df_expanded %>%
  dplyr::ungroup()%>%
  dplyr::filter(MPA_class =="Fully" | MPA_class == "Highly")%>%
  dplyr::group_by(Nation)%>%
  dplyr::summarise(overlap_area=sum(overlap_area))

names(study_region_df_strong) <- c("Nation","overlap_area_strong")

#Weak
study_region_df_weak <- study_region_df_expanded %>%
  dplyr::ungroup()%>%
  dplyr::filter(MPA_class !="Fully" & MPA_class != "Highly" & MPA_class != "Unclassified")%>%
  dplyr::group_by(Nation)%>%
  dplyr::summarise(overlap_area=sum(overlap_area))

names(study_region_df_weak) <- c("Nation","overlap_area_weak")

#now joining these datasets:
study_region_df_all <- study_region_df_all%>%
  left_join(study_region_df_strong%>%
              select(Nation,overlap_area_strong),by = c("Nation"))

study_region_df_all <- study_region_df_all%>%
  left_join(study_region_df_weak%>%
              select(Nation,overlap_area_weak),by = c("Nation"))

study_region_df_all$overlap_prop_strong <- (study_region_df_all$overlap_area_strong/study_region_df_all$total_area)*100
study_region_df_all$overlap_prop_weak <- (study_region_df_all$overlap_area_weak/study_region_df_all$total_area)*100

study_region_df_all <- study_region_df_all%>%
  select(Nation,overlap_prop_all,overlap_prop_strong,overlap_prop_weak)

#Now merging the full dataset:

study_region_df_wide <- study_region_df_wide%>%
  left_join(study_region_df_all,by = c("Nation"))

study_region_df_wide$Nation[study_region_df_wide$Nation=="Denmark"] <- "Kingdom of Denmark"

write.csv(study_region_df_wide,"Outputs/MPA_overlap_by_EEZ.csv")