rm(list=ls())
library(tidyverse)

#### Matching results to the biotic groupings ####
#load species groupings and SDM overlap data
SDM_overlaps <- readRDS("Data/Outputs/SDM_MPA_overlap.rds")

sp_info_df <- readRDS("Data/Inputs/Full_sp_groups.rds")

#tidying the group names
sp_info_df$biotic_group_new[which(sp_info_df$biotic_group_new=="elasmobranchs")] <- "Elasmobranchs"
sp_info_df$biotic_group_new[which(sp_info_df$biotic_group_new=="mammals")] <- "Mammals"

SDM_data <- merge(SDM_overlaps,sp_info_df,by="species_ID",all.x=FALSE)

# #renaming the MPA classes
SDM_data$MPA_class[SDM_data$MPA_class=="unclassified"] <- "Unclassified"
SDM_data$MPA_class[SDM_data$MPA_class=="incompatible"] <- "Incompatible"
SDM_data$MPA_class[SDM_data$MPA_class=="minimally"] <- "Minimally"
SDM_data$MPA_class[SDM_data$MPA_class=="lightly"] <- "Lightly"
SDM_data$MPA_class[SDM_data$MPA_class=="highly"] <- "Highly"
SDM_data$MPA_class[SDM_data$MPA_class=="fully"] <- "Fully"

#cutting out the remaining 'others' 
SDM_data <- SDM_data[which(SDM_data$biotic_group_new!="Others"),]

#now ensuring each species has a row for each MPA class, regardless of whether they overlap or not
table(SDM_data$MPA_class)

SDM_data_complete <- SDM_data %>%
  tidyr::complete(species_ID, MPA_class, fill = list(value = 0))#splitting taxa into categories of change:

SDM_data_complete$area_prop[which(is.na(SDM_data_complete$area_prop)==TRUE)] <- 0
SDM_data_complete$area[which(is.na(SDM_data_complete$area)==TRUE)] <- 0

#copying missing values for each column from other rows for same species
SDM_data_complete <- SDM_data_complete %>%
  dplyr::group_by(species_ID) %>%
  tidyr::fill(c(sp_area,Species,biotic_group,biotic_group_new,conservation_status,habitat_forming_spp,others_group), .direction = "downup") %>%
  dplyr::ungroup()

table(SDM_data_complete$MPA_class)

#Adding a new column with an 'All' designation to eventually create a dataset which includes coverage coverage across all taxa
SDM_data_all <- SDM_data_complete %>%
  dplyr::mutate(biotic_group_new = "All")

#combining the two datasets
SDM_data_sum_comb <- rbind(SDM_data_complete,SDM_data_all)

#converting columns to ordered factors
SDM_data_sum_comb$MPA_class <- factor(SDM_data_sum_comb$MPA_class,ordered = TRUE,levels=rev(c("Unclassified","Incompatible","Minimally","Lightly","Highly","Fully")))
SDM_data_sum_comb$biotic_group_new <- factor(SDM_data_sum_comb$biotic_group_new,ordered = TRUE,levels=c("Benthic invertebrates","Pelagic invertebrates","Demersal fish","Pelagic fish","Elasmobranchs","Mammals","Primary producers","All"))

## GENERATING SUMMARY TABLES -----
## BIOTIC GROUPS ####

# getting summary stats regardless of group or MPA class
SDM_data_sum_comb1 <- SDM_data_sum_comb%>%
  dplyr::filter(biotic_group_new!="All")%>%
  dplyr::group_by(species_ID)%>%
  dplyr::summarise(area_prop=sum(area_prop))%>%
  dplyr::ungroup()%>%
  dplyr::summarise(
    # mean_value = mean(area_prop, na.rm = TRUE),
    Median = median(area_prop*100, na.rm = TRUE),
    # q25 = quantile(area_prop, 0.25, na.rm = TRUE),
    # q75 = quantile(area_prop, 0.75, na.rm = TRUE),
    Q95 = quantile(area_prop*100, 0.95, na.rm = TRUE),
    Min = min(area_prop*100, na.rm = TRUE),
    Max = max(area_prop*100, na.rm = TRUE)
  )%>%
  tidyr::pivot_longer(cols = c(Median, Q95, Min, Max),
               names_to = "Statistic",
               values_to = "value")

# now getting summary stats by group and by MPA class
SDM_data_sum_comb2 <- SDM_data_sum_comb %>%
  dplyr::group_by(species_ID,biotic_group_new,MPA_class) %>%
  dplyr::summarise(area_prop=sum(area_prop)) %>%
  dplyr::group_by(biotic_group_new,MPA_class) %>%
  dplyr::summarise(
    # mean_value = mean(area_prop, na.rm = TRUE),
    Median = median(area_prop*100, na.rm = TRUE),
    # q25 = quantile(area_prop, 0.25, na.rm = TRUE),
    # q75 = quantile(area_prop, 0.75, na.rm = TRUE),
    Q95 = quantile(area_prop*100, 0.95, na.rm = TRUE),
    Min = min(area_prop*100, na.rm = TRUE),
    Max = max(area_prop*100, na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(cols = c(Median, Q95, Min, Max),
               names_to = "Statistic",
               values_to = "value") %>%
  tidyr::pivot_wider(
    names_from = MPA_class,
    values_from = value)

# now also getting the coverage summary stats for each group across all MPA classes
SDM_data_sum_comb3 <- SDM_data_sum_comb%>%
  dplyr::filter(biotic_group_new !="All")%>%
  dplyr::group_by(species_ID,biotic_group_new)%>%
  dplyr::summarise(area_prop=sum(area_prop))%>%
  dplyr::group_by(biotic_group_new)%>%
  dplyr::summarise(
    # mean_value = mean(area_prop, na.rm = TRUE),
    Median = median(area_prop*100, na.rm = TRUE),
    # q25 = quantile(area_prop, 0.25, na.rm = TRUE),
    # q75 = quantile(area_prop, 0.75, na.rm = TRUE),
    Q95 = quantile(area_prop*100, 0.95, na.rm = TRUE),
    Min = min(area_prop*100, na.rm = TRUE),
    Max = max(area_prop*100, na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(
    # cols = c(mean_value, median_value, q25, q75,q95, min_value, max_value),
    cols = c(Median, Q95, Min, Max),
    names_to = "Statistic",
    values_to = "value"
  )

#adding to main dataframe
SDM_data_sum_comb2$'All MPAs' <- NA
SDM_data_sum_comb2$'All MPAs'[1:nrow(SDM_data_sum_comb3)] <- SDM_data_sum_comb3$value
SDM_data_sum_comb2$'All MPAs'[which(is.na(SDM_data_sum_comb2$'All MPAs')==TRUE)] <- SDM_data_sum_comb1$value

#ADDING IN STRONG AND WEAK PROTECTION CLASSES
#Strong
combined_strong <- SDM_data_sum_comb %>%
  dplyr::ungroup()%>%
  dplyr::filter(MPA_class =="Fully" | MPA_class == "Highly")%>%
  dplyr::group_by(species_ID,biotic_group_new)%>%
  dplyr::summarise(area_prop=sum(area_prop))%>%
  dplyr::group_by(biotic_group_new)%>%
  dplyr::summarise(
    Median = median(area_prop*100, na.rm = TRUE),
    Q95 = quantile(area_prop*100, 0.95, na.rm = TRUE),
    Min = min(area_prop*100, na.rm = TRUE),
    Max = max(area_prop*100, na.rm = TRUE)
  )

#reshaping
combined_strong_long <- combined_strong %>%
  tidyr::pivot_longer(
    # cols = c(mean_value, median_value, q25, q75,q95, min_value, max_value),
    cols = c(Median, Q95, Min, Max),
    names_to = "Statistic",
    values_to = "value"
  )

SDM_data_sum_comb2$'Strong' <- combined_strong_long$value

combined_weak <- SDM_data_sum_comb %>%
  dplyr::ungroup()%>%
  dplyr::filter(MPA_class !="Fully" & MPA_class != "Highly" & MPA_class != "Unclassified")%>%
  dplyr::group_by(species_ID,biotic_group_new)%>%
  dplyr::summarise(area_prop=sum(area_prop))%>%
  dplyr::group_by(biotic_group_new)%>%
  dplyr::summarise(
    Median = median(area_prop*100, na.rm = TRUE),
    Q95 = quantile(area_prop*100, 0.95, na.rm = TRUE),
    Min = min(area_prop*100, na.rm = TRUE),
    Max = max(area_prop*100, na.rm = TRUE)
  )

#reshaping
combined_weak_long <- combined_weak %>%
  tidyr::pivot_longer(
    # cols = c(mean_value, median_value, q25, q75,q95, min_value, max_value),
    cols = c(Median, Q95, Min, Max),
    names_to = "Statistic",
    values_to = "value"
  )

SDM_data_sum_comb2$'Weak' <- combined_weak_long$value

#rounding values
SDM_data_sum_comb2 <- SDM_data_sum_comb2 %>%
  dplyr::mutate(dplyr::across(where(is.numeric), round, 2))

write.csv(SDM_data_sum_comb2,"Data/Outputs/SDM_MPA_overlap_summary_biotic_groups.csv",row.names = FALSE)

## THREATENED AND HABITAT FORMERS ####
SDM_data_cons_sum <- SDM_data[which(SDM_data$conservation_status=="threatened"),]

# getting summary stats for figure by group
summary_df_cons <- SDM_data_cons_sum %>%
  dplyr::group_by(MPA_class)%>%
  dplyr::summarise(
    Median = median(area_prop*100, na.rm = TRUE),
    Q95 = quantile(area_prop*100, 0.95, na.rm = TRUE),
    Min = min(area_prop*100, na.rm = TRUE),
    Max = max(area_prop*100, na.rm = TRUE))

#reshaping
summary_df_long_cons <- summary_df_cons %>%
  tidyr::pivot_longer(
    cols = c(Median, Q95, Min, Max),
    names_to = "stat",
    values_to = "value"
  )
summary_df_wide_cons <- summary_df_long_cons %>%
  tidyr::pivot_wider(
    names_from = MPA_class,
    values_from = value
  )

summary_df_wide_cons$group <- "Threatened"

summary_df_wide_cons$'All MPAs' <- NA

# getting summary stats regardless of MPA class
SDM_data_cons_sum1 <- SDM_data_cons_sum%>%
  dplyr::group_by(species_ID)%>%
  dplyr::summarise(area_prop=sum(area_prop))%>%
  dplyr::ungroup()%>%
  dplyr::summarise(
    Median = median(area_prop*100, na.rm = TRUE),
    Q95 = quantile(area_prop*100, 0.95, na.rm = TRUE),
    Min = min(area_prop*100, na.rm = TRUE),
    Max = max(area_prop*100, na.rm = TRUE))%>%
  tidyr::pivot_longer(cols = c(Median, Q95, Min, Max),
               names_to = "Statistic",
               values_to = "value")

summary_df_wide_cons$'All MPAs' <- SDM_data_cons_sum1$value

#ADDING IN STRONG AND WEAK PROTECTION CLASSES
#Strong
combined_strong <- SDM_data_cons_sum %>%
  dplyr::ungroup()%>%
  dplyr::filter(MPA_class =="Fully" | MPA_class == "Highly")%>%
  dplyr::group_by(species_ID)%>%
  dplyr::summarise(area_prop=sum(area_prop))%>%
  dplyr::ungroup()%>%
  dplyr::summarise(
    Median = median(area_prop*100, na.rm = TRUE),
    Q95 = quantile(area_prop*100, 0.95, na.rm = TRUE),
    Min = min(area_prop*100, na.rm = TRUE),
    Max = max(area_prop*100, na.rm = TRUE)
  )

#reshaping
combined_strong_long <- combined_strong %>%
  tidyr::pivot_longer(
    # cols = c(mean_value, median_value, q25, q75,q95, min_value, max_value),
    cols = c(Median, Q95, Min, Max),
    names_to = "Statistic",
    values_to = "value"
  )

summary_df_wide_cons$'Strong' <- combined_strong_long$value

combined_weak <- SDM_data_cons_sum %>%
  dplyr::ungroup()%>%
  dplyr::filter(MPA_class !="Fully" & MPA_class != "Highly" & MPA_class != "Unclassified")%>%
  dplyr::group_by(species_ID)%>%
  dplyr::summarise(area_prop=sum(area_prop))%>%
  dplyr::ungroup()%>%
  dplyr::summarise(
    Median = median(area_prop*100, na.rm = TRUE),
    Q95 = quantile(area_prop*100, 0.95, na.rm = TRUE),
    Min = min(area_prop*100, na.rm = TRUE),
    Max = max(area_prop*100, na.rm = TRUE))

#reshaping
combined_weak_long <- combined_weak %>%
  tidyr::pivot_longer(
    # cols = c(mean_value, median_value, q25, q75,q95, min_value, max_value),
    cols = c(Median, Q95, Min, Max),
    names_to = "Statistic",
    values_to = "value"
  )

summary_df_wide_cons$'Weak' <- combined_weak_long$value

#HABITAT FORMERS
#totalling the area covered for habitat formers by MPA class
SDM_data_hab_sum <- SDM_data[which(is.na(SDM_data$habitat_forming_spp)==FALSE),]

# getting summary stats for figure by group
summary_df_hab <- SDM_data_hab_sum %>%
  dplyr::group_by(MPA_class)%>%
  dplyr::summarise(
    Median = median(area_prop*100, na.rm = TRUE),
    Q95 = quantile(area_prop*100, 0.95, na.rm = TRUE),
    Min = min(area_prop*100, na.rm = TRUE),
    Max = max(area_prop*100, na.rm = TRUE))

#reshaping
summary_df_long_hab <- summary_df_hab %>%
  tidyr::pivot_longer(
    cols = c(Median, Q95, Min, Max),
    names_to = "stat",
    values_to = "value"
  )

summary_df_wide_hab <- summary_df_long_hab %>%
  tidyr::pivot_wider(
    names_from = MPA_class,
    values_from = value
  )

summary_df_wide_hab$group <- "Habitat-formers"

summary_df_wide_hab$'All MPAs' <- NA

# getting summary stats regardless of MPA class
SDM_data_hab_sum1 <- SDM_data_hab_sum%>%
  dplyr::group_by(species_ID)%>%
  dplyr::summarise(area_prop=sum(area_prop))%>%
  dplyr::ungroup()%>%
  dplyr::summarise(
    Median = median(area_prop*100, na.rm = TRUE),
    Q95 = quantile(area_prop*100, 0.95, na.rm = TRUE),
    Min = min(area_prop*100, na.rm = TRUE),
    Max = max(area_prop*100, na.rm = TRUE))%>%
  tidyr::pivot_longer(cols = c(Median, Q95, Min, Max),
               names_to = "Statistic",
               values_to = "value")

summary_df_wide_hab$'All MPAs' <- SDM_data_hab_sum1$value

#ADDING IN STRONG AND WEAK PROTECTION CLASSES
#Strong
combined_strong <- SDM_data_hab_sum %>%
  dplyr::ungroup()%>%
  dplyr::filter(MPA_class =="Fully" | MPA_class == "Highly")%>%
  dplyr::group_by(species_ID)%>%
  dplyr::summarise(area_prop=sum(area_prop))%>%
  dplyr::ungroup()%>%
  dplyr::summarise(
    Median = median(area_prop*100, na.rm = TRUE),
    Q95 = quantile(area_prop*100, 0.95, na.rm = TRUE),
    Min = min(area_prop*100, na.rm = TRUE),
    Max = max(area_prop*100, na.rm = TRUE)
  )

#reshaping
combined_strong_long <- combined_strong %>%
  tidyr::pivot_longer(
    # cols = c(mean_value, median_value, q25, q75,q95, min_value, max_value),
    cols = c(Median, Q95, Min, Max),
    names_to = "Statistic",
    values_to = "value"
  )

summary_df_wide_hab$'Strong' <- combined_strong_long$value

combined_weak <- SDM_data_hab_sum %>%
  dplyr::ungroup()%>%
  dplyr::filter(MPA_class !="Fully" & MPA_class != "Highly" & MPA_class != "Unclassified")%>%
  dplyr::group_by(species_ID)%>%
  dplyr::summarise(area_prop=sum(area_prop))%>%
  dplyr::ungroup()%>%
  dplyr::summarise(
    Median = median(area_prop*100, na.rm = TRUE),
    Q95 = quantile(area_prop*100, 0.95, na.rm = TRUE),
    Min = min(area_prop*100, na.rm = TRUE),
    Max = max(area_prop*100, na.rm = TRUE))

#reshaping
combined_weak_long <- combined_weak %>%
  tidyr::pivot_longer(
    # cols = c(mean_value, median_value, q25, q75,q95, min_value, max_value),
    cols = c(Median, Q95, Min, Max),
    names_to = "Statistic",
    values_to = "value"
  )

summary_df_wide_hab$'Weak' <- combined_weak_long$value

#combining the two datasets
summary_df_comb <- rbind(summary_df_wide_cons,summary_df_wide_hab)

#rounding values
summary_df_comb <- summary_df_comb %>%
  dplyr::mutate(dplyr::across(where(is.numeric), round, 2))

#saving
write.csv(summary_df_comb,"Data/Outputs/SDM_MPA_overlap_summary_threatened_habitat_formers.csv",row.names = FALSE)

#### CHANGE IN COVERAGE UNDER SSP245 -----
## PREPPING THE DATA #### 
#loading the two sets of results
pres_cover <- readRDS("Data/Outputs/SDM_MPA_overlap.rds")
future_cover <-  readRDS("Data/Outputs/SDM_MPA_overlap_SSP245_2100.rds")

#cutting out all species from results that aren't common across both
pres_cover <- pres_cover[pres_cover$species_ID%in%future_cover$species_ID,]
future_cover <- future_cover[future_cover$species_ID%in%pres_cover$species_ID,]

#now making sure to have a row for each MPA class for all species in each dataset
#present day
pres_cover_complete <- pres_cover %>%
  complete(species_ID, MPA_class, fill = list(value = 0))#splitting taxa into categories of change:

pres_cover_complete$area_prop[which(is.na(pres_cover_complete$area_prop)==TRUE)] <- 0
pres_cover_complete$area[which(is.na(pres_cover_complete$area)==TRUE)] <- 0

pres_cover_complete <- pres_cover_complete %>%
  group_by(species_ID) %>%
  fill(sp_area, .direction = "downup") %>%
  ungroup()

table(pres_cover_complete$MPA_class)

#future
future_cover_complete <- future_cover %>%
  complete(species_ID, MPA_class, fill = list(value = 0)) 

future_cover_complete$area_prop[which(is.na(future_cover_complete$area_prop)==TRUE)] <- 0
future_cover_complete$area[which(is.na(future_cover_complete$area)==TRUE)] <- 0

future_cover_complete <- future_cover_complete %>%
  group_by(species_ID) %>%
  fill(sp_area, .direction = "downup") %>%
  ungroup()

names(future_cover_complete) <- c("species_ID","MPA_class","area_245","sp_area_245","area_prop_245")
table(future_cover_complete$MPA_class)

#renaming the MPA classes
pres_cover_complete$MPA_class[pres_cover_complete$MPA_class=="unclassified"] <- "Unclassified"
pres_cover_complete$MPA_class[pres_cover_complete$MPA_class=="incompatible"] <- "Incompatible"
pres_cover_complete$MPA_class[pres_cover_complete$MPA_class=="minimally"] <- "Minimally"
pres_cover_complete$MPA_class[pres_cover_complete$MPA_class=="lightly"] <- "Lightly"
pres_cover_complete$MPA_class[pres_cover_complete$MPA_class=="highly"] <- "Highly"
pres_cover_complete$MPA_class[pres_cover_complete$MPA_class=="fully"] <- "Fully"

future_cover_complete$MPA_class[future_cover_complete$MPA_class=="unclassified"] <- "Unclassified"
future_cover_complete$MPA_class[future_cover_complete$MPA_class=="incompatible"] <- "Incompatible"
future_cover_complete$MPA_class[future_cover_complete$MPA_class=="minimally"] <- "Minimally"
future_cover_complete$MPA_class[future_cover_complete$MPA_class=="lightly"] <- "Lightly"
future_cover_complete$MPA_class[future_cover_complete$MPA_class=="highly"] <- "Highly"
future_cover_complete$MPA_class[future_cover_complete$MPA_class=="fully"] <- "Fully"

#Merging the datasets
merged_data <- merge(pres_cover_complete, future_cover_complete, by = c("species_ID", "MPA_class"), all.x = TRUE)
#identifying change in cover for each species
merged_data$prop_change <- merged_data$area_prop_245-merged_data$area_prop #calculating the change in proportional overlap with each MPA class 
merged_data$sp_area_change <- merged_data$sp_area_245/merged_data$sp_area #calculating overall proportional change in species range size

#establishing risk groups
merged_data$risk_group <- NA
merged_data$risk_group[merged_data$sp_area_change>=0.9] <- "Low risk"
merged_data$risk_group[merged_data$sp_area_change>=0.5 & merged_data$sp_area_change<0.9] <- "Medium risk"
merged_data$risk_group[merged_data$sp_area_change<0.5] <- "High risk"

## LINKING BIOTIC GROUPS
sp_info_df <- readRDS("Data/Inputs/Full_sp_groups.rds")

#tidying the group names
sp_info_df$biotic_group_new[which(sp_info_df$biotic_group_new=="elasmobranchs")] <- "Elasmobranchs"
sp_info_df$biotic_group_new[which(sp_info_df$biotic_group_new=="mammals")] <- "Mammals"

merged_data <- merge(merged_data,sp_info_df,by="species_ID",all.x=FALSE)

#cutting out the remaining 'others' 
merged_data <- merged_data[which(merged_data$biotic_group_new!="Others"),]

#adding an 'all' group
merged_data_all <- merged_data%>%
  mutate(biotic_group_new="All")

merged_data <- rbind(merged_data,merged_data_all)

merged_data$biotic_group_new <- factor(merged_data$biotic_group_new,ordered = TRUE,levels=c("Benthic invertebrates","Pelagic invertebrates","Demersal fish","Pelagic fish","Elasmobranchs","Mammals","Primary producers","All"))

merged_data$MPA_class <- factor(merged_data$MPA_class,ordered = TRUE,levels=c("Unclassified","Incompatible","Minimally","Lightly","Highly","Fully","Fully / Highly"))

## GETTING SUMMARY TABLES FOR PROPORTIONS OF SPECIES CHANGING ####

# calculating the proportion of species overall that gain or lose area
length(unique(merged_data[merged_data$sp_area_change>1,"species_ID"]))/length(unique(merged_data$species_ID))
length(unique(merged_data[merged_data$sp_area_change<1,"species_ID"]))/length(unique(merged_data$species_ID))
length(unique(merged_data[merged_data$sp_area_change==1,"species_ID"]))/length(unique(merged_data$species_ID))

# calculating the proportion of species that gain or lose area by biotic group
change_props <- merged_data%>%
  distinct(species_ID, .keep_all = TRUE) %>%
  group_by(biotic_group_new)%>%
  mutate(
    Change_Direction = case_when(
      sp_area_change > 1 ~ "Positive",
      sp_area_change < 1 ~ "Negative",
      TRUE       ~ "No Change" # Optional: include species with zero change
    )
  )%>%
  count(Change_Direction)%>%
  group_by(biotic_group_new)%>%
  mutate(
    Total_Species = sum(n),
    Proportion = n / Total_Species
  ) %>%
  ungroup()

#calculating the median change in area for each group
change_summary <- merged_data%>%
  distinct(species_ID, .keep_all = TRUE) %>%
  group_by(biotic_group_new)%>%
  summarise(median_change = median(sp_area_change))

change_summary$median_change <- change_summary$median_change-1

# GETTING SUMMARY TABLES FOR GENERAL COVERAGE CHANGES ####
#ALL TAXA ####
# getting summary stats regardless of group or MPA class
summary_stats_all <- merged_data%>%
  filter(MPA_class!="Fully / Highly" & biotic_group_new =="All")%>%
  group_by(species_ID,biotic_group_new)%>%
  summarise(prop_change=sum(prop_change))%>%
  group_by(biotic_group_new)%>%
  summarise(
    # mean_value = mean(area_prop, na.rm = TRUE),
    Median = median(prop_change*100, na.rm = TRUE),
    Q2.5 = quantile(prop_change*100, 0.025, na.rm = TRUE),
    Q97.5 = quantile(prop_change*100, 0.975, na.rm = TRUE),
    # q95 = quantile(prop_change*100, 0.95, na.rm = TRUE),
    Min = min(prop_change*100, na.rm = TRUE),
    Max = max(prop_change*100, na.rm = TRUE)
  )%>%
  pivot_longer(cols = c(Median, Q2.5,Q97.5, Min, Max),
               names_to = "Statistic",
               values_to = "value")

# getting summary stats by group regardless of MPA class
summary_stats_group_all <- merged_data%>%
  filter(MPA_class!="Fully / Highly" & biotic_group_new !="All")%>%
  group_by(species_ID,biotic_group_new)%>%
  summarise(prop_change=sum(prop_change))%>%
  group_by(biotic_group_new)%>%
  summarise(
    # mean_value = mean(area_prop, na.rm = TRUE),
    Median = median(prop_change*100, na.rm = TRUE),
    Q2.5 = quantile(prop_change*100, 0.025, na.rm = TRUE),
    Q97.5 = quantile(prop_change*100, 0.975, na.rm = TRUE),
    # q95 = quantile(prop_change*100, 0.95, na.rm = TRUE),
    Min = min(prop_change*100, na.rm = TRUE),
    Max = max(prop_change*100, na.rm = TRUE)
  )%>%
  pivot_longer(cols = c(Median, Q2.5,Q97.5, Min, Max),
               names_to = "Statistic",
               values_to = "value")

#getting summary stats by biotic group and MPA class
summary_stats_group_class <- merged_data %>%
  filter(MPA_class!="Fully / Highly")%>%
  group_by(MPA_class,biotic_group_new) %>%
  summarise(
    # mean_value = mean(area_prop, na.rm = TRUE),
    Median = median(prop_change*100, na.rm = TRUE),
    Q2.5 = quantile(prop_change*100, 0.025, na.rm = TRUE),
    Q97.5 = quantile(prop_change*100, 0.975, na.rm = TRUE),
    # q95 = quantile(prop_change*100, 0.95, na.rm = TRUE),
    Min = min(prop_change*100, na.rm = TRUE),
    Max = max(prop_change*100, na.rm = TRUE)
  )

#reshaping
summary_stats_group_class_long <- summary_stats_group_class %>%
  pivot_longer(
    cols = c(Median, Q2.5,Q97.5, Min, Max),
    names_to = "stat",
    values_to = "value"
  )

summary_stats_group_class_wide <- summary_stats_group_class_long %>%
  pivot_wider(
    names_from = MPA_class,
    values_from = value
  )

#combining
summary_stats_group_class_wide$'All MPAs' <- NA
summary_stats_group_class_wide$'All MPAs'[1:nrow(summary_stats_group_all)] <- summary_stats_group_all$value
summary_stats_group_class_wide$'All MPAs'[which(is.na(summary_stats_group_class_wide$'All MPAs')==TRUE)] <- summary_stats_all$value

#Adding in the Strong and Weak protection levels

#ADDING IN STRONG AND WEAK PROTECTION CLASSES
#Strong
combined_strong <- merged_data %>%
  ungroup()%>%
  filter(MPA_class =="Fully" | MPA_class == "Highly")%>%
  group_by(species_ID,biotic_group_new)%>%
  summarise(prop_change=sum(prop_change))%>%
  group_by(biotic_group_new)%>%
  summarise(
    Median = median(prop_change*100, na.rm = TRUE),
    Q2.5 = quantile(prop_change*100, 0.025, na.rm = TRUE),
    Q97.5 = quantile(prop_change*100, 0.975, na.rm = TRUE),
    Min = min(prop_change*100, na.rm = TRUE),
    Max = max(prop_change*100, na.rm = TRUE))

#reshaping
combined_strong_long <- combined_strong %>%
  pivot_longer(
    cols = c(Median, Q2.5,Q97.5, Min, Max),
    names_to = "Statistic",
    values_to = "value"
  )

summary_stats_group_class_wide$'Strong' <- combined_strong_long$value

combined_weak <- merged_data %>%
  ungroup()%>%
  filter(MPA_class !="Fully" & MPA_class != "Highly" & MPA_class != "Unclassified")%>%
  group_by(species_ID,biotic_group_new)%>%
  summarise(prop_change=sum(prop_change))%>%
  group_by(biotic_group_new)%>%
  summarise(
    Median = median(prop_change*100, na.rm = TRUE),
    Q2.5 = quantile(prop_change*100, 0.025, na.rm = TRUE),
    Q97.5 = quantile(prop_change*100, 0.975, na.rm = TRUE),
    Min = min(prop_change*100, na.rm = TRUE),
    Max = max(prop_change*100, na.rm = TRUE))

#reshaping
combined_weak_long <- combined_weak %>%
  pivot_longer(
    cols = c(Median, Q2.5,Q97.5, Min, Max),
    names_to = "Statistic",
    values_to = "value"
  )


summary_stats_group_class_wide$'Weak' <- combined_weak_long$value

#rounding values
summary_stats_group_class_wide <- summary_stats_group_class_wide %>%
  mutate(across(where(is.numeric), round, 2))

#reordering
summary_stats_group_class_wide <- summary_stats_group_class_wide %>%
  select(biotic_group_new,stat,`All MPAs`,Strong,Weak,Fully,Highly,Lightly,Minimally,Incompatible,Unclassified)

names(summary_stats_group_class_wide)[c(1,2)] <- c("Biotic group","Statistic")

write.csv(summary_stats_group_class_wide,"Data/Outputs/SSP245_change_all_risk_classes.csv",row.names = FALSE)

## LOW RISK TAXA ####
merged_data_low <- merged_data%>%
  filter(risk_group=="Low risk")

# getting summary stats regardless of group or MPA class
summary_stats_low1 <- merged_data_low%>%
  filter(biotic_group_new=="All")%>%
  group_by(species_ID)%>%
  summarise(prop_change=sum(prop_change))%>%
  ungroup()%>%
  summarise(
    # mean_value = mean(area_prop, na.rm = TRUE),
    median_value = median(prop_change*100, na.rm = TRUE),
    q0.25 = quantile(prop_change*100, 0.025, na.rm = TRUE),
    q97.5 = quantile(prop_change*100, 0.975, na.rm = TRUE),
    # q95 = quantile(prop_change*100, 0.95, na.rm = TRUE),
    min_value = min(prop_change*100, na.rm = TRUE),
    max_value = max(prop_change*100, na.rm = TRUE)
  )%>%
  pivot_longer(cols = c(median_value, q0.25,q97.5, min_value, max_value),
               names_to = "Statistic",
               values_to = "value")

# getting summary stats by group regardless of MPA class
summary_stats_low2 <- merged_data_low%>%
  group_by(species_ID,biotic_group_new)%>%
  summarise(prop_change=sum(prop_change))%>%
  group_by(biotic_group_new)%>%
  summarise(
    # mean_value = mean(area_prop, na.rm = TRUE),
    median_value = median(prop_change*100, na.rm = TRUE),
    q0.25 = quantile(prop_change*100, 0.025, na.rm = TRUE),
    q97.5 = quantile(prop_change*100, 0.975, na.rm = TRUE),
    # q95 = quantile(prop_change*100, 0.95, na.rm = TRUE),
    min_value = min(prop_change*100, na.rm = TRUE),
    max_value = max(prop_change*100, na.rm = TRUE)
  )%>%
  pivot_longer(cols = c(median_value, q0.25,q97.5, min_value, max_value),
               names_to = "Statistic",
               values_to = "value")

#getting summary stats by biotic group and MPA class
summary_stats_low3 <- merged_data_low %>%
  group_by(MPA_class,biotic_group_new) %>%
  summarise(
    # mean_value = mean(area_prop, na.rm = TRUE),
    median_value = median(prop_change*100, na.rm = TRUE),
    q0.25 = quantile(prop_change*100, 0.025, na.rm = TRUE),
    q97.5 = quantile(prop_change*100, 0.975, na.rm = TRUE),
    # q95 = quantile(prop_change*100, 0.95, na.rm = TRUE),
    min_value = min(prop_change*100, na.rm = TRUE),
    max_value = max(prop_change*100, na.rm = TRUE)
  )

#reshaping
summary_stats_low3_long <- summary_stats_low3 %>%
  pivot_longer(
    cols = c(median_value,q0.25,q97.5,min_value, max_value),
    names_to = "stat",
    values_to = "value"
  )

summary_stats_low3_wide <- summary_stats_low3_long %>%
  pivot_wider(
    names_from = MPA_class,
    values_from = value
  )

#adding to main dataframe
summary_stats_low3_wide$'All MPAs' <- NA
summary_stats_low3_wide$'All MPAs'[1:nrow(summary_stats_low2)] <- summary_stats_low2$value
summary_stats_low3_wide$'All MPAs'[which(is.na(summary_stats_low3_wide$'All MPAs')==TRUE)] <- summary_stats_low1$value

#ADDING IN STRONG AND WEAK PROTECTION CLASSES
#Strong
combined_strong <- merged_data_low %>%
  ungroup()%>%
  filter(MPA_class =="Fully" | MPA_class == "Highly")%>%
  group_by(species_ID,biotic_group_new)%>%
  summarise(prop_change=sum(prop_change))%>%
  group_by(biotic_group_new)%>%
  summarise(
    Median = median(prop_change*100, na.rm = TRUE),
    Q2.5 = quantile(prop_change*100, 0.025, na.rm = TRUE),
    Q97.5 = quantile(prop_change*100, 0.975, na.rm = TRUE),
    Min = min(prop_change*100, na.rm = TRUE),
    Max = max(prop_change*100, na.rm = TRUE))

#reshaping
combined_strong_long <- combined_strong %>%
  pivot_longer(
    cols = c(Median, Q2.5,Q97.5, Min, Max),
    names_to = "Statistic",
    values_to = "value"
  )

summary_stats_low3_wide$'Strong' <- combined_strong_long$value

combined_weak <- merged_data_low %>%
  ungroup()%>%
  filter(MPA_class !="Fully" & MPA_class != "Highly" & MPA_class != "Unclassified")%>%
  group_by(species_ID,biotic_group_new)%>%
  summarise(prop_change=sum(prop_change))%>%
  group_by(biotic_group_new)%>%
  summarise(
    Median = median(prop_change*100, na.rm = TRUE),
    Q2.5 = quantile(prop_change*100, 0.025, na.rm = TRUE),
    Q97.5 = quantile(prop_change*100, 0.975, na.rm = TRUE),
    Min = min(prop_change*100, na.rm = TRUE),
    Max = max(prop_change*100, na.rm = TRUE))

#reshaping
combined_weak_long <- combined_weak %>%
  pivot_longer(
    cols = c(Median, Q2.5,Q97.5, Min, Max),
    names_to = "Statistic",
    values_to = "value"
  )


summary_stats_low3_wide$'Weak' <- combined_weak_long$value

#rounding values
summary_stats_low3_wide <- summary_stats_low3_wide %>%
  mutate(across(where(is.numeric), round, 2))

#reordering
summary_stats_low3_wide <- summary_stats_low3_wide %>%
  select(biotic_group_new,stat,`All MPAs`,Strong,Weak,Fully,Highly,Lightly,Minimally,Incompatible,Unclassified)

names(summary_stats_low3_wide)[c(1,2)] <- c("Biotic group","Statistic")

write.csv(summary_stats_low3_wide,"Data/Outputs/SSP245_change_low_risk_classes2.csv",row.names = FALSE)


## MEDIUM RISK TAXA ####
merged_data_med <- merged_data%>%
  filter(risk_group=="Medium risk")

# getting summary stats regardless of group or MPA class
summary_stats_med1 <- merged_data_med%>%
  filter(biotic_group_new=="All")%>%
  group_by(species_ID)%>%
  summarise(prop_change=sum(prop_change))%>%
  ungroup()%>%
  summarise(
    # mean_value = mean(area_prop, na.rm = TRUE),
    median_value = median(prop_change*100, na.rm = TRUE),
    q0.25 = quantile(prop_change*100, 0.025, na.rm = TRUE),
    q97.5 = quantile(prop_change*100, 0.975, na.rm = TRUE),
    # q95 = quantile(prop_change*100, 0.95, na.rm = TRUE),
    min_value = min(prop_change*100, na.rm = TRUE),
    max_value = max(prop_change*100, na.rm = TRUE)
  )%>%
  pivot_longer(cols = c(median_value, q0.25,q97.5, min_value, max_value),
               names_to = "Statistic",
               values_to = "value")

# getting summary stats by group regardless of MPA class
summary_stats_med2 <- merged_data_med%>%
  group_by(species_ID,biotic_group_new)%>%
  summarise(prop_change=sum(prop_change))%>%
  group_by(biotic_group_new)%>%
  summarise(
    # mean_value = mean(area_prop, na.rm = TRUE),
    median_value = median(prop_change*100, na.rm = TRUE),
    q0.25 = quantile(prop_change*100, 0.025, na.rm = TRUE),
    q97.5 = quantile(prop_change*100, 0.975, na.rm = TRUE),
    # q95 = quantile(prop_change*100, 0.95, na.rm = TRUE),
    min_value = min(prop_change*100, na.rm = TRUE),
    max_value = max(prop_change*100, na.rm = TRUE)
  )%>%
  pivot_longer(cols = c(median_value, q0.25,q97.5, min_value, max_value),
               names_to = "Statistic",
               values_to = "value")

#getting summary stats by biotic group and MPA class
summary_stats_med3 <- merged_data_med %>%
  group_by(MPA_class,biotic_group_new) %>%
  summarise(
    # mean_value = mean(area_prop, na.rm = TRUE),
    median_value = median(prop_change*100, na.rm = TRUE),
    q0.25 = quantile(prop_change*100, 0.025, na.rm = TRUE),
    q97.5 = quantile(prop_change*100, 0.975, na.rm = TRUE),
    # q95 = quantile(prop_change*100, 0.95, na.rm = TRUE),
    min_value = min(prop_change*100, na.rm = TRUE),
    max_value = max(prop_change*100, na.rm = TRUE)
  )

#reshaping
summary_stats_med3_long <- summary_stats_med3 %>%
  pivot_longer(
    cols = c(median_value,q0.25,q97.5,min_value, max_value),
    names_to = "stat",
    values_to = "value"
  )

summary_stats_med3_wide <- summary_stats_med3_long %>%
  pivot_wider(
    names_from = MPA_class,
    values_from = value
  )

#adding to main dataframe
summary_stats_med3_wide$'All MPAs' <- NA
summary_stats_med3_wide$'All MPAs'[1:nrow(summary_stats_med2)] <- summary_stats_med2$value
summary_stats_med3_wide$'All MPAs'[which(is.na(summary_stats_med3_wide$'All MPAs')==TRUE)] <- summary_stats_med1$value

#ADDING IN STRONG AND WEAK PROTECTION CLASSES
#Strong
combined_strong <- merged_data_med %>%
  ungroup()%>%
  filter(MPA_class =="Fully" | MPA_class == "Highly")%>%
  group_by(species_ID,biotic_group_new)%>%
  summarise(prop_change=sum(prop_change))%>%
  group_by(biotic_group_new)%>%
  summarise(
    Median = median(prop_change*100, na.rm = TRUE),
    Q2.5 = quantile(prop_change*100, 0.025, na.rm = TRUE),
    Q97.5 = quantile(prop_change*100, 0.975, na.rm = TRUE),
    Min = min(prop_change*100, na.rm = TRUE),
    Max = max(prop_change*100, na.rm = TRUE))

#reshaping
combined_strong_long <- combined_strong %>%
  pivot_longer(
    cols = c(Median, Q2.5,Q97.5, Min, Max),
    names_to = "Statistic",
    values_to = "value")

summary_stats_med3_wide$'Strong' <- combined_strong_long$value

combined_weak <- merged_data_med %>%
  ungroup()%>%
  filter(MPA_class !="Fully" & MPA_class != "Highly" & MPA_class != "Unclassified")%>%
  group_by(species_ID,biotic_group_new)%>%
  summarise(prop_change=sum(prop_change))%>%
  group_by(biotic_group_new)%>%
  summarise(
    Median = median(prop_change*100, na.rm = TRUE),
    Q2.5 = quantile(prop_change*100, 0.025, na.rm = TRUE),
    Q97.5 = quantile(prop_change*100, 0.975, na.rm = TRUE),
    Min = min(prop_change*100, na.rm = TRUE),
    Max = max(prop_change*100, na.rm = TRUE))

#reshaping
combined_weak_long <- combined_weak %>%
  pivot_longer(
    cols = c(Median, Q2.5,Q97.5, Min, Max),
    names_to = "Statistic",
    values_to = "value")

summary_stats_med3_wide$'Weak' <- combined_weak_long$value

#rounding values
summary_stats_med3_wide <- summary_stats_med3_wide %>%
  mutate(across(where(is.numeric), round, 2))

#reordering
summary_stats_med3_wide <- summary_stats_med3_wide %>%
  select(biotic_group_new,stat,`All MPAs`,Strong,Weak,Fully,Highly,Lightly,Minimally,Incompatible,Unclassified)

names(summary_stats_med3_wide)[c(1,2)] <- c("Biotic group","Statistic")

write.csv(summary_stats_med3_wide,"Data/Outputs/SSP245_change_med_risk_classes2.csv",row.names = FALSE)

## HIGH RISK ####

merged_data_high <- merged_data%>%
  filter(risk_group=="High risk")

# getting summary stats regardless of group or MPA class
summary_stats_high1 <- merged_data_high%>%
  filter(biotic_group_new=="All")%>%
  group_by(species_ID)%>%
  summarise(prop_change=sum(prop_change))%>%
  ungroup()%>%
  summarise(
    # mean_value = mean(area_prop, na.rm = TRUE),
    median_value = median(prop_change*100, na.rm = TRUE),
    q0.25 = quantile(prop_change*100, 0.025, na.rm = TRUE),
    q97.5 = quantile(prop_change*100, 0.975, na.rm = TRUE),
    # q95 = quantile(prop_change*100, 0.95, na.rm = TRUE),
    min_value = min(prop_change*100, na.rm = TRUE),
    max_value = max(prop_change*100, na.rm = TRUE)
  )%>%
  pivot_longer(cols = c(median_value, q0.25,q97.5, min_value, max_value),
               names_to = "Statistic",
               values_to = "value")

# getting summary stats by group regardless of MPA class
summary_stats_high2 <- merged_data_high%>%
  group_by(species_ID,biotic_group_new)%>%
  summarise(prop_change=sum(prop_change))%>%
  group_by(biotic_group_new)%>%
  summarise(
    # mean_value = mean(area_prop, na.rm = TRUE),
    median_value = median(prop_change*100, na.rm = TRUE),
    q0.25 = quantile(prop_change*100, 0.025, na.rm = TRUE),
    q97.5 = quantile(prop_change*100, 0.975, na.rm = TRUE),
    # q95 = quantile(prop_change*100, 0.95, na.rm = TRUE),
    min_value = min(prop_change*100, na.rm = TRUE),
    max_value = max(prop_change*100, na.rm = TRUE)
  )%>%
  pivot_longer(cols = c(median_value, q0.25,q97.5, min_value, max_value),
               names_to = "Statistic",
               values_to = "value")

#getting summary stats by biotic group and MPA class
summary_stats_high3 <- merged_data_high %>%
  group_by(MPA_class,biotic_group_new) %>%
  summarise(
    # mean_value = mean(area_prop, na.rm = TRUE),
    median_value = median(prop_change*100, na.rm = TRUE),
    q0.25 = quantile(prop_change*100, 0.025, na.rm = TRUE),
    q97.5 = quantile(prop_change*100, 0.975, na.rm = TRUE),
    # q95 = quantile(prop_change*100, 0.95, na.rm = TRUE),
    min_value = min(prop_change*100, na.rm = TRUE),
    max_value = max(prop_change*100, na.rm = TRUE)
  )

#reshaping
summary_stats_high3_long <- summary_stats_high3 %>%
  pivot_longer(
    cols = c(median_value,q0.25,q97.5,min_value, max_value),
    names_to = "stat",
    values_to = "value"
  )

summary_stats_high3_wide <- summary_stats_high3_long %>%
  pivot_wider(
    names_from = MPA_class,
    values_from = value
  )

#adding to main dataframe
summary_stats_high3_wide$'All MPAs' <- NA
summary_stats_high3_wide$'All MPAs'[1:nrow(summary_stats_high2)] <- summary_stats_high2$value
summary_stats_high3_wide$'All MPAs'[which(is.na(summary_stats_high3_wide$'All MPAs')==TRUE)] <- summary_stats_high1$value

#ADDING IN STRONG AND WEAK PROTECTION CLASSES
#Strong
combined_strong <- merged_data_high %>%
  ungroup()%>%
  filter(MPA_class =="Fully" | MPA_class == "Highly")%>%
  group_by(species_ID,biotic_group_new)%>%
  summarise(prop_change=sum(prop_change))%>%
  group_by(biotic_group_new)%>%
  summarise(
    Median = median(prop_change*100, na.rm = TRUE),
    Q2.5 = quantile(prop_change*100, 0.025, na.rm = TRUE),
    Q97.5 = quantile(prop_change*100, 0.975, na.rm = TRUE),
    Min = min(prop_change*100, na.rm = TRUE),
    Max = max(prop_change*100, na.rm = TRUE))

#reshaping
combined_strong_long <- combined_strong %>%
  pivot_longer(
    cols = c(Median, Q2.5,Q97.5, Min, Max),
    names_to = "Statistic",
    values_to = "value")

summary_stats_high3_wide$'Strong' <- combined_strong_long$value

combined_weak <- merged_data_high %>%
  ungroup()%>%
  filter(MPA_class !="Fully" & MPA_class != "Highly" & MPA_class != "Unclassified")%>%
  group_by(species_ID,biotic_group_new)%>%
  summarise(prop_change=sum(prop_change))%>%
  group_by(biotic_group_new)%>%
  summarise(
    Median = median(prop_change*100, na.rm = TRUE),
    Q2.5 = quantile(prop_change*100, 0.025, na.rm = TRUE),
    Q97.5 = quantile(prop_change*100, 0.975, na.rm = TRUE),
    Min = min(prop_change*100, na.rm = TRUE),
    Max = max(prop_change*100, na.rm = TRUE))

#reshaping
combined_weak_long <- combined_weak %>%
  pivot_longer(
    cols = c(Median, Q2.5,Q97.5, Min, Max),
    names_to = "Statistic",
    values_to = "value")

summary_stats_high3_wide$'Weak' <- combined_weak_long$value

#rounding values
summary_stats_high3_wide <- summary_stats_high3_wide %>%
  mutate(across(where(is.numeric), round, 2))

#reordering
summary_stats_high3_wide <- summary_stats_high3_wide %>%
  select(biotic_group_new,stat,`All MPAs`,Strong,Weak,Fully,Highly,Lightly,Minimally,Incompatible,Unclassified)

names(summary_stats_high3_wide)[c(1,2)] <- c("Biotic group","Statistic")

write.csv(summary_stats_high3_wide,"Data/Outputs/SSP245_change_high_risk_classes2.csv",row.names = FALSE)
