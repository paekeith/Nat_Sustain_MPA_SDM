rm(list=ls())
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(terra)
library(cowplot)

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

names(SDM_data)
#now making sure to have a row for each MPA class for all species, to properly represent the coverage by all MPAs
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

#duplicating the data and adding with the 'All' group assignment to create a
SDM_data_all <- SDM_data_complete %>%
  dplyr::mutate(biotic_group_new = "All")

#combining the two datasets for the plotting
SDM_data_sum_comb <- rbind(SDM_data_complete,SDM_data_all)

SDM_data_sum_comb$MPA_class <- factor(SDM_data_sum_comb$MPA_class,ordered = TRUE,levels=rev(c("Unclassified","Incompatible","Minimally","Lightly","Highly","Fully")))
SDM_data_sum_comb$biotic_group_new <- factor(SDM_data_sum_comb$biotic_group_new,ordered = TRUE,levels=c("Benthic invertebrates","Pelagic invertebrates","Demersal fish","Pelagic fish","Elasmobranchs","Mammals","Primary producers","All"))

### PLOTTING BY BIOTIC GROUP --------------
# combining the full and high protection classes
groups_to_combine <- c("Fully", "Highly")

combined_rows <- SDM_data_sum_comb %>%
  # Keep only the rows for the groups we are summing
  dplyr::filter(MPA_class %in% groups_to_combine)%>%
# Group by the unique observation (ID and Species)
  dplyr::group_by(biotic_group_new,conservation_status,habitat_forming_spp,species_ID,Species) %>%
  # Calculate the sum for the new combined group
  dplyr::summarise(
    area_prop = sum(area_prop),
    .groups = 'drop'
  ) %>%
  dplyr::mutate(MPA_class = "Fully / Highly")

#removing the original highly and fully groups
remaining_rows <- SDM_data_sum_comb %>%
  dplyr::filter(!MPA_class %in% groups_to_combine)

# Now, bind them together
SDM_data_combined <- dplyr::bind_rows(remaining_rows, combined_rows) %>%
  # Optional: Arrange to see the results clearly
  dplyr::arrange(species_ID, MPA_class)

#adding in a combined all MPA class group too
groups_to_combine <- c(unique(SDM_data_sum_comb$MPA_class))

combined_rows <- SDM_data_sum_comb %>%
  # Keep only the rows for the groups we are summing
  dplyr::filter(MPA_class %in% groups_to_combine)%>%
  # Group by the unique observation (ID and Species)
  dplyr::group_by(biotic_group_new,conservation_status,habitat_forming_spp,species_ID,Species) %>%
  # Calculate the sum for the new combined group
  dplyr::summarise(
    area_prop = sum(area_prop),
    .groups = 'drop'
  ) %>%
  dplyr::mutate(MPA_class = "All MPAs")

#combining again
SDM_data_combined <- dplyr::bind_rows(SDM_data_combined, combined_rows) %>%
  # Optional: Arrange to see the results clearly
  dplyr::arrange(species_ID, MPA_class)

SDM_data_combined$MPA_class <- factor(SDM_data_combined$MPA_class,ordered = TRUE,levels=c("All MPAs","Unclassified","Incompatible","Minimally","Lightly","Fully / Highly"))

SDM_data_combined$biotic_group_new <- factor(SDM_data_combined$biotic_group_new,ordered = TRUE,levels=c("Benthic invertebrates","Pelagic invertebrates","Demersal fish","Pelagic fish","Elasmobranchs","Mammals","Primary producers","All"))

#identifying and cutting out the values beyond the 95th percentile
SDM_data_combined_95 <- SDM_data_combined %>%
  dplyr::group_by(biotic_group_new,MPA_class) %>%
  dplyr::filter(area_prop <= quantile(area_prop, 0.95, na.rm = TRUE)) %>%
  dplyr::ungroup()

#creating a dataframe that will identify how many species there are per group
facet_counts <- SDM_data_combined %>%
  dplyr::group_by(biotic_group_new) %>%
  dplyr::summarise(n_total = dplyr::n_distinct(species_ID)) %>%
  dplyr::ungroup()

# jpeg("Data/Outputs/Group_class_violin_facets_wide_2026_01_22.jpg",width=20,height=15,units = "in",res=300)
ggplot(SDM_data_combined_95, aes(x = area_prop, y = MPA_class)) +
  # Use geom_violin() to create the violin plot
  geom_violin(data=SDM_data_combined_95,aes(fill = MPA_class,color = MPA_class),scale="width",trim = TRUE,bw=0.01,width=0.9)+
  scale_fill_manual(values = c("white","lightgray","#939598","#2f8a40","#8fc84f","#119eda","#215dab"))+
  scale_color_manual(values = c("black","lightgray","#939598","#2f8a40","#8fc84f","#119eda","#215dab"))+
  geom_boxplot(data=SDM_data_combined,width = 0.2, outlier.shape = NA,fill=NA,colour="black") +
  geom_text(data = facet_counts,aes(x = 0.45, y = 5.2, label = paste("n = ",n_total,sep="")),size = 10,color = "black",family = "serif")+
  # Add labels and a clean theme
  labs(
    y = "MPA protection level",
    x = "Proportion (%) of overlap"
  ) +
  theme_classic() +
  # Adjust the axis labels for better readability
  theme(axis.text = element_text(colour="black",size=25,family = "serif"),
        axis.title = element_text(colour="black",size=30,family = "serif"),
        strip.text = element_text(size = 30,family = "serif"),
        # panel.grid.major.y = element_line(color = "lightgray", linetype = "solid",linewidth = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "lightgray", linetype = "solid",linewidth = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.spacing = unit(1, "cm"),
        plot.margin = unit(c(0, 0.5, 0, 0), "cm"),
        legend.position = "none")+
  scale_x_continuous(breaks=seq(0,1,0.1),labels=seq(0,100,10),limits=c(0,0.6),expand=c(0.01,0))+
  scale_y_discrete(labels=c("All MPAs (11.1%)","Unclassified (0.9%)","Incompatible (3.7%)","Minimally (5.2%)","Lightly (1.2%)","Fully / Highly (0.1%)"))+
  # facet_wrap(~biotic_group_new,ncol=2)
  facet_wrap(~biotic_group_new,ncol=4)

# dev.off()

#### PLOTTING BY CONSERVATION STATUS AND HABITAT FORMERS -------------------

## COMBINING THE FULLY AND HIGHLY CLASSES FOR EACH DATASET
groups_to_combine <- c("Fully", "Highly")

## THREATENED SPECIES
SDM_data_cons_sum <- SDM_data_sum_comb[which(SDM_data_sum_comb$conservation_status=="threatened"),]

combined_rows <- SDM_data_cons_sum %>%
  dplyr::filter(MPA_class %in% groups_to_combine)%>%
  dplyr::group_by(biotic_group_new,conservation_status,habitat_forming_spp,species_ID,Species) %>%
  dplyr::summarise(
    area_prop = sum(area_prop),
    .groups = 'drop'
  ) %>%
  dplyr::mutate(MPA_class = "Fully / Highly")

#removing the original highly and fully groups
remaining_rows <- SDM_data_cons_sum %>%
  dplyr::filter(!MPA_class %in% groups_to_combine)

# Now, bind them together
SDM_data_cons_sum <- dplyr::bind_rows(remaining_rows, combined_rows) %>%
  dplyr::arrange(species_ID, MPA_class)

#adding in a combined all MPA class group too
groups_to_combine <- c(unique(SDM_data_sum_comb$MPA_class))
SDM_data_cons_sum2 <- SDM_data_sum_comb[which(SDM_data_sum_comb$conservation_status=="threatened"),]

combined_rows <- SDM_data_cons_sum2 %>%
  # Keep only the rows for the groups we are summing
  dplyr::filter(MPA_class %in% groups_to_combine)%>%
  # Group by the unique observation (ID and Species)
  dplyr::group_by(biotic_group_new,conservation_status,habitat_forming_spp,species_ID,Species) %>%
  # Calculate the sum for the new combined group
  dplyr::summarise(
    area_prop = sum(area_prop),
    .groups = 'drop'
  ) %>%
  dplyr::mutate(MPA_class = "All MPAs")

#combining again
SDM_data_cons_sum <- dplyr::bind_rows(SDM_data_cons_sum, combined_rows) %>%
  # Optional: Arrange to see the results clearly
  dplyr::arrange(species_ID, MPA_class)

SDM_data_cons_sum$MPA_class <- factor(SDM_data_cons_sum$MPA_class,ordered = TRUE,levels=c("All MPAs","Unclassified","Incompatible","Minimally","Lightly","Fully / Highly"))

SDM_data_cons_sum$biotic_group_new <- factor(SDM_data_cons_sum$biotic_group_new,ordered = TRUE,levels=c("Benthic invertebrates","Pelagic invertebrates","Demersal fish","Pelagic fish","Elasmobranchs","Mammals","Primary producers","All"))

## HABITAT FORMERS
groups_to_combine <- c("Fully", "Highly")
SDM_data_hab_sum <- SDM_data_sum_comb[which(is.na(SDM_data_sum_comb$habitat_forming_spp)==FALSE),]

combined_rows <- SDM_data_hab_sum %>%
  dplyr::filter(MPA_class %in% groups_to_combine)%>%
  dplyr::group_by(biotic_group_new,conservation_status,habitat_forming_spp,species_ID,Species) %>%
  dplyr::summarise(
    area_prop = sum(area_prop),
    .groups = 'drop'
  ) %>%
  dplyr::mutate(MPA_class = "Fully / Highly")

#removing the original highly and fully groups
remaining_rows <- SDM_data_hab_sum %>%
  dplyr::filter(!MPA_class %in% groups_to_combine)

# Now, bind them together
SDM_data_hab_sum <- dplyr::bind_rows(remaining_rows, combined_rows) %>%
  dplyr::arrange(species_ID, MPA_class)

#adding in a combined all MPA class group too
groups_to_combine <- c(unique(SDM_data_sum_comb$MPA_class))
SDM_data_hab_sum2 <- SDM_data_sum_comb[which(is.na(SDM_data_sum_comb$habitat_forming_spp)==FALSE),]

combined_rows <- SDM_data_hab_sum2 %>%
  # Keep only the rows for the groups we are summing
  dplyr::filter(MPA_class %in% groups_to_combine)%>%
  # Group by the unique observation (ID and Species)
  dplyr::group_by(biotic_group_new,conservation_status,habitat_forming_spp,species_ID,Species) %>%
  # Calculate the sum for the new combined group
  dplyr::summarise(
    area_prop = sum(area_prop),
    .groups = 'drop'
  ) %>%
  dplyr::mutate(MPA_class = "All MPAs")

#combining again
SDM_data_hab_sum <- dplyr::bind_rows(SDM_data_hab_sum, combined_rows) %>%
  # Optional: Arrange to see the results clearly
  dplyr::arrange(species_ID, MPA_class)

SDM_data_hab_sum$MPA_class <- factor(SDM_data_hab_sum$MPA_class,ordered = TRUE,levels=c("All MPAs","Unclassified","Incompatible","Minimally","Lightly","Fully / Highly"))

SDM_data_hab_sum$biotic_group_new <- factor(SDM_data_hab_sum$biotic_group_new,ordered = TRUE,levels=c("Benthic invertebrates","Pelagic invertebrates","Demersal fish","Pelagic fish","Elasmobranchs","Mammals","Primary producers","All"))

## PLOTTING THE DATASETS
## THREATENED SPECIES
#identifying and cutting out the values beyond the 95th percentile
SDM_data_cons_sum_95 <- SDM_data_cons_sum %>%
  dplyr::group_by(MPA_class) %>%
  dplyr::filter(area_prop <= quantile(area_prop, 0.95, na.rm = TRUE)) %>%
  dplyr::ungroup()

#Identifying how many species there are per group
SDM_data_cons_sum %>%
  dplyr::group_by(MPA_class) %>%
  dplyr::filter(area_prop <= quantile(area_prop, 0.95, na.rm = TRUE)) %>%
  dplyr::ungroup()%>%
  dplyr::summarise(n_total = dplyr::n_distinct(species_ID)) %>%
  dplyr::ungroup()

#Identifying how many species there are
length(unique(SDM_data_cons_sum$species_ID))

cons_plot <- ggplot(SDM_data_cons_sum_95, aes(x = area_prop, y = MPA_class)) +
  # Use geom_violin() to create the violin plot
  geom_violin(aes(fill = MPA_class,color = MPA_class),position = position_dodge(width=1),scale="width",trim = TRUE,bw=0.01,width=0.9) +
  scale_fill_manual(values = c("white","lightgray","#939598","#2f8a40","#8fc84f","#119eda","#215dab"))+
  scale_color_manual(values = c("black","lightgray","#939598","#2f8a40","#8fc84f","#119eda","#215dab"))+
  geom_boxplot(data=SDM_data_cons_sum,position = position_dodge(width=1),width = 0.2, outlier.shape = NA,fill=NA,colour="black") +
  geom_text(aes(x = 0.48, y = 6.4, label = "n = 226"),size = 10,color = "black",family = "serif")+
    labs(
    y = "MPA protection level",
    x = "Proportion (%) of distribution overlap"
  ) +
  theme_classic() +
  # Adjust the axis labels for better readability
  theme(axis.text = element_text(colour="black",size=25,family="serif"),
        axis.title = element_text(colour="black",size=30,family="serif"),
        strip.text = element_text(size = 30,family="serif"),
        # panel.grid.major.y = element_line(color = "gray", linetype = "solid"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "lightgray", linetype = "solid"),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.position = "none")+
  scale_x_continuous(breaks=seq(0,1,0.1),labels=seq(0,100,10),limits=c(0,0.55),expand=c(0.01,0))+
  scale_y_discrete(labels=c("All MPAs (11.1%)","Unclassified (0.9%)","Incompatible (3.7%)","Minimally (5.2%)","Lightly (1.2%)","Fully / Highly (0.1%)"))
  
## HABITAT FORMERS
#identifying and cutting out the values beyond the 95th percentile
SDM_data_hab_sum_95 <- SDM_data_hab_sum %>%
  dplyr::group_by(MPA_class) %>%
  dplyr::filter(area_prop <= quantile(area_prop, 0.95, na.rm = TRUE)) %>%
  dplyr::ungroup()

#Identifying how many species there are
length(unique(SDM_data_hab_sum$species_ID))

hab_plot <- ggplot(SDM_data_hab_sum_95, aes(x = area_prop, y = MPA_class)) +
  # Use geom_violin() to create the violin plot
  geom_violin(data=SDM_data_hab_sum_95,scale="width",position = "dodge",trim = TRUE,bw=0.01,width=0.9,aes(fill=MPA_class,color=MPA_class)) +
  scale_fill_manual(values = c("white","lightgray","#939598","#2f8a40","#8fc84f","#119eda","#215dab"))+
  scale_color_manual(values = c("black","lightgray","#939598","#2f8a40","#8fc84f","#119eda","#215dab"))+
  geom_boxplot(data=SDM_data_hab_sum,width = 0.2, outlier.shape = NA,fill=NA,colour="black") +
  geom_text(aes(x = 0.48, y = 6.4, label = "n = 134"),size = 10,color = "black",family = "serif")+
  labs(
    y = "",
    x = "Proportion (%) of distribution overlap"
  ) +
  theme_classic() +
  # Adjust the axis labels for better readability
  theme(axis.text = element_text(colour="black",size=25,family="serif"),
        axis.title = element_text(colour="black",size=30,family="serif"),
        strip.text = element_text(size = 30,family="serif"),
        # panel.grid.major.y = element_line(color = "gray", linetype = "solid"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "lightgray", linetype = "solid"),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.position = "none")+
  scale_x_continuous(breaks=seq(0,1,0.1),labels=seq(0,100,10),limits=c(0,0.55),expand=c(0.01,0))+
  scale_y_discrete(labels=c("All MPAs (11.1%)","Unclassified (0.9%)","Incompatible (3.7%)","Minimally (5.2%)","Lightly (1.2%)","Fully / Highly (0.1%)"))
  
## PLOTTING BOTH TOGETHER
# jpeg("Data/Outputs/conservation_habitat_plot_2026_01_22.jpg",width=20,height=10,units = "in",res=300)
plot_grid(cons_plot,hab_plot,
          rel_widths = c(1, 1))+
  draw_label(label="a",x=0.01,y=0.96, fontfamily = "serif", fontface = "bold",size=50)+
  draw_label(label="b",x=0.55,y=0.96, fontfamily = "serif", fontface = "bold",size=50)
# dev.off()


## IDENTIFYING CHANGE IN COVERAGE BY SPECIES/GROUP BETWEEN PRESENT AND SSP2-4.5--------------
#loading the two sets of results
pres_cover <- readRDS("Data/Outputs/SDM_MPA_overlap.rds")
future_cover <- readRDS("Data/Outputs/SDM_MPA_overlap_SSP245_2100.rds")

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

# setting up dataframe for plotting:
# combining the full and high protection classes in both datasets

#present-day
groups_to_combine <- c("Fully", "Highly")

combined_rows <- pres_cover_complete %>%
  # Keep only the rows for the groups we are summing
  filter(MPA_class %in% groups_to_combine)%>%
  # Group by the unique observation (ID and Species)
  group_by(species_ID) %>%
  # Calculate the sum for the new combined group
  summarise(
    area_prop = sum(area_prop),
    area = sum(area),
    sp_area = sum(sp_area)/2,
    .groups = 'drop'
  ) %>%
  mutate(MPA_class = "Fully / Highly")

#removing the original highly and fully groups
remaining_rows <- pres_cover_complete %>%
  filter(!MPA_class %in% groups_to_combine)

# Now, bind them together
pres_cover_complete_comb <- bind_rows(remaining_rows, combined_rows) %>%
  # Optional: Arrange to see the results clearly
  arrange(species_ID, MPA_class)

#adding in a combined all MPA class group too
groups_to_combine <- c(unique(pres_cover_complete$MPA_class))

combined_rows <- pres_cover_complete %>%
  # Keep only the rows for the groups we are summing
  filter(MPA_class %in% groups_to_combine)%>%
  # Group by the unique observation (ID and Species)
  group_by(species_ID) %>%
  # Calculate the sum for the new combined group
  summarise(
    area_prop = sum(area_prop),
    area = sum(area),
    sp_area = sum(sp_area)/2,
    .groups = 'drop') %>%
  mutate(MPA_class = "All MPAs")

#combining again
pres_cover_complete_comb <- bind_rows(pres_cover_complete_comb, combined_rows) %>%
  # Optional: Arrange to see the results clearly
  arrange(species_ID, MPA_class)

pres_cover_complete_comb$MPA_class <- factor(pres_cover_complete_comb$MPA_class,ordered = TRUE,levels=c("All MPAs","Unclassified","Incompatible","Minimally","Lightly","Fully / Highly"))

#future
groups_to_combine <- c("Fully", "Highly")

combined_rows <- future_cover_complete %>%
  # Keep only the rows for the groups we are summing
  filter(MPA_class %in% groups_to_combine)%>%
  # Group by the unique observation (ID and Species)
  group_by(species_ID) %>%
  # Calculate the sum for the new combined group
  summarise(
    area_prop_245 = sum(area_prop_245),
    area_prop_245 = sum(area_prop_245),
    sp_area_245 = sum(sp_area_245)/2,
    .groups = 'drop'
  ) %>%
  mutate(MPA_class = "Fully / Highly")

#removing the original highly and fully groups
remaining_rows <- future_cover_complete %>%
  filter(!MPA_class %in% groups_to_combine)

# Now, bind them together
future_cover_complete_comb <- bind_rows(remaining_rows, combined_rows) %>%
  # Optional: Arrange to see the results clearly
  arrange(species_ID, MPA_class)

#adding in a combined all MPA class group too
groups_to_combine <- c(unique(future_cover_complete$MPA_class))

combined_rows <- future_cover_complete %>%
  # Keep only the rows for the groups we are summing
  filter(MPA_class %in% groups_to_combine)%>%
  # Group by the unique observation (ID and Species)
  group_by(species_ID) %>%
  # Calculate the sum for the new combined group
  summarise(
    area_prop_245 = sum(area_prop_245),
    area_prop_245 = sum(area_prop_245),
    sp_area_245 = sum(sp_area_245)/2,
    .groups = 'drop') %>%
  mutate(MPA_class = "All MPAs")

#combining again
future_cover_complete_comb <- bind_rows(future_cover_complete_comb, combined_rows) %>%
  # Optional: Arrange to see the results clearly
  arrange(species_ID, MPA_class)

future_cover_complete_comb$MPA_class <- factor(future_cover_complete_comb$MPA_class,ordered = TRUE,levels=c("All MPAs","Unclassified","Incompatible","Minimally","Lightly","Fully / Highly"))


#now for each species, identifying the change in cover proportion
merged_data <- merge(pres_cover_complete_comb, future_cover_complete_comb, by = c("species_ID", "MPA_class"), all.x = TRUE)
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

#dropping the 'others' and adding an 'all' group
merged_data <- merged_data[merged_data$biotic_group_new!="Others",]

merged_data_all <- merged_data%>%
  mutate(biotic_group_new="All")

merged_data <- rbind(merged_data,merged_data_all)

merged_data$biotic_group_new <- factor(merged_data$biotic_group_new,ordered = TRUE,levels=c("Benthic invertebrates","Pelagic invertebrates","Demersal fish","Pelagic fish","Elasmobranchs","Mammals","Primary producers","All"))

merged_data$MPA_class <- factor(merged_data$MPA_class,ordered = TRUE,levels=c("All MPAs","Unclassified","Incompatible","Minimally","Lightly","Fully / Highly"))

#calculating summary stats
merged_data2 <- merged_data%>%
  filter(MPA_class!="All MPAs")

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

#calculating the median change in distribution area for each group
change_summary <- merged_data%>%
  distinct(species_ID, .keep_all = TRUE) %>%
  group_by(biotic_group_new)%>%
  summarise(median_change = median(sp_area_change))
  
change_summary$median_change <- change_summary$median_change-1

# identifying how many species are high/med/low risk
length(unique(merged_data[merged_data$risk_group=="Low risk","species_ID"]))/length(unique(merged_data$species_ID))
length(unique(merged_data[merged_data$risk_group=="Medium risk","species_ID"]))/length(unique(merged_data$species_ID))
length(unique(merged_data[merged_data$risk_group=="High risk","species_ID"]))/length(unique(merged_data$species_ID))

## PLOTTING
merged_data_low <- merged_data%>%
  filter(risk_group=="Low risk")

summary_stats_low <- merged_data_low %>%
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

#getting facet counts
facet_counts_low <- merged_data_low %>%
  group_by(biotic_group_new) %>%
  summarise(n_total = n_distinct(species_ID)) %>%
  ungroup()  

#setting up the sequence of values for species proportions gaining and losing:
proportion_data_raw <- merged_data_low %>%
  group_by(biotic_group_new, MPA_class) %>%
  summarise(
    n_total = n(),
    prop_negative = sum(prop_change < 0) / n_total,
    prop_positive = sum(prop_change > 0) / n_total,
    .groups = 'drop'
  )

proportion_data_dynamic <- proportion_data_raw %>%
  left_join(
    summary_stats_low %>% select(biotic_group_new, MPA_class, q0.25, q97.5),
    by = c("biotic_group_new", "MPA_class")
  ) %>%
  # 3. Reshape the data
  pivot_longer(
    cols = starts_with("prop_"),
    names_to = "change_type",
    values_to = "proportion"
  ) %>%
  # 4. Define the x-position dynamically based on quantiles
  mutate(
    # If prop_negative, position it slightly to the LEFT of q0.25
    # If prop_positive, position it slightly to the RIGHT of q97.5
    x_position = case_when(
      change_type == "prop_negative" ~ q0.25 - 2, # Offset left by 2 units
      change_type == "prop_positive" ~ q97.5 + 2  # Offset right by 2 units
    ),
    # Format the label text
    label = paste0(round(proportion * 100), "%")
  )

#plotting:
# jpeg("Data/Outputs/ssp245_change_low_risk.jpg",width=20,height=15,units = "in",res=300)
ggplot(summary_stats_low, aes(x = prop_change, y = MPA_class)) +
  scale_color_manual(values = c("black","lightgray","#939598","#2f8a40","#8fc84f","#119eda","#215dab"))+
  geom_vline(xintercept = 0,linetype="dotted",lwd=1,colour="black")+
  geom_segment(aes(y = MPA_class, yend = MPA_class, x = q0.25, xend = q97.5,colour=MPA_class),size=1)+
  geom_segment(aes(x = q0.25, xend = q0.25, y = as.numeric(as.factor(MPA_class)) - 0.1, yend = as.numeric(as.factor(MPA_class)) + 0.1,colour=MPA_class), size = 1) +
  geom_segment(aes(x = q97.5, xend = q97.5, y = as.numeric(as.factor(MPA_class)) - 0.1, yend = as.numeric(as.factor(MPA_class)) + 0.1,colour=MPA_class), size = 1) +
  geom_point(aes(x=median_value,colour=MPA_class),size=5)+
  geom_text(data = facet_counts_low,aes(x = -26, y = 6.5, label = paste("n = ",n_total,sep="")),size = 10,color = "black",family = "serif")+
  geom_text(data = proportion_data_dynamic,
    # Use pre-calculated x_position, MPA_class for y, and the formatted label
    aes(x = x_position, y = MPA_class, label = label),
    colour = "black",
    size = 8, 
    # Use 'hjust' to align the text: 0.5 centers it on the x_position
    family = "serif",
    hjust = ifelse(proportion_data_dynamic$change_type == "prop_negative", 1, 0)) +
  # Add labels and a clean theme
  labs(
    y = "MPA protection level",
    x = "% change in overlap"
  ) +
  theme_classic() +
  # Adjust the axis labels for better readability
  theme(axis.text = element_text(colour="black",size=25,family = "serif"),
        axis.title = element_text(colour="black",size=30,family = "serif"),
        strip.text = element_text(size = 30,family = "serif"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.spacing = unit(1, "cm"),
        plot.margin = unit(c(0, 0.5, 0, 0), "cm"),
        legend.position = "none")+
  scale_x_continuous(breaks=seq(-100,100,10),labels=seq(-100,100,10),limits=c(-36,22))+
  scale_y_discrete(expand = expansion(mult = c(0, 0),add = c(0.4, 0.8)))+
  facet_wrap(~biotic_group_new,ncol=4)

# dev.off()

## MEDIUM RISK
## PLOTTING
merged_data_med <- merged_data%>%
  filter(risk_group=="Medium risk")

summary_stats_med <- merged_data_med %>%
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

#getting facet counts
facet_counts_med <- merged_data_med %>%
  group_by(biotic_group_new) %>%
  summarise(n_total = n_distinct(species_ID)) %>%
  ungroup()  

#setting up the sequence of values for species proportions gaining and losing:
proportion_data_raw <- merged_data_med %>%
  group_by(biotic_group_new, MPA_class) %>%
  summarise(
    n_total = n(),
    prop_negative = sum(prop_change < 0) / n_total,
    prop_positive = sum(prop_change > 0) / n_total,
    .groups = 'drop'
  )

proportion_data_dynamic <- proportion_data_raw %>%
  left_join(
    summary_stats_med %>% select(biotic_group_new, MPA_class, q0.25, q97.5),
    by = c("biotic_group_new", "MPA_class")
  ) %>%
  # 3. Reshape the data
  pivot_longer(
    cols = starts_with("prop_"),
    names_to = "change_type",
    values_to = "proportion"
  ) %>%
  # 4. Define the x-position dynamically based on quantiles
  mutate(
    # If prop_negative, position it slightly to the LEFT of q0.25
    # If prop_positive, position it slightly to the RIGHT of q97.5
    x_position = case_when(
      change_type == "prop_negative" ~ q0.25 - 2, # Offset left by 2 units
      change_type == "prop_positive" ~ q97.5 + 2  # Offset right by 2 units
    ),
    # Format the label text
    label = paste0(round(proportion * 100), "%")
  )

# jpeg("Data/Outputs/ssp245_change_med_risk.jpg",width=20,height=15,units = "in",res=300)
ggplot(summary_stats_med, aes(x = prop_change, y = MPA_class)) +
  scale_color_manual(values = c("black","lightgray","#939598","#2f8a40","#8fc84f","#119eda","#215dab"))+
  geom_vline(xintercept = 0,linetype="dotted",lwd=1,colour="black")+
  geom_segment(aes(y = MPA_class, yend = MPA_class, x = q0.25, xend = q97.5,colour=MPA_class),size=1)+
  geom_segment(aes(x = q0.25, xend = q0.25, y = as.numeric(as.factor(MPA_class)) - 0.1, yend = as.numeric(as.factor(MPA_class)) + 0.1,colour=MPA_class), size = 1) +
  geom_segment(aes(x = q97.5, xend = q97.5, y = as.numeric(as.factor(MPA_class)) - 0.1, yend = as.numeric(as.factor(MPA_class)) + 0.1,colour=MPA_class), size = 1) +
  geom_point(aes(x=median_value,colour=MPA_class),size=5)+
  geom_text(data = facet_counts_med,aes(x = -10, y = 6.5, label = paste("n = ",n_total,sep="")),size = 10,color = "black",family = "serif")+
  geom_text(data = proportion_data_dynamic,
            # Use pre-calculated x_position, MPA_class for y, and the formatted label
            aes(x = x_position, y = MPA_class, label = label),
            colour = "black",
            size = 8, 
            # Use 'hjust' to align the text: 0.5 centers it on the x_position
            family = "serif",
            hjust = ifelse(proportion_data_dynamic$change_type == "prop_negative", 1, 0)) +
  # Add labels and a clean theme
  labs(
    y = "MPA protection level",
    x = "% change in overlap"
  ) +
  theme_classic() +
  # Adjust the axis labels for better readability
  theme(axis.text = element_text(colour="black",size=25,family = "serif"),
        axis.title = element_text(colour="black",size=30,family = "serif"),
        strip.text = element_text(size = 30,family = "serif"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.spacing = unit(1, "cm"),
        plot.margin = unit(c(0, 0.5, 0, 0), "cm"),
        legend.position = "none")+
  scale_x_continuous(breaks=seq(-100,100,10),labels=seq(-100,100,10),limits=c(-18,28))+
  scale_y_discrete(expand = expansion(mult = c(0, 0),add = c(0.4, 0.8)))+
  facet_wrap(~biotic_group_new,ncol=4)

# dev.off()

## HIGH RISK
## PLOTTING
merged_data_high <- merged_data%>%
  filter(risk_group=="High risk")

summary_stats_high <- merged_data_high %>%
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

#getting facet counts
facet_counts_high <- merged_data_high %>%
  group_by(biotic_group_new) %>%
  summarise(n_total = n_distinct(species_ID)) %>%
  ungroup()  

#setting up the sequence of values for species proportions gaining and losing:
proportion_data_raw <- merged_data_high %>%
  group_by(biotic_group_new, MPA_class) %>%
  summarise(
    n_total = n(),
    prop_negative = sum(prop_change < 0) / n_total,
    prop_positive = sum(prop_change > 0) / n_total,
    .groups = 'drop'
  )

proportion_data_dynamic <- proportion_data_raw %>%
  left_join(
    summary_stats_high %>% select(biotic_group_new, MPA_class, q0.25, q97.5),
    by = c("biotic_group_new", "MPA_class")
  ) %>%
  # 3. Reshape the data
  pivot_longer(
    cols = starts_with("prop_"),
    names_to = "change_type",
    values_to = "proportion"
  ) %>%
  filter(proportion > 0) %>% #making sure proportions of 0 aren't plotted
  # 4. Define the x-position dynamically based on quantiles
  mutate(
    # If prop_negative, position it slightly to the LEFT of q0.25
    # If prop_positive, position it slightly to the RIGHT of q97.5
    x_position = case_when(
      change_type == "prop_negative" ~ q0.25 - 2, # Offset left by 2 units
      change_type == "prop_positive" ~ q97.5 + 2  # Offset right by 2 units
    ),
    # Format the label text
    label = paste0(round(proportion * 100), "%")
  )

# jpeg("Data/Outputs/ssp245_change_high_risk.jpg",width=20,height=15,units = "in",res=300)
ggplot(summary_stats_high, aes(x = prop_change, y = MPA_class)) +
  scale_color_manual(values = c("black","lightgray","#939598","#2f8a40","#8fc84f","#119eda","#215dab"))+
  geom_vline(xintercept = 0,linetype="dotted",lwd=1,colour="black")+
  geom_segment(aes(y = MPA_class, yend = MPA_class, x = q0.25, xend = q97.5,colour=MPA_class),size=1)+
  geom_segment(aes(x = q0.25, xend = q0.25, y = as.numeric(as.factor(MPA_class)) - 0.1, yend = as.numeric(as.factor(MPA_class)) + 0.1,colour=MPA_class), size = 1) +
  geom_segment(aes(x = q97.5, xend = q97.5, y = as.numeric(as.factor(MPA_class)) - 0.1, yend = as.numeric(as.factor(MPA_class)) + 0.1,colour=MPA_class), size = 1) +
  geom_point(aes(x=median_value,colour=MPA_class),size=5)+
  geom_text(data = facet_counts_high,aes(x = -25, y = 6.5, label = paste("n = ",n_total,sep="")),size = 10,color = "black",family = "serif")+
  geom_text(data = proportion_data_dynamic,
            # Use pre-calculated x_position, MPA_class for y, and the formatted label
            aes(x = x_position, y = MPA_class, label = label),
            colour = "black",
            size = 8, 
            # Use 'hjust' to align the text: 0.5 centers it on the x_position
            family = "serif",
            hjust = ifelse(proportion_data_dynamic$change_type == "prop_negative", 1, 0)) +
  # Add labels and a clean theme
  labs(
    y = "MPA protection level",
    x = "% change in overlap"
  ) +
  theme_classic() +
  # Adjust the axis labels for better readability
  theme(axis.text = element_text(colour="black",size=25,family = "serif"),
        axis.title = element_text(colour="black",size=30,family = "serif"),
        strip.text = element_text(size = 30,family = "serif"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.spacing = unit(1, "cm"),
        plot.margin = unit(c(0, 0.5, 0, 0), "cm"),
        legend.position = "none")+
  scale_x_continuous(breaks=seq(-100,100,20),labels=seq(-100,100,20),limits=c(-40,50))+
  scale_y_discrete(expand = expansion(mult = c(0, 0),add = c(0.4, 0.8)))+
  facet_wrap(~biotic_group_new,ncol=4)

# dev.off()
