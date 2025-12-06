###############
#Lochranza Data
###############

#########
#Plants
########

#Read the data
Plants <- read.csv("Lochranza Plants.csv", header = TRUE)

#Dplyr enables dataframe manipulation
install.packages("dplyr")
library(dplyr)
library(stringr)


#Subset quadrats by slope and location (High, Middle, Low)
Plants2 <- Plants %>%
  mutate(Block = str_extract(eventID, "^[NS][0-9]"))
Plants_sub <- Plants2 %>%
  filter(Block %in% c("N1", "N2", "N3", "S1", "S2", "S3"))
Block_richness <- Plants_sub %>%
  group_by(Block) %>%
  summarise(
    SpeciesRichness = n_distinct(Commonname)
  )

#Plot plant richness by slope and by each transect
install.packages("ggplot2")
library(ggplot2)

palette_blocks <- c(
  "N1" = "#6A5ACD",  
  "N2" = "#836FFF",  
  "N3" = "#9370DB",  
  "S1" = "#2E8B57",  
  "S2" = "#66CDAA",  
  "S3" = "#98FB98"   
)

ggplot(Block_richness, aes(x = Block, y = SpeciesRichness, fill = Block)) +
  geom_col() +
  scale_fill_manual(values = palette_blocks) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Quadrat Block",
    y = "Species Richness",
    title = "Plant Species Richness by Block"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

#Plotting Invasive Species
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)  # for percent_format()

# Read the data
invasive <- read.csv("Plant invasive.csv", header = TRUE)

#Clean data and create blocks per transect
Plantsinvasive <- invasive %>%
  distinct() %>%  # remove exact duplicate rows if any
  mutate(Block = str_extract(eventID, "^[NS][0-9]")) %>%  # N1, N2, ...
  filter(Block %in% c("N1", "N2", "N3", "S1", "S2", "S3"))

#Presence of invasive plants per quadrat
inv_presence <- Plantsinvasive %>%
  group_by(eventID, Block) %>%
  summarise(
    InvasivePresent = any(Invasive == "Invasive"),
    .groups = "drop"
  )

#Proportion of quadrats with/without invasives per transect
inv_summary <- inv_presence %>%
  group_by(Block, InvasivePresent) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Block) %>%
  mutate(prop = n / sum(n))

#Colors for if there is a presence of invasive plants species in the quadrat or if there isn't
palette_invasive <- c(
  "TRUE"  = "#E67E22",  
  "FALSE" = "#A3C9A8"  
)

#Plot proportion of quadrats with/without invasives per transect
ggplot(inv_summary, aes(x = Block, y = prop, fill = InvasivePresent)) +
  geom_col(color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = palette_invasive,
    labels = c(
      "FALSE" = "No invasive plants",
      "TRUE"  = "Invasive plants present"
    )
  ) +
  labs(
    x = "Transect",
    y = "Proportion of quadrats",
    fill = "Invasiveness",
    title = "Proportion of quadrats containing invasive plants"
  ) +
  theme_minimal(base_size = 14)

#Add a Region column: North vs South
library(dplyr)
library(stringr)

Plantsinvasive <- Plantsinvasive %>%
  distinct() %>%
  mutate(
    Block = str_extract(eventID, "^[NS][0-9]"),
    Region = if_else(str_starts(Block, "N"), "North slope", "South slope")
  ) %>%
  filter(Block %in% c("N1", "N2", "N3", "S1", "S2", "S3"))

#Species richness for North vs South
rich_region <- Plantsinvasive %>%
  group_by(Region) %>%
  summarise(
    SpeciesRichness = n_distinct(Commonname),
    .groups = "drop"
  )

#Invasive presence per quadrat, then per Region
inv_presence <- Plantsinvasive %>%
  group_by(eventID, Block, Region) %>%
  summarise(
    InvasivePresent = any(Invasive == "Invasive"),
    .groups = "drop"
  )
inv_region <- inv_presence %>%
  group_by(Region, InvasivePresent) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Region) %>%
  mutate(prop = n / sum(n))

#Plot richness: North vs South
ggplot(rich_region, aes(x = Region, y = SpeciesRichness, fill = Region)) +
  geom_col() +
  scale_fill_manual(values = c("North slope" = "#9370DB", "South slope" = "#2E8B57")) +
  labs(
    x = "",
    y = "Total species richness",
    title = "Plant species richness by slope"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

#Plot proportion of quadrats containing invasive plants by overall slope
palette_invasive <- c(
  "TRUE"  = "#E67E22",   
  "FALSE" = "#A3C9A8"    
)

ggplot(inv_region, aes(x = Region, y = prop, fill = InvasivePresent)) +
  geom_col(color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = palette_invasive,
    labels = c("FALSE" = "No invasive plants", "TRUE" = "Invasive plants present")
  ) +
  labs(
    x = "",
    y = "Proportion of quadrats",
    fill = "Invasiveness",
    title = "Quadrats containing invasive plants by slope"
  ) +
  theme_minimal(base_size = 14)

#Block-level richness
p_block_richness <- ggplot(Block_richness, aes(x = Block, y = SpeciesRichness, fill = Block)) +
  geom_col() +
  scale_fill_manual(values = palette_blocks) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Quadrat Block",
    y = "Species Richness",
    title = "Plant Species Richness by Block"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

#Arrange the plots side-by-side
#Transect richness and overall slope richness
#Transect invasive and slope invasive
install.packages("patchwork")
library(patchwork)
p_block_richness + p_region_richness
p_block_invasive + p_region_invasive

#########################################
# Invertebrate Analysis: Ground + Flying
########################################

# Install Packages

library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)

#Read the data

# Ground-dwelling invertebrates (pitfall + sweep net)
Insects <- read.csv("Ground insects.csv", header = TRUE)

# Flying invertebrates (moths, light traps)
FlyingInsect <- read.csv("Moths.csv", header = TRUE)

FlyingInsect

###Separate the data by slope and method

#Ground insects (pitfall + sweep net)
GroundInverts <- Insects %>%
  distinct() %>%  # remove any exact duplicate rows
  mutate(
    Block  = str_extract(eventID, "^[NS][0-9]"),              # e.g. N1, N2, S3
    Region = if_else(str_starts(Block, "N"), "North slope", "South slope"),
    Method = case_when(
      str_detect(eventID, "TIN") ~ "Sweep net",               #sweep nets
      str_detect(eventID, "TIP") ~ "Pitfall",                 #pitfalls
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    Block %in% c("N1", "N2", "N3", "S1", "S2", "S3"),
    !is.na(Method)
  ) %>%
  select(eventID, Block, Region, Method, Commonname, individualCount)


##Flying invertebrates (light traps, mostly moths)
FlyingInverts <- FlyingInsect %>%
  distinct() %>%
  mutate(
    Block  = str_extract(eventID, "^[NS][0-9]"),              # e.g. N3, S3
    Region = if_else(str_starts(Block, "N"), "North slope", "South slope"),
    Method = "Moth trap"                                      #Add this as a third method
  ) %>%
  # keep only the moth sampling blocks
  filter(Block %in% c("N3", "S3")) %>%
  select(eventID, Block, Region, Method, Commonname, individualCount)


###Combine both ground invertebrates and flying invertebrates
Inverts_All <- bind_rows(GroundInverts, FlyingInverts)


###Create Summary Tables for species richness and abundance per slope per method and overall

#Slope × Method (Pitfall, Sweep net, Moth trap)
region_method_summary <- Inverts_All %>%
  group_by(Region, Method) %>%
  summarise(
    SpeciesRichness = n_distinct(Commonname),
    Abundance       = sum(individualCount, na.rm = TRUE),
    .groups = "drop"
  )

print("Summary by Region × Method:")
print(region_method_summary)


#Overall Slopes totals (all methods combined)
region_overall <- Inverts_All %>%
  group_by(Region) %>%
  summarise(
    SpeciesRichness = n_distinct(Commonname),
    Abundance       = sum(individualCount, na.rm = TRUE),
    .groups = "drop"
  )

print("Overall Region totals:")
print(region_overall)

###Plots per method and per slope

# Color palettes
method_cols <- c(
  "Sweep net" = "#1B9E77",
  "Pitfall"   = "#D95F02",   
  "Moth trap" = "#7570B3"    
)


slope_cols <- c(
  "North slope" = "#9370DB",
  "South slope" = "#2E8B57"
)


#Richness by slope × method
p_rich_method <- ggplot(region_method_summary,
                        aes(x = Region, y = SpeciesRichness, fill = Method)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = method_cols) +
  labs(
    x = "",
    y = "Species richness",
    fill = "Method",
    title = "Invertebrate species richness by slope and sampling method"
  ) +
  theme_minimal(base_size = 14)

#Abundance by slope × method
p_abund_method <- ggplot(region_method_summary,
                         aes(x = Region, y = Abundance, fill = Method)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = method_cols) +
  labs(
    x = "",
    y = "Abundance (individuals)",
    fill = "Method",
    title = "Invertebrate abundance by slope and sampling method"
  ) +
  theme_minimal(base_size = 14)


#Overall richness (all methods combined)
p_rich_overall <- ggplot(region_overall,
                         aes(x = Region, y = SpeciesRichness, fill = Region)) +
  geom_col() +
  scale_fill_manual(values = slope_cols) +
  labs(
    x = "",
    y = "Total species richness",
    title = "Overall invertebrate species richness by slope"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


#Overall abundance (all methods combined)
p_abund_overall <- ggplot(region_overall,
                          aes(x = Region, y = Abundance, fill = Region)) +
  geom_col() +
  scale_fill_manual(values = slope_cols) +
  labs(
    x = "",
    y = "Total abundance (individuals)",
    title = "Overall invertebrate abundance by slope"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


###Combine plots

#Method split (richness, abundance)
#Overall North vs South (richness, abundance)

combined_plot <- (p_rich_method | p_abund_method)
overall_plot <-  (p_rich_overall | p_abund_overall)

# Print to RStudio plots panel
print(combined_plot)
print(overall_plot)

#Ground insects (pitfall + sweep net)
GroundInverts <- Insects %>%
  distinct() %>%
  mutate(
    Block  = str_extract(eventID, "^[NS][0-9]"),
    Region = if_else(str_starts(Block, "N"), "North slope", "South slope"),
    Method = case_when(
      str_detect(eventID, "TIN") ~ "Sweep net",
      str_detect(eventID, "TIP") ~ "Pitfall",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    Block %in% c("N1", "N2", "N3", "S1", "S2", "S3"),
    !is.na(Method)
  ) %>%
  select(eventID, Block, Region, Method, Order, Commonname, individualCount)

#Flying invertebrates (moths)
FlyingInverts <- FlyingInsect %>%
  distinct() %>%
  mutate(
    Block  = str_extract(eventID, "^[NS][0-9]"),
    Region = if_else(str_starts(Block, "N"), "North slope", "South slope"),
    Method = "Moth trap"
  ) %>%
  filter(Block %in% c("N3", "S3")) %>%
  select(eventID, Block, Region, Method, Order, Commonname, individualCount)

#Combine
Inverts_All <- bind_rows(GroundInverts, FlyingInverts)

#Add taxonomic groupings
inv_comp <- Inverts_All %>%
  mutate(
    Taxon_group = case_when(
      Order %in% "Araneae"                       ~ "Spiders",
      Order %in% "Ixodida"                       ~ "Ticks",
      Order %in% "Diptera"                       ~ "Flies & crane flies",
      Order %in% "Lepidoptera"                   ~ "Moths & caterpillars",
      Order %in% c("Hemiptera", "Homoptera")     ~ "True bugs & aphids",
      Order %in% "Hymenoptera"                   ~ "Ants & wasps",
      TRUE                                       ~ "Other invertebrates"
    )
  ) %>%
  group_by(Region, Taxon_group) %>%
  summarise(
    Abundance = sum(individualCount, na.rm = TRUE),
    .groups   = "drop"
  )

#Side by side bar plots
p_comp_abund <- ggplot(inv_comp,
                       aes(x = Taxon_group,
                           y = Abundance,
                           fill = Region)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = slope_cols) +
  labs(
    x = "Taxonomic group",
    y = "Total abundance (individuals)",
    fill = "Slope",
    title = "Invertebrate abundance by taxonomic group and slope"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_comp_abund

##########################
# Freshwater Invertebrates
##########################

# Read the data
Freshwater <- read.csv("Freshwater.csv", header = TRUE)

# Load packages
library(dplyr)
library(stringr)
library(ggplot2)
library(ggh4x)

# Prepare the dataset: remove duplicates, classify Region + Position
FreshInvert <- Freshwater %>%
  distinct() %>%
  mutate(
    Region = case_when(
      str_detect(eventID, "^N") ~ "North",
      str_detect(eventID, "^S") ~ "South"
    ),
    Position = case_when(
      str_detect(eventID, "^[NS]U") ~ "Upstream",
      str_detect(eventID, "^[NS]D") ~ "Downstream"
    )
  )

# Summarise species richness by site
fresh_sites <- FreshInvert %>%
  group_by(Region, Position, eventID = as.character(eventID)) %>%
  summarise(
    SpeciesRichness = n_distinct(Commonname),
    .groups = "drop"
  )

# Order regions
fresh_sites$Region <- factor(fresh_sites$Region, levels = c("North", "South"))

# Plot richness only
ggplot(fresh_sites,
       aes(x = eventID, y = SpeciesRichness, fill = Position)) +
  geom_col() +
  
  facet_wrap2(
    ~ Region,
    nrow = 1,
    scales = "free_x",
    strip = strip_themed(
      background_x = list(
        element_rect(fill = "#E6DAF5", colour = "#E6DAF5"),  # North = purple tint
        element_rect(fill = "#C8F7C5", colour = "#C8F7C5")   # South = green tint
      ),
      text_x = list(
        element_text(face = "bold"),
        element_text(face = "bold")
      )
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Downstream" = "#66B2FF",
      "Upstream"   = "#003B73"
    )
  ) +
  
  labs(
    x = "Site (eventID)",
    y = "Species richness",
    fill = "Position",
    title = "Freshwater invertebrate species richness by site"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#####
#Bats
#####

#Read Bat Data
Bats <- read.csv("Bats.csv", header = TRUE)

#Extract Region
BatData <- Bats %>%
  mutate(
    Region = ifelse(startsWith(eventID, "N"), "North", "South")
  )

#Species Richness per Event ID
library(dplyr)

bat_richness <- BatData %>%
  filter(!is.na(scientificName)) %>%      
  group_by(eventID, Region) %>%
  summarise(
    SpeciesRichness = n_distinct(scientificName),
    .groups = "drop"
  )

######
#Birds
######

#Read Bird Data
Birds <- read.csv("Bird.csv", header = TRUE)

#Create Regions and Inbound Columns
library(dplyr)

Birds <- Birds %>%
  mutate(
    Region   = ifelse(startsWith(eventID, "N"), "North", "South"),
    InBounds = !grepl("OoB", eventID)     # OoB = out of bounds
  )

#In-bound Species Richness
bird_richness <- Birds %>%
  filter(InBounds, !is.na(scientificName)) %>%
  group_by(eventID, Region) %>%
  summarise(
    SpeciesRichness = n_distinct(scientificName),
    .groups = "drop"
  )

#Abundance
bird_abundance <- Birds %>%
  filter(InBounds) %>%
  group_by(eventID, Region) %>%
  summarise(
    Abundance = sum(individualCount, na.rm = TRUE),
    .groups = "drop"
  )

#Table
bird_summary <- bird_richness %>%
  left_join(bird_abundance, by = c("eventID", "Region"))

#Plot abundance
library(ggplot2)

ggplot(bird_summary, aes(x = eventID, y = Abundance, fill = Region)) +
  geom_col() +
  scale_fill_manual(
    values = c("North" = "#9B59B6",  # purple
               "South" = "#27AE60")  # green
  ) +
  labs(
    title = "Bird Abundance by Site (In-Bounds Only)",
    x = "Site (eventID)",
    y = "Abundance (Number of Individuals)",
    fill = "Region"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bird_abundance_region <- Birds %>%
  filter(InBounds) %>%
  group_by(Region) %>%
  summarise(
    TotalAbundance = sum(individualCount, na.rm = TRUE),
    .groups = "drop"
  )

#Abundance Per Slope
bird_abundance_region <- Birds %>%
  filter(InBounds) %>%
  group_by(Region) %>%
  summarise(
    TotalAbundance = sum(individualCount, na.rm = TRUE),
    .groups = "drop"
  )


ggplot(bird_abundance_region, aes(x = Region, y = TotalAbundance, fill = Region)) +
  geom_col(width = 0.6) +
  scale_fill_manual(
    values = c("North" = "#9370DB", "South" = "#2E8B57")
  ) +
  labs(
    title = "Total Bird Abundance: North vs South",
    x = "Region",
    y = "Total Abundance"
  ) +
  theme_minimal(base_size = 14)

#Species Richness
bird_richness <- Birds %>%
  mutate(
    Region   = ifelse(startsWith(eventID, "N"), "North", "South"),
    InBounds = !grepl("OoB", eventID)
  ) %>%
  filter(InBounds, !is.na(scientificName)) %>%
  group_by(eventID, Region) %>%
  summarise(
    SpeciesRichness = n_distinct(scientificName),
    .groups = "drop"
  )

#Plot Species richness (in-bounds only)
library(ggplot2)

ggplot(bird_richness, aes(x = eventID, y = SpeciesRichness, fill = Region)) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "North" = "#9B59B6",  
      "South" = "#27AE60"   
    )
  ) +
  labs(
    title = "Bird Species Richness by Site (In-Bounds Only)",
    x = "Site (eventID)",
    y = "Species Richness",
    fill = "Region"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#By slope
bird_region_richness <- Birds %>%
  mutate(
    Region   = ifelse(startsWith(eventID, "N"), "North", "South"),
    InBounds = !grepl("OoB", eventID)
  ) %>%
  filter(InBounds, !is.na(scientificName)) %>%
  group_by(Region) %>%
  summarise(
    SpeciesRichness = n_distinct(scientificName),
    .groups = "drop"
  )
library(ggplot2)

ggplot(bird_region_richness, aes(x = Region, y = SpeciesRichness, fill = Region)) +
  geom_col(width = 0.6) +
  scale_fill_manual(
    values = c(
      "North" = "#9370DB",  # purple
      "South" = "#2E8B57"  # green
    )
  ) +
  labs(
    title = "Bird Species Richness: North vs South",
    x = "Region",
    y = "Species Richness"
  ) +
  theme_minimal(base_size = 14)

#Richness for both in and out of bounds
Birds <- Birds %>%
  mutate(
    Region   = ifelse(startsWith(eventID, "N"), "North", "South"),
    Bounds   = ifelse(grepl("OoB", eventID), "OutOfBounds", "InBounds")
  )
bird_bounds_region_richness <- Birds %>%
  filter(!is.na(scientificName)) %>% 
  group_by(Region, Bounds) %>%
  summarise(
    SpeciesRichness = n_distinct(scientificName),
    .groups = "drop"
  )
library(ggplot2)
library(ggh4x)

# complementary fill colours
bounds_cols <- c(
  "InBounds"    = "#2AA198",  # teal
  "OutOfBounds" = "#DFA000"   # golden amber
)

ggplot(bird_bounds_region_richness,
       aes(x = Bounds, y = SpeciesRichness, fill = Bounds)) +
  
  geom_col(width = 0.7) +
  
  facet_wrap2(
    ~ Region,
    nrow = 1,
    strip = strip_themed(
      background_x = list(
        element_rect(fill = "#E6DAF5", colour = "#E6DAF5"),  # North (purple tint)
        element_rect(fill = "#C8F7C5", colour = "#C8F7C5")   # South (green tint)
      ),
      text_x = list(
        element_text(face = "bold"),
        element_text(face = "bold")
      )
    )
  ) +
  
  scale_fill_manual(values = bounds_cols) +
  
  labs(
    title = "Bird Species Richness by Region and Bounds Category",
    x = "",
    y = "Species Richness",
    fill = "Site Category"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.text = element_text(size = 12)
  )
library(dplyr)
library(ggplot2)

# Abundance per slope
bird_abundance_region <- Birds %>%
  filter(InBounds) %>%
  group_by(Region) %>%
  summarise(
    TotalAbundance = sum(individualCount, na.rm = TRUE),
    .groups = "drop"
  )

# Richness per slope
bird_region_richness <- Birds %>%
  filter(InBounds, !is.na(scientificName)) %>%
  group_by(Region) %>%
  summarise(
    SpeciesRichness = n_distinct(scientificName),
    .groups = "drop"
  )
# Colours
region_cols <- c("North" = "#9370DB", "South" = "#2E8B57")

# Plot: richness per region
p_bird_rich_region <- ggplot(bird_region_richness,
                             aes(x = Region, y = SpeciesRichness, fill = Region)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = region_cols) +
  labs(
    title = "Bird species richness: North vs South",
    x = "Region",
    y = "Species richness"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Plot: abundance per region
p_bird_abund_region <- ggplot(bird_abundance_region,
                              aes(x = Region, y = TotalAbundance, fill = Region)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = region_cols) +
  labs(
    title = "Total bird abundance: North vs South",
    x = "Region",
    y = "Total abundance"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
library(patchwork)

p_bird_rich_region | p_bird_abund_region

library(ggplot2)

ggplot(bat_richness, aes(x = eventID, y = SpeciesRichness, fill = Region)) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "North" = "#9B59B6",  # purple
      "South" = "#27AE60"   # green
    )
  ) +
  labs(
    title = "Bat Species Richness by Site",
    x = "Site (eventID)",
    y = "Species Richness",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Richness per Region
bat_region_richness <- BatData %>%
  filter(!is.na(scientificName)) %>% 
  group_by(Region) %>%
  summarise(
    SpeciesRichness = n_distinct(scientificName),
    .groups = "drop"
  )

ggplot(bat_region_richness, aes(x = Region, y = SpeciesRichness, fill = Region)) +
  geom_col(width = 0.6) +
  scale_fill_manual(
    values = c(
      "North" = "#9B59B6",  # purple
      "South" = "#27AE60"   # green
    )
  ) +
  labs(
    title = "Bat Species Richness: North vs South",
    x = "Region",
    y = "Species Richness"
  ) +
  theme_minimal(base_size = 14)

#Activity
bat_activity_region <- BatData %>%
  group_by(Region) %>%
  summarise(
    Activity = n(),      # number of detections
    .groups = "drop"
  )

ggplot(bat_activity_region, aes(x = Region, y = Activity, fill = Region)) +
  geom_col(width = 0.6) +
  scale_fill_manual(
    values = c(
      "North" = "#9B59B6",  # purple
      "South" = "#27AE60"   # green
    )
  ) +
  labs(
    title = "Bat Presence: North vs South",
    x = "Region",
    y = "Activity (Number of Acoustic Detections)"
  ) +
  theme_minimal(base_size = 14)
library(patchwork)

region_cols <- c("North" = "#9370DB", "South" = "#2E8B57")

#Plot Bat species richness per region
p_bat_rich <- ggplot(bat_region_richness,
                     aes(x = Region, y = SpeciesRichness, fill = Region)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = region_cols) +
  labs(
    title = "Bat Species Richness: North vs South",
    x = "Region",
    y = "Species Richness"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


#Plot Bat activity (presence) per region
p_bat_activity <- ggplot(bat_activity_region,
                         aes(x = Region, y = Activity, fill = Region)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = region_cols) +
  labs(
    title = "Bat Activity: North vs South",
    x = "Region",
    y = "Activity (Acoustic Detections)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
p_bat_rich | p_bat_activity

######
#Birds
######

#Read Bird Data
Birds <- read.csv("Bird.csv", header = TRUE)

#Create Regions and Inbound Columns
library(dplyr)

Birds <- Birds %>%
  mutate(
    Region   = ifelse(startsWith(eventID, "N"), "North", "South"),
    InBounds = !grepl("OoB", eventID)     # OoB = out of bounds
  )

#In-bound Species Richness
bird_richness <- Birds %>%
  filter(InBounds, !is.na(scientificName)) %>%
  group_by(eventID, Region) %>%
  summarise(
    SpeciesRichness = n_distinct(scientificName),
    .groups = "drop"
  )

#Abundance
bird_abundance <- Birds %>%
  filter(InBounds) %>%
  group_by(eventID, Region) %>%
  summarise(
    Abundance = sum(individualCount, na.rm = TRUE),
    .groups = "drop"
  )

#Table
bird_summary <- bird_richness %>%
  left_join(bird_abundance, by = c("eventID", "Region"))

#Plot abundance
library(ggplot2)

ggplot(bird_summary, aes(x = eventID, y = Abundance, fill = Region)) +
  geom_col() +
  scale_fill_manual(
    values = c("North" = "#9B59B6",  # purple
               "South" = "#27AE60")  # green
  ) +
  labs(
    title = "Bird Abundance by Site (In-Bounds Only)",
    x = "Site (eventID)",
    y = "Abundance (Number of Individuals)",
    fill = "Region"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bird_abundance_region <- Birds %>%
  filter(InBounds) %>%
  group_by(Region) %>%
  summarise(
    TotalAbundance = sum(individualCount, na.rm = TRUE),
    .groups = "drop"
  )

#Abundance Per Slope
bird_abundance_region <- Birds %>%
  filter(InBounds) %>%
  group_by(Region) %>%
  summarise(
    TotalAbundance = sum(individualCount, na.rm = TRUE),
    .groups = "drop"
  )


ggplot(bird_abundance_region, aes(x = Region, y = TotalAbundance, fill = Region)) +
  geom_col(width = 0.6) +
  scale_fill_manual(
    values = c("North" = "#9370DB", "South" = "#2E8B57")
  ) +
  labs(
    title = "Total Bird Abundance: North vs South",
    x = "Region",
    y = "Total Abundance"
  ) +
  theme_minimal(base_size = 14)

#Species Richness
bird_richness <- Birds %>%
  mutate(
    Region   = ifelse(startsWith(eventID, "N"), "North", "South"),
    InBounds = !grepl("OoB", eventID)
  ) %>%
  filter(InBounds, !is.na(scientificName)) %>%
  group_by(eventID, Region) %>%
  summarise(
    SpeciesRichness = n_distinct(scientificName),
    .groups = "drop"
  )

#Plot Species richness (in-bounds only)
library(ggplot2)

ggplot(bird_richness, aes(x = eventID, y = SpeciesRichness, fill = Region)) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "North" = "#9B59B6",  
      "South" = "#27AE60"   
    )
  ) +
  labs(
    title = "Bird Species Richness by Site (In-Bounds Only)",
    x = "Site (eventID)",
    y = "Species Richness",
    fill = "Region"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#By slope
bird_region_richness <- Birds %>%
  mutate(
    Region   = ifelse(startsWith(eventID, "N"), "North", "South"),
    InBounds = !grepl("OoB", eventID)
  ) %>%
  filter(InBounds, !is.na(scientificName)) %>%
  group_by(Region) %>%
  summarise(
    SpeciesRichness = n_distinct(scientificName),
    .groups = "drop"
  )
library(ggplot2)

ggplot(bird_region_richness, aes(x = Region, y = SpeciesRichness, fill = Region)) +
  geom_col(width = 0.6) +
  scale_fill_manual(
    values = c(
      "North" = "#9370DB",  # purple
      "South" = "#2E8B57"  # green
    )
  ) +
  labs(
    title = "Bird Species Richness: North vs South",
    x = "Region",
    y = "Species Richness"
  ) +
  theme_minimal(base_size = 14)

#Richness for both in and out of bounds
Birds <- Birds %>%
  mutate(
    Region   = ifelse(startsWith(eventID, "N"), "North", "South"),
    Bounds   = ifelse(grepl("OoB", eventID), "OutOfBounds", "InBounds")
  )
bird_bounds_region_richness <- Birds %>%
  filter(!is.na(scientificName)) %>% 
  group_by(Region, Bounds) %>%
  summarise(
    SpeciesRichness = n_distinct(scientificName),
    .groups = "drop"
  )
library(ggplot2)
library(ggh4x)

# complementary fill colours
bounds_cols <- c(
  "InBounds"    = "#2AA198",  # teal
  "OutOfBounds" = "#DFA000"   # golden amber
)

ggplot(bird_bounds_region_richness,
       aes(x = Bounds, y = SpeciesRichness, fill = Bounds)) +
  
  geom_col(width = 0.7) +
  
  facet_wrap2(
    ~ Region,
    nrow = 1,
    strip = strip_themed(
      background_x = list(
        element_rect(fill = "#E6DAF5", colour = "#E6DAF5"),  # North (purple tint)
        element_rect(fill = "#C8F7C5", colour = "#C8F7C5")   # South (green tint)
      ),
      text_x = list(
        element_text(face = "bold"),
        element_text(face = "bold")
      )
    )
  ) +
  
  scale_fill_manual(values = bounds_cols) +
  
  labs(
    title = "Bird Species Richness by Region and Bounds Category",
    x = "",
    y = "Species Richness",
    fill = "Site Category"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.text = element_text(size = 12)
  )
library(dplyr)
library(ggplot2)

# Abundance per slope
bird_abundance_region <- Birds %>%
  filter(InBounds) %>%
  group_by(Region) %>%
  summarise(
    TotalAbundance = sum(individualCount, na.rm = TRUE),
    .groups = "drop"
  )

# Richness per slope
bird_region_richness <- Birds %>%
  filter(InBounds, !is.na(scientificName)) %>%
  group_by(Region) %>%
  summarise(
    SpeciesRichness = n_distinct(scientificName),
    .groups = "drop"
  )
# Colours
region_cols <- c("North" = "#9370DB", "South" = "#2E8B57")

# Plot: richness per region
p_bird_rich_region <- ggplot(bird_region_richness,
                             aes(x = Region, y = SpeciesRichness, fill = Region)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = region_cols) +
  labs(
    title = "Bird species richness: North vs South",
    x = "Region",
    y = "Species richness"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Plot: abundance per region
p_bird_abund_region <- ggplot(bird_abundance_region,
                              aes(x = Region, y = TotalAbundance, fill = Region)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = region_cols) +
  labs(
    title = "Total bird abundance: North vs South",
    x = "Region",
    y = "Total abundance"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
library(patchwork)

p_bird_rich_region | p_bird_abund_region

########
#Mammals
########

#Read the data
Mammals <- read.csv("Mammals.csv", header = TRUE)

#Keep only human observations + add Region
library(dplyr)

Mammals_human <- Mammals %>%
  filter(basisOfRecord == "Humanobservation") %>%
  mutate(
    Region = ifelse(startsWith(eventID, "N"), "North", "South")
  )

#Species richness per slope
mammal_region_richness <- Mammals_human %>%
  filter(!is.na(scientificName)) %>%
  group_by(Region) %>%
  summarise(
    SpeciesRichness = n_distinct(scientificName),
    .groups = "drop"
  )

#Plot
library(ggplot2)

ggplot(mammal_region_richness, aes(x = Region, y = SpeciesRichness, fill = Region)) +
  geom_col(width = 0.6) +
  scale_fill_manual(
    values = c(
      "North" = "#9B59B6",  # purple
      "South" = "#27AE60"   # green
    )
  ) +
  labs(
    title = "Mammal Species Richness (Human Observations Only)",
    x = "Region",
    y = "Species Richness"
  ) +
  theme_minimal(base_size = 14)

