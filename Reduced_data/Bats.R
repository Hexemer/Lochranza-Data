#####
#Bats
#####
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

#Plot 1: Bat species richness per region
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


#Plot 2: Bat activity per region
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

