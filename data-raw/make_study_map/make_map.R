library(sf)
library(tidyverse)
library(maps)
library(ggrepel)

# Get Iowa County Map
iowa <- 
  map_data("county") %>%
  filter(region == "iowa")
# Input centroid info
centroids <- 
  read_csv("data-raw/make_study_map/site_centroids_2019.csv") %>%
  mutate(group = 1)

#lat lon projection
proj4S <- "+proj=utm +zone=15 +north +ellps=WGS84 +datum=WGS84 +units=m +nodefs"

# translate coords
latlon <- 
  centroids %>%
  select(easting, northing)%>%
  proj4::project(., proj4S, inverse = TRUE)%>%
  as.data.frame()

# sites with the same seed mix
samesmIDS <- c("WOR", "NYK", "MRS", "SER", "ARM", "RHO")

# add together
centroids_lat_lon <- 
  bind_cols(centroids, latlon) %>%
  filter(siteID != "WAT") %>%
  mutate(yearSampled = replace(yearSampled, yearSampled == "both", "2018_2019"),
         samesm      = if_else(siteID %in% samesmIDS, "yes", "no"))

same_sub <- centroids_lat_lon %>%
  filter(samesm == "yes" )

iowa %>%
  ggplot(aes(x = long, y = lat, group = group))+
  geom_polygon(color = "black", fill = "white")+
  geom_label_repel(data = centroids_lat_lon, aes(x = x, y = y, label = mapID, fill = samesm),
             fontface = "bold", label.size = 0.5, box.padding = 0, force = 0.01) +
  coord_fixed(1.4)+
  labs(x ="", y = "", fill = "Year Sampled")+
  guides(fill = FALSE)+
  scale_fill_manual(values = c("white", "lightgrey"), labels=c("2018 and 2019", "2019 only"))+
  theme(panel.background = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        legend.direction = "horizontal",
        legend.position  = "bottom",
        legend.background = element_rect(color = "black"),
        legend.text      = element_text(size = rel(1.1)),
        legend.title     = element_text(size = rel(1.3)))

