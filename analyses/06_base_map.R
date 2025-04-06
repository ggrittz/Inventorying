
# Plot map ----------------------------------------------------------------

# Downloading ecorregions Dinerstein et al. (2017)
file_path <- here::here("data", "raw-data", "maps")
if(!dir.exists(file_path))
  dir.create(file_path)
file_name <- "ecoregions2017.zip"
url <- "https://storage.googleapis.com/teow2016/Ecoregions2017.zip"
download.file(url, file.path(file_path, file_name))
# Extracting, reading, and preparing
unzip(zipfile = file.path(file_path, file_name),
      exdir = file_path)

# Load Santa Catarina map
file_name1 <- "sc.shp"
sc <- terra::vect(file.path(file_path, file_name1))
sc <- terra::project(sc, "epsg:4326")
# Load ecoregions map
file_name2 <- "Ecoregions2017.shp"
er <- terra::vect(file.path(file_path, file_name2))
er <- terra::project(er, "epsg:4326")
# Load inventory plots
file_name4 <- "449UAs_Floresta.shp"
uas <- terra::vect(file.path(file_path, file_name4))
uas <- terra::project(uas, "epsg:4326")

# Crop ecoregions into SC
er_sc <- terra::crop(er, sc)
er_sc <- terra::mask(er_sc, sc)

# A bit bigger bbox for SC
bbox_sc <- terra::ext(er_sc) * 1.1

# Background map
er_bbox <- terra::crop(er, (bbox_sc * 1.1))
er_bbox_dis <- terra::aggregate(er_bbox, dissolve = TRUE, cores = 10)

##### Plotting #####
# Base map
terra::plot(er_bbox_dis, col = "lightgrey", border = "black",
            xlab = "longitude", ylab = "latitude\n", cex.lab = 1.1)
# Ecoregions
colors <- er_sc$COLOR
colors[3] <- "#F0EAD6"

terra::plot(er_sc, col = colors[as.factor(er_sc$ECO_NAME)], 
            lwd = 2, add = TRUE)
# Inventory plots
terra::plot(uas, col = "black", cex = 0.5, add = TRUE)
# Scale
terra::sbar(d = 100, xy = "bottomright", type = "bar", div = 3,
            lonlat = TRUE, below = "km")
# Legend
legend(x = -54.5, y = -27.7, legend = er_sc$ECO_NAME,
       # fill = er_sc$COLOR, 
       pch = 15,
       col = colors,
       pt.cex = 3,
       cex = 1,
       ncol = 1,
       y.intersp = 0.2,
       x.intersp = 0.3,
       bty = "n")


