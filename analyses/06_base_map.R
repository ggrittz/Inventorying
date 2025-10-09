  
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
# file_name1 <- "sc.shp"
# sc <- terra::vect(file.path(file_path, file_name1))
# sc <- terra::project(sc, "epsg:4326")
# Load ecoregions map
file_name2 <- "Ecoregions2017.shp"
er <- terra::vect(file.path(file_path, file_name2))
er <- terra::project(er, "epsg:4326")
# Load inventory plots
file_name4 <- "449UAs_Floresta.shp"
uas <- terra::vect(file.path(file_path, file_name4))
uas <- terra::project(uas, "epsg:4326")
# Load Brazil map
br <- rnaturalearth::ne_states(country = "brazil",
                               returnclass = "sv")
# Get Santa Catarina
sc <- br[br$iso_3166_2 %in% "BR-SC", ]
sc <- terra::project(sc, terra::crs(sc))
# Remove Santa Catarina since this is a horrible map for islands
br_no_sc <- br[!br$iso_3166_2 %in% "BR-SC", ]
br_no_sc <- terra::project(br_no_sc, terra::crs(er))
# Load Argentina map due to boundaries
ar <- rnaturalearth::ne_states(country = "argentina",
                               returnclass = "sv")
ar <- terra::project(ar, terra::crs(ar))
# Merge BR and AR
br_ar <- rbind(br_no_sc, ar)

# Crop ecoregions into SC
er_sc <- terra::crop(er, sc)
er_sc <- terra::mask(er_sc, sc)

# A bit bigger bbox for SC
bbox_sc <- terra::ext(er_sc) * 1.1

# Background map
br_ar_bbox <- terra::crop(br_ar, (bbox_sc * 1.1))
# br_bbox_dis <- terra::aggregate(br_bbox, dissolve = TRUE, cores = 10)


##### Plotting #####
# svg("figures/map.svg", width = 10, height = 12)
# As pdf
pdf("figures/map.pdf", width = 10, height = 12)
# Base map
terra::plot(br_ar_bbox, col = "lightgrey", border = "black",
            xlab = "Longitude", ylab = "Latitude\n", cex.lab = 1.5,
            pax = list(cex.axis = 1.4))
# Ecoregions
colors <- er_sc$COLOR
colors[3] <- "#F0EAD6"

terra::plot(er_sc, col = colors[as.factor(er_sc$ECO_NAME)], 
            lwd = 2, add = TRUE)
# Inventory plots
terra::plot(uas, col = "black", cex = 0.6, add = TRUE)
# Scale
terra::sbar(d = 100, xy = "bottomright", type = "bar", div = 3,
            lonlat = TRUE, below = "km")
# Legend
legend(x = -52.5, y = -28.5, legend = er_sc$ECO_NAME,
       # fill = er_sc$COLOR, 
       pch = 15,
       col = colors,
       pt.cex = 3,
       cex = 1,
       ncol = 1,
       y.intersp = 1.5,
       x.intersp = 1.5,
       bty = "n")

# Add inset
par(fig = c(0.03, 0.36, 0.22, 0.45),
    new = TRUE, mar = c(0, 0, 0, 0))
terra::plot(br, col = "lightgrey", border = "black", 
            xlim = c(-75, -30), ylim = c(-35, 5))
terra::plot(br[br$iso_3166_2 %in% "BR-SC", ], 
            col = "red", border = "black", add = TRUE)

dev.off()


