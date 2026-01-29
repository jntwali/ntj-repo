# This project creates a map of Musanze District (Rwanda), highlighting Cyuve sector and an inset map of Rwanda showing where Musanze is located. 
# verything is done with base R graphics and the sf package.
#############################################################################
# set working directory
setwd("~/Maps")
# Install once (if needed)
# install.packages(c("sf", "dplyr", "ggplot2", "ggspatial", "prettymapr"))
library(sf)

# 1. Read and subset data
###############################
# Load map of Rwanda with districts and sectors boundaries
rwanda_sectors <- st_read("~/R/Data files/GIS/Rwanda pol map/Sector.shp")

# Load hydrological map of Rwanda (for lakes and rivers)
rwanda_lakes <- st_read("~/R/Data files/GIS/rwanda_rivers/Water_Supply_Control-Rivers.shp")

# Select Musanze district (check spelling with unique(rwanda_sectors$District))
musanze <- rwanda_sectors[ rwanda_sectors$District == "Musanze",]

# Map of Cyuve sector inside Musanze (check the spelling with unique(musanze$Name))

cyuve <- musanze[musanze$Name == "Cyuve", ]

# create district boundaries polygon
# Create a dummy field so aggregate merges them into one feature
musanze$dummy <- 1

# Dissolve sectors into a single Musanze polygon

musanze_district <- aggregate(
  x   = musanze["dummy"],
  by  = list(musanze$dummy),
  FUN = function(x) 1
)

# Country outline without districts and sectors boundaries

rwanda_sectors$dummy <- 1

# Dissolve districts and sectors into a single Rwanda polygon
rwanda_outline <- aggregate(
  x   = rwanda_sectors["dummy"],
  by  = list(rwanda_sectors$dummy),
  FUN = function(x) 1)


# 2. Reproject to WGS84
###############################
musanze_4326          <- st_transform(musanze, 4326)
cyuve_4326            <- st_transform(cyuve,   4326)
musanze_district_4326 <- st_transform(musanze_district, 4326)
rwanda_outline_4326   <- st_transform(rwanda_outline, 4326)
lake_kivu_4326        <- st_transform(rwanda_lakes, 4326)

# Overall extent for main map (Musanze)
bb_musanze <- st_bbox(musanze_4326)

# Overall extent for inset (Rwanda)
bb_rwanda  <- st_bbox(rwanda_outline_4326)

# Optional: centroid of Musanze for a point marker in the inset
musanze_centroid <- st_centroid(musanze_district_4326)

# Clip farms to Musanze if desired
farms_musanze <- st_intersection(farms_sf, musanze_4326)

  
# 3. Plot with basemap
#######################################################

##1 2. Open device and split plotting region -----------------------------

# High-resolution PNG
png("Cyuve_in_musanze_map.png",
    width  = 2000, height = 1500, res = 300)

# Layout: 1 big panel; we will draw the inset manually in a sub-viewport
par(mar = c(4, 4, 4, 4), oma = c(0,.3,0,0))  # margins

## 2. MAIN MAP: Musanze + Cyuve + farms + dryer ------------------------

# Empty plot with Musanze extent
plot(st_geometry(musanze_4326),
     col = "grey95", border = "#444444",
     xlim = c(bb_musanze["xmin"], bb_musanze["xmax"]),
     ylim = c(bb_musanze["ymin"], bb_musanze["ymax"]),
     asp  = 1,axes = TRUE,tcl = -.3,las = 1,
     xlab = "", ylab = "")
mtext("Musanze Distric", side = 3, line = 2, font = 2)
mtext("Cyuve and farms location", cex = .9, line = 1, side = 3)
mtext("Latitude", side = 2, line = 3.5)
mtext("Longitude", side = 1, line = 2.5)

# Highlight Cyuve
plot(st_geometry(cyuve_4326),
     add = TRUE,
     col = adjustcolor("#F46A25", alpha.f = 0.5),
     border = "#56752B", lwd = 2)


# Legend for main map
legend("bottomleft",
       legend = c("Cyuve\n sector"),
       col    = "#56752B",
       lty    = 1, 
       lwd    = 2,
       bty    = "n")
cartography::north("topleft") #north arrow

## 4. NORTH ARROW (simple) -------------------------------------------
# Place it in user coordinates (adjust as you like)
usr <- par("usr")  # c(xmin, xmax, ymin, ymax)

nx <- usr[1] + 0.05 * (usr[2] - usr[1])  # 5% from left
ny <- usr[4] - 0.10 * (usr[4] - usr[3])  # 10% from top

arrows(x0 = nx, y0 = ny,
       x1 = nx, y1 = ny + 0.05 * (usr[4] - usr[3]),
       length = 0.08, lwd = 2)
text(nx, ny + 0.07 * (usr[4] - usr[3]), labels = "N", cex = 0.8, font = 2)

## 5. SCALE BAR (approximate, in degrees) ----------------------------
  # Choose scale bar length in degrees (roughly 10 km near -1.5° lat)
  # 1 degree of longitude ~ 111 km * cos(latitude)
  lat_mid <- (usr[3] + usr[4]) / 2
km_per_deg_lon <- 111 * cos(lat_mid * pi / 180)
len_km <- 10   # wanted scale length in km
len_deg <- len_km / km_per_deg_lon

# Position along bottom
sx0 <- usr[1] + 0.73 * (usr[2] - usr[1])
sx1 <- sx0 + len_deg
sy0 <- usr[3] + 0.04 * (usr[4] - usr[3])

# Draw bar
segments(sx0, sy0, sx1, sy0, lwd = 2)
segments(sx0, sy0 - 0.002, sx0, sy0 + 0.002, lwd = 2)
segments(sx1, sy0 - 0.002, sx1, sy0 + 0.002, lwd = 2)

# Label in km
text((sx0 + sx1) / 2, sy0 + 0.03 * (usr[4] - usr[3]),
     labels = paste0(len_km, " km"),
     cex = 0.8)

## 6. INSET: Rwanda + Musanze (+ Lake Kivu) ----------------------------

# Define inset position in figure coordinates (0–1)
# (x1,y1) bottom-left, (x2,y2) top-right
x1 <- 0.7; x2 <- 0.87
y1 <- 0.63; y2 <- 0.83
tiles <- maptiles::get_tiles(x = bb_rwanda, provider = "OpenStreetMap")
# Set up inset plotting region
par(xpd = T)  # allow drawing outside main plot
par(new = TRUE)
par(fig = c(x1, x2, y1, y2), mar = c(0, 0, 0, 0))
terra::plot(tiles)
# Plot Rwanda outline
plot(st_geometry(rwanda_outline_4326),
     #col = "grey85", 
     border = "grey40",
     xlim = c(bb_rwanda["xmin"], bb_rwanda["xmax"]),
     ylim = c(bb_rwanda["ymin"], bb_rwanda["ymax"]),
     asp  = 1,add = T,
     axes = FALSE, xlab = "", ylab = "", main = "")

# Optional: Lake Kivu
if (exists("lake_kivu_4326")) {
  plot(st_geometry(lake_kivu_4326),
       add = TRUE,
       col = "#0099d7", border = "#408ccc")
}

# Highlight Musanze
plot(st_geometry(musanze_district_4326),
     add = TRUE,
     col = adjustcolor("#f20000", alpha.f = 0.7),
     border = "black", lwd = 1.2)

# Mark Musanze centroid
plot(st_geometry(musanze_centroid),
     add = TRUE,
     col = "black", bg = "#ffe619",
     pch = 21, cex = 0.8)

# Draw box around the inset (in figure coordinates)
par(fig = c(0, 1, 0, 1), new = TRUE, mar = c(0, 0, 0, 0))
plot.new()
terra::
rect(xleft   = x1+.02, ybottom = y1,
     xright  = x2+.03, ytop    = y2+.02,
     border  = "black", lwd = 1, lty = 3)

dev.off()
