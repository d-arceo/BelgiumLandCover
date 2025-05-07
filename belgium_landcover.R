# Set the working directory
setwd("C:/Users/Daniel/Documents/Land Cover Map")

# ---------------------------
# 1. Load Packages
# ---------------------------

# List of packages we'll need
libs <- c(
  "terra",      # for raster data
  "giscoR",     # to get country borders
  "sf",         # spatial data stuff
  "tidyverse",  # data wrangling
  "ggtern",     # for converting RGB to hex
  "elevatr",    # to download elevation data
  "png",        # to read PNGs
  "rayshader",  # for the 3D magic
  "magick"      # for combining/overlaying images
)

# Install any missing packages
installed_libraries <- libs %in% rownames(installed.packages())
if(any(installed_libraries == FALSE)){
  install.packages(libs[!installed_libraries])
}

# Load everything
invisible(lapply(libs, library, character.only = TRUE))

# ---------------------------
# 2. Get Country Borders
# ---------------------------

# Pull in Belgium's borders from GISCO
country_sf <- giscoR::gisco_get_countries(country = "BE", resolution = "1")

# Quick plot to check
plot(sf::st_geometry(country_sf))

# Save it as a PNG too
png("bih-borders.png")
plot(sf::st_geometry(country_sf))
dev.off()

# ---------------------------
# 3. Download ESRI Land Cover Tiles
# ---------------------------

# Grab land cover data for Belgium (2 tiles)
urls <- c(
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2024/31U_20240101-20241231.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2024/32U_20240101-20241231.tif"
)

# Download each tile
for(url in urls){
  download.file(url = url, destfile = basename(url), mode = "wb")
}

# ---------------------------
# 4. Crop & Reproject to Belgium
# ---------------------------

# Get the files we just downloaded
raster_files <- list.files(path = getwd(), pattern = "20241231.tif$", full.names = TRUE)
crs <- "EPSG:4326"  # WGS84

# Loop through each raster
for(raster in raster_files){
  rasters <- terra::rast(raster)

  # Reproject the country shape to match the raster
  country <- country_sf |> sf::st_transform(crs = terra::crs(rasters))

  # Crop and mask to Belgium, downscale a bit, and reproject to WGS84
  land_cover <- terra::crop(rasters, terra::vect(country), snap = "in", mask = TRUE) |>
    terra::aggregate(fact = 5, fun = "modal") |>  # reduce resolution for speed
    terra::project(crs)

  # Save it
  terra::writeRaster(land_cover, paste0(raster, "_belgium", ".tif"))
}

# ---------------------------
# 5. Combine into One Raster (VRT)
# ---------------------------

# Grab all the processed belgium files
r_list <- list.files(path = getwd(), pattern = "_belgium", full.names = TRUE)

# Make a virtual raster to treat them as one
land_cover_vrt <- terra::vrt(r_list, "belgium_land_cover_vrt.vrt", overwrite = TRUE)

# ---------------------------
# 6. Get the Color Table
# ---------------------------

# Grab one raster to extract the color codes
ras <- terra::rast(raster_files[[1]])

# Extract the RGB table
raster_color_table <- do.call(data.frame, terra::coltab(ras))

# Convert to hex
hex_code <- ggtern::rgb2hex(
  r = raster_color_table[,2],
  g = raster_color_table[,3],
  b = raster_color_table[,4]
)

# ---------------------------
# 7. Color the Raster
# ---------------------------

# Pick a subset of colors for display
cols <- hex_code[c(2:3, 5:6, 8:12)]
from <- c(1:2, 4:5, 7:11)
to <- t(col2rgb(cols))

# Remove any NA values just in case
land_cover_vrt <- na.omit(land_cover_vrt)

# Assign RGB colors to the raster values
land_cover_belgium <- terra::subst(land_cover_vrt, from = from, to = to, names = cols)

# Quick look
terra::plotRGB(land_cover_belgium)

# ---------------------------
# 8. Get Elevation Data
# ---------------------------

# Download DEM for Belgium
elev <- elevatr::get_elev_raster(locations = country_sf, z = 9, clip = "locations")

# Define a Lambert projection (nice for Europe maps)
crs_lambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

# Match the elevation raster's resolution and reproject
land_cover_belgium_resampled <- terra::resample(land_cover_belgium, terra::rast(elev), method = "near") |>
  terra::project(crs_lambert)

# Preview it
terra::plotRGB(land_cover_belgium_resampled)

# Save as PNG
img_file <- "land_cover_belgium.png"
terra::writeRaster(land_cover_belgium_resampled, img_file, overwrite = TRUE, NAflag = 255)
img <- png::readPNG(img_file)

# ---------------------------
# 9. 3D Render with Rayshader
# ---------------------------

# Reproject DEM
elev_lambert <- elev |> terra::rast() |> terra::project(crs_lambert)

# Convert to matrix for rayshader
elmat <- rayshader::raster_to_matrix(elev_lambert)
h <- nrow(elev_lambert)
w <- ncol(elev_lambert)

# Render the 3D scene
elmat |>
  rayshader::height_shade(texture = colorRampPalette(cols[9])(256)) |>
  rayshader::add_overlay(img, alphalayer = 1) |>
  rayshader::plot_3d(
    elmat,
    zscale = 12,
    solid = FALSE,
    shadow = TRUE,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(w / 5, h / 5),
    zoom = .5,
    phi = 85,
    theta = 0
  )

# Adjust camera slightly
rayshader::render_camera(zoom = .58)

# ---------------------------
# 10. Render High-Quality Image
# ---------------------------

# Download HDRI background for lighting
u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/air_museum_playground_4k.hdr"
hdri_file <- basename(u)
download.file(url = u, destfile = hdri_file, mode = "wb")

# Save a high-quality 3D render
filename <- "3d_land_cover_belgium-dark.png"
rayshader::render_highquality(
  filename = filename,
  preview = TRUE,
  light = FALSE,
  environment_light = hdri_file,
  intensity_env = 1,
  rotate_env = 90,
  interactive = FALSE,
  parallel = TRUE,
  width = w * 1.5,
  height = h * 1.5
)

# ---------------------------
# 11. Add a Legend to the Final Image
# ---------------------------

# Color references
c(
  "#419bdf", "#397d49", "#7a87c6", 
  "#e49635", "#c4281b", "#a59b8f", 
  "#a8ebff", "#616161", "#e3e2c3"
)

# Build a legend image
legend_name <- "land_cover_legend.png"
png(legend_name)
par(family = "mono")
plot(NULL, xaxt = "n", yaxt = "n", bty = "n", ylab = "", xlab = "",
     xlim = 0:1, ylim = 0:1, xaxs = "i", yaxs = "i")
legend("center",
       legend = c("Water", "Trees", "Crops", "Built Area", "Rangeland"),
       pch = 15, cex = 2, pt.cex = 1, bty = "n",
       col = c(cols[c(1:2, 4:5, 9)]),
       fill = c(cols[c(1:2, 4:5, 9)]),
       border = "grey20")
dev.off()

# Load the rendered image and the legend
lc_img <- magick::image_read(filename)
my_legend <- magick::image_read(legend_name)

# Resize the legend to fit
my_legend_scaled <- magick::image_scale(magick::image_background(my_legend, "none"), 2500)

# Combine legend and main image
p <- magick::image_composite(
  magick::image_scale(lc_img, "x7000"),
  my_legend_scaled,
  gravity = "southwest",
  offset = "+100+0"
)

# Save the final image
magick::image_write(p, "3d_belgium_land_cover_final.png")
