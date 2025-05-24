# generate_figures_2_to_4.R
# Purpose: Export three standalone maps for manuscript Figures 2–4
#           • figure2.png  – CDL 2018 (major crops)
#           • figure3.png  – Winter cover‑crop layer
#           • figure4.png  – CDL 2019 (major crops)

# ------------------------------------------------------------------
# 0. Packages --------------------------------------------------------
# ------------------------------------------------------------------

req_pkgs <- c("terra", "sf", "tmap", "dplyr", "here")
new_pkgs <- req_pkgs[!req_pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) install.packages(new_pkgs)
for (p in req_pkgs) suppressPackageStartupMessages(library(p, character.only = TRUE))

tmap_mode("plot")

# ------------------------------------------------------------------
# 1. Parameters ------------------------------------------------------
# ------------------------------------------------------------------
set.seed(1234)
root_dir         <- here::here()
data_dir         <- file.path(root_dir, "data")
out_dir          <- file.path(root_dir, "output")
roi_buffer_m     <- 500
cover_field_frac <- 0.10

# ------------------------------------------------------------------
# 2. Crop classes & palettes ----------------------------------------
# ------------------------------------------------------------------
major_codes <- c(1, 2, 5, 3, 10, 4, 61)
label_map   <- c(`1` = "Corn", `2` = "Cotton", `5` = "Soybeans", `3` = "Rice",
                 `10` = "Peanuts", `4` = "Sorghum", `61` = "Fallow / Idle")
cb_palette  <- c("Corn" = "#F0E442", "Cotton" = "#D55E00", "Soybeans" = "#009E73",
                 "Rice" = "#56B4E9", "Peanuts" = "#E69F00", "Sorghum" = "#CC79A7",
                 "Fallow / Idle" = "#BBBBBB")
winter_pal  <- c("Fallow" = "#d8b365", "CoverCrop" = "#AA3377")   # purple

# ------------------------------------------------------------------
# 3. Read spatial & raster data -------------------------------------
# ------------------------------------------------------------------
fields <- sf::st_read(file.path(data_dir, "fields.kml"), quiet = TRUE) |> sf::st_make_valid()
fields$fid <- seq_len(nrow(fields))

cdl18 <- terra::rast(file.path(data_dir, "cdl_2018.tif"))
cdl19 <- terra::rast(file.path(data_dir, "cdl_2019.tif"))

aea <- terra::crs(cdl18)
fields <- sf::st_transform(fields, aea)

# ------------------------------------------------------------------
# 4. Prepare CDL rasters --------------------------------------------
# ------------------------------------------------------------------
roi <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(fields))) |> sf::st_buffer(roi_buffer_m)
cdl18_codes <- terra::crop(cdl18, terra::vect(roi))
cdl19_codes <- terra::crop(cdl19, terra::vect(roi))
cdl18_codes <- terra::subst(cdl18_codes, from = major_codes, to = major_codes, others = NA)
cdl19_codes <- terra::subst(cdl19_codes, from = major_codes, to = major_codes, others = NA)

as_factor_cdl <- function(r) {
  rf  <- terra::as.factor(r)
  rat <- levels(rf)[[1]]
  rat <- rat[rat$ID %in% major_codes, ]
  rat$Layer_1 <- label_map[as.character(rat$ID)]
  levels(rf) <- list(rat)
  rf
}

cdl18_fac <- as_factor_cdl(cdl18_codes)
cdl19_fac <- as_factor_cdl(cdl19_codes)

# ------------------------------------------------------------------
# 5. Build example winter cover crop -----------------------------------
# ------------------------------------------------------------------
fields$area_ha <- as.numeric(sf::st_area(fields)) / 1e4
sel_n   <- max(1, round(nrow(fields) * cover_field_frac))
sel_poly <- fields |> dplyr::arrange(dplyr::desc(area_ha)) |> dplyr::slice_head(n = sel_n) |> sf::st_union() |> terra::vect()
mask_ras <- terra::rasterize(sel_poly, cdl18_codes, field = 1, background = 0)

winter_num <- cdl18_codes
values(winter_num) <- ifelse(is.na(values(winter_num)), NA, 1)
winter_num[mask_ras == 1 & !is.na(winter_num)] <- 2

winter_fac <- terra::as.factor(winter_num)
levels(winter_fac) <- data.frame(ID = c(1, 2), winter = c("Fallow", "CoverCrop"))

# ------------------------------------------------------------------
# 6. Decoration helper ----------------------------------------------
# ------------------------------------------------------------------
add_decor <- function(map) {
  map +
    tm_scale_bar(position = c("left", "top")) +
    tm_compass(type = "arrow", position = c("right", "top"), size = 2) +
    tm_layout(frame = FALSE,
              bg.color = "grey90",             # footer band
              outer.margins = 0,
              inner.margins = c(0.02, 0.02, 0.18, 0.02))
}

# ------------------------------------------------------------------
# 7. Build single‑panel maps ----------------------------------------
# ------------------------------------------------------------------
fig2_map <- add_decor(
  tm_shape(cdl18_fac) +
    tm_raster(palette = cb_palette, style = "cat", title = "Major Crops 2018") +
    tm_shape(fields) + tm_borders(col = "grey50", lwd = 0.3)
)

fig2_map

fig3_map <- add_decor(
  tm_shape(winter_fac) +
    tm_raster(palette = winter_pal, style = "cat", title = "Winter Cover Crops\n2018–2019") +
    tm_shape(fields) + tm_borders(col = "grey50", lwd = 0.3)
)

fig3_map

fig4_map <- add_decor(
  tm_shape(cdl19_fac) +
    tm_raster(palette = cb_palette, style = "cat", title = "Major Crops 2019") +
    tm_shape(fields) + tm_borders(col = "grey50", lwd = 0.3)
)

fig4_map

# ------------------------------------------------------------------
# 8. Export figures --------------------------------------------------
# ------------------------------------------------------------------
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

tmap::tmap_save(fig2_map, file.path(out_dir, "Fig2.png"), width = 6, height = 6, units = "in", dpi = 300)
tmap::tmap_save(fig3_map, file.path(out_dir, "Fig3.png"), width = 6, height = 6, units = "in", dpi = 300)
tmap::tmap_save(fig4_map, file.path(out_dir, "Fig4.png"), width = 6, height = 6, units = "in", dpi = 300)

message("Individual maps exported → ", normalizePath(out_dir))
