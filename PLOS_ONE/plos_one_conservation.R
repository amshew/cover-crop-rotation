###### Preparing data for third paper #########

rm(list=ls())

options(repos = c(CRAN="https://cran.rstudio.com/"))

if (!require(pacman)) install.packages("pacman", dependencies=TRUE)
library(pacman)

# 2) load/installpackage
pacman::p_load(
  RColorBrewer,
  ggpubr,        
  flextable,
  grid,
  png,
  ggtext,
  viridis,
  gridExtra,
  patchwork,
  here,
  tidyverse,     
  readxl,
  writexl,
  scales,
  reshape2,
  sf,
  terra, 
  tigris,
  ggspatial,
  patchwork,
  CropScapeR,
  tidyterra,
  tidytext,
  trend,
  broom
)


############################################################
## Figure 1 – US map with Arkansas-Delta inset + 2019 CDL ##
############################################################

options(tigris_use_cache = TRUE, tigris_class = "sf")

#  ╭───────────────────╮
#  │  1. Vector data   │
#  ╰───────────────────╯
# ── CONUS states (drop AK, HI, PR, etc.) ─────────────────────────
us_states <- states(cb = TRUE, year = 2020) |>
  filter(!(STUSPS %in% c("AK", "HI", "PR", "VI", "GU", "MP", "AS"))) |>
  st_transform(5070)

# Arkansas counties
# ── 0. Arkansas counties --------------------------------------------------------
ar_counties <- counties(state = "AR", cb = TRUE, year = 2020) |>
  st_transform(5070)    # lon/lat so we can test longitudes

delta_fips <- c("05001", "05017", "05003", "05043", "05041",
                "05079","05069","05107","05095", "05117",
                "05119","05085","05145","05147","05077",
                "05123","05035","05037","05067","05111",
                "05031","05093","05055","05075", "05021",
                "05121","05135")   # adjust as needed

delta_cnty <- ar_counties %>%
  filter(GEOID %in% delta_fips)

# (Optional) inspect which counties made the cut:
sort(unique(delta_cnty$NAME))

#  ╭────────────────────╮
#  │  2. Raster (CDL)   │
#  ╰────────────────────╯
# Download 2019 CDL for Arkansas from USDA NASS Cropland Data Layer

# ------------------------------------------------------------------
# ── 1. Arkansas CDL 2019 as RasterLayer ─────────────────────────
cdl_ar_rl <- GetCDLData(
  aoi   = "05",      # Arkansas FIPS; character string is safest
  year  = 2019,
  type  = "f",       # state/county FIPS
  format = "raster"  # <- must be raster, table, or sf
)

# ── 2. Convert to SpatRaster for terra workflows ────────────────
cdl_ar <- terra::rast(cdl_ar_rl)

# ── 3. Delta counties as SpatVector in raster CRS ──────────────
delta_vect <- delta_cnty |>
  st_transform(crs(cdl_ar)) |>
  terra::vect()                      # SpatVector for terra

# ── 4.  Bounding box in raster CRS (optional, if you still want bbox) ─────
delta_bbox <- delta_cnty |>
  st_transform(crs(cdl_ar)) |>
  st_bbox() |>
  st_as_sfc() |>
  terra::vect()       

# ── 5. Crop + mask ──────────────────────────────────────────────
# ------------------------------------------------------------------
# 1.  Crop & mask CDL to Delta counties ----------------------------
# ------------------------------------------------------------------
delta_cdl <- terra::crop(cdl_ar, delta_vect) |>
  terra::mask(delta_vect)

# ------------------------------------------------------------------
# 2.  Keep only major classes; everything else → NA ----------------
# ------------------------------------------------------------------
major_codes <- c(1, 2, 3, 4, 5, 10, 24, 37, 61)   # corn, rice, soybean, cotton, fallow
delta_cdl   <- terra::subst(delta_cdl,
                            from   = major_codes,
                            to     = major_codes,
                            others = NA)         # minor crops become NA

# ------------------------------------------------------------------
# 3.  Promote to factor and grab the RAT ---------------------------
# ------------------------------------------------------------------
delta_cdl <- terra::as.factor(delta_cdl)

# b. lookup table that comes with the CDL tile
rat <- levels(delta_cdl)[[1]]

# c. keep only the nine classes you care about, in your preferred order
major_codes <- c(1, 2, 5, 3, 10, 4, 61)   # corn, cotton, …
rat_major   <- rat[match(major_codes, rat$ID), ]

# d. overwrite the generic numeric labels with real crop names
label_map <- c(
  `1`  = "Corn",
  `2`  = "Cotton",
  `5`  = "Soybeans",
  `3`  = "Rice",
  #`24` = "Winter Wheat",
  `10` = "Peanuts",
  `4`  = "Sorghum",
  `61` = "Fallow / Idle"
)

rat_major$Layer_1 <- label_map[ as.character(rat_major$ID) ]

# e. re-attach the cleaned RAT
levels(delta_cdl) <- list(rat_major)

## -----------------------------------------------------------------
## 4. Build a named palette (one colour per crop)
## -----------------------------------------------------------------

cb_palette <- c(
  "Corn"          = "#F0E442",   
  "Cotton"        = "#D55E00",
  "Soybeans"      = "#009E73",
  "Rice"          = "#56B4E9",
  #"Winter Wheat"  = "#a6d854",
  "Peanuts"       = "#E69F00",
  "Sorghum"       = "#CC79A7",
  "Fallow / Idle" = "#BBBBBB"
)
names(cb_palette) <- rat_major$Layer_1   # names drive legend text

## -----------------------------------------------------------------
## 5. Inset map with the new legend
## -----------------------------------------------------------------
p_inset <- ggplot() +
  geom_spatraster(data = delta_cdl) +
  
  scale_fill_manual(
    values       = cb_palette,
    na.value     = "grey85",        # minor crops draw grey
    na.translate = FALSE,           # (and stay out of the legend)
    breaks       = rat_major$Layer_1,   # text order in legend
    labels       = rat_major$Layer_1,
    name         = "2019 Cropland Data Layer\n(Arkansas Delta)"
  ) +
  
  geom_sf(data=ar_counties, fill = NA, colour = "grey60", linewidth = 0.25) +
  geom_sf(data = delta_bbox, fill = NA, color = "red", linewidth = 0.5) +
  coord_sf(crs = st_crs(ar_counties)) +
  
  annotation_scale(location = "bl",
                   width_hint = 0.35,
                   pad_x = unit(0.65, "npc"),
                   pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "bl",
                         which_north = "true",
                         pad_x = unit(0.75, "npc"),
                         pad_y = unit(0.75, "cm"),
                         style  = north_arrow_fancy_orienteering) +
  
  #labs(title = "Arkansas Delta Study Region") +
  theme_minimal(base_size = 10) +
  theme(axis.text  = element_blank(),
        panel.grid = element_blank(),
        
  legend.position = c(0.18, 0.10),   # (x, y) in npc units
  legend.justification = c(0, 0),    # bottom-left corner of legend box
  legend.background   = element_rect(fill = "grey90", colour = NA),
  legend.margin       = margin(2, 2, 2, 2, unit = "pt")
  )

p_inset


# Left panel – CONUS map with bounding box
p_us <- ggplot() +
  geom_sf(data = us_states, fill = "grey90", color = "white", size = 0.2) +
  geom_sf(data = delta_bbox, fill = NA, color = "red", linewidth = 0.8) +
  theme_void() #+
  #labs(title = "United States Highlighting\nArkansas Study Region")

p_us

# Combine & export
fig1 <- p_us + p_inset + plot_layout(widths = c(0.5, 1.5))
fig1

ggsave("output/Fig1.png", fig1, width = 10, height = 6, dpi = 300)


###################################################################################################333333

# 3.2	Adoption of cover crops on fall cash crop fields
# Fig. 6: Adoption of cover crops on major fall cash crop fields 
# (referring to which cash crop is planted before in the same cover crop fields)

# Load the data 

# Figure 6

# Defined the file path using the here package
file_path <- here("data", "Part 1.csv")
data <- read_csv(file_path)


# Clean and standardize the cropping pattern names
data$CDL_Cover <- str_trim(data$CDL_Cover)  
data$CDL_Cover <- str_to_title(data$CDL_Cover) 


# Replace blank cells with NAs
data <- data %>%
  mutate_if(is.character, ~na_if(.x, ""))


# Subset the data to include only the major crops (Soybean, Corn, and Cotton)
major_crops <- filter(data, CDL_Cover %in% c("Soybean-Cover Crops", "Corn-Cover Crops", "Cotton-Cover Crops"))

# Calculate the total cover crop percentage for other crops by year
other_crops <- data %>%
  filter(!(CDL_Cover %in% c("Soybean-Cover Crops", "Corn-Cover Crops", "Cotton-Cover Crops"))) %>%
  group_by(Year) %>%
  summarise(Percentage_Cover = sum(Percentage_Cover))

# Create a new data frame for the "Other Crops" category
other_crops <- data.frame(
  Year = other_crops$Year,
  CDL_Cover = "Other Crops-Cover Crops",
  Percentage_Cover = other_crops$Percentage_Cover
)

# Combine the major crops and other crops data frames using bind_rows
merged_data <- bind_rows(major_crops, other_crops)

# define just the filename
image_files <- c(
  "soybean.png",
  "corn.png",
  "cotton.png",
  "Other Crops.png"
)

# build a vector of full paths under my project’s img/ folder
image_paths <- here("img", image_files)

# name each path by the corresponding level in merged_data$CDL_Cover
names(image_paths) <- unique(merged_data$CDL_Cover)

# read them in as rasterGrobs
raster_images <- lapply(image_paths, function(path) {
  if (!file.exists(path)) {
    stop("Image not found: ", path)
  }
  rasterGrob(readPNG(path), interpolate = TRUE)
})


# find the maximum year for each CDL_Cover
df_max_year <- merged_data %>%
  group_by(CDL_Cover) %>%
  #summarise(MaxYear = max(Year), MaxCover = max(Percentage_Cover))
  summarise(MaxYear = max(Year), MaxCover = Percentage_Cover[which.max(Year)])

merged_data$CDL_Cover <- factor(merged_data$CDL_Cover, 
                                levels = c("Soybean-Cover Crops", 
                                           "Corn-Cover Crops", 
                                           "Cotton-Cover Crops", 
                                           "Other Crops-Cover Crops"))


p <- ggplot(merged_data, aes(x = Year, y = Percentage_Cover, color = CDL_Cover, group = CDL_Cover)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  labs(x = "Year", y = "% of MAP cropland acreage with cover crops") +
  scale_color_manual(values = c("Soybean-Cover Crops" = "#009E73",
                                "Corn-Cover Crops" = "#F0E442",
                                "Cotton-Cover Crops" = "#D55E00",
                                "Other Crops-Cover Crops" = "#56B4E9"))+
  scale_x_continuous(breaks = seq(2013, 2019, by = 1))+
  theme(
    text                  = element_text(size = 16),
    axis.text             = element_text(size = 12),
    panel.grid.major.x    = element_blank(),  # remove major vertical lines
    panel.grid.minor.x    = element_blank()   # remove minor vertical lines
  ) +
  guides(color = guide_legend(title = "CDL Crops > Cover Crops"))

print(p)

# save the plot
out_dir <- here("output")
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# define the full path
out_file <- here(
  "output",
  "Fig6.tiff"
)

# write the file
tiff(
  filename = out_file,
  units    = "in",
  width    = 10,
  height   = 5,
  res      = 300
)
print(p) 

dev.off()


# 3.3.	Succession of cash crops following winter cover crop fields   
# Fig. 7: Cash crop succession following winter cover crop fields
# (referring to which cash crop is planted after in the same cover crop fields)

# Figure 7

# Read the data
file_path <- here("data", "Part 2.csv")
data <- read_csv(file_path)

# Clean and standardize the cropping pattern names
data <- data %>%
  mutate(
    Cover_CDL = str_trim(Cover_CDL),
    Cover_CDL = str_to_title(Cover_CDL)
  ) %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))

# Split into major crops and calculate “Other Crops”
major_crops <- data %>%
  filter(Cover_CDL %in% c("Cover Crops-Soybean", "Cover Crops-Corn", "Cover Crops-Cotton"))

other_crops <- data %>%
  filter(! Cover_CDL %in% c("Cover Crops-Soybean", "Cover Crops-Corn", "Cover Crops-Cotton")) %>%
  group_by(Year) %>%
  summarise(Percentage_Cover = sum(Percentage_Cover), .groups = "drop") %>%
  mutate(Cover_CDL = "Cover Crops-Other Crops")

merged_data <- bind_rows(major_crops, other_crops)

# Prepare the images via here()
image_files <- c("soybean.png", "corn.png", "cotton.png", "Other Crops.png")
image_paths <- here("img", image_files)
names(image_paths) <- unique(merged_data$Cover_CDL)

raster_images <- lapply(image_paths, function(path) {
  if (!file.exists(path)) stop("Image not found: ", path)
  rasterGrob(readPNG(path), interpolate = TRUE)
})

# Find the max‐year and corresponding cover% for each group
df_max_year <- merged_data %>%
  group_by(Cover_CDL) %>%
  summarise(
    MaxYear  = max(Year),
    MaxCover = Percentage_Cover[which.max(Year)],
    .groups  = "drop"
  )

# Fix factor order
merged_data$Cover_CDL <- factor(
  merged_data$Cover_CDL,
  levels = c(
    "Cover Crops-Soybean",
    "Cover Crops-Corn",
    "Cover Crops-Cotton",
    "Cover Crops-Other Crops"
  )
)

# Build the plot
p <- ggplot(merged_data, aes(Year, Percentage_Cover, color = Cover_CDL, group = Cover_CDL)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  labs(
    x = "Year",
    y = "% of MAP cropland acreage with cover crops"
  ) +
  scale_color_manual(values = c(
    "Cover Crops-Soybean"       = "#009E73",
    "Cover Crops-Corn"          = "#F0E442",
    "Cover Crops-Cotton"        = "#D55E00",
    "Cover Crops-Other Crops"   = "#56B4E9"
  )) +
  scale_x_continuous(breaks = seq(2013, 2019, 1)) +
  scale_y_continuous(
    limits = c(0, 8),                     # ← hard cap at 8
    breaks = seq(0, 8, 2)                 # optional: control your tick marks
  ) +
  theme(
    text                  = element_text(size = 16),
    axis.text             = element_text(size = 12),
    panel.grid.major.x    = element_blank(),  # remove major vertical lines
    panel.grid.minor.x    = element_blank()   # remove minor vertical lines
  ) +
  guides(color = guide_legend(title = "Cover Crops > CDL Crops"))

print(p)

# Save to output/ as a TIFF
out_dir <- here("output")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

out_file <- here(
  "output",
  "Fig7.tiff"
)

tiff(
  filename = out_file,
  units    = "in",
  width    = 10,
  height   = 5,
  res      = 300
)
print(p)
dev.off()


# 3.4. Change detection in cropping patterns: Cash crops planted before and after winter cover crop 
# Fig. 8: Cropping patterns 2013- Cash crops planted before and after winter cover crop 
# (only top 15 cropping pattern combinations were presented here per year)

# ─────────────────────────────────────────────────────────────
# Figure 8 – Inter-annual cover-crop/cash-crop sequences
# Produces one 7-panel bar chart (facets by year, 2013-2019)
# ─────────────────────────────────────────────────────────────

file_path <- here("data", "Part_3_Top_15_2013to2019.csv")

data <- read_csv(file_path, show_col_types = FALSE) %>% 
  mutate(
    cropping_pattern  = str_to_title(str_trim(cropping_pattern)),
    Cover_vs_No_Cover = factor(Cover_vs_No_Cover)
  ) %>% 
  mutate(across(where(is.character), ~na_if(.x, ""))) 

# Keep the 15 largest patterns within **each** year
data_top <- data %>% 
  group_by(Year) %>% 
  slice_max(order_by = percentage, n = 15, with_ties = FALSE) %>% 
  ungroup()

# Use reorder_within() so bars are ordered inside each facet
fig8 <- ggplot(data_top,
               aes(x = reorder_within(cropping_pattern, percentage, Year),
                   y = percentage)) +
  # make bars 80% of the slot height so you see a sliver of gap
  geom_col(fill = "#AA3377", width = 0.8) +
  
  facet_wrap(~Year, ncol = 2, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(breaks = seq(0, 20, 5),
                     expand = expansion(mult = c(0, .05))) +
  labs(x = "Cropping pattern", y = "% of total cropland") +
  theme_minimal(base_size = 14) +
  theme(
    panel.spacing = unit(1, "lines"),
    axis.text.y   = element_text(size = 9)
  )

# draw it interactively
fig8

# (2) bump the height when you save so that each of the 7 facets has more inches
ggsave(
  "output/Fig8.tiff",
  plot   = fig8,
  device = "tiff",
  bg     = "white",
  width  = 13,
  height = 10,   # <- up from 8” to give more space
  units  = "in",
  dpi    = 300
)


# Quick stat significance of year over year cover crop adoption trends
trend_results <- data %>%            # or data_top for top-15 only
  group_by(cropping_pattern) %>% 
  do(tidy(lm(percentage ~ Year, data = .))) %>% 
  ungroup() %>% 
  filter(term == "Year") %>% 
  mutate(sig = ifelse(p.value < 0.05, "Yes", "No"))

# quick glance
trend_results %>%
  select(cropping_pattern, estimate, p.value, sig) %>%
  arrange(p.value) %>%
  # write.csv is a thin wrapper around write.table
  write.csv(
    "output/table_s1.csv",
    row.names = FALSE,
    quote     = FALSE
  )


#############################################################################################################333
# Table 1 - percent of total cropland by cropping pattern

# 1) read & clean
df <- read_csv(here("data","Part_3_Top_15_2013to2019.csv")) #%>%
  mutate(
    cropping_pattern  = str_trim(cropping_pattern) %>% str_to_title(),
    Cover_vs_No_Cover = factor(Cover_vs_No_Cover),
    cropping_pattern  = factor(cropping_pattern)
  ) %>%
  mutate(across(where(is.character), ~na_if(.x, "")))

table_patterns <- df %>%
  group_by(cropping_pattern, Year) %>%
  summarize(
    percentage = sum(percentage, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  pivot_wider(
    names_from   = Year,
    values_from  = percentage,
    values_fill  = 0
  ) %>%
  arrange(cropping_pattern)

# 3) Inspect in R
print(table_patterns)

# 4) Write out as CSV with no quotation marks
write.csv(
  table_patterns,
  "output/table_1.csv",
  row.names = FALSE,
  quote     = FALSE
)

# Cropping pattern over times from 2013 to 2019 (combined both cover crop and non-cover crops)

# Read and clean the data
file_path <- here("data", "Part_3_Top_15_2013to2019.csv")
data <- read_csv(file_path) %>%
  mutate(
    cropping_pattern = str_trim(cropping_pattern) %>% str_to_title()
  ) %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))

# Aggregate by Year & Pattern
merged_data <- data %>%
  group_by(Year, cropping_pattern) %>%
  summarise(
    area       = sum(area),
    percentage = mean(percentage),
    .groups    = "drop"
  )

# Plot grouped bar chart
p <- ggplot(merged_data, aes(
  x    = factor(Year),
  y    = percentage,
  fill = cropping_pattern
)) +
  geom_col(position = "dodge", width = 0.8) +
  labs(
    title = "Change in Cropping Patterns over Time",
    x     = "Year",
    y     = "Mean % of Total Cropland"
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_viridis_d(
    option = "D",
    labels = sort(unique(merged_data$cropping_pattern))
  ) +
  theme(
    text         = element_text(size = 16),
    axis.text    = element_text(size = 12),
    legend.position = "right"
  ) +
  guides(fill = guide_legend(title = "Cropping Pattern"))

print(p)

# Save to output/ as a TIFF
out_dir <- here("output")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

out_file <- here(
  "output",
  "Change_in_Cropping_Patterns_2013_2019_Combined.tiff"
)

tiff(
  filename = out_file,
  units    = "in",
  width    = 14,
  height   = 8,
  res      = 300
)
print(p)
dev.off()




#### As per Dr. Green Suggestion

# Part 1: Adoption of Cover Crops on CDL Fall Crop Fields (denominator = respective total crop acres)

# Figure S1:

# Read the data
data1 <- read_csv(here("data", "Part 1_Dr.Green.csv"))

# Build the plot
i <- ggplot(data1, aes(
  x    = factor(Year),
  y    = Percentage_Cover,
  fill = factor(
    CDL_Cover,
    levels = c(
      "Soybean-Cover Crops",
      "Corn-Cover Crops",
      "Cotton-Cover Crops",
      "Other Crops-Cover Crops"
    )
  )
)) +
  geom_col(position = "dodge", width = 0.8) +
  labs(
    x = "Year",
    y = "% Cover crops based on total respective crop acres"
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = c(
    "Soybean-Cover Crops"     = "#009E73",
    "Corn-Cover Crops"        = "#F0E442",
    "Cotton-Cover Crops"      = "#D55E00",
    "Other Crops-Cover Crops" = "#56B4E9"
  )) +
  scale_y_continuous(breaks = seq(0, 40, 10), limits = c(0, 40)) +
  theme(
    text                  = element_text(size = 16),
    axis.text             = element_text(size = 12),
    panel.grid.major.x    = element_blank(),  # remove major vertical lines
    panel.grid.minor.x    = element_blank()   # remove minor vertical lines
  ) +
  guides(fill = guide_legend(
    title = "Major Crops > Cover Crops",
    ncol  = 1
  ))

print(i)

# Save the plot
out_dir <- here("output")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

tiff(
  filename = here(
    "output",
    "FigS1.tiff"
  ),
  units = "in", width = 10, height = 6, res = 300
)
print(i)
dev.off()


# Part 2: Succession of Cash Crops Following Winter Cover Crop Fields (denominator = respective total crop acres)
# Read the data
data2 <- read_csv(here("data", "Part 2_Dr.Green.csv"))

# Build the plot
j <- ggplot(data2, aes(
  x    = factor(Year),
  y    = Percentage_Cover,
  fill = factor(
    Cover_CDL,
    levels = c(
      "Cover Crops-Soybean",
      "Cover Crops-Corn",
      "Cover Crops-Cotton",
      "Cover Crops-Other Crops"
    )
  )
)) +
  geom_col(position = "dodge", width = 0.8) +
  labs(
    x = "Year",
    y = "% Cover crops based on total respective crop acres"
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = c(
    "Cover Crops-Soybean"       = "#009E73",
    "Cover Crops-Corn"          = "#F0E442",
    "Cover Crops-Cotton"        = "#D55E00",
    "Cover Crops-Other Crops"   = "#56B4E9"
  )) +
  scale_y_continuous(breaks = seq(0, 40, 10), limits = c(0, 40)) +
  theme(
    text          = element_text(size = 16),
    axis.text     = element_text(size = 12),
    legend.position = "right"
  ) +
  guides(fill = guide_legend(
    title = "Cover Crops > Major Crops",
    ncol  = 1
  ))

print(j)

# Save the plot
tiff(
  filename = here(
    "output",
    "FigS2.tiff"
  ),
  units = "in", width = 10, height = 6, res = 300
)
print(j)
dev.off()


# County‐level Correlation of Govt vs. Voluntary Cover Crop Acres (2013–2019)

# Read & clean the county data
combined_county <- read_csv(
  here("data", "Govt_row_and_Model_row_mean_by_County.csv")
) %>%
  filter(complete.cases(.))

# Simple scatter with regression & Pearson’s r
p1 <- ggscatter(
  combined_county,
  x         = "Govt_Row Total_2013_2019_Acres",
  y         = "Voluntary_Adoption_Diff_Govt_row_minus_Model_row",
  color     = "black",
  add       = "reg.line",
  add.params= list(color = "blue", fill = "lightgray"),
  conf.int  = TRUE,
  cor.coef  = TRUE,
  cor.method= "pearson",
  xlab      = "Total Govt Cost-Shared Cover Crop Acres (2013–2019)",
  ylab      = "Total Voluntary Adoption Cover Crop Acres (2013–2019)"
) +
  scale_x_continuous(
    breaks = seq(0, 60000, 5000),
    labels = number_format(scale = 1e-3, suffix = "K")
  ) +
  scale_y_continuous(
    breaks = seq(0, 400000, 50000),
    labels = number_format(scale = 1e-3, suffix = "K")
  ) +
  theme(
    text      = element_text(size = 16),
    axis.text = element_text(size = 13)
  )

print(p1)


# Scatter with county labels (no regression)
p2 <- ggscatter(
  combined_county,
  x      = "Govt_Row Total_2013_2019_Acres",
  y      = "Voluntary_Adoption_Diff_Govt_row_minus_Model_row",
  color  = "County",
  label  = "County",
  repel  = TRUE,
  xlab   = "Total Govt Cost-Shared Cover Crop Acres (2013–2019)",
  ylab   = "Total Voluntary Adoption Cover Crop Acres (2013–2019)"
) +
  scale_x_continuous(
    breaks = seq(0, 60000, 5000),
    labels = number_format(scale = 1e-3, suffix = "K")
  ) +
  scale_y_continuous(
    breaks = seq(0, 400000, 50000),
    labels = number_format(scale = 1e-3, suffix = "K")
  ) +
  theme(
    legend.position = "none",
    text            = element_text(size = 14),
    axis.text       = element_text(size = 13)
  )

print(p2)

# Scatter with both regression & county labels
j <- ggscatter(
  combined_county,
  x         = "Govt_Row Total_2013_2019_Acres",
  y         = "Voluntary_Adoption_Diff_Govt_row_minus_Model_row",
  color     = "County",
  label     = "County",
  repel     = TRUE,
  add       = "reg.line",
  add.params= list(color = "blue", fill = "lightgray"),
  conf.int  = TRUE,
  cor.coef  = TRUE,
  cor.method= "pearson",
  label.x   = 3000,
  label.sep = "\n",
  xlab      = "Total Govt Cost-Shared Cover Crop Acres (2013–2019)",
  ylab      = "Total Voluntary Adoption Cover Crop Acres (2013–2019)"
) +
  scale_x_continuous(
    breaks = seq(0, 60000, 5000),
    labels = number_format(scale = 1e-3, suffix = "K")
  ) +
  scale_y_continuous(
    breaks = seq(0, 400000, 50000),
    labels = number_format(scale = 1e-3, suffix = "K")
  ) +
  theme(
    legend.position = "none",
    text            = element_text(size = 14),
    axis.text       = element_text(size = 13)
  )

print(j)

# Save the final correlation plot
out_dir <- here("output")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

out_file <- here("output", "County_CoverCrop_Correlation.tiff")
tiff(
  filename = out_file,
  units    = "in",
  width    = 10,
  height   = 7,
  res      = 300
)
print(j)
dev.off()


## Model testing

model <- lm(Voluntary_Adoption_Diff_Govt_row_minus_Model_row ~ `Govt_Row Total_2013_2019_Acres`, data = combined_county)
summary(model)



## Trying different model

# Melt the data for easier plotting
m_melted <- melt(combined_county, id.vars = "County", 
                 measure.vars = c("Govt_Row Total_2013_2019_Acres", "Voluntary_Adoption_Diff_Govt_row_minus_Model_row", "Model_Row_Total__2013_2019_Acres"),
                 variable.name = "Adoption_Type", value.name = "Acres")

# Determine max Y-axis value across all adoption metrics and set as upper limit
max_y <- ceiling(max(m_melted$Acres) / 10000) * 10000  # Rounds up to the nearest 10,000 for consistent scaling

# Plot the faceted bar plot
ggplot(m_melted, aes(x = Adoption_Type, y = Acres, fill = Adoption_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ County, scales = "free_y") + # Facet by county
  theme_minimal() +
  labs(x = "Adoption Type", y = "Acres",
       title = "Comparison of Government Cost-Shared, Voluntary, and Model-Predicted Cover Crop Adoption (2013-2019)") +
  scale_y_continuous(limits = c(0, max_y), labels = comma) + # Set consistent Y-axis limits and format with commas
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")








