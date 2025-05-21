###### Preparing data for third paper #########

rm(list=ls())


renv::restore()
#renv::init()

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
  reshape2
)

#renv::snapshot()


# 3.2.	Adoption of cover crops on fall cash crop fields
# Fig. XX: Adoption of cover crops on major fall cash crop fields 
# (referring to which cash crop is planted before in the same cover crop fields)

# Load the data 

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
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  labs(x = "Year", y = "% of MAP cropland acreage with cover crops") +
  scale_color_manual(values = c("Soybean-Cover Crops" = "#267000",
                                "Corn-Cover Crops" = "#ffd300",
                                "Cotton-Cover Crops" = "#ff2626",
                                "Other Crops-Cover Crops" = "#d69ebc"))+
  scale_x_continuous(breaks = seq(2013, 2019, by = 1))+
  theme(text = element_text(size = 16), axis.text = element_text(size = 12))+
  guides(color = guide_legend(title = "CDL Crops > Cover Crops"))


# add the images at the maximum year for each CDL_Cover
for (i in 1:nrow(df_max_year)) {
  p <- p + annotation_custom(
    raster_images[[df_max_year$CDL_Cover[i]]], 
    xmin = df_max_year$MaxYear[i] - 4, 
    xmax = df_max_year$MaxYear[i] + 4, 
    ymin = df_max_year$MaxCover[i] - 0.4, 
    ymax = df_max_year$MaxCover[i] + 0.4  
  )
}

print(p)


# save the plot
out_dir <- here("output")
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# define the full path
out_file <- here(
  "output",
  "Adoption of Cover Crops on CDL Fall Crop Fields_v1.tiff"
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
# Fig. XX: Cash crop succession following winter cover crop fields
# (referring to which cash crop is planted after in the same cover crop fields)


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
    "Cover Crops-Soybean"       = "#267000",
    "Cover Crops-Corn"          = "#ffd300",
    "Cover Crops-Cotton"        = "#ff2626",
    "Cover Crops-Other Crops"   = "#d69ebc"
  )) +
  scale_x_continuous(breaks = seq(2013, 2019, 1)) +
  theme(
    text      = element_text(size = 16),
    axis.text = element_text(size = 12)
  ) +
  guides(color = guide_legend(title = "Cover Crops > CDL Crops"))

# Annotate with images at each group’s max year
for (i in seq_len(nrow(df_max_year))) {
  lvl <- df_max_year$Cover_CDL[i]
  p <- p + annotation_custom(
    raster_images[[lvl]],
    xmin = df_max_year$MaxYear[i] - 3,
    xmax = df_max_year$MaxYear[i] + 3,
    ymin = df_max_year$MaxCover[i] - 0.3,
    ymax = df_max_year$MaxCover[i] + 0.3
  )
}

print(p)

# Save to output/ as a TIFF
out_dir <- here("output")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

out_file <- here(
  "output",
  "Succession of cash crops following winter cover crop fields_v2.tiff"
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
# Fig. XX: Cropping patterns 2013- Cash crops planted before and after winter cover crop 
# (only top 15 cropping pattern combinations were presented here per year)


# Read the data
file_path <- here("data", "Part_3_Top_15_2013to2019.csv")
data <- read_csv(file_path)

# Factor conversions and cleaning
data <- data %>%
  mutate(
    cropping_pattern = as.factor(cropping_pattern),
    Cover_vs_No_Cover = as.factor(Cover_vs_No_Cover),
    cropping_pattern = str_trim(cropping_pattern),
    cropping_pattern = str_to_title(cropping_pattern)
  ) %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))

# Prepare 2013 subset
data_2013 <- data %>% 
  filter(Year == 2013) %>%
  mutate(
    cropping_pattern = factor(
      cropping_pattern,
      levels = cropping_pattern[order(percentage, decreasing = FALSE)]
    )
  )

# Plot for 2013
year_2013 <- ggplot(data_2013, aes(x = cropping_pattern, y = percentage)) +
  geom_col(fill = "steelblue3") +
  labs(title = "Year 2013", x = "Cropping Pattern", y = "% of total cropland") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 20, 5)) +
  theme(
    text = element_text(size = 16),
    axis.text = element_text(size = 13),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )

# Prepare 2019 subset
data_2019 <- data %>% 
  filter(Year == 2019) %>%
  mutate(
    cropping_pattern = factor(
      cropping_pattern,
      levels = cropping_pattern[order(percentage, decreasing = FALSE)]
    )
  )

# Plot for 2019
year_2019 <- ggplot(data_2019, aes(x = cropping_pattern, y = percentage)) +
  geom_col(fill = "steelblue3") +
  labs(title = "Year 2019", x = "Cropping Pattern", y = "% of total cropland") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 20, 5), limits = c(0, 20)) +
  theme(
    text = element_text(size = 16),
    axis.text = element_text(size = 13),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )

# Combine with shared legend
g <- (year_2013 + year_2019 + plot_layout(guides = "collect")) +
  plot_layout(guides = "collect")

print(g)

# Save to output/ as a TIFF
out_dir <- here("output")
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

out_file <- here(
  "output",
  "Change Detection in Cropping Patterns_2013_2019.tiff"
)

tiff(
  filename = out_file,
  units    = "in",
  width    = 13,
  height   = 7,
  res      = 300
)
print(g)
dev.off()


# Fig. XX: Cropping patterns 2019- Cash crops planted before and after winter cover crop 
# (only top 15 cropping pattern combinations were presented here)


# Read and clean the data
file_path <- here("data", "Part_3_Top_15_2013to2019.csv")
data <- read_csv(file_path) %>%
  mutate(
    cropping_pattern  = str_trim(cropping_pattern) %>% str_to_title(),
    Cover_vs_No_Cover = as.factor(Cover_vs_No_Cover),
    cropping_pattern  = as.factor(cropping_pattern)
  ) %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))

# Define custom color palette
custom_palette <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b",
  "#e377c2", "#17becf", "#bcbd22", "#7f7f7f", "#dbdb8d", "#9edae5",
  "#a55194", "#843c39", "#f7b6d2", "#ad494a", "#8c6d31", "#636363",
  "#d6616b", "#b5bd22", "#d9d9d9", "#bc80bd", "#bebada"
)

# Plot for Cover Crops
plot_cover <- data %>%
  filter(Cover_vs_No_Cover == "Cover Crops") %>%
  ggplot(aes(x = factor(Year), y = percentage, fill = cropping_pattern)) +
  geom_col(position = "dodge", width = 0.8) +
  labs(x = "Year", y = "% of total cropland") +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = custom_palette) +
  theme(
    text         = element_text(size = 16),
    axis.text    = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12),
    legend.position = "right"
  ) +
  guides(fill = guide_legend(title = "Cropping Pattern")) +
  ggtitle("Cropping Patterns Following Cover Crops")

# Plot for Non-Cover Crops
plot_non_cover <- data %>%
  filter(Cover_vs_No_Cover == "Non Cover Crops") %>%
  ggplot(aes(x = factor(Year), y = percentage, fill = cropping_pattern)) +
  geom_col(position = "dodge", width = 0.8) +
  labs(x = "Year", y = "% of total cropland") +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = custom_palette) +
  theme(
    text         = element_text(size = 16),
    axis.text    = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12),
    legend.position = "right"
  ) +
  guides(fill = guide_legend(title = "Cropping Pattern")) +
  ggtitle("Cropping Patterns Without Cover Crops")

# Combine side-by-side
combined_plot <- plot_cover + plot_non_cover + plot_layout(guides = "collect")

print(combined_plot)

# Save the combined figure
out_dir <- here("output")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

out_file <- here("output", "Cropping_Patterns_Before_After_Cover_Crops_2013_2019.tiff")
tiff(filename = out_file, units = "in", width = 14, height = 8, res = 300)
print(combined_plot)
dev.off()


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


# HIGHLIGHTED COVER CROP USING A SINGKLE COLOR
# Cropping pattern over times from 2013 to 2019 (combined both cover crop and non-cover crops)
# Merge the cropping pattern names from 2013 to 2019

# Read & clean the data
data <- read_csv(here("data", "Part_3_Top_15_2013to2019.csv")) %>%
  mutate(
    cropping_pattern = str_trim(cropping_pattern) %>% str_to_title()
  ) %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))

# Aggregate across years & patterns
merged_data <- data %>%
  group_by(Year, cropping_pattern) %>%
  summarise(
    area       = sum(area, na.rm = TRUE),
    percentage = mean(percentage, na.rm = TRUE),
    .groups    = "drop"
  )

# Define which patterns to highlight in green
highlight_patterns <- c(
  "Soybean - Cover Crops - Soybean",
  "Minor Crops - Cover Crops - Minor Crops",
  "Soybean - Cover Crops - Minor Crops",
  "Cotton - Cover Crops - Cotton"
)

all_patterns   <- unique(merged_data$cropping_pattern)
cover_patterns <- intersect(all_patterns, highlight_patterns)
other_patterns <- setdiff(all_patterns, highlight_patterns)

# Build a named vector of colors:
# green for any cover‐crop sequence
# viridis palette for the rest
highlight_color   <- "#2ca02c"   # green
other_colors      <- viridis(length(other_patterns), option = "D")
fill_colors <- c(
  setNames(rep(highlight_color, length(cover_patterns)), cover_patterns),
  setNames(other_colors, other_patterns)
)

# Plot
h <- ggplot(merged_data, aes(
  x = factor(Year),
  y = percentage,
  fill = cropping_pattern
)) +
  geom_col(position = "dodge", width = 0.8) +
  labs(
    x = "Year",
    y = "Mean % of total cropland"
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = fill_colors) +
  guides(fill = guide_legend(
    title = "Cropping Pattern",
    ncol  = 1
  )) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text     = element_text(size = 13),
    legend.position = "right"
  )

print(h)

# Save to output/ as a TIFF
out_dir <- here("output")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

out_file <- here(
  "output",
  "Change_in_Cropping_Patterns_Time_Series.tiff"
)

tiff(
  filename = out_file,
  units    = "in",
  width    = 14,
  height   = 8,
  res      = 300
)
print(h)
dev.off()


#### As per Dr. Green Suggestion

# Part 1: Adoption of Cover Crops on CDL Fall Crop Fields (denominator = respective total crop acres)

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
    "Soybean-Cover Crops"     = "#267000",
    "Corn-Cover Crops"        = "#ffd300",
    "Cotton-Cover Crops"      = "#ff2626",
    "Other Crops-Cover Crops" = "#d69ebc"
  )) +
  scale_y_continuous(breaks = seq(0, 40, 10), limits = c(0, 40)) +
  theme(
    text          = element_text(size = 16),
    axis.text     = element_text(size = 12),
    legend.position = "right"
  ) +
  guides(fill = guide_legend(
    title = "CDL Crops > Cover Crops",
    ncol  = 1
  ))

print(i)

# Save the plot
out_dir <- here("output")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

tiff(
  filename = here(
    "output",
    "Adoption_of_Cover_Crops_on_CDL_Fall_Crop_Fields_denominator_respective_total_crop_acres_v2.tiff"
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
    "Cover Crops-Soybean"       = "#267000",
    "Cover Crops-Corn"          = "#ffd300",
    "Cover Crops-Cotton"        = "#ff2626",
    "Cover Crops-Other Crops"   = "#d69ebc"
  )) +
  scale_y_continuous(breaks = seq(0, 40, 10), limits = c(0, 40)) +
  theme(
    text          = element_text(size = 16),
    axis.text     = element_text(size = 12),
    legend.position = "right"
  ) +
  guides(fill = guide_legend(
    title = "Cover Crops > CDL Crops",
    ncol  = 1
  ))

print(j)

# Save the plot
tiff(
  filename = here(
    "output",
    "Succession_of_cash_crops_following_winter_cover_crop_fields_denominator_respective_total_crop_acres_v2.tiff"
  ),
  units = "in", width = 10, height = 6, res = 300
)
print(j)
dev.off()



# EXTRA ANALYSIS: 

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








