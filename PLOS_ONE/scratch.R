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
highlight_color   <- "#AA3377"   # green
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


#######################################################################################3
