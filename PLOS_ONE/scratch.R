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

