# R/01_initial_data_analysis.R

rm(list = ls())

# --------------------------------------------------
# 0. Set project root path
# --------------------------------------------------
project_dir <- "/Users/guanchujun/Desktop/4130 Project"

source(file.path(project_dir, "R", "00_packages.R"))

out_dir <- file.path(project_dir, "Output", "Initial_Data_Analysis")
fig_dir <- file.path(out_dir, "figures")
tab_dir <- file.path(out_dir, "tables")

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

# --------------------------------------------------
# 1. Import data
# --------------------------------------------------
dat <- read_excel(file.path(project_dir, "Data", "Real estate valuation data set.xlsx"))

names(dat) <- c(
  "id",
  "transaction_date",
  "house_age",
  "distance_mrt",
  "n_convenience_stores",
  "latitude",
  "longitude",
  "price_unit_area"
)

# --------------------------------------------------
# 2. Basic structure and summary
# --------------------------------------------------
cat("Current working directory:\n")
print(getwd())

cat("\nData structure:\n")
str(dat)

cat("\nSummary statistics:\n")
print(summary(dat))

capture.output(
  str(dat),
  file = file.path(tab_dir, "data_structure.txt")
)

capture.output(
  summary(dat),
  file = file.path(tab_dir, "summary_all_variables.txt")
)

# --------------------------------------------------
# 3. Response variable analysis
# --------------------------------------------------
p_hist <- ggplot(dat, aes(x = price_unit_area)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Histogram of House Price of Unit Area",
    x = "House Price of Unit Area",
    y = "Frequency"
  )

print(p_hist)

ggsave(
  filename = file.path(fig_dir, "hist_price_unit_area.png"),
  plot = p_hist,
  width = 7,
  height = 5
)

p_box <- ggplot(dat, aes(y = price_unit_area)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of House Price of Unit Area",
    y = "House Price of Unit Area"
  )

print(p_box)

ggsave(
  filename = file.path(fig_dir, "boxplot_price_unit_area.png"),
  plot = p_box,
  width = 5,
  height = 5
)

# --------------------------------------------------
# 4. Scatterplots of response vs predictors
# --------------------------------------------------
predictors <- c(
  "transaction_date",
  "house_age",
  "distance_mrt",
  "n_convenience_stores",
  "latitude",
  "longitude"
)

for (xvar in predictors) {
  p <- ggplot(dat, aes_string(x = xvar, y = "price_unit_area")) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = paste("Price Unit Area vs", xvar),
      x = xvar,
      y = "Price Unit Area"
    )
  
  print(p)
  
  ggsave(
    filename = file.path(fig_dir, paste0("scatter_price_vs_", xvar, ".png")),
    plot = p,
    width = 7,
    height = 5
  )
}

# --------------------------------------------------
# 5. Correlation matrix
# --------------------------------------------------
cor_dat <- dat |>
  dplyr::select(
    transaction_date,
    house_age,
    distance_mrt,
    n_convenience_stores,
    latitude,
    longitude,
    price_unit_area
  )

cor_matrix <- cor(cor_dat)

print(cor_matrix)

write.csv(
  cor_matrix,
  file = file.path(tab_dir, "correlation_matrix.csv"),
  row.names = TRUE
)

# --------------------------------------------------
# 6. Pairwise scatterplot matrix
# --------------------------------------------------
pairs(
  cor_dat,
  main = "Scatterplot Matrix of Main Variables"
)

png(
  filename = file.path(fig_dir, "pairs_plot_main_variables.png"),
  width = 1600,
  height = 1600,
  res = 150
)

pairs(
  cor_dat,
  main = "Scatterplot Matrix of Main Variables"
)

dev.off()

cat("\nInitial data analysis completed successfully.\n")