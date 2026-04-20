# R/02_full_model_and_diagnostics.R

rm(list = ls())

# --------------------------------------------------
# 0. Set project root path
# --------------------------------------------------
project_dir <- "/Users/guanchujun/Desktop/4130 Project"

source(file.path(project_dir, "R", "00_packages.R"))

out_dir <- file.path(project_dir, "Output", "Full_Model_and_Diagnostics")
fig_dir <- file.path(out_dir, "figures")
tab_dir <- file.path(out_dir, "tables")
mod_dir <- file.path(out_dir, "models")

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(mod_dir, recursive = TRUE, showWarnings = FALSE)

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
# 2. Fit full model
# --------------------------------------------------
full_model <- lm(
  price_unit_area ~ transaction_date + house_age + distance_mrt +
    n_convenience_stores + latitude + longitude,
  data = dat
)

saveRDS(
  full_model,
  file = file.path(mod_dir, "full_model.rds")
)

# --------------------------------------------------
# 3. Full model numerical output
# --------------------------------------------------
model_summary <- summary(full_model)
anova_full <- anova(full_model)

print(model_summary)
print(anova_full)

capture.output(
  model_summary,
  file = file.path(tab_dir, "full_model_summary.txt")
)

capture.output(
  anova_full,
  file = file.path(tab_dir, "full_model_anova.txt")
)

# --------------------------------------------------
# 4. Standard lm diagnostic plots
# --------------------------------------------------
par(mfrow = c(2, 3))
plot(full_model, which = 1:6)

png(
  filename = file.path(fig_dir, "full_model_diagnostic_plots_1to6.png"),
  width = 1800,
  height = 1100,
  res = 150
)

par(mfrow = c(2, 3))
plot(full_model, which = 1:6)

dev.off()

par(mfrow = c(1, 1))

# --------------------------------------------------
# 5. Build diagnostics table
# --------------------------------------------------
diag_df <- data.frame(
  id = dat$id,
  fitted = fitted(full_model),
  residual = resid(full_model),
  standardized_residual = rstandard(full_model),
  rstudent = rstudent(full_model),
  leverage = hatvalues(full_model),
  cooks_distance = cooks.distance(full_model)
)

write.csv(
  diag_df,
  file = file.path(tab_dir, "full_model_diagnostics_table.csv"),
  row.names = FALSE
)

# --------------------------------------------------
# 6. Rule-of-thumb cutoffs
# --------------------------------------------------
n <- nrow(dat)
p <- length(coef(full_model))
k <- p - 1

leverage_cutoff <- 2 * p / n
cooks_cutoff <- 4 / n

cat("n =", n, "\n")
cat("p =", p, "\n")
cat("k =", k, "\n")
cat("Leverage cutoff = 2p/n =", leverage_cutoff, "\n")
cat("Cook's distance cutoff = 4/n =", cooks_cutoff, "\n")

# --------------------------------------------------
# 7. Flag unusual observations
# --------------------------------------------------
flagged <- diag_df |>
  mutate(
    high_leverage = leverage > leverage_cutoff,
    outlier_y = abs(rstudent) > 2,
    influential = cooks_distance > cooks_cutoff
  )

write.csv(
  flagged,
  file = file.path(tab_dir, "flagged_observations.csv"),
  row.names = FALSE
)

# --------------------------------------------------
# 8. Additional diagnostic plots
# --------------------------------------------------
p_res_fit <- ggplot(diag_df, aes(x = fitted, y = residual)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  )

print(p_res_fit)

ggsave(
  filename = file.path(fig_dir, "residuals_vs_fitted.png"),
  plot = p_res_fit,
  width = 7,
  height = 5
)

qqnorm(
  rstandard(full_model),
  main = "Normal Q-Q Plot of Standardized Residuals"
)
qqline(rstandard(full_model), col = 2)

png(
  filename = file.path(fig_dir, "qqplot_standardized_residuals.png"),
  width = 900,
  height = 700,
  res = 150
)

qqnorm(
  rstandard(full_model),
  main = "Normal Q-Q Plot of Standardized Residuals"
)
qqline(rstandard(full_model), col = 2)

dev.off()

p_rstudent <- ggplot(flagged, aes(x = id, y = rstudent)) +
  geom_point() +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed") +
  labs(
    title = "R-student Residuals by Observation",
    x = "Observation ID",
    y = "R-student"
  )

print(p_rstudent)

ggsave(
  filename = file.path(fig_dir, "rstudent_by_observation.png"),
  plot = p_rstudent,
  width = 7,
  height = 5
)

p_lev <- ggplot(flagged, aes(x = id, y = leverage)) +
  geom_point() +
  geom_hline(yintercept = leverage_cutoff, linetype = "dashed") +
  labs(
    title = "Leverage by Observation",
    x = "Observation ID",
    y = "Leverage"
  )

print(p_lev)

ggsave(
  filename = file.path(fig_dir, "leverage_by_observation.png"),
  plot = p_lev,
  width = 7,
  height = 5
)

p_cook <- ggplot(flagged, aes(x = id, y = cooks_distance)) +
  geom_point() +
  geom_hline(yintercept = cooks_cutoff, linetype = "dashed") +
  labs(
    title = "Cook's Distance by Observation",
    x = "Observation ID",
    y = "Cook's Distance"
  )

print(p_cook)

ggsave(
  filename = file.path(fig_dir, "cooks_distance_by_observation.png"),
  plot = p_cook,
  width = 7,
  height = 5
)

# --------------------------------------------------
# 9. Formal normality tests
# --------------------------------------------------
shapiro_res <- shapiro.test(rstandard(full_model))
ad_res <- ad.test(rstandard(full_model))

print(shapiro_res)
print(ad_res)

capture.output(
  shapiro_res,
  file = file.path(tab_dir, "shapiro_test_standardized_residuals.txt")
)

capture.output(
  ad_res,
  file = file.path(tab_dir, "anderson_darling_test_standardized_residuals.txt")
)

# --------------------------------------------------
# 10. Breusch-Pagan test for constant variance
# --------------------------------------------------
bp_res <- bptest(full_model)

print(bp_res)

capture.output(
  bp_res,
  file = file.path(tab_dir, "breusch_pagan_test.txt")
)

cat("\nFull model fitting and diagnostics completed successfully.\n")