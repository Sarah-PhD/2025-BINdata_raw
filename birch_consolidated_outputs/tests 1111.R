

library(lme4)

# ==============================================
# 1. ORIGINAL NATIONAL STAND-LEVEL MODEL
# ==============================================

main_formula <- damage_N ~ species * (
  prop_s_stand +
    mean_north_c +
    total_birch_c +
    height_c +
    total_cervid_pellets_log_mean
) +
  pct + year +
  (1 | area/stand_id)

# Prepare the exact model data
model_vars <- all.vars(main_formula)

df_main <- stand_data[
  complete.cases(stand_data[, model_vars]),
  model_vars,
  drop = FALSE
]

df_main <- droplevels(df_main)

df_main$species <- factor(df_main$species)
df_main$area <- factor(df_main$area)
df_main$stand_id <- factor(df_main$stand_id)
df_main$pct <- factor(df_main$pct)
df_main$year <- factor(df_main$year)

# Fit the ORIGINAL model
m_main <- lmer(
  main_formula,
  data = df_main,
  REML = TRUE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 200000)
  )
)

summary(m_main)

# Check observations and grouping
nobs(m_main)
length(unique(df_main$area))
nrow(unique(df_main[c("area", "stand_id")]))

# ==============================================
# 2. RANDOM EFFECTS
# ==============================================

# Random-effect variances
VarCorr(m_main)

# Singular fit check
isSingular(m_main)

# Convergence information
m_main@optinfo$conv

# Area and stand random effects
ranef(m_main)

# Plot random effects with confidence intervals
re <- ranef(m_main, condVar = TRUE)

pdf("random_effects.pdf", width = 10, height = 7)

dotplot_re <- function(x, title) {
  plot(
    seq_along(x),
    sort(x),
    pch = 16,
    xlab = "Group (sorted)",
    ylab = "Random intercept",
    main = title
  )
  abline(h = 0, col = "red", lty = 2)
}

dotplot_re(
  re$area[, 1],
  "Area random intercepts"
)

dotplot_re(
  re[["stand_id:area"]][, 1],
  "Stand random intercepts"
)

dev.off()

# ==============================================
# 3. RESIDUAL NORMALITY AND SKEWNESS
# ==============================================

res <- residuals(m_main)
fit <- fitted(m_main)

# Residual skewness
skewness_value <- mean(
  (res - mean(res))^3
) / sd(res)^3

cat("Residual skewness:", skewness_value, "\n")

# Shapiro test (supplementary only)
shapiro.test(res)

pdf("residual_normality.pdf", width = 10, height = 8)

par(mfrow = c(2, 2))

hist(
  res,
  breaks = 35,
  main = "Residual distribution",
  xlab = "Residuals"
)

qqnorm(res, main = "Normal Q-Q plot")
qqline(res, col = "red", lwd = 2)

plot(
  fit, res,
  xlab = "Fitted damage",
  ylab = "Residuals",
  main = "Residuals vs fitted"
)
abline(h = 0, col = "red", lty = 2)

plot(
  fit,
  sqrt(abs(res / sigma(m_main))),
  xlab = "Fitted damage",
  ylab = "Sqrt(abs standardized residuals)",
  main = "Scale-location"
)

dev.off()

# ==============================================
# 4. HETEROSCEDASTICITY BY SPECIES AND AREA
# ==============================================

diag_df <- data.frame(
  residual = res,
  fitted = fit,
  species = df_main$species,
  area = df_main$area
)

pdf("residual_variance.pdf", width = 11, height = 8)

par(mfrow = c(2, 1))

boxplot(
  residual ~ species,
  data = diag_df,
  main = "Residuals by birch species",
  xlab = "Species",
  ylab = "Residuals"
)
abline(h = 0, col = "red", lty = 2)

boxplot(
  residual ~ area,
  data = diag_df,
  main = "Residuals by study area",
  xlab = "Area",
  ylab = "Residuals",
  las = 2
)
abline(h = 0, col = "red", lty = 2)

dev.off()

# Residual standard deviations by group
aggregate(
  residual ~ species,
  data = diag_df,
  FUN = sd
)

aggregate(
  residual ~ area,
  data = diag_df,
  FUN = sd
)

# ==============================================
# 5. RANDOM-EFFECT NORMALITY
# ==============================================

area_re <- ranef(m_main)$area[, 1]
stand_re <- ranef(m_main)[["stand_id:area"]][, 1]

pdf("random_effect_normality.pdf", width = 10, height = 5)

par(mfrow = c(1, 2))

qqnorm(
  area_re,
  main = "Area random effects"
)
qqline(area_re, col = "red")

qqnorm(
  stand_re,
  main = "Stand random effects"
)
qqline(stand_re, col = "red")

dev.off()

# ==============================================
# 6. SAVE FOR STATISTICIAN
# ==============================================

dir.create(
  "statistician_review",
  showWarnings = FALSE
)

saveRDS(
  m_main,
  "statistician_review/main_model.rds"
)

saveRDS(
  df_main,
  "statistician_review/main_model_data.rds"
)

write.csv(
  df_main,
  "statistician_review/main_model_data.csv",
  row.names = FALSE
)

capture.output(
  summary(m_main),
  VarCorr(m_main),
  isSingular(m_main),
  m_main@optinfo$conv,
  paste("Residual skewness:", skewness_value),
  file = "statistician_review/model_diagnostics.txt"
)

capture.output(
  sessionInfo(),
  file = "statistician_review/session_info.txt"
)

# Copy diagnostic PDFs into review folder
file.copy(
  c(
    "random_effects.pdf",
    "residual_normality.pdf",
    "residual_variance.pdf",
    "random_effect_normality.pdf"
  ),
  "statistician_review",
  overwrite = TRUE
)

cat(
  "Saved to:",
  normalizePath("statistician_review"),
  "\n"
)
# ============================================================
# NATIONAL STAND MODEL - UNEQUAL VARIANCE + DIAGNOSTICS
# ============================================================

library(nlme)

# 1. Prepare data
stopifnot(exists("stand_data"))

df_sensitivity <- stand_data

# Create region if it is not already present
if (!"region" %in% names(df_sensitivity)) {
  north <- c("Fredrika", "Lycksele", "Malå",
             "Nordmaling", "Råneå", "Sorsele")
  centre <- c("Furudal", "Ljusdal", "Lofsdalen")
  south <- c("Barksätter", "ÖsterMalma",
             "Åtvidaberg", "Växjö")
  
  df_sensitivity$region <- ifelse(
    df_sensitivity$area %in% north, "North",
    ifelse(df_sensitivity$area %in% centre, "Centre",
           ifelse(df_sensitivity$area %in% south,
                  "South", NA_character_))
  )
}

df_sensitivity$species <- factor(df_sensitivity$species)
df_sensitivity$area <- factor(df_sensitivity$area)
df_sensitivity$stand_id <- factor(df_sensitivity$stand_id)
df_sensitivity$pct <- factor(df_sensitivity$pct)
df_sensitivity$year <- factor(df_sensitivity$year)
df_sensitivity$region <- factor(df_sensitivity$region)

# Species-region groups for residual variance
df_sensitivity$species_region <- interaction(
  df_sensitivity$species,
  df_sensitivity$region,
  drop = TRUE
)

# Original fixed effects - NO three-way interaction
main_formula <- damage_N ~ species * (
  prop_s_stand +
    mean_north_c +
    total_birch_c +
    height_c +
    total_cervid_pellets_log_mean
) + pct + year

# Remove incomplete rows
needed <- c(
  all.vars(main_formula),
  "area", "stand_id", "species_region"
)

df_sensitivity <- droplevels(
  df_sensitivity[
    complete.cases(df_sensitivity[, needed]),
  ]
)

# Ensure downy birch is the reference species
df_sensitivity$species <- relevel(
  df_sensitivity$species,
  ref = "Downy"
)

# Nested grouping identifier
df_sensitivity$stand_nested <- interaction(
  df_sensitivity$area,
  df_sensitivity$stand_id,
  drop = TRUE
)

# 2. Fit unequal-variance sensitivity model
m_sensitivity <- nlme::lme(
  fixed = main_formula,
  random = ~ 1 | area/stand_nested,
  weights = nlme::varIdent(
    form = ~ 1 | species_region
  ),
  data = df_sensitivity,
  method = "REML",
  na.action = na.fail,
  control = nlme::lmeControl(
    maxIter = 200,
    msMaxIter = 200,
    niterEM = 50,
    opt = "optim"
  )
)

print(summary(m_sensitivity))

# 3. Normalized residuals
res <- residuals(
  m_sensitivity,
  type = "normalized"
)

fit <- fitted(
  m_sensitivity,
  level = 2
)

# Residual skewness
skewness_value <- mean(
  (res - mean(res))^3
) / mean(
  (res - mean(res))^2
)^(3/2)

cat("Normalized residual skewness:",
    skewness_value, "\n")

# 4. Create output folder
out_dir <- file.path(
  getwd(),
  "sensitivity_diagnostics"
)

dir.create(
  out_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

# 5. Main diagnostic plots
pdf(
  file.path(out_dir, "sensitivity_diagnostics.pdf"),
  width = 11,
  height = 9
)

par(mfrow = c(2, 2))

hist(
  res,
  breaks = 35,
  main = "Normalized residual distribution",
  xlab = "Normalized residuals"
)

qqnorm(
  res,
  main = "Normal Q-Q plot"
)
qqline(res, col = "red", lwd = 2)

plot(
  fit, res,
  xlab = "Fitted browsing damage",
  ylab = "Normalized residuals",
  main = "Residuals vs fitted"
)
abline(h = 0, col = "red", lty = 2)

plot(
  fit,
  sqrt(abs(res)),
  xlab = "Fitted browsing damage",
  ylab = "Sqrt(abs normalized residuals)",
  main = "Scale-location"
)

dev.off()

# 6. Residuals by species and area
diag_df <- data.frame(
  residual = as.numeric(res),
  fitted = as.numeric(fit),
  species = df_sensitivity$species,
  area = df_sensitivity$area,
  species_region = df_sensitivity$species_region
)

pdf(
  file.path(out_dir, "residuals_by_group.pdf"),
  width = 11,
  height = 9
)

par(mfrow = c(2, 1))

boxplot(
  residual ~ species,
  data = diag_df,
  main = "Normalized residuals by species",
  ylab = "Normalized residuals"
)
abline(h = 0, col = "red", lty = 2)

boxplot(
  residual ~ area,
  data = diag_df,
  main = "Normalized residuals by area",
  ylab = "Normalized residuals",
  las = 2
)
abline(h = 0, col = "red", lty = 2)

dev.off()

# 7. Check normalized residual SD by group
print(
  aggregate(
    residual ~ species_region,
    data = diag_df,
    FUN = sd
  )
)

# 8. Save model and data for statistician
saveRDS(
  m_sensitivity,
  file.path(out_dir, "sensitivity_model.rds")
)

saveRDS(
  df_sensitivity,
  file.path(out_dir, "sensitivity_model_data.rds")
)

write.csv(
  df_sensitivity,
  file.path(out_dir, "sensitivity_model_data.csv"),
  row.names = FALSE
)

capture.output(
  summary(m_sensitivity),
  paste("Residual skewness:", skewness_value),
  file = file.path(out_dir, "model_summary.txt")
)

capture.output(
  sessionInfo(),
  file = file.path(out_dir, "session_info.txt")
)

# 9. Show saved location
cat("\nAll results saved in:\n")
cat(normalizePath(out_dir), "\n")

# Save the cleaned stand-level data as an R file
saveRDS(
  stand_data,
  "C:/Users/shge0002/Downloads/stand_data.rds"
)

# Also save as a CSV file
write.csv(
  stand_data,
  "C:/Users/shge0002/Downloads/stand_data.csv",
  row.names = FALSE
)



library(lme4)

# =====================================================
# 1. CHECK DATA
# =====================================================

stopifnot(exists("plot_data"))
stopifnot(exists("stand_data"))

# Variables expected in the stand data
stand_vars <- c(
  "area", "stand_id", "year", "species",
  "prop_s_stand", "mean_north_c",
  "total_birch_c", "height_c",
  "total_cervid_pellets_log_mean", "pct"
)

stopifnot(all(stand_vars %in% names(stand_data)))

# Find the plot identifier
plot_id_col <- if ("plot_id" %in% names(plot_data)) {
  "plot_id"
} else if ("plot" %in% names(plot_data)) {
  "plot"
} else {
  stop("No plot_id or plot column found in plot_data")
}

# =====================================================
# 2. ADD STAND PREDICTORS TO PLOT DATA
# =====================================================

# Use year, area, stand and species as matching keys
keys <- c("year", "area", "stand_id", "species")

# Check that each stand-species key is unique
if (anyDuplicated(stand_data[keys]) > 0) {
  stop("Duplicate stand-species keys in stand_data")
}

# Convert matching keys to character
for (v in keys) {
  plot_data[[v]] <- as.character(plot_data[[v]])
  stand_data[[v]] <- as.character(stand_data[[v]])
}

# Match stand data to plot observations
stand_key <- do.call(
  paste,
  c(stand_data[keys], sep = "___")
)

plot_key <- do.call(
  paste,
  c(plot_data[keys], sep = "___")
)

match_index <- match(plot_key, stand_key)

# Check matching
cat("Unmatched plot rows:",
    sum(is.na(match_index)), "\n")

# Attach stand-level predictors
predictors <- setdiff(stand_vars, keys)

plot_model <- plot_data

for (v in predictors) {
  plot_model[[v]] <- stand_data[[v]][match_index]
}

# =====================================================
# 3. PREPARE MODEL DATA
# =====================================================

plot_model$plot_id <- plot_model[[plot_id_col]]

plot_model$species <- factor(plot_model$species)
plot_model$area <- factor(plot_model$area)
plot_model$stand_id <- factor(plot_model$stand_id)
plot_model$plot_id <- factor(plot_model$plot_id)
plot_model$pct <- factor(plot_model$pct)
plot_model$year <- factor(plot_model$year)

plot_model$species <- relevel(
  plot_model$species,
  ref = "Downy"
)

# Original ecological predictors
plot_formula <- damage_N ~ species * (
  prop_s_stand +
    mean_north_c +
    total_birch_c +
    height_c +
    total_cervid_pellets_log_mean
) +
  pct + year +
  (1 | area/stand_id/plot_id)

# Keep complete observations
model_vars <- all.vars(plot_formula)

plot_model <- droplevels(
  plot_model[
    complete.cases(plot_model[, model_vars]),
    ,
    drop = FALSE
  ]
)

# =====================================================
# 4. CHECK SAMPLE STRUCTURE
# =====================================================

cat("Plot-species observations:",
    nrow(plot_model), "\n")

cat("Study areas:",
    length(unique(plot_model$area)), "\n")

cat("Stands:",
    nrow(unique(plot_model[
      c("area", "stand_id")
    ])), "\n")

cat("Physical plots:",
    nrow(unique(plot_model[
      c("area", "stand_id", "plot_id")
    ])), "\n")

# Check no repeated species-plot records
duplicate_keys <- c(
  "year", "area", "stand_id",
  "plot_id", "species"
)

if (anyDuplicated(plot_model[duplicate_keys]) > 0) {
  stop(
    "Duplicate species-plot records found. ",
    "Check the plot data before modelling."
  )
}

# =====================================================
# 5. FIT NESTED PLOT-LEVEL MODEL
# =====================================================

m_plot_nested <- lmer(
  plot_formula,
  data = plot_model,
  REML = TRUE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 200000)
  )
)

summary(m_plot_nested)

# Random-effect variances
VarCorr(m_plot_nested)

# Check singularity and convergence
isSingular(m_plot_nested)
m_plot_nested@optinfo$conv

# =====================================================
# 6. RESIDUAL DIAGNOSTICS
# =====================================================

res <- residuals(m_plot_nested)
fit <- fitted(m_plot_nested)

pdf(
  "nested_plot_model_diagnostics.pdf",
  width = 11,
  height = 9
)

par(mfrow = c(2, 2))

hist(
  res,
  breaks = 40,
  main = "Residual distribution",
  xlab = "Residuals"
)

qqnorm(res, main = "Normal Q-Q plot")
qqline(res, col = "red", lwd = 2)

plot(
  fit, res,
  main = "Residuals vs fitted",
  xlab = "Fitted browsing damage",
  ylab = "Residuals"
)
abline(h = 0, col = "red", lty = 2)

plot(
  fit,
  sqrt(abs(res / sigma(m_plot_nested))),
  main = "Scale-location",
  xlab = "Fitted browsing damage",
  ylab = "Sqrt(abs standardized residuals)"
)

dev.off()

# =====================================================
# 7. SAVE MODEL AND DATA
# =====================================================

saveRDS(
  m_plot_nested,
  "nested_plot_model.rds"
)

saveRDS(
  plot_model,
  "nested_plot_model_data.rds"
)

capture.output(
  summary(m_plot_nested),
  VarCorr(m_plot_nested),
  isSingular(m_plot_nested),
  m_plot_nested@optinfo$conv,
  file = "nested_plot_model_summary.txt"
)

cat("\nPlot-level model completed.\n")
cat("Files saved in:", getwd(), "\n")



# Load the mixed-model packages
library(lme4)
library(lmerTest)

# Fit the plot model using your existing objects
m_plot_nested <- lmer(
  plot_formula,
  data = plot_model,
  REML = TRUE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 200000)
  )
)

summary(m_plot_nested)


# ============================================================
# CHECK SKEWNESS AFTER C2 SENSITIVITY ANALYSIS
# ============================================================

library(nlme)
library(ggplot2)

# Check C2 model exists
stopifnot(exists("m_sensitivity"))

# C2 normalized residuals
res_c2 <- residuals(
  m_sensitivity,
  type = "normalized"
)

# Skewness function
skewness_calc <- function(x) {
  x <- x[is.finite(x)]
  mean((x - mean(x))^3) /
    sd(x)^3
}

# Numerical summary
cat("\nC2 RESIDUAL SKEWNESS\n")
cat("-------------------------\n")
cat("Skewness:", skewness_calc(res_c2), "\n")
cat("Mean:", mean(res_c2), "\n")
cat("Median:", median(res_c2), "\n")
cat("SD:", sd(res_c2), "\n")
cat("Minimum:", min(res_c2), "\n")
cat("Maximum:", max(res_c2), "\n")

# Quantiles
print(quantile(
  res_c2,
  probs = c(0, .01, .05, .25, .5, .75, .95, .99, 1)
))

# ============================================================
# DIAGNOSTIC FIGURES
# ============================================================

df_diag <- data.frame(
  residual = as.numeric(res_c2),
  fitted = as.numeric(fitted(m_sensitivity))
)

p1 <- ggplot(df_diag, aes(x = residual)) +
  geom_histogram(
    bins = 40,
    fill = "grey65",
    colour = "white"
  ) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(
    title = "C2: Normalized residual distribution",
    x = "Normalized residual",
    y = "Frequency"
  ) +
  theme_classic()

p2 <- ggplot(df_diag, aes(sample = residual)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  labs(
    title = "C2: Normal Q-Q plot"
  ) +
  theme_classic()

p3 <- ggplot(df_diag, aes(x = fitted, y = residual)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    colour = "red"
  ) +
  labs(
    title = "C2: Residuals vs fitted",
    x = "Fitted browsing damage",
    y = "Normalized residual"
  ) +
  theme_classic()

p4 <- ggplot(
  df_diag,
  aes(x = fitted, y = sqrt(abs(residual)))
) +
  geom_point(alpha = 0.4) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    colour = "red"
  ) +
  labs(
    title = "C2: Scale-location",
    x = "Fitted browsing damage",
    y = "Sqrt(abs(normalized residual))"
  ) +
  theme_classic()

print(p1)
print(p2)
print(p3)
print(p4)

# Save figures
ggsave("C2_residual_histogram.png", p1, width = 7, height = 5)
ggsave("C2_residual_QQ.png", p2, width = 7, height = 5)
ggsave("C2_residual_vs_fitted.png", p3, width = 7, height = 5)
ggsave("C2_scale_location.png", p4, width = 7, height = 5)

cat("\nC2 diagnostic plots saved.\n")

# ============================================================
# MAIN NATIONAL STAND MODEL — C2 SENSITIVITY ANALYSIS
# ============================================================

library(nlme)

# 1. Prepare stand-level data
df_c2 <- stand_data

# Define regions if not already present
if (!"region" %in% names(df_c2)) {
  df_c2$region <- NA_character_
  
  df_c2$region[df_c2$area %in% c(
    "Fredrika", "Lycksele", "Malå",
    "Nordmaling", "Råneå", "Sorsele"
  )] <- "North"
  
  df_c2$region[df_c2$area %in% c(
    "Furudal", "Ljusdal", "Lofsdalen"
  )] <- "Centre"
  
  df_c2$region[df_c2$area %in% c(
    "Barksätter", "ÖsterMalma",
    "Åtvidaberg", "Växjö"
  )] <- "South"
}

df_c2$species <- factor(df_c2$species)
df_c2$species <- relevel(df_c2$species, ref = "Downy")
df_c2$area <- factor(df_c2$area)
df_c2$stand_id <- factor(df_c2$stand_id)
df_c2$region <- factor(df_c2$region)
df_c2$pct <- factor(df_c2$pct)
df_c2$year <- factor(df_c2$year)

# Residual variance groups
df_c2$species_region <- interaction(
  df_c2$species,
  df_c2$region,
  drop = TRUE
)

# 2. Original main stand-level fixed effects
main_formula <- damage_N ~ species * (
  prop_s_stand +
    mean_north_c +
    total_birch_c +
    height_c +
    total_cervid_pellets_log_mean
) +
  pct + year

# Complete observations
required_vars <- unique(c(
  all.vars(main_formula),
  "area", "stand_id", "region", "species_region"
))

df_c2 <- droplevels(
  df_c2[
    complete.cases(df_c2[, required_vars]),
    ,
    drop = FALSE
  ]
)

cat("Stand-species observations:", nrow(df_c2), "\n")
cat("Study areas:", length(unique(df_c2$area)), "\n")

# 3. Fit original equal-variance model
m_original <- nlme::lme(
  fixed = main_formula,
  random = ~1 | area/stand_id,
  data = df_c2,
  method = "REML",
  control = nlme::lmeControl(
    maxIter = 200,
    msMaxIter = 200,
    opt = "optim"
  )
)

# 4. Fit C2: different variance by species × region
m_C2 <- nlme::lme(
  fixed = main_formula,
  random = ~1 | area/stand_id,
  weights = nlme::varIdent(
    form = ~1 | species_region
  ),
  data = df_c2,
  method = "REML",
  control = nlme::lmeControl(
    maxIter = 200,
    msMaxIter = 200,
    opt = "optim"
  )
)

# 5. Compare models
print(AIC(m_original, m_C2))
print(summary(m_C2))

# 6. Calculate residual skewness
skewness_calc <- function(x) {
  x <- x[is.finite(x)]
  z <- x - mean(x)
  mean(z^3) / mean(z^2)^1.5
}

res_original <- residuals(m_original, type = "normalized")
res_C2 <- residuals(m_C2, type = "normalized")

cat("\nRESIDUAL SKEWNESS\n")
cat("Original:", skewness_calc(res_original), "\n")
cat("C2:", skewness_calc(res_C2), "\n")

cat("\nC2 RESIDUAL SUMMARY\n")
print(summary(res_C2))

# 7. Diagnostic plots
pdf("MAIN_STAND_C2_diagnostics.pdf", width = 11, height = 9)

par(mfrow = c(2, 2))

hist(
  res_C2,
  breaks = 35,
  main = "C2: Normalized residuals",
  xlab = "Normalized residual"
)

qqnorm(res_C2, main = "C2: Normal Q-Q")
qqline(res_C2, col = "red", lwd = 2)

plot(
  fitted(m_C2, level = 2),
  res_C2,
  main = "C2: Residuals vs fitted",
  xlab = "Fitted browsing damage",
  ylab = "Normalized residual"
)
abline(h = 0, col = "red", lty = 2)

plot(
  fitted(m_C2, level = 2),
  sqrt(abs(res_C2)),
  main = "C2: Scale-location",
  xlab = "Fitted browsing damage",
  ylab = "Sqrt(abs(normalized residual))"
)

dev.off()

# 8. Save model
saveRDS(m_C2, "MAIN_STAND_C2_model.rds")

cat("\nAnalysis complete.\n")
cat("Files saved in:", getwd(), "\n")


# Conditional fitted values from original stand model
pred <- fitted(m_original, level = 2)

cat("Predictions below 0:", sum(pred < 0), "\n")
cat("Predictions above 88:", sum(pred > 88), "\n")

cat("Percentage outside bounds:",
    round(mean(pred < 0 | pred > 88) * 100, 2),
    "%\n")

summary(pred)

# Observed damage distribution
summary(df_c2$damage_N)

cat("Observations at zero:",
    sum(df_c2$damage_N == 0), "\n")

cat("Observations at 88:",
    sum(df_c2$damage_N == 88), "\n")
