#extra analysis

###############################################################################
# PLOT-LEVEL MIXTURE ANALYSES
# 1) Full composition gradient
# 2) Mixed plots only
# 3) Plot type: Downy only / Mixed / Silver only
###############################################################################

library(dplyr)
library(lme4)
library(lmerTest)
library(MuMIn)
library(sjPlot)
library(performance)
library(emmeans)

#=============================================================================
# 1) CREATE BIOLOGICALLY VALID DAMAGE DATA
#    Keep only rows where the focal species is actually present
#=============================================================================

birch_plot_damage_present <- birch_plot_long_damage %>%
  filter(
    !is.na(damage_N),
    !is.na(stem_count),
    stem_count > 0,
    !is.na(prop_s_plot),
    !is.na(species),
    !is.na(north_c),
    !is.na(stem_count_c),
    !is.na(height_c),
    !is.na(year),
    !is.na(area),
    !is.na(stand_id)
  ) %>%
  mutate(
    species = droplevels(factor(species, levels = c("Downy", "Silver"))),
    year = droplevels(as.factor(year)),
    area = factor(area),
    stand_id = factor(stand_id)
  )

# Check how many rows remain
cat("\n--- BIOLOGICALLY VALID DAMAGE DATA ---\n")
print(nrow(birch_plot_damage_present))
print(table(birch_plot_damage_present$species))
print(summary(birch_plot_damage_present$prop_s_plot))

#=============================================================================
# 2) CREATE MIXED-ONLY DATA
#    This removes pure downy and pure silver plots
#=============================================================================

birch_plot_damage_mixed_only <- birch_plot_damage_present %>%
  filter(
    prop_s_plot > 0,
    prop_s_plot < 1
  ) %>%
  droplevels()

cat("\n--- MIXED-ONLY DAMAGE DATA ---\n")
print(nrow(birch_plot_damage_mixed_only))
print(table(birch_plot_damage_mixed_only$species))
print(summary(birch_plot_damage_mixed_only$prop_s_plot))

#=============================================================================
# 3) CREATE PLOT TYPE VARIABLE
#    Downy only / Mixed / Silver only
#=============================================================================

birch_plot_damage_present <- birch_plot_damage_present %>%
  mutate(
    plot_type = case_when(
      prop_s_plot == 0 ~ "Downy only",
      prop_s_plot == 1 ~ "Silver only",
      prop_s_plot > 0 & prop_s_plot < 1 ~ "Mixed",
      TRUE ~ NA_character_
    ),
    plot_type = factor(
      plot_type,
      levels = c("Downy only", "Mixed", "Silver only")
    )
  ) %>%
  filter(!is.na(plot_type)) %>%
  droplevels()

cat("\n--- PLOT TYPE CHECK ---\n")
print(table(birch_plot_damage_present$plot_type, birch_plot_damage_present$species))

#=============================================================================
# 4) MODEL 1: FULL COMPOSITION GRADIENT
#    Question: Does silver proportion affect browsing across all valid plots?
#=============================================================================

m_plot_prop_all <- lmer(
  damage_N ~ species * prop_s_plot +
    species * north_c +
    species * stem_count_c +
    species * height_c +
    year +
    (1 | area/stand_id/plot_id),
  data = birch_plot_damage_present,
  REML = TRUE
)

summary(m_plot_prop_all)
r.squaredGLMM(m_plot_prop_all)
drop1(update(m_plot_prop_all, REML = FALSE), test = "Chisq")

#=============================================================================
# 5) MODEL 2: MIXED PLOTS ONLY
#    Question: Within mixed plots, does increasing silver proportion affect damage?
#=============================================================================

m_plot_prop_mixed_only <- lmer(
  damage_N ~ species * prop_s_plot +
    species * north_c +
    species * stem_count_c +
    species * height_c +
    year +
    (1 | area/stand_id/plot_id),
  data = birch_plot_damage_mixed_only,
  REML = TRUE
)

summary(m_plot_prop_mixed_only)
r.squaredGLMM(m_plot_prop_mixed_only)
drop1(update(m_plot_prop_mixed_only, REML = FALSE), test = "Chisq")

#=============================================================================
# 6) MODEL 3: PLOT TYPE MODEL
#    Question: Are pure downy, mixed, and pure silver plots browsed differently?
#=============================================================================

m_plot_type <- lmer(
  damage_N ~ species * plot_type +
    species * north_c +
    species * stem_count_c +
    species * height_c +
    year +
    (1 | area/stand_id/plot_id),
  data = birch_plot_damage_present,
  REML = TRUE
)

summary(m_plot_type)
r.squaredGLMM(m_plot_type)
drop1(update(m_plot_type, REML = FALSE), test = "Chisq")

#=============================================================================
# 7) PAIRWISE COMPARISONS FOR PLOT TYPE MODEL
#=============================================================================

emm_plot_type <- emmeans(m_plot_type, ~ species * plot_type)

emm_plot_type_summary <- summary(emm_plot_type)
emm_plot_type_pairs <- pairs(emm_plot_type)

print(emm_plot_type_summary)
print(emm_plot_type_pairs)

#=============================================================================
# 8) MODEL COMPARISON TABLE
#    Do NOT use anova() to compare full vs mixed-only models because datasets differ.
#    But HTML side-by-side summaries are fine.
#=============================================================================

tab_model(
  m_plot_prop_all,
  m_plot_prop_mixed_only,
  m_plot_type,
  dv.labels = c(
    "Proportion model: all valid plots",
    "Proportion model: mixed plots only",
    "Plot type model: pure vs mixed"
  ),
  show.se = TRUE,
  show.ci = TRUE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "plot_mixture_models_comparison.html")
)

#=============================================================================
# 9) OPTIONAL: EXPORT EMMEANS TABLES AS CSV
#=============================================================================

write.csv(
  as.data.frame(emm_plot_type_summary),
  file.path(tab_dir, "plot_type_emmeans.csv"),
  row.names = FALSE
)

write.csv(
  as.data.frame(emm_plot_type_pairs),
  file.path(tab_dir, "plot_type_pairwise_comparisons.csv"),
  row.names = FALSE
)

#=============================================================================
# 10) SAVE MODELS
#=============================================================================

saveRDS(
  m_plot_prop_all,
  file.path(mod_dir, "m_plot_prop_all_valid_species_present.rds")
)

saveRDS(
  m_plot_prop_mixed_only,
  file.path(mod_dir, "m_plot_prop_mixed_only.rds")
)

saveRDS(
  m_plot_type,
  file.path(mod_dir, "m_plot_type_pure_mixed.rds")
)

cat("\nDone.\n")
cat("HTML table saved to:\n")
cat(file.path(tab_dir, "plot_mixture_models_comparison.html"), "\n")

###############################################################################
# STAND-LEVEL MIXTURE ANALYSES
# 1) Full composition gradient
# 2) Mixed stands only
# 3) Stand type: Downy only / Mixed / Silver only
###############################################################################

library(dplyr)
library(lme4)
library(lmerTest)
library(MuMIn)
library(sjPlot)
library(performance)
library(emmeans)

#=============================================================================
# 1) CREATE BIOLOGICALLY VALID STAND DAMAGE DATA
#    Keep only rows where the focal species is actually present in the stand
#=============================================================================

birch_stand_damage_present <- birch_stand_long_damage %>%
  filter(
    !is.na(damage_N),
    !is.na(stem_count),
    stem_count > 0,
    !is.na(prop_s_stand),
    !is.na(species),
    !is.na(mean_north_c),
    !is.na(stem_count_c),
    !is.na(height_c),
    !is.na(year),
    !is.na(area)
  ) %>%
  mutate(
    species = droplevels(factor(species, levels = c("Downy", "Silver"))),
    year = droplevels(as.factor(year)),
    area = factor(area)
  )

cat("\n--- BIOLOGICALLY VALID STAND DATA ---\n")
print(nrow(birch_stand_damage_present))
print(table(birch_stand_damage_present$species))
print(summary(birch_stand_damage_present$prop_s_stand))

#=============================================================================
# 2) MIXED STANDS ONLY
#=============================================================================

birch_stand_damage_mixed_only <- birch_stand_damage_present %>%
  filter(
    prop_s_stand > 0,
    prop_s_stand < 1
  ) %>%
  droplevels()

cat("\n--- MIXED-ONLY STAND DATA ---\n")
print(nrow(birch_stand_damage_mixed_only))
print(table(birch_stand_damage_mixed_only$species))
print(summary(birch_stand_damage_mixed_only$prop_s_stand))

#=============================================================================
# 3) CREATE STAND TYPE VARIABLE
#=============================================================================

birch_stand_damage_present <- birch_stand_damage_present %>%
  mutate(
    stand_type = case_when(
      prop_s_stand == 0 ~ "Downy only",
      prop_s_stand == 1 ~ "Silver only",
      prop_s_stand > 0 & prop_s_stand < 1 ~ "Mixed",
      TRUE ~ NA_character_
    ),
    stand_type = factor(
      stand_type,
      levels = c("Downy only", "Mixed", "Silver only")
    )
  ) %>%
  filter(!is.na(stand_type)) %>%
  droplevels()

cat("\n--- STAND TYPE CHECK ---\n")
print(table(birch_stand_damage_present$stand_type, birch_stand_damage_present$species))

#=============================================================================
# 4) MODEL 1: FULL STAND COMPOSITION GRADIENT
#=============================================================================

m_stand_prop_all <- lmer(
  damage_N ~ species * prop_s_stand +
    species * mean_north_c +
    species * stem_count_c +
    species * height_c +
    year +
    (1 | area),
  data = birch_stand_damage_present,
  REML = TRUE
)

summary(m_stand_prop_all)
r.squaredGLMM(m_stand_prop_all)
drop1(update(m_stand_prop_all, REML = FALSE), test = "Chisq")

#=============================================================================
# 5) MODEL 2: MIXED STANDS ONLY
#=============================================================================

m_stand_prop_mixed_only <- lmer(
  damage_N ~ species * prop_s_stand +
    species * mean_north_c +
    species * stem_count_c +
    species * height_c +
    year +
    (1 | area),
  data = birch_stand_damage_mixed_only,
  REML = TRUE
)

summary(m_stand_prop_mixed_only)
r.squaredGLMM(m_stand_prop_mixed_only)
drop1(update(m_stand_prop_mixed_only, REML = FALSE), test = "Chisq")

#=============================================================================
# 6) MODEL 3: STAND TYPE MODEL
#=============================================================================

m_stand_type <- lmer(
  damage_N ~ species * stand_type +
    species * mean_north_c +
    species * stem_count_c +
    species * height_c +
    year +
    (1 | area),
  data = birch_stand_damage_present,
  REML = TRUE
)

summary(m_stand_type)
r.squaredGLMM(m_stand_type)
drop1(update(m_stand_type, REML = FALSE), test = "Chisq")

#=============================================================================
# 7) PAIRWISE COMPARISONS FOR STAND TYPE MODEL
#=============================================================================

emm_stand_type <- emmeans(m_stand_type, ~ species * stand_type)

emm_stand_type_summary <- summary(emm_stand_type)
emm_stand_type_pairs <- pairs(emm_stand_type)

print(emm_stand_type_summary)
print(emm_stand_type_pairs)

#=============================================================================
# 8) HTML MODEL TABLE
#=============================================================================

tab_model(
  m_stand_prop_all,
  m_stand_prop_mixed_only,
  m_stand_type,
  dv.labels = c(
    "Stand proportion model: all valid stands",
    "Stand proportion model: mixed stands only",
    "Stand type model: pure vs mixed"
  ),
  show.se = TRUE,
  show.ci = TRUE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "stand_mixture_models_comparison.html")
)

#=============================================================================
# 9) EXPORT EMMEANS TABLES
#=============================================================================

write.csv(
  as.data.frame(emm_stand_type_summary),
  file.path(tab_dir, "stand_type_emmeans.csv"),
  row.names = FALSE
)

write.csv(
  as.data.frame(emm_stand_type_pairs),
  file.path(tab_dir, "stand_type_pairwise_comparisons.csv"),
  row.names = FALSE
)

#=============================================================================
# 10) SAVE MODELS
#=============================================================================

saveRDS(
  m_stand_prop_all,
  file.path(mod_dir, "m_stand_prop_all_valid_species_present.rds")
)

saveRDS(
  m_stand_prop_mixed_only,
  file.path(mod_dir, "m_stand_prop_mixed_only.rds")
)

saveRDS(
  m_stand_type,
  file.path(mod_dir, "m_stand_type_pure_mixed.rds")
)

cat("\nDone.\n")
cat("HTML table saved to:\n")
cat(file.path(tab_dir, "stand_mixture_models_comparison.html"), "\n")

###############################################################################
# SPECIES-SPECIFIC PURE VS MIXED MODELS
# Question:
# 1) Is downy birch more damaged in mixed plots than in pure downy plots?
# 2) Is silver birch more damaged in mixed plots than in pure silver plots?
###############################################################################

library(dplyr)
library(lme4)
library(lmerTest)
library(MuMIn)
library(sjPlot)
library(emmeans)
library(writexl)

#=============================================================================
# 1) CREATE SPECIES-SPECIFIC DATASETS
#=============================================================================

downy_pure_mixed <- birch_plot_damage_present %>%
  filter(
    species == "Downy",
    plot_type %in% c("Downy only", "Mixed")
  ) %>%
  mutate(
    plot_type_downy = factor(
      plot_type,
      levels = c("Downy only", "Mixed")
    )
  ) %>%
  droplevels()

silver_pure_mixed <- birch_plot_damage_present %>%
  filter(
    species == "Silver",
    plot_type %in% c("Mixed", "Silver only")
  ) %>%
  mutate(
    plot_type_silver = factor(
      plot_type,
      levels = c("Silver only", "Mixed")
    )
  ) %>%
  droplevels()

cat("\n--- DOWNY PURE VS MIXED ---\n")
print(table(downy_pure_mixed$plot_type_downy))
print(summary(downy_pure_mixed$damage_N))

cat("\n--- SILVER PURE VS MIXED ---\n")
print(table(silver_pure_mixed$plot_type_silver))
print(summary(silver_pure_mixed$damage_N))

#=============================================================================
# 2) DOWNY MODEL
#=============================================================================

m_downy_pure_mixed <- lmer(
  damage_N ~ plot_type_downy +
    north_c +
    stem_count_c +
    height_c +
    year +
    (1 | area/stand_id/plot_id),
  data = downy_pure_mixed,
  REML = TRUE
)

summary(m_downy_pure_mixed)
r.squaredGLMM(m_downy_pure_mixed)
drop1(update(m_downy_pure_mixed, REML = FALSE), test = "Chisq")

#=============================================================================
# 3) SILVER MODEL
#=============================================================================

m_silver_pure_mixed <- lmer(
  damage_N ~ plot_type_silver +
    north_c +
    stem_count_c +
    height_c +
    year +
    (1 | area/stand_id/plot_id),
  data = silver_pure_mixed,
  REML = TRUE
)

summary(m_silver_pure_mixed)
r.squaredGLMM(m_silver_pure_mixed)
drop1(update(m_silver_pure_mixed, REML = FALSE), test = "Chisq")

#=============================================================================
# 4) ESTIMATED MEANS
#=============================================================================

emm_downy_pure_mixed <- emmeans(
  m_downy_pure_mixed,
  ~ plot_type_downy
)

emm_silver_pure_mixed <- emmeans(
  m_silver_pure_mixed,
  ~ plot_type_silver
)

downy_emm <- summary(emm_downy_pure_mixed)
silver_emm <- summary(emm_silver_pure_mixed)

downy_pairs <- pairs(emm_downy_pure_mixed)
silver_pairs <- pairs(emm_silver_pure_mixed)

cat("\n--- DOWNY ESTIMATED MEANS ---\n")
print(downy_emm)

cat("\n--- DOWNY PURE VS MIXED CONTRAST ---\n")
print(downy_pairs)

cat("\n--- SILVER ESTIMATED MEANS ---\n")
print(silver_emm)

cat("\n--- SILVER PURE VS MIXED CONTRAST ---\n")
print(silver_pairs)

#=============================================================================
# 5) HTML MODEL TABLE
#=============================================================================

tab_model(
  m_downy_pure_mixed,
  m_silver_pure_mixed,
  dv.labels = c(
    "Downy birch: pure downy vs mixed plots",
    "Silver birch: pure silver vs mixed plots"
  ),
  pred.labels = c(
    "(Intercept)",
    "Mixed plot",
    "Latitude, centred",
    "Stem count, centred",
    "Height, centred",
    "Year 2025"
  ),
  show.se = TRUE,
  show.ci = TRUE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "plot_species_specific_pure_vs_mixed_models.html")
)

#=============================================================================
# 6) EXPORT EMMEANS + CONTRASTS
#=============================================================================

write.csv(
  as.data.frame(downy_emm),
  file.path(tab_dir, "downy_pure_vs_mixed_emmeans.csv"),
  row.names = FALSE
)

write.csv(
  as.data.frame(downy_pairs),
  file.path(tab_dir, "downy_pure_vs_mixed_contrast.csv"),
  row.names = FALSE
)

write.csv(
  as.data.frame(silver_emm),
  file.path(tab_dir, "silver_pure_vs_mixed_emmeans.csv"),
  row.names = FALSE
)

write.csv(
  as.data.frame(silver_pairs),
  file.path(tab_dir, "silver_pure_vs_mixed_contrast.csv"),
  row.names = FALSE
)

write_xlsx(
  list(
    "Downy_emmeans" = as.data.frame(downy_emm),
    "Downy_contrast" = as.data.frame(downy_pairs),
    "Silver_emmeans" = as.data.frame(silver_emm),
    "Silver_contrast" = as.data.frame(silver_pairs)
  ),
  file.path(tab_dir, "plot_species_specific_pure_vs_mixed_emmeans.xlsx")
)

#=============================================================================
# 7) SAVE MODELS
#=============================================================================

saveRDS(
  m_downy_pure_mixed,
  file.path(mod_dir, "m_downy_plot_pure_vs_mixed.rds")
)

saveRDS(
  m_silver_pure_mixed,
  file.path(mod_dir, "m_silver_plot_pure_vs_mixed.rds")
)

cat("\nDone.\n")
cat("HTML table saved to:\n")
cat(file.path(tab_dir, "plot_species_specific_pure_vs_mixed_models.html"), "\n")
cat("Emmeans Excel file saved to:\n")
cat(file.path(tab_dir, "plot_species_specific_pure_vs_mixed_emmeans.xlsx"), "\n")

