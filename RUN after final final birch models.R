# To be run after finaöfinalbirch models
m_plot_nested <- lmer(
  damage_N ~ species * prop_s_plot +
    species * north_c +
    species * stem_count_c +
    species * height_c +
    year +
    (1 | area/stand_id/plot),
  data = birch_plot_long_damage
)
anova(m_plot_height, m_plot_nested)
AIC(m_plot_height, m_plot_nested)
BIC(m_plot_height, m_plot_nested)
summary(m_plot_nested)

VarCorr(m_plot_nested)
###############################################################################
# NESTED, ADDITIVE, SPECIES-SPECIFIC, AND REGIONAL MODEL COMPARISONS
###############################################################################

library(dplyr)
library(lme4)
library(lmerTest)
library(MuMIn)
library(performance)
library(writexl)

model_dir <- "C:/Users/shge0002/Documents/R/R/ÄBIN2025raw/2025-BINdata_raw/models"
tab_dir   <- "C:/Users/shge0002/Documents/R/R/ÄBIN2025raw/2025-BINdata_raw/tables"

dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

###############################################################################
# 1) NESTED RANDOM EFFECT MODELS
###############################################################################

m_plot_nested <- lmer(
  damage_N ~ species * prop_s_plot +
    species * north_c +
    species * stem_count_c +
    species * height_c +
    year +
    (1 | area/stand_id/plot_id),
  data = birch_plot_long_damage,
  REML = TRUE
)

m_region_nested <- lmer(
  damage_N ~ species * prop_s_plot * region +
    species * height_c +
    species * stem_count_c +
    year +
    (1 | area/stand_id/plot_id),
  data = df_region_plot,
  REML = TRUE
)

###############################################################################
# 2) ADDITIVE MODELS — NO INTERACTIONS
###############################################################################

m_plot_additive <- lmer(
  damage_N ~ species +
    prop_s_plot +
    north_c +
    stem_count_c +
    height_c +
    year +
    (1 | area/stand_id/plot_id),
  data = birch_plot_long_damage,
  REML = TRUE
)

m_region_additive <- lmer(
  damage_N ~ species +
    prop_s_plot +
    region +
    height_c +
    stem_count_c +
    year +
    (1 | area/stand_id/plot_id),
  data = df_region_plot,
  REML = TRUE
)

###############################################################################
# 3) SPECIES-SPECIFIC MODELS
###############################################################################

df_downy <- birch_plot_long_damage %>%
  filter(species == "Downy") %>%
  droplevels()

df_silver <- birch_plot_long_damage %>%
  filter(species == "Silver") %>%
  droplevels()

m_downy_nested <- lmer(
  damage_N ~ prop_s_plot +
    north_c +
    stem_count_c +
    height_c +
    year +
    (1 | area/stand_id/plot_id),
  data = df_downy,
  REML = TRUE
)

m_silver_nested <- lmer(
  damage_N ~ prop_s_plot +
    north_c +
    stem_count_c +
    height_c +
    year +
    (1 | area/stand_id/plot_id),
  data = df_silver,
  REML = TRUE
)

###############################################################################
# 4) SEPARATE REGIONAL MODELS
###############################################################################

df_north  <- df_region_plot %>% filter(region == "North") %>% droplevels()
df_centre <- df_region_plot %>% filter(region == "Centre") %>% droplevels()
df_south  <- df_region_plot %>% filter(region == "South") %>% droplevels()

m_north_nested <- lmer(
  damage_N ~ species * prop_s_plot +
    species * height_c +
    species * stem_count_c +
    species * moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id/plot_id),
  data = df_north,
  REML = TRUE
)

m_centre_nested <- lmer(
  damage_N ~ species * prop_s_plot +
    species * height_c +
    species * stem_count_c +
    species * moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id/plot_id),
  data = df_centre,
  REML = TRUE
)

m_south_nested <- lmer(
  damage_N ~ species * prop_s_plot +
    species * height_c +
    species * stem_count_c +
    species * moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id/plot_id),
  data = df_south,
  REML = TRUE
)

###############################################################################
# 5) MODEL COMPARISON HELPER
###############################################################################

model_summary_row <- function(model, model_name) {
  r2 <- MuMIn::r.squaredGLMM(model)
  icc_val <- tryCatch(performance::icc(model)$ICC_adjusted, error = function(e) NA_real_)
  
  data.frame(
    model = model_name,
    n_parameters = attr(logLik(model), "df"),
    AIC = AIC(model),
    BIC = BIC(model),
    logLik = as.numeric(logLik(model)),
    R2_marginal = r2[1, "R2m"],
    R2_conditional = r2[1, "R2c"],
    ICC_adjusted = icc_val
  )
}

model_comparison_table <- bind_rows(
  model_summary_row(m_plot_height, "Original plot model"),
  model_summary_row(m_plot_nested, "Nested plot model"),
  model_summary_row(m_plot_additive, "Nested additive plot model"),
  model_summary_row(m_region_plot_height, "Original regional model"),
  model_summary_row(m_region_nested, "Nested regional model"),
  model_summary_row(m_region_additive, "Nested additive regional model"),
  model_summary_row(m_downy_nested, "Downy-only nested model"),
  model_summary_row(m_silver_nested, "Silver-only nested model"),
  model_summary_row(m_north_nested, "North-only nested model"),
  model_summary_row(m_centre_nested, "Centre-only nested model"),
  model_summary_row(m_south_nested, "South-only nested model")
)

print(model_comparison_table)

###############################################################################
# 6) LIKELIHOOD RATIO TESTS
###############################################################################

lrt_random_plot <- anova(
  update(m_plot_height, REML = FALSE),
  update(m_plot_nested, REML = FALSE)
)

lrt_interaction_plot <- anova(
  update(m_plot_additive, REML = FALSE),
  update(m_plot_nested, REML = FALSE)
)

lrt_random_region <- anova(
  update(m_region_plot_height, REML = FALSE),
  update(m_region_nested, REML = FALSE)
)

lrt_interaction_region <- anova(
  update(m_region_additive, REML = FALSE),
  update(m_region_nested, REML = FALSE)
)

print(lrt_random_plot)
print(lrt_interaction_plot)
print(lrt_random_region)
print(lrt_interaction_region)

###############################################################################
# 7) VARIANCE COMPONENTS
###############################################################################

varcorr_plot_nested <- as.data.frame(VarCorr(m_plot_nested))
varcorr_region_nested <- as.data.frame(VarCorr(m_region_nested))

print(varcorr_plot_nested)
print(varcorr_region_nested)

###############################################################################
# 8) SAVE MODELS
###############################################################################

saveRDS(m_plot_nested, file.path(model_dir, "m_plot_nested.rds"))
saveRDS(m_plot_additive, file.path(model_dir, "m_plot_additive_nested.rds"))
saveRDS(m_region_nested, file.path(model_dir, "m_region_nested.rds"))
saveRDS(m_region_additive, file.path(model_dir, "m_region_additive_nested.rds"))

saveRDS(m_downy_nested, file.path(model_dir, "m_downy_nested.rds"))
saveRDS(m_silver_nested, file.path(model_dir, "m_silver_nested.rds"))

saveRDS(m_north_nested, file.path(model_dir, "m_north_nested.rds"))
saveRDS(m_centre_nested, file.path(model_dir, "m_centre_nested.rds"))
saveRDS(m_south_nested, file.path(model_dir, "m_south_nested.rds"))

###############################################################################
# 9) EXPORT TABLES
###############################################################################

write.csv(
  model_comparison_table,
  file.path(tab_dir, "model_comparison_nested_additive_species_region.csv"),
  row.names = FALSE
)

write.csv(
  as.data.frame(lrt_random_plot),
  file.path(tab_dir, "lrt_original_vs_nested_plot.csv")
)

write.csv(
  as.data.frame(lrt_interaction_plot),
  file.path(tab_dir, "lrt_additive_vs_interaction_plot.csv")
)

write.csv(
  as.data.frame(lrt_random_region),
  file.path(tab_dir, "lrt_original_vs_nested_region.csv")
)

write.csv(
  as.data.frame(lrt_interaction_region),
  file.path(tab_dir, "lrt_additive_vs_interaction_region.csv")
)

write.csv(
  varcorr_plot_nested,
  file.path(tab_dir, "variance_components_plot_nested.csv"),
  row.names = FALSE
)

write.csv(
  varcorr_region_nested,
  file.path(tab_dir, "variance_components_region_nested.csv"),
  row.names = FALSE
)

write_xlsx(
  list(
    "Model_comparison" = model_comparison_table,
    "LRT_random_plot" = as.data.frame(lrt_random_plot),
    "LRT_interaction_plot" = as.data.frame(lrt_interaction_plot),
    "LRT_random_region" = as.data.frame(lrt_random_region),
    "LRT_interaction_region" = as.data.frame(lrt_interaction_region),
    "VarCorr_plot_nested" = varcorr_plot_nested,
    "VarCorr_region_nested" = varcorr_region_nested
  ),
  file.path(tab_dir, "nested_model_comparison_tables.xlsx")
)

###############################################################################
# 10) PRINT KEY SUMMARIES
###############################################################################

summary(m_plot_nested)
summary(m_plot_additive)
summary(m_region_nested)
summary(m_region_additive)

summary(m_downy_nested)
summary(m_silver_nested)

summary(m_north_nested)
summary(m_centre_nested)
summary(m_south_nested)

cat("\nDone. Models saved to:\n", model_dir, "\n")
cat("Tables saved to:\n", tab_dir, "\n")

###############################################################################
# STAND-LEVEL REGIONAL MODELS — MATCHING YOUR CURRENT SCRIPT
###############################################################################

df_region_stand <- birch_stand_long_pel_damage %>%
  mutate(
    area = as.character(area),
    
    area = case_when(
      area %in% c("Fredrika", "Lögda", "Fredrika/Lögda") ~ "Fredrika/Lögda",
      TRUE ~ area
    ),
    
    region = case_when(
      area %in% c("ÖsterMalma", "Växjö", "Åtvidaberg", "Barksätter") ~ "South",
      area %in% c("Ljusdal", "Furudal", "Lofsdalen") ~ "Centre",
      area %in% c("Fredrika/Lögda", "Malå", "Lycksele", "Nordmaling", "Sorsele", "Råneå") ~ "North",
      TRUE ~ NA_character_
    ),
    
    # ADD THIS
    stand_id = interaction(area, stand_number, drop = TRUE),
    
    region = factor(region, levels = c("South", "Centre", "North")),
    pct = factor(pct),
    year = droplevels(as.factor(year)),
    species = droplevels(as.factor(species)),
    area = factor(area),
    stand_id = factor(stand_id)
  ) %>%
  filter(
    !is.na(damage_N),
    !is.na(species),
    !is.na(prop_s_stand),
    !is.na(region),
    !is.na(height_c),
    !is.na(stem_count_c),
    !is.na(pct),
    !is.na(year),
    !is.na(stand_id)
  )
###############################################################################
# CHECK STAND DATA
###############################################################################

table(df_region_stand$region, useNA = "ifany")
table(df_region_stand$region, df_region_stand$species, useNA = "ifany")
table(df_region_stand$year, useNA = "ifany")

###############################################################################
# 1. STAND ADDITIVE MODEL
###############################################################################

m_stand_additive <- lmer(
  damage_N ~ species +
    prop_s_stand +
    mean_north_c +
    stem_count_c +
    height_c +
    moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_region_stand,
  REML = TRUE
)

summary(m_stand_additive)
r.squaredGLMM(m_stand_additive)
drop1(update(m_stand_additive, REML = FALSE), test = "Chisq")
check_collinearity(m_stand_additive)

###############################################################################
# 2. STAND SPECIES INTERACTION MODEL
###############################################################################

m_stand_species_int <- lmer(
  damage_N ~ species * prop_s_stand +
    species * mean_north_c +
    species * stem_count_c +
    species * height_c +
    species * moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_region_stand,
  REML = TRUE
)

summary(m_stand_species_int)
r.squaredGLMM(m_stand_species_int)
drop1(update(m_stand_species_int, REML = FALSE), test = "Chisq")
check_collinearity(m_stand_species_int)

###############################################################################
# 3. STAND REGIONAL INTERACTION MODEL
###############################################################################

m_stand_region_int <- lmer(
  damage_N ~ species * prop_s_stand * region +
    species * height_c +
    species * stem_count_c +
    species * moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_region_stand,
  REML = TRUE
)

summary(m_stand_region_int)
r.squaredGLMM(m_stand_region_int)
drop1(update(m_stand_region_int, REML = FALSE), test = "Chisq")
check_collinearity(m_stand_region_int)

###############################################################################
# 4. SEPARATE STAND MODELS BY REGION
###############################################################################

df_stand_south  <- df_region_stand %>% filter(region == "South") %>% droplevels()
df_stand_centre <- df_region_stand %>% filter(region == "Centre") %>% droplevels()
df_stand_north  <- df_region_stand %>% filter(region == "North") %>% droplevels()

m_stand_south <- lmer(
  damage_N ~ species * prop_s_stand +
    species * height_c +
    species * stem_count_c +
    species * moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_south,
  REML = TRUE
)

m_stand_centre <- lmer(
  damage_N ~ species * prop_s_stand +
    species * height_c +
    species * stem_count_c +
    species * moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_centre,
  REML = TRUE
)

m_stand_north <- lmer(
  damage_N ~ species * prop_s_stand +
    species * height_c +
    species * stem_count_c +
    species * moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_north,
  REML = TRUE
)

summary(m_stand_south)
summary(m_stand_centre)
summary(m_stand_north)

drop1(update(m_stand_south, REML = FALSE), test = "Chisq")
drop1(update(m_stand_centre, REML = FALSE), test = "Chisq")
drop1(update(m_stand_north, REML = FALSE), test = "Chisq")

###############################################################################
# 5. COMPARE STAND MODELS
###############################################################################

anova(
  update(m_stand_additive, REML = FALSE),
  update(m_stand_species_int, REML = FALSE),
  update(m_stand_region_int, REML = FALSE)
)

AIC(
  update(m_stand_additive, REML = FALSE),
  update(m_stand_species_int, REML = FALSE),
  update(m_stand_region_int, REML = FALSE)
)

###############################################################################
# 6. TABLE
###############################################################################

tab_model(
  m_stand_additive,
  m_stand_species_int,
  m_stand_region_int,
  dv.labels = c(
    "Stand additive",
    "Stand species interaction",
    "Stand regional interaction"
  ),
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "stand_level_models.html")
)

###############################################################################
# SEPARATE HTML TABLES — STAND MODELS, WITHOUT CI
###############################################################################

library(sjPlot)

###############################################################################
# 1. Stand additive model
###############################################################################

tab_model(
  m_stand_additive,
  dv.labels = "Stand additive",
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_stand_additive.html")
)

###############################################################################
# 2. Stand species interaction model
###############################################################################

tab_model(
  m_stand_species_int,
  dv.labels = "Stand species interaction",
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_stand_species_interaction.html")
)

###############################################################################
# 3. Stand regional interaction model
###############################################################################

tab_model(
  m_stand_region_int,
  dv.labels = "Stand regional interaction",
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_stand_regional_interaction.html")
)
###############################################################################
# 7. STAND-LEVEL REGIONAL MIXTURE FIGURE
###############################################################################

pred_stand_region <- ggpredict(
  m_stand_region_int,
  terms = c("prop_s_stand [0:1 by=0.05]", "species", "region")
) %>%
  as.data.frame()

p_stand_region <- ggplot(
  pred_stand_region,
  aes(x = x, y = predicted, colour = group, fill = group)
) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    alpha = 0.18,
    colour = NA
  ) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~facet) +
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    labels = c("Pure\nDowny", "Mixed", "Pure\nSilver")
  ) +
  labs(
    title = "Stand-level birch mixture effect by region",
    x = "Stand-level silver birch proportion",
    y = "Predicted browsing severity"
  ) +
  theme_pub()

print(p_stand_region)

ggsave(
  file.path(fig_dir, "stand_level_regional_mixture_effect.png"),
  p_stand_region,
  width = 9,
  height = 5.5,
  dpi = 600
)

###############################################################################
# 8. SAVE MODELS
###############################################################################

saveRDS(m_stand_additive, file.path(mod_dir, "m_stand_additive.rds"))
saveRDS(m_stand_species_int, file.path(mod_dir, "m_stand_species_int.rds"))
saveRDS(m_stand_region_int, file.path(mod_dir, "m_stand_region_int.rds"))
saveRDS(m_stand_south, file.path(mod_dir, "m_stand_south.rds"))
saveRDS(m_stand_centre, file.path(mod_dir, "m_stand_centre.rds"))
saveRDS(m_stand_north, file.path(mod_dir, "m_stand_north.rds"))

summary(m_stand_region_int)

install.packages("emmeans")  
library(emmeans)

emtrends(
  m_stand_region_int,
  ~ species | region,
  var = "prop_s_stand"
)
summary(
  emtrends(
    m_stand_region_int,
    ~ species | region,
    var = "prop_s_stand"
  ),
  infer = TRUE
)
pairs(
  emtrends(
    m_stand_region_int,
    ~ species | region,
    var = "prop_s_stand"
  )
)


#Table and model outputs
tab_model(
  m_plot_additive,
  m_plot_nested,
  m_region_nested,
  
  m_stand_additive,
  m_stand_species_int,
  m_stand_region_int,
  
  m_north_height,
  m_centre_height,
  m_south_height,
  
  m_downy_nested,
  m_silver_nested,
  
  dv.labels = c(
    "Plot additive",
    "Plot interaction",
    "Plot regional",
    
    "Stand additive",
    "Stand interaction",
    "Stand regional",
    
    "North",
    "Centre",
    "South",
    
    "Downy only",
    "Silver only"
  ),
  
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  
  file = "Supplementary_Table_S1_All_Models.html"
)


#Graphs
emm_df <- data.frame(
  region = c("South","South","Centre","Centre","North","North"),
  species = c("Downy","Silver","Downy","Silver","Downy","Silver"),
  estimate = c(21.56,-8.16,1.95,3.23,6.81,3.36),
  lower = c(8.01,-18.84,-9.72,-9.21,-1.12,-4.89),
  upper = c(35.12,2.52,13.63,15.67,14.74,11.62)
)

ggplot(
  emm_df,
  aes(y = interaction(region,species),
      x = estimate,
      colour = species)
) +
  geom_point(size = 3) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2
  ) +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_colour_manual(values = pal) +
  labs(
    x = "Slope of silver birch proportion",
    y = NULL
  ) +
  theme_classic()


###############################################################################
# SUPPLEMENTARY MODEL TABLES — SEPARATED BY ANALYSIS TYPE
###############################################################################

library(sjPlot)
library(emmeans)
library(dplyr)
library(ggplot2)

###############################################################################
# 1. PLOT-LEVEL MODEL TABLE
###############################################################################

tab_model(
  m_plot_additive,
  m_plot_nested,
  m_region_nested,
  dv.labels = c(
    "Plot additive",
    "Plot species interaction",
    "Plot regional interaction"
  ),
  show.se = TRUE,
  show.ci = TRUE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_S1_plot_level_models.html")
)

###############################################################################
# 2. STAND-LEVEL MODEL TABLE
###############################################################################

tab_model(
  m_stand_additive,
  m_stand_species_int,
  m_stand_region_int,
  dv.labels = c(
    "Stand additive",
    "Stand species interaction",
    "Stand regional interaction"
  ),
  show.se = TRUE,
  show.ci = TRUE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_S2_stand_level_models.html")
)

###############################################################################
# 3. REGION-SPECIFIC MODEL TABLES
###############################################################################

tab_model(
  m_stand_south,
  m_stand_centre,
  m_stand_north,
  dv.labels = c(
    "South stand model",
    "Centre stand model",
    "North stand model"
  ),
  show.se = TRUE,
  show.ci = TRUE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_S3_region_specific_stand_models.html")
)

###############################################################################
# 4. SPECIES-SPECIFIC MODEL TABLES
###############################################################################

tab_model(
  m_downy_nested,
  m_silver_nested,
  dv.labels = c(
    "Downy only plot model",
    "Silver only plot model"
  ),
  show.se = TRUE,
  show.ci = TRUE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_S4_species_specific_models.html")
)

###############################################################################
# 5. EMMEANS / EMTRENDS TABLE — SLOPES
###############################################################################

stand_emtrends <- emtrends(
  m_stand_region_int,
  ~ species | region,
  var = "prop_s_stand"
)

stand_emtrends_table <- as.data.frame(
  summary(stand_emtrends, infer = TRUE)
) %>%
  rename(
    slope = prop_s_stand.trend,
    lower_CI = lower.CL,
    upper_CI = upper.CL,
    p_value = p.value
  ) %>%
  mutate(
    slope = round(slope, 2),
    SE = round(SE, 2),
    lower_CI = round(lower_CI, 2),
    upper_CI = round(upper_CI, 2),
    t.ratio = round(t.ratio, 2),
    p_value = signif(p_value, 3)
  )

write.csv(
  stand_emtrends_table,
  file.path(tab_dir, "Table_S5_stand_emtrends_slopes.csv"),
  row.names = FALSE
)

print(stand_emtrends_table)

###############################################################################
# 6. EMMEANS / EMTRENDS PAIRWISE COMPARISONS
###############################################################################

stand_emtrends_pairs <- as.data.frame(
  pairs(stand_emtrends)
) %>%
  mutate(
    estimate = round(estimate, 2),
    SE = round(SE, 2),
    t.ratio = round(t.ratio, 2),
    p.value = signif(p.value, 3)
  )

write.csv(
  stand_emtrends_pairs,
  file.path(tab_dir, "Table_S6_stand_emtrends_pairwise_comparisons.csv"),
  row.names = FALSE
)

print(stand_emtrends_pairs)

###############################################################################
# 7. OPTIONAL: COMBINED EXCEL-FRIENDLY TABLES
###############################################################################

write.csv(
  stand_emtrends_table,
  file.path(tab_dir, "emtrends_slopes_for_paper.csv"),
  row.names = FALSE
)

write.csv(
  stand_emtrends_pairs,
  file.path(tab_dir, "emtrends_species_comparisons_for_paper.csv"),
  row.names = FALSE
)

###############################################################################
# 8. EMTRENDS FOREST PLOT
###############################################################################

emm_plot_df <- stand_emtrends_table %>%
  mutate(
    region = factor(region, levels = c("South", "Centre", "North")),
    species = factor(species, levels = c("Downy", "Silver")),
    label = paste(region, species, sep = " — ")
  )

p_emtrends_forest <- ggplot(
  emm_plot_df,
  aes(
    x = slope,
    y = reorder(label, slope),
    colour = species
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  geom_errorbarh(
    aes(xmin = lower_CI, xmax = upper_CI),
    height = 0.2,
    linewidth = 0.8
  ) +
  geom_point(size = 3) +
  scale_colour_manual(values = pal) +
  labs(
    title = "Estimated effect of stand-level silver birch proportion",
    subtitle = "Slopes from emtrends with 95% confidence intervals",
    x = "Slope of silver birch proportion",
    y = NULL,
    colour = "Species"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 11)
  )

print(p_emtrends_forest)

ggsave(
  file.path(fig_dir, "Figure_S1_emtrends_forest_plot.png"),
  p_emtrends_forest,
  width = 9,
  height = 5.5,
  dpi = 600
)

###############################################################################
# HTML MODEL OUTPUTS — SEPARATE SUPPLEMENTARY TABLES
###############################################################################

library(sjPlot)

###############################################################################
# Table S1 — Plot-level models
###############################################################################

tab_model(
  m_plot_additive,
  m_plot_nested,
  m_region_nested,
  dv.labels = c(
    "Plot additive",
    "Plot species interaction",
    "Plot regional interaction"
  ),
  show.se = TRUE,
  show.ci = TRUE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_S1_plot_level_models.html")
)

###############################################################################
# Table S2 — Region-specific plot models
###############################################################################

tab_model(
  m_south_height,
  m_centre_height,
  m_north_height,
  dv.labels = c(
    "South plot model",
    "Centre plot model",
    "North plot model"
  ),
  show.se = TRUE,
  show.ci = TRUE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_S2_region_specific_plot_models.html")
)

###############################################################################
# Table S3 — Stand-level models
###############################################################################

tab_model(
  m_stand_additive,
  m_stand_species_int,
  m_stand_region_int,
  dv.labels = c(
    "Stand additive",
    "Stand species interaction",
    "Stand regional interaction"
  ),
  show.se = TRUE,
  show.ci = TRUE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_S3_stand_level_models.html")
)

###############################################################################
# Table S4 — Region-specific stand models
###############################################################################

tab_model(
  m_stand_south,
  m_stand_centre,
  m_stand_north,
  dv.labels = c(
    "South stand model",
    "Centre stand model",
    "North stand model"
  ),
  show.se = TRUE,
  show.ci = TRUE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_S4_region_specific_stand_models.html")
)

###############################################################################
# Table S5 — emtrends slopes
###############################################################################

library(emmeans)
library(dplyr)

stand_emtrends <- emtrends(
  m_stand_region_int,
  ~ species | region,
  var = "prop_s_stand"
)

stand_emtrends_table <- as.data.frame(
  summary(stand_emtrends, infer = TRUE)
) %>%
  rename(
    slope = prop_s_stand.trend,
    lower_CI = lower.CL,
    upper_CI = upper.CL,
    p_value = p.value
  )

write.csv(
  stand_emtrends_table,
  file.path(tab_dir, "Table_S5_stand_emtrends_slopes.csv"),
  row.names = FALSE
)

###############################################################################
# Table S6 — emtrends pairwise comparisons
###############################################################################

stand_emtrends_pairs <- as.data.frame(
  pairs(stand_emtrends)
)

write.csv(
  stand_emtrends_pairs,
  file.path(tab_dir, "Table_S6_stand_emtrends_pairwise_comparisons.csv"),
  row.names = FALSE
)

print(stand_emtrends_table)
print(stand_emtrends_pairs)

###############################################################################
# STAND-LEVEL SPECIES-SPECIFIC MODELS
# S7 = Downy birch only
# S8 = Silver birch only
###############################################################################

library(dplyr)
library(lme4)
library(lmerTest)
library(MuMIn)
library(performance)
library(sjPlot)

###############################################################################
# 1. CHECK / CREATE stand_id
###############################################################################

# Check column names
names(df_region_stand)

# If stand_id is missing, create it from area + stand_number
df_region_stand <- df_region_stand %>%
  mutate(
    stand_id = if ("stand_id" %in% names(.)) {
      stand_id
    } else {
      interaction(area, stand_number, drop = TRUE)
    },
    area = factor(area),
    stand_id = factor(stand_id),
    species = droplevels(factor(species)),
    year = droplevels(factor(year)),
    pct = factor(pct)
  )

###############################################################################
# 2. CREATE SPECIES-SPECIFIC DATASETS
###############################################################################

df_stand_downy <- df_region_stand %>%
  filter(species == "Downy") %>%
  droplevels()

df_stand_silver <- df_region_stand %>%
  filter(species == "Silver") %>%
  droplevels()

###############################################################################
# 3. FIT SPECIES-SPECIFIC STAND MODELS
###############################################################################

# Model S7 – Downy birch only
m_stand_downy <- lmer(
  damage_N ~ prop_s_stand +
    mean_north_c +
    stem_count_c +
    height_c +
    moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_downy,
  REML = TRUE
)

# Model S8 – Silver birch only
m_stand_silver <- lmer(
  damage_N ~ prop_s_stand +
    mean_north_c +
    stem_count_c +
    height_c +
    moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_silver,
  REML = TRUE
)

###############################################################################
# 4. MODEL SUMMARIES
###############################################################################

summary(m_stand_downy)
summary(m_stand_silver)

###############################################################################
# 5. MODEL R2 VALUES
###############################################################################

r.squaredGLMM(m_stand_downy)
r.squaredGLMM(m_stand_silver)

###############################################################################
# 6. TERM TESTS
###############################################################################

drop1(update(m_stand_downy, REML = FALSE), test = "Chisq")
drop1(update(m_stand_silver, REML = FALSE), test = "Chisq")

###############################################################################
# 7. COLLINEARITY CHECKS
###############################################################################

check_collinearity(m_stand_downy)
check_collinearity(m_stand_silver)

###############################################################################
# 8. HTML OUTPUT TABLE
###############################################################################

tab_model(
  m_stand_downy,
  m_stand_silver,
  dv.labels = c(
    "S7 Stand Downy only",
    "S8 Stand Silver only"
  ),
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_S5_species_specific_stand_models.html")
)

###############################################################################
# 9. SAVE MODEL OBJECTS
###############################################################################

saveRDS(m_stand_downy, file.path(mod_dir, "m_stand_downy.rds"))
saveRDS(m_stand_silver, file.path(mod_dir, "m_stand_silver.rds"))

m_plot_presence <- lmer(
  damage_N ~ species * silver_present_plot +
    species * north_c +
    species * stem_count_c +
    species * height_c +
    year +
    (1 | area) +
    (1 | stand_id),
  data = birch_plot_long_damage,
  REML = TRUE
)

summary(m_plot_presence)
drop1(update(m_plot_presence, REML = FALSE), test = "Chisq")


###############################################################################
# STAND-LEVEL DEER PELLET MODELS — WITH prop_s_stand INCLUDED
###############################################################################

library(dplyr)
library(lme4)
library(lmerTest)
library(MuMIn)
library(performance)
library(sjPlot)

df_stand_deer <- df_region_stand %>%
  filter(
    !is.na(damage_N),
    !is.na(species),
    !is.na(prop_s_stand),
    !is.na(moose_pellets_log_mean),
    !is.na(red_deer_pellets_log_mean),
    !is.na(small_deer_pellets_log_mean),
    !is.na(total_cervid_pellets_log_mean),
    !is.na(mean_north_c),
    !is.na(stem_count_c),
    !is.na(height_c),
    !is.na(pct),
    !is.na(year),
    !is.na(area),
    !is.na(stand_id)
  ) %>%
  mutate(
    species = droplevels(factor(species, levels = c("Downy", "Silver"))),
    pct = factor(pct),
    year = droplevels(factor(year)),
    area = factor(area),
    stand_id = factor(stand_id)
  )

###############################################################################
# 1. ALL DEER SPECIES — WITH SPECIES INTERACTIONS
###############################################################################

m_stand_deer_int <- lmer(
  damage_N ~ species * prop_s_stand +
    species * moose_pellets_log_mean +
    species * red_deer_pellets_log_mean +
    species * small_deer_pellets_log_mean +
    species * mean_north_c +
    species * stem_count_c +
    species * height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_deer,
  REML = TRUE
)

summary(m_stand_deer_int)
r.squaredGLMM(m_stand_deer_int)
drop1(update(m_stand_deer_int, REML = FALSE), test = "Chisq")
check_collinearity(m_stand_deer_int)

###############################################################################
# 2. ALL DEER SPECIES — NO INTERACTIONS
###############################################################################

m_stand_deer_additive <- lmer(
  damage_N ~ species +
    prop_s_stand +
    moose_pellets_log_mean +
    red_deer_pellets_log_mean +
    small_deer_pellets_log_mean +
    mean_north_c +
    stem_count_c +
    height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_deer,
  REML = TRUE
)

summary(m_stand_deer_additive)
r.squaredGLMM(m_stand_deer_additive)
drop1(update(m_stand_deer_additive, REML = FALSE), test = "Chisq")
check_collinearity(m_stand_deer_additive)

###############################################################################
# 3. TOTAL CERVID — WITH SPECIES INTERACTION
###############################################################################

m_stand_cervid_int <- lmer(
  damage_N ~ species * prop_s_stand +
    species * total_cervid_pellets_log_mean +
    species * mean_north_c +
    species * stem_count_c +
    species * height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_deer,
  REML = TRUE
)

summary(m_stand_cervid_int)
r.squaredGLMM(m_stand_cervid_int)
drop1(update(m_stand_cervid_int, REML = FALSE), test = "Chisq")
check_collinearity(m_stand_cervid_int)

###############################################################################
# 4. TOTAL CERVID — NO INTERACTIONS
###############################################################################

m_stand_cervid_additive <- lmer(
  damage_N ~ species +
    prop_s_stand +
    total_cervid_pellets_log_mean +
    mean_north_c +
    stem_count_c +
    height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_deer,
  REML = TRUE
)

summary(m_stand_cervid_additive)
r.squaredGLMM(m_stand_cervid_additive)
drop1(update(m_stand_cervid_additive, REML = FALSE), test = "Chisq")
check_collinearity(m_stand_cervid_additive)

###############################################################################
# 5. SPECIES-SPECIFIC DATASETS
###############################################################################

df_stand_deer_downy <- df_stand_deer %>%
  filter(species == "Downy") %>%
  droplevels()

df_stand_deer_silver <- df_stand_deer %>%
  filter(species == "Silver") %>%
  droplevels()

###############################################################################
# 6. DOWNY ONLY — ALL DEER SPECIES
###############################################################################

m_stand_deer_downy <- lmer(
  damage_N ~ prop_s_stand +
    moose_pellets_log_mean +
    red_deer_pellets_log_mean +
    small_deer_pellets_log_mean +
    mean_north_c +
    stem_count_c +
    height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_deer_downy,
  REML = TRUE
)

summary(m_stand_deer_downy)
r.squaredGLMM(m_stand_deer_downy)
drop1(update(m_stand_deer_downy, REML = FALSE), test = "Chisq")
check_collinearity(m_stand_deer_downy)

###############################################################################
# 7. SILVER ONLY — ALL DEER SPECIES
###############################################################################

m_stand_deer_silver <- lmer(
  damage_N ~ prop_s_stand +
    moose_pellets_log_mean +
    red_deer_pellets_log_mean +
    small_deer_pellets_log_mean +
    mean_north_c +
    stem_count_c +
    height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_deer_silver,
  REML = TRUE
)

summary(m_stand_deer_silver)
r.squaredGLMM(m_stand_deer_silver)
drop1(update(m_stand_deer_silver, REML = FALSE), test = "Chisq")
check_collinearity(m_stand_deer_silver)

###############################################################################
# 8. DOWNY ONLY — TOTAL CERVID
###############################################################################

m_stand_cervid_downy <- lmer(
  damage_N ~ prop_s_stand +
    total_cervid_pellets_log_mean +
    mean_north_c +
    stem_count_c +
    height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_deer_downy,
  REML = TRUE
)

summary(m_stand_cervid_downy)
r.squaredGLMM(m_stand_cervid_downy)
drop1(update(m_stand_cervid_downy, REML = FALSE), test = "Chisq")
check_collinearity(m_stand_cervid_downy)

###############################################################################
# 9. SILVER ONLY — TOTAL CERVID
###############################################################################

m_stand_cervid_silver <- lmer(
  damage_N ~ prop_s_stand +
    total_cervid_pellets_log_mean +
    mean_north_c +
    stem_count_c +
    height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_deer_silver,
  REML = TRUE
)

summary(m_stand_cervid_silver)
r.squaredGLMM(m_stand_cervid_silver)
drop1(update(m_stand_cervid_silver, REML = FALSE), test = "Chisq")
check_collinearity(m_stand_cervid_silver)

###############################################################################
# DEER MODEL TABLES AND FIGURES
###############################################################################

library(ggeffects)
library(ggplot2)
library(sjPlot)
library(dplyr)

###############################################################################
# 1. HTML MODEL TABLE
###############################################################################

tab_model(
  m_stand_deer_additive,
  m_stand_deer_int,
  m_stand_cervid_additive,
  m_stand_cervid_int,
  m_stand_deer_downy,
  m_stand_deer_silver,
  m_stand_cervid_downy,
  m_stand_cervid_silver,
  dv.labels = c(
    "All deer additive",
    "All deer species interaction",
    "Total cervid additive",
    "Total cervid species interaction",
    "Downy only: all deer",
    "Silver only: all deer",
    "Downy only: total cervid",
    "Silver only: total cervid"
  ),
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_stand_level_deer_models.html")
)

###############################################################################
# 2. FUNCTION FOR PREDICTION PLOTS
###############################################################################

plot_deer_effect <- function(model, deer_var, title_text, file_name) {
  
  pred <- ggpredict(
    model,
    terms = c(paste0(deer_var, " [all]"), "species")
  ) %>%
    as.data.frame()
  
  p <- ggplot(
    pred,
    aes(x = x, y = predicted, colour = group, fill = group)
  ) +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high),
      alpha = 0.18,
      colour = NA
    ) +
    geom_line(linewidth = 1.1) +
    scale_colour_manual(values = pal) +
    scale_fill_manual(values = pal) +
    labs(
      title = title_text,
      x = deer_var,
      y = "Predicted browsing severity",
      colour = "Species",
      fill = "Species"
    ) +
    theme_pub()
  
  print(p)
  
  ggsave(
    file.path(fig_dir, file_name),
    p,
    width = 7,
    height = 5,
    dpi = 600
  )
}

###############################################################################
# 3. ALL-DEER INTERACTION MODEL FIGURES
###############################################################################

plot_deer_effect(
  m_stand_deer_int,
  "moose_pellets_log_mean",
  "Effect of moose pellets on browsing severity",
  "stand_deer_moose_effect.png"
)

plot_deer_effect(
  m_stand_deer_int,
  "red_deer_pellets_log_mean",
  "Effect of red deer pellets on browsing severity",
  "stand_deer_red_deer_effect.png"
)

plot_deer_effect(
  m_stand_deer_int,
  "small_deer_pellets_log_mean",
  "Effect of small deer pellets on browsing severity",
  "stand_deer_small_deer_effect.png"
)

###############################################################################
# 4. TOTAL CERVID INTERACTION MODEL FIGURE
###############################################################################

plot_deer_effect(
  m_stand_cervid_int,
  "total_cervid_pellets_log_mean",
  "Effect of total cervid pellets on browsing severity",
  "stand_deer_total_cervid_effect.png"
)

###############################################################################
# 5. SPECIES-SPECIFIC FIGURE FUNCTION
###############################################################################

plot_species_deer_effect <- function(model, deer_var, title_text, file_name) {
  
  pred <- ggpredict(
    model,
    terms = paste0(deer_var, " [all]")
  ) %>%
    as.data.frame()
  
  p <- ggplot(
    pred,
    aes(x = x, y = predicted)
  ) +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high),
      alpha = 0.18
    ) +
    geom_line(linewidth = 1.1) +
    labs(
      title = title_text,
      x = deer_var,
      y = "Predicted browsing severity"
    ) +
    theme_pub()
  
  print(p)
  
  ggsave(
    file.path(fig_dir, file_name),
    p,
    width = 7,
    height = 5,
    dpi = 600
  )
}

###############################################################################
# 6. DOWNY-ONLY FIGURES
###############################################################################

plot_species_deer_effect(
  m_stand_deer_downy,
  "moose_pellets_log_mean",
  "Downy birch: effect of moose pellets",
  "downy_moose_effect.png"
)

plot_species_deer_effect(
  m_stand_deer_downy,
  "red_deer_pellets_log_mean",
  "Downy birch: effect of red deer pellets",
  "downy_red_deer_effect.png"
)

plot_species_deer_effect(
  m_stand_deer_downy,
  "small_deer_pellets_log_mean",
  "Downy birch: effect of small deer pellets",
  "downy_small_deer_effect.png"
)

plot_species_deer_effect(
  m_stand_cervid_downy,
  "total_cervid_pellets_log_mean",
  "Downy birch: effect of total cervid pellets",
  "downy_total_cervid_effect.png"
)

###############################################################################
# 7. SILVER-ONLY FIGURES
###############################################################################

plot_species_deer_effect(
  m_stand_deer_silver,
  "moose_pellets_log_mean",
  "Silver birch: effect of moose pellets",
  "silver_moose_effect.png"
)

plot_species_deer_effect(
  m_stand_deer_silver,
  "red_deer_pellets_log_mean",
  "Silver birch: effect of red deer pellets",
  "silver_red_deer_effect.png"
)

plot_species_deer_effect(
  m_stand_deer_silver,
  "small_deer_pellets_log_mean",
  "Silver birch: effect of small deer pellets",
  "silver_small_deer_effect.png"
)

plot_species_deer_effect(
  m_stand_cervid_silver,
  "total_cervid_pellets_log_mean",
  "Silver birch: effect of total cervid pellets",
  "silver_total_cervid_effect.png"
)

cat("\nDone. Deer model HTML and figures saved.\n")

###############################################################################
# DEER PELLET FIGURE — STAND LEVEL, BY SPECIES AND REGION
###############################################################################

library(dplyr)
library(lme4)
library(lmerTest)
library(ggeffects)
library(ggplot2)
library(patchwork)

###############################################################################
# 1. REGION-SPECIFIC DEER INTERACTION MODEL
###############################################################################

m_stand_deer_region_int <- lmer(
  damage_N ~ species * prop_s_stand +
    species * moose_pellets_log_mean * region +
    species * red_deer_pellets_log_mean * region +
    species * small_deer_pellets_log_mean * region +
    species * mean_north_c +
    species * stem_count_c +
    species * height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_deer,
  REML = TRUE
)

summary(m_stand_deer_region_int)
r.squaredGLMM(m_stand_deer_region_int)
drop1(update(m_stand_deer_region_int, REML = FALSE), test = "Chisq")
check_collinearity(m_stand_deer_region_int)


m_stand_deer_Addative <- lmer(
  damage_N ~ species + prop_s_stand +
    moose_pellets_log_mean + region +
    red_deer_pellets_log_mean +
    small_deer_pellets_log_mean  +
    mean_north_c +
    stem_count_c +
    height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_deer,
  REML = TRUE
)

summary(m_stand_deer_Addative)
###############################################################################
# 2. PLOT FUNCTION
###############################################################################

plot_deer_region_effect <- function(model, pellet_var, title_text) {
  
  pred <- ggpredict(
    model,
    terms = c(
      paste0(pellet_var, " [all]"),
      "species",
      "region"
    )
  ) %>%
    as.data.frame()
  
  ggplot(
    pred,
    aes(x = x, y = predicted, colour = group, fill = group)
  ) +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high),
      alpha = 0.18,
      colour = NA
    ) +
    geom_line(linewidth = 1.1) +
    facet_wrap(~ facet, nrow = 1) +
    scale_colour_manual(values = pal) +
    scale_fill_manual(values = pal) +
    labs(
      title = title_text,
      x = "Pellets, log mean",
      y = "Predicted browsing severity",
      colour = "Species",
      fill = "Species"
    ) +
    theme_pub() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold")
    )
}

###############################################################################
# 3. CREATE FIGURES
###############################################################################

p_moose_region <- plot_deer_region_effect(
  m_stand_deer_region_int,
  "moose_pellets_log_mean",
  "Moose pellets"
)

p_red_region <- plot_deer_region_effect(
  m_stand_deer_region_int,
  "red_deer_pellets_log_mean",
  "Red deer pellets"
)

p_small_region <- plot_deer_region_effect(
  m_stand_deer_region_int,
  "small_deer_pellets_log_mean",
  "Small deer pellets"
)

###############################################################################
# 4. COMBINE INTO ONE FIGURE
###############################################################################

p_deer_region_combined <- 
  p_moose_region /
  p_red_region /
  p_small_region +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Effects of deer pellet abundance on birch browsing severity",
    subtitle = "Stand-level mixed model predictions by birch species and region"
  )

print(p_deer_region_combined)

ggsave(
  file.path(fig_dir, "stand_deer_pellet_effects_by_region.png"),
  p_deer_region_combined,
  width = 11,
  height = 12,
  dpi = 600
)

###############################################################################
# 5. HTML MODEL TABLE — INTERACTION MODEL ONLY
###############################################################################

tab_model(
  m_stand_deer_int,
  m_stand_deer_region_int,
  m_stand_deer_downy,
  m_stand_deer_silver,
  dv.labels = c(
    "Stand deer interaction",
    "Stand deer interaction by region",
    "Downy only",
    "Silver only"
  ),
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_stand_level_deer_interaction_models.html")
)

saveRDS(
  m_stand_deer_region_int,
  file.path(mod_dir, "m_stand_deer_region_int.rds")
)

cat("\nSaved figure to:\n")
cat(file.path(fig_dir, "stand_deer_pellet_effects_by_region.png"), "\n")

cat("\nSaved HTML table to:\n")
cat(file.path(tab_dir, "Table_stand_level_deer_interaction_models.html"), "\n")

###############################################################################
# 3-PANEL DEER PELLET FIGURE
# Moose, red deer, small deer
# Downy and Silver shown as coloured lines
###############################################################################

library(ggeffects)
library(ggplot2)
library(dplyr)
library(patchwork)
library(performance)

pal <- c(
  "Downy"  = "#3BA64C",
  "Silver" = "#4C5C8C"
)

###############################################################################
# R2 AND P VALUES FROM INTERACTION MODEL
###############################################################################

r2_deer <- r.squaredGLMM(m_stand_deer_int)

R2m <- round(r2_deer[1, "R2m"], 3)
R2c <- round(r2_deer[1, "R2c"], 3)

coef_tab <- summary(m_stand_deer_int)$coefficients

p_moose <- signif(
  coef_tab["speciesSilver:moose_pellets_log_mean", "Pr(>|t|)"], 3
)

p_red <- signif(
  coef_tab["speciesSilver:red_deer_pellets_log_mean", "Pr(>|t|)"], 3
)

p_small <- signif(
  coef_tab["speciesSilver:small_deer_pellets_log_mean", "Pr(>|t|)"], 3
)

###############################################################################
# PLOT FUNCTION
###############################################################################

plot_deer_panel <- function(model, pellet_var, panel_title, p_value) {
  
  pred <- ggpredict(
    model,
    terms = c(
      paste0(pellet_var, " [all]"),
      "species"
    )
  ) %>%
    as.data.frame()
  
  ggplot(
    pred,
    aes(
      x = x,
      y = predicted,
      colour = group,
      fill = group
    )
  ) +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high),
      alpha = 0.18,
      colour = NA
    ) +
    geom_line(linewidth = 1.2) +
    scale_colour_manual(values = pal) +
    scale_fill_manual(values = pal) +
    labs(
      title = panel_title,
      subtitle = paste0(
        "Species × pellet p = ", p_value,
        "\nR²m = ", R2m,
        "   R²c = ", R2c
      ),
      x = "Pellet abundance, log mean",
      y = "Predicted browsing damage (%)",
      colour = "Birch species",
      fill = "Birch species"
    ) +
    theme_classic(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 10)
    )
}

###############################################################################
# MAKE THREE PANELS
###############################################################################

p_moose <- plot_deer_panel(
  m_stand_deer_int,
  "moose_pellets_log_mean",
  "Moose pellets",
  p_moose
)

p_red <- plot_deer_panel(
  m_stand_deer_int,
  "red_deer_pellets_log_mean",
  "Red deer pellets",
  p_red
)

p_small <- plot_deer_panel(
  m_stand_deer_int,
  "small_deer_pellets_log_mean",
  "Small deer pellets",
  p_small
)

###############################################################################
# COMBINE FIGURE
###############################################################################

figure_deer_3panel <- 
  p_moose | p_red | p_small +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Effects of deer pellet abundance on birch browsing damage",
    subtitle = "Stand-level mixed model with species interactions and prop_s_stand included"
  ) &
  theme(
    legend.position = "bottom"
  )

print(figure_deer_3panel)

###############################################################################
# SAVE FIGURE
###############################################################################

ggsave(
  file.path(fig_dir, "Figure_deer_pellet_effects_3panel.png"),
  figure_deer_3panel,
  width = 12,
  height = 4.5,
  dpi = 600
)

ggsave(
  file.path(fig_dir, "Figure_deer_pellet_effects_3panel.pdf"),
  figure_deer_3panel,
  width = 12,
  height = 4.5
)

###############################################################################
# FIGURE: SPECIES-SPECIFIC DEER PELLET MODELS
# 3 panels: Moose, Red deer, Small deer
# Lines: Downy model and Silver model
###############################################################################

library(dplyr)
library(ggplot2)
library(ggeffects)
library(patchwork)
library(MuMIn)

pal <- c(
  "Downy"  = "#3BA64C",
  "Silver" = "#4C5C8C"
)

###############################################################################
# 1. EXTRACT MODEL R2 VALUES
###############################################################################

r2_downy  <- r.squaredGLMM(m_stand_deer_downy)
r2_silver <- r.squaredGLMM(m_stand_deer_silver)

R2m_downy  <- round(r2_downy[1, "R2m"], 3)
R2c_downy  <- round(r2_downy[1, "R2c"], 3)

R2m_silver <- round(r2_silver[1, "R2m"], 3)
R2c_silver <- round(r2_silver[1, "R2c"], 3)

###############################################################################
# 2. EXTRACT P VALUES
###############################################################################

coef_downy  <- summary(m_stand_deer_downy)$coefficients
coef_silver <- summary(m_stand_deer_silver)$coefficients

p_downy_moose <- signif(coef_downy["moose_pellets_log_mean", "Pr(>|t|)"], 3)
p_downy_red   <- signif(coef_downy["red_deer_pellets_log_mean", "Pr(>|t|)"], 3)
p_downy_small <- signif(coef_downy["small_deer_pellets_log_mean", "Pr(>|t|)"], 3)

p_silver_moose <- signif(coef_silver["moose_pellets_log_mean", "Pr(>|t|)"], 3)
p_silver_red   <- signif(coef_silver["red_deer_pellets_log_mean", "Pr(>|t|)"], 3)
p_silver_small <- signif(coef_silver["small_deer_pellets_log_mean", "Pr(>|t|)"], 3)

###############################################################################
# 3. PREDICTION FUNCTION
###############################################################################

make_species_predictions <- function(pellet_var) {
  
  pred_downy <- ggpredict(
    m_stand_deer_downy,
    terms = paste0(pellet_var, " [all]")
  ) %>%
    as.data.frame() %>%
    mutate(species = "Downy")
  
  pred_silver <- ggpredict(
    m_stand_deer_silver,
    terms = paste0(pellet_var, " [all]")
  ) %>%
    as.data.frame() %>%
    mutate(species = "Silver")
  
  bind_rows(pred_downy, pred_silver)
}

###############################################################################
# 4. PANEL FUNCTION
###############################################################################

plot_deer_species_panel <- function(pellet_var,
                                    title_text,
                                    p_downy,
                                    p_silver) {
  
  pred <- make_species_predictions(pellet_var)
  
  ggplot(
    pred,
    aes(
      x = x,
      y = predicted,
      colour = species,
      fill = species
    )
  ) +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high),
      alpha = 0.18,
      colour = NA
    ) +
    geom_line(linewidth = 1.2) +
    scale_colour_manual(values = pal) +
    scale_fill_manual(values = pal) +
    labs(
      title = title_text,
      subtitle = paste0(
        "Downy p = ", p_downy,
        " | Silver p = ", p_silver,
        "\nDowny R²m = ", R2m_downy,
        ", R²c = ", R2c_downy,
        " | Silver R²m = ", R2m_silver,
        ", R²c = ", R2c_silver
      ),
      x = "Pellet abundance, log mean",
      y = "Predicted browsing damage (%)",
      colour = "Birch species",
      fill = "Birch species"
    ) +
    theme_classic(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 9)
    )
}

###############################################################################
# 5. MAKE THREE PANELS
###############################################################################

p_moose_species <- plot_deer_species_panel(
  pellet_var = "moose_pellets_log_mean",
  title_text = "Moose pellets",
  p_downy = p_downy_moose,
  p_silver = p_silver_moose
)

p_red_species <- plot_deer_species_panel(
  pellet_var = "red_deer_pellets_log_mean",
  title_text = "Red deer pellets",
  p_downy = p_downy_red,
  p_silver = p_silver_red
)

p_small_species <- plot_deer_species_panel(
  pellet_var = "small_deer_pellets_log_mean",
  title_text = "Small deer pellets",
  p_downy = p_downy_small,
  p_silver = p_silver_small
)

###############################################################################
# 6. COMBINE FIGURE
###############################################################################

figure_deer_species_specific <-
  p_moose_species |
  p_red_species |
  p_small_species +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Effects of deer pellet abundance on birch browsing damage",
    subtitle = "Species-specific stand-level mixed models"
  ) &
  theme(
    legend.position = "bottom"
  )

print(figure_deer_species_specific)

###############################################################################
# 7. SAVE FIGURE
###############################################################################

ggsave(
  file.path(fig_dir, "Figure_deer_species_specific_3panel.png"),
  figure_deer_species_specific,
  width = 13,
  height = 5,
  dpi = 600
)

ggsave(
  file.path(fig_dir, "Figure_deer_species_specific_3panel.pdf"),
  figure_deer_species_specific,
  width = 13,
  height = 5
)

###############################################################################
# FIGURE: MEAN STEM COUNT BY AREA AND SPECIES
###############################################################################

library(dplyr)
library(ggplot2)

###############################################################################
# COLOURS
###############################################################################

pal <- c(
  "Downy" = "#3BA64C",
  "Silver" = "#4C5C8C"
)

###############################################################################
# CALCULATE MEANS AND STANDARD ERRORS
###############################################################################

stem_summary <- df_region_stand %>%
  group_by(area, species) %>%
  summarise(
    n = sum(!is.na(stem_count)),
    mean_stems = mean(stem_count, na.rm = TRUE),
    sd = sd(stem_count, na.rm = TRUE),
    se = sd / sqrt(n),
    .groups = "drop"
  )

###############################################################################
# OPTIONAL: ORDER AREAS FROM NORTH TO SOUTH
###############################################################################

stem_summary$area <- factor(
  stem_summary$area,
  levels = c(
    "Råneå",
    "Sorsele",
    "Lycksele",
    "Fredrika",
    "Nordmaling",
    "Lofsdalen",
    "Ljusdal",
    "Furudal",
    "ÖsterMalma",
    "Åtvidaberg",
    "Växjö"
  )
)

###############################################################################
# BAR CHART
###############################################################################

p_stems <-
  
  ggplot(
    stem_summary,
    aes(
      x = area,
      y = mean_stems,
      fill = species
    )
  ) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_stems - se,
      ymax = mean_stems + se
    ),
    width = 0.2,
    linewidth = 0.6,
    position = position_dodge(width = 0.8)
  ) +
  
  scale_fill_manual(values = pal) +
  
  labs(
    x = "Area",
    y = "Mean stem count",
    fill = "Species",
    title = "Mean birch stem count by area"
  ) +
  
  theme_classic(base_size = 13) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )

print(p_stems)

###############################################################################
# SAVE
###############################################################################

ggsave(
  file.path(fig_dir, "Figure_mean_stem_count_by_area.png"),
  p_stems,
  width = 10,
  height = 5,
  dpi = 600
)

ggsave(
  file.path(fig_dir, "Figure_mean_stem_count_by_area.pdf"),
  p_stems,
  width = 10,
  height = 5
)

###############################################################################
# FIGURE: MEAN STEM COUNT PER HECTARE BY AREA AND SPECIES
# Plot radius = 3.5 m
# 10 plots per stand
# Fredrika/Lögda merged with Fredrika
###############################################################################

library(dplyr)
library(ggplot2)

###############################################################################
# COLOURS
###############################################################################

pal <- c(
  "Downy"  = "#3BA64C",
  "Silver" = "#4C5C8C"
)

###############################################################################
# PLOT AREA AND CONVERSION FACTOR
###############################################################################

plot_radius_m <- 3.5

plot_area_m2 <- pi * plot_radius_m^2

stems_per_ha_factor <- 10000 / plot_area_m2

cat("Area of one plot:", plot_area_m2, "m2\n")
cat("Conversion factor from stems per plot to stems per hectare:",
    stems_per_ha_factor, "\n")

###############################################################################
# CREATE PLOTTING DATA
###############################################################################

df_region_stand_plot <- df_region_stand %>%
  mutate(
    area = recode(
      area,
      "Fredrika/Lögda" = "Fredrika"
    ),
    stem_count_ha = stem_count * stems_per_ha_factor
  )

###############################################################################
# CALCULATE MEANS ± SE
###############################################################################

stem_summary_ha <- df_region_stand_plot %>%
  filter(
    !is.na(area),
    !is.na(species),
    !is.na(stem_count_ha)
  ) %>%
  group_by(area, species) %>%
  summarise(
    n = n(),
    mean_stems_ha = mean(stem_count_ha),
    sd_stems_ha = sd(stem_count_ha),
    se_stems_ha = sd_stems_ha / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    area = factor(
      area,
      levels = c(
        "Råneå",
        "Sorsele",
        "Malå",
        "Lycksele",
        "Fredrika",
        "Nordmaling",
        "Lofsdalen",
        "Ljusdal",
        "Furudal",
        "Barksätter",
        "ÖsterMalma",
        "Åtvidaberg",
        "Växjö"
      )
    )
  )

###############################################################################
# BAR CHART
###############################################################################

p_stem_area_ha <- ggplot(
  stem_summary_ha,
  aes(
    x = area,
    y = mean_stems_ha,
    fill = species
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin = mean_stems_ha - se_stems_ha,
      ymax = mean_stems_ha + se_stems_ha
    ),
    position = position_dodge(width = 0.8),
    width = 0.2,
    linewidth = 0.6
  ) +
  scale_fill_manual(values = pal) +
  labs(
    title = "Mean birch stem density by area",
    x = "Area",
    y = "Mean stems per hectare (± SE)",
    fill = "Species"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(p_stem_area_ha)

###############################################################################
# SAVE FIGURE
###############################################################################

ggsave(
  file.path(fig_dir, "Figure_mean_stem_density_per_ha_by_area_species.png"),
  p_stem_area_ha,
  width = 11,
  height = 5.5,
  dpi = 600
)

ggsave(
  file.path(fig_dir, "Figure_mean_stem_density_per_ha_by_area_species.pdf"),
  p_stem_area_ha,
  width = 11,
  height = 5.5
)

###############################################################################
# AREA DOMINANCE MODELS
# Are patterns different in areas with more Silver vs more Downy stems?
###############################################################################

library(dplyr)
library(lme4)
library(lmerTest)
library(MuMIn)
library(performance)
library(sjPlot)
library(ggeffects)
library(ggplot2)

###############################################################################
# 1. AREA-LEVEL STEM DENSITY
# Assumes stem_count is mean stems per 3.5 m radius plot
###############################################################################

plot_radius_m <- 3.5
plot_area_m2 <- pi * plot_radius_m^2
stems_per_ha_factor <- 10000 / plot_area_m2

area_stem_density <- df_region_stand %>%
  mutate(
    area = recode(
      area,
      "Fredrika/Lögda" = "Fredrika"
    ),
    stem_count_ha = stem_count * stems_per_ha_factor
  ) %>%
  group_by(area, species) %>%
  summarise(
    mean_stems_ha = mean(stem_count_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = species,
    values_from = mean_stems_ha
  ) %>%
  mutate(
    area_dominance = case_when(
      Silver > Downy ~ "Silver-dominated",
      Downy >= Silver ~ "Downy-dominated",
      TRUE ~ NA_character_
    )
  )

print(area_stem_density)

###############################################################################
# 2. ADD AREA DOMINANCE BACK TO STAND DATA
###############################################################################

df_region_stand_dom <- df_region_stand %>%
  mutate(
    area = recode(
      area,
      "Fredrika/Lögda" = "Fredrika"
    )
  ) %>%
  left_join(
    area_stem_density %>% select(area, area_dominance),
    by = "area"
  ) %>%
  filter(
    !is.na(area_dominance),
    !is.na(damage_N),
    !is.na(species),
    !is.na(prop_s_stand),
    !is.na(mean_north_c),
    !is.na(stem_count_c),
    !is.na(height_c),
    !is.na(pct),
    !is.na(year),
    !is.na(area),
    !is.na(stand_id)
  ) %>%
  mutate(
    area_dominance = factor(
      area_dominance,
      levels = c("Downy-dominated", "Silver-dominated")
    ),
    species = droplevels(factor(species, levels = c("Downy", "Silver"))),
    pct = factor(pct),
    year = droplevels(factor(year)),
    area = factor(area),
    stand_id = factor(stand_id)
  )

###############################################################################
# 3. MAIN MODEL: TEST WHETHER MIXTURE EFFECT DIFFERS BY AREA DOMINANCE
###############################################################################

m_stand_dominance_int <- lmer(
  damage_N ~ species * prop_s_stand +
    species * area_dominance +
    species * mean_north_c +
    species * stem_count_c +
    species * height_c +
    species * moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_region_stand_dom,
  REML = TRUE
)

summary(m_stand_dominance_int)
r.squaredGLMM(m_stand_dominance_int)
drop1(update(m_stand_dominance_int, REML = FALSE), test = "Chisq")
check_collinearity(m_stand_dominance_int)

###############################################################################
# 4. SEPARATE MODELS FOR INTERPRETATION
###############################################################################

df_downy_dom <- df_region_stand_dom %>%
  filter(area_dominance == "Downy-dominated") %>%
  droplevels()

df_silver_dom <- df_region_stand_dom %>%
  filter(area_dominance == "Silver-dominated") %>%
  droplevels()

m_stand_downy_dom <- lmer(
  damage_N ~ species * prop_s_stand +
    species * mean_north_c +
    species * stem_count_c +
    species * height_c +
    species * moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_downy_dom,
  REML = TRUE
)

m_stand_silver_dom <- lmer(
  damage_N ~ species * prop_s_stand +
    species * mean_north_c +
    species * stem_count_c +
    species * height_c +
    species * moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_silver_dom,
  REML = TRUE
)

summary(m_stand_downy_dom)
summary(m_stand_silver_dom)

r.squaredGLMM(m_stand_downy_dom)
r.squaredGLMM(m_stand_silver_dom)

drop1(update(m_stand_downy_dom, REML = FALSE), test = "Chisq")
drop1(update(m_stand_silver_dom, REML = FALSE), test = "Chisq")

###############################################################################
# 5. HTML TABLE
###############################################################################

tab_model(
  m_stand_dominance_int,
  m_stand_downy_dom,
  m_stand_silver_dom,
  dv.labels = c(
    "Area dominance interaction",
    "Downy-dominated areas",
    "Silver-dominated areas"
  ),
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_area_dominance_models.html")
)

###############################################################################
# 6. PREDICTION FIGURE
###############################################################################

pal <- c(
  "Downy"  = "#3BA64C",
  "Silver" = "#4C5C8C"
)

pred_dom <- ggpredict(
  m_stand_dominance_int,
  terms = c(
    "prop_s_stand [0:1 by=0.05]",
    "species",
    "area_dominance"
  )
) %>%
  as.data.frame()

p_dom <- ggplot(
  pred_dom,
  aes(
    x = x,
    y = predicted,
    colour = group,
    fill = group
  )
) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    alpha = 0.18,
    colour = NA
  ) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~ facet) +
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    labels = c("Pure\nDowny", "Mixed", "Pure\nSilver")
  ) +
  labs(
    title = "Stand-level birch mixture effect by area dominance",
    x = "Stand-level silver birch proportion",
    y = "Predicted browsing damage (%)",
    colour = "Species",
    fill = "Species"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )

print(p_dom)

ggsave(
  file.path(fig_dir, "Figure_area_dominance_mixture_effect.png"),
  p_dom,
  width = 9,
  height = 5.5,
  dpi = 600
)

ggsave(
  file.path(fig_dir, "Figure_area_dominance_mixture_effect.pdf"),
  p_dom,
  width = 9,
  height = 5.5
)
###############################################################################
# LANDSCAPE EFFECT MODEL
# Does landscape Silver stem density modify the stand-level mixture effect?
###############################################################################

library(dplyr)
library(lme4)
library(lmerTest)
library(MuMIn)
library(performance)
library(sjPlot)
library(ggeffects)
library(ggplot2)

###############################################################################
# CONVERT TO STEMS PER HECTARE
###############################################################################

plot_radius <- 3.5
plot_area <- pi * plot_radius^2
stems_per_ha_factor <- 10000 / plot_area

###############################################################################
# CREATE AREA-LEVEL SILVER STEM DENSITY
###############################################################################

area_silver_density <- df_region_stand %>%
  mutate(
    area = recode(
      area,
      "Fredrika/Lögda" = "Fredrika"
    ),
    stem_count_ha = stem_count * stems_per_ha_factor
  ) %>%
  filter(species == "Silver") %>%
  group_by(area) %>%
  summarise(
    silver_density_ha = mean(stem_count_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    silver_density_c =
      scale(silver_density_ha)[,1]
  )

print(area_silver_density)

###############################################################################
# ADD TO STAND DATA
###############################################################################

df_region_landscape <- df_region_stand %>%
  mutate(
    area = recode(
      area,
      "Fredrika/Lögda" = "Fredrika"
    )
  ) %>%
  left_join(
    area_silver_density,
    by = "area"
  ) %>%
  filter(
    !is.na(silver_density_c)
  )

###############################################################################
# LANDSCAPE MODEL
###############################################################################

m_landscape_silver <- lmer(
  damage_N ~
    species *
    prop_s_stand *
    silver_density_c +
    species * mean_north_c +
    species * stem_count_c +
    species * height_c +
    species * moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_region_landscape,
  REML = TRUE
)
 


m_landscape_silverb <- lmer(
  damage_N ~
    species *
    prop_s_stand +
    species * silver_density_c +
    species * mean_north_c +
    species * stem_count_c +
    species * height_c +
    species * moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_region_landscape,
  REML = TRUE
)

summary(m_landscape_silverb)
r.squaredGLMM(m_landscape_silverb)

drop1(
  update(m_landscape_silverb, REML = FALSE),
  test = "Chisq"
)

check_collinearity(m_landscape_silverb)
###############################################################################
# MODEL OUTPUT
###############################################################################

summary(m_landscape_silver)

r.squaredGLMM(m_landscape_silver)

drop1(
  update(m_landscape_silver, REML = FALSE),
  test = "Chisq"
)

check_collinearity(m_landscape_silver)

###############################################################################
# HTML TABLE
###############################################################################

tab_model(
  m_landscape_silver,
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(
    tab_dir,
    "Table_landscape_silver_density_model.html"
  )
)
tab_model(
  m_landscape_silverb,
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(
    tab_dir,
    "Table_landscape_silverb_density_model.html"
  )
)
###############################################################################
# PREDICTIONS
###############################################################################

pal <- c(
  "Downy" = "#3BA64C",
  "Silver" = "#4C5C8C"
)

pred_landscape <- ggpredict(
  m_landscape_silver,
  terms = c(
    "prop_s_stand [0:1 by=0.05]",
    "species",
    "silver_density_c [-1,0,1]"
  )
) %>%
  as.data.frame()

pred_landscape$facet <- factor(
  pred_landscape$facet,
  levels = c("-1", "0", "1"),
  labels = c(
    "Low landscape Silver density",
    "Average landscape Silver density",
    "High landscape Silver density"
  )
)

###############################################################################
# FIGURE
###############################################################################

p_landscape <-
  
  ggplot(
    pred_landscape,
    aes(
      x = x,
      y = predicted,
      colour = group,
      fill = group
    )
  ) +
  
  geom_ribbon(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    alpha = 0.18,
    colour = NA
  ) +
  
  geom_line(
    linewidth = 1.2
  ) +
  
  facet_wrap(~facet) +
  
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  
  scale_x_continuous(
    limits = c(0,1),
    breaks = c(0,0.5,1),
    labels = c(
      "Pure\nDowny",
      "Mixed",
      "Pure\nSilver"
    )
  ) +
  
  labs(
    title = "Stand-level birch mixture across a landscape Silver density gradient",
    x = "Stand-level Silver birch proportion",
    y = "Predicted browsing damage (%)",
    colour = "Species",
    fill = "Species"
  ) +
  
  theme_classic(base_size = 13) +
  
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )

print(p_landscape)

###############################################################################
# SAVE
###############################################################################

ggsave(
  file.path(
    fig_dir,
    "Figure_landscape_silver_density.png"
  ),
  p_landscape,
  width = 10,
  height = 5,
  dpi = 600
)

ggsave(
  file.path(
    fig_dir,
    "Figure_landscape_silver_density.pdf"
  ),
  p_landscape,
  width = 10,
  height = 5
)

###############################################################################
# STEM DENSITY + DAMAGE BY AREA
###############################################################################

library(dplyr)
library(tidyr)
library(ggplot2)

###############################################################################
# COLOURS
###############################################################################

pal <- c(
  "Downy" = "#3BA64C",
  "Silver" = "#4C5C8C"
)

###############################################################################
# STEM DENSITY (stems/ha)
###############################################################################

plot_radius <- 3.5
plot_area <- pi * plot_radius^2
factor_ha <- 10000 / plot_area

###############################################################################
# PREPARE DATA
###############################################################################

df_plot <- df_region_stand %>%
  mutate(
    area = recode(
      area,
      "Fredrika/Lögda" = "Fredrika"
    ),
    stem_ha = stem_count * factor_ha
  )

###############################################################################
# STEM SUMMARY
###############################################################################

stem_summary <- df_plot %>%
  group_by(area, species) %>%
  summarise(
    stems = mean(stem_ha),
    se = sd(stem_ha)/sqrt(n()),
    .groups="drop"
  )

###############################################################################
# DAMAGE SUMMARY
###############################################################################

damage_summary <- df_plot %>%
  group_by(area, species) %>%
  summarise(
    damage = mean(damage_N),
    .groups="drop"
  )

###############################################################################
# ORDER AREAS
###############################################################################

area_order <- c(
  "Råneå",
  "Sorsele",
  "Malå",
  "Lycksele",
  "Fredrika",
  "Nordmaling",
  "Lofsdalen",
  "Ljusdal",
  "Furudal",
  "Barksätter",
  "ÖsterMalma",
  "Åtvidaberg",
  "Växjö"
)

stem_summary$area <- factor(stem_summary$area,
                            levels=area_order)

damage_summary$area <- factor(damage_summary$area,
                              levels=area_order)

###############################################################################
# SCALE DAMAGE TO MATCH STEM DENSITY AXIS
###############################################################################

scale_factor <- max(stem_summary$stems) /
  max(damage_summary$damage)

###############################################################################
# FIGURE
###############################################################################

ggplot() +
  
  geom_col(
    data=stem_summary,
    aes(area,
        stems,
        fill=species),
    position=position_dodge(.8),
    width=.7
  )+
  
  geom_errorbar(
    data=stem_summary,
    aes(area,
        ymin=stems-se,
        ymax=stems+se,
        group=species),
    position=position_dodge(.8),
    width=.2
  )+
  
  geom_line(
    data=damage_summary,
    aes(area,
        damage*scale_factor,
        colour=species,
        group=species),
    linewidth=1.3
  )+
  
  geom_point(
    data=damage_summary,
    aes(area,
        damage*scale_factor,
        colour=species),
    size=3
  )+
  
  scale_fill_manual(values=pal)+
  scale_colour_manual(values=pal)+
  
  scale_y_continuous(
    name="Mean stem density (stems/ha)",
    
    sec.axis=sec_axis(
      ~./scale_factor,
      name="Mean browsing damage (%)"
    )
  )+
  
  labs(
    x="Area",
    fill="Stem density",
    colour="Mean damage",
    title="Stem density and browsing damage by area"
  )+
  
  theme_classic(base_size=13)+
  
  theme(
    legend.position="top",
    axis.text.x=element_text(angle=45,hjust=1),
    plot.title=element_text(face="bold")
  )

###############################################################################
# SAVE
###############################################################################

ggsave(
  file.path(fig_dir,
            "Figure_stems_damage_by_area.png"),
  width=11,
  height=6,
  dpi=600
)

ggsave(
  file.path(fig_dir,
            "Figure_stems_damage_by_area.pdf"),
  width=11,
  height=6
)

###############################################################################
# REGION-SPECIFIC DEER MODELS
# Separate models for South, Centre, and North
###############################################################################

library(dplyr)
library(lme4)
library(lmerTest)
library(MuMIn)
library(performance)
library(sjPlot)

###############################################################################
# 1. CREATE REGION DATASETS
###############################################################################

df_deer_south <- df_stand_deer %>%
  filter(region == "South") %>%
  droplevels()

df_deer_centre <- df_stand_deer %>%
  filter(region == "Centre") %>%
  droplevels()

df_deer_north <- df_stand_deer %>%
  filter(region == "North") %>%
  droplevels()

###############################################################################
# 2. SOUTH MODEL
###############################################################################

m_stand_deer_south <- lmer(
  damage_N ~ species * prop_s_stand +
    species * moose_pellets_log_mean +
    species * red_deer_pellets_log_mean +
    species * small_deer_pellets_log_mean +
    species * mean_north_c +
    species * stem_count_c +
    species * height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_deer_south,
  REML = TRUE
)

summary(m_stand_deer_south)
r.squaredGLMM(m_stand_deer_south)
drop1(update(m_stand_deer_south, REML = FALSE), test = "Chisq")
check_collinearity(m_stand_deer_south)

###############################################################################
# 3. CENTRE MODEL
###############################################################################

m_stand_deer_centre <- lmer(
  damage_N ~ species * prop_s_stand +
    species * moose_pellets_log_mean +
    species * red_deer_pellets_log_mean +
    species * small_deer_pellets_log_mean +
    species * mean_north_c +
    species * stem_count_c +
    species * height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_deer_centre,
  REML = TRUE
)

summary(m_stand_deer_centre)
r.squaredGLMM(m_stand_deer_centre)
drop1(update(m_stand_deer_centre, REML = FALSE), test = "Chisq")
check_collinearity(m_stand_deer_centre)

###############################################################################
# 4. NORTH MODEL
###############################################################################

m_stand_deer_north <- lmer(
  damage_N ~ species * prop_s_stand +
    species * moose_pellets_log_mean +
    species * red_deer_pellets_log_mean +
    species * small_deer_pellets_log_mean +
    species * mean_north_c +
    species * stem_count_c +
    species * height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_deer_north,
  REML = TRUE
)

summary(m_stand_deer_north)
r.squaredGLMM(m_stand_deer_north)
drop1(update(m_stand_deer_north, REML = FALSE), test = "Chisq")
check_collinearity(m_stand_deer_north)

###############################################################################
# 5. SEPARATE HTML TABLES, WITHOUT CI
###############################################################################

tab_model(
  m_stand_deer_south,
  dv.labels = "South deer model",
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_deer_model_south.html")
)

tab_model(
  m_stand_deer_centre,
  dv.labels = "Centre deer model",
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_deer_model_centre.html")
)

tab_model(
  m_stand_deer_north,
  dv.labels = "North deer model",
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_deer_model_north.html")
)

###############################################################################
# 6. OPTIONAL COMBINED TABLE
###############################################################################

tab_model(
  m_stand_deer_south,
  m_stand_deer_centre,
  m_stand_deer_north,
  dv.labels = c(
    "South deer model",
    "Centre deer model",
    "North deer model"
  ),
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_deer_models_by_region.html")
)


###############################################################################
# SIX DEER MODELS: SPECIES × REGION
# Downy/Silver separately within South, Centre, and North
###############################################################################

library(dplyr)
library(lme4)
library(lmerTest)
library(MuMIn)
library(performance)
library(sjPlot)
library(broom.mixed)
library(ggplot2)

###############################################################################
# 1. CREATE DATASETS
###############################################################################

df_downy_south <- df_stand_deer %>%
  filter(species == "Downy", region == "South") %>%
  droplevels()

df_silver_south <- df_stand_deer %>%
  filter(species == "Silver", region == "South") %>%
  droplevels()

df_downy_centre <- df_stand_deer %>%
  filter(species == "Downy", region == "Centre") %>%
  droplevels()

df_silver_centre <- df_stand_deer %>%
  filter(species == "Silver", region == "Centre") %>%
  droplevels()

df_downy_north <- df_stand_deer %>%
  filter(species == "Downy", region == "North") %>%
  droplevels()

df_silver_north <- df_stand_deer %>%
  filter(species == "Silver", region == "North") %>%
  droplevels()

###############################################################################
# 2. FIT MODELS
###############################################################################

m_downy_south <- lmer(
  damage_N ~ prop_s_stand +
    moose_pellets_log_mean +
    red_deer_pellets_log_mean +
    small_deer_pellets_log_mean +
    mean_north_c +
    stem_count_c +
    height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_downy_south,
  REML = TRUE
)

m_silver_south <- lmer(
  damage_N ~ prop_s_stand +
    moose_pellets_log_mean +
    red_deer_pellets_log_mean +
    small_deer_pellets_log_mean +
    mean_north_c +
    stem_count_c +
    height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_silver_south,
  REML = TRUE
)

m_downy_centre <- lmer(
  damage_N ~ prop_s_stand +
    moose_pellets_log_mean +
    red_deer_pellets_log_mean +
    small_deer_pellets_log_mean +
    mean_north_c +
    stem_count_c +
    height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_downy_centre,
  REML = TRUE
)

m_silver_centre <- lmer(
  damage_N ~ prop_s_stand +
    moose_pellets_log_mean +
    red_deer_pellets_log_mean +
    small_deer_pellets_log_mean +
    mean_north_c +
    stem_count_c +
    height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_silver_centre,
  REML = TRUE
)

m_downy_north <- lmer(
  damage_N ~ prop_s_stand +
    moose_pellets_log_mean +
    red_deer_pellets_log_mean +
    small_deer_pellets_log_mean +
    mean_north_c +
    stem_count_c +
    height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_downy_north,
  REML = TRUE
)

m_silver_north <- lmer(
  damage_N ~ prop_s_stand +
    moose_pellets_log_mean +
    red_deer_pellets_log_mean +
    small_deer_pellets_log_mean +
    mean_north_c +
    stem_count_c +
    height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_silver_north,
  REML = TRUE
)

###############################################################################
# 3. MODEL SUMMARIES
###############################################################################

summary(m_downy_south)
summary(m_silver_south)
summary(m_downy_centre)
summary(m_silver_centre)
summary(m_downy_north)
summary(m_silver_north)

r.squaredGLMM(m_downy_south)
r.squaredGLMM(m_silver_south)
r.squaredGLMM(m_downy_centre)
r.squaredGLMM(m_silver_centre)
r.squaredGLMM(m_downy_north)
r.squaredGLMM(m_silver_north)

###############################################################################
# 4. TERM TESTS
###############################################################################

drop1(update(m_downy_south, REML = FALSE), test = "Chisq")
drop1(update(m_silver_south, REML = FALSE), test = "Chisq")
drop1(update(m_downy_centre, REML = FALSE), test = "Chisq")
drop1(update(m_silver_centre, REML = FALSE), test = "Chisq")
drop1(update(m_downy_north, REML = FALSE), test = "Chisq")
drop1(update(m_silver_north, REML = FALSE), test = "Chisq")

###############################################################################
# 5. HTML TABLES
###############################################################################

tab_model(
  m_downy_south,
  m_silver_south,
  m_downy_centre,
  m_silver_centre,
  m_downy_north,
  m_silver_north,
  dv.labels = c(
    "Downy South",
    "Silver South",
    "Downy Centre",
    "Silver Centre",
    "Downy North",
    "Silver North"
  ),
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_six_species_region_deer_models.html")
)

###############################################################################
# 6. COEFFICIENT FIGURE FOR DEER EFFECTS
###############################################################################

coef_deer <- bind_rows(
  tidy(m_downy_south, effects = "fixed") %>%
    mutate(region = "South", species = "Downy"),
  tidy(m_silver_south, effects = "fixed") %>%
    mutate(region = "South", species = "Silver"),
  tidy(m_downy_centre, effects = "fixed") %>%
    mutate(region = "Centre", species = "Downy"),
  tidy(m_silver_centre, effects = "fixed") %>%
    mutate(region = "Centre", species = "Silver"),
  tidy(m_downy_north, effects = "fixed") %>%
    mutate(region = "North", species = "Downy"),
  tidy(m_silver_north, effects = "fixed") %>%
    mutate(region = "North", species = "Silver")
) %>%
  filter(
    term %in% c(
      "moose_pellets_log_mean",
      "red_deer_pellets_log_mean",
      "small_deer_pellets_log_mean"
    )
  ) %>%
  mutate(
    deer = recode(
      term,
      "moose_pellets_log_mean" = "Moose",
      "red_deer_pellets_log_mean" = "Red deer",
      "small_deer_pellets_log_mean" = "Small deer"
    ),
    region = factor(region, levels = c("South", "Centre", "North")),
    species = factor(species, levels = c("Downy", "Silver")),
    deer = factor(deer, levels = c("Moose", "Red deer", "Small deer"))
  )

pal <- c(
  "Downy" = "#3BA64C",
  "Silver" = "#4C5C8C"
)

p_deer_coef <- ggplot(
  coef_deer,
  aes(
    x = region,
    y = estimate,
    colour = species,
    group = species
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey40"
  ) +
  geom_errorbar(
    aes(
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error
    ),
    width = 0.12,
    linewidth = 0.7,
    position = position_dodge(width = 0.45)
  ) +
  geom_point(
    size = 3,
    position = position_dodge(width = 0.45)
  ) +
  facet_wrap(~ deer, nrow = 1, scales = "free_y") +
  scale_colour_manual(values = pal) +
  labs(
    title = "Estimated deer pellet effects by birch species and region",
    x = "Region",
    y = "Model estimate ± 1.96 SE",
    colour = "Birch species"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

print(p_deer_coef)

ggsave(
  file.path(fig_dir, "Figure_six_species_region_deer_model_coefficients.png"),
  p_deer_coef,
  width = 11,
  height = 5,
  dpi = 600
)

ggsave(
  file.path(fig_dir, "Figure_six_species_region_deer_model_coefficients.pdf"),
  p_deer_coef,
  width = 11,
  height = 5
)

###############################################################################
# SIX MODELS: SPECIES × REGION
# Combined deer pellets = moose + red deer + small deer
# Reindeer excluded
###############################################################################

library(dplyr)
library(lme4)
library(lmerTest)
library(MuMIn)
library(sjPlot)
library(broom.mixed)
library(ggplot2)

###############################################################################
# 1. CREATE COMBINED DEER PELLET VARIABLE
# Assumes pellet variables are log1p-transformed:
# raw = exp(log_value) - 1
###############################################################################

df_stand_deer_combined <- df_stand_deer %>%
  mutate(
    combined_deer_pellets =
      (exp(moose_pellets_log_mean) - 1) +
      (exp(red_deer_pellets_log_mean) - 1) +
      (exp(small_deer_pellets_log_mean) - 1),
    
    combined_deer_pellets_log_mean =
      log1p(combined_deer_pellets)
  )

###############################################################################
# 2. CREATE DATASETS
###############################################################################

df_downy_south <- df_stand_deer_combined %>%
  filter(species == "Downy", region == "South") %>%
  droplevels()

df_silver_south <- df_stand_deer_combined %>%
  filter(species == "Silver", region == "South") %>%
  droplevels()

df_downy_centre <- df_stand_deer_combined %>%
  filter(species == "Downy", region == "Centre") %>%
  droplevels()

df_silver_centre <- df_stand_deer_combined %>%
  filter(species == "Silver", region == "Centre") %>%
  droplevels()

df_downy_north <- df_stand_deer_combined %>%
  filter(species == "Downy", region == "North") %>%
  droplevels()

df_silver_north <- df_stand_deer_combined %>%
  filter(species == "Silver", region == "North") %>%
  droplevels()

###############################################################################
# 3. FIT SIX MODELS
###############################################################################

m_combined_downy_south <- lmer(
  damage_N ~ prop_s_stand +
    combined_deer_pellets_log_mean +
    mean_north_c +
    stem_count_c +
    height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_downy_south,
  REML = TRUE
)

m_combined_silver_south <- lmer(
  damage_N ~ prop_s_stand +
    combined_deer_pellets_log_mean +
    mean_north_c +
    stem_count_c +
    height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_silver_south,
  REML = TRUE
)

m_combined_downy_centre <- lmer(
  damage_N ~ prop_s_stand +
    combined_deer_pellets_log_mean +
    mean_north_c +
    stem_count_c +
    height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_downy_centre,
  REML = TRUE
)

m_combined_silver_centre <- lmer(
  damage_N ~ prop_s_stand +
    combined_deer_pellets_log_mean +
    mean_north_c +
    stem_count_c +
    height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_silver_centre,
  REML = TRUE
)

m_combined_downy_north <- lmer(
  damage_N ~ prop_s_stand +
    combined_deer_pellets_log_mean +
    mean_north_c +
    stem_count_c +
    height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_downy_north,
  REML = TRUE
)

m_combined_silver_north <- lmer(
  damage_N ~ prop_s_stand +
    combined_deer_pellets_log_mean +
    mean_north_c +
    stem_count_c +
    height_c +
    pct +
    year +
    (1 | area/stand_id),
  data = df_silver_north,
  REML = TRUE
)

###############################################################################
# 4. MODEL OUTPUTS
###############################################################################

summary(m_combined_downy_south)
summary(m_combined_silver_south)
summary(m_combined_downy_centre)
summary(m_combined_silver_centre)
summary(m_combined_downy_north)
summary(m_combined_silver_north)

r.squaredGLMM(m_combined_downy_south)
r.squaredGLMM(m_combined_silver_south)
r.squaredGLMM(m_combined_downy_centre)
r.squaredGLMM(m_combined_silver_centre)
r.squaredGLMM(m_combined_downy_north)
r.squaredGLMM(m_combined_silver_north)

drop1(update(m_combined_downy_south, REML = FALSE), test = "Chisq")
drop1(update(m_combined_silver_south, REML = FALSE), test = "Chisq")
drop1(update(m_combined_downy_centre, REML = FALSE), test = "Chisq")
drop1(update(m_combined_silver_centre, REML = FALSE), test = "Chisq")
drop1(update(m_combined_downy_north, REML = FALSE), test = "Chisq")
drop1(update(m_combined_silver_north, REML = FALSE), test = "Chisq")

###############################################################################
# 5. HTML TABLE
###############################################################################

tab_model(
  m_combined_downy_south,
  m_combined_silver_south,
  m_combined_downy_centre,
  m_combined_silver_centre,
  m_combined_downy_north,
  m_combined_silver_north,
  dv.labels = c(
    "Downy South",
    "Silver South",
    "Downy Centre",
    "Silver Centre",
    "Downy North",
    "Silver North"
  ),
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_species_region_combined_deer_models.html")
)

###############################################################################
# 6. COEFFICIENT FIGURE
###############################################################################

coef_combined_deer <- bind_rows(
  tidy(m_combined_downy_south, effects = "fixed") %>%
    mutate(region = "South", species = "Downy"),
  tidy(m_combined_silver_south, effects = "fixed") %>%
    mutate(region = "South", species = "Silver"),
  tidy(m_combined_downy_centre, effects = "fixed") %>%
    mutate(region = "Centre", species = "Downy"),
  tidy(m_combined_silver_centre, effects = "fixed") %>%
    mutate(region = "Centre", species = "Silver"),
  tidy(m_combined_downy_north, effects = "fixed") %>%
    mutate(region = "North", species = "Downy"),
  tidy(m_combined_silver_north, effects = "fixed") %>%
    mutate(region = "North", species = "Silver")
) %>%
  filter(term == "combined_deer_pellets_log_mean") %>%
  mutate(
    region = factor(region, levels = c("South", "Centre", "North")),
    species = factor(species, levels = c("Downy", "Silver"))
  )

pal <- c(
  "Downy" = "#3BA64C",
  "Silver" = "#4C5C8C"
)

p_combined_deer <- ggplot(
  coef_combined_deer,
  aes(
    x = region,
    y = estimate,
    colour = species,
    group = species
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey40"
  ) +
  geom_errorbar(
    aes(
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error
    ),
    width = 0.12,
    linewidth = 0.7,
    position = position_dodge(width = 0.45)
  ) +
  geom_point(
    size = 3,
    position = position_dodge(width = 0.45)
  ) +
  scale_colour_manual(values = pal) +
  labs(
    title = "Combined deer pellet effect by birch species and region",
    subtitle = "Moose + red deer + small deer; reindeer excluded",
    x = "Region",
    y = "Model estimate ± 1.96 SE",
    colour = "Birch species"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )

print(p_combined_deer)

ggsave(
  file.path(fig_dir, "Figure_species_region_combined_deer_coefficients.png"),
  p_combined_deer,
  width = 8,
  height = 5,
  dpi = 600
)

ggsave(
  file.path(fig_dir, "Figure_species_region_combined_deer_coefficients.pdf"),
  p_combined_deer,
  width = 8,
  height = 5
)

###############################################################################
# FOREST PLOT: prop_s_stand SLOPES ACROSS YOUR MODEL SET
###############################################################################

library(dplyr)
library(emmeans)
library(broom.mixed)
library(ggplot2)

pal <- c(
  "Downy"  = "#3BA64C",
  "Silver" = "#4C5C8C"
)

###############################################################################
# Helper for simple single-species models
###############################################################################

extract_prop_slope <- function(model, model_group, model_label, region_label, species_label) {
  
  broom.mixed::tidy(model, effects = "fixed") %>%
    filter(term == "prop_s_stand") %>%
    transmute(
      model_group = model_group,
      model = model_label,
      region = region_label,
      species = species_label,
      estimate = estimate,
      SE = std.error,
      lower = estimate - 1.96 * std.error,
      upper = estimate + 1.96 * std.error,
      p.value = p.value
    )
}

###############################################################################
# 1. Main stand species-interaction model
###############################################################################

main_prop_trends <- emtrends(
  m_stand_species_int,
  ~ species,
  var = "prop_s_stand"
) %>%
  summary(infer = TRUE) %>%
  as.data.frame() %>%
  transmute(
    model_group = "Main stand model",
    model = "All areas",
    region = "All",
    species = as.character(species),
    estimate = prop_s_stand.trend,
    SE = SE,
    lower = lower.CL,
    upper = upper.CL,
    p.value = p.value
  )

###############################################################################
# 2. Stand regional interaction model
###############################################################################

region_prop_trends <- emtrends(
  m_stand_region_int,
  ~ species | region,
  var = "prop_s_stand"
) %>%
  summary(infer = TRUE) %>%
  as.data.frame() %>%
  transmute(
    model_group = "Regional interaction model",
    model = as.character(region),
    region = as.character(region),
    species = as.character(species),
    estimate = prop_s_stand.trend,
    SE = SE,
    lower = lower.CL,
    upper = upper.CL,
    p.value = p.value
  )

###############################################################################
# 3. Species-only stand models
###############################################################################

species_only_prop <- bind_rows(
  extract_prop_slope(
    m_stand_downy,
    "Species-only stand model",
    "Downy only",
    "All",
    "Downy"
  ),
  extract_prop_slope(
    m_stand_silver,
    "Species-only stand model",
    "Silver only",
    "All",
    "Silver"
  )
)

###############################################################################
# 4. Separate regional stand models
###############################################################################

get_region_model_trends <- function(model, region_label) {
  
  emtrends(
    model,
    ~ species,
    var = "prop_s_stand"
  ) %>%
    summary(infer = TRUE) %>%
    as.data.frame() %>%
    transmute(
      model_group = "Separate regional stand model",
      model = region_label,
      region = region_label,
      species = as.character(species),
      estimate = prop_s_stand.trend,
      SE = SE,
      lower = lower.CL,
      upper = upper.CL,
      p.value = p.value
    )
}

regional_separate_prop <- bind_rows(
  get_region_model_trends(m_stand_south, "South"),
  get_region_model_trends(m_stand_centre, "Centre"),
  get_region_model_trends(m_stand_north, "North")
)

###############################################################################
# 5. Optional: six species × region models, if they exist
###############################################################################

six_model_list <- list(
  "Downy South" = list(object = "m_downy_south", region = "South", species = "Downy"),
  "Silver South" = list(object = "m_silver_south", region = "South", species = "Silver"),
  "Downy Centre" = list(object = "m_downy_centre", region = "Centre", species = "Downy"),
  "Silver Centre" = list(object = "m_silver_centre", region = "Centre", species = "Silver"),
  "Downy North" = list(object = "m_downy_north", region = "North", species = "Downy"),
  "Silver North" = list(object = "m_silver_north", region = "North", species = "Silver")
)

six_species_region_prop <- bind_rows(
  lapply(names(six_model_list), function(label) {
    
    obj_name <- six_model_list[[label]]$object
    
    if (exists(obj_name)) {
      extract_prop_slope(
        get(obj_name),
        "Species × region subset model",
        label,
        six_model_list[[label]]$region,
        six_model_list[[label]]$species
      )
    } else {
      NULL
    }
  })
)

###############################################################################
# 6. Combine all estimates
###############################################################################

prop_slope_forest <- bind_rows(
  main_prop_trends,
  region_prop_trends,
  species_only_prop,
  regional_separate_prop,
  six_species_region_prop
) %>%
  mutate(
    species = factor(species, levels = c("Downy", "Silver")),
    model_group = factor(
      model_group,
      levels = c(
        "Main stand model",
        "Regional interaction model",
        "Species-only stand model",
        "Separate regional stand model",
        "Species × region subset model"
      )
    ),
    label = paste(model, species, sep = " — "),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE ~ ""
    )
  )


print(prop_slope_forest)
###############################################################################
# 7. Forest plot
###############################################################################

p_prop_forest <- ggplot(
  prop_slope_forest,
  aes(
    x = estimate,
    y = reorder(label, estimate),
    colour = species
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey40"
  ) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.18,
    linewidth = 0.7
  ) +
  geom_point(size = 3) +
  geom_text(
    aes(label = sig),
    nudge_x = 1.2,
    size = 5,
    show.legend = FALSE
  ) +
  facet_wrap(
    ~ model_group,
    scales = "free_y",
    ncol = 1
  ) +
  scale_colour_manual(values = pal) +
  labs(
    title = "Estimated effects of stand-level Silver birch proportion",
    subtitle = "Comparison of prop_s_stand slopes across stand-level model structures",
    x = "Estimated slope for prop_s_stand",
    y = NULL,
    colour = "Birch species"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 8)
  )

print(p_prop_forest)

###############################################################################
# 8. Save figure and table
###############################################################################

ggsave(
  file.path(fig_dir, "Figure_prop_s_stand_slope_forest_plot.png"),
  p_prop_forest,
  width = 12,
  height = 11,
  dpi = 600
)

ggsave(
  file.path(fig_dir, "Figure_prop_s_stand_slope_forest_plot.pdf"),
  p_prop_forest,
  width = 12,
  height = 11
)

write.csv(
  prop_slope_forest,
  file.path(tab_dir, "prop_s_stand_slope_forest_table.csv"),
  row.names = FALSE
)

###############################################################################
# CLEAN COEFFICIENT PLOT: prop_s_stand SLOPES
# Downy vs Silver across model structures
###############################################################################

library(dplyr)
library(emmeans)
library(broom.mixed)
library(ggplot2)

pal <- c(
  "Downy"  = "#3BA64C",
  "Silver" = "#4C5C8C"
)

###############################################################################
# Helper for models without species interaction
###############################################################################

extract_simple_prop <- function(model, section, comparison, species_label) {
  
  broom.mixed::tidy(model, effects = "fixed") %>%
    filter(term == "prop_s_stand") %>%
    transmute(
      section = section,
      comparison = comparison,
      species = species_label,
      estimate = estimate,
      SE = std.error,
      lower = estimate - 1.96 * std.error,
      upper = estimate + 1.96 * std.error,
      p.value = p.value
    )
}

###############################################################################
# 1. MAIN STAND MODEL
###############################################################################

main_prop <- emtrends(
  m_stand_species_int,
  ~ species,
  var = "prop_s_stand"
) %>%
  summary(infer = TRUE) %>%
  as.data.frame() %>%
  transmute(
    section = "Overall Sweden",
    comparison = "Main stand model",
    species = as.character(species),
    estimate = prop_s_stand.trend,
    SE = SE,
    lower = lower.CL,
    upper = upper.CL,
    p.value = p.value
  )

###############################################################################
# 2. REGIONAL INTERACTION MODEL
###############################################################################

regional_prop <- emtrends(
  m_stand_region_int,
  ~ species | region,
  var = "prop_s_stand"
) %>%
  summary(infer = TRUE) %>%
  as.data.frame() %>%
  transmute(
    section = "Regional interaction model",
    comparison = as.character(region),
    species = as.character(species),
    estimate = prop_s_stand.trend,
    SE = SE,
    lower = lower.CL,
    upper = upper.CL,
    p.value = p.value
  )

###############################################################################
# 3. SPECIES-ONLY STAND MODELS
###############################################################################

species_only_prop <- bind_rows(
  extract_simple_prop(
    m_stand_downy,
    "Species-only stand models",
    "Downy-only model",
    "Downy"
  ),
  extract_simple_prop(
    m_stand_silver,
    "Species-only stand models",
    "Silver-only model",
    "Silver"
  )
)

###############################################################################
# 4. SEPARATE REGIONAL STAND MODELS
###############################################################################

get_regional_model_prop <- function(model, region_name) {
  
  emtrends(
    model,
    ~ species,
    var = "prop_s_stand"
  ) %>%
    summary(infer = TRUE) %>%
    as.data.frame() %>%
    transmute(
      section = "Separate regional models",
      comparison = region_name,
      species = as.character(species),
      estimate = prop_s_stand.trend,
      SE = SE,
      lower = lower.CL,
      upper = upper.CL,
      p.value = p.value
    )
}

separate_region_prop <- bind_rows(
  get_regional_model_prop(m_stand_south, "South"),
  get_regional_model_prop(m_stand_centre, "Centre"),
  get_regional_model_prop(m_stand_north, "North")
)

###############################################################################
# 5. SIX SPECIES × REGION SUBSET MODELS
# Only included if these model objects exist
###############################################################################

six_models <- list(
  list(obj = "m_downy_south",  section = "Species × region subset models", comparison = "South",  species = "Downy"),
  list(obj = "m_silver_south", section = "Species × region subset models", comparison = "South",  species = "Silver"),
  list(obj = "m_downy_centre",  section = "Species × region subset models", comparison = "Centre", species = "Downy"),
  list(obj = "m_silver_centre", section = "Species × region subset models", comparison = "Centre", species = "Silver"),
  list(obj = "m_downy_north",  section = "Species × region subset models", comparison = "North",  species = "Downy"),
  list(obj = "m_silver_north", section = "Species × region subset models", comparison = "North",  species = "Silver")
)

six_prop <- bind_rows(
  lapply(six_models, function(x) {
    
    if (exists(x$obj)) {
      extract_simple_prop(
        get(x$obj),
        x$section,
        x$comparison,
        x$species
      )
    } else {
      NULL
    }
  })
)

###############################################################################
# 6. COMBINE
###############################################################################

prop_plot_df <- bind_rows(
  main_prop,
  regional_prop,
  species_only_prop,
  separate_region_prop,
  six_prop
) %>%
  mutate(
    species = factor(species, levels = c("Downy", "Silver")),
    section = factor(
      section,
      levels = c(
        "Overall Sweden",
        "Regional interaction model",
        "Species-only stand models",
        "Separate regional models",
        "Species × region subset models"
      )
    ),
    comparison = factor(
      comparison,
      levels = c(
        "Main stand model",
        "South",
        "Centre",
        "North",
        "Downy-only model",
        "Silver-only model"
      )
    ),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE ~ ""
    )
  )
###############################################################################
# PUBLICATION COEFFICIENT PLOT: prop_s_stand SLOPES
# Includes beta estimates, significance stars, and sample sizes
###############################################################################

library(dplyr)
library(ggplot2)

###############################################################################
# ADD LABELS
###############################################################################

prop_plot_df <- prop_plot_df %>%
  mutate(
    beta_label = paste0("\u03B2 = ", round(estimate, 1)),
    
    n = case_when(
      comparison == "Main stand model" ~ 788,
      
      comparison == "Downy-only model" ~ 403,
      comparison == "Silver-only model" ~ 385,
      
      section == "Regional interaction model" & comparison == "South" ~ 244,
      section == "Regional interaction model" & comparison == "Centre" ~ 170,
      section == "Regional interaction model" & comparison == "North" ~ 392,
      
      section == "Separate regional models" & comparison == "South" ~ 244,
      section == "Separate regional models" & comparison == "Centre" ~ 170,
      section == "Separate regional models" & comparison == "North" ~ 392,
      
      section == "Species × region subset models" &
        comparison == "South" & species == "Downy" ~ 79,
      
      section == "Species × region subset models" &
        comparison == "South" & species == "Silver" ~ 165,
      
      section == "Species × region subset models" &
        comparison == "Centre" & species == "Downy" ~ 94,
      
      section == "Species × region subset models" &
        comparison == "Centre" & species == "Silver" ~ 76,
      
      section == "Species × region subset models" &
        comparison == "North" & species == "Downy" ~ 230,
      
      section == "Species × region subset models" &
        comparison == "North" & species == "Silver" ~ 162,
      
      TRUE ~ NA_real_
    ),
    
    n_label = paste0("n = ", n),
    
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE ~ ""
    ),
    
    species = factor(species, levels = c("Downy", "Silver"))
  )

###############################################################################
# DODGE POSITION
###############################################################################

dodge <- position_dodge(width = 0.55)

###############################################################################
# FIGURE
###############################################################################

p_prop_publication <- ggplot(
  prop_plot_df,
  aes(
    x = comparison,
    y = estimate,
    colour = species,
    group = species
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey45",
    linewidth = 0.5
  ) +
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    width = 0.12,
    linewidth = 0.8,
    position = dodge
  ) +
  geom_point(
    size = 3.8,
    position = dodge
  ) +
  geom_text(
    aes(
      label = beta_label,
      y = estimate + 4
    ),
    position = dodge,
    size = 3.2,
    fontface = "bold",
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      label = sig,
      y = estimate + 9
    ),
    position = dodge,
    size = 5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      label = n_label,
      y = estimate - 8
    ),
    position = dodge,
    colour = "grey35",
    size = 3,
    show.legend = FALSE
  ) +
  facet_grid(
    section ~ .,
    scales = "free_x",
    space = "free_x"
  ) +
  scale_colour_manual(
    values = c(
      "Downy"  = "#3BA64C",
      "Silver" = "#4C5C8C"
    )
  ) +
  labs(
    title = "Estimated effect of stand-level Silver birch proportion",
    subtitle = "Comparison of prop_s_stand coefficients across model structures",
    x = NULL,
    y = expression(beta~"estimate for "*italic(prop[s~stand])),
    colour = "Birch species"
  ) +
  coord_cartesian(clip = "off") +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    strip.text.y = element_text(face = "bold", size = 11),
    axis.text.x = element_text(angle = 35, hjust = 1, face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.spacing = unit(1.1, "lines")
  )

print(p_prop_publication)

###############################################################################
# SAVE
###############################################################################

ggsave(
  file.path(fig_dir, "Figure_prop_s_stand_publication.png"),
  p_prop_publication,
  width = 10,
  height = 10,
  dpi = 600
)

ggsave(
  file.path(fig_dir, "Figure_prop_s_stand_publication.pdf"),
  p_prop_publication,
  width = 10,
  height = 10
)

###############################################################################
# FINAL STAND-LEVEL MODELS FOR PAPER
# Main model, species-specific models, region-specific models
# Moose version + total cervid check
###############################################################################

library(dplyr)
library(lme4)
library(lmerTest)
library(MuMIn)
library(performance)
library(sjPlot)

###############################################################################
# 1. MAIN STAND SPECIES INTERACTION MODEL — MOOSE
###############################################################################

m_stand_species_int <- lmer(
  damage_N ~ species * prop_s_stand +
    species * mean_north_c +
    species * stem_count_c +
    species * height_c +
    species * moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_region_stand,
  REML = TRUE
)

summary(m_stand_species_int)
r.squaredGLMM(m_stand_species_int)
drop1(update(m_stand_species_int, REML = FALSE), test = "Chisq")
check_collinearity(m_stand_species_int)

###############################################################################
# 2. SPECIES-SPECIFIC STAND MODELS — MOOSE
###############################################################################

df_stand_downy <- df_region_stand %>%
  filter(species == "Downy") %>%
  droplevels()

df_stand_silver <- df_region_stand %>%
  filter(species == "Silver") %>%
  droplevels()

m_stand_downy <- lmer(
  damage_N ~ prop_s_stand +
    mean_north_c +
    stem_count_c +
    height_c +
    moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_downy,
  REML = TRUE
)

m_stand_silver <- lmer(
  damage_N ~ prop_s_stand +
    mean_north_c +
    stem_count_c +
    height_c +
    moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_silver,
  REML = TRUE
)

summary(m_stand_downy)
summary(m_stand_silver)

r.squaredGLMM(m_stand_downy)
r.squaredGLMM(m_stand_silver)

drop1(update(m_stand_downy, REML = FALSE), test = "Chisq")
drop1(update(m_stand_silver, REML = FALSE), test = "Chisq")

###############################################################################
# 3. REGION-SPECIFIC STAND MODELS — MOOSE
###############################################################################

df_stand_south <- df_region_stand %>%
  filter(region == "South") %>%
  droplevels()

df_stand_centre <- df_region_stand %>%
  filter(region == "Centre") %>%
  droplevels()

df_stand_north <- df_region_stand %>%
  filter(region == "North") %>%
  droplevels()

m_stand_south <- lmer(
  damage_N ~ species * prop_s_stand +
    species * height_c +
    species * stem_count_c +
    species * moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_south,
  REML = TRUE
)

m_stand_centre <- lmer(
  damage_N ~ species * prop_s_stand +
    species * height_c +
    species * stem_count_c +
    species * moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_centre,
  REML = TRUE
)

m_stand_north <- lmer(
  damage_N ~ species * prop_s_stand +
    species * height_c +
    species * stem_count_c +
    species * moose_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_north,
  REML = TRUE
)

summary(m_stand_south)
summary(m_stand_centre)
summary(m_stand_north)

r.squaredGLMM(m_stand_south)
r.squaredGLMM(m_stand_centre)
r.squaredGLMM(m_stand_north)

drop1(update(m_stand_south, REML = FALSE), test = "Chisq")
drop1(update(m_stand_centre, REML = FALSE), test = "Chisq")
drop1(update(m_stand_north, REML = FALSE), test = "Chisq")

###############################################################################
# 4. HTML TABLES — MOOSE MODELS
###############################################################################

tab_model(
  m_stand_species_int,
  dv.labels = "Stand species interaction",
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_stand_species_interaction_moose.html")
)

tab_model(
  m_stand_downy,
  m_stand_silver,
  dv.labels = c(
    "S7 Stand Downy only",
    "S8 Stand Silver only"
  ),
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_stand_species_specific_moose.html")
)

tab_model(
  m_stand_south,
  m_stand_centre,
  m_stand_north,
  dv.labels = c(
    "South stand model",
    "Centre stand model",
    "North stand model"
  ),
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_stand_region_specific_moose.html")
)

###############################################################################
# 5. TOTAL CERVID CHECK — MAIN MODEL
###############################################################################

m_stand_species_int_cervid <- lmer(
  damage_N ~ species * prop_s_stand +
    species * mean_north_c +
    species * stem_count_c +
    species * height_c +
    species * total_cervid_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_region_stand,
  REML = TRUE
)

summary(m_stand_species_int_cervid)
r.squaredGLMM(m_stand_species_int_cervid)
drop1(update(m_stand_species_int_cervid, REML = FALSE), test = "Chisq")
check_collinearity(m_stand_species_int_cervid)

###############################################################################
# 6. TOTAL CERVID CHECK — SPECIES-SPECIFIC MODELS
###############################################################################

m_stand_downy_cervid <- lmer(
  damage_N ~ prop_s_stand +
    mean_north_c +
    stem_count_c +
    height_c +
    total_cervid_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_downy,
  REML = TRUE
)

m_stand_silver_cervid <- lmer(
  damage_N ~ prop_s_stand +
    mean_north_c +
    stem_count_c +
    height_c +
    total_cervid_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_silver,
  REML = TRUE
)

summary(m_stand_downy_cervid)
summary(m_stand_silver_cervid)

r.squaredGLMM(m_stand_downy_cervid)
r.squaredGLMM(m_stand_silver_cervid)

drop1(update(m_stand_downy_cervid, REML = FALSE), test = "Chisq")
drop1(update(m_stand_silver_cervid, REML = FALSE), test = "Chisq")

###############################################################################
# 7. TOTAL CERVID CHECK — REGION-SPECIFIC MODELS
###############################################################################

m_stand_south_cervid <- lmer(
  damage_N ~ species * prop_s_stand +
    species * height_c +
    species * stem_count_c +
    species * total_cervid_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_south,
  REML = TRUE
)

m_stand_centre_cervid <- lmer(
  damage_N ~ species * prop_s_stand +
    species * height_c +
    species * stem_count_c +
    species * total_cervid_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_centre,
  REML = TRUE
)

m_stand_north_cervid <- lmer(
  damage_N ~ species * prop_s_stand +
    species * height_c +
    species * stem_count_c +
    species * total_cervid_pellets_log_mean +
    pct +
    year +
    (1 | area/stand_id),
  data = df_stand_north,
  REML = TRUE
)

summary(m_stand_south_cervid)
summary(m_stand_centre_cervid)
summary(m_stand_north_cervid)

r.squaredGLMM(m_stand_south_cervid)
r.squaredGLMM(m_stand_centre_cervid)
r.squaredGLMM(m_stand_north_cervid)

drop1(update(m_stand_south_cervid, REML = FALSE), test = "Chisq")
drop1(update(m_stand_centre_cervid, REML = FALSE), test = "Chisq")
drop1(update(m_stand_north_cervid, REML = FALSE), test = "Chisq")

###############################################################################
# 8. HTML TABLES — TOTAL CERVID CHECK
###############################################################################

tab_model(
  m_stand_species_int_cervid,
  dv.labels = "Stand species interaction: total cervid",
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_stand_species_interaction_total_cervid.html")
)

tab_model(
  m_stand_downy_cervid,
  m_stand_silver_cervid,
  dv.labels = c(
    "Downy only: total cervid",
    "Silver only: total cervid"
  ),
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_stand_species_specific_total_cervid.html")
)

tab_model(
  m_stand_south_cervid,
  m_stand_centre_cervid,
  m_stand_north_cervid,
  dv.labels = c(
    "South stand model: total cervid",
    "Centre stand model: total cervid",
    "North stand model: total cervid"
  ),
  show.se = TRUE,
  show.ci = FALSE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "Table_stand_region_specific_total_cervid.html")
)

