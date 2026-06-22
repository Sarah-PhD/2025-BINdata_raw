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
    year +
    (1 | area/stand_id/plot_id),
  data = df_north,
  REML = TRUE
)

m_centre_nested <- lmer(
  damage_N ~ species * prop_s_plot +
    species * height_c +
    species * stem_count_c +
    year +
    (1 | area/stand_id/plot_id),
  data = df_centre,
  REML = TRUE
)

m_south_nested <- lmer(
  damage_N ~ species * prop_s_plot +
    species * height_c +
    species * stem_count_c +
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
    
    region = factor(region, levels = c("South", "Centre", "North")),
    pct = factor(pct),
    year = droplevels(as.factor(year)),
    species = droplevels(as.factor(species)),
    area = factor(area)
  ) %>%
  filter(
    !is.na(damage_N),
    !is.na(species),
    !is.na(prop_s_stand),
    !is.na(region),
    !is.na(height_c),
    !is.na(stem_count_c),
    !is.na(pct),
    !is.na(year)
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
  show.ci = TRUE,
  show.stat = TRUE,
  show.p = TRUE,
  show.re.var = TRUE,
  show.icc = TRUE,
  show.r2 = TRUE,
  show.ngroups = TRUE,
  file = file.path(tab_dir, "stand_level_models.html")
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
  show.ci = TRUE,
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
# Table S5 — emtrends slopes as HTML
###############################################################################

library(emmeans)
library(dplyr)
library(gt)

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

stand_emtrends_table %>%
  gt() %>%
  fmt_number(
    columns = c(slope, SE, lower_CI, upper_CI, t.ratio, p_value),
    decimals = 3
  ) %>%
  tab_header(
    title = "Table S5. Estimated slopes from stand-level regional model"
  ) %>%
  gtsave(
    file.path(tab_dir, "Table_S5_stand_emtrends_slopes.html")
  )

###############################################################################
# Table S6 — emtrends pairwise comparisons as HTML
###############################################################################

stand_emtrends_pairs <- as.data.frame(
  pairs(stand_emtrends)
)

stand_emtrends_pairs %>%
  gt() %>%
  fmt_number(
    columns = c(estimate, SE, t.ratio, p.value),
    decimals = 3
  ) %>%
  tab_header(
    title = "Table S6. Pairwise species comparisons of stand-level slopes"
  ) %>%
  gtsave(
    file.path(tab_dir, "Table_S6_stand_emtrends_pairwise_comparisons.html")
  )

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
  show.ci = TRUE,
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

