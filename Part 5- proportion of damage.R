##############################################################################
# ÄBIN2008_2025 — Pine stems + winter browsing proportion (clean flow)
##############################################################################

library(dplyr)
library(stringr)

ÄBIN2008_2025 <- read.csv("ÄBIN2008_2025.csv")
names(ÄBIN2008_2025) <- gsub("\\.", "_", names(ÄBIN2008_2025))
# ----------------------------
# 1) Define allowed damage codes and identify pine combo columns
# ----------------------------
codes <- c("fts","o","od","fb","fs","ps","ss")

pine_combo_regex <- paste0(
  "^pine_(",
  paste(codes, collapse = "|"),
  ")(,(",
  paste(codes, collapse = "|"),
  "))*$"
)

pine_combo_cols <- names(ÄBIN2008_2025)[str_detect(names(ÄBIN2008_2025), pine_combo_regex)]

# Total stems = all pine combo cols + pine_unbrowsed (if it exists)
pine_sum_cols <- intersect(c(pine_combo_cols, "pine_unbrowsed"), names(ÄBIN2008_2025))
if (length(pine_sum_cols) == 0) stop("No pine stem columns found (pine_sum_cols is empty).")

# Winter browsing = any combo containing fts OR fb OR fs as a token
winter_tokens <- c("fts","fb","fs")
winter_regex  <- paste0("(^pine_|,)(?:", paste(winter_tokens, collapse="|"), ")(,|$)")
winter_cols   <- pine_combo_cols[str_detect(pine_combo_cols, winter_regex)]
if (length(winter_cols) == 0) stop("No winter browsing columns found (winter_cols is empty).")

# ----------------------------
# 2) Helpers: safe numeric conversion + rowSums that keeps NA if all NA
# ----------------------------
to_num <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x))  return(x)
  suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))
}

rowSums_keepNA_df <- function(df_subset) {
  m <- as.data.frame(lapply(df_subset, to_num))
  rs <- rowSums(m, na.rm = TRUE)
  rs[rowSums(!is.na(m)) == 0] <- NA_real_
  rs
}

# ----------------------------
# 3) Compute Pine_stem, winter stems, and proportion ON ÄBIN2008_2025
# ----------------------------
ÄBIN2008_2025 <- ÄBIN2008_2025 %>%
  # convert only columns we actually use (keeps df lighter + avoids surprises)
  mutate(across(all_of(unique(c(pine_sum_cols, winter_cols))), to_num)) %>%
  mutate(
    Pine_stem = rowSums_keepNA_df(select(., all_of(pine_sum_cols))),
    pine_winter_browsed_stems = rowSums_keepNA_df(select(., all_of(winter_cols))),
    winter_browsing = if_else(
      is.na(Pine_stem) | Pine_stem == 0,
      NA_real_,
      pine_winter_browsed_stems / Pine_stem
    )
  )

# ----------------------------
# 4) Optional quick checks
# ----------------------------
cat("Pine stem columns used (first 30):\n"); print(head(pine_sum_cols, 30))
cat("\nWinter browsing columns used:\n"); print(winter_cols)

summary(ÄBIN2008_2025$Pine_stem)
summary(ÄBIN2008_2025$pine_winter_browsed_stems)
summary(ÄBIN2008_2025$winter_browsing)

###############################################################################
##################    TIDY UP   ###############################################
########## Deleting some columns and ordering others###########################
###############################################################################

library(dplyr)

names(ÄBIN2008_2025)

glimpse(ÄBIN2008_2025)

library(dplyr)
ÄBIN2008_2025 <- ÄBIN2008_2025 %>%
  relocate(Area, Year, Stand, Plot, Age)
################################################################################
# ----------------------------------------------------------------------------#
#                     Now i will include summer browsing
#-----------------------------------------------------------------------------#
###############################################################################

library(dplyr)
library(stringr)

# ----------------------------
# 1) Identify pine damage-combo columns (same definition as before)
# ----------------------------
codes <- c("fts","o","od","fb","fs","ps","ss")

pine_combo_regex <- paste0(
  "^pine_(",
  paste(codes, collapse = "|"),
  ")(,(",
  paste(codes, collapse = "|"),
  "))*$"
)

pine_combo_cols <- names(ÄBIN2008_2025)[str_detect(names(ÄBIN2008_2025), pine_combo_regex)]

# Total stems columns (for denominator Pine_stem)
pine_sum_cols <- intersect(c(pine_combo_cols, "pine_unbrowsed"), names(ÄBIN2008_2025))
if (length(pine_sum_cols) == 0) stop("No pine stem columns found (pine_sum_cols is empty).")

# ----------------------------
# 2) Helpers
# ----------------------------
to_num <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x))  return(x)
  suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))
}

rowSums_keepNA_df <- function(df_subset) {
  m <- as.data.frame(lapply(df_subset, to_num))
  rs <- rowSums(m, na.rm = TRUE)
  rs[rowSums(!is.na(m)) == 0] <- NA_real_
  rs
}

# Ensure numeric only for columns we will use
ÄBIN2008_2025 <- ÄBIN2008_2025 %>%
  mutate(across(all_of(pine_sum_cols), to_num))

# If Pine_stem doesn't exist yet, compute it (safe)
if (!"Pine_stem" %in% names(ÄBIN2008_2025)) {
  ÄBIN2008_2025 <- ÄBIN2008_2025 %>%
    mutate(Pine_stem = rowSums_keepNA_df(select(., all_of(pine_sum_cols))))
}

# ----------------------------
# 3) SUMMER browsing = any combo containing 'ps' as a token
# ----------------------------
summer_tokens <- c("ps")
summer_regex  <- paste0("(^pine_|,)(?:", paste(summer_tokens, collapse="|"), ")(,|$)")
summer_cols   <- pine_combo_cols[str_detect(pine_combo_cols, summer_regex)]

if (length(summer_cols) == 0) stop("No summer browsing columns found (summer_cols is empty).")

ÄBIN2008_2025 <- ÄBIN2008_2025 %>%
  mutate(across(all_of(summer_cols), to_num)) %>%
  mutate(
    pine_summer_browsed_stems = rowSums_keepNA_df(select(., all_of(summer_cols))),
    summer_browsing = if_else(
      is.na(Pine_stem) | Pine_stem == 0,
      NA_real_,
      pine_summer_browsed_stems / Pine_stem
    )
  )

# ----------------------------
# 4) REBROWSING = fresh (fts, fb, fs, ss, ps) + old browsing (od)
# ----------------------------
rebrows_tokens <- c("fts","fb","fs","ss","ps","od")
rebrows_regex  <- paste0("(^pine_|,)(?:", paste(rebrows_tokens, collapse="|"), ")(,|$)")
rebrows_cols   <- pine_combo_cols[str_detect(pine_combo_cols, rebrows_regex)]

if (length(rebrows_cols) == 0) stop("No rebrows columns found (rebrows_cols is empty).")

ÄBIN2008_2025 <- ÄBIN2008_2025 %>%
  mutate(across(all_of(rebrows_cols), to_num)) %>%
  mutate(
    pine_rebrowsed_stems = rowSums_keepNA_df(select(., all_of(rebrows_cols))),
    rebrowsing = if_else(
      is.na(Pine_stem) | Pine_stem == 0,
      NA_real_,
      pine_rebrowsed_stems / Pine_stem
    )
  )

# ----------------------------
# 5) Optional sanity prints
# ----------------------------
cat("Summer columns (ps):\n"); print(summer_cols)
cat("\nRebrows columns (fts,fb,fs,ss,ps + od):\n"); print(rebrows_cols)

summary(ÄBIN2008_2025$pine_summer_browsed_stems)
summary(ÄBIN2008_2025$summer_browsing)
summary(ÄBIN2008_2025$pine_rebrowsed_stems)
summary(ÄBIN2008_2025$rebrowsing)

##############################################################################
##                   Spruce and contorta too                                 ##
###############################################################################

library(dplyr)
library(stringr)

to_num <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x))  return(x)
  suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))
}

rowSums_keepNA_df <- function(df_subset) {
  m <- as.data.frame(lapply(df_subset, to_num))
  rs <- rowSums(m, na.rm = TRUE)
  rs[rowSums(!is.na(m)) == 0] <- NA_real_
  rs
}

add_props_dotcombo <- function(df, prefix, codes, ub_col,
                               winter_tokens = c("fts","fb","fs"),
                               summer_token  = "ps",
                               rebrows_tokens = c("fts","fb","fs","ss","ps","od")) {
  
  # combo columns like: spruce_fts, spruce_fts.od, contorta_fb.o.od, etc.
  combo_regex <- paste0(
    "^", prefix, "_(",
    paste(codes, collapse="|"),
    ")(\\.(",
    paste(codes, collapse="|"),
    "))*$"
  )
  combo_cols <- names(df)[str_detect(names(df), combo_regex)]
  if (length(combo_cols) == 0) stop("No combo columns found for ", prefix)
  
  # Total stems = all combo columns + ub column (if present)
  sum_cols <- intersect(c(combo_cols, ub_col), names(df))
  if (length(sum_cols) == 0) stop("No stem columns found for ", prefix, " (sum_cols empty)")
  
  # token boundary regex with dot separator
  token_regex <- function(tokens) {
    paste0("(^", prefix, "_|\\.)(?:", paste(tokens, collapse="|"), ")(\\.|$)")
  }
  
  # Only keep tokens that exist for this species (spruce has no ss)
  winter_tokens_use  <- intersect(winter_tokens, codes)
  summer_tokens_use  <- intersect(c(summer_token), codes)
  rebrows_tokens_use <- intersect(rebrows_tokens, codes)
  
  winter_cols  <- combo_cols[str_detect(combo_cols, token_regex(winter_tokens_use))]
  summer_cols  <- combo_cols[str_detect(combo_cols, token_regex(summer_tokens_use))]
  rebrows_cols <- combo_cols[str_detect(combo_cols, token_regex(rebrows_tokens_use))]
  
  # Convert only needed columns
  df <- df %>%
    mutate(across(all_of(unique(c(sum_cols, winter_cols, summer_cols, rebrows_cols))), to_num))
  
  stem_col <- paste0(prefix, "_stem")
  
  df <- df %>%
    mutate(
      !!stem_col := rowSums_keepNA_df(select(., all_of(sum_cols))),
      
      !!paste0(prefix, "_winter_browsed_stems") := if (length(winter_cols) > 0)
        rowSums_keepNA_df(select(., all_of(winter_cols))) else NA_real_,
      
      !!paste0(prefix, "_winter_browsing") := if_else(
        is.na(.data[[stem_col]]) | .data[[stem_col]] == 0,
        NA_real_,
        .data[[paste0(prefix, "_winter_browsed_stems")]] / .data[[stem_col]]
      ),
      
      !!paste0(prefix, "_summer_browsed_stems") := if (length(summer_cols) > 0)
        rowSums_keepNA_df(select(., all_of(summer_cols))) else NA_real_,
      
      !!paste0(prefix, "_summer_browsing") := if_else(
        is.na(.data[[stem_col]]) | .data[[stem_col]] == 0,
        NA_real_,
        .data[[paste0(prefix, "_summer_browsed_stems")]] / .data[[stem_col]]
      ),
      
      !!paste0(prefix, "_rebrowsed_stems") := if (length(rebrows_cols) > 0)
        rowSums_keepNA_df(select(., all_of(rebrows_cols))) else NA_real_,
      
      !!paste0(prefix, "_rebrowsing") := if_else(
        is.na(.data[[stem_col]]) | .data[[stem_col]] == 0,
        NA_real_,
        .data[[paste0(prefix, "_rebrowsed_stems")]] / .data[[stem_col]]
      )
    )
  
  df
}

# ----------------------------
# SPRUCE (no ss) + ub column is spruce_ub
# ----------------------------
spruce_codes <- c("fts","o","od","fb","fs","ps")
ÄBIN2008_2025 <- add_props_dotcombo(
  ÄBIN2008_2025, prefix = "spruce", codes = spruce_codes, ub_col = "spruce_ub"
)

# ----------------------------
# CONTORTA (has ss) + ub column is contorta_ub
# ----------------------------
contorta_codes <- c("fts","o","od","fb","fs","ps","ss")
ÄBIN2008_2025 <- add_props_dotcombo(
  ÄBIN2008_2025, prefix = "contorta", codes = contorta_codes, ub_col = "contorta_ub"
)

# Optional: quick summaries
summary(ÄBIN2008_2025$spruce_stem)
summary(ÄBIN2008_2025$spruce_summer_browsing)
summary(ÄBIN2008_2025$spruce_rebrowsing)

summary(ÄBIN2008_2025$contorta_stem)
summary(ÄBIN2008_2025$contorta_summer_browsing)
summary(ÄBIN2008_2025$contorta_rebrowsing)


names(ÄBIN2008_2025)

###############################################################################
#                       Pine damage columns 2025 and other years
################################################################################
# The pine damage columns in the 2025 years were in different columns from the other years

#-------------------------------------------------
# HARMONISE 2025 pine columns with historical columns
# Data: ÄBIN2008_2025
#-------------------------------------------------

df <- ÄBIN2008_2025

#-------------------------------------------------
# 1) Safety check — ensure no overlap in same rows
#-------------------------------------------------

overlap_stems <- sum(
  !is.na(df$Pine_stems) & !is.na(df$Pine_stem)
)

overlap_winter <- sum(
  !is.na(df$`Pine Winter Damage Stems`) &
    !is.na(df$pine_winter_browsed_stems)
)

overlap_prop <- sum(
  !is.na(df$`Proportion Pine Damage Winter`) &
    !is.na(df$winter_browsing)
)

cat("Overlap rows (should be 0):\n")
cat("Stems:", overlap_stems, "\n")
cat("Winter stems:", overlap_winter, "\n")
cat("Proportion:", overlap_prop, "\n\n")

if (any(c(overlap_stems, overlap_winter, overlap_prop) > 0)) {
  stop("Overlap detected — columns contain values in same rows. Inspect before merging.")
}

#-------------------------------------------------
# 2) Merge into standard columns
#   (Only fills NA values — does not overwrite)
#-------------------------------------------------

df$`Pine stems` <- ifelse(
  is.na(df$`Pine stems`),
  df$Pine_stem,
  df$`Pine stems`
)


df$`Pine Winter Damage Stems` <- ifelse(
  is.na(df$`Pine Winter Damage Stems`),
  df$pine_winter_browsed_stems,
  df$`Pine Winter Damage Stems`
)

df$`Proportion Pine Damage Winter` <- ifelse(
  is.na(df$`Proportion Pine Damage Winter`),
  df$winter_browsing,
  df$`Proportion Pine Damage Winter`
)

#-------------------------------------------------
# 3) Verify 2025 now lives in standard columns
#-------------------------------------------------

subset_2025 <- subset(df, Year == 2025)

cat("Non-NA counts in 2025 after merge:\n")
cat("Pine stems:",
    sum(!is.na(subset_2025$`Pine stems`)), "\n")
cat("Winter stems:",
    sum(!is.na(subset_2025$`Pine Winter Damage Stems`)), "\n")
cat("Proportion:",
    sum(!is.na(subset_2025$`Proportion Pine Damage Winter`)), "\n")

#-------------------------------------------------
# 4) Replace original dataframe (only if satisfied)
#-------------------------------------------------

ÄBIN2008_2025 <- df

#-------------------------------------------------
# INSPECT OVERLAP ROWS + MERGE SAFELY (NO OVERWRITE)
#-------------------------------------------------

library(dplyr)

df <- ÄBIN2008_2025

#----------------------------#
# 1) Extract overlapping 2025 rows
#----------------------------#

overlap_2025 <- df %>%
  filter(Year == 2025) %>%
  mutate(
    overlap_winter = !is.na(`Pine Winter Damage Stems`) & 
      !is.na(pine_winter_browsed_stems),
    overlap_prop   = !is.na(`Proportion Pine Damage Winter`) & 
      !is.na(winter_browsing)
  ) %>%
  filter(overlap_winter | overlap_prop) %>%
  select(
    Year,
    Pine_stem, `Pine stems`,
    pine_winter_browsed_stems, `Pine Winter Damage Stems`,
    winter_browsing, `Proportion Pine Damage Winter`,
    overlap_winter, overlap_prop
  )

print(overlap_2025)

to_num <- function(x) {
  if (is.numeric(x)) return(x)
  x <- trimws(as.character(x))
  x[x == ""] <- NA
  x <- gsub(",", ".", x)
  suppressWarnings(as.numeric(x))
}

# Winter stems identical?
winter_match <- with(overlap_2025, {
  a <- to_num(pine_winter_browsed_stems)
  b <- to_num(`Pine Winter Damage Stems`)
  all((a == b) | (is.na(a) & is.na(b)))
})

# Proportion identical?
prop_match <- with(overlap_2025, {
  a <- to_num(winter_browsing)
  b <- to_num(`Proportion Pine Damage Winter`)
  all((a == b) | (is.na(a) & is.na(b)))
})

cat("Do overlapping winter stem values match? ", winter_match, "\n")
cat("Do overlapping proportion values match?  ", prop_match, "\n")


#-------------------------------------------------
# HARMONISE 2025 pine columns into standard columns (SAFE: fill NAs only)
# Data: ÄBIN2008_2025
# Result: standard columns cover all years including 2025
#-------------------------------------------------

library(dplyr)

ÄBIN2008_2025 <- ÄBIN2008_2025 %>%
  mutate(
    # Stems
    `Pine stems` = coalesce(`Pine stems`, Pine_stem),
    
    # Winter damage stems
    `Pine Winter Damage Stems` = coalesce(`Pine Winter Damage Stems`, pine_winter_browsed_stems),
    
    # Winter browsing proportion
    `Proportion Pine Damage Winter` = coalesce(`Proportion Pine Damage Winter`, winter_browsing)
  )

#-------------------------------------------------
# Quick verification for 2025
#-------------------------------------------------
check_2025 <- ÄBIN2008_2025 %>%
  filter(Year == 2025) %>%
  summarise(
    n_rows = n(),
    nonNA_pine_stems = sum(!is.na(`Pine stems`)),
    nonNA_winter_stems = sum(!is.na(`Pine Winter Damage Stems`)),
    nonNA_prop = sum(!is.na(`Proportion Pine Damage Winter`))
  )

print(check_2025)

#-------------------------------------------------
# OPTIONAL: After you are confident, drop the 2025-only columns
# (commented out by default)
#-------------------------------------------------
ÄBIN2008_2025 <- ÄBIN2008_2025 %>%
  select(-Pine_stem, -pine_winter_browsed_stems, -winter_browsing)


library(writexl)

write_xlsx(
  ÄBIN2008_2025,
  "ÄBIN2008_2025.xlsx"
)

write.csv(
  ÄBIN2008_2025,
  "ÄBIN2008_2025.csv",
  row.names = FALSE
)



#-------------------------------------------------
# MERGE 2025 columns into the existing standard columns (NO NAME CHANGES)
#-------------------------------------------------

df <- ÄBIN2008_2025

# Stems: Pine_stem -> Pine_stems
df$Pine_stems <- ifelse(
  is.na(df$Pine_stems),
  df$Pine_stem,
  df$Pine_stems
)

# Winter damage stems: pine_winter_browsed_stems -> Pine Winter Damage Stems
df$`Pine Winter Damage Stems` <- ifelse(
  is.na(df$`Pine Winter Damage Stems`),
  df$pine_winter_browsed_stems,
  df$`Pine Winter Damage Stems`
)

# Proportion: winter_browsing -> Proportion Pine Damage Winter
df$`Proportion Pine Damage Winter` <- ifelse(
  is.na(df$`Proportion Pine Damage Winter`),
  df$winter_browsing,
  df$`Proportion Pine Damage Winter`
)

ÄBIN2008_2025 <- df

#################################################################################
#                        Create ÄBIN_compact                                   #
################################################################################
# just summary info abd not lots about the damage types

library(dplyr)

ÄBIN_compact <- ÄBIN2008_2025 %>%
  select(
    # ---- Basic identifiers ----
    Area, Year, Stand, Plot, Age, Surveyor, Date,
    Nearest.Tract, Productivity,
    North, East, PCT, Half.Height,
    
    # ---- Pine ----
    pine_unbrowsed,
    Pine_stem,
    pine_winter_browsed_stems,
    winter_browsing,
    pine_summer_browsed_stems,
    summer_browsing,
    pine_rebrowsed_stems,
    rebrowsing,
    
    # ---- Spruce ----
    spruce_ub,
    spruce_stem,
    spruce_winter_browsed_stems,
    spruce_winter_browsing,
    spruce_summer_browsed_stems,
    spruce_summer_browsing,
    spruce_rebrowsed_stems,
    spruce_rebrowsing,
    
    # ---- Contorta ----
    contorta_ub,
    contorta_stem,
    contorta_winter_browsed_stems,
    contorta_winter_browsing,
    contorta_summer_browsed_stems,
    contorta_summer_browsing,
    contorta_rebrowsed_stems,
    contorta_rebrowsing,
    
    # ---- Broadleaves ----
    Downy.Total, Downy.Height, Downy.Damage,
    Silver.Total, Silver.Height, Silver.Damage,
    Rowan.Height, Rowan.Total, Rowan.Damaged,
    Aspen.Height, Aspen.Total, Aspen.Damaged,
    Salix.Height, Salix.Total, Salix.Damaged,
    Oak.Height, Oak.Total, Oak.Damaged,
    
    # ---- Ungulates ----
    Moose.Pellets,
    Red.Deer.Pellets,
    Small.Deer.Pellets,
    Reindeer.Pellets,
    Wild.Boar
  )
write.csv(ÄBIN_compact, "ÄBIN_compact")
