##############################################################################
# ÄBIN2008_2025 — ONE CLEAN FLOW
# Goal:
# 1) Load data
# 2) Standardise names
# 3) Calculate pine browsing variables from raw combo columns
# 4) Create one harmonised set of final pine columns
#    - use historical columns for older years
#    - use recalculated columns for 2025
# 5) Save cleaned file
##############################################################################

library(dplyr)
library(stringr)
library(writexl)

##############################################################################
# 1) LOAD DATA
##############################################################################

ÄBIN2008_2025 <- read.csv("ÄBIN2008_2025.csv", check.names = FALSE)

# Make names consistent with underscores
names(ÄBIN2008_2025) <- gsub("\\.", "_", names(ÄBIN2008_2025))

df <- ÄBIN2008_2025

##############################################################################
# 2) HELPERS
##############################################################################

to_num <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x)) return(x)
  x <- trimws(as.character(x))
  x[x == ""] <- NA
  x <- gsub(",", ".", x)
  suppressWarnings(as.numeric(x))
}

rowSums_keepNA_df <- function(df_subset) {
  m <- as.data.frame(lapply(df_subset, to_num))
  rs <- rowSums(m, na.rm = TRUE)
  rs[rowSums(!is.na(m)) == 0] <- NA_real_
  rs
}

##############################################################################
# 3) PINE: IDENTIFY RAW DAMAGE COMBINATION COLUMNS
##############################################################################

codes <- c("fts", "o", "od", "fb", "fs", "ps", "ss")

pine_combo_regex <- paste0(
  "^pine_(",
  paste(codes, collapse = "|"),
  ")(,(",
  paste(codes, collapse = "|"),
  "))*$"
)

pine_combo_cols <- names(df)[str_detect(names(df), pine_combo_regex)]

pine_sum_cols <- intersect(c(pine_combo_cols, "pine_unbrowsed"), names(df))
if (length(pine_sum_cols) == 0) stop("No pine stem columns found.")

winter_tokens <- c("fts", "fb", "fs")
winter_regex  <- paste0("(^pine_|,)(?:", paste(winter_tokens, collapse = "|"), ")(,|$)")
winter_cols   <- pine_combo_cols[str_detect(pine_combo_cols, winter_regex)]
if (length(winter_cols) == 0) stop("No pine winter browsing columns found.")

summer_tokens <- c("ps")
summer_regex  <- paste0("(^pine_|,)(?:", paste(summer_tokens, collapse = "|"), ")(,|$)")
summer_cols   <- pine_combo_cols[str_detect(pine_combo_cols, summer_regex)]
if (length(summer_cols) == 0) stop("No pine summer browsing columns found.")

rebrows_tokens <- c("fts", "fb", "fs", "ss", "ps", "od")
rebrows_regex  <- paste0("(^pine_|,)(?:", paste(rebrows_tokens, collapse = "|"), ")(,|$)")
rebrows_cols   <- pine_combo_cols[str_detect(pine_combo_cols, rebrows_regex)]
if (length(rebrows_cols) == 0) stop("No pine rebrowsing columns found.")

##############################################################################
# 4) CALCULATE PINE VARIABLES FROM RAW COMBO COLUMNS
##############################################################################

df <- df %>%
  mutate(across(all_of(unique(c(pine_sum_cols, winter_cols, summer_cols, rebrows_cols))), to_num)) %>%
  mutate(
    Pine_stem_calc = rowSums_keepNA_df(select(., all_of(pine_sum_cols))),
    pine_winter_browsed_stems_calc = rowSums_keepNA_df(select(., all_of(winter_cols))),
    winter_browsing_calc = if_else(
      is.na(Pine_stem_calc) | Pine_stem_calc == 0,
      NA_real_,
      pine_winter_browsed_stems_calc / Pine_stem_calc
    ),
    pine_summer_browsed_stems_calc = rowSums_keepNA_df(select(., all_of(summer_cols))),
    summer_browsing_calc = if_else(
      is.na(Pine_stem_calc) | Pine_stem_calc == 0,
      NA_real_,
      pine_summer_browsed_stems_calc / Pine_stem_calc
    ),
    pine_rebrowsed_stems_calc = rowSums_keepNA_df(select(., all_of(rebrows_cols))),
    rebrowsing_calc = if_else(
      is.na(Pine_stem_calc) | Pine_stem_calc == 0,
      NA_real_,
      pine_rebrowsed_stems_calc / Pine_stem_calc
    )
  )

##############################################################################
# 5) OPTIONAL: SPRUCE + CONTORTA
##############################################################################

add_props_dotcombo <- function(df, prefix, codes, ub_col,
                               winter_tokens = c("fts", "fb", "fs"),
                               summer_token = "ps",
                               rebrows_tokens = c("fts", "fb", "fs", "ss", "ps", "od")) {
  
  combo_regex <- paste0(
    "^", prefix, "_(",
    paste(codes, collapse = "|"),
    ")(\\.(",
    paste(codes, collapse = "|"),
    "))*$"
  )
  
  combo_cols <- names(df)[str_detect(names(df), combo_regex)]
  if (length(combo_cols) == 0) stop("No combo columns found for ", prefix)
  
  sum_cols <- intersect(c(combo_cols, ub_col), names(df))
  if (length(sum_cols) == 0) stop("No stem columns found for ", prefix)
  
  token_regex <- function(tokens) {
    paste0("(^", prefix, "_|\\.)(?:", paste(tokens, collapse = "|"), ")(\\.|$)")
  }
  
  winter_tokens_use  <- intersect(winter_tokens, codes)
  summer_tokens_use  <- intersect(c(summer_token), codes)
  rebrows_tokens_use <- intersect(rebrows_tokens, codes)
  
  winter_cols  <- combo_cols[str_detect(combo_cols, token_regex(winter_tokens_use))]
  summer_cols  <- combo_cols[str_detect(combo_cols, token_regex(summer_tokens_use))]
  rebrows_cols <- combo_cols[str_detect(combo_cols, token_regex(rebrows_tokens_use))]
  
  df <- df %>%
    mutate(across(all_of(unique(c(sum_cols, winter_cols, summer_cols, rebrows_cols))), to_num))
  
  stem_col <- paste0(prefix, "_stem")
  
  df <- df %>%
    mutate(
      !!stem_col := rowSums_keepNA_df(select(., all_of(sum_cols))),
      !!paste0(prefix, "_winter_browsed_stems") := if (length(winter_cols) > 0) rowSums_keepNA_df(select(., all_of(winter_cols))) else NA_real_,
      !!paste0(prefix, "_winter_browsing") := if_else(
        is.na(.data[[stem_col]]) | .data[[stem_col]] == 0,
        NA_real_,
        .data[[paste0(prefix, "_winter_browsed_stems")]] / .data[[stem_col]]
      ),
      !!paste0(prefix, "_summer_browsed_stems") := if (length(summer_cols) > 0) rowSums_keepNA_df(select(., all_of(summer_cols))) else NA_real_,
      !!paste0(prefix, "_summer_browsing") := if_else(
        is.na(.data[[stem_col]]) | .data[[stem_col]] == 0,
        NA_real_,
        .data[[paste0(prefix, "_summer_browsed_stems")]] / .data[[stem_col]]
      ),
      !!paste0(prefix, "_rebrowsed_stems") := if (length(rebrows_cols) > 0) rowSums_keepNA_df(select(., all_of(rebrows_cols))) else NA_real_,
      !!paste0(prefix, "_rebrowsing") := if_else(
        is.na(.data[[stem_col]]) | .data[[stem_col]] == 0,
        NA_real_,
        .data[[paste0(prefix, "_rebrowsed_stems")]] / .data[[stem_col]]
      )
    )
  
  df
}

spruce_codes <- c("fts", "o", "od", "fb", "fs", "ps")
df <- add_props_dotcombo(df, prefix = "spruce", codes = spruce_codes, ub_col = "spruce_ub")

contorta_codes <- c("fts", "o", "od", "fb", "fs", "ps", "ss")
df <- add_props_dotcombo(df, prefix = "contorta", codes = contorta_codes, ub_col = "contorta_ub")

##############################################################################
# 6) HARMONISE PINE COLUMNS ACROSS YEARS
#
# IMPORTANT:
# Your historical columns and recalculated columns do NOT match exactly.
# So do NOT blindly merge across all years.
#
# Strategy:
# - for 2025, use the recalculated columns from raw combo columns
# - for older years, use the historical standard columns
##############################################################################

# Make sure historical columns are numeric if they exist
historical_cols <- intersect(
  c("Pine_stems", "Pine_Winter_Damage_Stems", "Proportion_Pine_Damage_Winter"),
  names(df)
)

df <- df %>%
  mutate(across(all_of(historical_cols), to_num))

df <- df %>%
  mutate(
    Pine_stems_final = if_else(
      Year == 2025,
      Pine_stem_calc,
      Pine_stems
    ),
    
    Pine_Winter_Damage_Stems_final = if_else(
      Year == 2025,
      pine_winter_browsed_stems_calc,
      Pine_Winter_Damage_Stems
    ),
    
    Proportion_Pine_Damage_Winter_final = if_else(
      Year == 2025,
      winter_browsing_calc,
      Proportion_Pine_Damage_Winter
    )
  )

##############################################################################
# 7) OPTIONAL: REPLACE OLD STANDARD COLUMNS WITH FINAL ONES
##############################################################################

df <- df %>%
  mutate(
    Pine_stems = Pine_stems_final,
    Pine_Winter_Damage_Stems = Pine_Winter_Damage_Stems_final,
    Proportion_Pine_Damage_Winter = Proportion_Pine_Damage_Winter_final
  ) %>%
  select(
    -Pine_stems_final,
    -Pine_Winter_Damage_Stems_final,
    -Proportion_Pine_Damage_Winter_final
  )

##############################################################################
# 8) QUICK CHECKS
##############################################################################

cat("\n---------------- CHECKS ----------------\n")

cat("Rows in 2025 with Pine_stems:\n")
print(sum(!is.na(df$Pine_stems[df$Year == 2025])))

cat("Rows in 2025 with Pine_Winter_Damage_Stems:\n")
print(sum(!is.na(df$Pine_Winter_Damage_Stems[df$Year == 2025])))

cat("Rows in 2025 with Proportion_Pine_Damage_Winter:\n")
print(sum(!is.na(df$Proportion_Pine_Damage_Winter[df$Year == 2025])))

cat("\nSummary of final pine columns:\n")
print(summary(df$Pine_stems))
print(summary(df$Pine_Winter_Damage_Stems))
print(summary(df$Proportion_Pine_Damage_Winter))

##############################################################################
# 9) OPTIONAL: RELOCATE KEY COLUMNS
##############################################################################

df <- df %>%
  relocate(Area, Year, Stand, Plot, Age)

##############################################################################
# 10) SAVE CLEANED DATA
##############################################################################

ÄBIN2008_2025 <- df

write.csv(ÄBIN2008_2025, "ÄBIN2008_2025_cleaned.csv", row.names = FALSE)
write_xlsx(ÄBIN2008_2025, "ÄBIN2008_2025_cleaned.xlsx")

cat("\nDone. Files saved:\n")
cat(" - ÄBIN2008_2025_cleaned.csv\n")
cat(" - ÄBIN2008_2025_cleaned.xlsx\n")

