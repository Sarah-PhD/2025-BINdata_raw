### Barksätter for FW###
library(dplyr)

Barksatter_2025 <- ÄBIN2008_2025 %>%
  filter(
    Area == "Barksätter",
    Year == 2025
  )
nrow(Barksatter_2025)
unique(Barksatter_2025$Area)
unique(Barksatter_2025$Year)


### Now I want to add up stems from all damage types

library(dplyr)
library(stringr)

# 1) Define which short codes are allowed in the combo columns
codes <- c("fts","o","od","fb","fs","ps","ss")

# 2) Regex: matches ONLY columns like:
# pine_od, pine_fts, pine_fb,od,ss, etc.
pine_combo_regex <- paste0(
  "^pine_(",
  paste(codes, collapse = "|"),
  ")(,(",
  paste(codes, collapse = "|"),
  "))*$"
)

# 3) Select the columns to sum:
pine_sum_cols <- names(ÄBIN2008_2025)[str_detect(names(ÄBIN2008_2025), pine_combo_regex)]
pine_sum_cols <- c(pine_sum_cols, "pine_unbrowsed")
pine_sum_cols <- intersect(pine_sum_cols, names(ÄBIN2008_2025))  # safety

# 4) Safe row sum: NA if all NA, otherwise sum (ignoring NA)
rowSums_keepNA <- function(m) {
  m_num <- as.data.frame(lapply(m, function(x) suppressWarnings(as.numeric(x))))
  rs <- rowSums(m_num, na.rm = TRUE)
  all_na <- rowSums(!is.na(m_num)) == 0
  rs[all_na] <- NA_real_
  rs
}

# 5) Create Pine_stem
ÄBIN2008_2025 <- ÄBIN2008_2025 %>%
  mutate(
    Pine_stem = rowSums_keepNA(select(., all_of(pine_sum_cols)))
  )

# Optional: see which columns were included
pine_sum_cols


# Now i will add up the damages that contain top shoot fts, bark damage = fb
# and stem damage = fs

library(dplyr)
library(stringr)

codes <- c("fts","o","od","fb","fs","ps","ss")

# All pine combo columns (exactly damage-code combos)
pine_combo_regex <- paste0(
  "^pine_(",
  paste(codes, collapse = "|"),
  ")(,(",
  paste(codes, collapse = "|"),
  "))*$"
)

pine_combo_cols <- names(ÄBIN2008_2025)[str_detect(names(ÄBIN2008_2025), pine_combo_regex)]

# Winter browsing columns = any combo that contains fts OR fb OR fs as a token
winter_tokens <- c("fts", "fb", "fs")
winter_regex <- paste0("^pine_.*(^|,)(?:", paste(winter_tokens, collapse="|"), ")(,|$)")

winter_cols <- pine_combo_cols[str_detect(pine_combo_cols, winter_regex)]

# Safe row sum: NA if all NA, otherwise sum ignoring NA
rowSums_keepNA <- function(m) {
  m_num <- as.data.frame(lapply(m, function(x) suppressWarnings(as.numeric(x))))
  rs <- rowSums(m_num, na.rm = TRUE)
  all_na <- rowSums(!is.na(m_num)) == 0
  rs[all_na] <- NA_real_
  rs
}

# Compute numerator + proportion
ÄBIN2008_2025 <- ÄBIN2008_2025 %>%
  mutate(
    pine_winter_browsed_stems = rowSums_keepNA(select(., all_of(winter_cols))),
    winter_browsing = ifelse(
      is.na(Pine_stem) | Pine_stem == 0,
      NA_real_,
      pine_winter_browsed_stems / Pine_stem
    )
  )

# Optional: see which columns were counted as "winter"
winter_cols


#I will now delete unnesscary columns
Barksatter_2025 <- ÄBIN2008_2025 %>%
  filter(
    Area == "Barksätter",
    Year == 2025
  )
nrow(Barksatter_2025)
unique(Barksatter_2025$Area)
unique(Barksatter_2025$Year)
names(Barksatter_2025)
cat(names(Barksatter_2025), sep = "\n")



library(dplyr)
library(dplyr)

keep_cols <- c(
  "Area","Year","Surveyor","Date","Stand","Plot","Nearest Tract",
  "Productivity","North","East","PCT","Half Height","pine_unbrowsed",
  
  # ---- Pine damage single ----
  "pine_fb","pine_fs","pine_fts","pine_o","pine_od","pine_ps","pine_ss",
  
  # ---- Pine damage double ----
  "pine_fb,fs","pine_fb,fts","pine_fb,o","pine_fb,od","pine_fb,ps","pine_fb,ss",
  "pine_fs,od","pine_fs,ps","pine_fs,ss",
  "pine_fts,o","pine_fts,od","pine_fts,ps","pine_fts,ss",
  "pine_o,od","pine_o,ps","pine_o,ss",
  "pine_od,ps","pine_od,ss","pine_ps,ss",
  
  # ---- Pine damage triple ----
  "pine_fb,fs,od","pine_fb,fs,ss","pine_fb,fts,od","pine_fb,fts,ss",
  "pine_fb,o,od","pine_fb,od,ps","pine_fb,od,ss","pine_fb,ps,ss",
  "pine_fs,o,ps","pine_fs,od,ps","pine_fs,od,ss","pine_fs,ps,ss",
  "pine_fts,o,ss","pine_fts,od,ps","pine_fts,od,ss","pine_fts,ps,ss",
  "pine_o,od,ps","pine_o,od,ss","pine_o,ps,ss","pine_od,ps,ss",
  
  # ---- Pine damage quadruple+ ----
  "pine_fb,fs,ps,ss","pine_fb,fts,od,ps","pine_fb,fts,od,ss",
  "pine_fb,fts,ps,ss","pine_fb,o,od,ss","pine_fb,od,ps,ss",
  "pine_fs,od,ps,ss","pine_fts,o,ps,ss","pine_fts,od,ps,ss",
  "pine_fb,fs,od,ps,ss","pine_fb,fts,od,ps,ss","pine_fs,fts,od,ps,ss",
  
  # ---- Pine + ub combinations ----
  "pine_o,ub,ss","pine_fts,ub","pine_o,ps,ub","pine_o,ub",
  "pine_od,ps,ub","pine_od,ss,ub","pine_od,ub",
  "pine_ps,ss,ub","pine_ps,ub","pine_ss,ub",
  
  # ---- Spruce ----
  "spruce_ub","spruce_fts","spruce_fts,od","spruce_fs","spruce_fs,od",
  "spruce_fb","spruce_fb,od","spruce_od","spruce_fts,o",
  "spruce_o","spruce_od,o","spruce_o,od",
  
  # ---- Broadleaves ----
  "Downy Total","Downy Height","Downy Damage",
  "Silver Total","Silver Height","Silver Damage",
  "Rowan Height","Rowan Total","Rowan Damaged",
  "Aspen Height","Aspen Total","Aspen Damaged",
  "Salix Height","Salix Total","Salix Damaged",
  "Oak Height","Oak Total","Oak Damaged",
  
  # ---- Other ----
  "Comments",
  "Moose Pellets","Red Deer Pellets","Small Deer Pellets",
  "Reindeer Pellets","Wild Boar",
  
  # ---- Derived ----
  "Pine_stem","pine_winter_browsed_stems","winter_browsing"
)

# Keep only desired columns
keep_cols <- intersect(keep_cols, names(Barksatter_2025))

Barksatter_2025_clean <- Barksatter_2025 %>%
  select(all_of(keep_cols)) %>%
  relocate(
    Pine_stem,
    pine_winter_browsed_stems,
    winter_browsing,
    .after = pine_unbrowsed
  )

#### Barksätter for all years ##

Barksatter <- ÄBIN2008_2025 %>%
  filter(
    Area == "Barksätter"
  )


keep_cols_all <- intersect(keep_cols, names(Barksatter))

Barksatter_clean <- Barksatter %>%
  select(all_of(keep_cols_all)) %>%
  relocate(
    Pine_stem,
    pine_winter_browsed_stems,
    winter_browsing,
    .after = pine_unbrowsed
  )



# Why ismt the pine_stms and wimter browsing cod eworking
length(pine_sum_cols)
head(pine_sum_cols, 30)

length(winter_cols)
head(winter_cols, 30)

length(pine_sum_cols)
head(pine_sum_cols, 20)

length(winter_cols)
head(winter_cols, 20)

library(dplyr)
library(stringr)
library(readr)
library(dplyr)

cols_to_convert <- unique(c(pine_sum_cols, winter_cols))

ÄBIN2008_2025 <- ÄBIN2008_2025 %>%
  mutate(across(all_of(cols_to_convert), \(x) {
    if (is.character(x)) {
      # Handles "0", "1", "1,5" etc.
      suppressWarnings(as.numeric(gsub(",", ".", x)))
    } else if (is.logical(x)) {
      as.numeric(x)
    } else {
      x  # already numeric/integer/etc.
    }
  }))


codes <- c("fts","o","od","fb","fs","ps","ss")

pine_combo_regex <- paste0(
  "^pine_(",
  paste(codes, collapse = "|"),
  ")(,(",
  paste(codes, collapse = "|"),
  "))*$"
)

pine_combo_cols <- names(ÄBIN2008_2025)[str_detect(names(ÄBIN2008_2025), pine_combo_regex)]

# Total stems = all pine combo cols + pine_unbrowsed
pine_sum_cols <- intersect(c(pine_combo_cols, "pine_unbrowsed"), names(ÄBIN2008_2025))

# Winter browsing = any combo containing fts OR fb OR fs
winter_tokens <- c("fts","fb","fs")
winter_regex <- paste0("(^|,)(?:", paste(winter_tokens, collapse="|"), ")(,|$)")
winter_cols <- pine_combo_cols[str_detect(pine_combo_cols, winter_regex)]

# Convert ONLY the columns we use to numeric, safely
ÄBIN2008_2025 <- ÄBIN2008_2025 %>%
  mutate(across(all_of(unique(c(pine_sum_cols, winter_cols))), readr::parse_number))

# Now compute sums and proportion
ÄBIN2008_2025 <- ÄBIN2008_2025 %>%
  mutate(
    Pine_stem = {
      m <- select(., all_of(pine_sum_cols))
      rs <- rowSums(m, na.rm = TRUE)
      rs[rowSums(!is.na(m)) == 0] <- NA_real_
      rs
    },
    pine_winter_browsed_stems = {
      m <- select(., all_of(winter_cols))
      rs <- rowSums(m, na.rm = TRUE)
      rs[rowSums(!is.na(m)) == 0] <- NA_real_
      rs
    },
    winter_browsing = if_else(
      is.na(Pine_stem) | Pine_stem == 0,
      NA_real_,
      pine_winter_browsed_stems / Pine_stem
    )
  )
summary(ÄBIN2008_2025$Pine_stem)
summary(ÄBIN2008_2025$pine_winter_browsed_stems)
summary(ÄBIN2008_2025$winter_browsing)
sapply(ÄBIN2008_2025[cols_to_convert], class)


##############################################################################
############   DO OVER ################################
# ============================================================
# Barksätter 2025 — Pine stems + winter browsing proportion
# COMPLETE, WORKING CODE
# ============================================================

library(dplyr)
library(stringr)

# ----------------------------
# 0) Filter to Barksätter 2025
# ----------------------------
Barksatter_2025 <- ÄBIN2008_2025 %>%
  filter(Area == "Barksätter", Year == 2025)

# ----------------------------
# 1) Identify pine damage-combo columns
#    (ONLY tokens fts,o,od,fb,fs,ps,ss; no derived pine_* cols)
# ----------------------------
codes <- c("fts","o","od","fb","fs","ps","ss")

pine_combo_regex <- paste0(
  "^pine_(",
  paste(codes, collapse = "|"),
  ")(,(",
  paste(codes, collapse = "|"),
  "))*$"
)

pine_combo_cols <- names(Barksatter_2025)[str_detect(names(Barksatter_2025), pine_combo_regex)]

# Total stems should include all pine damage-combo cols + pine_unbrowsed
pine_sum_cols <- intersect(c(pine_combo_cols, "pine_unbrowsed"), names(Barksatter_2025))

if (length(pine_sum_cols) == 0) stop("No pine stem columns found (pine_sum_cols is empty).")

# ----------------------------
# 2) Helper: safe numeric conversion + safe row sums
# ----------------------------
to_num <- function(x) {
  if (is.logical(x)) return(as.numeric(x))
  if (is.numeric(x)) return(x)
  suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))
}

rowSums_keepNA_df <- function(df_subset) {
  m <- as.data.frame(lapply(df_subset, to_num))
  rs <- rowSums(m, na.rm = TRUE)
  rs[rowSums(!is.na(m)) == 0] <- NA_real_
  rs
}

# ----------------------------
# 3) Compute Pine_stem (total)
# ----------------------------
Barksatter_2025 <- Barksatter_2025 %>%
  mutate(
    Pine_stem = rowSums_keepNA_df(select(., all_of(pine_sum_cols)))
  )

# ----------------------------
# 4) Winter browsing columns: any combo containing fts OR fb OR fs
#    FIXED token-boundary regex (includes singles + combos)
# ----------------------------
winter_tokens <- c("fts","fb","fs")
winter_regex  <- paste0("(^pine_|,)(?:", paste(winter_tokens, collapse="|"), ")(,|$)")

winter_cols <- pine_combo_cols[str_detect(pine_combo_cols, winter_regex)]

if (length(winter_cols) == 0) stop("No winter browsing columns found (winter_cols is empty).")

# ----------------------------
# 5) Compute winter stems + proportion
# ----------------------------
Barksatter_2025 <- Barksatter_2025 %>%
  mutate(
    pine_winter_browsed_stems = rowSums_keepNA_df(select(., all_of(winter_cols))),
    winter_browsing = if_else(
      is.na(Pine_stem) | Pine_stem == 0,
      NA_real_,
      pine_winter_browsed_stems / Pine_stem
    )
  )

# ----------------------------
# 6) Quick checks / debugging prints
# ----------------------------
cat("Pine stem columns used (first 30):\n")
print(head(pine_sum_cols, 30))

cat("\nWinter browsing columns used:\n")
print(winter_cols)

cat("\nSummaries:\n")
print(summary(Barksatter_2025$Pine_stem))
print(summary(Barksatter_2025$pine_winter_browsed_stems))
print(summary(Barksatter_2025$winter_browsing))

# Optional: view first rows of key inputs + outputs
Barksatter_2025 %>%
  select(all_of(intersect(c("pine_unbrowsed", winter_cols[1:min(8, length(winter_cols))]), names(Barksatter_2025))),
         Pine_stem, pine_winter_browsed_stems, winter_browsing) %>%
  slice(1:10)

# Barksatter_2025 now contains Pine_stem, pine_winter_browsed_stems, winter_browsing

write_xlsx(
  Barksatter_2025_clean,
  "//storage-ume.slu.se/home$/shge0002/My Documents/ÄBIN Data/Barksatter_2025_clean.xlsx"
)
write.csv(
  Barksatter_2025_clean,
  "Barksatter_2025_clean.csv",
  row.names = FALSE
)

summary(ÄBIN2008_2025$Age)
summary(ÄBIN2008_2025$`fb,fs,od`)
summary(ÄBIN2008_2025$`contorta_od,ps,ss`)
summary(ÄBIN2008_2025$North)
