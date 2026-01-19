
###############################################################################
# Date: 2026-01-19
# Author: Sarah GOre
# Title: Changing Column titles to aline with the ÄBIN file 2008 to 2024
#        Deleting columns that are not common
###############################################################################


#Library
library(dplyr)

# Not actually changing name of the column but the values in the column


ÄBIN2025col <- ÄBIN2025 %>%
  mutate(
    Area = as.character(Area),
    Area = recode(
      Area,
      om = "OsterMalma",
      at = "Åtvidaberg",
      vx = "Växjö",
      bk = "Barksätter",
      fk = "Fredrika",
      fu = "Furudal",
      lf = "Lofsdalen",
      lj = "Ljusdal",
      ly = "Lycksele",
      ma = "Malå",
      no = "Nordmaling",
      ra = "Råneå",
      so = "Sorsele"
    )
  )

# Multiple columns at once

ÄBIN2025col <- ÄBIN2025col %>%
  rename(
    `Plot`= `Sampling plot 1-10`,
    `PCT`  = `Pre-commercially thinned?`,
    `Half Height` =`Half height (m)`,
    `North` = `x.x`,
    `East` = `y.x`
  )


# Deleting columns not included in the 2008 to 2024 file
ÄBIN2025col <- ÄBIN2025col %>%
  select(
    -ObjectID.x,
    -ObjectID.y,
    -plot_id,
    -CreationDate.x,
    -CreationDate.y,
    -Creator.x,
    -Creator.y,
    -EditDate.x,
    -EditDate.y,
    -Editor.x,
    -Editor.y,
    -stand_id
    
  )

sort(names(ÄBIN2025col))

df <- ÄBIN2025col   # just to keep the code shorter

code_cols <- 37:min(199, ncol(df))

# Quick check
code_cols
names(df)[code_cols][1:10]   # look at a few names

orig_names <- names(df)[code_cols]

norm_names <- vapply(
  strsplit(orig_names, ","),        # split each name into tokens
  function(parts) {
    parts <- trimws(parts)          # remove spaces around tokens
    parts <- sort(parts)            # sort alphabetically
    paste(parts, collapse = ",")    # recombine into "fb,o" etc.
  },
  FUN.VALUE = character(1)
)

# Optional: inspect
orig_names[1:10]
norm_names[1:10]


groups <- split(code_cols, norm_names)

# Optional: look at what one group looks like
groups[[1]]
names(groups)[1]     # the normalised name for that group


combined_mat <- sapply(groups, function(idx) {
  # subset the columns in this group
  m <- df[, idx, drop = FALSE]
  
  # coerce all columns to numeric (handles factor/character)
  m_num <- as.data.frame(lapply(m, function(x) {
    as.numeric(as.character(x))
  }))
  
  # row-wise sum
  rowSums(m_num, na.rm = TRUE)
})

# Turn into a data frame
combined_df <- as.data.frame(combined_mat)

# Make sure column names are the normalised combo names
colnames(combined_df) <- names(groups)

# Optional: inspect a bit
head(combined_df)

ÄBIN2025_combined <- cbind(
  df[, -code_cols],   # all the other columns unchanged
  combined_df         # your new collapsed combination columns
)

# Optional: check new names
names(ÄBIN2025_combined)

