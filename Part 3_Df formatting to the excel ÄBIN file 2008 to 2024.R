
###############################################################################
# Date: 2026-01-19
# Author: Sarah GOre
# Title: Changing Column titles to aline with the ÄBIN file 2008 to 2024
#        Deleting columns that are not common
###############################################################################


#Library
library(dplyr)
library(readxl)

###LOad ÄBIN2025 from directory
ÄBIN2025 <- read_excel("ÄBIN2025.xlsx")

#For later ill add the big 2008 to 2024 df
X2008_2024_ÄBIN1024 <- read_excel("X2008_2024_ÄBIN1024.xlsx")

X2008_2024_ÄBIN1024 <- X2008_2024_ÄBIN1024 %>%
  rename(
    `Stand`= `Stand_number`
  )        
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
    `East` = `y.x`, 
    `North` = `x.x`,
    `East` = `y.x`,
    `Nearest Tract` = `Closest tract`,
    `Productivity` = `Site productivity`,
    `Downy Height` = `Downy Birch Highest Height (m)`,
    `Downy Total` = `Downy Birch Stems`,
    `Downy Damage` = `Downy birch damage (whole plot)`,
    `Silver Height` = `Silver Birch Highest Height (m)`,
    `Silver Total` = `Silver Birch Stems`,
    `Silver Damage` = `Silver Birch damage (whole plot)`,
    `Rowan Height` = `Rowan Highest Height (m)`,
    `Rowan Total` = `Rowan Stems`,
    `Rowan Damage` = `Rowan damage (whole plot)`,
    `Oak Height` = `Oak Highest Height (m)`,
    `Oak Total` = `Oak Stems`,
    `Oak Damage` = `Oak Damage (whole plot)`,
    `Salix Height` = `Salix Highest Height (m)`,
    `Salix Total` = `Salix Stems`,
    `Salix Damage` = `Salix Damage (whole plot)`,
    `Aspen Height` = `Aspen Highest Height (m)`,
    `Aspen Total` = `Aspen Stems`,
    `Aspen Damage` = `Aspen Damage (whole plot)`
  )


X2008_2024_ÄBIN1024 <- X2008_2024_ÄBIN1024 %>%
  rename(
    `Surveyor`= `Surveyer`
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
    -stand_id,
    -`NA`,
    -`Tallest tree 1 (m)`,
    -`Tallest tree 2 (m)`,
    -spruce_NA,
    -contorta_NA,
    -`spruce_o,ub`,
    -`spruce_ub,o`
    
    
    
  )



# In the damage types there are a lot of duplicates, such as fb,o and o, fb
# same damage, so i would like to clump the duplicates



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

###################### Spruce and contorta duplicates##########################
# Spruce
ÄBIN2025_combined$`spruce_o,od` <- rowSums(
  ÄBIN2025_combined[, c("spruce_o,od", "spruce_od,o")],
  na.rm = TRUE
)


# Contorta 
df <- ÄBIN2025_combined

cont_cols <- grep("^contorta_", names(df), value = TRUE)

canon_names <- vapply(cont_cols, function(nm) {
  codes <- sub("^contorta_", "", nm)
  parts <- sort(trimws(strsplit(codes, ",")[[1]]))
  paste0("contorta_", paste(parts, collapse = ","))
}, FUN.VALUE = character(1))

groups <- split(cont_cols, canon_names)

combined_df <- as.data.frame(lapply(groups, function(cols) {
  m <- df[, cols, drop = FALSE]
  m_num <- as.data.frame(lapply(m, function(x) as.numeric(as.character(x))))
  rowSums(m_num, na.rm = TRUE)
}))
names(combined_df) <- names(groups)

df <- cbind(df[, setdiff(names(df), cont_cols), drop = FALSE], combined_df)
ÄBIN2025_combined <- df


########## Just to confirm uniqueness of column titles) #############
# Optional: check new names
names(ÄBIN2025_combined)

length(unique(names(ÄBIN2025)))
length(names(ÄBIN2025))


######################## ######################################################
### I would now like to re order the df to match the 2008 to 2024 df ###

# First I'll look at all the column names in the two dfs

x_all <- X2008_2024_ÄBIN1024

x_2025 <- ÄBIN2025_combined

names(x_all)
names(x_2025)

# Sometimes easier sorted:
sort(names(x_all))
sort(names(x_2025))

#Find differences in column sets

# Columns that exist in 2008–2024 but not in 2025
setdiff(names(x_all), names(x_2025))

# Columns that exist in 2025 but not in 2008–2024
setdiff(names(x_2025), names(x_all))

# Columns present in both
intersect(names(x_all), names(x_2025))


## I want a lis style to look at in excel
# Get the two name vectors
col_2008_2024 <- names(x_all)
col_2025      <- names(x_2025)

# Determine max length for equal row padding
max_len <- max(length(col_2008_2024), length(col_2025))

# Pad with "" so Excel aligns cleanly
col_2008_2024 <- c(col_2008_2024, rep("", max_len - length(col_2008_2024)))
col_2025      <- c(col_2025,      rep("", max_len - length(col_2025)))

# Create comparison table
compare_cols <- data.frame(
  cols_2008_2024 = col_2008_2024,
  cols_2025      = col_2025,
  stringsAsFactors = FALSE
)

# View in R
View(compare_cols)

# Copy to clipboard (for Excel)
writeClipboard(
  paste(
    apply(compare_cols, 1, paste, collapse = "\t"),
    collapse = "\n"
  )
)


###### MERGE X2008_2025_ÄBIN1024 with ÄBIN2025_combined ##########
############# First Adding Year to 2025 data ################
ÄBIN2025_combined <- ÄBIN2025_combined %>%
  mutate(Year = 2025) %>%
  select(Year, everything())

X2008_2025_ÄBIN1024 <- bind_rows(
  X2008_2024_ÄBIN1024,
  ÄBIN2025_combined
)

# Confirm 2025 was added
table(X2008_2025_ÄBIN1024$Year)

# See which columns were unique to each df (optional)
setdiff(names(X2008_2024_ÄBIN1024), names(ÄBIN2025_combined))

setdiff(names(ÄBIN2025_combined), names(X2008_2024_ÄBIN1024))



####### REname ######

ÄBIN2008_2025 <- X2008_2025_ÄBIN1024

####### Save on directory ####

      
write_xlsx(ÄBIN2008_2025, "ÄBIN2008_2025.xlsx")
