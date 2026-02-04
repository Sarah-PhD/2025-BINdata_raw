###########
# Author: Sarah Gore
# Date: 2026-02-02
# Title: Trying to take away boolean columns
##########

df <- colÄBIN2008_2025

# Count column classes
table(sapply(df, function(x) paste(class(x), collapse="/")))

# Which columns are actually logical in R?
log_cols <- names(df)[sapply(df, is.logical)]
log_cols

df[log_cols] <- lapply(df[log_cols], as.numeric)

colÄBIN2008_2025 <- df
write_xlsx(colÄBIN2008_2025, "colÄBIN2008_2025.xlsx")


table(df[[log_cols[1]]], useNA="ifany")
class(df[[log_cols[1]]])

all_na_cols <- names(df)[sapply(df, function(x) all(is.na(x)))]
length(all_na_cols)
all_na_cols
sapply(df[all_na_cols], class)
str(df)
col <- "pine_fb,fts,od"

class(df[[col]])
table(df[[col]], useNA = "ifany")
head(df[[col]], 20)


df_MA20 <- df %>%
  filter(Stand == "MA20")
nrow(df_MA20)
unique(df_MA20$Year)
View(df_MA20)
