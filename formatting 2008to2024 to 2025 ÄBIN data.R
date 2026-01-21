#### Title: Current ÄBIN 2008 to 2024 file, transforming it to be more streamline
#### Author: Sarah Gore
#### Date: 2026-01-21 
#### Discription: My objective is to make this current full Äbin file more streamline
#### and also make it more compatible to the 2025 ÄBIN file fore merge later



X2008_2024_ÄBIN1024 <- read_excel("//storage-ume.slu.se/home$/shge0002/Desktop/ABIN files/2008-2024 ÄBIN1024.xlsx", 
                                  sheet = "Raw")


#1️⃣ Define a helper that converts names → codes

# We’ll write a small function that:
  
# 1) Removes the "Pine " prefix

# 2) Splits on " + "

# 3) Maps each term to its code

# 4) Sorts and joins codes with commas

rename_damage_name <- function(nm) {
  # If it's the Undamaged column (anywhere), just return ub
  if (grepl("Undamaged", nm, ignore.case = TRUE)) {
    return("ub")
  }
  
  # Remove "Pine " prefix if present
  nm_clean <- sub("^Pine\\s+", "", nm)
  
  # Split on " + " (allowing arbitrary spaces around +)
  parts <- strsplit(nm_clean, "\\s*\\+\\s*")[[1]]
  parts <- trimws(parts)
  
  # Map each part to its code
  codes <- vapply(parts, function(p) {
    if (p %in% c("Bark"))               return("fb")
    if (p %in% c("Stem"))               return("fs")
    if (p %in% c("Winter Top Shoot","WTS")) return("fts")
    if (p %in% c("Other"))              return("o")
    if (p %in% c("Old"))                return("od")
    if (p %in% c("Summer Top Shoot","STS")) return("ps")
    if (p %in% c("SS"))                 return("ss")
    if (p %in% c("Undamaged"))          return("ub")
    
    # fallback: keep original text if something unexpected appears
    p
  }, FUN.VALUE = character(1))
  
  # Sort and de-duplicate codes to match your other df style
  codes <- sort(unique(codes))
  
  # Combine like "fb,fs,od,ps,ss"
  paste(codes, collapse = ",")
}


#Now apply function to the df columns
# Work with the original df
cn <- names(X2008_2024_ÄBIN1024)

# 1. Change damage columns 31 to 58
cols_damage <- 31:58
cn[cols_damage] <- vapply(
  cn[cols_damage],
  rename_damage_name,
  FUN.VALUE = character(1)
)

# 2. Change the Undamaged column (col 13) to ub
cn[13] <- "ub"

# 3. Assign the new names back
names(X2008_2024_ÄBIN1024) <- cn
