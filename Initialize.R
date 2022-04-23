# Initialize
libs <- c("igraph", "reshape", "dplyr", "outliers", "stringr")
lapply(libs, require, character.only=T)
options (max.print=10000); options (width=150); options (scipen=999); options(digits=20)

# Functional codes path
CodePath <- "\\OpenRCodes"

# Call relevant functions
source(paste(CodePath, "UtilFunctions.R", sep="\\"))
source(paste(CodePath, "CleanUpFunctions.R", sep="\\"))
source(paste(CodePath, "GraphFunctions.R", sep="\\"))
source(paste(CodePath, "cSplit.R", sep="\\"))
=======
# Initialize
libs <- c("igraph", "reshape", "dplyr", "outliers", "stringr")
lapply(libs, require, character.only=T)
options (max.print=10000); options (width=150); options (scipen=999); options(digits=20)