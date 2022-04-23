##################################################################################################
# Util functions
##################################################################################################
# replace missing chars with NAs
blankToNA <- function(x) {
    x[sapply(x, nchar)==0] = NA
    return(x)
}

# trim strings
trim <- function( x ) {
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

# remove if exists
ifrm <- function(arg) {
  if (exists(as.character(substitute(arg)))) {
    rm(list=as.character(substitute(arg)), envir=sys.frame())
    }
  }

detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

# ------------------------------------------
# UTIL CODES - NOT CRITICAL FOR GRAPH MODELS
# ------------------------------------------
USERR <- "C:/MyCodes/RForum"
source(paste(USERR, "View_in_Excel.r", sep="/"))
source(paste(USERR, "List of Objects.r", sep="/"))
=======
##################################################################################################
# Util functions
##################################################################################################
# replace missing chars with NAs
blankToNA <- function(x) {
    x[sapply(x, nchar)==0] = NA
    return(x)
}

# trim strings
trim <- function( x ) {
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

# remove if exists
ifrm <- function(arg) {
  if (exists(as.character(substitute(arg)))) {
    rm(list=as.character(substitute(arg)), envir=sys.frame())
    }
  }

detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

# ------------------------------------------
# UTIL CODES - NOT CRITICAL FOR GRAPH MODELS
# ------------------------------------------
USERR <- "C:/MyCodes/RForum"
source(paste(USERR, "View_in_Excel.r", sep="/"))
source(paste(USERR, "List of Objects.r", sep="/"))
source(paste(USERR, "showMemoryUse.r", sep="/"))