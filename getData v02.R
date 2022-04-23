# Clear workspace
rm(list=setdiff(ls(all.names=TRUE), lsf.str(all.names=TRUE))); gc()
source("\\Initialize.R")
inPath <-"\\Psuedo Digital Key Ring"
location <- paste(inPath, "indata", sep="\\")

# ------------------------------------------------------------------------------------
# Fetch and Save for later use
# ------------------------------------------------------------------------------------
uid = ""    ;  pwd = ""
# Aster Packages
library(RODBC)                #This is available on Cran
library(TeradataAsterR)       #This is available from Teradata, not Cran

# Establish AsterR Connection
# Edit dsn and database where necessary
database <- None
AsterR1  <- ta.connect("mydsn", uid = uid, pwd = pwd, database = database, dType = "odbc")
# Ignore warning message that R.ta.session already exists
AsterR1

system.time (tadf1 <- data.frame (ta.data.frame( "query", sourceType = "query" ), stringsAsFactors = F))
saveRDS (tadf1, file=paste(location, "matchData.Rda", sep="\\"))

some_comFile <- None
some_com <- read.csv ( paste(location, some_comFile, sep="\\"), colClasses=c(rep("character",4)),
								header=T, stringsAsFactors=F, strip.white=T, sep="," )

some_tureFile <- None
omnitdata <- read.csv ( paste(location, some_tureFile, sep="\\"), colClasses=c(rep("character",5)),
                                header=T, stringsAsFactors=F, strip.white=T, sep=",", na.strings = c("","NULL") )
omnitdata$visitorid_bigint <- NULL

# Browser file
brFile <- paste(location, smefile, sep="\\")
brFileRead <- read.csv (brFile, colClasses=c(rep("character",7)),
                                header=T, stringsAsFactors=F, strip.white=T, sep=",", na.strings = c("","NULL") )
# Save for later use
saveRDS (brFileRead, file=paste(location, "Identity_DAGR_ROI_ExtractIDs_extractIdentityColumns.Rda", sep="\\"))

# Also save fewer features version
brFileRead$ip_address <- brFileRead$user_agent_string <- brFileRead$operating_system <- ip_addressCount <- brFileRead$ip_addressCount <- NULL
brFileRead_fewerColumns <- brFileRead
rm (brFileRead) ; gc()
saveRDS (brFileRead_fewerColumns, file=paste(location, "Identity_DAGR_fewerColumns.Rda", sep="\\"))


####################################################################################################
# Test read
####################################################################################################
brFile <- paste(location, smefile, sep="\\")
brFileRead <- read.csv (brFile, colClasses=c(rep("character",7)),
                                header=T, stringsAsFactors=F, strip.white=T, sep=",", na.strings = c("","NULL"), quote = "\"", comment.char = "" )
saveRDS (brFileRead, file=paste(location, "brFileRead.Rda", sep="\\"))

# Splitting list columns values into separate columns having one value each
brFileRead2 <- cSplit(brFileRead, c("device_type", "browser", "ip_address"), sep = ",")

# cSplits converts data frame to data tables, converting them back
brFileRead  <- as.data.frame(brFileRead)
brFileRead2 <- as.data.frame(brFileRead2)
device_type.vars.index <- grep(pattern="^device_type_", names(brFileRead2))
browser.vars.index <- grep(pattern="^browser_", names(brFileRead2))
ip_address.vars.index <- grep(pattern="^ip_address_", names(brFileRead2))


extractElem <- function(frame, frame.index, inf, pattern) {
 for (index in frame.index) {
    frame[, index] <- str_extract(frame[, index],  pattern)
    }
        frame[, paste0(inf, "_concat")] <- as.character(apply(frame[, frame.index], 1, function(x) { paste(x, collapse = "," ) } ))
        frame[, paste0(inf, "_unique")] <- as.character(apply(frame[, frame.index], 1, function(x) { na.omit(unique(x)) } ))
          return(frame)
}

pattern1 <- "[A-Za-z]+"
pattern2 <- "(?:\\d{1,3}+\\.){3}+\\d{1,3}"
 brFileRead2 <- extractElem(brFileRead2, browser.vars.index, "browser", pattern1)
 brFileRead2 <- extractElem(brFileRead2, device_type.vars.index, "device_type", pattern1)
 brFileRead2 <- extractElem(brFileRead2, ip_address.vars.index, "ip_address", pattern2)
head(brFileRead2)
saveRDS (brFileRead2, file=paste(location, "brFileRead2.Rda", sep="\\"))


# ------------------------------------------------------------------------------------
# Cleaning
# ------------------------------------------------------------------------------------
# some_com file
system.time(some_com_ <- URL_clean (some_com, "mpuid"))
str(some_com_[which(grepl("%", some_com_$mpuid)), ])
# 'data.frame':   0 obs. of  4 variables:
#  $ some_plex_cookie_id: chr
#  $ Min_roi_date       : chr
#  $ Max_Max_roi_date   : chr
#  $ mpuid              : chr
oth_clean(some_com_, "mpuid")
some_com__ <- regex_clean(some_com_, "mpuid", toMatch)
some_com__$mpuid [which(!(some_com__$v_all_ap))] <- ""
some_com__copy <- some_com__
some_com__copy$v_all_ap <- NULL
saveRDS (some_com__copy, file=paste(location, "some_comFile.Rda", sep="\\"))
# rm(some_com, some_com_, some_com__, some_com__copy); gc()

# NOTE: OBSERVE THE DIFFERENCE HERE
cat ("Printing dim of original some_com --", dim(unique(some_com)), "\n")
cat ("Printing dim of cleaned some_com --",  dim(unique(some_com__copy)), "\n")


# Teradata file
# Add more steps to add missing where regex_clean finds an issue
names(tadf1)[names(tadf1) == "svi"] <- "visitorid_svi"
system.time(tadf1_ <- URL_clean (tadf1, "visitorid_svi"))
tadf1__  <- regex_clean(tadf1_, "visitorid_svi", toMatch);
tafd1__  <- set_id_to_missing (tadf1__, "visitorid_svi")

tadf1__2 <- regex_clean(tadf1__, "mmcid", toMatch)
tadf1__2 <- set_id_to_missing (tadf1__2, "mmcid")

tadf1__3 <- regex_clean(tadf1__2, "mmuid", toMatch)
tadf1__3 <- set_id_to_missing (tadf1__3, "mmuid")

oth_clean(tadf1__3, "mmcid")
oth_clean(tadf1__3, "mmuid")

# NOTE: OBSERVE THE DIFFERENCE HERE
cat ("Printing dim of original tadf --", dim(unique(tadf1)), "\n")
cat ("Printing dim of cleaned tadf --",  dim(unique(tadf1__3)), "\n")

# Save for later use
saveRDS (tadf1__3, file=paste(location, "TerdataFileData.Rda", sep="\\"))

# some_ture file
system.time(omni_   <- URL_clean (omnitdata, "visitorid_svi"))
omni__  <- regex_clean(omni_, "visitorid_svi", toMatch);
omni__ <- set_id_to_missing(omni__, "visitorid_svi")

omni__2 <- regex_clean(omni__, "mmuid", toMatch);
omni__2 <- set_id_to_missing(omni__2, "mmuid")

omni__3 <- regex_clean(omni__2, "mmcid", toMatch);
omni__3 <- set_id_to_missing(omni__3, "mmcid")

oth_clean(omni__3, "mmuid")
oth_clean(omni__3, "mmcid")

# NOTE: OBSERVE THE DIFFERENCE HERE
cat ("Printing dim of original omnitdata --", dim(unique(omnitdata)), "\n")
cat ("Printing dim of cleaned omnitdata --",  dim(unique(omni__3)), "\n")


# Save for later use
saveRDS (omni__3, file=paste(location, "some_tureFileData.Rda", sep="\\"))

# clean workspace
rm(list=setdiff(ls(all.names=TRUE), lsf.str(all.names=TRUE))); gc()

# Clear workspace
rm(list=setdiff(ls(all.names=TRUE), lsf.str(all.names=TRUE))); gc()
source("\\Initialize.R")
inPath <-"\\Psuedo Digital Key Ring"
location <- paste(inPath, "indata", sep="\\")

# ------------------------------------------------------------------------------------
# Fetch and Save for later use
# ------------------------------------------------------------------------------------
uid = None    ;  pwd = None
# Aster Packages
library(RODBC)                #This is available on Cran
library(TeradataAsterR)       #This is available from Teradata, not Cran

# Establish AsterR Connection
# Edit dsn and database where necessary
database <- None
AsterR1  <- ta.connect("mydsn", uid = uid, pwd = pwd, database = database, dType = "odbc")
# Ignore warning message that R.ta.session already exists
AsterR1

system.time (tadf1 <- data.frame (ta.data.frame( None, sourceType = "query" ), stringsAsFactors = F))
saveRDS (tadf1, file=paste(location, "matchData.Rda", sep="\\"))

some_comFile <- None
some_com <- read.csv ( paste(location, some_comFile, sep="\\"), colClasses=c(rep("character",4)),
								header=T, stringsAsFactors=F, strip.white=T, sep="," )

some_tureFile <- "query_result_ConnectedID_some_ture.csv"
omnitdata <- read.csv ( paste(location, some_tureFile, sep="\\"), colClasses=c(rep("character",5)),
                                header=T, stringsAsFactors=F, strip.white=T, sep=",", na.strings = c("","NULL") )
omnitdata$visitorid_bigint <- NULL

# Browser file
brFile <- paste(location, smefile, sep="\\")
brFileRead <- read.csv (brFile, colClasses=c(rep("character",7)),
                                header=T, stringsAsFactors=F, strip.white=T, sep=",", na.strings = c("","NULL") )
# Save for later use
saveRDS (brFileRead, file=paste(location, someDat, sep="\\"))

# Also save fewer features version
brFileRead$ip_address <- brFileRead$user_agent_string <- brFileRead$operating_system <- ip_addressCount <- brFileRead$ip_addressCount <- NULL
brFileRead_fewerColumns <- brFileRead
rm (brFileRead) ; gc()
saveRDS (brFileRead_fewerColumns, file=paste(location, "Identity_DAGR_fewerColumns.Rda", sep="\\"))


####################################################################################################
# Test read
####################################################################################################
brFile <- paste(location, smfile, sep="\\")
brFileRead <- read.csv (brFile, colClasses=c(rep("character",7)),
                                header=T, stringsAsFactors=F, strip.white=T, sep=",", na.strings = c("","NULL"), quote = "\"", comment.char = "" )
saveRDS (brFileRead, file=paste(location, "brFileRead.Rda", sep="\\"))

# Splitting list columns values into separate columns having one value each
brFileRead2 <- cSplit(brFileRead, c("device_type", "browser", "ip_address"), sep = ",")

# cSplits converts data frame to data tables, converting them back
brFileRead  <- as.data.frame(brFileRead)
brFileRead2 <- as.data.frame(brFileRead2)
device_type.vars.index <- grep(pattern="^device_type_", names(brFileRead2))
browser.vars.index <- grep(pattern="^browser_", names(brFileRead2))
ip_address.vars.index <- grep(pattern="^ip_address_", names(brFileRead2))


extractElem <- function(frame, frame.index, inf, pattern) {
 for (index in frame.index) {
    frame[, index] <- str_extract(frame[, index],  pattern)
    }
        frame[, paste0(inf, "_concat")] <- as.character(apply(frame[, frame.index], 1, function(x) { paste(x, collapse = "," ) } ))
        frame[, paste0(inf, "_unique")] <- as.character(apply(frame[, frame.index], 1, function(x) { na.omit(unique(x)) } ))
          return(frame)
}

pattern1 <- "[A-Za-z]+"
pattern2 <- "(?:\\d{1,3}+\\.){3}+\\d{1,3}"
 brFileRead2 <- extractElem(brFileRead2, browser.vars.index, "browser", pattern1)
 brFileRead2 <- extractElem(brFileRead2, device_type.vars.index, "device_type", pattern1)
 brFileRead2 <- extractElem(brFileRead2, ip_address.vars.index, "ip_address", pattern2)
head(brFileRead2)
saveRDS (brFileRead2, file=paste(location, "brFileRead2.Rda", sep="\\"))


# ------------------------------------------------------------------------------------
# Cleaning
# ------------------------------------------------------------------------------------
# some_com file
system.time(some_com_ <- URL_clean (some_com, "mpuid"))
str(some_com_[which(grepl("%", some_com_$mpuid)), ])

oth_clean(some_com_, "mpuid")
some_com__ <- regex_clean(some_com_, "mpuid", toMatch)
some_com__$mpuid [which(!(some_com__$v_all_ap))] <- ""
some_com__copy <- some_com__
some_com__copy$v_all_ap <- NULL
saveRDS (some_com__copy, file=paste(location, "some_comFile.Rda", sep="\\"))
# rm(some_com, some_com_, some_com__, some_com__copy); gc()

# NOTE: OBSERVE THE DIFFERENCE HERE
cat ("Printing dim of original some_com --", dim(unique(some_com)), "\n")
cat ("Printing dim of cleaned some_com --",  dim(unique(some_com__copy)), "\n")


# Teradata file
# Add more steps to add missing where regex_clean finds an issue
names(tadf1)[names(tadf1) == "svi"] <- "visitorid_svi"
system.time(tadf1_ <- URL_clean (tadf1, "visitorid_svi"))
tadf1__  <- regex_clean(tadf1_, "visitorid_svi", toMatch);
tafd1__  <- set_id_to_missing (tadf1__, "visitorid_svi")

tadf1__2 <- regex_clean(tadf1__, "mmcid", toMatch)
tadf1__2 <- set_id_to_missing (tadf1__2, "mmcid")

tadf1__3 <- regex_clean(tadf1__2, "mmuid", toMatch)
tadf1__3 <- set_id_to_missing (tadf1__3, "mmuid")

oth_clean(tadf1__3, "mmcid")
oth_clean(tadf1__3, "mmuid")

# NOTE: OBSERVE THE DIFFERENCE HERE
cat ("Printing dim of original tadf --", dim(unique(tadf1)), "\n")
cat ("Printing dim of cleaned tadf --",  dim(unique(tadf1__3)), "\n")

# Save for later use
saveRDS (tadf1__3, file=paste(location, "TerdataFileData.Rda", sep="\\"))

# some_ture file
system.time(omni_   <- URL_clean (omnitdata, "visitorid_svi"))
omni__  <- regex_clean(omni_, "visitorid_svi", toMatch);
omni__ <- set_id_to_missing(omni__, "visitorid_svi")

omni__2 <- regex_clean(omni__, "mmuid", toMatch);
omni__2 <- set_id_to_missing(omni__2, "mmuid")

omni__3 <- regex_clean(omni__2, "mmcid", toMatch);
omni__3 <- set_id_to_missing(omni__3, "mmcid")

oth_clean(omni__3, "mmuid")
oth_clean(omni__3, "mmcid")

# NOTE: OBSERVE THE DIFFERENCE HERE
cat ("Printing dim of original omnitdata --", dim(unique(omnitdata)), "\n")
cat ("Printing dim of cleaned omnitdata --",  dim(unique(omni__3)), "\n")


# Save for later use
saveRDS (omni__3, file=paste(location, "some_tureFileData.Rda", sep="\\"))

# clean workspace
rm(list=setdiff(ls(all.names=TRUE), lsf.str(all.names=TRUE))); gc()
