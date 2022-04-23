# Clear workspace
rm(list=setdiff(ls(all.names=TRUE), lsf.str(all.names=TRUE))); gc()
source("\\Initialize.R")
inPath <- None
location <- paste(inPath, "indata", sep="\\")


#############################################################################################################################################
# Load data
#############################################################################################################################################
some_com <- readRDS (file = paste(location, "some_comFile.Rda", sep="\\"))
some_com$mpuid <- tolower(some_com$mpuid)
# indices of duplicates
ind <- numeric(0)
ind <- which(duplicated(some_com) | duplicated(some_com[nrow(some_com):1, ])[nrow(some_com):1])

if (length(ind) > 0) some_com <- unique(some_com)

# split mpuid into separate columns of svi and device/conncted-id
cookieidn1 <- grepl("^(\\[cs\\]v1\\|)", trim(some_com$mpuid), perl=T)
cookieidn2 <- grepl("(\\[ce\\])$", trim(some_com$mpuid), perl=T)

some_comSep1 <- some_com [(cookieidn1 & cookieidn2)==T, ]
names(some_comSep1)[names(some_comSep1) == "mpuid"] <- "visitorid_svi"
some_comSep2 <- some_com [(cookieidn1 & cookieidn2)==F, ]

indSep1 <- DuplicateIndices(some_comSep1, keepvars=names(some_comSep1[, c(1:3)]))
length(indSep1)
# [1] 236

indSep2 <- DuplicateIndices(some_comSep2, keepvars=names(some_comSep2[, c(1:3)]))
length(indSep2)
# [1] 126

# Presence of Duplicates at the level of merging keys
# in both data frames indicate they cannot be joined in tabular form
#
# some_comRecon <- merge(x=some_comSep1,
#                        y=some_comSep2[, c("some_plex_cookie_id", "Max_Max_roi_date", "Min_roi_date", "mpuid")],
#                        by=c("some_plex_cookie_id", "Max_Max_roi_date", "Min_roi_date"),
#                        all=TRUE
#                        )

# rm(some_com, some_comSep1, some_comSep2); gc()

# load test file having browser info
brFile <- paste(location, "somefile.csv", sep="\\")
brFileRead <- read.csv (brFile, colClasses=c(rep("character",7)),
                                header=T, stringsAsFactors=F, strip.white=T, sep=",", na.strings = c("","NULL"), quote = "\"", comment.char = "" )
brFileRead2 <- readRDS (file = paste(location, "brFileRead2.Rda", sep="\\"))


# Join browserinfo having some_plex cookie id to some_comRecon file
# having server-visitor-ID
br_svi <- merge(some_comRecon, brFileRead2, by=c("some_plex_cookie_id"))

instant_pkgs("sqldf")
br_svi2 <-  (sqldf ("query"))

# Count unique by group
AD1 <- within(br_svi[, c("visitorid_svi", "some_plex_cookie_id")], { count <- ave(visitorid_svi, some_plex_cookie_id, FUN=function(x) length(unique(x)))})

AD1 <- as.data.frame (
                        as.data.table(br_svi)[, count := uniqueN(some_plex_cookie_id), by = list(visitorid_svi, device_type_unique, browser_unique)]
                     )

# Look at the browser and device instances where svi is mapping to
# multiple some_plex_cookie_ids
table(AD1[which(AD1$count > 1), c("visitorid_svi", "device_type_unique", "browser_unique", "count")][, c("device_type_unique", "browser_unique")])

AD1[which(AD1$count > 1 &
            !(is.na(AD1$device_type_unique) | is.na(AD1$browser_unique) )),
    c("visitorid_svi", "device_type_unique", "browser_unique", "count")]
library(ggvis)
brFileRead2 %>%
  group_by(device_type_unique) %>%
  summarize(count=length(device_type_unique)) %>%
  ggvis(~device_type_unique, ~count) %>%
  layer_bars(fill:="#20beff")

AD1 %>%
  group_by(device_type_unique) %>%
  summarize(count=length(device_type_unique)) %>%
  ggvis(~device_type_unique, ~count) %>%
  layer_bars(fill:="#f79744")


