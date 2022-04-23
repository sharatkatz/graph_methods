# Patterns to clean data
# 1) mpuid, mmuid, mmcid
# 2) visitor server id
# 3) some_plex cookie id
# ... Keep adding id patterns here

# mpuid has URL characters..
# clearing.. in passes thru apply using URLdecode
URL_clean <- function (indat, invar) {

    decodeFunc <- function (v) {
    CONTINUE <- TRUE
    while (CONTINUE) {
        cvar <- URLdecode(toupper(v))
        v <- cvar
        if (length(grep("%", cvar))==0) { CONTINUE <- FALSE }
     }
    return (cvar)
    }
        outd <- data.frame(outd = apply( array(indat[[invar]]), 1, FUN=decodeFunc ))
        outd <- cbind(indat, outd)
         outd[, invar] <- NULL
         names(outd)[names(outd) == "outd"] <- invar
         pos <- grep(invar, names(outd))
          outd[, pos] <- as.character(outd[, pos])
   return (outd)
}

# Regex clean-up
regex_clean <- function (indat, invar, toMatch) {
    type1 <- "^([[:alnum:]]{8}\\-[[:alnum:]]{4}\\-[[:alnum:]]{4}\\-[[:alnum:]]{4}\\-[[:alnum:]]{12})$"
    type2 <- "^(\\[cs\\]v1\\|[[:alnum:]]{8,16}\\-[[:alnum:]]{8,16}\\[ce\\])$"
    toMatch <- c(type1, type2)
         matches <- sapply(toMatch, grepl, indat[, invar], ignore.case=TRUE)
         v_all_ap <- apply(matches, 1, any)
         outdat <- cbind(indat, v_all_ap)
          return (outdat)
}

# Some zero clean ups
oth_clean <- function (indat, invar) {
    indat[, invar][indat[, invar] == "00000000-0000-0000-0000-000000000000"] <- ""
}

set_id_to_missing <- function (ind, setvar) {
     ind[, setvar][which(!(ind$v_all_ap))] <- "";
      ind$v_all_ap <- NULL
     return (ind)
}

# Find indices based on DUPLICATE RECORDS
DuplicateIndices <- function(ind, keepvars) {
    ind <- ind[, keepvars]
    return(which(duplicated(ind) | duplicated(ind[nrow(ind):1, ])[nrow(ind):1]))
     rm (ind)

# Patterns to clean data
# 1) mpuid, mmuid, mmcid
# 2) visitor server id
# 3) some_plex cookie id
# ... Keep adding id patterns here

# mpuid has URL characters..
# clearing.. in passes thru apply using URLdecode
URL_clean <- function (indat, invar) {

    decodeFunc <- function (v) {
    CONTINUE <- TRUE
    while (CONTINUE) {
        cvar <- URLdecode(toupper(v))
        v <- cvar
        if (length(grep("%", cvar))==0) { CONTINUE <- FALSE }
     }
    return (cvar)
    }
        outd <- data.frame(outd = apply( array(indat[[invar]]), 1, FUN=decodeFunc ))
        outd <- cbind(indat, outd)
         outd[, invar] <- NULL
         names(outd)[names(outd) == "outd"] <- invar
         pos <- grep(invar, names(outd))
          outd[, pos] <- as.character(outd[, pos])
   return (outd)
}

# Regex clean-up
regex_clean <- function (indat, invar, toMatch) {
    type1 <- "^([[:alnum:]]{8}\\-[[:alnum:]]{4}\\-[[:alnum:]]{4}\\-[[:alnum:]]{4}\\-[[:alnum:]]{12})$"
    type2 <- "^(\\[cs\\]v1\\|[[:alnum:]]{8,16}\\-[[:alnum:]]{8,16}\\[ce\\])$"
    toMatch <- c(type1, type2)
         matches <- sapply(toMatch, grepl, indat[, invar], ignore.case=TRUE)
         v_all_ap <- apply(matches, 1, any)
         outdat <- cbind(indat, v_all_ap)
          return (outdat)
}

# Some zero clean ups
oth_clean <- function (indat, invar) {
    indat[, invar][indat[, invar] == "00000000-0000-0000-0000-000000000000"] <- ""
}

set_id_to_missing <- function (ind, setvar) {
     ind[, setvar][which(!(ind$v_all_ap))] <- "";
      ind$v_all_ap <- NULL
     return (ind)
}

# Find indices based on DUPLICATE RECORDS
DuplicateIndices <- function(ind, keepvars) {
    ind <- ind[, keepvars]
    return(which(duplicated(ind) | duplicated(ind[nrow(ind):1, ])[nrow(ind):1]))
     rm (ind)
}