# Clear workspace
rm(list=setdiff(ls(all.names=TRUE), lsf.str(all.names=TRUE))); gc()
source("\\Initialize.R")


##################################################################################################
# Load and prepare the data
##################################################################################################
CodePath <- "C:\\Users\\sharat_sharma\\Documents\\DellPr\\Marketing Science\\Psuedo Digital Key Ring"
location <- paste(CodePath, "indata", sep="\\")
myGraphData  <- readRDS (file = paste(location, "TerdataFileData.Rda", sep="\\"))


# Patterns to clean data
# 1) mpuid, mmuid, mmcid
# 2) visitor server id
# 3) some_plex cookie id
# ... Keep adding id patterns here
type1 <- "^([[:alnum:]]{8}\\-[[:alnum:]]{4}\\-[[:alnum:]]{4}\\-[[:alnum:]]{4}\\-[[:alnum:]]{12})$"
type2 <- "^(\\[cs\\]v1\\|[[:alnum:]]{16}\\-[[:alnum:]]{16}\\[ce\\])$"

# limit data
# myGraphData <- myGraphData[1:1000000, ]
if (length(DuplicateIndices(myGraphData, names(myGraphData)) > 0)) myGraphData <- unique(myGraphData)
dim(myGraphData)


# Load some_plex file
some_com <- readRDS (file = paste(location, "some_comFile.Rda", sep="\\"))
some_com$mpuid <- tolower(some_com$mpuid)
cat("\n", "Dimensions before deleting unique records-- ",dim(some_com))
# indices of duplicates
if (length(DuplicateIndices(some_com, names(some_com)) > 0)) some_com <- unique(some_com)
cat("\n", "Dimensions after deleting unique records-- ",dim(some_com))


# split mpuid into separate columns of svi and device/connected-id
cookieidn1 <- grepl("^(\\[cs\\]v1\\|)", trim(some_com$mpuid), perl=T)
cookieidn2 <- grepl("(\\[ce\\])$", trim(some_com$mpuid), perl=T)

some_comSep1 <- some_com [(cookieidn1 & cookieidn2)==T, ]
names(some_comSep1)[names(some_comSep1) == "mpuid"] <- "visitorid_svi"
some_comSep2 <- some_com [(cookieidn1 & cookieidn2)==F, ]
rm(some_com); gc()

# load test file having browser info and merge it to Separated files from some_com
brFileRead <- readRDS (file = paste(location, "brFileRead.Rda", sep="\\"))
some_comSepBrPr1 <- merge(some_comSep1, brFileRead, by=("some_plex_cookie_id"))
some_comSepBrPr2 <- merge(some_comSep2, brFileRead, by=("some_plex_cookie_id"))



# ---------------------------------- A Detect 1 -------------------------------------------------------------------------- #
instant_pkgs("data.table")
ADSepBrPr1 <- as.data.frame(
                              as.data.table(some_comSepBrPr1)[, count := uniqueN(some_plex_cookie_id), by = list(visitorid_svi, device_type, browser)]
                            )

if ( nrow(ADSepBrPr1[which(ADSepBrPr1$count>1), ]) > 0 ) {
    message ("\n", "|--- NOTE: For device:browser 1:1 combination cookieID(s) is/are found mapping to more than 1 some_plex cookie id ---|", "\n")
}
write.table(ADSepBrPr1[which(ADSepBrPr1$count>1), ],
    file = paste(CodePath, "outdata", "AReport1.txt", sep="\\"),
    row.names=F,
    sep="|")

ADSepBrPr2 <- as.data.frame(
                              as.data.table(some_comSepBrPr2)[, count := uniqueN(some_plex_cookie_id), by = list(mpuid, device_type, browser)]
                            )

if ( nrow(ADSepBrPr1[which(ADSepBrPr1$count>1), ]) > 0 ) {
    message ("\n", "|--- NOTE: For device:browser 1:1 combination mpuid(s) is/are found mapping to more than 1 some_plex cookie id ---|", "\n")
}
write.table(ADSepBrPr2[which(ADSepBrPr2$count>1), ],
    file = paste(CodePath, "outdata", "AReport2.txt", sep="\\"),
    row.names=F,
    sep="|")



# ---------------------------------- A Detect 2 -------------------------------------------------------------------------- #
Agg.some_comSepBrPr1 <- some_comSepBrPr1 %>%
                                    group_by(visitorid_svi) %>%
                                        summarise(browser.2=toString(browser))
Agg.some_comSepBrPr1 <- as.data.frame(Agg.some_comSepBrPr1)

myGraphData$v_all_ap <- NULL
myGraphData$visitorid_svi <- tolower(myGraphData$visitorid_svi)
Devc.Agg.some_comSepBrPr1 <- merge(myGraphData, Agg.some_comSepBrPr1, by=("visitorid_svi"))
AD.Devc.Agg.some_comSepBrPr1 <- as.data.frame(
                                      as.data.table(Devc.Agg.some_comSepBrPr1[Devc.Agg.some_comSepBrPr1$mmuid!="", ])
                                                [, count := uniqueN(browser.2), by = list(mmuid)]
                                             )
if ( nrow(AD.Devc.Agg.some_comSepBrPr1[which(AD.Devc.Agg.some_comSepBrPr1$count>1), ]) > 0 ) {
    message ("\n",
                "|--- NOTE: The MM UID may not be 1:1 to a browser---|",
                "\n")
}


AD.Conn.Agg.some_comSepBrPr1 <- as.data.frame(
                                      as.data.table(Devc.Agg.some_comSepBrPr1[Devc.Agg.some_comSepBrPr1$mmuid!="", ])
                                                [, count := uniqueN(browser.2), by = list(mmuid)]
                                             )
if ( nrow(AD.Conn.Agg.some_comSepBrPr1[which(AD.Conn.Agg.some_comSepBrPr1$count>1), ]) > 0 ) {
    message ("\n",
                "|--- NOTE: The MM CID may not be 1:1 to a browser---|",
                "\n")
}

AD.Conn.myGraphData <- as.data.frame(
                                      as.data.table(myGraphData[myGraphData$mmcid!="", ])
                                                [, count := uniqueN(mmuid), by = list(mmuid)]
                                             )
if ( nrow(AD.Conn.myGraphData[which(AD.Conn.myGraphData$count>1), ]) > 0 ) {
    message ("\n",
                "|--- NOTE: The MM CID may not be 1:1 to a Devices ---|",
                "\n")
}


# ---------------------------------- A Detect 3 -------------------------------------------------------------------------- #


##################################################################################################
# Form edges and nodes
##################################################################################################
g1 <- getGraphObject (myGraphData, "visitorid_svi", "mmuid")
g2 <- getGraphObject (myGraphData, "visitorid_svi", "mmcid")
g3 <- getGraphObject (myGraphData, "mmcid", "mmuid")

# Getting graph objects from some_comRecon suspended - waiting for data
mg1 <- getGraphObject (some_comSepBrPr1, "some_plex_cookie_id", "visitorid_svi")
mg2 <- getGraphObject (some_comSepBrPr2, "some_plex_cookie_id", "mpuid")

# set_vertex_attr(mg1, "browser", index = V(mg1), as.character((some_comSepBrPr1$browser)))
# set_vertex_attr(mg1, "device_type", index = V(mg1), as.character((some_comSepBrPr1$device_type)))
# set_vertex_attr(mg1, "ip_address", index = V(mg1), as.character((some_comSepBrPr1$ip_address)))
# set_vertex_attr(mg2, "browser", index = V(mg2), as.character((some_comSepBrPr2$browser)))
# set_vertex_attr(mg2, "device_type", index = V(mg2), as.character((some_comSepBrPr2$device_type)))
# set_vertex_attr(mg2, "ip_address", index = V(mg2), as.character((some_comSepBrPr2$ip_address)))


############################################################################################################################
# GRAPH UNION, with outlier detection
############################################################################################################################
# graph.union creates the union of two or more graphs. Ie. only edges which are
# included in at least one graph will be part of the new graph.
# This function can be also used via the %u% operator.
# get number of members first before removing outliers - do not blindly remove
# outlier {outliers}
# Description
# Finds value with largest difference between it and sample mean, which can be an outlier.
# Use: table (clusters( simplify(g4, remove.loops=T) )$csize) to see more outliers and keep removing
# The main purpose of the extremevalues package is to provide functions
# which can detect outliers using the methods described above. Additionally,
# plotfunctions are provided for graphical analysis of the result. The package
# currently supports four model distributions:
# • Lognormal distribution
# • Exponential distribution
# • Pareto distribution
# • Weibull distribution
# • Normal distribution
g4 <- g1 %u% g2 %u% g3
mg3 <- mg1 %u% mg2

GG1 <- g4 %u% mg3
GG1_csize <- clusters( simplify(GG1, remove.loops=T) )$csize
# this is to plot distribution of number of edges nodes have, if it shows sparsity,
# then it means that most of the graph is made of disjoint nodes.
# plotDegDist (GG1)

# # Other tests for outlier detection
library("extremevalues")
library("fitdistrplus")
# plotdist(GG1_csize, histo = TRUE, demp = TRUE)

# A skewness-kurtosis plot such as the one proposed by Cullen and Frey (1999) is provided by the descdist
# function for the empirical distribution . On this
# plot, values for common distributions are displayed in order to help the choice of distributions
# to fit to data. For some distributions (normal, uniform, logistic, exponential), there is only
# one possible value for the skewness and the kurtosis. Thus, the distribution is represented by
# a single point on the plot. For other distributions, areas of possible values are represented,
# consisting in lines (as for the gamma and lognormal distributions), or larger areas (as for the
# beta distribution).
# Nevertheless, the user needs to know that skewness and kurtosis, like all higher moments,
# have a very high variance. This is a problem which cannot be completely solved by the
# use of bootstrap. The skewness-kurtosis plot should then be regarded as indicative only. The
# properties of the random variable should be considered, notably its expected value and its
# range, as a complement to the use of the plotdist and descdist functions.
par(mfrow=c(1,2))
descdist(GG1_csize, boot=100); gc()

# summary statistics
# ------
# min:  1   max:  308
# median:  2
# mean:  1.6144812266773889
# estimated sd:  0.74965124896185897
# estimated skewness:  90.859389581592211
# estimated kurtosis:  34603.195992705332
#            used               (Mb) gc trigger               (Mb)   max used               (Mb)
# Ncells  7204574 384.80000000000001   12002346   641.000000000000   12002346   641.000000000000
# Vcells 93177700 710.90000000000009 1589542626 12127.300000000001 2058861894 15707.900000000001


# Fit of distributions by maximum likelihood estimation
# Once selected, one or more parametric distributions f(·|θ) (with parameter θ ∈ R
# d) may be fitted to the data set, one at a time, using the fitdist function. Under the i.i.d. sample
# assumption, distribution parameters θ are by default estimated by maximizing the likelihood function
Sub.GG1_csize <- GG1_csize[!GG1_csize %in% c(1,2)]
fw <- fitdist(Sub.GG1_csize, "gamma")
summary(fw)
# Fitting of the distribution ' gamma ' by maximum likelihood
# Parameters :
#                  estimate           Std. Error
# shape 13.7515730486202052 0.110477244700937602
# rate   4.0780028267338935 0.033366262321872793
# Loglikelihood:  -39307.744985858575   AIC:  78619.48997171715   BIC:  78636.124805195257
# Correlation matrix:
#                     shape                rate
# shape 1.00000000000000000 0.98188442973545254
# rate  0.98188442973545265 1.00000000000000000

# Re-produce univariate data based on estimated shape and scae/rate
set.seed(1234)
Sample.GG1_csize <- rgamma(length(Sub.GG1_csize), shape=fw$estimate[["shape"]], rate = fw$estimate[["rate"]])
par(mfrow=c(1,2))
descdist(Sub.GG1_csize, boot=100)
descdist(Sample.GG1_csize, boot=100)

# ---------- Chi-Square test for re-produced data -------------------------- #
chisq.test(x=GG1_csize, y=Sample.GG1_csize, correct = TRUE,
           p = rep(1/length(x), length(x)), rescale.p = FALSE,
           simulate.p.value = F)
# Pearson's Chi-squared test
# data:  GG1_csize and Sample.GG1_csize
# X-squared = 34260743.999999799, df = 34260702, p-value = 0.49794371383381664

# Warning message:
# In chisq.test(GG1_csize, Sample.GG1_csize) :
#   Chi-squared approximation may be incorrect

# ------------------ OUTLIERS OUTLIERS OUTLIERS ----------------------------------------------------------- #
# Beanplot
# The density shape The density shape used is a polygon given by a normal density trace
# and its mirrored version. Such a polygon often looks a bit like a violin, and is also used in a
# violin plot. In R a density trace can be computed by using density. For computing such a
# density trace, a bandwidth has to be selected. Per default, the implementation of beanplot
# uses the Sheather-Jones method to select a bandwidth per batch, which seems to be preferred
# and close to optimal (Venables and Ripley 2002, page 129). The bandwidths per batch are
# averaged over all batches in order to have a fair comparison between batches.
# The use of the same bandwidth for all beans has a small side-effect for batches that contain
# few data points. In such a case, the width of such a bean can become quite huge, drawing
# attention to a less-interesting bean in terms of statistical significance. To overcome this
# problem, the width of beans with fewer than 10 data points is scaled linearly (so a bean with
# 3 data points is only 3/10 of its normal width).
instant_pkgs("beanplot")
par(mfrow=c(2,2))
plot(density(Sub.GG1_csize)); rug(Sub.GG1_csize)
beanplot(Sub.GG1_csize, bw = "nrd0", horizontal = TRUE)
plot(density(Sample.GG1_csize)); rug(Sample.GG1_csize)
beanplot(Sample.GG1_csize, bw = "nrd0", horizontal = TRUE)



# ---------------------------------- EXAMINE THE OUTLIER NUMBER HERE AND REMOVE LATER ----------------------- #
# chi-squared test for outlier
chisq.outlier.scores <- outliers::chisq.out.test( Sub.GG1_csize, variance=var(GG1_csize), opposite = FALSE)
# data:  GG1_csize
# X-squared = 261652.78610334379, p-value < 0.000000000000000222044604925031
# alternative hypothesis: highest value 308 is an outlier
# X <- Sub.GG1_csize
# while(chisq.out.test(X, variance=var(GG1_csize), opposite = F)$p.value == 0)
# {
#   # str: string contains the outlier value and some text
#   #   n: extract the outlier value and transform to numeric
#   str <- chisq.out.test(X)$alternative
#   cat ("\n", str)
#   n <- as.numeric(unlist(regmatches(str,
#                                     gregexpr("(?<=value)(.*)(?=is an outlier)", str, perl = T))))
#   X <- X[X != n]
#   cat ("\n",length(X))
# }

libary(robustbase)
# mc {robustbase}
# Description
# Compute the ‘medcouple’, a robust concept and estimator of skewness.
# The medcouple is defined as a scaled median difference of the left and right half
# of distribution, and hence not based on the third moment as the classical skewness.
# Qˆ0.75 + 1.5*exp(4 -MCn)* IQRn
IQR.GG1_csize <- IQR(GG1_csize, na.rm = FALSE, type = 7)
MC.GG1_csize <- mc(GG1_csize, na.rm = FALSE, eps1 = .Machine$double.xmin, maxit = 100, trace.lev = 3, full.result = TRUE)
Qhat <- boxplot(GG1_csize, plot=F)$stats[5]
RightFence.GG1_csize <- Qhat + 1.5 * exp( 4 * -MC.GG1_csize) * IQR.GG1_csize
message ("\n", "Value of RightFence.GG1_csize is: ", RightFence.GG1_csize, "\n")
# Value of RightFence.GG1_csize is: 84.8972250497164

# out >=  Q3 + (1.5 * factor * IQR)
b_GG1_csize <- boxplot(GG1_csize, plot=F)$stats[5] + 1.5 * 50 * (1/fw$estimate[["rate"]])
b_GG1_csize
# [1] 21.351012720302521

# ----------------------------------------- REMOVAL OF OUTLIERS ----------------------------------------------- #
# --------------------------- USER HAS TO INPUT IF THE OUTLIE NEEDS TO BE REMOVED ----------------------------- #
ifrm(remove); ifrm(outg)
remove="N"
out_=RightFence.GG1_csize
outg <- graphRemoveOutliers.2 (GG1, visual="N", remove, out_= out_)
table(clusters( simplify(outg, remove.loops=T))$csize)


# Export zipped file
cl <- clusters( simplify(outg, remove.loops=T) )$membership
writeFileName <- gzfile("C:\\Users\\sharat_sharma\\Documents\\DellPr\\Marketing Science\\Psuedo Digital Key Ring\\outdata\\GraphUnion.gz","w")
write.csv (cl,writeFileName,fileEncoding="Windows-1252")
close(writeFileName)


# ------------------------------------------------------------ COMMUNIY DETECTION ------------------------------------------------------------ #
# ---------------------------------------------------------------- FYI & NA ------------------------------------------------------------------ #

#    Observations specifically on community detection –
# •    Notes of graph object (svi-mmuid): this is distribution of number of edges nodes have,
#       which shows the sparsity directing that most of the graph is made of disjoint nodes.
        # Metric of group adhesion can be measured by function edge_connectivity


# •   Edge Connectivity (measure of group adhesion) for this graph is 0.
# •   Transitivity (measures the probability that the adjacent vertices of a vertex are connected) for this graph is 0.
# •   Graph density (ratio of the number of edges and the number of possible edges) for this graph is 0.0000005806.
# •   Clusters in the graph (existing groups of nodes having edges, not estimated) for this graph are 804357 which is same as number of members in community size.
# •   Modularity of community detected for this graph is 0.9999973452. In good prediction scenarios I have seen read
        # this number tends to be not more than 0.6. Range itself of modularity is [1/2, 1), which again indicates (as in previous point)
        # community detection is going all the way down to pre-existing group levels.

#    Further readings and conclusion –
#   o   http://www.unc.edu/~jameswd/Lectures/Community_Detection.pdf - states that a Network is  a construct of
#       1.  Objects x1, . . . , xn in feature space X and
#       2.  Similarity matrix S = {s(xi , xj) : 1 ≤ i, j ≤ n}
#   o   Community detection is done by identifying vertex sets C1, . . . , Ck ⊆ V such that
#       1.  Edge density within sets Ci is large
#       2.  Edge density between sets Ci is small
#       3.  Sets Ci are called communities
#   Therefore, it can be inferred that primary requirement of community detection  is presence of connected nodes.
#   This does not find/discover edges between nodes, which essentially is our objective in graph terminology.

# Community detection
# memberships <- list()
# system.time(fc <- fastgreedy.community(simplify((g1_reduced))))
# g1_memb_s1 <- cutat(fc, wt$merges, steps=which.max(wt$modularity)-1)
# memberships$`Fast greedy` <- g1_memb$membership


## STOP ADDITIONAL CHECKS
# plot (g.ap.grap , vertex.size=1, vertex.label.cex=0.5, edge.width=5, edge.color='orange', vertex.label=NA, layout = layout.graphopt)
# plot (g.ap.grap , vertex.size=1, vertex.label.cex=0.5, edge.width=5, edge.color='orange', vertex.label=NA, layout = layout.kamada.kawai)
# tkplot(g.ap.grap , vertex.size=1, vertex.label.cex=0.5, edge.width=5, edge.color='orange', vertex.label=NA, layout = layout.graphopt)
# ceb <- cluster_edge_betweenness(g4s)
# plot(ceb, g4s, vertex.label=NA)

# Start-Stop time code
# Time it...
# Start the clock!
# ptm1 <- proc.time()
# Stop the clock
# cat ("\n")
# proc.time() - ptm1
=======
# Clear workspace
rm(list=setdiff(ls(all.names=TRUE), lsf.str(all.names=TRUE))); gc()
source("C:\\Users\\sharat_sharma\\Documents\\DellPr\\Marketing Science\\Psuedo Digital Key Ring\\OpenRCodes\\Initialize.R")


##################################################################################################
# Load and prepare the data
##################################################################################################
CodePath <- "C:\\Users\\sharat_sharma\\Documents\\DellPr\\Marketing Science\\Psuedo Digital Key Ring"
location <- paste(CodePath, "indata", sep="\\")
myGraphData  <- readRDS (file = paste(location, "TerdataFileData.Rda", sep="\\"))


# Patterns to clean data
# 1) mpuid, mmuid, mmcid
# 2) visitor server id
# 3) some_plex cookie id
# ... Keep adding id patterns here
type1 <- "^([[:alnum:]]{8}\\-[[:alnum:]]{4}\\-[[:alnum:]]{4}\\-[[:alnum:]]{4}\\-[[:alnum:]]{12})$"
type2 <- "^(\\[cs\\]v1\\|[[:alnum:]]{16}\\-[[:alnum:]]{16}\\[ce\\])$"

# limit data
# myGraphData <- myGraphData[1:1000000, ]
if (length(DuplicateIndices(myGraphData, names(myGraphData)) > 0)) myGraphData <- unique(myGraphData)
dim(myGraphData)


# Load some_plex file
some_com <- readRDS (file = paste(location, "some_comFile.Rda", sep="\\"))
some_com$mpuid <- tolower(some_com$mpuid)
cat("\n", "Dimensions before deleting unique records-- ",dim(some_com))
# indices of duplicates
if (length(DuplicateIndices(some_com, names(some_com)) > 0)) some_com <- unique(some_com)
cat("\n", "Dimensions after deleting unique records-- ",dim(some_com))


# split mpuid into separate columns of svi and device/connected-id
cookieidn1 <- grepl("^(\\[cs\\]v1\\|)", trim(some_com$mpuid), perl=T)
cookieidn2 <- grepl("(\\[ce\\])$", trim(some_com$mpuid), perl=T)

some_comSep1 <- some_com [(cookieidn1 & cookieidn2)==T, ]
names(some_comSep1)[names(some_comSep1) == "mpuid"] <- "visitorid_svi"
some_comSep2 <- some_com [(cookieidn1 & cookieidn2)==F, ]
rm(some_com); gc()

# load test file having browser info and merge it to Separated files from some_com
brFileRead <- readRDS (file = paste(location, "brFileRead.Rda", sep="\\"))
some_comSepBrPr1 <- merge(some_comSep1, brFileRead, by=("some_plex_cookie_id"))
some_comSepBrPr2 <- merge(some_comSep2, brFileRead, by=("some_plex_cookie_id"))



# ---------------------------------- A Detect 1 -------------------------------------------------------------------------- #
instant_pkgs("data.table")
ADSepBrPr1 <- as.data.frame(
                              as.data.table(some_comSepBrPr1)[, count := uniqueN(some_plex_cookie_id), by = list(visitorid_svi, device_type, browser)]
                            )

if ( nrow(ADSepBrPr1[which(ADSepBrPr1$count>1), ]) > 0 ) {
    message ("\n", "|--- NOTE: For device:browser 1:1 combination cookieID(s) is/are found mapping to more than 1 some_plex cookie id ---|", "\n")
}
write.table(ADSepBrPr1[which(ADSepBrPr1$count>1), ],
    file = paste(CodePath, "outdata", "AReport1.txt", sep="\\"),
    row.names=F,
    sep="|")

ADSepBrPr2 <- as.data.frame(
                              as.data.table(some_comSepBrPr2)[, count := uniqueN(some_plex_cookie_id), by = list(mpuid, device_type, browser)]
                            )

if ( nrow(ADSepBrPr1[which(ADSepBrPr1$count>1), ]) > 0 ) {
    message ("\n", "|--- NOTE: For device:browser 1:1 combination mpuid(s) is/are found mapping to more than 1 some_plex cookie id ---|", "\n")
}
write.table(ADSepBrPr2[which(ADSepBrPr2$count>1), ],
    file = paste(CodePath, "outdata", "AReport2.txt", sep="\\"),
    row.names=F,
    sep="|")



# ---------------------------------- A Detect 2 -------------------------------------------------------------------------- #
Agg.some_comSepBrPr1 <- some_comSepBrPr1 %>%
                                    group_by(visitorid_svi) %>%
                                        summarise(browser.2=toString(browser))
Agg.some_comSepBrPr1 <- as.data.frame(Agg.some_comSepBrPr1)

myGraphData$v_all_ap <- NULL
myGraphData$visitorid_svi <- tolower(myGraphData$visitorid_svi)
Devc.Agg.some_comSepBrPr1 <- merge(myGraphData, Agg.some_comSepBrPr1, by=("visitorid_svi"))
AD.Devc.Agg.some_comSepBrPr1 <- as.data.frame(
                                      as.data.table(Devc.Agg.some_comSepBrPr1[Devc.Agg.some_comSepBrPr1$mmuid!="", ])
                                                [, count := uniqueN(browser.2), by = list(mmuid)]
                                             )
if ( nrow(AD.Devc.Agg.some_comSepBrPr1[which(AD.Devc.Agg.some_comSepBrPr1$count>1), ]) > 0 ) {
    message ("\n",
                "|--- NOTE: The MM UID may not be 1:1 to a browser---|",
                "\n")
}


AD.Conn.Agg.some_comSepBrPr1 <- as.data.frame(
                                      as.data.table(Devc.Agg.some_comSepBrPr1[Devc.Agg.some_comSepBrPr1$mmuid!="", ])
                                                [, count := uniqueN(browser.2), by = list(mmuid)]
                                             )
if ( nrow(AD.Conn.Agg.some_comSepBrPr1[which(AD.Conn.Agg.some_comSepBrPr1$count>1), ]) > 0 ) {
    message ("\n",
                "|--- NOTE: The MM CID may not be 1:1 to a browser---|",
                "\n")
}

AD.Conn.myGraphData <- as.data.frame(
                                      as.data.table(myGraphData[myGraphData$mmcid!="", ])
                                                [, count := uniqueN(mmuid), by = list(mmuid)]
                                             )
if ( nrow(AD.Conn.myGraphData[which(AD.Conn.myGraphData$count>1), ]) > 0 ) {
    message ("\n",
                "|--- NOTE: The MM CID may not be 1:1 to a Devices ---|",
                "\n")
}


# ---------------------------------- A Detect 3 -------------------------------------------------------------------------- #


##################################################################################################
# Form edges and nodes
##################################################################################################
g1 <- getGraphObject (myGraphData, "visitorid_svi", "mmuid")
g2 <- getGraphObject (myGraphData, "visitorid_svi", "mmcid")
g3 <- getGraphObject (myGraphData, "mmcid", "mmuid")

# Getting graph objects from some_comRecon suspended - waiting for data
mg1 <- getGraphObject (some_comSepBrPr1, "some_plex_cookie_id", "visitorid_svi")
mg2 <- getGraphObject (some_comSepBrPr2, "some_plex_cookie_id", "mpuid")

# set_vertex_attr(mg1, "browser", index = V(mg1), as.character((some_comSepBrPr1$browser)))
# set_vertex_attr(mg1, "device_type", index = V(mg1), as.character((some_comSepBrPr1$device_type)))
# set_vertex_attr(mg1, "ip_address", index = V(mg1), as.character((some_comSepBrPr1$ip_address)))
# set_vertex_attr(mg2, "browser", index = V(mg2), as.character((some_comSepBrPr2$browser)))
# set_vertex_attr(mg2, "device_type", index = V(mg2), as.character((some_comSepBrPr2$device_type)))
# set_vertex_attr(mg2, "ip_address", index = V(mg2), as.character((some_comSepBrPr2$ip_address)))


############################################################################################################################
# GRAPH UNION, with outlier detection
############################################################################################################################
# graph.union creates the union of two or more graphs. Ie. only edges which are
# included in at least one graph will be part of the new graph.
# This function can be also used via the %u% operator.
# get number of members first before removing outliers - do not blindly remove
# outlier {outliers}
# Description
# Finds value with largest difference between it and sample mean, which can be an outlier.
# Use: table (clusters( simplify(g4, remove.loops=T) )$csize) to see more outliers and keep removing
# The main purpose of the extremevalues package is to provide functions
# which can detect outliers using the methods described above. Additionally,
# plotfunctions are provided for graphical analysis of the result. The package
# currently supports four model distributions:
# • Lognormal distribution
# • Exponential distribution
# • Pareto distribution
# • Weibull distribution
# • Normal distribution
g4 <- g1 %u% g2 %u% g3
mg3 <- mg1 %u% mg2

GG1 <- g4 %u% mg3
GG1_csize <- clusters( simplify(GG1, remove.loops=T) )$csize
# this is to plot distribution of number of edges nodes have, if it shows sparsity,
# then it means that most of the graph is made of disjoint nodes.
# plotDegDist (GG1)

# # Other tests for outlier detection
library("extremevalues")
library("fitdistrplus")
# plotdist(GG1_csize, histo = TRUE, demp = TRUE)

# A skewness-kurtosis plot such as the one proposed by Cullen and Frey (1999) is provided by the descdist
# function for the empirical distribution . On this
# plot, values for common distributions are displayed in order to help the choice of distributions
# to fit to data. For some distributions (normal, uniform, logistic, exponential), there is only
# one possible value for the skewness and the kurtosis. Thus, the distribution is represented by
# a single point on the plot. For other distributions, areas of possible values are represented,
# consisting in lines (as for the gamma and lognormal distributions), or larger areas (as for the
# beta distribution).
# Nevertheless, the user needs to know that skewness and kurtosis, like all higher moments,
# have a very high variance. This is a problem which cannot be completely solved by the
# use of bootstrap. The skewness-kurtosis plot should then be regarded as indicative only. The
# properties of the random variable should be considered, notably its expected value and its
# range, as a complement to the use of the plotdist and descdist functions.
par(mfrow=c(1,2))
descdist(GG1_csize, boot=100); gc()

# summary statistics
# ------
# min:  1   max:  308
# median:  2
# mean:  1.6144812266773889
# estimated sd:  0.74965124896185897
# estimated skewness:  90.859389581592211
# estimated kurtosis:  34603.195992705332
#            used               (Mb) gc trigger               (Mb)   max used               (Mb)
# Ncells  7204574 384.80000000000001   12002346   641.000000000000   12002346   641.000000000000
# Vcells 93177700 710.90000000000009 1589542626 12127.300000000001 2058861894 15707.900000000001


# Fit of distributions by maximum likelihood estimation
# Once selected, one or more parametric distributions f(·|θ) (with parameter θ ∈ R
# d) may be fitted to the data set, one at a time, using the fitdist function. Under the i.i.d. sample
# assumption, distribution parameters θ are by default estimated by maximizing the likelihood function
Sub.GG1_csize <- GG1_csize[!GG1_csize %in% c(1,2)]
fw <- fitdist(Sub.GG1_csize, "gamma")
summary(fw)
# Fitting of the distribution ' gamma ' by maximum likelihood
# Parameters :
#                  estimate           Std. Error
# shape 13.7515730486202052 0.110477244700937602
# rate   4.0780028267338935 0.033366262321872793
# Loglikelihood:  -39307.744985858575   AIC:  78619.48997171715   BIC:  78636.124805195257
# Correlation matrix:
#                     shape                rate
# shape 1.00000000000000000 0.98188442973545254
# rate  0.98188442973545265 1.00000000000000000

# Re-produce univariate data based on estimated shape and scae/rate
set.seed(1234)
Sample.GG1_csize <- rgamma(length(Sub.GG1_csize), shape=fw$estimate[["shape"]], rate = fw$estimate[["rate"]])
par(mfrow=c(1,2))
descdist(Sub.GG1_csize, boot=100)
descdist(Sample.GG1_csize, boot=100)

# ---------- Chi-Square test for re-produced data -------------------------- #
chisq.test(x=GG1_csize, y=Sample.GG1_csize, correct = TRUE,
           p = rep(1/length(x), length(x)), rescale.p = FALSE,
           simulate.p.value = F)
# Pearson's Chi-squared test
# data:  GG1_csize and Sample.GG1_csize
# X-squared = 34260743.999999799, df = 34260702, p-value = 0.49794371383381664

# Warning message:
# In chisq.test(GG1_csize, Sample.GG1_csize) :
#   Chi-squared approximation may be incorrect

# ------------------ OUTLIERS OUTLIERS OUTLIERS ----------------------------------------------------------- #
# Beanplot
# The density shape The density shape used is a polygon given by a normal density trace
# and its mirrored version. Such a polygon often looks a bit like a violin, and is also used in a
# violin plot. In R a density trace can be computed by using density. For computing such a
# density trace, a bandwidth has to be selected. Per default, the implementation of beanplot
# uses the Sheather-Jones method to select a bandwidth per batch, which seems to be preferred
# and close to optimal (Venables and Ripley 2002, page 129). The bandwidths per batch are
# averaged over all batches in order to have a fair comparison between batches.
# The use of the same bandwidth for all beans has a small side-effect for batches that contain
# few data points. In such a case, the width of such a bean can become quite huge, drawing
# attention to a less-interesting bean in terms of statistical significance. To overcome this
# problem, the width of beans with fewer than 10 data points is scaled linearly (so a bean with
# 3 data points is only 3/10 of its normal width).
instant_pkgs("beanplot")
par(mfrow=c(2,2))
plot(density(Sub.GG1_csize)); rug(Sub.GG1_csize)
beanplot(Sub.GG1_csize, bw = "nrd0", horizontal = TRUE)
plot(density(Sample.GG1_csize)); rug(Sample.GG1_csize)
beanplot(Sample.GG1_csize, bw = "nrd0", horizontal = TRUE)



# ---------------------------------- EXAMINE THE OUTLIER NUMBER HERE AND REMOVE LATER ----------------------- #
# chi-squared test for outlier
chisq.outlier.scores <- outliers::chisq.out.test( Sub.GG1_csize, variance=var(GG1_csize), opposite = FALSE)
# data:  GG1_csize
# X-squared = 261652.78610334379, p-value < 0.000000000000000222044604925031
# alternative hypothesis: highest value 308 is an outlier
# X <- Sub.GG1_csize
# while(chisq.out.test(X, variance=var(GG1_csize), opposite = F)$p.value == 0)
# {
#   # str: string contains the outlier value and some text
#   #   n: extract the outlier value and transform to numeric
#   str <- chisq.out.test(X)$alternative
#   cat ("\n", str)
#   n <- as.numeric(unlist(regmatches(str,
#                                     gregexpr("(?<=value)(.*)(?=is an outlier)", str, perl = T))))
#   X <- X[X != n]
#   cat ("\n",length(X))
# }

libary(robustbase)
# mc {robustbase}
# Description
# Compute the ‘medcouple’, a robust concept and estimator of skewness.
# The medcouple is defined as a scaled median difference of the left and right half
# of distribution, and hence not based on the third moment as the classical skewness.
# Qˆ0.75 + 1.5*exp(4 -MCn)* IQRn
IQR.GG1_csize <- IQR(GG1_csize, na.rm = FALSE, type = 7)
MC.GG1_csize <- mc(GG1_csize, na.rm = FALSE, eps1 = .Machine$double.xmin, maxit = 100, trace.lev = 3, full.result = TRUE)
Qhat <- boxplot(GG1_csize, plot=F)$stats[5]
RightFence.GG1_csize <- Qhat + 1.5 * exp( 4 * -MC.GG1_csize) * IQR.GG1_csize
message ("\n", "Value of RightFence.GG1_csize is: ", RightFence.GG1_csize, "\n")
# Value of RightFence.GG1_csize is: 84.8972250497164

# out >=  Q3 + (1.5 * factor * IQR)
b_GG1_csize <- boxplot(GG1_csize, plot=F)$stats[5] + 1.5 * 50 * (1/fw$estimate[["rate"]])
b_GG1_csize
# [1] 21.351012720302521

# ----------------------------------------- REMOVAL OF OUTLIERS ----------------------------------------------- #
# --------------------------- USER HAS TO INPUT IF THE OUTLIE NEEDS TO BE REMOVED ----------------------------- #
ifrm(remove); ifrm(outg)
remove="N"
out_=RightFence.GG1_csize
outg <- graphRemoveOutliers.2 (GG1, visual="N", remove, out_= out_)
table(clusters( simplify(outg, remove.loops=T))$csize)


# Export zipped file
cl <- clusters( simplify(outg, remove.loops=T) )$membership
writeFileName <- gzfile("GraphUnion.gz","w")
write.csv (cl,writeFileName,fileEncoding="Windows-1252")
close(writeFileName)


# ------------------------------------------------------------ COMMUNIY DETECTION ------------------------------------------------------------ #
# ---------------------------------------------------------------- FYI & NA ------------------------------------------------------------------ #

#    Observations specifically on community detection –
# •    Notes of graph object (svi-mmuid): this is distribution of number of edges nodes have,
#       which shows the sparsity directing that most of the graph is made of disjoint nodes.
        # Metric of group adhesion can be measured by function edge_connectivity


# •   Edge Connectivity (measure of group adhesion) for this graph is 0.
# •   Transitivity (measures the probability that the adjacent vertices of a vertex are connected) for this graph is 0.
# •   Graph density (ratio of the number of edges and the number of possible edges) for this graph is 0.0000005806.
# •   Clusters in the graph (existing groups of nodes having edges, not estimated) for this graph are 804357 which is same as number of members in community size.
# •   Modularity of community detected for this graph is 0.9999973452. In good prediction scenarios I have seen read
        # this number tends to be not more than 0.6. Range itself of modularity is [1/2, 1), which again indicates (as in previous point)
        # community detection is going all the way down to pre-existing group levels.

#    Further readings and conclusion –
#   o   http://www.unc.edu/~jameswd/Lectures/Community_Detection.pdf - states that a Network is  a construct of
#       1.  Objects x1, . . . , xn in feature space X and
#       2.  Similarity matrix S = {s(xi , xj) : 1 ≤ i, j ≤ n}
#   o   Community detection is done by identifying vertex sets C1, . . . , Ck ⊆ V such that
#       1.  Edge density within sets Ci is large
#       2.  Edge density between sets Ci is small
#       3.  Sets Ci are called communities
#   Therefore, it can be inferred that primary requirement of community detection  is presence of connected nodes.
#   This does not find/discover edges between nodes, which essentially is our objective in graph terminology.

# Community detection
# memberships <- list()
# system.time(fc <- fastgreedy.community(simplify((g1_reduced))))
# g1_memb_s1 <- cutat(fc, wt$merges, steps=which.max(wt$modularity)-1)
# memberships$`Fast greedy` <- g1_memb$membership


## STOP ADDITIONAL CHECKS
# plot (g.ap.grap , vertex.size=1, vertex.label.cex=0.5, edge.width=5, edge.color='orange', vertex.label=NA, layout = layout.graphopt)
# plot (g.ap.grap , vertex.size=1, vertex.label.cex=0.5, edge.width=5, edge.color='orange', vertex.label=NA, layout = layout.kamada.kawai)
# tkplot(g.ap.grap , vertex.size=1, vertex.label.cex=0.5, edge.width=5, edge.color='orange', vertex.label=NA, layout = layout.graphopt)
# ceb <- cluster_edge_betweenness(g4s)
# plot(ceb, g4s, vertex.label=NA)

# Start-Stop time code
# Time it...
# Start the clock!
# ptm1 <- proc.time()
# Stop the clock
# cat ("\n")
# proc.time() - ptm1
# cat ("\n")