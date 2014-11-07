# ----------------------
# Initialize Environment & Connect to Db
# ----------------------

# environment for variables to avoid collisions with .GlobalEnv
pkgEnv <- new.env(parent = emptyenv())

initializeEnvironment()  

# ----------------------
# Connect to Db
# ----------------------
# note app must be in both assignment as argument of dbfilepathname otherwise it doesn't work
#app <- dbfilepathname(app, "U:/NCPN_WORK/___TEST_DATA/NPSTORET_BE_rw_20140814.MDB")
#app <- dbfilepathname(app, "U:/NCPN_WORK/___WQ_R/NPSTORET_DATA_BU/NPSTORET_backup_TEST.mdb")

# home
app <- dbfilepathname(app, "U:/NCPN_WORK/___TEST_DATA/NPSTORET_BE_rw_20140814alt.MDB")
# work
#app <- dbfilepathname(app, "C:/___TEST_DATA/NPSTORET_BE_rw_20140814alt.MDB")

app$connect <- odbcConnectAccess2007(app[["dbfilepathname"]])

#environment(app)<-pkgEnv

assign("npstoret",app,envir=pkgEnv)

# Start the clock!
  ptm <- proc.time()

initializeData()

# Stop clock
  proc.time() - ptm
  print(proc.time()-ptm)

# ------------------------
#  Subset Results
# ------------------------

# prepare subsets for identifying if results have indep std / dep std / no std


# chunk the result df allres

# ------------------------
#  Split Results
# ------------------------
# http://stackoverflow.com/questions/14164525/splitting-a-large-data-frame-into-smaller-segments
# BondedDust Jan 24, 2013
# split(df, (seq(nrow(df))-1) %/% 200) 

# split the results df into X segments

splitResults <- split(allres, (seq(nrow(allres))-1) %/% 10000)

df1 <- splitResults[[1]] # dataframe of first set

# lookup & populate pH, hardness, etc.
allres$pH <- unlist(mapply(lookupDependentCharResult,allres$DepCharLookup, 
                           MoreArgs = list(park=allres$Park, stationID =allres$StationID,
                                           startDate = allres$START_DATE, startTimeZone = allres$START_TIME_ZONE, 
                                           smpl_frac = allres$SMPL_FRAC_TYPE_NM, medium = allres$MEDIUM, 
                                           field_lab = allres$FIELD_LAB, uom = allres$UOM)))

#> allres$pH <- unlist(mapply(lookupDependentCharResult,"pH", allres$Park, allres$StationID,
#                             +                            allres$START_DATE, allres$START_TIME_ZONE, allres$SMPL_FRAC_TYPE_NM,
#                             +                            allres$MEDIUM, allres$FIELD_LAB, allres$UOM))
#Error in exists(dfName) : invalid first argument 

df1$pH <- unlist(mapply(lookupDependentCharResult,df1$DepCharLookup, 
                        MoreArgs = list(park=df1$Park, stationID =df1$StationID,
                                        startDate = df1$START_DATE, startTimeZone = df1$START_TIME_ZONE, 
                                        smpl_frac = df1$SMPL_FRAC_TYPE_NM, medium = df1$MEDIUM, 
                                        field_lab = df1$FIELD_LAB, uom = df1$UOM)))
# Error in nrow(dfMatch) : object 'dfMatch' not found 

# handle START_DATE in POSIXct > DATE
ps <- head(tail(pH,500),1)
phSTART <- as.Date(as.POSIXct(ps$START_DATE,tz=ps$START_TIME_ZONE))
class(phSTART)
[1] "Date"
[1] "2009-12-16"
class(pH$START_DATE) #[1] "POSIXct" "POSIXt" 
class(allres$START_DATE) #[1] "POSIXct" "POSIXt" 
phSTART <- as.Date(as.POSIXct(ps$START_DATE,tz=ps$START_TIME_ZONE,"%Y-%m-%d"))
# timezones -> OlsonNames() does NOT include "MDT" or "MWT" so, these values must not be used directly in conversions
# dates -> values are "1945-08-03 MST" or "1943-01-01 MWT"
as.Date(as.character(pH$START_DATE,"%Y-%m-%d"))

#Error in Ops.Date("START_DATE=", as.Date(as.character(pH$START_DATE, "%Y-%m-%d"))) : 
#  & not defined for "Date" objects

# ------------------------
#  Subset Results
# ------------------------
# subset to results that have indep / dep / no std
depResults <- allres[allres$LocChDef_IS_NUMBER %in% depStds$LocCHDEF_IS_NUMBER,] # dep results  46517 / 65  
indepResults <- allres[allres$LocChDef_IS_NUMBER %in% indepStds$LocCHDEF_IS_NUMBER,] # indep results   144786 / 65

# set Std_Code values to identify results w/o std & those which require dependent / independent std
depResults$Std_Code <- "D"
indepResults$Std_Code <- "I"

# convert SMPL_FRAC_TYPE_NM & MEDIUM to lower case
indepResults$SMPL_FRAC_TYPE_NM <- tolower(indepResults$SMPL_FRAC_TYPE_NM)
indepResults$MEDIUM <- tolower(indepResults$MEDIUM)
depResults$SMPL_FRAC_TYPE_NM <- tolower(depResults$SMPL_FRAC_TYPE_NM)
depResults$MEDIUM <- tolower(depResults$MEDIUM)





# ====> NO OVERLAP of Dep & Indep Results (compare by %in% LocChDef_IS_NUMBER in both)

# see which results are not in bothResults??
# http://stackoverflow.com/questions/3171426/compare-two-data-frames-to-find-the-rows-in-data-frame-1-that-are-not-present-in
# nulglob July 3, 2010
# use package Compare
#pkgTest("Compare") # package 'Compare' is not available (for R version 3.1.0)
#library("Compare") # Error in library("Compare") : there is no package called 'Compare'

# merge the 3
mResults <- rbind(depResults,indepResults,nostdResults)

#dupRows <- dupsBetweenGroups(df,"Std_Code")




# ----------------------------------------------
#  Dependent Characteristic Std Results
# ----------------------------------------------
# pH, Hardness, Temp, Hardness_UOM, Temp_UOM
# coeffs 1-8
# Std_CalcFormula
# ----------------------------------------------

# populate results into pH, Hardness, Temp, Hardness_UOM, Temp_UOM

# populate Std_CalcFormula


# ----------------------------------------------
#  Independent Charactersitic Std Results
# ----------------------------------------------
# - no additional columns -

# ----------------------------------------------
#  ALL
# ----------------------------------------------
# Std_Value
# Std_LoHi  +1/0/-1
# Std_UOM
# Std_Medium
# Std_SMPL_FRAC_TYPE_NM
# Std_Compliance +1/0/-1# prepare list
# ----------------------------------------------

# Populate Std Characteristic Values



# ----------------------------------------------
# results & indepStds MUST match the following values
#
# match:  PARK_ID, Station_ID, Medium, UOM, SMPL_FRAC_TYPE_NM & LocChDef_IS_NUMBER/LocCHDEF_IS_NUMBER
# ----------------------------------------------

# check for NA values  --> all OK no NAs
# unique(ires$StationID)
# unique(ires$Park)
# unique(ires$MEDIUM)  # [1] "Water" "water"  --> use tolower to avoid case affecting match
# unique(ires$SMPL_FRAC_TYPE_NM)  # [1] NA  "Total"   "Dissolved"   "Suspended" [5] "Total Recovrble" "Acid Soluble" 
# unique(dfIndepChars$Station_ID)
# unique(dfIndepChars$PARK_ID)
# unique(dfIndepChars$Medium)   # [1] "Water" --> use tolower to avoid case affecting match
# unique(dfIndepChars$SMPL_FRAC_TYPE_NM)  # [1] NA "Dissolved" "Total Recovrble" "Total" 

# X <- merge(ires, dfIndepChars, 
#           by.x = c("LocChDef_IS_NUMBER","Park","StationID", "MEDIUM", "SMPL_FRAC_TYPE_NM"), 
#           by.y = c("LocCHDEF_IS_NUMBER","PARK_ID","Station_ID", "Medium", "SMPL_FRAC_TYPE_NM"), suffixes=c(".r",".s")) #7702 / 104
#Warning message:
#  In merge.data.frame(ires, dfIndepChars, by.x = c("LocChDef_IS_NUMBER",  :
#  column name 'StationID' is duplicated in the result

# set the column names such that you can distinguish between results & characters
# colnames(ires) <- paste(colnames(res), "r", sep=".")
# colnames(dfIndepChars) <- paste(colnames(dfIndepChars), "s", sep=".")
# colnames(ires) <- paste(colnames(res), "r", sep="_")
# colnames(dfIndepChars) <- paste(colnames(dfIndepChars), "s", sep="_")
# merge fails even w/ renamed by.x & by.y columns --> 0/104

# DO NOT ADD SUFFIX TO COLUMN NAMES! --> RESULTS in 0 for merge!
# colnames(dfIndepChars)

# merge including UOM/SHORT_FORM_NAME
# X <- merge(ires, dfIndepChars, 
#           by.x = c("LocChDef_IS_NUMBER","Park","StationID", "MEDIUM", "SMPL_FRAC_TYPE_NM",  "UOM"), 
#           by.y = c("LocCHDEF_IS_NUMBER","PARK_ID","Station_ID", "Medium", "SMPL_FRAC_TYPE_NM", "SHORT_FORM_NAME"), suffixes=c(".r",".s")) #7702 / 104

# check if StationID can be used also for indep chars
X <- merge(ires, dfIndepChars, 
           by.x = c("LocChDef_IS_NUMBER","Park","StationID", "MEDIUM", "SMPL_FRAC_TYPE_NM",  "UOM"), 
           by.y = c("LocCHDEF_IS_NUMBER","PARK_ID","StationID", "Medium", "SMPL_FRAC_TYPE_NM", "SHORT_FORM_NAME"), suffixes=c(".r",".s")) #37870 / 103

# for comparison to determine why changing from Station_ID to StationID changed results # dramatically
# Y <- merge(ires, dfIndepChars, 
#           by.x = c("LocChDef_IS_NUMBER","Park","StationID", "MEDIUM", "SMPL_FRAC_TYPE_NM",  "UOM"), 
#           by.y = c("LocCHDEF_IS_NUMBER","PARK_ID","Station_ID", "Medium", "SMPL_FRAC_TYPE_NM", "SHORT_FORM_NAME"), suffixes=c(".r",".s")) #7702 / 104

unique(X$Station_ID)==> 89 diff values, chr
unique(X$StationID) ==> 89 diff values, num
unique(Y$StationID) ==> 21 diff values, num sci(?) & only long values??
[1] 404417108524900 383418107471401 383537107471500 381633107054700 381934107133500
[6] 382418107242600 382702107203900 382900107101600 382902107140400 382937107033500
[11] 382943107015300 383028107162200 383103106594200 383137107183600 382644107271000
[16] 382702107315400 382829107122200 382831107172600 382856107050000 382924107352300
[21] 383024107371800
unique(dfIndepChars$Station_ID)==> 89 diff values, chr
unique(ires$StationID) ==> 91 diff values, num
unique(dfIndepChars$StationID)==> 89 diff values, num sci


dfResults$pH <- unlist (mapply (lookupDependentCharResult, dfResults$DepCharLookup, 
                                MoreArgs = list(stationID = dfResults$stationID,
                                                startDate = dfResults$START_DATE, 
                                                startTimeZone = dfResults$START_TIME_ZONE,
                                                uom = dfResults$UOM, 
                                                medium = dfResults$MEDIUM, 
                                                field_lab = dfResults$FIELD_LAB)))

# indepResults
indepResults$pH <- unlist (mapply (lookupDependentCharResult, indepResults$DepCharLookup, 
                                   MoreArgs = list(stationID = indepResults$stationID,
                                                   startDate = indepResults$START_DATE, 
                                                   startTimeZone = indepResults$START_TIME_ZONE,
                                                   uom = indepResults$UOM, 
                                                   medium = indepResults$MEDIUM, 
                                                   field_lab = indepResults$FIELD_LAB)))
unique(indepResults$pH) # NULL?

# requires DepCharLookup
# create a list that is static for @ CharDependent
# ==> add a column that is just that value
dfResults$DepCharLookup <- "1e"
indepResults$DepCharLookup <- "1e" #pH
indepResults$DepCharLookup <- "261" #Hardness
indepResults$DepCharLOokup <- "481" #Temp

# lookupDependentCharResults requires DepCharLookup values & dfDepCharLookup via getDepCharResults
dfDepCharLookup <- getDepCharResults(dfResults)

# indepResults
indepResults$pH <- unlist (mapply (lookupDependentCharResult, indepResults$DepCharLookup, 
                                   MoreArgs = list(stationID = indepResults$stationID,
                                                   startDate = indepResults$START_DATE, 
                                                   startTimeZone = indepResults$START_TIME_ZONE,
                                                   uom = indepResults$UOM, 
                                                   medium = indepResults$MEDIUM, 
                                                   field_lab = indepResults$FIELD_LAB)))
