############################################################
# R initialize Functions
############################################################
# Purpose:    Single file access to miscellaneous NCPN app initialization functions
#
# Notes:      N/A
#
# Sources:  See individual functions
#              
# Revisions:  0.1  2014-09-29  B. Campbell  initial version
# ==========================================================

# ----------------------------------------------------------------------
#' @title initializeEnvironment
#' @description Load functions and information into global (working) environment
#'                    
#' @param none
#'
#' @section Requirements:
#'   none
#'
#' @section Sources:
#' \tabular{llllllll}{
#'   \tab 2014-09-30 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#' \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-09-30  \tab\tab BLC   \tab\tab Initial version \cr
#'   }
#' @family app initialization functions
#' @export
# ----------------------------------------------------------------------
initializeEnvironment <- function(){
  
# get this file's working directory
#  this.dir <- dirname(parent.frame(2)$ofile)
#  setwd(this.dir)
  
#  system.file("NCPN_NPSTORET_WQ", "R_NCPN_initialize_functions.R", package="NCPN_NPSTORET_WQ")
  
  # set the working directory
  #setwd("H:/Bonnie_files/__Projects/R_code_NPSTORET_WQstd_import")

  setWindowTitle("NCPN NPSTORET WQ App")

  # increase memory limit
  memory.limit(size = 4095)
  
  # ----------------------
  # --- load packages  --- 
  # ----------------------
  # ensure required packages are installed, if not install them
  #pkgTest("stringr")
  pkgTest("tools")
  pkgTest("RODBC")
  pkgTest("sqldf")
  pkgTest("shiny")
  pkgTest("sqldf")
  pkgTest("reshape2")
  
  # reference packages
  #library(stringr)
  library(tools)
  library(RODBC)
  library(shiny)
  library(sqldf)
  #library(reshape2)
  
  # ----------------------
  # --- load classes --- 
  # ----------------------
  
  # App Class
  filePath = paste(getwd(),"/R/R_app_class.R",sep="")
  source(filePath)
  
  # Samples & Standards Classes
  #filePath = paste(getwd(),"/R/R_sample_class.R",sep="")
  #source(filePath)  
  
  # ----------------------
  # --- load functions --- 
  # ----------------------
  
  # helpers
  filePath = paste(getwd(),"/R/R_helper_functions.R",sep="")
  source(filePath)
  # db
  #filePath = paste(getwd(),"/R/R_db_functions.R",sep="")
  #source(filePath)
  # NPSTORET
  filePath = paste(getwd(),"/R/R_NPSTORET_functions.R",sep="")
  source(filePath)
  # WQ
  filePath = paste(getwd(),"/R/R_WQstds_functions.R",sep="")
  source(filePath)
  # WQ Results
  filePath = paste(getwd(),"/R/R_WQ_results_functions.R",sep="")
  source(filePath)  
}

# ----------------------------------------------------------------------
#' @title initializeData
#' @description Load NPSTORET data into global (working) environment
#'              The following data frames are generated:
#'              \itemize{
#'              \item dfResults - contains ALL NPSTORET results
#'              \item dfRAWStds - unmodified NPSTORET standard characteristics
#'              \item dfIndepChars - unmodified NPSTORET independent characteristics
#'              \item dfRAWDepChars - unmodified NPSTORET dependent characteristics
#'              \item dfDepCharResults - sample results for pH, hardness (Ca+Mg), temperature (dependencies)
#'              \item dfRAWChars - all unmodified NPSTORET characteristics
#'              \item depStds - dependent standards
#'              \item indepStds - independent standards
#'              \item dStds - dependent standards
#'              \item iStds -  independent standards
#'              \item pH - all pH sample results
#'              \item Hardness - all Hardness (Ca + Mg) sample results
#'              \item Temp - all water Temperature sample results
#'              }
#'              * Dependent characteristics include standard values which depend on other sample results (like pH, temp, hardness (Ca + Mg), time).
#'                Independent characteristics include standard values that do not rely on other sample results.      
#' @param app - The application variable in the Global Environment which contains the 
#'              database filename and path (app[['dbfilepathname']]). 
#'              Without this value the function will fail to find the defined file name and path and
#'              the database connections for loading from NPSTORET will fail
#'              with a somewhat cryptic "first argument is not an open RODBC channel" error.
#'
#' @section Requirements:
#'   \tabular{l}{
#'   RODBC library \cr
#'   sqldf library \cr
#'   app dbfilepathname for valid NPSTORET database with required tables \cr
#'   }
#'
#' @section Sources:
#' \tabular{llllllll}{
#'   \tab 2014-10-05 \tab\tab B. Campbell \tab\tab 0.1 \tab\tab Initial version \cr
#'   }
#' @section Revisions:
#' \tabular{llllllll}{
#'   \tab 0.1   \tab\tab 2014-10-05  \tab\tab BLC   \tab\tab Initial version \cr
#'   }
#' @family app initialization functions
#' @export
# ----------------------------------------------------------------------
initializeData <- function(){
  
#  app <- dbfilepathname(app, dbfilepathname)
  #npstoret <- odbcConnectAccess2007(app[["dbfilepathname"]])
  #dfResults <- loadNPSTORETWQData("Results")
  
#  app$connect <- odbcConnectAccess2007(app[["dbfilepathname"]])
  
  
  # ------------------------
  #  Fetch NPSTORET Results
  # ------------------------
  # Start the clock!
  ptm <- proc.time()
  
  dfResults <- loadNPSTORETWQData("Results")
  assign("dfResults", dfResults, envir=.GlobalEnv)
  
  # ------------------------
  #  Subset Results
  # ------------------------
  
  # prepare subsets for identifying if results have indep std / dep std / no std
  
  #app <- dbfilepathname(app, "U:/NCPN_WORK/___TEST_DATA/NPSTORET_BE_rw_20140814.MDB")
  dfRAWStds <- getWQRawStdCharacteristics()
  assign("dfRAWStds", dfRAWStds, envir=.GlobalEnv)
  
  # -------------------------------------------
  #  Get Independent Characteristics
  # -------------------------------------------
  dfIndepChars <- getIndepWQStdChars() #characteristics
  assign("dfIndepChars", dfIndepChars, envir=.GlobalEnv)
  
  # -------------------------------------------
  #  Get Dependent Characteristics
  # -------------------------------------------
  dfRAWDepChars <- getRAWDepWQStdChars()
  assign("dfRAWDepChars", dfRAWDepChars, envir=.GlobalEnv)
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # DATA CHECKS: 
  # dfDeps <- sqldf("SELECT DISTINCT CharDependent FROM dfRAWDepChars WHERE IsDependent = 1;")
  # RESULT ==> 5 values: 261, time, 481n, 481x, 1e
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # -------------------------------------------
  #  Get Dependent Characteristic Results (for std calculations)
  # -------------------------------------------
  dfDepCharResults <- getDepCharResults(dfResults)
  assign("dfDepCharResults", dfDepCharResults, envir=.GlobalEnv)
  
  # ------------------------
  #  Get Characteristics
  # ------------------------
  dfRAWChars <- getWQRawStdCharacteristics()
  assign("dfRAWChars", dfRAWChars, envir=.GlobalEnv)
  
  # ------------------------
  #  Subset Stds
  # ------------------------
  # subsets for quick match to subset results into indep/dep/no std results
  indepStds <- dfIndepChars[,c("DISPLAY_NAME", "LocCHDEF_IS_NUMBER")] # indep std   41586 / 2
  depStds <- dfRAWDepChars[,c("DISPLAY_NAME", "LocCHDEF_IS_NUMBER")] # dep std    9626 / 2
  
  # working dfs
  iStds <- dfIndepChars
  dStds <- dfRAWDepChars
  
  # convert SMPL_FRAC_TYPE_NM & MEDIUM to lower case for matching
  iStds$SMPL_FRAC_TYPE_NM <- tolower(iStds$SMPL_FRAC_TYPE_NM)
  dStds$SMPL_FRAC_TYPE_NM <- tolower(dStds$SMPL_FRAC_TYPE_NM)
  
  assign("indepStds", indepStds, envir=.GlobalEnv)
  assign("depStds", depStds, envir=.GlobalEnv)
  assign("iStds", iStds, envir=.GlobalEnv)
  assign("dStds", dStds, envir=.GlobalEnv)
  
  # unique(dfRAWDepChars$DISPLAY_NAME) # 11 stds: Cd, Cu, Pb, Ni, Ag, U, Mn, N, Zn, PCP, Temp
  # unique(depStds$DISPLAY_NAME) # matches dfRAWDepChars
  # unique(depStds$LocCHDEF_IS_NUMBER)
  
  # remove chars
  # rm(dfRAWDepChars)
  # rm(dfIndepChars)
  # rm(dfDepCharResults) #38171
  
  # ------------------------
  #  Prep Results (add cols)
  # ------------------------
  # add columns to results sets
  allres <- AddResultColumns(dfResults)
  
  # convert SMPL_FRAC_TYPE_NM & MEDIUM to lower case
  allres$SMPL_FRAC_TYPE_NM <- tolower(allres$SMPL_FRAC_TYPE_NM)
  allres$MEDIUM <- tolower(allres$MEDIUM)
  
  # set Std_Code value
  allres$Std_Code <- "R"
  
  # ------------------------
  #  Add DepChar Result Values
  # ------------------------
  
  # set DepCharLookup
  allres$DepCharLookup <- "pH"
  
  assign("allres", allres, envir=.GlobalEnv)
  
  # lookup same sample station/date/medium/field_lab/smpl_frac_type_nm/uom in dfDepCharResults
  #rm(dStds)
  #rm(dfResults)
  #rm(indepStds)
  #rm(res)
  #rm(depStds)
  #rm(dfIndepChars)
  
  # subset dfDepCharResults to reduce memory issues
  pH <- dfDepCharResults[dfDepCharResults$DISPLAY_NAME == "pH",] # 14827 / 65
  Temp <- dfDepCharResults[dfDepCharResults$DISPLAY_NAME == "Temperature, water",] # 12111 / 65
  Hardness <- dfDepCharResults[dfDepCharResults$DISPLAY_NAME == "Hardness, Ca + Mg",] # 11233 / 65
  
  # make dfs available to .GlobalEnv
  assign("pH", pH, envir=.GlobalEnv)
  assign("Temp", Temp, envir=.GlobalEnv)
  assign("Hardness", Hardness, envir=.GlobalEnv)

  # Stop clock
  proc.time() - ptm
  print(proc.time()-ptm)
}