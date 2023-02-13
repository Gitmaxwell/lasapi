#' Prepare a LAS file for use in API
#'
#' Reads a CWLS LAS file and returns a list object with the VERSION, WELL, CURVE, PARAMETER, OTHER and LOG sections, as well as storing the original path of the file.
#' @param filepath string       path to the LAS file
#' @param replace_null boolean  replace the NULLS in the LOG section (as given by the WELL table) to the R NA value
#' @return Returns a data frame of cleaned las data for passing into API
#' @export
#'
prepare_las <- function(x)
{
  #x <- "./data/PMI1008DEN.las"
  las <- lastools::read_las(x) #add try catch
  #cleaning up the file
  #remove trailing and leading white spaces and spaces and convert to upper
  well_name <- subset(las$WELL$VALUE,las$WELL$MNEM =="WELL")
  well_name <- trimws(well_name, which = c("both"))
  well_name <- gsub(" ", "",well_name, fixed = TRUE)
  well_name <- gsub("-", "_",well_name, fixed = TRUE)
  well_name <- toupper(well_name)

  las$CURVE$MNEM <- trimws(las$CURVE$MNEM, which = c("both"))
  las$CURVE$MNEM <- gsub(" ", "",las$CURVE$MNEM, fixed = TRUE)
  las$CURVE$MNEM <- toupper(las$CURVE$MNEM)
  las$CURVE$UNIT <- trimws(las$CURVE$UNIT, which = c("both"))
  las$CURVE$UNIT <- gsub(" ", "",las$CURVE$UNIT, fixed = TRUE)
  las$CURVE$UNIT <- toupper(las$CURVE$UNIT)
  las$CURVE$MNEM <- trimws(las$CURVE$MNEM, which = c("both"))
  las$CURVE$MNEM <- gsub(" ", "",las$CURVE$MNEM, fixed = TRUE)
  las$CURVE$MNEM <- toupper(las$CURVE$MNEM)
  #concatenate the variable and units name
  las$MNEM.UNIT <- paste(las$CURVE$MNEM, las$CURVE$UNIT , sep=".")
  las$CURVE$DESCRIPTION <- trimws(las$CURVE$DESCRIPTION, which = c("both"))
  las$CURVE$DESCRIPTION <- gsub(" ", "",las$CURVE$DESCRIPTION, fixed = TRUE)
  las$CURVE$DESCRIPTION <- toupper(las$CURVE$DESCRIPTION)

  #now standardize the column names
  las_curve <- data.frame(MNEM =las$CURVE$MNEM, UNIT = las$CURVE$UNIT,DESCRIPTION =las$CURVE$DESCRIPTION)
  las_curve$MNEM_UNIT <- paste(las_curve$MNEM,las_curve$UNIT,sep=".")

  curvemap <- lasapi::las_curve_map

  library(dplyr)
  las_curve <- las_curve %>% left_join (curvemap, by = join_by(MNEM_UNIT == MAPPED_MNEM))
  las_curve$MNEM_CODE_UNIT <- paste(las_curve$MNEM_CODE, las_curve$UNIT.CODE, sep=".")

  #here we need to check the MNEM_CODE contains valid curves including :: DENB, DENL, CODE, CADE, GAMMA
  #check density information

  #check contains gamma
  if(!"GRDE.API" %in% las_curve$MNEM_CODE_UNIT)
  {"error code";}

  if(!"DENB.G/C3" %in% las_curve$MNEM_CODE_UNIT | !"DENL.G/C3" %in% las_curve$MNEM_CODE_UNIT | !"CODE.G/C3" %in% las_curve$MNEM_CODE_UNIT | "HDEN.G/C3")
  {"error code";}

  #now construct the las data frame
  Las_df <- las$LOG
  names(Las_df) <- las_curve$MNEM_CODE_UNIT
  Las_df$WELL <- well_name

  #now clean up data
  #convert and add CADE in MM

  if("CADE.CM" %in% colnames(Las_df))
  {
    Las_df$CADE.MM = Las_df$CADE.CM*10;
  }
  return(Las_df)
}
