#' Extracts log age 
#'
#' @details
#' The user specifies a cohort and time period and then a covariate is constructed whether they are in the
#' cohort during the time periods relative to target population cohort index
#'
#' @param connection  The database connection
#' @param tempEmulationSchema The schema to use for temp tables
#' @param oracleTempSchema  DEPRECATED The temp schema if using oracle
#' @param cdmDatabaseSchema  The schema of the OMOP CDM data
#' @param cdmVersion  version of the OMOP CDM data
#' @param cohortTable  the table name that contains the target population cohort
#' @param rowIdField  string representing the unique identifier in the target population cohort
#' @param aggregated  whether the covariate should be aggregated
#' @param cohortId cohort id for the target cohort
#' @param covariateSettings  settings for the covariate cohorts and time periods
#' @param ...  additional arguments from FeatureExtraction
#'
#' @return
#' CovariateData object with covariates, covariateRef, and analysisRef tables
#' @export
getLogAgeCovariateData <- function(connection,
                                   tempEmulationSchema = NULL,
                                        oracleTempSchema = NULL,
                                        cdmDatabaseSchema,
                                        cdmVersion = "5",
                                        cohortTable = "#cohort_person",
                                        rowIdField = "row_id",
                                        aggregated,
                                        cohortId,
                                        covariateSettings, 
                                        ...
) {
  
  # to get table 1 - take source values and then map them - dont map in SQL
  
  # Some SQL to construct the covariate:
  sql <- paste("SELECT c.@row_id_field AS row_id, ",
               "@covariate_id as covariate_id,",
               "LOG((YEAR(c.cohort_start_date)- p.year_of_birth)*@age_multiply) as covariate_value",
               "FROM @cdm_database_schema.person p INNER JOIN @cohort_temp_table c 
                ON c.subject_id = p.person_id",
               ";"
  )
  
  sql <- SqlRender::render(sql,
                           cohort_temp_table = cohortTable,
                           row_id_field = rowIdField,
                           cdm_database_schema = cdmDatabaseSchema,
                           covariate_id = 1020,
                           age_multiply = covariateSettings$ageMultiply
                           
  )
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"),
                              oracleTempSchema = oracleTempSchema)
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql(connection, sql)
  # Convert colum names to camelCase:
  colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))
  
  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = 1020,
                             covariateName = 'Log age',
                             analysisId = 20,
                             conceptId = 0,
                             valueAsConceptId = 0,
                             collisions = NA
                             )
  
  analysisRef <- data.frame(analysisId = 20,
                            analysisName = "log age covariate",
                            domainId = "log age covariate",
                            startDay = 0,
                            endDay = 0,
                            isBinary = "N",
                            missingMeansZero = "N")
  
  result <- Andromeda::andromeda(covariates = covariates,
                                 covariateRef = covariateRef,
                                 analysisRef = analysisRef)
  class(result) <- "CovariateData"	
  return(result)
}

#' Create covariates settings for log age covariate
#'
#' @details
#' The user specifies 
#'
#' @param ageMultiply a value to multiple age by before logging
#'
#' @return
#' An object of class `covariateSettings` specifying how to create the cohort covariate with the covariateId
#'  cohortId x 100000 + settingId x 1000 + analysisId
#'  
#' @export
createLogAgeCovariateSettings <- function(ageMultiply = 1) {
  
  covariateSettings <- list(ageMultiply = ageMultiply)
  
  attr(covariateSettings, "fun") <- "QRISKvalidation::getLogAgeCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}
