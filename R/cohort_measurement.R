#' Extracts measurement covariates based on being in a cohort
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
getCohortMeasurementCovariateData <- function(connection,
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
  sql <- paste("SELECT c.@row_id_field AS row_id, 
               @covariate_id as covariate_id,
               measurement_concept_id, 
               unit_concept_id,",
               "m.value_as_number,",
               "m.measurement_date,",
               "ABS(datediff(dd, measurement_date, c.cohort_start_date)) AS index_time",
               "FROM @cdm_database_schema.measurement m INNER JOIN @cohort_temp_table c 
                ON c.subject_id = m.person_id",
               "AND measurement_date >= dateadd(day, @startDay, cohort_start_date) ",
               "AND measurement_date <= dateadd(day, @endDay, cohort_start_date) ",
               "INNER JOIN @cdm_database_schema.person p ON p.person_id=c.subject_id",
               "INNER JOIN (select * from @cohort_cov_schema.@cohort_cov WHERE
                            cohort_definition_id in (@cohort_cov_ids)) cohort_of_int ",
               "ON cohort_of_int.subject_id = c.subject_id ",
               "AND cohort_of_int.cohort_start_date <= dateadd(day, @c_end_day , c.cohort_start_date)",
               "AND cohort_of_int.cohort_end_date >= dateadd(day, @c_start_day , c.cohort_start_date)",
               "WHERE m.measurement_concept_id IN (@concepts) 
                AND value_as_number IS NOT NULL
               {@use_min}?{AND value_as_number >= @min_val}
               {@use_max}?{AND value_as_number <= @max_val}
               {@restrict_units}?{
               AND (unit_concept_id IN (@units) {@na_unit}?{OR unit_concept_id  is NULL})
               }
               ;
               "
  )
  
  sql <- SqlRender::render(sql,
                           covariate_id = covariateSettings$covariateId,
                           cohort_temp_table = cohortTable,
                           row_id_field = rowIdField,
                           
                           cohort_cov_schema = covariateSettings$cohortDatabaseSchema,
                           cohort_cov = covariateSettings$cohortTable,
                           cohort_cov_ids = covariateSettings$cohortId,
                           c_start_day = covariateSettings$cohortStartDay,
                           c_end_day = covariateSettings$cohortEndDay,
                           
                           startDay=covariateSettings$measurementStartDay,
                           endDay=covariateSettings$measurementEndDay,
                           concepts = paste(covariateSettings$measurementConceptSet, collapse = ','),
                           cdm_database_schema = cdmDatabaseSchema,
                           use_min  = !is.null(covariateSettings$measurementMinVal),
                           min_val  = covariateSettings$measurementMinVal,
                           use_max = !is.null(covariateSettings$measurementMaxVal),
                           max_val  = covariateSettings$measurementMaxVal,
                           restrict_units = !is.null(covariateSettings$measurementUnitSet),
                           na_unit = NA %in% covariateSettings$measurementUnitSet,
                           units = paste(covariateSettings$measurementUnitSet[!is.na(covariateSettings$measurementUnitSet)], collapse = ',')
  )
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"),
                              oracleTempSchema = oracleTempSchema)
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql(connection, sql)
  # Convert colum names to camelCase:
  colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))
  
  # map data:
  covariates <- covariates[!is.na(covariates$valueAsNumber),]
  
  # if scaleMap is a function call it otherwise eval the string function
  if(inherits(covariateSettings$measurementScaleMap, 'function')){
    covariates <- covariateSettings$measurementScaleMap(covariates)
  } else{
    scaleMap <- eval(parse(text = covariateSettings$measurementScaleMap))
    covariates <- scaleMap(covariates)
  }
  
  # aggregate data:
  if(covariateSettings$measurementAggregateMethod == 'max'){
    covariates <- covariates %>% 
      dplyr::group_by(.data$rowId, .data$covariateId) %>%
      dplyr::summarize(covariateValue = max(.data$valueAsNumber))
  } else if(covariateSettings$measurementAggregateMethod == 'min'){
    covariates <- covariates %>% 
      dplyr::group_by(.data$rowId, .data$covariateId) %>%
      dplyr::summarize(covariateValue = min(.data$valueAsNumber))
  } else if(covariateSettings$measurementAggregateMethod == 'mean'){
    covariates <- covariates %>% 
      dplyr::group_by(.data$rowId, .data$covariateId) %>%
      dplyr::summarize(covariateValue = mean(.data$valueAsNumber))
  } else if(covariateSettings$measurementAggregateMethod == 'median'){
    covariates <- covariates %>% 
      dplyr::group_by(.data$rowId, .data$covariateId) %>%
      dplyr::summarize(covariateValue = median(.data$valueAsNumber))
  } else{
    last <- covariates %>% 
      dplyr::group_by(.data$rowId, .data$covariateId) %>%
      dplyr::summarize(lastTime = min(.data$indexTime))
    
    covariates <- merge(covariates,last, 
                        by.x = c('rowId','covariateId','indexTime'), 
                        by.y = c('rowId','covariateId','lastTime') )
    
    covariates <- covariates %>% 
      dplyr::group_by(.data$rowId, .data$covariateId) %>%
      dplyr::summarize(
        covariateValue = mean(.data$valueAsNumber)
      )
  }
  
  covariates <- covariates %>% dplyr::select("rowId", "covariateId", "covariateValue")
  
  
  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = covariateSettings$covariateId,
                             covariateName = paste('Cohort measurement during day',
                                                   covariateSettings$measurementStartDay,
                                                   'through',
                                                   covariateSettings$measurementEndDay,
                                                   'days relative to index:',
                                                   covariateSettings$covariateName
                             ),
                             analysisId = covariateSettings$analysisId,
                             conceptId = 0,
                             valueAsConceptId = 0,
                             collisions = NA
                             )
  
  analysisRef <- data.frame(analysisId = covariateSettings$analysisId,
                            analysisName = "cohort measurement covariate",
                            domainId = "cohort measurement covariate",
                            startDay = covariateSettings$cohortStartDay,
                            endDay = covariateSettings$cohortEndDay,
                            isBinary = "N",
                            missingMeansZero = "Y")
  
  result <- Andromeda::andromeda(covariates = covariates,
                                 covariateRef = covariateRef,
                                 analysisRef = analysisRef)
  class(result) <- "CovariateData"	
  return(result)
}

#' Create covariates settings for measurements
#'
#' @details
#' The user specifies 
#'
#' @param covariateName The name of the covariate
#' @param cohortDatabaseSchema The schema where the cohort is in
#' @param cohortTable The table name where the cohort is in
#' @param cohortId The cohortDefinitionId of the cohort to restrict to
#' @param cohortStartDay the start time relative to index to look for the person being in the cohort
#' @param cohortEndDay the end time relative to index to look for the person being in the cohort
#' @param measurementConceptSet A vector of conceptIds that are the measurement concepts
#' @param measurementUnitSet NULL or a vector of conceptIds to restrict the units to (can include NA)
#' @param measurementStartDay the start time before index to look for the measurement
#' @param measurementEndDay the end time before index to look for the measurement
#' @param measurementScaleMap A function that lets you concept units into a standard unit and do any scaling
#' @param measurementMinVal NULL or the min valid value for the measurements (value less than this are excluded)
#' @param measurementMaxVal NULL or the max valid value for the measurements (value more than this are excluded)
#' @param measurementAggregateMethod one of max/min/mean/median/recent how to handle multiple measurements
#' @param covariateId a unique value for the covariateId
#' @param analysisId a unique value for the analysisId
#'
#' @return
#' An object of class `covariateSettings` specifying how to create the cohort covariate with the covariateId
#'  cohortId x 100000 + settingId x 1000 + analysisId
#'  
#' @export
createCohortMeasurementCovariateSettings <- function(
    covariateName,
    cohortDatabaseSchema,
    cohortTable,
    cohortId, 
    cohortStartDay,
    cohortEndDay,
    measurementConceptSet,
    measurementUnitSet = NULL,
    measurementStartDay=-30, 
    measurementEndDay=0, 
    measurementScaleMap = function(x){sapply(x, function(y){y})}, 
    measurementMinVal = NULL,
    measurementMaxVal = NULL,
    measurementAggregateMethod = 'recent',
    covariateId = 1566,
    analysisId = 566
) {
  
  covariateSettings <- list(
    covariateName = covariateName,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    cohortId = cohortId, 
    cohortStartDay = cohortStartDay,
    cohortEndDay = cohortEndDay,
    measurementConceptSet = measurementConceptSet,
    measurementUnitSet = measurementUnitSet,
    measurementStartDay = measurementStartDay, 
    measurementEndDay = measurementEndDay, 
    measurementScaleMap = measurementScaleMap, 
    measurementMinVal = measurementMinVal,
    measurementMaxVal = measurementMaxVal,
    measurementAggregateMethod = measurementAggregateMethod,
    covariateId = covariateId,
    analysisId = analysisId
  )
  
  attr(covariateSettings, "fun") <- "QRISKvalidation::getCohortMeasurementCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}