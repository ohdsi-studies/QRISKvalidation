#' Extracts measurement ratio covariates
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
getMeasurementRatioCovariateData <- function(connection,
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
  message(paste0('running getMeasurementRatioCovariateData'))
  
  # Some SQL to construct the covariate:
  sqlTemplate <- paste("SELECT c.@row_id_field AS row_id, 
               measurement_concept_id, 
               unit_concept_id,",
               "value_as_number,",
               "measurement_date,",
               "YEAR(GETDATE()) - p.year_of_birth AS age_in_years,",
               "ABS(datediff(dd, measurement_date, c.cohort_start_date)) AS index_time",
               "FROM @cdm_database_schema.measurement m INNER JOIN @cohort_temp_table c 
                ON c.subject_id = m.person_id",
               "AND measurement_date >= dateadd(day, @startDay, cohort_start_date) ",
               "AND measurement_date <= dateadd(day, @endDay, cohort_start_date) ",
               "INNER JOIN @cdm_database_schema.person p ON p.person_id=c.subject_id",
               "WHERE m.measurement_concept_id IN (@concepts) 
                AND value_as_number IS NOT NULL
               {@use_min}?{AND value_as_number >= @min_val}
               {@use_max}?{AND value_as_number <= @max_val}
               {@restrict_units}?{
               AND (unit_concept_id IN (@units) {@na_unit}?{OR unit_concept_id  = 'NA'})
               }
               ;
               "
  )
  
  #==================
  # Fetching first covariates
  #==================
  sql <- SqlRender::render(sqlTemplate,
                           cohort_temp_table = cohortTable,
                           row_id_field = rowIdField,
                           startDay=covariateSettings$startDay,
                           endDay=covariateSettings$endDay,
                           concepts = paste(covariateSettings$conceptSet1, collapse = ','),
                           cdm_database_schema = cdmDatabaseSchema,
                           use_min  = !is.null(covariateSettings$minVal1),
                           min_val  = covariateSettings$minVal1,
                           use_max = !is.null(covariateSettings$maxVal1),
                           max_val  = covariateSettings$maxVal1,
                           restrict_units = !is.null(covariateSettings$unitSet1),
                           na_unit = NA %in% covariateSettings$unitSet1,
                           units = paste(covariateSettings$unitSet1[!is.na(covariateSettings$unitSet1)], collapse = ',')
  )
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"),
                              oracleTempSchema = oracleTempSchema)
  # Retrieve the covariate:
  covariates1 <- DatabaseConnector::querySql(connection, sql)
  # Convert colum names to camelCase:
  colnames(covariates1) <- SqlRender::snakeCaseToCamelCase(colnames(covariates1))
  
  # map data:
  covariates1 <- covariates1[!is.na(covariates1$valueAsNumber),]
  
  #==================
  # Fetching second covariates
  #==================
  sql <- SqlRender::render(sqlTemplate,
                           cohort_temp_table = cohortTable,
                           row_id_field = rowIdField,
                           startDay=covariateSettings$startDay,
                           endDay=covariateSettings$endDay,
                           concepts = paste(covariateSettings$conceptSet2, collapse = ','),
                           cdm_database_schema = cdmDatabaseSchema,
                           use_min  = !is.null(covariateSettings$minVal2),
                           min_val  = covariateSettings$minVal2,
                           use_max = !is.null(covariateSettings$maxVal2),
                           max_val  = covariateSettings$maxVal2,
                           restrict_units = !is.null(covariateSettings$unitSet2),
                           na_unit = NA %in% covariateSettings$unitSet2,
                           units = paste(covariateSettings$unitSet2[!is.na(covariateSettings$unitSet2)], collapse = ',')
  )
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"),
                              oracleTempSchema = oracleTempSchema)
  # Retrieve the covariate:
  covariates2 <- DatabaseConnector::querySql(connection, sql)
  # Convert colum names to camelCase:
  colnames(covariates2) <- SqlRender::snakeCaseToCamelCase(colnames(covariates2))
  
  # map data:
  covariates2 <- covariates2[!is.na(covariates2$valueAsNumber),]
  
  #==================
  # process both covariates
  # ====================
  
  # if scaleMap is a function call it otherwise eval the string function
  if(inherits(covariateSettings$scaleMap1, 'function')){
    covariates1 <- covariateSettings$scaleMap1(covariates1)
  } else{
    scaleMap1 <- eval(parse(text = covariateSettings$scaleMap1))
    covariates1 <- scaleMap1(covariates1)
  }
  if(inherits(covariateSettings$scaleMap2, 'function')){
    covariates2 <- covariateSettings$scaleMap2(covariates2)
  } else{
    scaleMap2 <- eval(parse(text = covariateSettings$scaleMap2))
    covariates2 <- scaleMap2(covariates2)
  }
  
  # aggregate data:
  if(covariateSettings$aggregateMethod == 'max'){
    covariates1 <- covariates1 %>% 
      dplyr::group_by(.data$rowId, .data$ageInYears) %>%
      dplyr::summarize(covariateValue1 = max(.data$valueAsNumber,na.rm = TRUE))
    covariates2 <- covariates2 %>% 
      dplyr::group_by(.data$rowId, .data$ageInYears) %>%
      dplyr::summarize(covariateValue2 = max(.data$valueAsNumber,na.rm = TRUE))
  } else if(covariateSettings$aggregateMethod == 'min'){
    covariates1 <- covariates1 %>% 
      dplyr::group_by(.data$rowId, .data$ageInYears) %>%
      dplyr::summarize(covariateValue1 = min(.data$valueAsNumber,na.rm = TRUE))
    covariates2 <- covariates2 %>% 
      dplyr::group_by(.data$rowId, .data$ageInYears) %>%
      dplyr::summarize(covariateValue2 = min(.data$valueAsNumber,na.rm = TRUE))
  } else if(covariateSettings$aggregateMethod == 'mean'){
    covariates1 <- covariates1 %>% 
      dplyr::group_by(.data$rowId, .data$ageInYears) %>%
      dplyr::summarize(covariateValue1 = mean(.data$valueAsNumber,na.rm = TRUE))
    covariates2 <- covariates2 %>% 
      dplyr::group_by(.data$rowId, .data$ageInYears) %>%
      dplyr::summarize(covariateValue2 = mean(.data$valueAsNumber,na.rm = TRUE))
  } else if(covariateSettings$aggregateMethod == 'median'){
    covariates1 <- covariates1 %>% 
      dplyr::group_by(.data$rowId, .data$ageInYears) %>%
      dplyr::summarize(covariateValue1 = median(.data$valueAsNumber,na.rm = TRUE))
    covariates2 <- covariates2 %>% 
      dplyr::group_by(.data$rowId, .data$ageInYears) %>%
      dplyr::summarize(covariateValue2 = median(.data$valueAsNumber,na.rm = TRUE))
  } else if(covariateSettings$aggregateMethod == 'stdev'){
    covariates1 <- covariates1 %>% 
      dplyr::group_by(.data$rowId, .data$ageInYears) %>%
      dplyr::summarize(covariateValue1 = sd(.data$valueAsNumber,na.rm = TRUE))
    covariates2 <- covariates2 %>% 
      dplyr::group_by(.data$rowId, .data$ageInYears) %>%
      dplyr::summarize(covariateValue2 = sd(.data$valueAsNumber,na.rm = TRUE))
  } else{
    last1 <- covariates1 %>% 
      dplyr::group_by(.data$rowId) %>%
      dplyr::summarize(lastTime = min(.data$indexTime, na.rm = TRUE))
    
    covariates1 <- merge(covariates1,last1, 
                        by.x = c('rowId','indexTime'), 
                        by.y = c('rowId','lastTime') )
    
    covariates1 <- covariates1 %>% 
      dplyr::group_by(.data$rowId, .data$ageInYears) %>%
      dplyr::summarize(
        covariateValue1 = mean(.data$valueAsNumber)
      )
    
    last2 <- covariates2 %>% 
      dplyr::group_by(.data$rowId) %>%
      dplyr::summarize(lastTime = min(.data$indexTime, na.rm = TRUE))
    
    covariates2 <- merge(covariates2,last2, 
                         by.x = c('rowId','indexTime'), 
                         by.y = c('rowId','lastTime') )
    
    covariates2 <- covariates2 %>% 
      dplyr::group_by(.data$rowId, .data$ageInYears) %>%
      dplyr::summarize(
        covariateValue2 = mean(.data$valueAsNumber)
      )
  }
  
  #================
  # now do the ratio
  #================
  covariates <- merge(covariates1, covariates2, by = c('rowId','ageInYears')) %>%
    dplyr::mutate(covariateValue = .data$covariateValue1/covariateValue2) %>%
    dplyr::select("rowId", "ageInYears", "covariateValue") %>%
    dplyr::filter(is.finite(.data$covariateValue)) # remove non-finite values
  
  
  # do age interaction
  if(covariateSettings$ageInteract){
    covariates <- covariates %>% 
      dplyr::mutate(covariateValue = .data$covariateValue*.data$ageInYears)
  }
  
  # center values 
  if(inherits(covariateSettings$centeringMap, 'function')){
    covariates <- covariateSettings$centeringMap(covariates)
  } else{
    centeringMap <- eval(parse(text = covariateSettings$centeringMap))
    covariates <- centeringMap(covariates)
  }
  
  # add covariateID:
  covariates$covariateId <- covariateSettings$covariateId
  
  covariates <- covariates %>% dplyr::select("rowId", "covariateId", "covariateValue")
  
  
  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = covariateSettings$covariateId,
                             covariateName = paste('Measurement ratio during day',
                                                   covariateSettings$startDay,
                                                   'through',
                                                   covariateSettings$endDay,
                                                   'days relative to index:',
                                                   covariateSettings$covariateName
                             ),
                             analysisId = covariateSettings$analysisId,
                             conceptId = 0,
                             valueAsConceptId = 0,
                             collisions = NA
                             )
  
  analysisRef <- data.frame(analysisId = covariateSettings$analysisId,
                            analysisName = "measurement ratio covariate",
                            domainId = "measurement ratio covariate",
                            startDay = covariateSettings$startDay,
                            endDay = covariateSettings$endDay,
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
#' @param conceptSet1 A vector of conceptIds that are the numerator measurement concepts#' 
#' @param conceptSet2 A vector of conceptIds that are the denominator measurement concepts
#' @param unitSet1 NULL or a vector of numerator conceptIds to restrict the units to (can include NA)
#' @param unitSet2 NULL or a vector of denominator conceptIds to restrict the units to (can include NA)
#' @param startDay the start time before index to look for the measurement
#' @param endDay the end time before index to look for the measurement
#' @param scaleMap1 A function that lets you map numerator concept units into a standard unit and do any scaling
#' @param scaleMap2 A function that lets you map denominator concept units into a standard unit and do any scaling
#' @param centeringMap A function to center the final ratio value
#' @param ageInteract Whether to do interaction with age in years
#' @param minVal1 NULL or the min valid value for the numerator measurements (value less than this are excluded)
#' @param maxVal1 NULL or the max valid value for the numerator measurements (value more than this are excluded)
#' @param minVal2 NULL or the min valid value for the denominator measurements (value less than this are excluded)
#' @param maxVal2 NULL or the max valid value for the denominator measurements (value more than this are excluded)
#' @param aggregateMethod one of max/min/mean/median/recent how to handle multiple measurements
#' @param covariateId a unique value for the covariateId
#' @param analysisId a unique value for the analysisId
#'
#' @return
#' An object of class `covariateSettings` specifying how to create the cohort covariate with the covariateId
#'  cohortId x 100000 + settingId x 1000 + analysisId
#'  
#' @export
createMeasurementRatioCovariateSettings <- function(
    covariateName, 
    conceptSet1,
    conceptSet2,
    unitSet1 = NULL,
    unitSet2 = NULL,
    startDay=-30, 
    endDay=0, 
    scaleMap1 = function(x){return(x)}, 
    scaleMap2 = function(x){return(x)},
    centeringMap = function(x){return(x)},
    ageInteract = FALSE,
    minVal1 = NULL,
    maxVal1 = NULL,
    minVal2 = NULL,
    maxVal2 = NULL,
    aggregateMethod = 'recent',
    covariateId = 1486,
    analysisId = 486
) {
  
  covariateSettings <- list(covariateName=covariateName, 
                            conceptSet1=conceptSet1,
                            conceptSet2=conceptSet2,
                            unitSet1 = unitSet1,
                            unitSet2 = unitSet2,
                            startDay=startDay,
                            endDay=endDay,
                            scaleMap1=scaleMap1,
                            scaleMap2=scaleMap2,
                            centeringMap = centeringMap,
                            ageInteract = ageInteract,
                            aggregateMethod = aggregateMethod,
                            minVal1 = minVal1,
                            maxVal1 = maxVal1,
                            minVal2 = minVal2,
                            maxVal2 = maxVal2,
                            covariateId = covariateId,
                            analysisId = analysisId
  )
  
  attr(covariateSettings, "fun") <- "QRISKvalidation::getMeasurementRatioCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}
