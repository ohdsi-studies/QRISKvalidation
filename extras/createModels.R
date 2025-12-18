library(FeatureExtraction)
library(PatientLevelPrediction)
cohortDatabaseSchema <- 'cohortDatabaseSchema'
cohortTableName <- 'cohortTableName'

#======================================================
# Specify the Time-At-Risk 
#======================================================

#1 year TAR
populationSettings1 <- PatientLevelPrediction::createStudyPopulationSettings(
  requireTimeAtRisk = FALSE, 
  riskWindowStart = 1, 
  startAnchor = 'cohort start',
  riskWindowEnd = 365, 
  endAnchor = 'cohort start'
  )

#3 years TAR
populationSettings3 <- PatientLevelPrediction::createStudyPopulationSettings(
  requireTimeAtRisk = FALSE, 
  riskWindowStart = 1, 
  startAnchor = 'cohort start',
  riskWindowEnd = 1095, 
  endAnchor = 'cohort start'
  )

#5 year TAR
populationSettings5 <- PatientLevelPrediction::createStudyPopulationSettings(
  requireTimeAtRisk = FALSE, 
  riskWindowStart = 1, 
  startAnchor = 'cohort start',
  riskWindowEnd = 1825, 
  endAnchor = 'cohort start'
  )

#10 years TAR
populationSettings10 <- PatientLevelPrediction::createStudyPopulationSettings(
  requireTimeAtRisk = FALSE, 
  riskWindowStart = 1, 
  startAnchor = 'cohort start',
  riskWindowEnd = 3650, 
  endAnchor = 'cohort start'
  )

#=======================================================================================
#Mappings for different QRISK models
#=======================================================================================

#ADD THE DIFFERENT BASELINES TO THE FORMULAS

#QRISK1_male_1
QRISK1_male_1 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.0009293994
baseline*exp(x)
})}"

#QRISK1_female_1
QRISK1_female_1 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.0004715117
baseline*exp(x)
})}"

#QRISK2_male_1
QRISK2_male_1 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.0008006027
baseline*exp(x)
})}"

#QRISK2_female_1
QRISK2_female_1 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.0004125753
baseline*exp(x)
})}"

#QRISK3_male_1
QRISK3_male_1 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 	0.0010752090
baseline*exp(x)
})}"

#QRISK3_female_1
QRISK3_female_1 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <-		0.0007901144
baseline*exp(x)
})}"

#QRISK4_male_1
QRISK4_male_1 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.0009563304
baseline*exp(x)
})}"

#QRISK4_female_1
QRISK4_female_1 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.0007016125
baseline*exp(x)
})}"

#QRISK1_male_3
QRISK1_male_3 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.005023713
baseline*exp(x)
})}"

#QRISK1_female_3
QRISK1_female_3 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.002722517
baseline*exp(x)
})}"

#QRISK2_male_3
QRISK2_male_3 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.004285459
baseline*exp(x)
})}"

#QRISK2_female_3
QRISK2_female_3 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.002345942
baseline*exp(x)
})}"

#QRISK3_male_3
QRISK3_male_3 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.005287401
baseline*exp(x)
})}"

#QRISK3_female_3
QRISK3_female_3 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 	0.003954327
baseline*exp(x)
})}"

#QRISK4_male_3
QRISK4_male_3 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 	0.004712633
baseline*exp(x)
})}"

#QRISK4_female_3
QRISK4_female_3 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.003544200
baseline*exp(x)
})}"

#QRISK1_male_5
QRISK1_male_5 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.01178898
baseline*exp(x)
})}"

#QRISK1_female_5
QRISK1_female_5 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.006423349
baseline*exp(x)
})}"

#QRISK2_male_5
QRISK2_male_5 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.010099676
baseline*exp(x)
})}"

#QRISK2_female_5
QRISK2_female_5 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <-  0.005569924
baseline*exp(x)
})}"

#QRISK3_male_5
QRISK3_male_5 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.01195869
baseline*exp(x)
})}"

#QRISK3_female_5
QRISK3_female_5 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.008979295
baseline*exp(x)
})}"

#QRISK4_male_5
QRISK4_male_5 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.01068184
baseline*exp(x)
})}"

#QRISK4_female_5
QRISK4_female_5 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.008118336
baseline*exp(x)
})}"

#QRISK1_male_10
QRISK1_male_10 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.04239061
baseline*exp(x)
})}"

#QRISK1_female_10
QRISK1_female_10 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.02390315
baseline*exp(x)
})}"

#QRISK2_male_10
QRISK2_male_10 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.03680173
baseline*exp(x)
})}"

#QRISK2_female_10
QRISK2_female_10 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.02103937
baseline*exp(x)
})}"

#QRISK3_male_10
QRISK3_male_10 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.04021597
baseline*exp(x)
})}"

#QRISK3_female_10
QRISK3_female_10 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.03020239
baseline*exp(x)
})}"

#QRISK4_male_10
QRISK4_male_10 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.03603195
baseline*exp(x)
})}"

#QRISK4_female_10
QRISK4_female_10 <- updatedQRISKmap <- "function(x){ sapply(x, function(x){
baseline <- 0.02771843
baseline*exp(x)
})}"

#===========================================================
#CENTERING VARIABLES
#==========================================================
getMeasurementCovariateData <- function(connection,
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
               measurement_concept_id, 
               unit_concept_id,",
               "value_as_number,",
               "measurement_date,",
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
  
  sql <- SqlRender::render(sql,
                           cohort_temp_table = cohortTable,
                           row_id_field = rowIdField,
                           startDay=covariateSettings$startDay,
                           endDay=covariateSettings$endDay,
                           concepts = paste(covariateSettings$conceptSet, collapse = ','),
                           cdm_database_schema = cdmDatabaseSchema,
                           use_min  = !is.null(covariateSettings$minVal),
                           min_val  = covariateSettings$minVal,
                           use_max = !is.null(covariateSettings$maxVal),
                           max_val  = covariateSettings$maxVal,
                           restrict_units = !is.null(covariateSettings$unitSet),
                           na_unit = NA %in% covariateSettings$unitSet,
                           units = paste(covariateSettings$unitSet[!is.na(covariateSettings$unitSet)], collapse = ',')
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
  if(inherits(covariateSettings$scaleMap, 'function')){
    covariates <- covariateSettings$scaleMap(covariates)
  } else{
    scaleMap <- eval(parse(text = covariateSettings$scaleMap))
    covariates <- scaleMap(covariates)
  }
  
  # aggregate data:
  if(covariateSettings$aggregateMethod == 'max'){
    covariates <- covariates %>% 
      dplyr::group_by(.data$rowId) %>%
      dplyr::summarize(covariateValue = max(.data$valueAsNumber))
  } else if(covariateSettings$aggregateMethod == 'min'){
    covariates <- covariates %>% 
      dplyr::group_by(.data$rowId) %>%
      dplyr::summarize(covariateValue = min(.data$valueAsNumber))
  } else if(covariateSettings$aggregateMethod == 'mean'){
    covariates <- covariates %>% 
      dplyr::group_by(.data$rowId) %>%
      dplyr::summarize(covariateValue = mean(.data$valueAsNumber))
  } else if(covariateSettings$aggregateMethod == 'median'){
    covariates <- covariates %>% 
      dplyr::group_by(.data$rowId) %>%
      dplyr::summarize(covariateValue = median(.data$valueAsNumber))
  } else{
    last <- covariates %>% 
      dplyr::group_by(.data$rowId) %>%
      dplyr::summarize(lastTime = min(.data$indexTime))
    
    covariates <- merge(covariates,last, 
                        by.x = c('rowId','indexTime'), 
                        by.y = c('rowId','lastTime') )
    
    covariates <- covariates %>% 
      dplyr::group_by(.data$rowId) %>%
      dplyr::summarize(
        covariateValue = mean(.data$valueAsNumber)
        )
  }
  
  # add covariateID:
  covariates$covariateId <- covariateSettings$covariateId

  
  covariates <- covariates %>% dplyr::select(rowId, covariateId, covariateValue)

  
  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = covariateSettings$covariateId,
                             covariateName = paste('Measurement during day',
                                                   covariateSettings$startDay,
                                                   'through',
                                                   covariateSettings$endDay,
                                                   'days relative to index:',
                                                   covariateSettings$covariateName
                                                   ),
                             analysisId = covariateSettings$analysisId,
                             conceptId = 0)
  
  analysisRef <- data.frame(analysisId = covariateSettings$analysisId,
                            analysisName = "measurement covariate",
                            domainId = "measurement covariate",
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

#============================================================
#CREATE MEASUREMENT COVARIATE SETTINGS
#============================================================

createMeasurementCovariateSettings <- function(
    covariateName, 
    conceptSet,
    unitSet = NULL,
    startDay=-30, 
    endDay=0, 
    scaleMap = function(x){sapply(x, function(y){y})}, 
    minVal = NULL,
    maxVal = NULL,
    aggregateMethod = 'recent',
    covariateId = 1466,
    analysisId = 466
) {
  
  covariateSettings <- list(covariateName=covariateName, 
                            conceptSet=conceptSet,
                            unitSet = unitSet,
                            startDay=startDay,
                            endDay=endDay,
                            scaleMap=scaleMap,
                            aggregateMethod = aggregateMethod,
                            minVal = minVal,
                            maxVal = maxVal,
                            covariateId = covariateId,
                            analysisId = analysisId
  )
  
  attr(covariateSettings, "fun") <- "getMeasurementCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)

#=============================================================
#CHOLESTEROL/HDL ratio CENTERING
#=============================================================

scaleMapchdl <- function(covariates){
  covariates$valueAsNumber <- sapply(covariates$valueAsNumber, function(y){y - 4})
  return(covariates)
  }

chdlCovSetting <- createMeasurementCovariateSettings(
    covariateName = 'Chol/HDL ratio', 
    conceptSet = c(4195214, 4042587,4195490,4198116,36314015,36314016,36314017,36314018,36361945,36361947,36361949,36361951),
    unitSet = c(NA, 0, 8523),
    startDay = -365, 
    endDay = 0, 
    scaleMap = scaleMapchdl, 
    minVal = 0,
    maxVal = 20,
    aggregateMethod = 'recent',
    covariateId = 1466, 
    analysisId = 466
)

#======================================================
#BODY MASS INDEX (BMI) CENTERING
#======================================================
  
scaleMapBmi <- function(covariates){
  covariates$valueAsNumber <- sapply(covariates$valueAsNumber, function(y){y - 26})
  return(covariates)
}

bmiCovSetting <- createMeasurementCovariateSettings(
  covariateName = 'BMI', 
  conceptSet = c(3038553),
  unitSet = NULL, 
  startDay = -365, 
  endDay = 0, 
  scaleMap = scaleMapBmi, 
  minVal = 5,
  maxVal = 250, 
  aggregateMethod = 'recent',
  covariateId = 2466, 
  analysisId = 466
)

#===============================================
#BLOOD PRESSURE CENTERING
#===============================================
  
scaleMapBldp <- function(covariates){
  covariates$valueAsNumber <- sapply(covariates$valueAsNumber, function(y){y - 132.6})
  return(covariates)
}

bldpCovSetting <- createMeasurementCovariateSettings(
  covariateName = 'Systolic Blood Pressure', 
  conceptSet = c(3004249),
  unitSet = c(NA, 0, 8876), 
  startDay = -365, 
  endDay = 0, 
  scaleMap = scaleMapBldp, 
  minVal = 5,
  maxVal = 250, 
  aggregateMethod = 'recent',
  covariateId = 3466, 
  analysisId = 466
)

#========================================================================================
#ADJUSTMENTS TO THE COHORTS USED
#========================================================================================
  
#LOGARITHMIC AGE
#=======================================================================================

#CREATE
createLogAge <- function(){
  # create list of inputs to implement function
  featureEngineeringSettings <- list()
  # specify the function that will implement the sampling
  attr(featureEngineeringSettings, "fun") <- "implementLogAge"
  # make sure the object returned is of class "sampleSettings"
  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
}

#IMPLEMENT
implementLogAge <- function(trainData, featureEngineeringSettings) {
  # get covariate 1002 values
  ageData <- trainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 1002) %>%
    dplyr::select("rowId", 'covariateValue') %>%
    dplyr::collect()

  newData <- data.frame(
    rowId = ageData$rowId,
    covariateId = 3002, # this is the covariateId for logAge
    covariateValue = log(ageData$covariateValue)
  )

  # remove existing age if in covariates
  trainData$covariateData$covariates <- trainData$covariateData$covariates |>
    dplyr::filter(!covariateId %in% c(1002))
  # update covRef
  Andromeda::appendToTable(trainData$covariateData$covariateRef,
                           data.frame(covariateId=3002,
                                      covariateName='Log age in years',
                                      analysisId=2,
                                      conceptId=3002))

  # update covariates
  Andromeda::appendToTable(trainData$covariateData$covariates, newData)

  featureEngineering <- list(
    funct = 'implementLogAge',
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings
    )
  )

  attr(trainData$covariateData, 'metaData')$featureEngineering = listAppend(
    attr(trainData$covariateData, 'metaData')$featureEngineering,
    featureEngineering
  )

  return(trainData)
}

#INTERACTION TERM SBP x ANTIHYPERTENSIVE TREATMENT                                                          
#=======================================================================================
#CREATE
createInteractionTermSBPAT <- function() {
  attr(featureEngineeringSettings, "fun") <- "implementInteractionTermSBPAT"
  class(FeatureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
  }

#IMPLEMENT
implementInteractionTermSBPATI <- function(trainData, featureEngineeringSettings) {
  # get covariate values
  InteractionTermSBPATDataI <- trainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == c(21395) %>%                            
    dplyr::select("rowId", 'covariateValue') %>%
    dplyr::collect()

   InteractionTermSBPATDataII <-   trainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 19280) %>%                            
    dplyr::select("rowId", 'covariateValue') %>%
    dplyr::collect()             
  
  newData <- data.frame(
    rowIds = c(InteractionTermSBPATDataI$rowId, InteractionTermSBPATDataII$rowId)
    covariateId = 3004, 
    covariateValue = InteractionTermSBPATDataI$covariateValue*InteractionTermSBPATDataII$covariateValue
  )

  # remove existing age if in covariates
  trainData$covariateData$covariates <- trainData$covariateData$covariates |>
    dplyr::filter(!covariateId %in% c(21395))                                  
  # update covRef
  Andromeda::appendToTable(trainData$covariateData$covariateRef,
                           data.frame(covariateId=3004,
                                      covariateName='Interaction term SBPxAntihypertensive treatment',
                                      analysisId=4,
                                      conceptId=3004))

  # update covariates
  Andromeda::appendToTable(trainData$covariateData$covariates, newData)

  featureEngineering <- list(
    funct = 'InteractionTermSBPAT',
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings
    )
  )

  attr(trainData$covariateData, 'metaData')$featureEngineering = listAppend(
    attr(trainData$covariateData, 'metaData')$featureEngineering,
    featureEngineering
  )

  return(trainData)
}

#STANDARD DEVIATION OF BLOOD PRESSURE 
#=======================================================================================
#CREATE
createSDBP <- function(){
  # create list of inputs to implement function
  featureEngineeringSettings <- list()
  # specify the function that will implement the sampling
  attr(featureEngineeringSettings, "fun") <- "implementSDBP"
  # make sure the object returned is of class "sampleSettings"
  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
}

#IMPLEMENT
implementSDBP <- function(trainData, featureEngineeringSettings) {
  # get initial covariate values
  sdData <- trainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 21395) %>%                                  
    dplyr::select("rowId", 'covariateValue') %>%
    dplyr::collect()

  newData <- data.frame(
    rowId = sdData$rowId,                                  
    covariateId = 3003,                                         
    covariateValue = sd(sdData$covariateValue)            
  )

  # remove existing SBP if in covariates
  trainData$covariateData$covariates <- trainData$covariateData$covariates |>
    dplyr::filter(!covariateId %in% c(21395))                                     

  # update covRef
  Andromeda::appendToTable(trainData$covariateData$covariateRef,
                           data.frame(covariateId=3003, 
                                      covariateName='Standard deviation of blood pressure',
                                      analysisId=3,
                                      conceptId=3003))

  # update covariates
  Andromeda::appendToTable(trainData$covariateData$covariates, newData)

  featureEngineering <- list(
    funct = 'implementSDBP',
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings
    )
  )

  attr(trainData$covariateData, 'metaData')$featureEngineering = listAppend(
    attr(trainData$covariateData, 'metaData')$featureEngineering,
    featureEngineering
  )

  return(trainData)
}

#=======================================================================================
#CREATE THE MODELS
#=======================================================================================

#=======================================================================================
#ORIGINAL MODELS
#=======================================================================================

#QRISK1_male_OG_1
#============================================================================
canalysisId <- 668
plpModelQRISK1_male_OG_1 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395)*1000+canalysisId, 
    coefficient = c(50.634, 1.001, 1.022, 1.300, 1.417, 1.017, 1.004, 1.847, 0.993) 
  ), 
  intercept = 0, 
  mapping = QRISK1_male_1, 
  populationSettings = populationSettings1,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Body mass index', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of premature cardiovascular disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Antihypertensive agent', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SBPxantihypertensive agent interaction term', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = list(createLogAge(),createInteractionTermSBPAT())                  
)

plpModelQRISK1_male$modelDesign$targetId <- 21398                  
plpModelQRISK1_male$modelDesign$outcomeId <- 21397
plpModelQRISK1_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395),
    cohortName = c('Age','Cholesterol/HDL','Body mass index','Family history of premature cardiovascular disease','Current smoker','Townsend score', 'Systolic blood pressure', 'Antihypertensive agent','SBPxantihypertensive agent interaction term') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK1_male_OG_1,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK1_male_OG_1, 
  dirPath = './inst/models/QRISK1_male_OG_1'
    )
                  
#QRISK1_female_OG_1
#============================================================================
canalysisId <- 668
plpModelQRISK1_female_OG_1 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395)*1000+canalysisId, 
    coefficient = c(87.75, 1.001, 1.015, 1.229, 1.530, 1.035, 1.005, 1.734, 0.996) 
  ), 
  intercept = 0, 
  mapping = QRISK1_female_1, 
  populationSettings = populationSettings1,
    covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Body mass index', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of premature cardiovascular disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Antihypertensive agent', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SBPxantihypertensive agent interaction term', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = list(createLogAge(), createInteractionTermSBPAT())                            
)

plpModelQRISK1_female$modelDesign$targetId <- 21399
plpModelQRISK1_female$modelDesign$outcomeId <- 21397
plpModelQRISK1_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395),
    cohortName = c('Age','Cholesterol/HDL','Body mass index','Family history of premature cardiovascular disease','Current smoker','Townsend score', 'Systolic blood pressure', 'Antihypertensive agent','SBPxantihypertensive agent interaction term')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
   FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK1_female_OG_1,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK1_female_OG_1, 
  dirPath = './inst/models/QRISK1_female_OG_1'
    )

#QRISK2_male_OG_1
#============================================================================
canalysisId <- 668
plpModelQRISK2_male_OG_1 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 18838, 18841, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 18841)*1000+canalysisId, 
    coefficient = c(1.00, 1.45, 1.97, 1.67, 1.37, 0.62, 0.63, 0.51, 0.91, 1.59, 0.218, 0.236, 0.0595, 1.19, 2.14, 1.65, 1.68, 2.20, 1.38, 2.40, 1.75, 0.985, 0.1946, 0.0482, 0.923, 0.932, 0.916, 0.902, 0.893) 
  ), 
  intercept = 0, 
  mapping = QRISK2_male_1,  
  populationSettings = populationSettings1,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'White or not recorded', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22466,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'BMI', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexBMI interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexTownsend interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsystolicbloodpressure interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexfamilyhistory interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsmoking interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agextreatedhypertension interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgextypeIIdiabetes interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexatrial fibrillation interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createLogAge()
)

plpModelQRISK2_male$modelDesign$targetId <- 21400
plpModelQRISK2_male$modelDesign$outcomeId <- 21397
plpModelQRISK2_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 18838, 18841, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 18841)
    cohortName = c('White or not recorded','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Age','BMI','Townsend score','Systolic blood pressure','Cholesterol/HDL','Family history coronary heart disease','Current smoker','Treated hypertension','Type 2 diabetes','Rheumatoid arthritis','Atrial fibrillation','Renal disease','AgexBMI interaction','AgexTownsend interaction','Agexsystolicbloodpressure interaction','Agexfamilyhistory interaction','Agexsmoking interaction','Agextreatedhypertension interaction','Agextype2diabetes interaction', 'Agexatrialfibrillation interaction') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK2_male_OG_1,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK2_male_OG_1, 
  dirPath = './inst/models/QRISK2_male_OG_1'
    )
  
#QRISK2_female_OG_1
#============================================================================
canalysisId <- 668
plpModelQRISK2_female_OG_1 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 18838, 18841, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 18841)*1000+canalysisId, 
    coefficient = c(1.00, 1.43, 1.80, 1.35, 1.15, 1.08, 0.58, 0.69, 1.04, 1.66, 0.216, 0.274, 0.06, 1.17, 1.99, 1.80, 1.54, 2.54, 1.50, 3.06, 1.70, 0.976, 0.1876, 0.0483, 0.927, 0.931, 0.952, 0.904, 0.858)
  ), 
  intercept = 0, 
  mapping = QRISK2_female_1,  
  populationSettings = populationSettings1,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'White or not recorded', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22466,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'BMI', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexBMI interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexTownsend interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsystolicbloodpressure interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexfamilyhistory interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsmoking interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agextreatedhypertension interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgextypeIIdiabetes interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexatrial fibrillation interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createLogAge()
)

plpModelQRISK2_female$modelDesign$targetId <- 21401
plpModelQRISK2_female$modelDesign$outcomeId <- 21397
plpModelQRISK2_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 18838, 18841, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 18841),
    cohortName = c('White or not recorded', 'Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Age','BMI','Townsend score','Systolic blood pressure','Cholesterol/HDL','Family history coronary heart disease','Current smoker','Treated hypertension','Type 2 diabetes','Rheumatoid arthritis','Atrial fibrillation','Renal disease','AgexBMI interaction','AgexTownsend interaction','Agexsystolicbloodpressure interaction','Agexfamilyhistory interaction','Agexsmoking interaction','Agextreatedhypertension interaction','Agextype2diabetes interaction', 'Agexatrialfibrillation interaction')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK2_female_OG_1,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK2_female_OG_1, 
  dirPath = './inst/models/QRISK2_female_OG_1'
    )

#QRISK3_male_OG_1
#============================================================================

# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK3_male_OG_1 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19165, 18779, 18822, 21395)*1000+canalysisId, 
    coefficient = c(0.236, 1.32, 1.61, 1.70, 1.04, 0.699, 0.670, 0.660, 0.769, 1.21, 1.74, 1.89, 2.20, 1.72, 3.44, 2.36, 1.68, 1.23, 2.42, 2.05, 1.29, 1.58, 1.55, 1.14, 1.13, 1.25, 1.19, 0.057, 0.111) 
  ), 
  intercept = 0, 
  mapping = QRISK3_male_1, 
  populationSettings = populationSettings1,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Former smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18820, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chronic kidney disease (stage 3,4 or 5)', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroid use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus erythematosus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19164, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372,
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    , createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Erectile dysfunction or treatment', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19165,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD of blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
  ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP()                         
)

plpModelQRISK3_male$modelDesign$targetId <- 21402
plpModelQRISK3_male$modelDesign$outcomeId <- 21397
plpModelQRISK3_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19165, 18779, 18822, 21395),
    cohortName = c('Townsend score','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Former smoker', 'Light smoker','Moderate smoker','Heavy smoker','Family history of coronary heart disease','Type I Diabetes','Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Chronic kidney disease (stage 3,4 or 5)','Migraine', 'Corticosteroid use', 'Systemic lupus erythematosus', 'Atypical antipsychotic use', 'Severe mental illness', 'Erectile dysfunction or treatment', 'Cholesterol/HDL', 'Systolic blood pressure', 'SD of blood pressure') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK3_male_OG_1,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK3_male_OG_1, 
  dirPath = './inst/models/QRISK3_male_OG_1'
    )
                  
#QRISK3_female_OG_1
#============================================================================

# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK3_female_OG_1 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291,18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 18779, 18822, 21395)*1000+canalysisId, 
    coefficient = c(0.294, 1.32, 1.76, 1.34, 1.08, 0.843, 0.675, 0.722, 0.843, 1.14, 1.75, 1.95, 2.34, 1.58, 5.62, 2.91, 1.66, 1.24, 4.92, 1.92, 1.35, 1.81, 2.14, 1.29, 1.13, 1.17, 0.057, 0.108)
  ), 
  intercept = 0
  mapping = QRISK3_female_1, 
  populationSettings = populationSettings1,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Former smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18820,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chronic kidney disease (stage 3,4 or 5)', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19379,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroid use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus erythematosus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19164, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Standard deviation of blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK3_female$modelDesign$targetId <- 21403
plpModelQRISK3_female$modelDesign$outcomeId <- 21397
plpModelQRISK3_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291,18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 18779, 18822, 21395),
    cohortName = c('Townsend score','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Former smoker','Light smoker','Moderate smoker','Heavy smoker','Family history of coronary heart disease','Type I Diabetes','Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Chronic kidney disease (stage 3,4 or 5)','Migraine', 'Corticosteroid use', 'Systemic lupus erythematosus', 'Atypical antipsychotic use', 'Severe mental illness', 'Cholesterol/HDL', 'Systolic blood pressure', 'SD of blood pressure') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK3_female_OG_1,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK3_female_OG_1, 
  dirPath = './inst/models/QRISK3_female_OG_1'
    )



#QRISK4_male_OG_1
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK4_male_OG_1 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(18822,21395,18779,21387,21288, 21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 22465, 21344, 19881, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19380, 19792, 19788, 19787, 19174, 19165)*1000+canalysisId, 
    coefficient = c(0.065, 1.14, 1.15, 0.222, 1.19, 2.00, 2.08, 2.60, 1.19, 1.47, 1.41, 1.04, 0.67, 0.66, 0.72, 0.81, 1.00, 1.17, 2.35, 1.62, 3.28, 2.03, 2.20, 1.19, 2.59, 1.70, 1.41, 1.64, 1.68, 1.18, 1.37, 1.66, 1.49, 2.06, 5.45, 1.40)
  ), 
  intercept = 0, 
  mapping = QRISK4_male_1, 
  populationSettings = populationSettings1,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'SBP', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD SBP',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Deprivation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Ex-smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other ethnic group', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'No learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22465, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21344, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Down syndrome', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19881, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'FH of CHD', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type 1 Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18820, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type 2 Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal failure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroids', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19164, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'COPD',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19380,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Lung cancer',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19792,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Oral cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19788,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Blood cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19787, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Brain cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19174, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Erectile dysfunction',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19165,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK4_male$modelDesign$targetId <- 21404
plpModelQRISK4_male$modelDesign$outcomeId <- 21397
plpModelQRISK4_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(18822,21395,18779,21387,21288, 21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 22465, 21344, 19881, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19380, 19792, 19788, 19787, 19174, 19165),
    cohortName = c('SBP', 'SD SBP', 'Cholesterol/HDL', 'Deprivation', 'Ex-smoker', 'Light smoker', 'Moderate smoker', 'Heavy smoker', 'Indian', 'Pakistani', 'Bangladeshi', 'Other Asian', 'Caribbean', 'Black African', 'Chinese', 'Other ethnic group', 'No learning disability','Learning disability', 'Down syndrome', 'FH of CHD', 'Type I Diabetes', 'Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Renal failure', 'Migraine', 'Corticosteroids', 'Systemic lupus', 'Atypical antipsychotics','Severe mental illness', 'COPD', 'Lung cancer', 'Oral cancer', 'Blood cancer', 'Brain cancer', 'Erectile dysfunction') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK4_male_OG_1,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK4_male_OG_1, 
  dirPath = './inst/models/QRISK4_male_OG_1'
    )

  
#QRISK4_female_OG_1
#============================================================================

# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK4_female_OG_1 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(18822,21395,18779,21387,21288,21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 22465, 21344, 19881, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19380, 19792, 19788, 19787, 19174, 19793, 14512)*1000+canalysisId, 
    coefficient = c(0.0635, 1.16, 1.14, 0.252, 1.19, 2.12, 2.30, 2.90, 1.13, 1.56, 1.28, 0.98, 0.94, 0.76, 0.68, 0.94, 1.00, 1.45, 3.18, 1.46, 4.52, 2.49, 2.20, 1.28, 4.50, 1.81, 1.46, 1.70, 2.22, 1.24, 1.22, 1.85, 3.50, 1.55, 2.13, 4.52, 1.18, 1.56) 
  ), 
  intercept = 0, 
  mapping = QRISK4_female_1,  
  populationSettings = populationSettings1,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'SBP', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD SBP',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL ratio', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Deprivation',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Ex-smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Caribbean',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other ethnic group',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'No learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22465, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21344,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Down syndrome',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19881,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'FH of CHD', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18820,          
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal failure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroids', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19164, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'COPD',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Lung cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19792,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Oral cancer',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19788,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Blood cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19787, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Brain cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19174,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Postnatal depression',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19793, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pre-eclampsia', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 14512,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK4_female$modelDesign$targetId <- 21405
plpModelQRISK4_female$modelDesign$outcomeId <- 21397
plpModelQRISK4_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(18822,21395,18779,21387,21288,21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 22465, 21344, 19881, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19380, 19792, 19788, 19787, 19174, 19793, 14512), 
    cohortName = c('SBP', 'SD SBP', 'Cholesterol/HDL', 'Deprivation', 'Ex-smoker', 'Light smoker', 'Moderate smoker', 'Heavy smoker', 'Indian', 'Pakistani', 'Bangladeshi', 'Other Asian', 'Caribbean', 'Black African', 'Chinese', 'Other ethnic group', 'No learning disability', 'Learning disability', 'Down syndrome', 'FH of CHD', 'Type I Diabetes', 'Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Renal failure', 'Migraine', 'Corticosteroids', 'Systemic lupus', 'Atypical antipsychotic', 'Severe mental illness', 'COPD', 'Lung cancer', 'Oral cancer', 'Blood cancer', 'Brain cancer', 'Postnatal depression', 'Pre-eclampsia')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK4_female_OG_1,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK4_female_OG_1, 
  dirPath = './inst/models/QRISK4_female_OG_1'
    )
              
#QRISK1_male_OG_3
#=======================================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK1_male_OG_3 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395)*1000+canalysisId, 
    coefficient = c(50.634, 1.001, 1.022, 1.300, 1.417, 1.017, 1.004, 1.847, 0.993) 
  ), 
  intercept = 0, 
  mapping = QRISK1_male_3,  
  populationSettings = populationSettings3,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Body mass index', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of premature cardiovascular disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Antihypertensive agent', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SBPxantihypertensive agent interaction term', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = list(createLogAge(),createInteractionTermSBPAT())                  
)

plpModelQRISK1_male$modelDesign$targetId <- 21398                  
plpModelQRISK1_male$modelDesign$outcomeId <- 21397
plpModelQRISK1_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395),
    cohortName = c('Age','Cholesterol/HDL','Body mass index','Family history of premature cardiovascular disease','Current smoker','Townsend score', 'Systolic blood pressure', 'Antihypertensive agent','SBPxantihypertensive agent interaction term') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK1_male_OG_3,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK1_male_OG_3, 
  dirPath = './inst/models/QRISK1_male_OG_3'
    )

#QRISK1_female_OG_3
#=======================================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK1_female_OG_3 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395)*1000+canalysisId, 
    coefficient = c(87.75, 1.001, 1.015, 1.229, 1.530, 1.035, 1.005, 1.734, 0.996) 
  ), 
  intercept = 0, 
  mapping = QRISK1_female_3, 
  populationSettings = populationSettings3,
    covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Body mass index', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of premature cardiovascular disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Antihypertensive agent', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SBPxantihypertensive agent interaction term', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
   ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = list(createLogAge(), createInteractionTermSBPAT())                            
)

plpModelQRISK1_female$modelDesign$targetId <- 21399
plpModelQRISK1_female$modelDesign$outcomeId <- 21397
plpModelQRISK1_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395),
    cohortName = c('Age','Cholesterol/HDL','Body mass index','Family history of premature cardiovascular disease','Current smoker','Townsend score', 'Systolic blood pressure', 'Antihypertensive agent','SBPxantihypertensive agent interaction term')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK1_female_OG_3,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK1_female_OG_3, 
  dirPath = './inst/models/QRISK1_female_OG_3'
    )

#QRISK2_male_OG_3
#=============================================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK2_male_OG_3 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(22466,21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 18838, 18841, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 18841)*1000+canalysisId, 
    coefficient = c(1.00, 1.45, 1.97, 1.67, 1.37, 0.62, 0.63, 0.51, 0.91, 1.59, 0.218, 0.236, 0.0595, 1.19, 2.14, 1.65, 1.68, 2.20, 1.38, 2.40, 1.75, 0.985, 0.1946, 0.0482, 0.923, 0.932, 0.916, 0.902, 0.893) 
  ), 
  intercept = 0, 
  mapping = QRISK2_male_3,  
  populationSettings = populationSettings3,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'White or not recorded', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22466,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'BMI', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexBMI interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexTownsend interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsystolicbloodpressure interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexfamilyhistory interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsmoking interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agextreatedhypertension interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgextypeIIdiabetes interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexatrial fibrillation interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createLogAge()
)

plpModelQRISK2_male$modelDesign$targetId <- 21400
plpModelQRISK2_male$modelDesign$outcomeId <- 21397
plpModelQRISK2_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 18838, 18841, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 18841)
    cohortName = c('White or not recorded', 'Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Age','BMI','Townsend score','Systolic blood pressure','Cholesterol/HDL','Family history coronary heart disease','Current smoker','Treated hypertension','Type 2 diabetes','Rheumatoid arthritis','Atrial fibrillation','Renal disease','AgexBMI interaction','AgexTownsend interaction','Agexsystolicbloodpressure interaction','Agexfamilyhistory interaction','Agexsmoking interaction','Agextreatedhypertension interaction','Agextype2diabetes interaction', 'Agexatrialfibrillation interaction') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK2_male_OG_3,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK2_male_OG_3, 
  dirPath = './inst/models/QRISK2_male_OG_3'
    )

#QRISK2_female_OG_3
#=============================================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK2_female_OG_3 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 18838, 18841, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 18841)*1000+canalysisId, 
    coefficient = c(1.00, 1.43, 1.80, 1.35, 1.15, 1.08, 0.58, 0.69, 1.04, 1.66, 0.216, 0.274, 0.06, 1.17, 1.99, 1.80, 1.54, 2.54, 1.50, 3.06, 1.70, 0.976, 0.1876, 0.0483, 0.927, 0.931, 0.952, 0.904, 0.858)
  ), 
  intercept = 0, 
  mapping = QRISK2_female_3, 
  populationSettings = populationSettings3,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'White or not recorded', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22466,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'BMI', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexBMI interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexTownsend interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsystolicbloodpressure interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexfamilyhistory interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsmoking interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agextreatedhypertension interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgextypeIIdiabetes interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexatrial fibrillation interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createLogAge()
)

plpModelQRISK2_female$modelDesign$targetId <- 21401
plpModelQRISK2_female$modelDesign$outcomeId <- 21397
plpModelQRISK2_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 18838, 18841, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 18841),
    cohortName = c('White or not recorded','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Age','BMI','Townsend score','Systolic blood pressure','Cholesterol/HDL','Family history coronary heart disease','Current smoker','Treated hypertension','Type 2 diabetes','Rheumatoid arthritis','Atrial fibrillation','Renal disease','AgexBMI interaction','AgexTownsend interaction','Agexsystolicbloodpressure interaction','Agexfamilyhistory interaction','Agexsmoking interaction','Agextreatedhypertension interaction','Agextype2diabetes interaction', 'Agexatrialfibrillation interaction')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK2_female_OG_3,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK2_female_OG_3, 
  dirPath = './inst/models/QRISK2_female_OG_3'
    )

#QRISK3_male_OG_3
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK3_male_OG_3 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19165, 18779, 18822, 21395)*1000+canalysisId, 
    coefficient = c(0.236, 1.32, 1.61, 1.70, 1.04, 0.699, 0.670, 0.660, 0.769, 1.21, 1.74, 1.89, 2.20, 1.72, 3.44, 2.36, 1.68, 1.23, 2.42, 2.05, 1.29, 1.58, 1.55, 1.14, 1.13, 1.25, 1.19, 0.057, 0.111) 
  ), 
  intercept = 0, 
  mapping = QRISK3_male_3, 
  populationSettings = populationSettings3,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Former smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18820, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chronic kidney disease (stage 3,4 or 5)', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroid use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus erythematosus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19164, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372,
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    , createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Erectile dysfunction or treatment', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19165,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD of blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP()                         
)

plpModelQRISK3_male$modelDesign$targetId <- 21402
plpModelQRISK3_male$modelDesign$outcomeId <- 21397
plpModelQRISK3_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19165, 18779, 18822, 21395),
    cohortName = c('Townsend score','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Former smoker', 'Light smoker','Moderate smoker','Heavy smoker','Family history of coronary heart disease','Type I Diabetes','Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Chronic kidney disease (stage 3,4 or 5)','Migraine', 'Corticosteroid use', 'Systemic lupus erythematosus', 'Atypical antipsychotic use', 'Severe mental illness', 'Erectile dysfunction or treatment', 'Cholesterol/HDL', 'Systolic blood pressure', 'SD of blood pressure') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK3_male_OG_3,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK3_male_OG_3, 
  dirPath = './inst/models/QRISK3_male_OG_3'
    )

#QRISK3_female_OG_3
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK3_female_OG_3 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291,18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 18779, 18822, 21395)*1000+canalysisId, 
    coefficient = c(0.294, 1.32, 1.76, 1.34, 1.08, 0.843, 0.675, 0.722, 0.843, 1.14, 1.75, 1.95, 2.34, 1.58, 5.62, 2.91, 1.66, 1.24, 4.92, 1.92, 1.35, 1.81, 2.14, 1.29, 1.13, 1.17, 0.057, 0.108)
  ), 
  intercept = 0
  mapping = QRISK3_female_3,
  populationSettings = populationSettings3,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Former smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18820,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chronic kidney disease (stage 3,4 or 5)', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19379,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroid use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus erythematosus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19164, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Standard deviation of blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK3_female$modelDesign$targetId <- 21403
plpModelQRISK3_female$modelDesign$outcomeId <- 21397
plpModelQRISK3_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291,18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 18779, 18822, 21395),
    cohortName = c('Townsend score','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Former smoker','Light smoker','Moderate smoker','Heavy smoker','Family history of coronary heart disease','Type I Diabetes','Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Chronic kidney disease (stage 3,4 or 5)','Migraine', 'Corticosteroid use', 'Systemic lupus erythematosus', 'Atypical antipsychotic use', 'Severe mental illness', 'Cholesterol/HDL', 'Systolic blood pressure', 'SD of blood pressure') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK3_female_OG_3,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK3_female_OG_3, 
  dirPath = './inst/models/QRISK3_female_OG_3'
    )


#QRISK4_male_OG_3
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK4_male_OG_3 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(18822,21395,18779,21387,21288, 21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 22465, 21344, 19881, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19380, 19792, 19788, 19787, 19174, 19165)*1000+canalysisId, 
    coefficient = c(0.065, 1.14, 1.15, 0.222, 1.19, 2.00, 2.08, 2.60, 1.19, 1.47, 1.41, 1.04, 0.67, 0.66, 0.72, 0.81, 1.00, 1.17, 2.35, 1.62, 3.28, 2.03, 2.20, 1.19, 2.59, 1.70, 1.41, 1.64, 1.68, 1.18, 1.37, 1.66, 1.49, 2.06, 5.45, 1.40)
  ), 
  intercept = 0, 
  mapping = QRISK4_male_3, 
  populationSettings = populationSettings3,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'SBP', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD SBP',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Deprivation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Ex-smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other ethnic group', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'No learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22465, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21344, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Down syndrome', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19881, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'FH of CHD', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type 1 Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18820, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type 2 Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal failure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroids', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19164, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'COPD',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19380,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Lung cancer',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19792,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Oral cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19788,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Blood cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19787, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Brain cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19174, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Erectile dysfunction',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19165,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK4_male$modelDesign$targetId <- 21404
plpModelQRISK4_male$modelDesign$outcomeId <- 21397
plpModelQRISK4_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(18822,21395,18779,21387,21288, 21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 22465, 21344, 19881, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19380, 19792, 19788, 19787, 19174, 19165),
    cohortName = c('SBP', 'SD SBP', 'Cholesterol/HDL', 'Deprivation', 'Ex-smoker', 'Light smoker', 'Moderate smoker', 'Heavy smoker', 'Indian', 'Pakistani', 'Bangladeshi', 'Other Asian', 'Caribbean', 'Black African', 'Chinese', 'Other ethnic group', 'No learning disability', 'Learning disability', 'Down syndrome', 'FH of CHD', 'Type I Diabetes', 'Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Renal failure', 'Migraine', 'Corticosteroids', 'Systemic lupus', 'Atypical antipsychotics','Severe mental illness', 'COPD', 'Lung cancer', 'Oral cancer', 'Blood cancer', 'Brain cancer', 'Erectile dysfunction') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK4_male_OG_3,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK4_male_OG_3, 
  dirPath = './inst/models/QRISK4_male_OG_3'
    )

#QRISK4_female_OG_3
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK4_female_OG_3 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(18822,21395,18779,21387,21288,21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 22465, 21344, 19881, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19380, 19792, 19788, 19787, 19174, 19793, 14512)*1000+canalysisId, 
    coefficient = c(0.0635, 1.16, 1.14, 0.252, 1.19, 2.12, 2.30, 2.90, 1.13, 1.56, 1.28, 0.98, 0.94, 0.76, 0.68, 0.94, 1.00, 1.45, 3.18, 1.46, 4.52, 2.49, 2.20, 1.28, 4.50, 1.81, 1.46, 1.70, 2.22, 1.24, 1.22, 1.85, 3.50, 1.55, 2.13, 4.52, 1.18, 1.56) 
  ), 
  intercept = 0, 
  mapping = QRISK4_female_3, 
  populationSettings = populationSettings3,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'SBP', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD SBP',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL ratio', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Deprivation',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Ex-smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Caribbean',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other ethnic group',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'No learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22465, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21344,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Down syndrome',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19881,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'FH of CHD', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18820,          
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal failure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroids', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19164, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'COPD',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Lung cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19792,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Oral cancer',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19788,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Blood cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19787, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Brain cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19174,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Postnatal depression',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19793, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pre-eclampsia', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 14512,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK4_female$modelDesign$targetId <- 21405
plpModelQRISK4_female$modelDesign$outcomeId <- 21397
plpModelQRISK4_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(18822,21395,18779,21387,21288,21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 22465, 21344, 19881, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19380, 19792, 19788, 19787, 19174, 19793, 14512), 
    cohortName = c('SBP', 'SD SBP', 'Cholesterol/HDL', 'Deprivation', 'Ex-smoker', 'Light smoker', 'Moderate smoker', 'Heavy smoker', 'Indian', 'Pakistani', 'Bangladeshi', 'Other Asian', 'Caribbean', 'Black African', 'Chinese', 'Other ethnic group','No learning disability', 'Learning disability', 'Down syndrome', 'FH of CHD', 'Type I Diabetes', 'Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Renal failure', 'Migraine', 'Corticosteroids', 'Systemic lupus', 'Atypical antipsychotic', 'Severe mental illness', 'COPD', 'Lung cancer', 'Oral cancer', 'Blood cancer', 'Brain cancer', 'Postnatal depression', 'Pre-eclampsia')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK4_female_OG_3,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK4_female_OG_3, 
  dirPath = './inst/models/QRISK4_female_OG_3'
    )


#QRISK1_male_OG_5
#============================================================================
canalysisId <- 668
plpModelQRISK1_male_OG_5 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395)*1000+canalysisId, 
    coefficient = c(50.634, 1.001, 1.022, 1.300, 1.417, 1.017, 1.004, 1.847, 0.993) 
  ), 
  intercept = 0, 
  mapping = QRISK1_male_5, 
  populationSettings = populationSettings5,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Body mass index', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of premature cardiovascular disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Antihypertensive agent', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SBPxantihypertensive agent interaction term', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = list(createLogAge(),createInteractionTermSBPAT())                  
)

plpModelQRISK1_male$modelDesign$targetId <- 21398                  
plpModelQRISK1_male$modelDesign$outcomeId <- 21397
plpModelQRISK1_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395),
    cohortName = c('Age','Cholesterol/HDL','Body mass index','Family history of premature cardiovascular disease','Current smoker','Townsend score', 'Systolic blood pressure', 'Antihypertensive agent','SBPxantihypertensive agent interaction term') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK1_male_OG_5,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK1_male_OG_5, 
  dirPath = './inst/models/QRISK1_male_OG_5'
    )

#QRISK1_female_OG_5
#============================================================================
canalysisId <- 668
plpModelQRISK1_female_OG_5 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395)*1000+canalysisId, 
    coefficient = c(87.75, 1.001, 1.015, 1.229, 1.530, 1.035, 1.005, 1.734, 0.996) 
  ), 
  intercept = 0, 
  mapping = QRISK1_female_5,  
  populationSettings = populationSettings5,
    covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Body mass index', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of premature cardiovascular disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Antihypertensive agent', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SBPxantihypertensive agent interaction term', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = list(createLogAge(), createInteractionTermSBPAT())                            
)

plpModelQRISK1_female$modelDesign$targetId <- 21399
plpModelQRISK1_female$modelDesign$outcomeId <- 21397
plpModelQRISK1_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395),
    cohortName = c('Age','Cholesterol/HDL','Body mass index','Family history of premature cardiovascular disease','Current smoker','Townsend score', 'Systolic blood pressure', 'Antihypertensive agent','SBPxantihypertensive agent interaction term')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK1_female_OG_5,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK1_female_OG_5, 
  dirPath = './inst/models/QRISK1_female_OG_5'
    )

#QRISK2_male_OG_5
#============================================================================
canalysisId <- 668
plpModelQRISK2_male_OG_5 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 18838, 18841, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 18841)*1000+canalysisId, 
    coefficient = c(1.00, 1.45, 1.97, 1.67, 1.37, 0.62, 0.63, 0.51, 0.91, 1.59, 0.218, 0.236, 0.0595, 1.19, 2.14, 1.65, 1.68, 2.20, 1.38, 2.40, 1.75, 0.985, 0.1946, 0.0482, 0.923, 0.932, 0.916, 0.902, 0.893) 
  ), 
  intercept = 0, 
  mapping = QRISK2_male_5, 
  populationSettings = populationSettings5,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'White or not recorded', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22466,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'BMI', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexBMI interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexTownsend interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsystolicbloodpressure interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexfamilyhistory interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsmoking interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agextreatedhypertension interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgextypeIIdiabetes interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexatrial fibrillation interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createLogAge()
)

plpModelQRISK2_male$modelDesign$targetId <- 21400
plpModelQRISK2_male$modelDesign$outcomeId <- 21397
plpModelQRISK2_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 18838, 18841, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 18841)
    cohortName = c('White or not recorded','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Age','BMI','Townsend score','Systolic blood pressure','Cholesterol/HDL','Family history coronary heart disease','Current smoker','Treated hypertension','Type 2 diabetes','Rheumatoid arthritis','Atrial fibrillation','Renal disease','AgexBMI interaction','AgexTownsend interaction','Agexsystolicbloodpressure interaction','Agexfamilyhistory interaction','Agexsmoking interaction','Agextreatedhypertension interaction','Agextype2diabetes interaction', 'Agexatrialfibrillation interaction') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK2_male_OG_5,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK2_male_OG_5, 
  dirPath = './inst/models/QRISK2_male_OG_5'
    )
  
#QRISK2_female_OG_5
#============================================================================
canalysisId <- 668
plpModelQRISK2_female_OG_5 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 18838, 18841, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 18841)*1000+canalysisId, 
    coefficient = c(1.00, 1.43, 1.80, 1.35, 1.15, 1.08, 0.58, 0.69, 1.04, 1.66, 0.216, 0.274, 0.06, 1.17, 1.99, 1.80, 1.54, 2.54, 1.50, 3.06, 1.70, 0.976, 0.1876, 0.0483, 0.927, 0.931, 0.952, 0.904, 0.858)
  ), 
  intercept = 0, 
  mapping = QRISK2_female_5, 
  populationSettings = populationSettings5,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'White or not recorded', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22466,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'BMI', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexBMI interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexTownsend interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsystolicbloodpressure interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexfamilyhistory interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsmoking interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agextreatedhypertension interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgextypeIIdiabetes interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexatrial fibrillation interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createLogAge()
)

plpModelQRISK2_female$modelDesign$targetId <- 21401
plpModelQRISK2_female$modelDesign$outcomeId <- 21397
plpModelQRISK2_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 18838, 18841, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 18841),
    cohortName = c('White or not recorded', 'Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Age','BMI','Townsend score','Systolic blood pressure','Cholesterol/HDL','Family history coronary heart disease','Current smoker','Treated hypertension','Type 2 diabetes','Rheumatoid arthritis','Atrial fibrillation','Renal disease','AgexBMI interaction','AgexTownsend interaction','Agexsystolicbloodpressure interaction','Agexfamilyhistory interaction','Agexsmoking interaction','Agextreatedhypertension interaction','Agextype2diabetes interaction', 'Agexatrialfibrillation interaction')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK2_female_OG_5,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK2_female_OG_5, 
  dirPath = './inst/models/QRISK2_female_OG_5'
    )

#QRISK3_male_OG_5
#============================================================================

# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK3_male_OG_5 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19165, 18779, 18822, 21395)*1000+canalysisId, 
    coefficient = c(0.236, 1.32, 1.61, 1.70, 1.04, 0.699, 0.670, 0.660, 0.769, 1.21, 1.74, 1.89, 2.20, 1.72, 3.44, 2.36, 1.68, 1.23, 2.42, 2.05, 1.29, 1.58, 1.55, 1.14, 1.13, 1.25, 1.19, 0.057, 0.111) 
  ), 
  intercept = 0, 
  mapping = QRISK3_male_5, 
  populationSettings = populationSettings5,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Former smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18820, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chronic kidney disease (stage 3,4 or 5)', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroid use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus erythematosus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19164, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372,
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    , createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Erectile dysfunction or treatment', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19165,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD of blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
   ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP()                         
)

plpModelQRISK3_male$modelDesign$targetId <- 21402
plpModelQRISK3_male$modelDesign$outcomeId <- 21397
plpModelQRISK3_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19165, 18779, 18822, 21395),
    cohortName = c('Townsend score','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Former smoker', 'Light smoker','Moderate smoker','Heavy smoker','Family history of coronary heart disease','Type I Diabetes','Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Chronic kidney disease (stage 3,4 or 5)','Migraine', 'Corticosteroid use', 'Systemic lupus erythematosus', 'Atypical antipsychotic use', 'Severe mental illness', 'Erectile dysfunction or treatment', 'Cholesterol/HDL', 'Systolic blood pressure', 'SD of blood pressure') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK3_male_OG_5,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK3_male_OG_5, 
  dirPath = './inst/models/QRISK3_male_OG_5'
    )
                  
#QRISK3_female_OG_5
#============================================================================

# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK3_female_OG_5 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291,18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 18779, 18822, 21395)*1000+canalysisId, 
    coefficient = c(0.294, 1.32, 1.76, 1.34, 1.08, 0.843, 0.675, 0.722, 0.843, 1.14, 1.75, 1.95, 2.34, 1.58, 5.62, 2.91, 1.66, 1.24, 4.92, 1.92, 1.35, 1.81, 2.14, 1.29, 1.13, 1.17, 0.057, 0.108)
  ), 
  intercept = 0
  mapping = QRISK3_female_5, 
  populationSettings = populationSettings5,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Former smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18820,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chronic kidney disease (stage 3,4 or 5)', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19379,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroid use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus erythematosus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19164, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Standard deviation of blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK3_female$modelDesign$targetId <- 21403
plpModelQRISK3_female$modelDesign$outcomeId <- 21397
plpModelQRISK3_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291,18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 18779, 18822, 21395),
    cohortName = c('Townsend score','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Former smoker','Light smoker','Moderate smoker','Heavy smoker','Family history of coronary heart disease','Type I Diabetes','Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Chronic kidney disease (stage 3,4 or 5)','Migraine', 'Corticosteroid use', 'Systemic lupus erythematosus', 'Atypical antipsychotic use', 'Severe mental illness', 'Cholesterol/HDL', 'Systolic blood pressure', 'SD of blood pressure') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK3_female_OG_5,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK3_female_OG_5, 
  dirPath = './inst/models/QRISK3_female_OG_5'
    )



#QRISK4_male_OG_5
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK4_male_OG_5 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(18822,21395,18779,21387,21288, 21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 22465, 21344, 19881, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19380, 19792, 19788, 19787, 19174, 19165)*1000+canalysisId, #TODO
    coefficient = c(0.065, 1.14, 1.15, 0.222, 1.19, 2.00, 2.08, 2.60, 1.19, 1.47, 1.41, 1.04, 0.67, 0.66, 0.72, 0.81, 1.00, 1.17, 2.35, 1.62, 3.28, 2.03, 2.20, 1.19, 2.59, 1.70, 1.41, 1.64, 1.68, 1.18, 1.37, 1.66, 1.49, 2.06, 5.45, 1.40)
  ), 
  intercept = 0, 
  mapping = QRISK4_male_5,  
  populationSettings = populationSettings5,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'SBP', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD SBP',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Deprivation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Ex-smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other ethnic group', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'No learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22465, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21344, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Down syndrome', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19881, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'FH of CHD', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type 1 Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18820, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type 2 Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal failure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroids', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19164, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'COPD',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19380,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Lung cancer',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19792,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Oral cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19788,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Blood cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19787, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Brain cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19174, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Erectile dysfunction',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19165,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK4_male$modelDesign$targetId <- 21404
plpModelQRISK4_male$modelDesign$outcomeId <- 21397
plpModelQRISK4_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(18822,21395,18779,21387,21288, 21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 22465, 21344, 19881, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19380, 19792, 19788, 19787, 19174, 19165),
    cohortName = c('SBP', 'SD SBP', 'Cholesterol/HDL', 'Deprivation', 'Ex-smoker', 'Light smoker', 'Moderate smoker', 'Heavy smoker', 'Indian', 'Pakistani', 'Bangladeshi', 'Other Asian', 'Caribbean', 'Black African', 'Chinese', 'Other ethnic group', 'No learning disability', 'Learning disability', 'Down syndrome', 'FH of CHD', 'Type I Diabetes', 'Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Renal failure', 'Migraine', 'Corticosteroids', 'Systemic lupus', 'Atypical antipsychotics','Severe mental illness', 'COPD', 'Lung cancer', 'Oral cancer', 'Blood cancer', 'Brain cancer', 'Erectile dysfunction') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK4_male_OG_5,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK4_male_OG_5, 
  dirPath = './inst/models/QRISK4_male_OG_5'
    )

  
#QRISK4_female_OG_5
#============================================================================

# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK4_female_OG_5 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(18822,21395,18779,21387,21288,21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 22465, 21344, 19881, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19380, 19792, 19788, 19787, 19174, 19793, 14512)*1000+canalysisId, 
    coefficient = c(0.0635, 1.16, 1.14, 0.252, 1.19, 2.12, 2.30, 2.90, 1.13, 1.56, 1.28, 0.98, 0.94, 0.76, 0.68, 0.94, 1.00, 1.45, 3.18, 1.46, 4.52, 2.49, 2.20, 1.28, 4.50, 1.81, 1.46, 1.70, 2.22, 1.24, 1.22, 1.85, 3.50, 1.55, 2.13, 4.52, 1.18, 1.56) 
  ), 
  intercept = 0, 
  mapping = QRISK4_female_5, 
  populationSettings = populationSettings5,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'SBP', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD SBP',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL ratio', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Deprivation',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Ex-smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Caribbean',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other ethnic group',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'No learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22465, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21344,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Down syndrome',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19881,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'FH of CHD', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18820,          
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal failure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroids', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19164, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'COPD',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Lung cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19792,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Oral cancer',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19788,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Blood cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19787, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Brain cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19174,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Postnatal depression',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19793, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pre-eclampsia', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 14512,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK4_female$modelDesign$targetId <- 21405
plpModelQRISK4_female$modelDesign$outcomeId <- 21397
plpModelQRISK4_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(18822,21395,18779,21387,21288,21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 22465, 21344, 19881, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19380, 19792, 19788, 19787, 19174, 19793, 14512), 
    cohortName = c('SBP', 'SD SBP', 'Cholesterol/HDL', 'Deprivation', 'Ex-smoker', 'Light smoker', 'Moderate smoker', 'Heavy smoker', 'Indian', 'Pakistani', 'Bangladeshi', 'Other Asian', 'Caribbean', 'Black African', 'Chinese', 'Other ethnic group', 'No learning disability', 'Learning disability', 'Down syndrome', 'FH of CHD', 'Type I Diabetes', 'Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Renal failure', 'Migraine', 'Corticosteroids', 'Systemic lupus', 'Atypical antipsychotic', 'Severe mental illness', 'COPD', 'Lung cancer', 'Oral cancer', 'Blood cancer', 'Brain cancer', 'Postnatal depression', 'Pre-eclampsia')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK4_female_OG_5,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK4_female_OG_5, 
  dirPath = './inst/models/QRISK4_female_OG_5'
    )

  

#QRISK1_male_OG_10
#============================================================================
canalysisId <- 668
plpModelQRISK1_male_OG_10 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395)*1000+canalysisId, 
    coefficient = c(50.634, 1.001, 1.022, 1.300, 1.417, 1.017, 1.004, 1.847, 0.993) 
  ), 
  intercept = 0, 
  mapping = QRISK1_male_10, 
  populationSettings = populationSettings10,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Body mass index', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of premature cardiovascular disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Antihypertensive agent', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SBPxantihypertensive agent interaction term', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = list(createLogAge(),createInteractionTermSBPAT())                  
)

plpModelQRISK1_male$modelDesign$targetId <- 21398                  
plpModelQRISK1_male$modelDesign$outcomeId <- 21397
plpModelQRISK1_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395),
    cohortName = c('Age','Cholesterol/HDL','Body mass index','Family history of premature cardiovascular disease','Current smoker','Townsend score', 'Systolic blood pressure', 'Antihypertensive agent','SBPxantihypertensive agent interaction term') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK1_male_OG_10,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK1_male_OG_10, 
  dirPath = './inst/models/QRISK1_male_OG_10'
    )

#QRISK1_female_OG_10
#============================================================================
canalysisId <- 668
plpModelQRISK1_female_OG_10 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395)*1000+canalysisId, 
    coefficient = c(87.75, 1.001, 1.015, 1.229, 1.530, 1.035, 1.005, 1.734, 0.996) 
  ), 
  intercept = 0, 
  mapping = QRISK1_female_10, 
  populationSettings = populationSettings10,
    covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Body mass index', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of premature cardiovascular disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Antihypertensive agent', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SBPxantihypertensive agent interaction term', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = list(createLogAge(), createInteractionTermSBPAT())                            
)

plpModelQRISK1_female$modelDesign$targetId <- 21399
plpModelQRISK1_female$modelDesign$outcomeId <- 21397
plpModelQRISK1_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395),
    cohortName = c('Age','Cholesterol/HDL','Body mass index','Family history of premature cardiovascular disease','Current smoker','Townsend score', 'Systolic blood pressure', 'Antihypertensive agent','SBPxantihypertensive agent interaction term')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK1_female_OG_10,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK1_female_OG_10, 
  dirPath = './inst/models/QRISK1_female_OG_10'
    )

#QRISK2_male_OG_10
#============================================================================
canalysisId <- 668
plpModelQRISK2_male_OG_10 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 18838, 18841, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 18841)*1000+canalysisId, 
    coefficient = c(1.00, 1.45, 1.97, 1.67, 1.37, 0.62, 0.63, 0.51, 0.91, 1.59, 0.218, 0.236, 0.0595, 1.19, 2.14, 1.65, 1.68, 2.20, 1.38, 2.40, 1.75, 0.985, 0.1946, 0.0482, 0.923, 0.932, 0.916, 0.902, 0.893) 
  ), 
  intercept = 0, 
  mapping = QRISK2_male_10, 
  populationSettings = populationSettings10,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'White or not recorded', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22466,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'BMI', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexBMI interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexTownsend interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsystolicbloodpressure interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexfamilyhistory interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsmoking interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agextreatedhypertension interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgextypeIIdiabetes interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexatrial fibrillation interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createLogAge()
)

plpModelQRISK2_male$modelDesign$targetId <- 21400
plpModelQRISK2_male$modelDesign$outcomeId <- 21397
plpModelQRISK2_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 18838, 18841, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 18841)
    cohortName = c('White or not recorded', 'Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Age','BMI','Townsend score','Systolic blood pressure','Cholesterol/HDL','Family history coronary heart disease','Current smoker','Treated hypertension','Type 2 diabetes','Rheumatoid arthritis','Atrial fibrillation','Renal disease','AgexBMI interaction','AgexTownsend interaction','Agexsystolicbloodpressure interaction','Agexfamilyhistory interaction','Agexsmoking interaction','Agextreatedhypertension interaction','Agextype2diabetes interaction', 'Agexatrialfibrillation interaction') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK2_male_OG_10,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK2_male_OG_10, 
  dirPath = './inst/models/QRISK2_male_OG_10'
    )
  
#QRISK2_female_OG_10
#============================================================================
canalysisId <- 668
plpModelQRISK2_female_OG_10 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(22466,21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 18838, 18841, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 18841)*1000+canalysisId, 
    coefficient = c(1.00, 1.43, 1.80, 1.35, 1.15, 1.08, 0.58, 0.69, 1.04, 1.66, 0.216, 0.274, 0.06, 1.17, 1.99, 1.80, 1.54, 2.54, 1.50, 3.06, 1.70, 0.976, 0.1876, 0.0483, 0.927, 0.931, 0.952, 0.904, 0.858)
  ), 
  intercept = 0, 
  mapping = QRISK2_female_10, 
  populationSettings = populationSettings10,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'White or not recorded', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22466,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'BMI', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexBMI interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexTownsend interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsystolicbloodpressure interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexfamilyhistory interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsmoking interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agextreatedhypertension interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgextypeIIdiabetes interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexatrial fibrillation interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createLogAge()
)

plpModelQRISK2_female$modelDesign$targetId <- 21401
plpModelQRISK2_female$modelDesign$outcomeId <- 21397
plpModelQRISK2_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 18838, 18841, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 18841),
    cohortName = c('White or not recorded','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Age','BMI','Townsend score','Systolic blood pressure','Cholesterol/HDL','Family history coronary heart disease','Current smoker','Treated hypertension','Type 2 diabetes','Rheumatoid arthritis','Atrial fibrillation','Renal disease','AgexBMI interaction','AgexTownsend interaction','Agexsystolicbloodpressure interaction','Agexfamilyhistory interaction','Agexsmoking interaction','Agextreatedhypertension interaction','Agextype2diabetes interaction', 'Agexatrialfibrillation interaction')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK2_female_OG_10,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK2_female_OG_10, 
  dirPath = './inst/models/QRISK2_female_OG_10'
    )

#QRISK3_male_OG_10
#============================================================================

# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK3_male_OG_10 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19165, 18779, 18822, 21395)*1000+canalysisId, 
    coefficient = c(0.236, 1.32, 1.61, 1.70, 1.04, 0.699, 0.670, 0.660, 0.769, 1.21, 1.74, 1.89, 2.20, 1.72, 3.44, 2.36, 1.68, 1.23, 2.42, 2.05, 1.29, 1.58, 1.55, 1.14, 1.13, 1.25, 1.19, 0.057, 0.111) 
  ), 
  intercept = 0, 
  mapping = QRISK3_male_10, 
  populationSettings = populationSettings10,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Former smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18820, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chronic kidney disease (stage 3,4 or 5)', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroid use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus erythematosus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19164, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372,
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    , createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Erectile dysfunction or treatment', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19165,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD of blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP()                         
)

plpModelQRISK3_male$modelDesign$targetId <- 21402
plpModelQRISK3_male$modelDesign$outcomeId <- 21397
plpModelQRISK3_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19165, 18779, 18822, 21395),
    cohortName = c('Townsend score','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Former smoker', 'Light smoker','Moderate smoker','Heavy smoker','Family history of coronary heart disease','Type I Diabetes','Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Chronic kidney disease (stage 3,4 or 5)','Migraine', 'Corticosteroid use', 'Systemic lupus erythematosus', 'Atypical antipsychotic use', 'Severe mental illness', 'Erectile dysfunction or treatment', 'Cholesterol/HDL', 'Systolic blood pressure', 'SD of blood pressure') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK3_male_OG_10,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK3_male_OG_10, 
  dirPath = './inst/models/QRISK3_male_OG_10'
    )
                  
#QRISK3_female_OG_10
#============================================================================

# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK3_female_OG_10 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291,18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 18779, 18822, 21395)*1000+canalysisId, 
    coefficient = c(0.294, 1.32, 1.76, 1.34, 1.08, 0.843, 0.675, 0.722, 0.843, 1.00, 1.14, 1.75, 1.95, 2.34, 1.58, 5.62, 2.91, 1.66, 1.24, 4.92, 1.92, 1.35, 1.81, 2.14, 1.29, 1.13, 1.17, 0.057, 0.108)
  ), 
  intercept = 0
  mapping = QRISK3_female_10, 
  populationSettings = populationSettings10,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Former smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18820,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chronic kidney disease (stage 3,4 or 5)', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19379,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroid use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus erythematosus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19164, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Standard deviation of blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK3_female$modelDesign$targetId <- 21403
plpModelQRISK3_female$modelDesign$outcomeId <- 21397
plpModelQRISK3_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291,18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 18779, 18822, 21395),
    cohortName = c('Townsend score','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Former smoker','Light smoker','Moderate smoker','Heavy smoker','Family history of coronary heart disease','Type I Diabetes','Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Chronic kidney disease (stage 3,4 or 5)','Migraine', 'Corticosteroid use', 'Systemic lupus erythematosus', 'Atypical antipsychotic use', 'Severe mental illness', 'Cholesterol/HDL', 'Systolic blood pressure', 'SD of blood pressure') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK3_female_OG_10,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK3_female_OG_10, 
  dirPath = './inst/models/QRISK3_female_OG_10'
    )



#QRISK4_male_OG_10
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK4_male_OG_10 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(18822,21395,18779,21387,21288, 21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 22465, 21344, 19881, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19380, 19792, 19788, 19787, 19174, 19165)*1000+canalysisId, 
    coefficient = c(0.065, 1.14, 1.15, 0.222, 1.19, 2.00, 2.08, 2.60, 1.19, 1.47, 1.41, 1.04, 0.67, 0.66, 0.72, 0.81, 1.00, 1.17, 2.35, 1.62, 3.28, 2.03, 2.20, 1.19, 2.59, 1.70, 1.41, 1.64, 1.68, 1.18, 1.37, 1.66, 1.49, 2.06, 5.45, 1.40)
  ), 
  intercept = 0, 
  mapping = QRISK4_male_10, 
  populationSettings = populationSettings10,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'SBP', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD SBP',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Deprivation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Ex-smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other ethnic group', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'No learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22465, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21344, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Down syndrome', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19881, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'FH of CHD', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type 1 Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18820, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type 2 Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal failure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroids', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19164, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'COPD',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19380,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Lung cancer',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19792,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Oral cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19788,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Blood cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19787, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Brain cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19174, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Erectile dysfunction',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19165,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK4_male$modelDesign$targetId <- 21404
plpModelQRISK4_male$modelDesign$outcomeId <- 21397
plpModelQRISK4_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(18822,21395,18779,21387,21288, 21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 22465, 21344, 19881, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19380, 19792, 19788, 19787, 19174, 19165),
    cohortName = c('SBP', 'SD SBP', 'Cholesterol/HDL', 'Deprivation', 'Ex-smoker', 'Light smoker', 'Moderate smoker', 'Heavy smoker', 'Indian', 'Pakistani', 'Bangladeshi', 'Other Asian', 'Caribbean', 'Black African', 'Chinese', 'Other ethnic group', 'No learning disability', 'Learning disability', 'Down syndrome', 'FH of CHD', 'Type I Diabetes', 'Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Renal failure', 'Migraine', 'Corticosteroids', 'Systemic lupus', 'Atypical antipsychotics','Severe mental illness', 'COPD', 'Lung cancer', 'Oral cancer', 'Blood cancer', 'Brain cancer', 'Erectile dysfunction') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK4_male_OG_10,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK4_male_OG_10, 
  dirPath = './inst/models/QRISK4_male_OG_10'
    )

  
#QRISK4_female_OG_10
#============================================================================

# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK4_female_OG_10 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(18822,21395,18779,21387,21288,21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 22465, 21344, 19881, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19380, 19792, 19788, 19787, 19174, 19793, 14512)*1000+canalysisId, 
    coefficient = c(0.0635, 1.16, 1.14, 0.252, 1.19, 2.12, 2.30, 2.90, 1.13, 1.56, 1.28, 0.98, 0.94, 0.76, 0.68, 0.94, 1.00, 1.45, 3.18, 1.46, 4.52, 2.49, 2.20, 1.28, 4.50, 1.81, 1.46, 1.70, 2.22, 1.24, 1.22, 1.85, 3.50, 1.55, 2.13, 4.52, 1.18, 1.56) 
  ), 
  intercept = 0, 
  mapping = QRISK4_female_10, 
  populationSettings = populationSettings10,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'SBP', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD SBP',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL ratio', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Deprivation',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Ex-smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Caribbean',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other ethnic group',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'No learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22465, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21344,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Down syndrome',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19881,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'FH of CHD', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18820,          
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal failure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroids', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19164, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'COPD',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Lung cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19792,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Oral cancer',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19788,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Blood cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19787, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Brain cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19174,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Postnatal depression',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19793, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pre-eclampsia', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 14512,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK4_female$modelDesign$targetId <- 21405
plpModelQRISK4_female$modelDesign$outcomeId <- 21397
plpModelQRISK4_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(18822,21395,18779,21387,21288,21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 22465, 21344, 19881, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19380, 19792, 19788, 19787, 19174, 19793, 14512), 
    cohortName = c('SBP', 'SD SBP', 'Cholesterol/HDL', 'Deprivation', 'Ex-smoker', 'Light smoker', 'Moderate smoker', 'Heavy smoker', 'Indian', 'Pakistani', 'Bangladeshi', 'Other Asian', 'Caribbean', 'Black African', 'Chinese', 'Other ethnic group', 'No learning disability', 'Learning disability', 'Down syndrome', 'FH of CHD', 'Type I Diabetes', 'Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Renal failure', 'Migraine', 'Corticosteroids', 'Systemic lupus', 'Atypical antipsychotic', 'Severe mental illness', 'COPD', 'Lung cancer', 'Oral cancer', 'Blood cancer', 'Brain cancer', 'Postnatal depression', 'Pre-eclampsia')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK4_female_OG_10,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK4_female_OG_10, 
  dirPath = './inst/models/QRISK4_female_OG_10'
    )

 #
  #
  #
  #
  #
  #
  #
  #
  #
#
  
#============================================================================
#JJ DEFINITIONS
#============================================================================

#QRISK1_male_JJ_1
#============================================================================
canalysisId <- 668
plpModelQRISK1_male_JJ_1 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395)*1000+canalysisId, #TODO
    coefficient = c(50.634, 1.001, 1.022, 1.300, 1.417, 1.017, 1.004, 1.847, 0.993) 
  ), 
  intercept = 0, 
  mapping = QRISK1_male_1,  
  populationSettings = populationSettings1,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Body mass index', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of premature cardiovascular disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Antihypertensive agent', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SBPxantihypertensive agent interaction term', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = list(createLogAge(),createInteractionTermSBPAT())                  
)

plpModelQRISK1_male$modelDesign$targetId <- 21398                  
plpModelQRISK1_male$modelDesign$outcomeId <- 21397
plpModelQRISK1_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395),
    cohortName = c('Age','Cholesterol/HDL','Body mass index','Family history of premature cardiovascular disease','Current smoker','Townsend score', 'Systolic blood pressure', 'Antihypertensive agent','SBPxantihypertensive agent interaction term') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK1_male_JJ_1,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK1_male_JJ_1, 
  dirPath = './inst/models/QRISK1_male_JJ_1'
    )

#QRISK1_female_JJ_1
#============================================================================
canalysisId <- 668
plpModelQRISK1_female_JJ_1 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395)*1000+canalysisId, 
    coefficient = c(87.75, 1.001, 1.015, 1.229, 1.530, 1.035, 1.005, 1.734, 0.996) 
  ), 
  intercept = 0, 
  mapping = QRISK1_female_1, 
  populationSettings = populationSettings1,
    covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Body mass index', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of premature cardiovascular disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Antihypertensive agent', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SBPxantihypertensive agent interaction term', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = list(createLogAge(), createInteractionTermSBPAT())                            
)

plpModelQRISK1_female$modelDesign$targetId <- 21399
plpModelQRISK1_female$modelDesign$outcomeId <- 21397
plpModelQRISK1_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395),
    cohortName = c('Age','Cholesterol/HDL','Body mass index','Family history of premature cardiovascular disease','Current smoker','Townsend score', 'Systolic blood pressure', 'Antihypertensive agent','SBPxantihypertensive agent interaction term')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK1_female_JJ_1,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK1_female_JJ_1, 
  dirPath = './inst/models/QRISK1_female_JJ_1'
    )

#QRISK2_male_JJ_1
#============================================================================
canalysisId <- 668
plpModelQRISK2_male_JJ_1 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 19882, 19884, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 19884)*1000+canalysisId, 
    coefficient = c(1.00, 1.45, 1.97, 1.67, 1.37, 0.62, 0.63, 0.51, 0.91, 1.59, 0.218, 0.236, 0.0595, 1.19, 2.14, 1.65, 1.68, 2.20, 1.38, 2.40, 1.75, 0.985, 0.1946, 0.0482, 0.923, 0.932, 0.916, 0.902, 0.893) 
  ), 
  intercept = 0, 
  mapping = QRISK2_male_1, 
  populationSettings = populationSettings1,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'White or not recorded', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22466,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'BMI', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexBMI interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexTownsend interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsystolicbloodpressure interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexfamilyhistory interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsmoking interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agextreatedhypertension interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgextypeIIdiabetes interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexatrial fibrillation interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createLogAge()
)

plpModelQRISK2_male$modelDesign$targetId <- 21400
plpModelQRISK2_male$modelDesign$outcomeId <- 21397
plpModelQRISK2_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 19882, 19884, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 19884)
    cohortName = c('White or not recorded', 'Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Age','BMI','Townsend score','Systolic blood pressure','Cholesterol/HDL','Family history coronary heart disease','Current smoker','Treated hypertension','Type 2 diabetes','Rheumatoid arthritis','Atrial fibrillation','Renal disease','AgexBMI interaction','AgexTownsend interaction','Agexsystolicbloodpressure interaction','Agexfamilyhistory interaction','Agexsmoking interaction','Agextreatedhypertension interaction','Agextype2diabetes interaction', 'Agexatrialfibrillation interaction') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK2_male_JJ_1,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK2_male_JJ_1, 
  dirPath = './inst/models/QRISK2_male_JJ_1'
    )
  
#QRISK2_female_JJ_1
#============================================================================
canalysisId <- 668
plpModelQRISK2_female_JJ_1 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 19882, 19884, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 19884)*1000+canalysisId, 
    coefficient = c(1.00, 1.43, 1.80, 1.35, 1.15, 1.08, 0.58, 0.69, 1.04, 1.66, 0.216, 0.274, 0.06, 1.17, 1.99, 1.80, 1.54, 2.54, 1.50, 3.06, 1.70, 0.976, 0.1876, 0.0483, 0.927, 0.931, 0.952, 0.904, 0.858)
  ), 
  intercept = 0, 
  mapping = QRISK2_female_1, 
  populationSettings = populationSettings1,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'White or not recorded', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22466,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'BMI', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexBMI interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexTownsend interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsystolicbloodpressure interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexfamilyhistory interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsmoking interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agextreatedhypertension interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgextypeIIdiabetes interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexatrial fibrillation interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createLogAge()
)

plpModelQRISK2_female$modelDesign$targetId <- 21401
plpModelQRISK2_female$modelDesign$outcomeId <- 21397
plpModelQRISK2_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 19882, 19884, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 19884),
    cohortName = c('White or not recorded', 'Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Age','BMI','Townsend score','Systolic blood pressure','Cholesterol/HDL','Family history coronary heart disease','Current smoker','Treated hypertension','Type 2 diabetes','Rheumatoid arthritis','Atrial fibrillation','Renal disease','AgexBMI interaction','AgexTownsend interaction','Agexsystolicbloodpressure interaction','Agexfamilyhistory interaction','Agexsmoking interaction','Agextreatedhypertension interaction','Agextype2diabetes interaction', 'Agexatrialfibrillation interaction')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK2_female_JJ_1,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK2_female_JJ_1, 
  dirPath = './inst/models/QRISK2_female_JJ_1'
    )

#QRISK3_male_JJ_1
#============================================================================

# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK3_male_JJ_1 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19887, 18779, 18822, 21395)*1000+canalysisId, 
    coefficient = c(0.236, 1.32, 1.61, 1.70, 1.04, 0.699, 0.670, 0.660, 0.769, 1.21, 1.74, 1.89, 2.20, 1.72, 3.44, 2.36, 1.68, 1.23, 2.42, 2.05, 1.29, 1.58, 1.55, 1.14, 1.13, 1.25, 1.19, 0.057, 0.111) 
  ), 
  intercept = 0, 
  mapping = QRISK3_male_1, 
  populationSettings = populationSettings1,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Former smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19893, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chronic kidney disease (stage 3,4 or 5)', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 12467,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroid use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus erythematosus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19880, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372,
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    , createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Erectile dysfunction or treatment', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19887,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD of blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP()                         
)

plpModelQRISK3_male$modelDesign$targetId <- 21402
plpModelQRISK3_male$modelDesign$outcomeId <- 21397
plpModelQRISK3_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19887, 18779, 18822, 21395),
    cohortName = c('Townsend score','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Former smoker', 'Light smoker','Moderate smoker','Heavy smoker','Family history of coronary heart disease','Type I Diabetes','Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Chronic kidney disease (stage 3,4 or 5)','Migraine', 'Corticosteroid use', 'Systemic lupus erythematosus', 'Atypical antipsychotic use', 'Severe mental illness', 'Erectile dysfunction or treatment', 'Cholesterol/HDL', 'Systolic blood pressure', 'SD of blood pressure') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK3_male_JJ_1,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK3_male_JJ_1, 
  dirPath = './inst/models/QRISK3_male_JJ_1'
    )
                  
#QRISK3_female_JJ_1
#============================================================================

# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK3_female_JJ_1 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291,18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 18779, 18822, 21395)*1000+canalysisId, 
    coefficient = c(0.294, 1.32, 1.76, 1.34, 1.08, 0.843, 0.675, 0.722, 0.843, 1.00, 1.14, 1.75, 1.95, 2.34, 1.58, 5.62, 2.91, 1.66, 1.24, 4.92, 1.92, 1.35, 1.81, 2.14, 1.29, 1.13, 1.17, 0.057, 0.108)
  ), 
  intercept = 0
  mapping = QRISK3_female_1, 
  populationSettings = populationSettings1,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Former smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19893,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chronic kidney disease (stage 3,4 or 5)', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 12467,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroid use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus erythematosus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19880, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Standard deviation of blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK3_female$modelDesign$targetId <- 21403
plpModelQRISK3_female$modelDesign$outcomeId <- 21397
plpModelQRISK3_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291,18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 18779, 18822, 21395),
    cohortName = c('Townsend score','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Former smoker','Light smoker','Moderate smoker','Heavy smoker','Family history of coronary heart disease','Type I Diabetes','Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Chronic kidney disease (stage 3,4 or 5)','Migraine', 'Corticosteroid use', 'Systemic lupus erythematosus', 'Atypical antipsychotic use', 'Severe mental illness', 'Cholesterol/HDL', 'Systolic blood pressure', 'SD of blood pressure') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
 FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK3_female_JJ_1,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK3_female_JJ_1, 
  dirPath = './inst/models/QRISK3_female_JJ_1'
    )



#QRISK4_male_JJ_1
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK4_male_JJ_1 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(18822,21395,18779,21387,21288, 21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21344, 14949, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19380, 19888, 19788, 19787, 19174, 19887)*1000+canalysisId, 
    coefficient = c(0.065, 1.14, 1.15, 0.222, 1.19, 2.00, 2.08, 2.60, 1.19, 1.47, 1.41, 1.04, 0.67, 0.66, 0.72, 0.81, 1.17, 2.35, 1.62, 3.28, 2.03, 2.20, 1.19, 2.59, 1.70, 1.41, 1.64, 1.68, 1.18, 1.37, 1.66, 1.49, 2.06, 5.45, 1.40)
  ), 
  intercept = 0, 
  mapping = QRISK4_male_1, 
  populationSettings = populationSettings1,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'SBP', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD SBP',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Deprivation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Ex-smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other ethnic group', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21344, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Down syndrome', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 14949, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'FH of CHD', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type 1 Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19893, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type 2 Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal failure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 12467,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroids', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19880, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'COPD',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19380,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Lung cancer',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19888,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Oral cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19788,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Blood cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19787, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Brain cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19174, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Erectile dysfunction',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19887,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK4_male$modelDesign$targetId <- 21404
plpModelQRISK4_male$modelDesign$outcomeId <- 21397
plpModelQRISK4_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(18822,21395,18779,21387,21288, 21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21344, 14949, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19380, 19888, 19788, 19787, 19174, 19887),
    cohortName = c('SBP', 'SD SBP', 'Cholesterol/HDL', 'Deprivation', 'Ex-smoker', 'Light smoker', 'Moderate smoker', 'Heavy smoker', 'Indian', 'Pakistani', 'Bangladeshi', 'Other Asian', 'Caribbean', 'Black African', 'Chinese', 'Other ethnic group', 'Learning disability', 'Down syndrome', 'FH of CHD', 'Type I Diabetes', 'Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Renal failure', 'Migraine', 'Corticosteroids', 'Systemic lupus', 'Atypical antipsychotics','Severe mental illness', 'COPD', 'Lung cancer', 'Oral cancer', 'Blood cancer', 'Brain cancer', 'Erectile dysfunction') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK4_male_JJ_1,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK4_male_JJ_1, 
  dirPath = './inst/models/QRISK4_male_JJ_1'
    )

  
#QRISK4_female_JJ_1
#============================================================================

# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK4_female_JJ_1 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(18822,21395,18779,21387,21288,21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21344, 14949, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19380, 19888, 19788, 19787, 19174, 19793, 14512)*1000+canalysisId, 
    coefficient = c(0.0635, 1.16, 1.14, 0.252, 1.19, 2.12, 2.30, 2.90, 1.13, 1.56, 1.28, 0.98, 0.94, 0.76, 0.68, 0.94, 1.45, 3.18, 1.46, 4.52, 2.49, 2.20, 1.28, 4.50, 1.81, 1.46, 1.70, 2.22, 1.24, 1.22, 1.85, 3.50, 1.55, 2.13, 4.52, 1.18, 1.56) 
  ), 
  intercept = 0, 
  mapping = QRISK4_female_1, 
  populationSettings = populationSettings1,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'SBP', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD SBP',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL ratio', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Deprivation',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Ex-smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Caribbean',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other ethnic group',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21344,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Down syndrome',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 14949,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'FH of CHD', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19893,          
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal failure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 12467,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroids', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19880, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'COPD',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Lung cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19888,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Oral cancer',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19788,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Blood cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19787, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Brain cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19174,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Postnatal depression',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19793, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pre-eclampsia', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 14512,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK4_female$modelDesign$targetId <- 21405
plpModelQRISK4_female$modelDesign$outcomeId <- 21397
plpModelQRISK4_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(18822,21395,18779,21387,21288,21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21344, 14949, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19380, 19888, 19788, 19787, 19174, 19793, 14512), 
    cohortName = c('SBP', 'SD SBP', 'Cholesterol/HDL', 'Deprivation', 'Ex-smoker', 'Light smoker', 'Moderate smoker', 'Heavy smoker', 'Indian', 'Pakistani', 'Bangladeshi', 'Other Asian', 'Caribbean', 'Black African', 'Chinese', 'Other ethnic group', 'Learning disability', 'Down syndrome', 'FH of CHD', 'Type I Diabetes', 'Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Renal failure', 'Migraine', 'Corticosteroids', 'Systemic lupus', 'Atypical antipsychotic', 'Severe mental illness', 'COPD', 'Lung cancer', 'Oral cancer', 'Blood cancer', 'Brain cancer', 'Postnatal depression', 'Pre-eclampsia')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK4_female_JJ_1,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK4_female_JJ_1, 
  dirPath = './inst/models/QRISK4_female_JJ_1'
    )



                  
                  
#QRISK1_male_JJ_3
#=======================================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK1_male_JJ_3 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395)*1000+canalysisId, 
    coefficient = c(50.634, 1.001, 1.022, 1.300, 1.417, 1.017, 1.004, 1.847, 0.993) 
  ), 
  intercept = 0, 
  mapping = QRISK1_male_3, 
  populationSettings = populationSettings3,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Body mass index', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of premature cardiovascular disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Antihypertensive agent', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SBPxantihypertensive agent interaction term', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = list(createLogAge(),createInteractionTermSBPAT())                  
)

plpModelQRISK1_male$modelDesign$targetId <- 21398                  
plpModelQRISK1_male$modelDesign$outcomeId <- 21397
plpModelQRISK1_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395),
    cohortName = c('Age','Cholesterol/HDL','Body mass index','Family history of premature cardiovascular disease','Current smoker','Townsend score', 'Systolic blood pressure', 'Antihypertensive agent','SBPxantihypertensive agent interaction term') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK1_male_JJ_3,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK1_male_JJ_3, 
  dirPath = './inst/models/QRISK1_male_JJ_3'
    )

#QRISK1_female_JJ_3
#=======================================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK1_female_JJ_3 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395)*1000+canalysisId, 
    coefficient = c(87.75, 1.001, 1.015, 1.229, 1.530, 1.035, 1.005, 1.734, 0.996) 
  ), 
  intercept = 0, 
  mapping = QRISK1_female_3, 
  populationSettings = populationSettings3,
    covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Body mass index', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of premature cardiovascular disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Antihypertensive agent', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SBPxantihypertensive agent interaction term', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = list(createLogAge(), createInteractionTermSBPAT())                            
)

plpModelQRISK1_female$modelDesign$targetId <- 21399
plpModelQRISK1_female$modelDesign$outcomeId <- 21397
plpModelQRISK1_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395),
    cohortName = c('Age','Cholesterol/HDL','Body mass index','Family history of premature cardiovascular disease','Current smoker','Townsend score', 'Systolic blood pressure', 'Antihypertensive agent','SBPxantihypertensive agent interaction term')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK1_female_JJ_3,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK1_female_JJ_3, 
  dirPath = './inst/models/QRISK1_female_JJ_3'
    )

#QRISK2_male_JJ_3
#=============================================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK2_male_JJ_3 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 19882, 19884, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 19884)*1000+canalysisId, 
    coefficient = c(1.00, 1.45, 1.97, 1.67, 1.37, 0.62, 0.63, 0.51, 0.91, 1.59, 0.218, 0.236, 0.0595, 1.19, 2.14, 1.65, 1.68, 2.20, 1.38, 2.40, 1.75, 0.985, 0.1946, 0.0482, 0.923, 0.932, 0.916, 0.902, 0.893) 
  ), 
  intercept = 0, 
  mapping = QRISK2_male_3, 
  populationSettings = populationSettings3,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'White or not recorded', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22466,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'BMI', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexBMI interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexTownsend interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsystolicbloodpressure interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexfamilyhistory interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsmoking interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agextreatedhypertension interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgextypeIIdiabetes interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexatrial fibrillation interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createLogAge()
)

plpModelQRISK2_male$modelDesign$targetId <- 21400
plpModelQRISK2_male$modelDesign$outcomeId <- 21397
plpModelQRISK2_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 19882, 19884, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 19884)
    cohortName = c('White or not recorded','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Age','BMI','Townsend score','Systolic blood pressure','Cholesterol/HDL','Family history coronary heart disease','Current smoker','Treated hypertension','Type 2 diabetes','Rheumatoid arthritis','Atrial fibrillation','Renal disease','AgexBMI interaction','AgexTownsend interaction','Agexsystolicbloodpressure interaction','Agexfamilyhistory interaction','Agexsmoking interaction','Agextreatedhypertension interaction','Agextype2diabetes interaction', 'Agexatrialfibrillation interaction') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK2_male_JJ_3,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK2_male_JJ_3, 
  dirPath = './inst/models/QRISK2_male_JJ_3'
    )

#QRISK2_female_JJ_3
#=============================================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK2_female_JJ_3 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 19882, 19884, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 19884)*1000+canalysisId, 
    coefficient = c(1.00, 1.43, 1.80, 1.35, 1.15, 1.08, 0.58, 0.69, 1.04, 1.66, 0.216, 0.274, 0.06, 1.17, 1.99, 1.80, 1.54, 2.54, 1.50, 3.06, 1.70, 0.976, 0.1876, 0.0483, 0.927, 0.931, 0.952, 0.904, 0.858)
  ), 
  intercept = 0, 
  mapping = QRISK2_female_3, 
  populationSettings = populationSettings3,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'White or not recorded', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22466,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'BMI', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexBMI interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexTownsend interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsystolicbloodpressure interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexfamilyhistory interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsmoking interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agextreatedhypertension interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgextypeIIdiabetes interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexatrial fibrillation interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createLogAge()
)

plpModelQRISK2_female$modelDesign$targetId <- 21401
plpModelQRISK2_female$modelDesign$outcomeId <- 21397
plpModelQRISK2_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 19882, 19884, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 19884),
    cohortName = c('White or not recorded', 'Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Age','BMI','Townsend score','Systolic blood pressure','Cholesterol/HDL','Family history coronary heart disease','Current smoker','Treated hypertension','Type 2 diabetes','Rheumatoid arthritis','Atrial fibrillation','Renal disease','AgexBMI interaction','AgexTownsend interaction','Agexsystolicbloodpressure interaction','Agexfamilyhistory interaction','Agexsmoking interaction','Agextreatedhypertension interaction','Agextype2diabetes interaction', 'Agexatrialfibrillation interaction')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK2_female_JJ_3,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK2_female_JJ_3, 
  dirPath = './inst/models/QRISK2_female_JJ_3'
    )

#QRISK3_male_JJ_3
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK3_male_JJ_3 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19887, 18779, 18822, 21395)*1000+canalysisId, 
    coefficient = c(0.236, 1.32, 1.61, 1.70, 1.04, 0.699, 0.670, 0.660, 0.769, 1.21, 1.74, 1.89, 2.20, 1.72, 3.44, 2.36, 1.68, 1.23, 2.42, 2.05, 1.29, 1.58, 1.55, 1.14, 1.13, 1.25, 1.19, 0.057, 0.111) 
  ), 
  intercept = 0, 
  mapping = QRISK3_male_3, 
  populationSettings = populationSettings3,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Former smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19893, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chronic kidney disease (stage 3,4 or 5)', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 12467,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroid use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus erythematosus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19880, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372,
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    , createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Erectile dysfunction or treatment', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19887,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD of blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP()                         
)

plpModelQRISK3_male$modelDesign$targetId <- 21402
plpModelQRISK3_male$modelDesign$outcomeId <- 21397
plpModelQRISK3_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19887, 18779, 18822, 21395),
    cohortName = c('Townsend score','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Former smoker', 'Light smoker','Moderate smoker','Heavy smoker','Family history of coronary heart disease','Type I Diabetes','Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Chronic kidney disease (stage 3,4 or 5)','Migraine', 'Corticosteroid use', 'Systemic lupus erythematosus', 'Atypical antipsychotic use', 'Severe mental illness', 'Erectile dysfunction or treatment', 'Cholesterol/HDL', 'Systolic blood pressure', 'SD of blood pressure') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK3_male_JJ_3,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK3_male_JJ_3, 
  dirPath = './inst/models/QRISK3_male_JJ_3'
    )

#QRISK3_female_JJ_3
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK3_female_JJ_3 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291,18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 18779, 18822, 21395)*1000+canalysisId, 
    coefficient = c(0.294, 1.32, 1.76, 1.34, 1.08, 0.843, 0.675, 0.722, 0.843, 1.00, 1.14, 1.75, 1.95, 2.34, 1.58, 5.62, 2.91, 1.66, 1.24, 4.92, 1.92, 1.35, 1.81, 2.14, 1.29, 1.13, 1.17, 0.057, 0.108)
  ), 
  intercept = 0
  mapping = QRISK3_female_3, 
  populationSettings = populationSettings3,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Former smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19893,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chronic kidney disease (stage 3,4 or 5)', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 12467,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroid use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus erythematosus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19880, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Standard deviation of blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK3_female$modelDesign$targetId <- 21403
plpModelQRISK3_female$modelDesign$outcomeId <- 21397
plpModelQRISK3_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291,18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 18779, 18822, 21395),
    cohortName = c('Townsend score','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Former smoker','Light smoker','Moderate smoker','Heavy smoker','Family history of coronary heart disease','Type I Diabetes','Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Chronic kidney disease (stage 3,4 or 5)','Migraine', 'Corticosteroid use', 'Systemic lupus erythematosus', 'Atypical antipsychotic use', 'Severe mental illness', 'Cholesterol/HDL', 'Systolic blood pressure', 'SD of blood pressure') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK3_female_JJ_3,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK3_female_JJ_3, 
  dirPath = './inst/models/QRISK3_female_JJ_3'
    )


#QRISK4_male_JJ_3
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK4_male_JJ_3 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(18822,21395,18779,21387,21288, 21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21344, 14949, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19380, 19888, 19788, 19787, 19174, 19887)*1000+canalysisId, 
    coefficient = c(0.065, 1.14, 1.15, 0.222, 1.19, 2.00, 2.08, 2.60, 1.19, 1.47, 1.41, 1.04, 0.67, 0.66, 0.72, 0.81, 1.17, 2.35, 1.62, 3.28, 2.03, 2.20, 1.19, 2.59, 1.70, 1.41, 1.64, 1.68, 1.18, 1.37, 1.66, 1.49, 2.06, 5.45, 1.40)
  ), 
  intercept = 0, 
  mapping = QRISK4_male_3, 
  populationSettings = populationSettings3,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'SBP', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD SBP',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Deprivation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Ex-smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other ethnic group', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21344, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Down syndrome', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 14949, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'FH of CHD', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type 1 Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19893, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type 2 Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal failure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 12467,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroids', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19880, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'COPD',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19380,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Lung cancer',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19888,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Oral cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19788,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Blood cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19787, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Brain cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19174, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Erectile dysfunction',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19887,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK4_male$modelDesign$targetId <- 21404
plpModelQRISK4_male$modelDesign$outcomeId <- 21397
plpModelQRISK4_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(18822,21395,18779,21387,21288, 21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21344, 14949, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19380, 19888, 19788, 19787, 19174, 19887),
    cohortName = c('SBP', 'SD SBP', 'Cholesterol/HDL', 'Deprivation', 'Ex-smoker', 'Light smoker', 'Moderate smoker', 'Heavy smoker', 'Indian', 'Pakistani', 'Bangladeshi', 'Other Asian', 'Caribbean', 'Black African', 'Chinese', 'Other ethnic group', 'Learning disability', 'Down syndrome', 'FH of CHD', 'Type I Diabetes', 'Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Renal failure', 'Migraine', 'Corticosteroids', 'Systemic lupus', 'Atypical antipsychotics','Severe mental illness', 'COPD', 'Lung cancer', 'Oral cancer', 'Blood cancer', 'Brain cancer', 'Erectile dysfunction') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK4_male_JJ_3,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK4_male_JJ_3, 
  dirPath = './inst/models/QRISK4_male_JJ_3'
    )

#QRISK4_female_JJ_3
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK4_female_JJ_3 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(18822,21395,18779,21387,21288,21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21344, 14949, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19380, 19888, 19788, 19787, 19174, 19793, 14512)*1000+canalysisId, 
    coefficient = c(0.0635, 1.16, 1.14, 0.252, 1.19, 2.12, 2.30, 2.90, 1.13, 1.56, 1.28, 0.98, 0.94, 0.76, 0.68, 0.94, 1.45, 3.18, 1.46, 4.52, 2.49, 2.20, 1.28, 4.50, 1.81, 1.46, 1.70, 2.22, 1.24, 1.22, 1.85, 3.50, 1.55, 2.13, 4.52, 1.18, 1.56) 
  ), 
  intercept = 0, 
  mapping = QRISK4_female_3, 
  populationSettings = populationSettings3,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'SBP', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD SBP',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL ratio', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Deprivation',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Ex-smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Caribbean',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other ethnic group',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21344,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Down syndrome',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 14949,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'FH of CHD', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19893,          
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal failure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 12467,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroids', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19880, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'COPD',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Lung cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19888,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Oral cancer',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19788,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Blood cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19787, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Brain cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19174,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Postnatal depression',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19793, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pre-eclampsia', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 14512,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK4_female$modelDesign$targetId <- 21405
plpModelQRISK4_female$modelDesign$outcomeId <- 21397
plpModelQRISK4_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(18822,21395,18779,21387,21288,21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21344, 14949, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19380, 19888, 19788, 19787, 19174, 19793, 14512), 
    cohortName = c('SBP', 'SD SBP', 'Cholesterol/HDL', 'Deprivation', 'Ex-smoker', 'Light smoker', 'Moderate smoker', 'Heavy smoker', 'Indian', 'Pakistani', 'Bangladeshi', 'Other Asian', 'Caribbean', 'Black African', 'Chinese', 'Other ethnic group', 'Learning disability', 'Down syndrome', 'FH of CHD', 'Type I Diabetes', 'Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Renal failure', 'Migraine', 'Corticosteroids', 'Systemic lupus', 'Atypical antipsychotic', 'Severe mental illness', 'COPD', 'Lung cancer', 'Oral cancer', 'Blood cancer', 'Brain cancer', 'Postnatal depression', 'Pre-eclampsia')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK4_female_JJ_3,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK4_female_JJ_3, 
  dirPath = './inst/models/QRISK4_female_JJ_3'
    )


#QRISK1_male_JJ_5
#============================================================================
canalysisId <- 668
plpModelQRISK1_male_JJ_5 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395)*1000+canalysisId, 
    coefficient = c(50.634, 1.001, 1.022, 1.300, 1.417, 1.017, 1.004, 1.847, 0.993) 
  ), 
  intercept = 0, 
  mapping = QRISK1_male_5, 
  populationSettings = populationSettings5,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Body mass index', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of premature cardiovascular disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Antihypertensive agent', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SBPxantihypertensive agent interaction term', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = list(createLogAge(),createInteractionTermSBPAT())                  
)

plpModelQRISK1_male$modelDesign$targetId <- 21398                  
plpModelQRISK1_male$modelDesign$outcomeId <- 21397
plpModelQRISK1_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395),
    cohortName = c('Age','Cholesterol/HDL','Body mass index','Family history of premature cardiovascular disease','Current smoker','Townsend score', 'Systolic blood pressure', 'Antihypertensive agent','SBPxantihypertensive agent interaction term') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK1_male_JJ_5,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK1_male_JJ_5, 
  dirPath = './inst/models/QRISK1_male_JJ_5'
    )

#QRISK1_female_JJ_5
#============================================================================
canalysisId <- 668
plpModelQRISK1_female_JJ_5 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395)*1000+canalysisId, 
    coefficient = c(87.75, 1.001, 1.015, 1.229, 1.530, 1.035, 1.005, 1.734, 0.996) 
  ), 
  intercept = 0, 
  mapping = QRISK1_female_5,  
  populationSettings = populationSettings5,
    covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Body mass index', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of premature cardiovascular disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Antihypertensive agent', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SBPxantihypertensive agent interaction term', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = list(createLogAge(), createInteractionTermSBPAT())                            
)

plpModelQRISK1_female$modelDesign$targetId <- 21399
plpModelQRISK1_female$modelDesign$outcomeId <- 21397
plpModelQRISK1_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395),
    cohortName = c('Age','Cholesterol/HDL','Body mass index','Family history of premature cardiovascular disease','Current smoker','Townsend score', 'Systolic blood pressure', 'Antihypertensive agent','SBPxantihypertensive agent interaction term')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK1_female_JJ_5,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK1_female_JJ_5, 
  dirPath = './inst/models/QRISK1_female_JJ_5'
    )

#QRISK2_male_JJ_5
#============================================================================
canalysisId <- 668
plpModelQRISK2_male_JJ_5 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 19882, 19884, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 19884)*1000+canalysisId, 
    coefficient = c(1.00, 1.45, 1.97, 1.67, 1.37, 0.62, 0.63, 0.51, 0.91, 1.59, 0.218, 0.236, 0.0595, 1.19, 2.14, 1.65, 1.68, 2.20, 1.38, 2.40, 1.75, 0.985, 0.1946, 0.0482, 0.923, 0.932, 0.916, 0.902, 0.893) 
  ), 
  intercept = 0, 
  mapping = QRISK2_male_5,  
  populationSettings = populationSettings5,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'White or not recorded', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22466,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'BMI', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexBMI interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexTownsend interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsystolicbloodpressure interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexfamilyhistory interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsmoking interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agextreatedhypertension interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgextypeIIdiabetes interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexatrial fibrillation interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createLogAge()
)

plpModelQRISK2_male$modelDesign$targetId <- 21400
plpModelQRISK2_male$modelDesign$outcomeId <- 21397
plpModelQRISK2_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 19882, 19884, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 19884)
    cohortName = c('White or not recorded', 'Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Age','BMI','Townsend score','Systolic blood pressure','Cholesterol/HDL','Family history coronary heart disease','Current smoker','Treated hypertension','Type 2 diabetes','Rheumatoid arthritis','Atrial fibrillation','Renal disease','AgexBMI interaction','AgexTownsend interaction','Agexsystolicbloodpressure interaction','Agexfamilyhistory interaction','Agexsmoking interaction','Agextreatedhypertension interaction','Agextype2diabetes interaction', 'Agexatrialfibrillation interaction') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK2_male_JJ_5,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK2_male_JJ_5, 
  dirPath = './inst/models/QRISK2_male_JJ_5'
    )
  
#QRISK2_female_JJ_5
#============================================================================
canalysisId <- 668
plpModelQRISK2_female_JJ_5 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 19882, 19884, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 19884)*1000+canalysisId, 
    coefficient = c(1.00, 1.43, 1.80, 1.35, 1.15, 1.08, 0.58, 0.69, 1.04, 1.66, 0.216, 0.274, 0.06, 1.17, 1.99, 1.80, 1.54, 2.54, 1.50, 3.06, 1.70, 0.976, 0.1876, 0.0483, 0.927, 0.931, 0.952, 0.904, 0.858)
  ), 
  intercept = 0, 
  mapping = QRISK2_female_5, 
  populationSettings = populationSettings5,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'White or not recorded', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22466,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'BMI', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexBMI interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexTownsend interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsystolicbloodpressure interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexfamilyhistory interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsmoking interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agextreatedhypertension interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgextypeIIdiabetes interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexatrial fibrillation interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createLogAge()
)

plpModelQRISK2_female$modelDesign$targetId <- 21401
plpModelQRISK2_female$modelDesign$outcomeId <- 21397
plpModelQRISK2_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 19882, 19884, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 19884),
    cohortName = c('White or not recorded', 'Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Age','BMI','Townsend score','Systolic blood pressure','Cholesterol/HDL','Family history coronary heart disease','Current smoker','Treated hypertension','Type 2 diabetes','Rheumatoid arthritis','Atrial fibrillation','Renal disease','AgexBMI interaction','AgexTownsend interaction','Agexsystolicbloodpressure interaction','Agexfamilyhistory interaction','Agexsmoking interaction','Agextreatedhypertension interaction','Agextype2diabetes interaction', 'Agexatrialfibrillation interaction')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK2_female_JJ_5,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK2_female_JJ_5, 
  dirPath = './inst/models/QRISK2_female_JJ_5'
    )

#QRISK3_male_JJ_5
#============================================================================

# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK3_male_JJ_5 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19887, 18779, 18822, 21395)*1000+canalysisId, 
    coefficient = c(0.236, 1.32, 1.61, 1.70, 1.04, 0.699, 0.670, 0.660, 0.769, 1.21, 1.74, 1.89, 2.20, 1.72, 3.44, 2.36, 1.68, 1.23, 2.42, 2.05, 1.29, 1.58, 1.55, 1.14, 1.13, 1.25, 1.19, 0.057, 0.111) 
  ), 
  intercept = 0, 
  mapping = QRISK3_male_5, 
  populationSettings = populationSettings5,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Former smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19893, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chronic kidney disease (stage 3,4 or 5)', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 12467,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroid use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus erythematosus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19880, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372,
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    , createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Erectile dysfunction or treatment', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19887,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD of blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP()                         
)

plpModelQRISK3_male$modelDesign$targetId <- 21402
plpModelQRISK3_male$modelDesign$outcomeId <- 21397
plpModelQRISK3_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19887, 18779, 18822, 21395),
    cohortName = c('Townsend score','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Former smoker', 'Light smoker','Moderate smoker','Heavy smoker','Family history of coronary heart disease','Type I Diabetes','Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Chronic kidney disease (stage 3,4 or 5)','Migraine', 'Corticosteroid use', 'Systemic lupus erythematosus', 'Atypical antipsychotic use', 'Severe mental illness', 'Erectile dysfunction or treatment', 'Cholesterol/HDL', 'Systolic blood pressure', 'SD of blood pressure') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK3_male_JJ_5,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK3_male_JJ_5, 
  dirPath = './inst/models/QRISK3_male_JJ_5'
    )
                  
#QRISK3_female_JJ_5
#============================================================================

# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK3_female_JJ_5 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291,18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 18779, 18822, 21395)*1000+canalysisId, 
    coefficient = c(0.294, 1.32, 1.76, 1.34, 1.08, 0.843, 0.675, 0.722, 0.843, 1.00, 1.14, 1.75, 1.95, 2.34, 1.58, 5.62, 2.91, 1.66, 1.24, 4.92, 1.92, 1.35, 1.81, 2.14, 1.29, 1.13, 1.17, 0.057, 0.108)
  ), 
  intercept = 0
  mapping = QRISK3_female_5, 
  populationSettings = populationSettings5,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Former smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19893,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chronic kidney disease (stage 3,4 or 5)', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 12467,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroid use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus erythematosus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19880, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Standard deviation of blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK3_female$modelDesign$targetId <- 21403
plpModelQRISK3_female$modelDesign$outcomeId <- 21397
plpModelQRISK3_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291,18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 18779, 18822, 21395),
    cohortName = c('Townsend score','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Former smoker','Light smoker','Moderate smoker','Heavy smoker','Family history of coronary heart disease','Type I Diabetes','Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Chronic kidney disease (stage 3,4 or 5)','Migraine', 'Corticosteroid use', 'Systemic lupus erythematosus', 'Atypical antipsychotic use', 'Severe mental illness', 'Cholesterol/HDL', 'Systolic blood pressure', 'SD of blood pressure') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK3_female_JJ_5,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK3_female_JJ_5, 
  dirPath = './inst/models/QRISK3_female_JJ_5'
    )



#QRISK4_male_JJ_5
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK4_male_JJ_5 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(18822,21395,18779,21387,21288, 21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21344, 14949, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19380, 19888, 19788, 19787, 19174, 19887)*1000+canalysisId, 
    coefficient = c(0.065, 1.14, 1.15, 0.222, 1.19, 2.00, 2.08, 2.60, 1.19, 1.47, 1.41, 1.04, 0.67, 0.66, 0.72, 0.81, 1.17, 2.35, 1.62, 3.28, 2.03, 2.20, 1.19, 2.59, 1.70, 1.41, 1.64, 1.68, 1.18, 1.37, 1.66, 1.49, 2.06, 5.45, 1.40)
  ), 
  intercept = 0, 
  mapping = QRISK4_male_5, 
  populationSettings = populationSettings5,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'SBP', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD SBP',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Deprivation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Ex-smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other ethnic group', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21344, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Down syndrome', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 14949, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'FH of CHD', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type 1 Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19893, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type 2 Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal failure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 12467,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroids', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19880, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'COPD',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19380,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Lung cancer',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19888,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Oral cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19788,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Blood cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19787, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Brain cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19174, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Erectile dysfunction',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19887,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK4_male$modelDesign$targetId <- 21404
plpModelQRISK4_male$modelDesign$outcomeId <- 21397
plpModelQRISK4_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(18822,21395,18779,21387,21288, 21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21344, 14949, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19380, 19888, 19788, 19787, 19174, 19887),
    cohortName = c('SBP', 'SD SBP', 'Cholesterol/HDL', 'Deprivation', 'Ex-smoker', 'Light smoker', 'Moderate smoker', 'Heavy smoker', 'Indian', 'Pakistani', 'Bangladeshi', 'Other Asian', 'Caribbean', 'Black African', 'Chinese', 'Other ethnic group', 'Learning disability', 'Down syndrome', 'FH of CHD', 'Type I Diabetes', 'Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Renal failure', 'Migraine', 'Corticosteroids', 'Systemic lupus', 'Atypical antipsychotics','Severe mental illness', 'COPD', 'Lung cancer', 'Oral cancer', 'Blood cancer', 'Brain cancer', 'Erectile dysfunction') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK4_male_JJ_5,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK4_male_JJ_5, 
  dirPath = './inst/models/QRISK4_male_JJ_5'
    )

  
#QRISK4_female_JJ_5
#============================================================================

# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK4_female_JJ_5 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(18822,21395,18779,21387,21288,21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21344, 14949, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19380, 19888, 19788, 19787, 19174, 19793, 14512)*1000+canalysisId, 
    coefficient = c(0.0635, 1.16, 1.14, 0.252, 1.19, 2.12, 2.30, 2.90, 1.13, 1.56, 1.28, 0.98, 0.94, 0.76, 0.68, 0.94, 1.45, 3.18, 1.46, 4.52, 2.49, 2.20, 1.28, 4.50, 1.81, 1.46, 1.70, 2.22, 1.24, 1.22, 1.85, 3.50, 1.55, 2.13, 4.52, 1.18, 1.56) 
  ), 
  intercept = 0, 
  mapping = QRISK4_female_5, 
  populationSettings = populationSettings5,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'SBP', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD SBP',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL ratio', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Deprivation',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Ex-smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Caribbean',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other ethnic group',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21344,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Down syndrome',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 14949,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'FH of CHD', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19893,          
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal failure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 12467,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroids', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19880, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'COPD',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Lung cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19888,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Oral cancer',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19788,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Blood cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19787, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Brain cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19174,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Postnatal depression',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19793, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pre-eclampsia', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 14512,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK4_female$modelDesign$targetId <- 21405
plpModelQRISK4_female$modelDesign$outcomeId <- 21397
plpModelQRISK4_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(18822,21395,18779,21387,21288,21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21344, 14949, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19380, 19888, 19788, 19787, 19174, 19793, 14512), 
    cohortName = c('SBP', 'SD SBP', 'Cholesterol/HDL', 'Deprivation', 'Ex-smoker', 'Light smoker', 'Moderate smoker', 'Heavy smoker', 'Indian', 'Pakistani', 'Bangladeshi', 'Other Asian', 'Caribbean', 'Black African', 'Chinese', 'Other ethnic group', 'Learning disability', 'Down syndrome', 'FH of CHD', 'Type I Diabetes', 'Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Renal failure', 'Migraine', 'Corticosteroids', 'Systemic lupus', 'Atypical antipsychotic', 'Severe mental illness', 'COPD', 'Lung cancer', 'Oral cancer', 'Blood cancer', 'Brain cancer', 'Postnatal depression', 'Pre-eclampsia')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK4_female_JJ_5,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK4_female_JJ_5, 
  dirPath = './inst/models/QRISK4_female_JJ_5'
    )

  

#QRISK1_male_JJ_10
#============================================================================
canalysisId <- 668
plpModelQRISK1_male_JJ_10 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395)*1000+canalysisId, 
    coefficient = c(50.634, 1.001, 1.022, 1.300, 1.417, 1.017, 1.004, 1.847, 0.993) 
  ), 
  intercept = 0, 
  mapping = QRISK1_male_10, 
  populationSettings = populationSettings10,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Body mass index', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of premature cardiovascular disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Antihypertensive agent', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SBPxantihypertensive agent interaction term', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
   ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = list(createLogAge(),createInteractionTermSBPAT())                  
)

plpModelQRISK1_male$modelDesign$targetId <- 21398                  
plpModelQRISK1_male$modelDesign$outcomeId <- 21397
plpModelQRISK1_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395),
    cohortName = c('Age','Cholesterol/HDL','Body mass index','Family history of premature cardiovascular disease','Current smoker','Townsend score', 'Systolic blood pressure', 'Antihypertensive agent','SBPxantihypertensive agent interaction term') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK1_male_JJ_10,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK1_male_JJ_10, 
  dirPath = './inst/models/QRISK1_male_JJ_10'
    )

#QRISK1_female_JJ_10
#============================================================================
canalysisId <- 668
plpModelQRISK1_female_JJ_10 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395)*1000+canalysisId, 
    coefficient = c(87.75, 1.001, 1.015, 1.229, 1.530, 1.035, 1.005, 1.734, 0.996) 
  ), 
  intercept = 0, 
  mapping = QRISK1_female_10, 
  populationSettings = populationSettings10,
    covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Body mass index', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of premature cardiovascular disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Antihypertensive agent', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SBPxantihypertensive agent interaction term', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = list(createLogAge(), createInteractionTermSBPAT())                            
)

plpModelQRISK1_female$modelDesign$targetId <- 21399
plpModelQRISK1_female$modelDesign$outcomeId <- 21397
plpModelQRISK1_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, 21395),
    cohortName = c('Age','Cholesterol/HDL','Body mass index','Family history of premature cardiovascular disease','Current smoker','Townsend score', 'Systolic blood pressure', 'Antihypertensive agent','SBPxantihypertensive agent interaction term')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK1_female_JJ_10,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK1_female_JJ_10, 
  dirPath = './inst/models/QRISK1_female_JJ_10'
    )

#QRISK2_male_JJ_10
#============================================================================
canalysisId <- 668
plpModelQRISK2_male_JJ_10 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 19882, 19884, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 19884)*1000+canalysisId, 
    coefficient = c(1.00, 1.45, 1.97, 1.67, 1.37, 0.62, 0.63, 0.51, 0.91, 1.59, 0.218, 0.236, 0.0595, 1.19, 2.14, 1.65, 1.68, 2.20, 1.38, 2.40, 1.75, 0.985, 0.1946, 0.0482, 0.923, 0.932, 0.916, 0.902, 0.893) 
  ), 
  intercept = 0, 
  mapping = QRISK2_male_10, 
  populationSettings = populationSettings10,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'White or not recorded', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22466,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'BMI', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexBMI interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexTownsend interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsystolicbloodpressure interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexfamilyhistory interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsmoking interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agextreatedhypertension interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgextypeIIdiabetes interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexatrial fibrillation interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createLogAge()
)

plpModelQRISK2_male$modelDesign$targetId <- 21400
plpModelQRISK2_male$modelDesign$outcomeId <- 21397
plpModelQRISK2_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 19882, 19884, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 19884)
    cohortName = c('White or not recorded','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Age','BMI','Townsend score','Systolic blood pressure','Cholesterol/HDL','Family history coronary heart disease','Current smoker','Treated hypertension','Type 2 diabetes','Rheumatoid arthritis','Atrial fibrillation','Renal disease','AgexBMI interaction','AgexTownsend interaction','Agexsystolicbloodpressure interaction','Agexfamilyhistory interaction','Agexsmoking interaction','Agextreatedhypertension interaction','Agextype2diabetes interaction', 'Agexatrialfibrillation interaction') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK2_male_JJ_10,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK2_male_JJ_10, 
  dirPath = './inst/models/QRISK2_male_JJ_10'
    )
  
#QRISK2_female_JJ_10
#============================================================================
canalysisId <- 668
plpModelQRISK2_female_JJ_10 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 19882, 19884, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 19884)*1000+canalysisId, 
    coefficient = c(1.00, 1.43, 1.80, 1.35, 1.15, 1.08, 0.58, 0.69, 1.04, 1.66, 0.216, 0.274, 0.06, 1.17, 1.99, 1.80, 1.54, 2.54, 1.50, 3.06, 1.70, 0.976, 0.1876, 0.0483, 0.927, 0.931, 0.952, 0.904, 0.858)
  ), 
  intercept = 0, 
  mapping = QRISK2_female_10,
  populationSettings = populationSettings10,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'White or not recorded', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 22466,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'BMI', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexBMI interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexTownsend interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsystolicbloodpressure interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexfamilyhistory interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsmoking interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agextreatedhypertension interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgextypeIIdiabetes interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexatrial fibrillation interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createLogAge()
)

plpModelQRISK2_female$modelDesign$targetId <- 21401
plpModelQRISK2_female$modelDesign$outcomeId <- 21397
plpModelQRISK2_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(22466, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 19882, 19884, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 19884),
    cohortName = c('White or not recorded', 'Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Age','BMI','Townsend score','Systolic blood pressure','Cholesterol/HDL','Family history coronary heart disease','Current smoker','Treated hypertension','Type 2 diabetes','Rheumatoid arthritis','Atrial fibrillation','Renal disease','AgexBMI interaction','AgexTownsend interaction','Agexsystolicbloodpressure interaction','Agexfamilyhistory interaction','Agexsmoking interaction','Agextreatedhypertension interaction','Agextype2diabetes interaction', 'Agexatrialfibrillation interaction')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK2_female_JJ_10,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK2_female_JJ_10, 
  dirPath = './inst/models/QRISK2_female_JJ_10'
    )

#QRISK3_male_JJ_10
#============================================================================

# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK3_male_JJ_10 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19887, 18779, 18822, 21395)*1000+canalysisId, 
    coefficient = c(0.236, 1.32, 1.61, 1.70, 1.04, 0.699, 0.670, 0.660, 0.769, 1.21, 1.74, 1.89, 2.20, 1.72, 3.44, 2.36, 1.68, 1.23, 2.42, 2.05, 1.29, 1.58, 1.55, 1.14, 1.13, 1.25, 1.19, 0.057, 0.111) 
  ), 
  intercept = 0, 
  mapping = QRISK3_male_10, 
  populationSettings = populationSettings10,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Former smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19893, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chronic kidney disease (stage 3,4 or 5)', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 12467,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroid use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus erythematosus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19880, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372,
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    , createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Erectile dysfunction or treatment', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19887,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD of blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP()                         
)

plpModelQRISK3_male$modelDesign$targetId <- 21402
plpModelQRISK3_male$modelDesign$outcomeId <- 21397
plpModelQRISK3_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19887, 18779, 18822, 21395),
    cohortName = c('Townsend score','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Former smoker', 'Light smoker','Moderate smoker','Heavy smoker','Family history of coronary heart disease','Type I Diabetes','Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Chronic kidney disease (stage 3,4 or 5)','Migraine', 'Corticosteroid use', 'Systemic lupus erythematosus', 'Atypical antipsychotic use', 'Severe mental illness', 'Erectile dysfunction or treatment', 'Cholesterol/HDL', 'Systolic blood pressure', 'SD of blood pressure') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK3_male_JJ_10,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK3_male_JJ_10, 
  dirPath = './inst/models/QRISK3_male_JJ_10'
    )
                  
#QRISK3_female_JJ_10
#============================================================================

# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK3_female_JJ_10 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291,18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 18779, 18822, 21395)*1000+canalysisId, 
    coefficient = c(0.294, 1.32, 1.76, 1.34, 1.08, 0.843, 0.675, 0.722, 0.843, 1.00, 1.14, 1.75, 1.95, 2.34, 1.58, 5.62, 2.91, 1.66, 1.24, 4.92, 1.92, 1.35, 1.81, 2.14, 1.29, 1.13, 1.17, 0.057, 0.108)
  ), 
  intercept = 0
  mapping = QRISK3_female_10, 
  populationSettings = populationSettings10,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Former smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19893,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chronic kidney disease (stage 3,4 or 5)', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 12467,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroid use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus erythematosus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19880, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Standard deviation of blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK3_female$modelDesign$targetId <- 21403
plpModelQRISK3_female$modelDesign$outcomeId <- 21397
plpModelQRISK3_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291,18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 18779, 18822, 21395),
    cohortName = c('Townsend score','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Former smoker','Light smoker','Moderate smoker','Heavy smoker','Family history of coronary heart disease','Type I Diabetes','Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Chronic kidney disease (stage 3,4 or 5)','Migraine', 'Corticosteroid use', 'Systemic lupus erythematosus', 'Atypical antipsychotic use', 'Severe mental illness', 'Cholesterol/HDL', 'Systolic blood pressure', 'SD of blood pressure') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK3_female_JJ_10,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK3_female_JJ_10, 
  dirPath = './inst/models/QRISK3_female_JJ_10'
    )



#QRISK4_male_JJ_10
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK4_male_JJ_10 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(18822,21395,18779,21387,21288, 21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21344, 14949, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19380, 19888, 19788, 19787, 19174, 19887)*1000+canalysisId, 
    coefficient = c(0.065, 1.14, 1.15, 0.222, 1.19, 2.00, 2.08, 2.60, 1.19, 1.47, 1.41, 1.04, 0.67, 0.66, 0.72, 0.81, 1.17, 2.35, 1.62, 3.28, 2.03, 2.20, 1.19, 2.59, 1.70, 1.41, 1.64, 1.68, 1.18, 1.37, 1.66, 1.49, 2.06, 5.45, 1.40)
  ), 
  intercept = 0, 
  mapping = QRISK4_male_10, 
  populationSettings = populationSettings10,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'SBP', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD SBP',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Deprivation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Ex-smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other ethnic group', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21344, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Down syndrome', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 14949, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'FH of CHD', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type 1 Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19893, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type 2 Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal failure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 12467,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroids', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19880, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'COPD',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19380,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Lung cancer',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19888,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Oral cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19788,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Blood cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19787, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Brain cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19174, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Erectile dysfunction',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19887,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK4_male$modelDesign$targetId <- 21404
plpModelQRISK4_male$modelDesign$outcomeId <- 21397
plpModelQRISK4_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(18822,21395,18779,21387,21288, 21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21344, 14949, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19380, 19888, 19788, 19787, 19174, 19887),
    cohortName = c('SBP', 'SD SBP', 'Cholesterol/HDL', 'Deprivation', 'Ex-smoker', 'Light smoker', 'Moderate smoker', 'Heavy smoker', 'Indian', 'Pakistani', 'Bangladeshi', 'Other Asian', 'Caribbean', 'Black African', 'Chinese', 'Other ethnic group', 'Learning disability', 'Down syndrome', 'FH of CHD', 'Type I Diabetes', 'Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Renal failure', 'Migraine', 'Corticosteroids', 'Systemic lupus', 'Atypical antipsychotics','Severe mental illness', 'COPD', 'Lung cancer', 'Oral cancer', 'Blood cancer', 'Brain cancer', 'Erectile dysfunction') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK4_male_JJ_10,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK4_male_JJ_10, 
  dirPath = './inst/models/QRISK4_male_JJ_10'
    )

  
#QRISK4_female_JJ_10
#============================================================================

# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK4_female_JJ_10 <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = c(18822,21395,18779,21387,21288,21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21344, 14949, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19380, 19888, 19788, 19787, 19174, 19793, 14512)*1000+canalysisId, 
    coefficient = c(0.0635, 1.16, 1.14, 0.252, 1.19, 2.12, 2.30, 2.90, 1.13, 1.56, 1.28, 0.98, 0.94, 0.76, 0.68, 0.94, 1.45, 3.18, 1.46, 4.52, 2.49, 2.20, 1.28, 4.50, 1.81, 1.46, 1.70, 2.22, 1.24, 1.22, 1.85, 3.50, 1.55, 2.13, 4.52, 1.18, 1.56) 
  ), 
  intercept = 0, 
  mapping = QRISK4_female_10, 
  populationSettings = populationSettings10,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'SBP', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD SBP',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21395,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL ratio', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Deprivation',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Ex-smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Caribbean',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other ethnic group',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21344,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Down syndrome',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 14949,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'FH of CHD', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19893,          
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19882,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19884,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal failure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 12467,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroids', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19880, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'COPD',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Lung cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19888,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Oral cancer',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19788,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Blood cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19787, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Brain cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19174,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Postnatal depression',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19793, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pre-eclampsia', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 14512,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ),
    chdlCovSetting,bmiCovSetting,bldpCovSetting
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK4_female$modelDesign$targetId <- 21405
plpModelQRISK4_female$modelDesign$outcomeId <- 21397
plpModelQRISK4_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(18822,21395,18779,21387,21288,21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21344, 14949, 18821, 19893, 18815, 19280, 19882, 19884, 21347, 12467, 21371, 19880, 21372, 21294, 19380, 19888, 19788, 19787, 19174, 19793, 14512), 
    cohortName = c('SBP', 'SD SBP', 'Cholesterol/HDL', 'Deprivation', 'Ex-smoker', 'Light smoker', 'Moderate smoker', 'Heavy smoker', 'Indian', 'Pakistani', 'Bangladeshi', 'Other Asian', 'Caribbean', 'Black African', 'Chinese', 'Other ethnic group', 'Learning disability', 'Down syndrome', 'FH of CHD', 'Type I Diabetes', 'Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Renal failure', 'Migraine', 'Corticosteroids', 'Systemic lupus', 'Atypical antipsychotic', 'Severe mental illness', 'COPD', 'Lung cancer', 'Oral cancer', 'Blood cancer', 'Brain cancer', 'Postnatal depression', 'Pre-eclampsia')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T,
    useDemographicsRace = T
    ),
  chdlCovSetting,bmiCovSetting,bldpCovSetting
)

# bug that needs fixing in PLP
attr(plpModelQRISK4_female_JJ_10,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK4_female_JJ_10, 
  dirPath = './inst/models/QRISK4_female_JJ_10'
    )
