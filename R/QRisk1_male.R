#' Create qrisk 1 model for males usign Qrisk cohorts
#'
#' @details
#' The user specifies 
#'
#' @param cohortDatabaseSchema the schema where the cohorts are generated into
#' @param cohortTableName the cohort table name
#'
#' @return
#' An plpModel
#' 
#' @export
createQRISK1MaleOG10 <- function(
    cohortDatabaseSchema,
    cohortTableName = 'qrisk_cprd'
  ){
  
  # Male qrisk
  # 50.634* log(age/10)
  # 1.001*Ratio of total serum cholesterol to high density lipoprotein cholesterol levels
  # 1.022*BMI
  # 1.300*Family history of premature cardiovascular disease
  # 1.417*smoking
  # 1.017*townsend
  # 1.004*sys blood pressure
  # 1.847* treatment blood pressure at index
  # 0.993*Interaction terms for systolic blood pressure×blood pressure treatment
  
plpModelQRISK1_male_OG_10 <- PatientLevelPrediction::createGlmModel(
  targetId = 1, # the first cohort I create manually
  outcomeId = 21397, 
  restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(
    sampleSize = 10000 # sampling for testing
    ),# can use this to restrict dates as well
  coefficients = data.frame(
    covariateId = c(
      1020, # logAge id 
      1466, # chol ratio measurement
      2466, # BMI measurement
      18821668, # family history cohort cov - all prior
      19285678, # smoker cohort cov - 365 prior
      4466, # townsend measurement - need to make using conceptId 715996
      3466, # sys blood pressure
      19280688, # treatment blood pressure needs to be on index - 30 prior
      19280768 # interaction for systolic blood pressure×blood pressure treatment
    ), 
    coefficient = c(50.634, 
                    1.001, 1.022, 1.300, 1.417, 1.017, 1.004, 1.847, 0.993
    ) 
  ), 
  intercept = 0, 
  mapping = "function(x){ sapply(x, function(x){
baseline <- 0.0009293994
baseline*exp(x)
})}", 
  populationSettings = PatientLevelPrediction::createStudyPopulationSettings(
    requireTimeAtRisk = FALSE, 
    riskWindowStart = 1, 
    startAnchor = 'cohort start',
    riskWindowEnd = 3650, 
    endAnchor = 'cohort start'
  ),
  covariateSettings = list(
    # creates a log(age/10) covariate with id 1020
    QRISKvalidation::createLogAgeCovariateSettings(ageMultiply = 1/10),
    
    # creates a Chol/HDL ratio covariate with id 1466
    QRISKvalidation::createMeasurementCovariateSettings(
      covariateName = 'Chol/HDL ratio', 
      conceptSet = c(4195214, 4042587,4195490,4198116,36314015,36314016,36314017,36314018,36361945,36361947,36361949,36361951),
      unitSet = c(NA, 0, 8523),
      startDay = -365, 
      endDay = 0, 
      scaleMap = function(covariates){
        covariates$valueAsNumber <- sapply(covariates$valueAsNumber, function(y){y - 4})
        return(covariates)
      }, 
      minVal = 0,
      maxVal = 20,
      aggregateMethod = 'recent',
      covariateId = 1466, 
      analysisId = 466
    ),
    
    # BMI cov with id 2466
    QRISKvalidation::createMeasurementCovariateSettings(
      covariateName = 'BMI', 
      conceptSet = c(3038553),
      unitSet = NULL, 
      startDay = -365, 
      endDay = 0, 
      scaleMap = function(covariates){
        covariates$valueAsNumber <- sapply(covariates$valueAsNumber, function(y){y - 26})
        return(covariates)
      }, 
      minVal = 5,
      maxVal = 250, 
      aggregateMethod = 'recent',
      covariateId = 2466, 
      analysisId = 466
    ),
    
    # family history of CVD covariate 18821668
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 668, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(18821), 
        cohortName = c('Family history of premature cardiovascular disease')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),
    
    # current smoker - covariateId 19285678
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 668, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(19285), 
        cohortName = c('Current smoker')
      ), 
      valueType = 'binary', 
      startDay = -365, 
      endDay = 0
    ),
    
    # townsend measurement covariate 4466
    QRISKvalidation::createMeasurementCovariateSettings(
      covariateName = 'Townsend', 
      conceptSet = c(715996),
      unitSet = NULL, 
      startDay = -9999, 
      endDay = 0, 
      scaleMap = function(covariates){
        covariates$valueAsNumber <- sapply(covariates$valueAsNumber, function(y){y - 0})
        return(covariates)
      }, 
      minVal = NULL,
      maxVal = NULL, 
      aggregateMethod = 'recent',
      covariateId = 4466, 
      analysisId = 466
    ),
    
    # Systolic blood pressure covariateId 3466
    QRISKvalidation::createMeasurementCovariateSettings(
      covariateName = 'Systolic Blood Pressure', 
      conceptSet = c(3004249),
      unitSet = c(NA, 0, 8876), 
      startDay = -365, 
      endDay = 0, 
      scaleMap = function(covariates){
        covariates$valueAsNumber <- sapply(covariates$valueAsNumber, function(y){y - 132.6})
        return(covariates)
      }, 
      minVal = 5,
      maxVal = 250, 
      aggregateMethod = 'recent',
      covariateId = 3466, 
      analysisId = 466
    ),
    
    # treatment for blood pressure 19280688
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 688, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(19280), 
        cohortName = c('Antihypertensive agent')
      ), 
      valueType = 'binary', 
      startDay = -30, 
      endDay = 0
    ),
    
    # interaction for treatment and SBP - covariate 19280768
    QRISKvalidation::createCohortMeasurementCovariateSettings(
      covariateName = 'SBPxantihypertensive agent interaction term', 
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280, 
      cohortStartDay = -30,
      cohortEndDay = 0,
      measurementConceptSet = c(3004249),
      measurementUnitSet = c(NA, 0, 8876), 
      measurementStartDay = -365, 
      measurementEndDay = 0, 
      measurementScaleMap = function(covariates){
        covariates$valueAsNumber <- sapply(covariates$valueAsNumber, function(y){y - 132.6})
        return(covariates)
      }, 
      measurementMinVal = 5,
      measurementMaxVal = 250, 
      measurementAggregateMethod = 'recent',
      covariateId = 19280768,
      analysisId = 768
    )
  )                 
)

return(plpModelQRISK1_male_OG_10)
}


