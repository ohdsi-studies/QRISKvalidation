#' Create qrisk 3 model for males using Qrisk cohorts
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
createQRISK3MaleOG10 <- function(
    cohortDatabaseSchema,
    cohortTableName = 'qrisk_cprd'
  ){
  
  # Male qrisk3
    
plpModelQRISK3_male_OG_10 <- PatientLevelPrediction::createGlmModel(
  targetId = 5, # the first cohort I create manually
  outcomeId = 21397, 
  restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(
    sampleSize = 10000 # sampling for testing
    ),# can use this to restrict dates as well
  coefficients = data.frame(
    covariateId = c(
        4466, # Townsend
        22466, # White or not recorded
        21377, # Indian
        21378, # Pakistani
        21379, # Bangladeshi
        21380, # Other Asian
        21381, # Black Caribbean
        21382, # Black African
        21383, # Chinese
        21386, # Other
        21287, # Non smoker
        21288, # Former smoker
        21289, # Light smoker
        21290, # Moderate smoker
        21291, # Heavy smoker
        18821668, # Family history of coronary heart disease
        18820, # Type I Diabetes
        18815, # Type II Diabetes
        19280688, #Treated hypertension
        18838, # Rheumatoid arthritis
        18841, # Atrial fibrillation
        21347, # Renal disease
        19379, # Migraine
        21371, # Corticosteroid use
        19164, # Systemic lupus erythematosus
        21372, # Atypical antipsychotic use
        21294, # Severe mental illness
        19165, # Erectile dysfunction or treatment
        1466, # Cholesterol/HDL
        3466, # Systolic blood pressure
        21395 # SD of blood pressure                                                           #TO DO
    ), 
    coefficient = c(0.236, 1.00, 1.32, 1.61, 1.70, 1.04, 0.699, 0.670, 0.660, 0.769, 1.00, 1.21, 1.74, 1.89, 2.20, 1.72, 3.44, 2.36, 1.68, 1.23, 2.42, 2.05, 1.29, 1.58, 1.55, 1.14, 1.13, 1.25, 1.19, 0.057, 0.111
                   )  
  ), 
  intercept = 0, 
  mapping = "function(x){ sapply(x, function(x){
baseline <- 0.03956830
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
    # creates a Chol/HDL ratio covariate with id 1466
    QRISKvalidation::createMeasurementCovariateSettings(
      covariateName = 'Chol/HDL ratio', 
      conceptSet = c(4195214, 4042587,4195490,4198116,36314015,36314016,36314017,36314018,36361945,36361947,36361949,36361951),
      unitSet = c(NA, 0, 8523),
      startDay = -365, 
      endDay = 0, 
      scaleMap = function(covariates) {
        covariates$valueAsNumber <- sapply(covariates$valueAsNumber, function(y){y - 4})
        return(covariates)
      }, 
      minVal = 0,
      maxVal = 20,
      aggregateMethod = 'recent',
      covariateId = 1466, 
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
      analysisId = 678, 
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
    
  )                 
)

return(plpModelQRISK3_male_OG_10)
}
