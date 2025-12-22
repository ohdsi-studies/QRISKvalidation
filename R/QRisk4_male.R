#' Create qrisk 4 model for males using Qrisk cohorts
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
createQRISK4MaleOG10 <- function(
    cohortDatabaseSchema,
    cohortTableName = 'qrisk_cprd'
  ){
  
  # Male qrisk4
    
plpModelQRISK4_male_OG_10 <- PatientLevelPrediction::createGlmModel(
  targetId = 7, # the first cohort I create manually
  outcomeId = 21397, 
  restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(
    sampleSize = 10000 # sampling for testing
    ),# can use this to restrict dates as well
  coefficients = data.frame(
    covariateId = c(
        3466, # Systolic blood pressure
        21395 # SD of blood pressure                     # TO DO
        1466, # Cholesterol/HDL
        4466, # Deprivation (Townsend)
        21288, # Former smoker
        21289, # Light smoker
        21290, # Moderate smoker
        21291, # Heavy smoker
        22466, # White or not recorded
        21377, # Indian
        21378, # Pakistani
        21379, # Bangladeshi
        21380, # Other Asian
        21381, # Black Caribbean
        21382, # Black African
        21383, # Chinese
        21386, # Other
        22465, # No learning disability
        21344, # Learning disability
        19881, # Down syndrome
        18821668, # Family history of coronary heart disease
        18820657, # Type I Diabetes
        18815698, # Type II Diabetes
        19280688, #Treated hypertension
        18838708, # Rheumatoid arthritis
        18841718, # Atrial fibrillation
        21347728, # Renal disease
        19379667, # Migraine
        21371677, # Corticosteroid use
        19164687, # Systemic lupus erythematosus
        21372697, # Atypical antipsychotic use
        21294707, # Severe mental illness
        19380, # COPD
        19792, # Lung cancer
        19788, # Oral cancer
        19787, # Blood cancer
        19174, # Brain cancer
        19165717, # Erectile dysfunction or treatment
    ),       
    coefficient = c(0.065, 1.14, 1.15, 0.222, 1.19, 2.00, 2.08, 2.60, 1.00, 1.19, 1.47, 1.41, 1.04, 0.67, 0.66, 0.72, 0.81, 1.00, 1.17, 2.35, 1.62, 3.28, 2.03, 2.20, 1.19, 2.59, 1.70, 1.41, 1.64, 1.68, 1.18, 1.37, 1.66, 1.49, 2.06, 5.45, 1.40
                   )
  ), 
  intercept = 0, 
  mapping = "function(x){ sapply(x, function(x){
baseline <- 0.03550895
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

       # type 2 diabetes 18815698
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 698, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(18815), 
        cohortName = c('Type 2 diabetes')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),
    
    # rheumatoid arthritis 18838708
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 708, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(18838), 
        cohortName = c('Rheumatoid arthritis')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),
   
    # atrial fibrillation 18841718
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 718, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(18841), 
        cohortName = c('Atrial fibrillation')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),
   
    # renal disease 21347728
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 728, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(21347), 
        cohortName = c('Renal disease')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),

   #Type I diabetes 18820657
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 657, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(18820), 
        cohortName = c('Type I Diabetes')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),
      
    #Migraine 19379667
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 667, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(19379), 
        cohortName = c('Migraine')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),

    #Corticosteroid use 21371677
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 677, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(21371), 
        cohortName = c('Corticosteroids')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),

    #SLE 19164687
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 687, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(19164), 
        cohortName = c('SLE')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),

    #Atypical antipsychotic use 21372697
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 697, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(21372), 
        cohortName = c('Atypical antipsychotic use')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),

    #Severe mental illness 21294707
      FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 707, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(21294), 
        cohortName = c('Severe mental illness')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),

    #Erectile dysfunction or treatment 19165717
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 717, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(19164), 
        cohortName = c('Erectile dysfunction')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),

    #No learning disability 22465654
    22465, # No learning disability

    #Learning disability 21344664
    21344, # Learning disability

    #Down syndrome 19881674
    19881, # Down syndrome

    #COPD 19380684
    19380, # COPD

    #Lung cancer 19792694
    19792, # Lung cancer

    #Oral cancer 19788704
    19788, # Oral cancer

    #Blood cancer 19787714
    19787, # Blood cancer

    #Brain cancer 19174724
    19174, # Brain cancer
  )                 
)

return(plpModelQRISK4_male_OG_10)
}
