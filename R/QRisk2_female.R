#' Create qrisk 2 model for females using Qrisk cohorts
#'
#' @details
#' The user specifies 
#'
#' @param cohortDatabaseSchema the schema where the cohorts are generated into
#' @param cohortTableName the cohort table name
#' @param sampleSize the sample size to take of the target cohort if it is too large
#'
#' @return
#' An plpModel
#' 
#' @export
createQRISK2FemaleOG10 <- function(
    cohortDatabaseSchema,
    cohortTableName = 'qrisk_cprd',
    sampleSize = 10000
  ){
  
  # Female qrisk2
    
plpModelQRISK2_female_OG_10 <- PatientLevelPrediction::createGlmModel(
  targetId = 3, # the first cohort I create manually
  outcomeId = 21397, 
  restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(
    sampleSize = sampleSize # sampling for testing
    ),# can use this to restrict dates as well
  coefficients = data.frame(
    covariateId = c(
        22466652, # White or not recorded               
        21377652, # Indian
        21378652, # Pakistani
        21379652, # Bangladeshi
        21380652, # Other Asian
        21381652, # Black Caribbean
        21382652, # Black African
        21383652, # Chinese
        21386652, # Other
        1020,  # Age
        2466, # BMI 
        4466, # Townsend score
        3466, # Systolic blood pressure 
        1486, # Cholesterol/HDL 
        18821668, # Family history coronary heart disease
        19285678, # Current smoker
        19280688, # Treated hypertension
        18815698, # Type 2 diabetes
        18838698, # Rheumatoid arthritis
        18841698, # Atrial fibrillation
        21347698, # Renal disease
        1333, # AgexBMI interaction             #TO DO 
        0, # AgexTownsend interaction        #TO DO
        2333, # Agexsystolicbloodpressure interaction #TO DO
        1882101651, # Agexfamilyhistory interaction
        1928501661, # Agexsmoking interaction
        1928001671, # Agextreatedhypertension interaction
        1881501651, # Agextype2diabetes interaction
        1884101651  # Agexatrialfibrillation interaction
    ), 
    coefficient = c(1.00, 1.43, 1.80, 1.35, 1.15, 1.08, 0.58, 0.69, 1.04, 1.66, 0.216, 0.274, 0.06, 1.17, 1.99, 1.80, 1.54, 2.54, 1.50, 3.06, 1.70, 0.976, 0.1876, 0.0483, 0.927, 0.931, 0.952, 0.904, 0.858)
                  )
  ), 
  intercept = 0, 
  mapping = "function(x){ sapply(x, function(x){
baseline <- 0.06398843
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
    QRISKvalidation::createMeasurementRatioCovariateSettings(
      covariateName = 'Chol/HDL ratio', 
      conceptSet1 = c(4260765), # first measurement
      conceptSet2 = c(4042059,4076704), # second measurement 
      unitSet1 = NULL, # may need to edit this if units can be different across network
      unitSet2 = NULL, # may need to edit this if units can be different across network
      startDay = -365*3, # last three years - TODO edit this
      endDay = 0, 
      centeringMap = function(covariates){
        covariates$covariateValue <- sapply(covariates$covariateValue, function(y){y - 4})
        return(covariates)
      }, 
      minVal1 = 0,
      maxVal1 = 1000, 
      minVal2 =  0,
      maxVal2 = 250, 
      aggregateMethod = 'recent',
      covariateId = 1486, 
      analysisId = 486
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
    # rheumatoid arthritis 18838698
    # atrial fibrillation 18841698
    # renal disease 21347698
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 698, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(18815,18838, 18841, 21347), 
        cohortName = c('Type 2 diabetes',
                       'Rheumatoid arthritis',
                       'Atrial fibrillation',
                       'Renal disease')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),
   

    # white or not recorded 22466652
    # indian 21377652
    # Pakistani 21378652
    # Bangladeshi 21379652
    # Other Asian 21380652
    # Black Caribbean 21381652
    # Black African 21382652
    # Chinese 21383652
    # Other 21386652
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 652, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(22466, 21377, 21378,
                     21379, 21380, 21381,
                     21382, 21383, 21386), 
        cohortName = c('White or not recorded',
                       'Indian', 'Pakistani',
                       'Bangladeshi',
                       'Other Asian',
                       'Black Caribbean',
                       'Black African',
                       'Chinese',
                       'Other')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),

    # Age x BMI Interaction
    QRISKvalidation::createMeasurementCovariateSettings(
      covariateName = 'Age x BMI Interaction', 
      conceptSet = c(3038553),
      unitSet = NULL, 
      startDay = -365, 
      endDay = 0, 
      scaleMap = function(covariates){
        covariates$valueAsNumber <- sapply(covariates$valueAsNumber, function(y){y - 26})
        return(covariates)
      }, 
      ageInteract = TRUE, # NOTE: this is not log - does it need to be?
      minVal = 5,
      maxVal = 250, 
      aggregateMethod = 'recent',
      covariateId = 1333, 
      analysisId = 333
      ),
    # Age x Townsend Interaction

    # Age x SBP Interaction
    QRISKvalidation::createMeasurementCovariateSettings(
      covariateName = 'Age x SBP Interaction', 
      conceptSet = c(3004249),
      unitSet = NULL, 
      startDay = -365*1, # last 1 years - check how long to look back
      endDay = 0, 
      scaleMap = function(covariates){
        covariates$valueAsNumber <- sapply(covariates$valueAsNumber, function(y){y - 132.6})
        return(covariates)
      }, 
      ageInteract = TRUE, # NOTE: this is not log - does it need to be?
      minVal = 5,
      maxVal = 250, 
      aggregateMethod = 'recent',
      covariateId = 2333, 
      analysisId = 333
    ),

    # Age x Family History of cardiovascular disease Interaction 1882101651
    PatientLevelPrediction::createCohortCovariateSettings(
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
      analysisId = 651
      ),

    # Age x Smoking Interaction 1928510661
    PatientLevelPrediction::createCohortCovariateSettings(
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
      analysisId = 661
    ),

    # Age x Treated Hypertension Interaction 1928001671
    PatientLevelPrediction::createCohortCovariateSettings(
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
      analysisId = 671
    ),

    # Age x Type II DM Interaction 1881501651
    PatientLevelPrediction::createCohortCovariateSettings(
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
      analysisId = 651
    ),

    # Age x Atrial Fibrillation  Interaction 1884101651
    PatientLevelPrediction::createCohortCovariateSettings(
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
      analysisId = 651
    )
    
  )                 
)

return(plpModelQRISK2_female_OG_10)
}
