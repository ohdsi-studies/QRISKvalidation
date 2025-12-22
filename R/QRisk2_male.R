#' Create qrisk 2 model for males using Qrisk cohorts
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
createQRISK2MaleOG10 <- function(
    cohortDatabaseSchema,
    cohortTableName = 'qrisk_cprd'
  ){
  
  # Male qrisk2
    
plpModelQRISK2_male_OG_10 <- PatientLevelPrediction::createGlmModel(
  targetId = 3, # the first cohort I create manually
  outcomeId = 21397, 
  restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(
    sampleSize = 10000 # sampling for testing
    ),# can use this to restrict dates as well
  coefficients = data.frame(
    covariateId = c(
        22466652, # White or not recorded               
        21377662, # Indian
        21378672, # Pakistani
        21379682, # Bangladeshi
        21380692, # Other Asian
        21381702, # Black Caribbean
        21382712, # Black African
        21383722, # Chinese
        21386732, # Other
        1020,  # Age
        2466, # BMI 
        4466, # Townsend score
        3466, # Systolic blood pressure 
        1466, # Cholesterol/HDL 
        18821668, # Family history coronary heart disease
        19285678, # Current smoker
        19280688, # Treated hypertension
        18815698, # Type 2 diabetes
        18838708, # Rheumatoid arthritis
        18841718, # Atrial fibrillation
        21347728, # Renal disease
        18778, # AgexBMI interaction             #TO DO 
        21387, # AgexTownsend interaction        #TO DO
        18822, # Agexsystolicbloodpressure interaction #TO DO
        18821651, # Agexfamilyhistory interaction
        19285661, # Agexsmoking interaction
        19280671, # Agextreatedhypertension interaction
        18815681, # Agextype2diabetes interaction
        18841691  # Agexatrialfibrillation interaction
    ), 
    coefficient = c(1.00, 1.45, 1.97, 1.67, 1.37, 0.62, 0.63, 0.51, 0.91, 1.59, 0.218, 0.236, 0.0595, 1.19, 2.14, 1.65, 1.68, 2.20, 1.38, 2.40, 1.75, 0.985, 0.1946, 0.0482, 0.923, 0.932, 0.916, 0.902, 0.893
                   )  
  ), 
  intercept = 0, 
  mapping = "function(x){ sapply(x, function(x){
baseline <- 0.10478258
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

    # white or not recorded 22466652
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 652, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(22466), 
        cohortName = c('White or not recorded')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),
      

    # indian 21377662
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 662, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(21377), 
        cohortName = c('Indian')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),

    #Pakistani 21378672
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 672, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(21378), 
        cohortName = c('Pakistani')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),

    #Bangladeshi 21379682
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 682, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(21379), 
        cohortName = c('Bangladeshi')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),
   
    #Other Asian 21380692
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 692, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(21380), 
        cohortName = c('Other Asian')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),
    
    #Black Caribbean 21381702
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 702, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(21381), 
        cohortName = c('Black Caribbean')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),

    #Black African 21382712
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 712, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(21382), 
        cohortName = c('Black African')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),

    #Chinese 21383722
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 722, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(21383), 
        cohortName = c('Chinese')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),

    #Other 21386732
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 732, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(21386), 
        cohortName = c('Other')
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),

    # Age x BMI Interaction

    # Age x Townsend Interaction

    # Age x SBP Interaction

    # Age x Family History of cardiovascular disease Interaction 18821651
    createCohortCovariateSettings(
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
      analysisId = 651),

    # Age x Smoking Interaction 19285661
    createCohortCovariateSettings(
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

    # Age x Treated Hypertension Interaction 19280671
     createCohortCovariateSettings(
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

    # Age x Type II DM Interaction 18815681
    createCohortCovariateSettings(
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
      analysisId = 681
    ),

    # Age x Atrial Fibrillation  Interaction 18841691
     createCohortCovariateSettings(
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
      analysisId = 691
    )
    
  )                 
)

return(plpModelQRISK2_male_OG_10)
}
