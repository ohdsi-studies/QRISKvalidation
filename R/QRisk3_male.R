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
        22466652, # White or not recorded               
        21377662, # Indian
        21378672, # Pakistani
        21379682, # Bangladeshi
        21380692, # Other Asian
        21381702, # Black Caribbean
        21382712, # Black African
        21383722, # Chinese
        21386732, # Other                          
        21287655, # Non smoker
        21288665, # Former smoker
        21289675, # Light smoker
        21290685, # Moderate smoker
        21291695, # Heavy smoker                    
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
        19165717, # Erectile dysfunction or treatment
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

     # white or not recorded
    22466, # White or not recorded

    # indian
    21377, # Indian

    #Pakistani
    21378, # Pakistani

    #Bangladeshi
    21379, # Bangladeshi

    #Other Asian
    21380, # Other Asian

    #Black Caribbean
    21381, # Black Caribbean

    #Black African
    21382, # Black African

    #Chinese
    21383, # Chinese

    #Other
    21386, # Other

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

    #Non smoker 21287655
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 655, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(21287), 
        cohortName = c('Non smoker')
      ), 
      valueType = 'binary', 
      startDay = -365, 
      endDay = 0
    ),

    #Former smoker 21288665
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 665, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(21288), 
        cohortName = c('Former smoker')
      ), 
      valueType = 'binary', 
      startDay = -365, 
      endDay = 0
    ),

    #Light smoker 21289675
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 675, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(21289), 
        cohortName = c('Light smoker')
      ), 
      valueType = 'binary', 
      startDay = -365, 
      endDay = 0
    ),

    #Moderate smoker 21290685
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 685, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(21290), 
        cohortName = c('Moderate smoker')
      ), 
      valueType = 'binary', 
      startDay = -365, 
      endDay = 0
    ),

    #Heavy smoker 21291695
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 695, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(21291), 
        cohortName = c('Heavy smoker')
      ), 
      valueType = 'binary', 
      startDay = -365, 
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
  )                 
)

return(plpModelQRISK3_male_OG_10)
}
