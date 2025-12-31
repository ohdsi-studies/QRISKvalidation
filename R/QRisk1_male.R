#' Create qrisk 1 model for males using Qrisk cohorts
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
createQRISK1MaleOG10 <- function(
    cohortDatabaseSchema,
    cohortTableName = 'qrisk_cprd',
    sampleSize = 10000
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
    sampleSize = sampleSize # sampling for testing
    ),# can use this to restrict dates as well
  coefficients = data.frame(
    covariateId = c(
      1020, # logAge id 
      1466, # chol ratio measurement
      2496, # BMI measurement
      18821668, # family history cohort cov - all prior
      19285678, # smoker cohort cov - 365 prior
      4466, # townsend measurement - need to make using conceptId 715996
      3466, # sys blood pressure
      19280688, # treatment blood pressure needs to be on index - 30 prior
      19280688070 # interaction for systolic blood pressure×blood pressure treatment
    ), 
    coefficient = c(log(50.634), 
                    log(1.001), log(1.022), log(1.300), log(1.417), log(1.017), log(1.004), log(1.847), log(0.993)
    ) 
  ), 
  intercept = 0, 
  mapping = "function(x){ sapply(x, function(x){
baselineSurv <- 1-0.1225593 # not correct but need a value for now
1-baselineSurv^exp(x)
})}", 
  populationSettings = PatientLevelPrediction::createStudyPopulationSettings(
    requireTimeAtRisk = FALSE, 
    riskWindowStart = 1, 
    startAnchor = 'cohort start',
    riskWindowEnd = 3650, 
    endAnchor = 'cohort start'
  ),
  
  
  covariateSettings = list(
    
    FeatureExtraction::createCovariateSettings(
      useDemographicsAge = TRUE, 
      useDemographicsEthnicity = TRUE,
      useDemographicsRace = TRUE
    ),
    
    # creates a Chol/HDL ratio covariate with id 1466/1486/1496
    
    QRISKvalidation::createMeasurementRatioCovariateSettings(
      covariateName = 'Chol/HDL ratio', 
      conceptSet1 = c(4260765), # first measurement
      conceptSet2 = c(4042059,4076704), # second measurement 
      unitSet1 = NULL, # may need to edit this if units can be different across network
      unitSet2 = NULL, # may need to edit this if units can be different across network
      startDay = -365*1, # last year
      endDay = 0, 
      minVal1 = 0,
      maxVal1 = 1000, 
      minVal2 =  0,
      maxVal2 = 250,
      aggregateMethod = 'recent',
      covariateId = 1466, 
      analysisId = 466
    ),
    
    # Chol/HDL specific ratio concept 
    QRISKvalidation::createMeasurementCovariateSettings(
      covariateName = 'Ratio', 
      conceptSet = c(4195214, 4042587,4195490,4198116,36314015,36314016,36314017,36314018,36361945,36361947,36361949,36361951),
      unitSet = NULL, 
      startDay = -365, # last year
      endDay = 0, 
      minVal = 0,
      maxVal = 1000, 
      aggregateMethod = 'recent',
      covariateId = 8466, 
      analysisId = 466
    ),
    

    QRISKvalidation::createMeasurementCovariateSettings(
      covariateName = 'BMI', 
      conceptSet = c(3038553),
      unitSet = NULL, 
      startDay = -365*5, # last 5 years
      endDay = 0, 
      minVal = 5,
      maxVal = 250, 
      aggregateMethod = 'recent',
      covariateId = 2496, 
      analysisId = 496
    ),
    
    # Systolic blood pressure covariateId 3466
    QRISKvalidation::createMeasurementCovariateSettings(
      covariateName = 'Systolic Blood Pressure', 
      conceptSet = c(3004249),
      unitSet = NULL, 
      startDay = -365*1, # last 1 years - check how long to look back
      endDay = 0, 
      minVal = 5,
      maxVal = 250, 
      aggregateMethod = 'recent',
      covariateId = 3466, 
      analysisId = 466
    ),
    
    # townsend measurement covariate 4466 - does not exist
    QRISKvalidation::createMeasurementCovariateSettings(
      covariateName = 'Townsend', 
      conceptSet = c(715996),
      unitSet = NULL, 
      startDay = -9999, 
      endDay = 0, 
      minVal = NULL,
      maxVal = NULL, 
      aggregateMethod = 'recent',
      covariateId = 4466, 
      analysisId = 466
    ),
    
    
    # binary
    #============
    
    # family history of CVD covariate 18821668 - QR1/2
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
    )
    
  ),
    # interaction for treatment and SBP - covariate 19280768 using FE
featureEngineering = list(
  # log age/10
  QRISKvalidation::createMeasurementFe(
    covariateId = 1002,
    analysisId = 20,
    mappings = list(
      function(x){sapply(x, function(x) log(x/10))}
    )  
  ),
    QRISKvalidation::createCenteringFe(centers = data.frame(
      covariateId = c(1020, 1466, 2496, 3466),
      centerValue = c(0, 4, 26, 132.6)
    )),
    QRISKvalidation::createInteractionFe(
    interactionCovariateId = 3466, 
    covariateIdsOfInterest = 19280688,
    analysisId = 70
    )
  )                 
)

return(plpModelQRISK1_male_OG_10)
}


