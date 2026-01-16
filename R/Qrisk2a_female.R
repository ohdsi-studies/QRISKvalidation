#' Create qrisk 2 model for males using Qrisk cohorts
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
createQRISK1FemaleOG10 <- function(
    cohortDatabaseSchema,
    cohortTableName = 'qrisk_cprd',
    sampleSize = 10000
  ){
  
  # Female qrisk2 2015
    
plpModelQRISK1_female_OG_10 <- PatientLevelPrediction::createGlmModel(
  targetId = 4, # the first cohort I create manually
  outcomeId = 21397, 
  restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(
    sampleSize = sampleSize # sampling for testing
    ),# can use this to restrict dates as well
  coefficients = data.frame(
    covariateId = c(
        4466, # Townsend
        
        8527004, # White or not recorded 
        38003614004, # white
        38003574004, # Indian
        38003589004, # Pakistani
        38003575004, # Bangladeshi
        8515004, # Other Asian
        38003590004, # Other Asian
        38003581004, # Other Asian 
        38003609004, # Black Caribbean
        38003600004, # Black African
        8516004, # Black African
        38003579004, # Chinese
        38003615004, # Other
        
        21287655, # Non smoker
        21288655, # Former smoker
        21289655, # Light smoker
        21290655, # Moderate smoker
        21291655, # Heavy smoker 
        
        18841668, # Atrial fibrillation
        18838668, # Rheumatoid arthritis
        21347668, # Renal disease
        19280688, # Treated hypertension
        18820668, # Type I Diabetes
        18815668, # Type II Diabetes
        18821668, # Family history of coronary heart disease
        
        1020, #age1
        2020, #age2
        1021, #bmi 1
        2021, #bmi 2
        1466, # Cholesterol/HDL
        3466, # Systolic blood pressure
        
        # age1 interaction
        21288655123, # Former smoker
        21289655123, # Light smoker
        21290655123, # Moderate smoker
        21291655123, # Heavy smoker 
        18841668123, # Atrial fibrillation
        21347668123, # Renal disease
        19280688123, # TODO- Treated hypertension
        18820668123, # Type I Diabetes
        18815668123, # Type II Diabetes
        1021123,  # bmi 1
        2021123,  # bmi 2
        18821668123, # Family history of coronary heart disease
        3466123, #sbp
        4466123, #townsend
        

        # age2 interaction
        21288655124, # Former smoker
        21289655124, # Light smoker
        21290655124, # Moderate smoker
        21291655124, # Heavy smoker 
        18841668124, # Atrial fibrillation
        21347668124, # Renal disease
        19280688124, # TODO- Treated hypertension
        18820668124, # Type I Diabetes
        18815668124, # Type II Diabetes
        1021124,  # bmi 1
        2021124,  # bmi 2
        18821668124, # Family history of coronary heart disease
        3466124, #sbp
        4466124 #townsend
    ), 
    coefficient = c(
      # townsend
      0.0664772630011438850000000,
      
      # race/ethnicity
      0, 0, 0.2574099349831925900000000, 0.6129795430571779400000000,
	  0.3362159841669621300000000, 0.1512517303224336400000000, 0.1512517303224336400000000,
	  0.1512517303224336400000000, -0.1794156259657768100000000,
	 -0.3503423610057745400000000, -0.3503423610057745400000000,
	 -0.2778372483233216800000000, -0.1592734122665366000000000,
      
      # smoker low to high
      0, 0.2119377108760385200000000, 0.6618634379685941500000000, 0.7570714587132305600000000,
	  0.9496298251457036000000000,
      
      # binary
      1.6284780236484424000000000, 0.2901233104088770700000000,
	  1.0043796680368302000000000, 0.6180430562788129500000000,
	  1.8400348250874599000000000, 1.1711626412196512000000000,
	  0.5147261203665195500000000,

      # measurements
      4.4417863976316578000000000, 0.0281637210672999180000000,
	  0.8942365304710663300000000, -6.5748047596104335000000000,
	  0.1433900561621420900000000, 0.0128971795843613720000000,
      
      # interactions - age 1
      0.7464406144391666500000000, 0.2568541711879666600000000,
	  -1.5452226707866523000000000, -1.7113013709043405000000000,
	  -7.0177986441269269000000000, -2.9684019256454390000000000,
	  -4.2219906452967848000000000, 1.6835769546040080000000000,
	  -2.9371798540034648000000000, 0.1797196207044682300000000,
	  40.2428166760658140000000000, 0.1439979240753906700000000,
	  -0.0362575233899774460000000, 0.3735138031433442600000000,
        
      # interactions - age 2
      -0.1927057741748231000000000, -0.1526965063458932700000000,
	   0.2313563976521429400000000, 0.2307165013868296700000000,
	   1.1395776028337732000000000, 0.4356963208330940600000000,
	   0.7265947108887239600000000, -0.6320977766275653900000000,
	   0.4023270434871086800000000, 0.1319276622711877700000000,
	  -7.3211322435546409000000000, -0.1330260018273720400000000,
	   0.0045842850495397955000000, -0.0952370300845990780000000
        
                   )  
  ), 
  intercept = 0, 
  mapping = "function(x){ sapply(x, function(x){
  (1 - (0.989747583866119)^exp(x) )
})}", 
  populationSettings = PatientLevelPrediction::createStudyPopulationSettings(
    requireTimeAtRisk = FALSE, 
    riskWindowStart = 1, 
    startAnchor = 'cohort start',
    riskWindowEnd = 3650, 
    endAnchor = 'cohort start'
  ),
  
  covariateSettings <- list(
    
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
    
    
    # QRISK 2 extras
    #------------
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
    
    
    # QRISK 3 extras
    # Type I diabetes 18820668
    # Migraine 19379668
    # Corticosteroid use 21371668
    # SLE 19164668
    # Atypical antipsychotic use 21372668
    # Severe mental illness 21294668
    # Erectile dysfunction or treatment 19165668
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 668, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(18820, 19379,
                     21371, 19164,
                     21372, 21294,
                     19165), 
        cohortName = c('Type I Diabetes', 'Migraine',
                       'Corticosteroids', 'SLE',
                       'Atypical antipsychotic use',
                       'Severe mental illness',
                       'Erectile dysfunction'
        )
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),
    
    # Non smoker 21287655
    # Former smoker 21288655
    # Light smoker 21289655
    # Moderate smoker 21290655
    # Heavy smoker 21291655
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 655, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(21287, 21288, 21289,
                     21290, 21291), 
        cohortName = c('Non smoker', 'Former smoker',
                       'Light smoker', 'Moderate smoker',
                       'Heavy smoker')
      ), 
      valueType = 'binary', 
      startDay = -365, 
      endDay = 0
    )
    
  ), 
  
  featureEngineering = list(
    
    # create the age 1 and 2
    QRISKvalidation::createMeasurementFe(
      covariateId = 1002, 
      analysisId = 20, 
      mappings = list(
        function(x){sapply(x, function(x) (x/10)^(0.5))},
        function(x){sapply(x, function(x) (x/10))}
      )
        ),
    
    # create the bmi 1 and 2
    QRISKvalidation::createMeasurementFe(
      covariateId = 2496, 
      analysisId = 21, 
      mappings = list(
        function(x){sapply(x, function(x) (x/10)^(-2))},
        function(x){sapply(x, function(x) ((x/10)^(-2))*log(x/10))}
      )
    ),
    
    # center all the non-binary
    QRISKvalidation::createCenteringFe(
      centers = data.frame(
        covariateId = c(
          1020, 2020, 
          1021, 2021,
          1466, 3466,
          4466
        ),
        centerValue = c(
          2.086397409439087, 4.353054523468018,
		  0.152244374155998, 0.143282383680344,
		  3.506655454635620, 125.040039062500000,
		  0.416743695735931
        )
      )),
    
    # interactions
    
    # interaction with age 1
    QRISKvalidation::createInteractionFe(
      interactionCovariateId = 1020, 
      covariateIdsOfInterest = c(
        21288655, # Former smoker
        21289655, # Light smoker
        21290655, # Moderate smoker
        21291655, # Heavy smoker 
        18841668, # Atrial fibrillation
        21347668, # Renal disease
        19280688, # TODO- Treated hypertension
        18820668, # Type I Diabetes
        18815668, # Type II Diabetes
        1021,  # bmi 1
        2021,  # bmi 2
        18821668,
        3466,
        4466
      ), 
      analysisId = 123
  ),
  
  # interaction with age 2
  QRISKvalidation::createInteractionFe(
    interactionCovariateId = 2020, 
    covariateIdsOfInterest = c(
      21288655, # Former smoker
      21289655, # Light smoker
      21290655, # Moderate smoker
      21291655, # Heavy smoker 
      18841668, # Atrial fibrillation
      21347668, # Renal disease
      19280688, # TODO- Treated hypertension
      18820668, # Type I Diabetes
      18815668, # Type II Diabetes
      1021,  # bmi 1
      2021,  # bmi 2
      18821668,
      3466,
      4466
    ), 
    analysisId = 124
  )
  )
    
)

return(plpModelQRISK1_female_OG_10)
}
