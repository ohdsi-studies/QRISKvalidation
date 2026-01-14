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
  
  # Female qrisk2 2017
    
plpModelQRISK2_female_OG_10 <- PatientLevelPrediction::createGlmModel(
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
      0.0341796842507185380000000,
      
      # race/ethnicity
      0, 0, 0.2717593862650211100000000, 0.4799065031659037700000000,0.5300105698156540900000000,
	  0.0331145170237558750000000, 0.0331145170237558750000000, 0.0331145170237558750000000,
	 -0.3573412299288998800000000, -0.3994896694326299200000000, -0.3994896694326299200000000,
	 -0.4279733002077463800000000, -0.2608790157463588000000000,
      
      # smoker low to high
      0, 0.1954144000852508200000000, 0.5610054208888730600000000, 0.6462513368790358000000000,
	  0.7995079430716418600000000,
      
      # binary
      0.9004854694526234200000000, 0.2616323573143903300000000, 0.8726038104301818700000000,
      0.5646653750961342400000000, 1.2783149408699921000000000, 0.8856927378645980100000000,
      0.5466400053697254600000000,
      
      # measurements
      -17.9632288727087150000000000, 0.0023003210703012446000000, 2.6004176854867018000000000,
      -8.7874836873828883000000000, 0.1723937893559204000000000, 0.0131735419568876150000000,
      
      # interactions - age 1
      -0.2470676570445800600000000, 0.7559948015362365500000000, 0.9987976238739433300000000,
      2.1469893873101267000000000, 3.3492802912954538000000000, 0.3375011451871609600000000,
	  6.4787444211188108000000000, 4.99748065882827410000000, 3.5070343218130682000000000, 
      31.8460902846817220000000000, -113.9663633551666100000000000, 2.8569050076828209000000000,
	  0.0180195333921339130000000, -0.1093273800862979300000000,
        
      # interactions - age 2
      -0.0005038867775553299600000, -0.0008074429991273904300000, -0.0008498950338905568400000,
	  -0.0007932224366769850500000, -0.0004072611661344105000000, -0.0015779977786932139000000,
	   0.0005649716745566484400000, 0.0004461201910180285300000, -0.0003467151069882253400000,
	   0.0051641134542654149000000, -0.0129594059405700540000000,-0.0002289189683731372600000,
	  -0.0000129657994760069200000, -0.0000978873336172942920000
        
                   )  
  ), 
  intercept = 0, 
  mapping = "function(x){ sapply(x, function(x){
  (1 - (0.988349735736847)^exp(x) )
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
        function(x){sapply(x, function(x) (x/10)^(-1))},
        function(x){sapply(x, function(x) (x/10)^3)}
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
          0.234766781330109, 77.284080505371094,
		  0.149176135659218, 0.141913309693336,
		  4.300998687744141, 128.571578979492190,
		  0.526304900646210
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

return(plpModelQRISK2_female_OG_10)
}
