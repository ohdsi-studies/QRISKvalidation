#' Create qrisk 4 model for females using Qrisk cohorts
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
createQRISK4FemaleOG10 <- function(
    cohortDatabaseSchema,
    cohortTableName = 'qrisk_cprd',
    sampleSize = 10000
  ){
  
  # Female qrisk4
    
plpModelQRISK4_female_OG_10 <- PatientLevelPrediction::createGlmModel(
  targetId = 8, # the first cohort I create manually
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

        22465668, # No learning disability
        21344668, # Learning disability 
        19881668, # Down syndrome
        
        21287655, # Non smoker
        21288655, # Former smoker
        21289655, # Light smoker
        21290655, # Moderate smoker
        21291655, # Heavy smoker 
        
        18841668, # Atrial fibrillation
        21372668, #Atypical antipsychotic use
        19787668, # Blood cancer
        19174668, # Brain cancer
        19380668, # COPD
        21371668, # Corticosteroid use
        19792668, # Lung cancer
        19379668, # Migraine
        19788668, # Oral cancer
        19793668, # Post natal depression
        14512668, # Pre eclampsia
        18838668, # Rheumatoid arthritis
        21347668, # Renal disease
        21294668, # Severe mental illness
        19164668, # Systemic lupus erythematosus
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
        3495, # SD of blood pressure 5-year
        
        # age1 interaction
        21288655123, # Former smoker
        21289655123, # Light smoker
        21290655123, # Moderate smoker
        21291655123, # Heavy smoker 
        18841668123, # Atrial fibrillation
        19787668123, #Blood cancer
        19174668123, #Brain cancer
        #21372668123, # Atypical antipsychotic use
        19380668123, #COPD
        21371668123, # Corticosteroid use
        19792668123, #Lung cancer
        19379668123, # Migraine
        14512668123, #Preeclampsia
        #18838668, # Rheumatoid arthritis
        21347668123, # Renal disease
        #21294668, # Severe mental illness
        19164668, # Systemic lupus erythematosus
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
        19787668124, #Blood cancer
        19174668124, #Brain cancer
        #21372668124, # Atypical antipsychotic use
        19380668124,  #COPD
        21371668124, # Corticosteroid use
        19792668124, #Lung cancer
        19379668124, # Migraine
        14512668124, #Preeclampsia
        #18838668, # Rheumatoid arthritis
        21347668124, # Renal disease
        #21294668, # Severe mental illness
        19164668, # Systemic lupus erythematosus
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
      0.0454453912085431635525801,
      
      # race/ethnicity
     0, 0, 0.1260967017814430268796144, 0.4447705239699963342125955, 0.2477444278286677903366808,
    -0.0157243672799900063852263, -0.0157243672799900063852263, -0.0157243672799900063852263,
    -0.0577696651849263498323950, -0.2697651695209241085038343, -0.2697651695209241085038343,
    -0.3832410026548082604413992, -0.0580485370353437737933611,
        
      #Learning categories
      0, 0.3725963227883137274254466, 1.1580969512183603153943068,
      
      # smoker low to high
       0, 0.1769493814113501928275696, 0.7530771206033387565881299,
       0.8323776592964649001515909, 1.0631500558832056579916525,
      
      # binary
      1.5049638006326653272282101, 0.2171747082630743896913827, 0.7579813938209226664710627,
      1.5084042727648279136332121, 0.6165461854848822698116351, 0.5335361445534146929148278,
      1.2528769976931071195025424, 0.3751930214750018310887469, 0.4388380908140788183580128,
      0.1678231201272606942787036, 0.4424110333722044430260212, 0.2497516452859188107460398,
      0.5935115176663330327500034, 0.1985625969486422004450787, 0.7966517501419697611098059,
      0.7863375922041283550001367, 1.5077632920269206096008929, 0.9135733041828619782620535,
      0.3793174445100093161720167,
      
      # measurements
      -11.3740979687460033176193974, 0.0038622551690345647591729, 2.7115897815278255933435503,
      -10.1872545425824796438973863, 0.1319686023801584506820461, 0.0118797181927175182075684,
       0.0151380159464541211300492,
      
      # interactions - age 1
      
     -1.3891809751673183104259124,
     -2.3750307370655678873561101,
     -1.2954796918429318797194583,
     -0.3825287654812418725036594,
      6.0624462289999527797590417,
      2.7361639268389335022391151,
      8.9648389569604827187276896,
      1.4863115018524808430555595,
      0.7032837685194606169858389,
      -2.1730551280439018846379895,
      1.0942124072326970818380687,
      4.5377644750987675692499579,
      2.8561257561763402357257746,
      1.8724878621202134532097716,
      5.3049020735756862165999337,
      3.4226030970198060288112174,
      3.5347828664374265628111971,
      13.8552935620458157472967287,
      -58.4511151699750826082890853,
      0.7833661297441812942921047,
      -0.0276950405975453228801797,
      -0.1088724922904488190411598,
      
      # interactions - age 2
      -0.0006714031962156862725147,
      -0.0018289163439225532888782,
      -0.0016084669138282403112739,
      -0.0015598697717469156912018,
      -0.0000695060798473468282460,
      -0.0003271540469791135979233,
       0.0014783423889687652939828,
      -0.0004133691848084355556676,
      -0.0007068419785311776809555,
      -0.0031462898560263505287615,
      -0.0001193563028629556757240,
       0.0008784258401504376704422,
      -0.0001305210904347752886700,
      -0.0004398014653144450419875,
       0.0003494359879836227470405,
       0.0001360404616328204354878,
       0.0000123261909873032668634,
       0.0038248210244411426361744,
      -0.0050416212930483586193708,
      -0.0004558405723194490478077,
       0.0000284310696238052556086,
      -0.0001338329490869009366291
                   )  
  ), 
  intercept = 0,                                                  #STILL LOOK AT MAPPING
  mapping = "function(x){ sapply(x, function(x){
  (1 - (0.977268040180206)^exp(x) )
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
    
    
    QRISKvalidation::createMeasurementCovariateSettings(
      covariateName = 'Systolic Blood Pressure stdev 5-years', 
      conceptSet = c(3004249),
      unitSet = NULL, 
      startDay = -365*5, # last 3 years - check how long to look back
      endDay = 0, 
      minVal = 5,
      maxVal = 250, 
      aggregateMethod = 'stdev',
      covariateId = 3495, 
      analysisId = 495
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
                     #21372, 
                     21294,
                     19165), 
        cohortName = c('Type I Diabetes', 'Migraine',
                       'Corticosteroids', 'SLE',
                       #'Atypical antipsychotic use',
                       'Severe mental illness',
                       'Erectile dysfunction'
        )
      ), 
      valueType = 'binary', 
      startDay = -9999, 
      endDay = 0
    ),

    #QRISK4 extras
    # No learning disability 22465668
    # Learning disability 21344668
    # Down syndrome 19881668
    # COPD 19380668
    # Lung cancer 19792668
    # Oral cancer 19788668
    # Blood cancer 19787668
    # Brain cancer 19174668
    
    FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 668, 
      covariateCohortDatabaseSchema = cohortDatabaseSchema,
      covariateCohortTable = cohortTableName, 
      covariateCohorts = data.frame(
        cohortId = c(22465, 21344, 19881, 
                     19380, 19792, 19788, 19787,
                     19174, 14512, 19793), 
        cohortName = c('No learning disability',
                       'Learning disability',
                       'Down syndrome',
                       'COPD',
                       'Lung cancer',
                       'Oral cancer',
                       'Blood cancer',
                       'Brain cancer',
                       'Pre eclampsia',
                       'Post natal depression'
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
        function(x){sapply(x, function(x) (x/10)^(-0.5))},
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
          3495,
          4466
        ),
        centerValue = c(
          0.506232738494873, 59.415359497070312, 0.154721871018410,
          0.144365265965462, 3.262585163116455, 120.015731811523438,
          8.925912857055664, 0.698884844779968
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
        19787668, # Blood cancer
        19174668, # Brain cancer
        21372668, # Atypical antipsychotic use
        21371668, # Corticosteroid use
        19165668, # Erectile dysfunction or treatment
        14512668, #Pre eclampsia
        19792668, #Lung cancer
        19380668, #COPD
        19164668, #SLE
        19379668, # Migraine
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
      19787668, # Blood cancer
      19174668, # Brain cancer
      21372668, # Atypical antipsychotic use
      21371668, # Corticosteroid use
      19165668, # Erectile dysfunction or treatment
      14512668, #Pre eclampsia
      19792668, #Lung cancer
      19380668, #COPD
      19164668, #SLE
      19379668, # Migraine
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

return(plpModelQRISK4_female_OG_10)
}
