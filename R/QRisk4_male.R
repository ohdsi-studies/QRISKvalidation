#' Create qrisk 4 model for males using Qrisk cohorts
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
createQRISK4MaleOG10 <- function(
    cohortDatabaseSchema,
    cohortTableName = 'qrisk_cprd',
    sampleSize = 10000
  ){
  
  # Male qrisk4
    
plpModelQRISK4_male_OG_10 <- PatientLevelPrediction::createGlmModel(
  targetId = 7, # the first cohort I create manually
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
        19787668, # Blood cancer
        19174668, # Brain cancer
        19380668, # COPD
        21371668, # Corticosteroid use
        19165668, # Erectile dysfunction or treatment
        19792668, # Lung cancer
        19379668, # Migraine
        19788668, # Oral cancer
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
        #21372668123, # Atypical antipsychotic use
        21371668123, # Corticosteroid use
        19165668123, # Erectile dysfunction or treatment
        19379668123, # Migraine
        #18838668, # Rheumatoid arthritis
        21347668123, # Renal disease
        #21294668, # Severe mental illness
        #19164668, # Systemic lupus erythematosus
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
        #21372668124, # Atypical antipsychotic use
        21371668124, # Corticosteroid use
        19165668124, # Erectile dysfunction or treatment
        19379668124, # Migraine
        #18838668, # Rheumatoid arthritis
        21347668124, # Renal disease
        #21294668, # Severe mental illness
        #19164668, # Systemic lupus erythematosus
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
      0.0200886529411730269112368,
      
      # race/ethnicity
      0, 0, 0.1723470364618767958209133, 0.3838229122963082162733883, 0.3420581171270106746185036,
      0.0386352325794289963578620,-0.4041201761899239075503942, -0.4198460764758025209886227,
      -0.3309767222744185444227583, -0.2061952053636268988867641,

      #Learning categories
      0, 0.1602966737992703727400112, 0.8531336245195806355923196,
      
      # smoker low to high
      0, 0.1774041204864900411752870, 0.6939317073387069045864450,
      0.7336346084621455654328770, 0.9543555923524250639289335,
      
      # binary
      0.9499228826197083641602603;
            double cb_bloodcancer = 0.7234457495597065301851103;
            double cb_braincancer = 1.6950927431790501209718514;
            double cb_copd = 0.3130585676582609000462298;
            double cb_corticosteroids = 0.4960617445738730868498578;
            double cb_impotence2 = 0.3332089730365202506767730;
            double cb_lungcancer = 0.5097969118514203978875798;
            double cb_migraine = 0.3401370244733452619101399;
            double cb_oralcancer = 0.3973168187492682745798334;
            double cb_ra = 0.1741572314252004538559504;
            double cb_renal = 0.5323571529843488248090466;
            double cb_semi = 0.1666852562198190002007436;
            double cb_sle = 0.5176501988771743389960989;
            double cb_treatedhyp = 0.7878502537263655236543514;
            double cb_type1 = 1.1876309028842588766394783;
            double cb_type2 = 0.7082139892808299030946273;
            double cfh_cvd = 0.4819917453481203373222286
      
      # measurements
      -17.8397816660055750000000000,
      0.0022964880605765492000000,
      2.4562776660536358000000000,
      -8.3011122314711354000000000,
      0.1734019685632711100000000,
      0.0129101265425533050000000,
      0.0102519142912904560000000,
      
      # interactions - age 1
      
      -0.2101113393351634600000000, # Former smoker
      0.7526867644750319100000000,
      0.9931588755640579100000000,
      2.1331163414389076000000000,
      3.4896675530623207000000000, #afib
      1.1708133653489108000000000, #steroids
      -1.5064009857454310000000000, # impotence
      2.3491159871402441000000000, #migraine
      -0.5065671632722369400000000, # renal
      6.5114581098532671000000000, #treated hyper
      5.3379864878006531000000000, # type 1
      3.6461817406221311000000000, # type 2
      31.0049529560338860000000000, #bmi 1
      -111.2915718439164300000000000, #bmi2
      2.7808628508531887000000000, #fhc
      0.0188585244698658530000000, #sbp
      -0.1007554870063731000000000, #townsend
      
      # interactions - age 2
      -0.0004985487027532612100000, # smoke
      -0.0007987563331738541400000, # smoke
      -0.0008370618426625129600000, # smoke
      -0.0007840031915563728900000, # smoke
      -0.0003499560834063604900000, #afib
      -0.0002496045095297166000000, #steriods
      -0.0011058218441227373000000, #impotence
      0.0001989644604147863100000, #migraine
      -0.0018325930166498813000000, #renale
      0.0006383805310416501300000, #treated hypertension
      0.0006409780808752897000000, # type 1
      -0.0002469569558886831500000, #type 2
      0.0050380102356322029000000, #bmi 1
      -0.0130744830025243190000000, #bmi 2
      -0.0002479180990739603700000, # fhc
      -0.0000127187419158845700000, #sbp
      -0.0000932996423232728880000 # townsend
                   )  
  ), 
  intercept = 0, 
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
          3495,
          4466
        ),
        centerValue = c(
          0.234766781330109, 77.284080505371094,
          0.149176135659218, 0.141913309693336,
          4.300998687744141, 128.571578979492190,
          8.756621360778809,
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
        21372668, # Atypical antipsychotic use
        21371668, # Corticosteroid use
        19165668, # Erectile dysfunction or treatment
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
      21372668, # Atypical antipsychotic use
      21371668, # Corticosteroid use
      19165668, # Erectile dysfunction or treatment
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

return(plpModelQRISK4_male_OG_10)
}












































                           







#' Create qrisk 4 model for males using Qrisk cohorts
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
createQRISK4MaleOG10 <- function(
    cohortDatabaseSchema,
    cohortTableName = 'qrisk_cprd',
    sampleSize = 10000
  ){
  
  # Male qrisk4
    
plpModelQRISK4_male_OG_10 <- PatientLevelPrediction::createGlmModel(
  targetId = 7, # the first cohort I create manually
  outcomeId = 21397, 
  restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(
    sampleSize = sampleSize # sampling for testing
    ),# can use this to restrict dates as well
  coefficients = data.frame(
    covariateId = c(
        3466, # Systolic blood pressure
        3467, # SD of blood pressure                     
        1486, # Cholesterol/HDL
        4466, # Deprivation (Townsend)
        
        21288655, # Former smoker
        21289655, # Light smoker
        21290655, # Moderate smoker
        21291655, # Heavy smoker
        
        22466652, # White or not recorded               
        21377652, # Indian
        21378652, # Pakistani
        21379652, # Bangladeshi
        21380652, # Other Asian
        21381652, # Black Caribbean
        21382652, # Black African
        21383652, # Chinese
        21386652, # Other  
        
        22465668, # No learning disability
        21344668, # Learning disability - Jenna note: is this recorded?
        19881668, # Down syndrome
        
        18821668, # Family history of coronary heart disease
        18820668, # Type I Diabetes
        18815668, # Type II Diabetes
        19280688, # Treated hypertension - recent 
        18838668, # Rheumatoid arthritis
        18841668, # Atrial fibrillation
        21347668, # Renal disease
        19379668, # Migraine
        21371668, # Corticosteroid use
        19164668, # Systemic lupus erythematosus
        21294668, # Severe mental illness
        19380668, # COPD
        19792668, # Lung cancer
        19788668, # Oral cancer
        19787668, # Blood cancer
        19174668, # Brain cancer
        19165668 # Erectile dysfunction or treatment
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
      unitSet = NULL, 
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
    
    QRISKvalidation::createMeasurementCovariateSettings(
      covariateName = 'Systolic Blood Pressure', 
      conceptSet = c(3004249),
      unitSet = NULL, 
      startDay = -365, 
      endDay = 0, 
      scaleMap = function(covariates){
        covariates$valueAsNumber <- sapply(covariates$valueAsNumber, function(y){y - 132.6})
        return(covariates)
      }, 
      minVal = 5,
      maxVal = 250, 
      aggregateMethod = 'stdev', 
      covariateId = 3467, 
      analysisId = 467
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
    
    # family history of CVD covariate 18821668
    # type 2 diabetes 18815668
    # rheumatoid arthritis 18838668
    # atrial fibrillation 18841668
    # renal disease 21347668
    # Type I diabetes 18820668
    # Migraine 19379668
    # Corticosteroid use 21371668
    # SLE 19164668
    # Atypical antipsychotic use 21372668
    # Severe mental illness 21294668
    # Erectile dysfunction or treatment 19165668
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
        cohortId = c(18821, 18815, 18838, 18841,
                     21347, 18820, 19379, 21371,
                     19164, 21372, 21294, 19165,
                     22465, 21344, 19881, 
                     19380, 19792, 19788, 19787,
                     19174), 
        cohortName = c('Family history of premature cardiovascular disease',
                       'Type 2 diabetes',
                       'Rheumatoid arthritis',
                       'Atrial fibrillation',
                       'Renal disease',
                       'Type I Diabetes',
                       'Migraine',
                       'Corticosteroids',
                       'SLE',
                       'Atypical antipsychotic use',
                       'Severe mental illness',
                       'Erectile dysfunction',
                       'No learning disability',
                       'Learning disability',
                       'Down syndrome',
                       'COPD',
                       'Lung cancer',
                       'Oral cancer',
                       'Blood cancer',
                       'Brain cancer'
                       )
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
    
  )                 
)

return(plpModelQRISK4_male_OG_10)
}
