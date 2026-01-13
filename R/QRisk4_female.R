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
        19787668124, #Blood cancer
        19174668124, #Brain cancer
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
      0.0454453912085431635525801,
      
      # race/ethnicity
     0, 0, 0.1260967017814430268796144, 0.4447705239699963342125955, 0.2477444278286677903366808,
    -0.0157243672799900063852263, -0.0157243672799900063852263, -0.0157243672799900063852263,
    -0.0577696651849263498323950, -0.2697651695209241085038343, -0.2697651695209241085038343,
    -0.3832410026548082604413992, -0.0580485370353437737933611
        
      #Learning categories
      0, 0.3725963227883137274254466, 1.1580969512183603153943068,
      
      # smoker low to high
      0, 0.1774041204864900411752870, 0.6939317073387069045864450,
      0.7336346084621455654328770, 0.9543555923524250639289335,
      
      # binary
      0.9499228826197083641602603, 0.7234457495597065301851103, 1.6950927431790501209718514,
      0.3130585676582609000462298, 0.4960617445738730868498578, 0.3332089730365202506767730,
      0.5097969118514203978875798, 0.3401370244733452619101399, 0.3973168187492682745798334,
      0.1741572314252004538559504, 0.5323571529843488248090466, 0.1666852562198190002007436,
      0.5176501988771743389960989, 0.7878502537263655236543514, 1.1876309028842588766394783,
      0.7082139892808299030946273, 0.4819917453481203373222286,
      
      # measurements
      -16.1922498195396364906173403, 0.0016779086596045746093697, 3.0731260756448430804255167,
      -11.4015314691208153874413256, 0.1354252218467675294988339, 0.0131633412762922549776867,
      0.0133785316988928315584673,0.0200886529411730269112368,
      
      # interactions - age 1
      
     -1.0285101268864735857277992, #smoke1
     -2.1616656275579138046794014, #smoke2
     -2.0959397919017006550745919, #smoke3
     -1.3134274702552712899006337, #smoke4
      4.6456825565369861408271390, #Afib
      5.8090699215701908642017770, #BLOOD CANCER
      9.8746965146404459545692589, #BRAIN CANCER
      2.6640415466372848740661539, #corticosteroids
      1.0935983468822383368745932, #erectile disfunction
      1.6230280460782404716724159, #migraine
      1.9630049259959354568394474, #renal disease
      7.6753571591474916147035401, #treated hypertension
      5.1514884947767107803429099, #type 1
      3.3917619227997812814123790, #type 2
      41.2983320016480064396091620, #bmi1
     -115.7270264987409120749362046, #bmi2
      1.1403390693832313740330164, #fhc
     -0.0244294742580109976171077, #sbp
     -0.1157444302410534642255868, #townsend
      
      # interactions - age 2
      -0.0007170260141850262570681, #smoke1
      -0.0020007003027763745062195, #smoke2
      -0.0019925858287754764745781, #smoke3
      -0.0021815836785603276784529, #smoke4
       0.0003534227276465033186492, #Afib
       0.0007810425520552304349750, #BLOOD CANCER
       0.0011146879108776548261001, #BRAIN CANCER
      -0.0000151511139894314848385, #Corticosteroids
      -0.0002272981023377529548584, #Erectile disfunction
      -0.0000244258813485797575155, #Migraine
      -0.0002632053208727756408135, #Renal disease
       0.0011248066588910984788668, #Treated hypertension
       0.0010984673002846123864018, #Type I
       0.0001480557938612134922206, #Type II
       0.0124268421539723422641899, #BMI I
      -0.0211322182290514942737403, #BMI II
      -0.0007843385905331079661906, #FHC
      -0.0000332155446999523516717, #SBP
      -0.0000712446284345675926895  #Townsend
                   )  
  ), 
  intercept = 0,          #STILL LOOK AT MAPPING
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
                     19174), 
        cohortName = c('No learning disability',
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
          0.506450295448303, 59.262386322021484,
          0.152559399604797, 0.143421187996864,
          3.989714860916138, 126.330375671386719,
          8.690177917480469, 0.788934886455536;
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
































createQRISK4FemaleOG10 <- function(
    cohortDatabaseSchema,
    cohortTableName = 'qrisk_cprd',
    sampleSize = 10000
  ){
  
  # Female qrisk4
    
plpModelQRISK4_female_OG_10 <- PatientLevelPrediction::createGlmModel(
  targetId = 8, 
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
        21372668, # Atypical antipsychotic use
        21294668, # Severe mental illness
        19380668, # COPD
        19792668, # Lung cancer
        19788668, # Oral cancer
        19787668, # Blood cancer
        19174668, # Brain cancer
        19793668, # Post natal depression
        14512668  # Pre eclampsia
    ),       
    coefficient = c(0.0635, 1.16, 1.14, 0.252, 1.19, 2.12, 2.30, 2.90, 1.00, 1.13, 1.56, 1.28, 0.98, 0.94, 0.76, 0.68, 0.94, 1.00, 1.45, 3.18, 1.46, 4.52, 2.49, 2.20, 1.28, 4.50, 1.81, 1.46, 1.70, 2.22, 1.24, 1.22, 1.85, 3.50, 1.55, 2.13, 4.52, 1.18, 1.56) 
  ), 
  intercept = 0, 
  mapping = "function(x){ sapply(x, function(x){
baseline <- 0.02123679
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
    # Pre eclampsia 14512668
    # Postnatal depression 19793668
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
                     19164, 21372, 21294, 14512,
                     19793, 22465, 21344, 19881, 
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
                       'Pre eclampsia',
                       'Postnatal depression',
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

return(plpModelQRISK4_female_OG_10)
}
