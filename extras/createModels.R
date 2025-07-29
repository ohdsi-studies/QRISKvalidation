library(FeatureExtraction)
library(PatientLevelPrediction)
cohortDatabaseSchema <- 'cohortDatabaseSchema'
cohortTableName <- 'cohortTableName'
targetId <-  # ATLAS id for target cohort TO DO
outcomeId <-  # ATLAS id for outcome cohort TO DO

# specify the time-at-risk and remove requiring full 365 days
#3 years TAR
populationSettings3 <- PatientLevelPrediction::createStudyPopulationSettings(
  requireTimeAtRisk = FALSE, 
  riskWindowStart = 1, 
  startAnchor = 'cohort start',
  riskWindowEnd = 1095, 
  endAnchor = 'cohort start'
  )

#10 years TAR
populationSettings10 <- PatientLevelPrediction::createStudyPopulationSettings(
  requireTimeAtRisk = FALSE, 
  riskWindowStart = 1, 
  startAnchor = 'cohort start',
  riskWindowEnd = 3650, 
  endAnchor = 'cohort start'
  )

#========================================================================================
#ADJUSTMENTS TO THE COHORTS USED
#========================================================================================

#LOGARITHMIC AGE
#=======================================================================================

#CREATE
createLogAge <- function(){
  # create list of inputs to implement function
  featureEngineeringSettings <- list()
  # specify the function that will implement the sampling
  attr(featureEngineeringSettings, "fun") <- "implementLogAge"
  # make sure the object returned is of class "sampleSettings"
  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
}

#IMPLEMENT
implementLogAge <- function(trainData, featureEngineeringSettings) {
  # get covariate 1002 values
  ageData <- trainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 1002) %>%
    dplyr::select("rowId", 'covariateValue') %>%
    dplyr::collect()

  newData <- data.frame(
    rowId = ageData$rowId,
    covariateId = 3002, # this is the covariateId for logAge
    covariateValue = log(ageData$covariateValue)
  )

  # remove existing age if in covariates
  trainData$covariateData$covariates <- trainData$covariateData$covariates |>
    dplyr::filter(!covariateId %in% c(1002))
  # update covRef
  Andromeda::appendToTable(trainData$covariateData$covariateRef,
                           data.frame(covariateId=3002,
                                      covariateName='Log age in years',
                                      analysisId=2,
                                      conceptId=3002))

  # update covariates
  Andromeda::appendToTable(trainData$covariateData$covariates, newData)

  featureEngineering <- list(
    funct = 'implementLogAge',
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings
    )
  )

  attr(trainData$covariateData, 'metaData')$featureEngineering = listAppend(
    attr(trainData$covariateData, 'metaData')$featureEngineering,
    featureEngineering
  )

  return(trainData)
}

#INTERACTION TERM SBP x ANTIHYPERTENSIVE TREATMENT #TODO
#=======================================================================================
#CREATE
createInteractionTerm <- function {
  attr(featureEngineeringSettings, "fun") <- "implementInteractionTerm"
  class(FeatureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
  }

#IMPLEMENT
implementInteractionTerm <- function(trainData, featureEngineeringSettings, model) {
    ageData <- trainData$labels
    X <- trainData$labels$ageYear
    y <- ageData$outcomeCount
    newData <- data.frame(y = y, X = X)
    yHat <- predict(model, newData)
    newData <- data.frame(
      rowId = trainData$labels$rowId,
      covariateId = 2002,
      covariateValue = yHat
    )
  }

 #REMOVE EXISTING COVARIATE
  trainData$covariateData$covariates <- trainData$covariateData$covariates |>
    dplyr::filter(!.data$covariateId %in% c(1002)) #CHANGE 1002

#UPDATE COVARIATE REFERENCE
  Andromeda::appendToTable( 
    trainData$covariateData$covariateRef,
    data.frame(
      covariateId = 2002, #CHANGE 2002
      covariateName = "InteractionTerm",
      analysisId = 2,
      conceptId = 2002 #CHANGE 2002
    )
  )

  #UPDATE COVARIATE
  Andromeda::appendToTable(trainData$covariateData$covariates, newData)

  featureEngineering <- list(
    funct = "implementInteractionTerm",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      model = model
    )
  )

  attr(trainData$covariateData, "metaData")$featureEngineering <- listAppend(
    attr(trainData$covariateData, "metaData")$featureEngineering,
    featureEngineering
  )

  return(trainData)
}


#STANDARD DEVIATION OF BLOOD PRESSURE 
#=======================================================================================
#CREATE
createSDBP <- function(){
  # create list of inputs to implement function
  featureEngineeringSettings <- list()
  # specify the function that will implement the sampling
  attr(featureEngineeringSettings, "fun") <- "implementSDBP"
  # make sure the object returned is of class "sampleSettings"
  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
}

#IMPLEMENT
implementSDBP <- function(trainData, featureEngineeringSettings) {
  # get initial covariate values
  sdData <- trainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == ) %>%                                  #ADD INITIAL COVARIATE ID
    dplyr::select("rowId", 'covariateValue') %>%
    dplyr::collect()

  newData <- data.frame(
    rowId = sdData$rowId,                                  
    covariateId = 3003,                                         
    covariateValue = sd(sdData$covariateValue)            
  )

  # remove existing SBP if in covariates
  trainData$covariateData$covariates <- trainData$covariateData$covariates |>
    dplyr::filter(!covariateId %in% c())                                     #ADD INITIAL COVARIATE ID
  # update covRef
  Andromeda::appendToTable(trainData$covariateData$covariateRef,
                           data.frame(covariateId=3003, 
                                      covariateName='Standard deviation of blood pressure',
                                      analysisId=3,
                                      conceptId=3003))

  # update covariates
  Andromeda::appendToTable(trainData$covariateData$covariates, newData)

  featureEngineering <- list(
    funct = 'implementSDBP',
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings
    )
  )

  attr(trainData$covariateData, 'metaData')$featureEngineering = listAppend(
    attr(trainData$covariateData, 'metaData')$featureEngineering,
    featureEngineering
  )

  return(trainData)
}

#=======================================================================================
#CREATE THE MODELS
#=======================================================================================

#QRISK1_male_OG_3
#=======================================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK1_male <- PatientLevelPrediction::createGlmModel(
  targetId = , #TODO
  outcomeId = , #TODO
  coefficients = data.frame(
    covariateId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, "")*1000+canalysisId, #TODO
    coefficient = c(50.634, 1.001, 1.022, 1.300, 1.417, 1.017, 1.004, 1.847, 0.993) 
  ), 
  intercept = 0, 
  mapping = "", #TODO 
  populationSettings = populationSettings,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Body mass index', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of premature cardiovascular disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Antihypertensive agent', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SBPxantihypertensive agent interaction term', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    )
    ),
  featureEngineeringSettings = createLogAge()
)

plpModelQRISK1_male$modelDesign$targetId <- targetId
plpModelQRISK1_male$modelDesign$outcomeId <- outcomeId
plpModelQRISK1_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, ""),
    cohortName = c('Age','Cholesterol/HDL','Body mass index','Family history of premature cardiovascular disease','Current smoker','Townsend score', 'Systolic blood pressure', 'Antihypertensive agent','SBPxantihypertensive agent interaction term') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T
    )
)

# bug that needs fixing in PLP
attr(plpModelQRISK1_male,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK1_male, 
  dirPath = './inst/models/QRISK1_male'
    )

#QRISK1_female_OG_3
#=======================================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK1_female <- PatientLevelPrediction::createGlmModel(
  targetId = , #TODO
  outcomeId = , #TODO
  coefficients = data.frame(
    covariateId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, "")*1000+canalysisId, #TODO
    coefficient = c(87.75, 1.001, 1.015, 1.229, 1.530, 1.035, 1.005, 1.734, 0.996) 
  ), 
  intercept = 0, 
  mapping = "", #TODO 
  populationSettings = populationSettings,
    covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Body mass index', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of premature cardiovascular disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Antihypertensive agent', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SBPxantihypertensive agent interaction term', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    )
    ),
  featureEngineeringSettings = createLogAge()
)

plpModelQRISK1_female$modelDesign$targetId <- targetId
plpModelQRISK1_female$modelDesign$outcomeId <- outcomeId
plpModelQRISK1_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(1002, 18779, 18778, 18821, 19285, 21387, 18822, 19280, ""),
    cohortName = c('Age','Cholesterol/HDL','Body mass index','Family history of premature cardiovascular disease','Current smoker','Townsend score', 'Systolic blood pressure', 'Antihypertensive agent','SBPxantihypertensive agent interaction term')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T
    )
)

# bug that needs fixing in PLP
attr(plpModelQRISK1_female,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK1_female, 
  dirPath = './inst/models/QRISK1_female'
    )

#QRISK2_male_OG_3
#=============================================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK2_male <- PatientLevelPrediction::createGlmModel(
  targetId = , #TODO
  outcomeId = , #TODO
  coefficients = data.frame(
    covariateId = c(21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 18838, 18841, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 18841)*1000+canalysisId, #TODO
    coefficient = c(1.45, 1.97, 1.67, 1.37, 0.62, 0.63, 0.51, 0.91, 1.59, 0.218, 0.236, 0.0595, 1.19, 2.14, 1.65, 1.68, 2.20, 1.38, 2.40, 1.75, 0.985, 0.1946, 0.0482, 0.923, 0.932, 0.916, 0.902, 0.893) 
  ), 
  intercept = 0, 
  mapping = "", #TODO 
  populationSettings = populationSettings,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'BMI', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexBMI interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexTownsend interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsystolicbloodpressure interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
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
      analysisId = 668
    ), createCohortCovariateSettings(
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
      analysisId = 668
    ), createCohortCovariateSettings(
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
      analysisId = 668
    ), createCohortCovariateSettings(
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
      analysisId = 668
    ), createCohortCovariateSettings(
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
      analysisId = 668
    )
    ),
  featureEngineeringSettings = createLogAge()
)

plpModelQRISK2_male$modelDesign$targetId <- targetId
plpModelQRISK2_male$modelDesign$outcomeId <- outcomeId
plpModelQRISK2_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 18838, 18841, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 18841)
    cohortName = c('Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Age','BMI','Townsend score','Systolic blood pressure','Cholesterol/HDL','Family history coronary heart disease','Current smoker','Treated hypertension','Type 2 diabetes','Rheumatoid arthritis','Atrial fibrillation','Renal disease','AgexBMI interaction','AgexTownsend interaction','Agexsystolicbloodpressure interaction','Agexfamilyhistory interaction','Agexsmoking interaction','Agextreatedhypertension interaction','Agextype2diabetes interaction', 'Agexatrialfibrillation interaction') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T
    )
)

# bug that needs fixing in PLP
attr(plpModelQRISK2_male,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK2_male, 
  dirPath = './inst/models/QRISK2_male'
    )

#QRISK2_female_OG_3
#=============================================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK2_female <- PatientLevelPrediction::createGlmModel(
  targetId = , #TODO
  outcomeId = , #TODO
  coefficients = data.frame(
    covariateId = c(21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 18838, 18841, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 18841)*1000+canalysisId, #TODO
    coefficient = c(1.43, 1.80, 1.35, 1.15, 1.08, 0.58, 0.69, 1.04, 1.66, 0.216, 0.274, 0.06, 1.17, 1.99, 1.80, 1.54, 2.54, 1.50, 3.06, 1.70, 0.976, 0.1876, 0.0483, 0.927, 0.931, 0.952, 0.904, 0.858)
  ), 
  intercept = 0, 
  mapping = "", #TODO 
  populationSettings = populationSettings,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 1002, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'BMI', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Current smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19285, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexBMI interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18778,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F,
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'AgexTownsend interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Agexsystolicbloodpressure interaction', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      lnAgeInteraction = TRUE,
      analysisId = 668
    ), createCohortCovariateSettings(
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
      analysisId = 668
    ), createCohortCovariateSettings(
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
      analysisId = 668
    ), createCohortCovariateSettings(
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
      analysisId = 668
    ), createCohortCovariateSettings(
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
      analysisId = 668
    ), createCohortCovariateSettings(
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
      analysisId = 668
    )
    ),
  featureEngineeringSettings = createLogAge()
)

plpModelQRISK2_female$modelDesign$targetId <- targetId
plpModelQRISK2_female$modelDesign$outcomeId <- outcomeId
plpModelQRISK2_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 1002, 18778, 21387, 18822, 18779, 18821, 19285, 19280, 18815, 18838, 18841, 21347, 18778, 21387, 18822, 18821, 19285, 19280, 18815, 18841),
    cohortName = c('Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Age','BMI','Townsend score','Systolic blood pressure','Cholesterol/HDL','Family history coronary heart disease','Current smoker','Treated hypertension','Type 2 diabetes','Rheumatoid arthritis','Atrial fibrillation','Renal disease','AgexBMI interaction','AgexTownsend interaction','Agexsystolicbloodpressure interaction','Agexfamilyhistory interaction','Agexsmoking interaction','Agextreatedhypertension interaction','Agextype2diabetes interaction', 'Agexatrialfibrillation interaction')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T
    )
)

# bug that needs fixing in PLP
attr(plpModelQRISK2_female,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK2_female, 
  dirPath = './inst/models/QRISK2_female'
    )

#QRISK3_male_OG_3
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK3_male <- PatientLevelPrediction::createGlmModel(
  targetId = , #TODO
  outcomeId = , #TODO
  coefficients = data.frame(
    covariateId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19165, 18779, 18822, "")*1000+canalysisId, #TODO
    coefficient = c(0.236, 1.32, 1.61, 1.70, 1.04, 0.699, 0.670, 0.660, 0.769, 1.21, 1.74, 1.89, 2.20, 1.72, 3.44, 2.36, 1.68, 1.23, 2.42, 2.05, 1.29, 1.58, 1.55, 1.14, 1.13, 1.25, 1.19, 0.057, 0.111) 
  ), 
  intercept = 0, 
  mapping = "", #TODO
  populationSettings = populationSettings,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Former smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18820, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chronic kidney disease (stage 3,4 or 5)', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroid use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus erythematosus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19164, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372,
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    , createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Erectile dysfunction or treatment', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19165,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD of blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    )
    ),
  featureEngineeringSettings = createSDBP()                         
)

plpModelQRISK3_male$modelDesign$targetId <- targetId
plpModelQRISK3_male$modelDesign$outcomeId <- outcomeId
plpModelQRISK3_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19165, 18779, 18822, ""),
    cohortName = c('Townsend score','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Former smoker', 'Light smoker','Moderate smoker','Heavy smoker','Family history of coronary heart disease','Type I Diabetes','Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Chronic kidney disease (stage 3,4 or 5)','Migraine', 'Corticosteroid use', 'Systemic lupus erythematosus', 'Atypical antipsychotic use', 'Severe mental illness', 'Erectile dysfunction or treatment', 'Cholesterol/HDL', 'Systolic blood pressure', 'SD of blood pressure') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T
    )
)

# bug that needs fixing in PLP
attr(plpModelQRISK3_male,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK3_male, 
  dirPath = './inst/models/QRISK3_male'
    )

#QRISK3_female_OG_3
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK3_female <- PatientLevelPrediction::createGlmModel(
  targetId = , #TODO
  outcomeId = , #TODO
  coefficients = data.frame(
    covariateId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291,18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 18779, 18822, "")*1000+canalysisId, #TODO
    coefficient = c(0.294, 1.32, 1.76, 1.34, 1.08, 0.843, 0.675, 0.722, 0.843, 1.00, 1.14, 1.75, 1.95, 2.34, 1.58, 5.62, 2.91, 1.66, 1.24, 4.92, 1.92, 1.35, 1.81, 2.14, 1.29, 1.13, 1.17, 0.057, 0.108)
  ), 
  intercept = 0
  mapping = "", #TODO
  populationSettings = populationSettings,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377,   
      startDay = -999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Former smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of coronary heart disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18820,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chronic kidney disease (stage 3,4 or 5)', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19379,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroid use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus erythematosus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19164, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic use', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systolic blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Standard deviation of blood pressure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    )
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK3_female$modelDesign$targetId <- targetId
plpModelQRISK3_female$modelDesign$outcomeId <- outcomeId
plpModelQRISK3_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(21387, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21288, 21289, 21290, 21291,18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 18779, 18822, ""),
    cohortName = c('Townsend score','Indian','Pakistani','Bangladeshi','Other Asian','Black Caribbean','Black African','Chinese','Other','Former smoker','Light smoker','Moderate smoker','Heavy smoker','Family history of coronary heart disease','Type I Diabetes','Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Chronic kidney disease (stage 3,4 or 5)','Migraine', 'Corticosteroid use', 'Systemic lupus erythematosus', 'Atypical antipsychotic use', 'Severe mental illness', 'Cholesterol/HDL', 'Systolic blood pressure', 'SD of blood pressure') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T
    )
)

# bug that needs fixing in PLP
attr(plpModelQRISK3_female,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK3_female, 
  dirPath = './inst/models/QRISK3_female'
    )


#QRISK4_male_OG_3
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK4_male <- PatientLevelPrediction::createGlmModel(
  targetId = , #TODO
  outcomeId = , #TODO
  coefficients = data.frame(
    covariateId = c(18822,"",18779,21387,21288, 21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21344, 19881, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19380, 19792, 19788, 19787, 19174, 19165)*1000+canalysisId, #TODO
    coefficient = c(0.065, 1.14, 1.15, 0.222, 1.19, 2.00, 2.08, 2.60, 1.19, 1.47, 1.41, 1.04, 0.67, 0.66, 0.72, 0.81, 1.17, 2.35, 1.62, 3.28, 2.03, 2.20, 1.19, 2.59, 1.70, 1.41, 1.64, 1.68, 1.18, 1.37, 1.66, 1.49, 2.06, 5.45, 1.40)
  ), 
  intercept = 0, 
  mapping = "", #TODO 
  populationSettings = populationSettings,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'SBP', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD SBP',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,   
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Deprivation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Ex-smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290,
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Caribbean', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other ethnic group', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21344, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Down syndrome', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19881, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'FH of CHD', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type 1 Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18820, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type 2 Diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal failure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroids', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19164, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'COPD',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19380,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Lung cancer',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19792,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Oral cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19788,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Blood cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19787, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Brain cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19174, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Erectile dysfunction',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19165,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    )
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK4_male$modelDesign$targetId <- targetId
plpModelQRISK4_male$modelDesign$outcomeId <- outcomeId
plpModelQRISK4_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(18822,"",18779,21387,21288, 21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21344, 19881, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19380, 19792, 19788, 19787, 19174, 19165),
    cohortName = c('SBP', 'SD SBP', 'Cholesterol/HDL', 'Deprivation', 'Ex-smoker', 'Light smoker', 'Moderate smoker', 'Heavy smoker', 'Indian', 'Pakistani', 'Bangladeshi', 'Other Asian', 'Caribbean', 'Black African', 'Chinese', 'Other ethnic group', 'Learning disability', 'Down syndrome', 'FH of CHD', 'Type I Diabetes', 'Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Renal failure', 'Migraine', 'Corticosteroids', 'Systemic lupus', 'Atypical antipsychotics','Severe mental illness', 'COPD', 'Lung cancer', 'Oral cancer', 'Blood cancer', 'Brain cancer', 'Erectile dysfunction') 
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T
    )
)

# bug that needs fixing in PLP
attr(plpModelQRISK4_male,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK4_male, 
  dirPath = './inst/models/QRISK4_male'
    )

#QRISK4_female_OG_3
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK4_female <- PatientLevelPrediction::createGlmModel(
  targetId = , #TODO
  outcomeId = , #TODO
  coefficients = data.frame(
    covariateId = c(18822,"",18779,21387,21288,21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21344, 19881, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19380, 19792, 19788, 19787, 19174, 19793, 14512)*1000+canalysisId, #TODO
    coefficient = c(0.0635, 1.16, 1.14, 0.252, 1.19, 2.12, 2.30, 2.90, 1.13, 1.56, 1.28, 0.98, 0.94, 0.76, 0.68, 0.94, 1.45, 3.18, 1.46, 4.52, 2.49, 2.20, 1.28, 4.50, 1.81, 1.46, 1.70, 2.22, 1.24, 1.22, 1.85, 3.50, 1.55, 2.13, 4.52, 1.18, 1.56) 
  ), 
  intercept = 0, 
  mapping = "", #TODO 
  populationSettings = populationSettings,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'SBP', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18822,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'SD SBP',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Cholesterol/HDL ratio', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18779,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Deprivation',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21387, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Ex-smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21288, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Light smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21289, 
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Moderate smoker',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21290,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Heavy smoker', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21291,  
      startDay = -365,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Indian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21377, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pakistani', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21378, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Bangladeshi',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other Asian', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Caribbean',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21381, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Black African', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21382, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Chinese', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21383,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Other ethnic group',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21386,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Learning disability', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21344,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Down syndrome',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19881,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'FH of CHD', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18821, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type I diabetes', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18820,          
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Type II Diabetes',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18815,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Treated hypertension', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19280,   
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Rheumatoid arthritis',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18838,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atrial fibrillation', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 18841,
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Renal failure', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21347, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Migraine',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19379,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Corticosteroids', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21371,  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Systemic lupus',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19164, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Atypical antipsychotic', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21372, 
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Severe mental illness', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 21294,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'COPD',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19380, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Lung cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19792,   
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Oral cancer',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19788,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Blood cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19787, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Brain cancer', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19174,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Postnatal depression',
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 19793, 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Pre-eclampsia', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = 14512,  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    )
    ),
  featureEngineeringSettings = createSDBP() 
)

plpModelQRISK4_female$modelDesign$targetId <- targetId
plpModelQRISK4_female$modelDesign$outcomeId <- outcomeId
plpModelQRISK4_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = c(18822,"",18779,21387,21288,21289, 21290, 21291, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21344, 19881, 18821, 18820, 18815, 19280, 18838, 18841, 21347, 19379, 21371, 19164, 21372, 21294, 19380, 19792, 19788, 19787, 19174, 19793, 14512), #TODO
    cohortName = c('SBP', 'SD SBP', 'Cholesterol/HDL', 'Deprivation', 'Ex-smoker', 'Light smoker', 'Moderate smoker', 'Heavy smoker', 'Indian', 'Pakistani', 'Bangladeshi', 'Other Asian', 'Caribbean', 'Black African', 'Chinese', 'Other ethnic group', 'Learning disability', 'Down syndrome', 'FH of CHD', 'Type I Diabetes', 'Type II Diabetes', 'Treated hypertension', 'Rheumatoid arthritis', 'Atrial fibrillation', 'Renal failure', 'Migraine', 'Corticosteroids', 'Systemic lupus', 'Atypical antipsychotic', 'Severe mental illness', 'COPD', 'Lung cancer', 'Oral cancer', 'Blood cancer', 'Brain cancer', 'Postnatal depression', 'Pre-eclampsia')
    ), 
  valueType = 'binary', 
  startDay = -365, 
  endDay = 0
  ),
  FeatureExtraction::createCovariateSettings(
    useDemographicsAge = T
    )
)

# bug that needs fixing in PLP
attr(plpModelQRISK4_female,"saveType") <- 'RtoJson'

# RtoJson
PatientLevelPrediction::savePlpModel(
  plpModel = plpModelQRISK4_female, 
  dirPath = './inst/models/QRISK4_female'
    )

#QRISK1_male_OG_10
#============================================================================

#QRISK1_female_OG_10 
#============================================================================

#QRISK2_male_OG_10
#============================================================================

#QRISK2_female_OG_10
#============================================================================

#QRISK3_male_OG_10
#============================================================================

#QRISK3_female_OG_10
#============================================================================

#QRISK4_male_OG_10
#============================================================================

#QRISK4_female_OG_10
#============================================================================
  
#QRISK1_male_JJ_3
#============================================================================

#QRISK1_female_JJ_3
#============================================================================

#QRISK2_male_JJ_3
#============================================================================

#QRISK2_female_JJ_3
#============================================================================

#QRISK3_male_JJ_3
#============================================================================

#QRISK3_female_JJ_3
#============================================================================

#QRISK4_male_JJ_3
#============================================================================

#QRISK4_female_JJ_3
#============================================================================

#QRISK1_male_JJ_10
#============================================================================

#QRISK1_female_JJ_10 
#============================================================================

#QRISK2_male_JJ_10
#============================================================================

#QRISK2_female_JJ_10
#============================================================================

#QRISK3_male_JJ_10
#============================================================================

#QRISK3_female_JJ_10
#============================================================================

#QRISK4_male_JJ_10
#============================================================================

#QRISK4_female_JJ_10
#============================================================================
