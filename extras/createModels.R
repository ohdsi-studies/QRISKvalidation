library(FeatureExtraction)
library(PatientLevelPrediction)
cohortDatabaseSchema <- 'cohortDatabaseSchema'
cohortTableName <- 'cohortTableName'
targetId <-  # ATLAS id for target cohort TO DO
outcomeId <-  # ATLAS id for outcome cohort TO DO
predictorIds <- c() # ATLAS ids for predictors TO DO

# specify the time-at-risk and remove requring full 365 days
populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(
  requireTimeAtRisk = FALSE, 
  riskWindowStart = 1, 
  startAnchor = 'cohort start',
  riskWindowEnd = 365, 
  endAnchor = 'cohort start'
  )
  
#=======================================================================================
#CREATE THE MODELS
#=======================================================================================

#QRISK1_male 
#=======================================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK1_male <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = predictorIds*1000+canalysisId,
    coefficient = c(50.634, 1.001, 1.022, 1.300, 1.417, 1.017, 1.004, 1.847, 0.993) #TODO
  ), 
  intercept = 0, 
  mapping = "logistic", #TODO
  populationSettings = populationSettings,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Total cholesterol/HDL cholesterol ratio', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Body mass index', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of premature cardiovascular disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
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
      cohortId = , #TODO
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
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
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Antihypertensive agent', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
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
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    )
    )
)

plpModelQRISK1_male$modelDesign$targetId <- targetId
plpModelQRISK1_male$modelDesign$outcomeId <- outcomeId
plpModelQRISK1_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = predictorIds,
    cohortName = c('','','','','','') #TODO
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

#QRISK1_female 
#=======================================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK1_female <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = predictorIds*1000+canalysisId,
    coefficient = c(87.75, 1.001, 1.015, 1.229, 1.530, 1.035, 1.005, 1.734, 0.996) #TODO
  ), 
  intercept = 0, 
  mapping = "logistic", #TODO
  populationSettings = populationSettings,
    covariateSettings = list(createCohortCovariateSettings(
      cohortName = 'Age', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Total cholesterol/HDL cholesterol ratio', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Body mass index', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Family history of premature cardiovascular disease', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
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
      cohortId = , #TODO
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Townsend score', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
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
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = 'Antihypertensive agent', 
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
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
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    )
    )
)

plpModelQRISK1_female$modelDesign$targetId <- targetId
plpModelQRISK1_female$modelDesign$outcomeId <- outcomeId
plpModelQRISK1_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = predictorIds,
    cohortName = c('','','','','','') #TODO
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

#QRISK2_male
#=============================================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK2_male <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = predictorIds*1000+canalysisId,
    coefficient = c() #TODO
  ), 
  intercept = 0, 
  mapping = "logistic", #TODO
  populationSettings = populationSettings,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    )
    )
)

plpModelQRISK2_male$modelDesign$targetId <- targetId
plpModelQRISK2_male$modelDesign$outcomeId <- outcomeId
plpModelQRISK2_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = predictorIds,
    cohortName = c('','','','','','') #TODO
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

#QRISK2_female
#=============================================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK2_female <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = predictorIds*1000+canalysisId,
    coefficient = c() #TODO
  ), 
  intercept = 0, 
  mapping = "logistic", #TODO
  populationSettings = populationSettings,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    )
    )
)

plpModelQRISK2_female$modelDesign$targetId <- targetId
plpModelQRISK2_female$modelDesign$outcomeId <- outcomeId
plpModelQRISK2_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = predictorIds,
    cohortName = c('','','','','','') #TODO
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

#QRISK3_male
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK3_male <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = predictorIds*1000+canalysisId,
    coefficient = c() #TODO
  ), 
  intercept = 0, 
  mapping = "logistic", #TODO
  populationSettings = populationSettings,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    )
    )
)

plpModelQRISK3_male$modelDesign$targetId <- targetId
plpModelQRISK3_male$modelDesign$outcomeId <- outcomeId
plpModelQRISK3_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = predictorIds,
    cohortName = c('','','','','','') #TODO
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

#QRISK3_female
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK3_female <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = predictorIds*1000+canalysisId,
    coefficient = c() #TODO
  ), 
  intercept = 0, 
  mapping = "logistic", #TODO
  populationSettings = populationSettings,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    )
    )
)

plpModelQRISK3_female$modelDesign$targetId <- targetId
plpModelQRISK3_female$modelDesign$outcomeId <- outcomeId
plpModelQRISK3_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = predictorIds,
    cohortName = c('','','','','','') #TODO
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


#QRISK4_male
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK4_male <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = predictorIds*1000+canalysisId,
    coefficient = c() #TODO
  ), 
  intercept = 0, 
  mapping = "logistic", #TODO
  populationSettings = populationSettings,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    )
    )
)

plpModelQRISK4_male$modelDesign$targetId <- targetId
plpModelQRISK4_male$modelDesign$outcomeId <- outcomeId
plpModelQRISK4_male$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = predictorIds,
    cohortName = c('','','','','','') #TODO
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

#QRISK4_female
#============================================================================
# need to add covariateSettings and stuff to this?
canalysisId <- 668
plpModelQRISK4_female <- PatientLevelPrediction::createGlmModel(
  coefficients = data.frame(
    covariateId = predictorIds*1000+canalysisId,
    coefficient = c() #TODO
  ), 
  intercept = 0, 
  mapping = "logistic", #TODO
  populationSettings = populationSettings,
  covariateSettings = list(createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO  
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO  
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO
      startDay = -30,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    ), createCohortCovariateSettings(
      cohortName = '', #TODO
      settingId = 1,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTableName,
      cohortId = , #TODO 
      startDay = -9999,
      endDay = 0,
      count = F, 
      ageInteraction = F, 
      analysisId = 668
    )
    )
)

plpModelQRISK4_female$modelDesign$targetId <- targetId
plpModelQRISK4_female$modelDesign$outcomeId <- outcomeId
plpModelQRISK4_female$modelDesign$covariateSettings <- list(
  FeatureExtraction::createCohortBasedCovariateSettings(
  analysisId = canalysisId,
  covariateCohortDatabaseSchema = '', 
  covariateCohortTable = '', 
  covariateCohorts = data.frame(
    cohortId = predictorIds,
    cohortName = c('','','','','','') #TODO
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


