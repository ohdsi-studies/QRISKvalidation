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


