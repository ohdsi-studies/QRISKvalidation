# code to create validation analysis script
library(Strategus) # need https://github.com/OHDSI/Strategus/tree/v1.0-plpv-mt-modules branch


targetId <-  # ATLAS id for target cohort TO DO
outcomeId <-  # ATLAS id for outcome cohort TO DO
predictorIds <- c() # ATLAS ids for predictors TO DO
#remotes::install_github('ohdsi/Strategus', ref = 'v1.0-plpv-mt-modules')
library(Strategus)
ROhdsiWebApi::authorizeWebApi(
  baseUrl = Sys.getenv('baseUrl'), 
  authMethod = 'windows', 
  webApiUsername = keyring::key_get('webApiUsername', 'all'), 
  webApiPassword =  keyring::key_get('webApiPassword', 'all')
    )

# get the cohort ids from ATLAS used as the target, outcome and predictors
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  cohortIds = c(targetId, outcomeId, predictorIds), 
  generateStats = T,
  baseUrl = Sys.getenv('baseUrl')
  )


# Cohort Diagnostics -----------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cdModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE,
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE
)

# Cohort Generator -----------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create the settings & validate them
cohortSharedResourcesSpecifications <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
cgModuleSettingsCreator$validateCohortSharedResourceSpecifications(cohortSharedResourcesSpecifications)

cgModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications()

# Restrict Data Settings to every 6 months --------------------------------
generateRestrictDataSettings <- function(start, end, interval = months(6)) {
  startDate <- lubridate::ymd(start)
  endDate <- lubridate::ymd(end)
  
  settingsList <- list()
  currentStart <- startDate
  
  while (currentStart < endDate) {
    currentEnd <- currentStart + interval - days(1)
    
    if (currentEnd > endDate) {
      currentEnd <- endDate
    }
    
    settings <- createRestrictPlpDataSettings(
      studyStartDate = format(currentStart, "%Y%m%d"),
      studyEndDate = format(currentEnd, "%Y%m%d")
    )
    
    settingsList <- append(settingsList, list(settings))
    currentStart <- currentStart + interval
  }
  
  return(settingsList)
  
}

restrictPlpDataSettings <- generateRestrictDataSettings("2010-01-01", "2024-06-01")

# PatientLevelPredictionValidation -------------------------------
createPackageModel <- function(modelFolder, package){
  result <- list(
    type = 'package',
    modelFolder = modelFolder,
    package = package
  )
  class(result) <- 'plpModel'
  
  return(result)
}
validationList <- list()

# Code to validate 8 models
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = targetId,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK1_male', 
      package = 'QRISKvalidation'
    )), # list of locations of models
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = targetId,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK1_female', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = targetId,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK2_male', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = targetId,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK2_female', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = targetId,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK3_male', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = targetId,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK3_female',
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = targetId,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK4_male', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = targetId,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK4_female', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)

allValList <- do.call('c', validationList)

plpValModuleSettingsCreator <- PatientLevelPredictionValidationModule$new()
plpValModuleSpecifications <- plpValModuleSettingsCreator$createModuleSpecifications(
  allValList
)

# Create analysis specifications CDM modules ---------------
analysisSpecifications <- createEmptyAnalysisSpecificiations() |>
  addSharedResources(cohortSharedResourcesSpecifications) |>
  addCohortDiagnosticsModuleSpecifications(cdModuleSpecifications) |>
  addCohortGeneratorModuleSpecifications(cgModuleSpecifications) |>
  addPatientLevelPredictionValidationModuleSpecifications(plpValModuleSpecifications)

ParallelLogger::saveSettingsToJson(
  object = analysisSpecifications,
  fileName = "inst/study_execution_jsons/validation.json"
)
