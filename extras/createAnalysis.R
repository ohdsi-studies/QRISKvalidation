# code to create validation analysis script
library(Strategus) # need https://github.com/OHDSI/Strategus/tree/v1.0-plpv-mt-modules branch


targetIds <-  c(21398, 21399, 21400, 21401, 21402, 21403, 21404, 21405) 
outcomeId <-  21397
predictorIds <- c(18778,18779, 18815, 18820, 18821, 18822, 18838, 18841, 19379, 21371, 21347, 19164, 21372, 21294, 19165, 19174, 19792, 19881, 19787, 19380, 19788, 21344, 19285, 19280, 21377, 21378, 21379, 21380, 21381, 21382, 21383, 21386, 21387, 21288, 21289, 21290, 21291, 16927, 19893, 19882, 19884, 12467, 19880, 19887, 19888, 14512, 19793, 21395) 
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
  cohortIds = c(targetIds, outcomeId, predictorIds), 
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

# Code to validate 32 models
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21398,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK1_male_OG_3', 
      package = 'QRISKvalidation'
    )), # list of locations of models
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21399,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK1_female_OG_3', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21400,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK2_male_OG_3', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21401,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK2_female_OG_3', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21402,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK3_male_OG_3', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21403,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK3_female_OG_3',
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21404,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK4_male_OG_3', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21405,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK4_female_OG_3', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21398,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK1_male_OG_10', 
      package = 'QRISKvalidation'
    )), # list of locations of models
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21399,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK1_female_OG_10', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21400,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK2_male_OG_10', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21401,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK2_female_OG_10', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21402,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK3_male_OG_10', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21403,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK3_female_OG_10',
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21404,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK4_male_OG_10', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21405,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK4_female_OG_10', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21398,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK1_male_JJ_3', 
      package = 'QRISKvalidation'
    )), # list of locations of models
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21399,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK1_female_JJ_3', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21400,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK2_male_JJ_3', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21401,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK2_female_JJ_3', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21402,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK3_male_JJ_3', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21403,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK3_female_JJ_3',
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21404,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK4_male_JJ_3', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21405,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK4_female_JJ_3', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21398,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK1_male_JJ_10', 
      package = 'QRISKvalidation'
    )), # list of locations of models
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21399,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK1_female_JJ_10', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21400,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK2_male_JJ_10', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21401,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK2_female_JJ_10', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21402,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK3_male_JJ_10', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21403,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK3_female_JJ_10',
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21404,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK4_male_JJ_10', 
      package = 'QRISKvalidation'
    )),
  recalibrate = "weakRecalibration"
)
validationList[[length(validationList) + 1]] <- PatientLevelPrediction::createValidationDesign(
  targetId = 21405,
  outcomeId = outcomeId,
  populationSettings = NULL, # use models
  restrictPlpDataSettings = restrictPlpDataSettings,
  plpModelList = list(
    createPackageModel(
      modelFolder = 'models/QRISK4_female_JJ_10', 
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
