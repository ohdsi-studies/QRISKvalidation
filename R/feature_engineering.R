#' Feature engineering that can add interaction terms for any covariate 
#'
#' @details
#' The user specifies a main covariateId called interactionCovariateId and other covariateIds to
#' interact with covariateIdsOfInterest and the code will create new covariateIds corresponding
#' to the interactions with the id of covariateIdsOfInterest*1000+analysisId
#'
#' @param interactionCovariateId the main covariateId such as the id for age in years
#' @param covariateIdsOfInterest a single covariateId or vector of covariateIds to interact with
#' @param analysisId A unique analysisId for the new interaction covariates
#'
#' @return
#' The feature engineering settings that can be used in runPlp to add the interaction covariates
#' @export
createInteractionFe <- function(
    interactionCovariateId, 
    covariateIdsOfInterest,
    analysisId = 70
){
  
  fe <- list(
    funct = 'QRISKvalidation::interactionFeFunc', 
    settings = list(
      interactionCovariateId = interactionCovariateId,
      covariateIdsOfInterest = covariateIdsOfInterest,
      analysisId = analysisId 
    )
    )
  
  return(fe)
}

#' Feature engineering that can subtract values enabling centering
#'
#' @details
#' The user specifies a data.frame with columns covariateId and centerValue if the covariateIds
#' are found in the data their value will be modified by subtracting the centerValue
#'
#' @param centers a data.frame with columns covariateId and centerValue
#'
#' @return
#' The feature engineering settings that can be used in runPlp to perform centering
#' @export
createCenteringFe <- function(centers){
  
  fe <- list(
    funct = 'QRISKvalidation::centeringFeFunc', 
    settings = list(
      centers = centers
    )
  )
  
  return(fe)
}

#' Feature engineering that can do mapping of a measurement variable
#'
#' @details
#' The user specifies the covariateId of the measurement covariate, an analysisId to use for the
#' new variables and a list of mappings that are performed the first mapping will have the new
#' covariateId of 1x1000+analysisId, the second mapping will have the new covariateId of 
#' 2x1000+analysisId, etc.
#'
#' @param covariateId the covariateId of the measurementId to map
#' @param analysisId the analysisId of the new covariates created by mapping the measurement
#' @param mappings A list of functions or string names of function to apply to the measurement
#' 
#'
#' @return
#' The feature engineering settings that can be used in runPlp to map the measurement
#' @export
createMeasurementFe <- function(
    covariateId = 2466,
    analysisId = 366,
    mappings = list(
      function(x){sapply(x, function(x) 1/(x/10)^2)},
      function(x){sapply(x, function(x) (1/(x/10)^2)*log(x/10))}
    )  
  ){
  
  fe <- list(
    funct = 'QRISKvalidation::convertMeasurementFunc', 
    settings = list(
      covariateId = covariateId,
      analysisId = analysisId,
      mappings = mappings
    )
  )
  
  return(fe)
}



#' Performs feature engineering that can subtract values enabling centering
#'
#' @details
#' This takes the featurEengineering settings for centering and does it 
#'
#' @param trainData The data to perform the feature engineering to
#' @param centers a data.frame with columns covariateId and centerValue
#'
#' @return
#' The data with centering performed
#' @export
centeringFeFunc <- function(trainData,centers){
  metaData <- attr(trainData, "metaData")
  
  # should we check centering not already applied?
  
  print(paste0('applying centering mapping for ', nrow(centers), ' covariates'))
  
  trainData$covariateData$covariates <- trainData$covariateData$covariates %>%
    dplyr::left_join(centers, by = c('covariateId'), copy = TRUE) %>%
    dplyr::mutate(
      covariateValue = if_else(!is.na(.data$centerValue), .data$covariateValue - .data$centerValue, .data$covariateValue) 
    ) %>%
    dplyr::select(-"centerValue")
  
  # edit covRef to show centering was done
  trainData$covariateData$covariateRef <- trainData$covariateData$covariateRef %>%
    dplyr::left_join(centers, by = c('covariateId'), copy = TRUE) %>%
    dplyr::mutate(
      covariateName = if_else(!is.na(.data$centerValue), paste0(.data$covariateName, ' - ', .data$centerValue), .data$covariateName) 
    ) %>%
    dplyr::select(-"centerValue")
  
  
  attr(trainData, "metaData") <- metaData
  return(trainData) 
}


#' Performs feature engineering for interaction terms for any covariate 
#'
#' @details
#' Given the settings this will do the interactions as requested
#'
#' @param trainData The data to perform the feature engineering to description
#' @param interactionCovariateId the main covariateId such as the id for age in years
#' @param covariateIdsOfInterest a single covariateId or vector of covariateIds to interact with
#' @param analysisId A unique analysisId for the new interaction covariates
#'
#' @return
#' The data with interactions as specified added
#' @export
interactionFeFunc <- function(trainData,interactionCovariateId,covariateIdsOfInterest,analysisId){
  metaData <- attr(trainData, "metaData")
  
  mainCovariate <- trainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == !!interactionCovariateId) %>%
    dplyr::collect() %>%
    dplyr::select(-"covariateId") %>%
    dplyr::rename(
      covariateValue2 = "covariateValue"
    )
  
  print(paste0('applying interaction mapping for ', nrow(mainCovariate), ' people'))
  
  interactionData <- trainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId %in% !!covariateIdsOfInterest) %>%
    dplyr::inner_join(mainCovariate, by = c('rowId'), copy = TRUE) %>%
    dplyr::mutate(
      covariateValue = .data$covariateValue*.data$covariateValue2,
      covariateId = .data$covariateId*1000 + !!analysisId
    ) %>%
    dplyr::select(-"covariateValue2") %>%
    dplyr::collect()
  
  # add interactions to trainData
  Andromeda::appendToTable(trainData$covariateData$covariates, interactionData)
  
  
  covRef <- trainData$covariateData$covariateRef %>%
    dplyr::filter(.data$covariateId %in% !!covariateIdsOfInterest) %>%
    dplyr::mutate(
      covariateId = .data$covariateId*1000 + !!analysisId,
      covariateName = paste0(.data$covariateName, ' interaction with ', !!interactionCovariateId),
      analysisId = !!analysisId
    ) %>%
    dplyr::collect()
  
  # add to covariateRef
  Andromeda::appendToTable(
    tbl = trainData$covariateData$covariateRef,
    data = covRef
  )
  
  # add analysisRef
  Andromeda::appendToTable(
    tbl = trainData$covariateData$analysisRef,
    data = data.frame(
      analysisId = !!analysisId,
      analysisName = paste0('Interaction with ', !!interactionCovariateId),
      domainId = 'interaction',
      startDay = 0,
      endDay = 0,
      isBinary = 'N',
      missingMeansZero = 'Y'
    )
  )
  
  attr(trainData, "metaData") <- metaData
  return(trainData) 
}


#' Performs feature engineering for measurement mapping  
#'
#' @details
#' Given the settings this will do the measurement mapping as requested
#'
#' @param trainData The data to perform the feature engineering to description
#' @param covariateId the covariateId of the measurementId to map
#' @param analysisId the analysisId of the new covariates created by mapping the measurement
#' @param mappings A list of functions or string names of function to apply to the measurement
#'
#' @return
#' The data with measurements mapped as specified 
#' @export
convertMeasurementFunc <- function(trainData,analysisId,covariateId,mappings){ 
  
  metaData <- attr(trainData, "metaData")
  
  cData <- trainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == !!covariateId) %>%
    dplyr::collect()
  
  print(paste0('applying mapping for ', nrow(cData), ' people'))
  
  for(i in 1:length(mappings)){
    
    if(inherits(mappings[[i]], 'function')){
      mapFun <- mappings[[i]]
    } else{
      mapFun <- eval(parse(text = mappings[[i]]))
    }
    
    cDatai <- cData
    cDatai$covariateId <- i*1000+analysisId # this is covId
    cDatai$covariateValue <- mapFun(cDatai$covariateValue)
    
    # add bmi back to trainData
    Andromeda::appendToTable(trainData$covariateData$covariates, cDatai)
    
    # add to covariateRef
    Andromeda::appendToTable(
      tbl = trainData$covariateData$covariateRef,
      data = data.frame(
        covariateId = i*1000+analysisId, covariateName = paste(covariateId,i), analysisId = analysisId,
        conceptId = 0, valueAsConceptId = 0, collisions = NA 
      )
    )
  }

  
  attr(trainData, "metaData") <- metaData
  return(trainData) 
}
