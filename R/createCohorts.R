#' @export
generateCohorts <- function(
    connectionDetails,
    cdmDatabaseSchema,
    cohortDatabaseSchema,
    cohortTable = 'qrisk_cprd',
    tempDatabaseSchema
){
  
  male <- 8507
  female <- 8532
  
  cardio <- 22471 
  diabetes <- 22472 
  statins <- 22473
  
  connectionHandler <- ResultModelManager::ConnectionHandler$new(connectionDetails = connectionDetails)
  on.exit(connectionHandler$closeConnection)
  
  cohortDefinitionSet <- readRDS(system.file('cohortDefinitionSet.rds', package = 'QRISKvalidation'))
  
  21397
  
  cohortTableNames <- CohortGenerator::getCohortTableNames(
    cohortTable = cohortTable
  )
  
  CohortGenerator::createCohortTables(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableNames = cohortTableNames
  )
  
  CohortGenerator::generateCohortSet(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    tempEmulationSchema = cohortDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableNames = cohortTableNames,
    cohortDefinitionSet = cohortDefinitionSet
  )
  
  # then create custom cohorts
  targets <- list(
    
    list(
      targetId = 1,
      gender = male,
      minAge = 35,
      maxAge = 74,
      startDate = '1995-01-01',
      endDate = '2007-04-01',
      exclusions = c(diabetes, cardio) # also needs townsend?
    ),
    
    list(
      targetId = 2,
      gender = female,
      minAge = 35,
      maxAge = 74,
      startDate = '1995-01-01',
      endDate = '2007-04-01',
      exclusions = c(diabetes, cardio) # also needs townsend?
    ),
    
    
    list(
      targetId = 3,
      gender = male,
      minAge = 35,
      maxAge = 74,
      startDate = '1993-01-01',
      endDate = '2008-03-31',
      exclusions = c(statins, cardio) # also needs townsend?
    ),
    
    list(
      targetId = 4,
      gender = female,
      minAge = 35,
      maxAge = 74,
      startDate = '1993-01-01',
      endDate = '2008-03-31',
      exclusions = c(statins, cardio) # also needs townsend?
    ),
    
    
    list(
      targetId = 5,
      gender = male,
      minAge = 25,
      maxAge = 84,
      startDate = '1998-01-01',
      endDate = '2015-12-31',
      exclusions = c(statins, cardio) # also needs townsend?
    ),
    
    list(
      targetId = 6,
      gender = female,
      minAge = 25,
      maxAge = 84,
      startDate = '1998-01-01',
      endDate = '2015-12-31',
      exclusions = c(statins, cardio) # also needs townsend?
    ),
    
    list(
      targetId = 7,
      gender = male,
      minAge = 18,
      maxAge = 84,
      startDate = '1998-01-01',
      endDate = '2015-12-31',
      exclusions = c(statins, cardio) # also needs townsend?
    ),
    
    list(
      targetId = 8,
      gender = female,
      minAge = 18,
      maxAge = 84,
      startDate = '2010-01-01',
      endDate = '2021-12-31',
      exclusions = c(statins, cardio) # also needs townsend?
    )
    
  )
  
  # creating the target cohorts in a loop:
  for(i in 1:length(targets)){
    print(i)
    sql <- "SELECT p.person_id,

CASE 
WHEN dateadd(day, 365, obs.observation_period_start_date) >= cast(dateadd(year, @min_age, CONCAT(p.year_of_birth,'-01-01')) as date)
AND dateadd(day, 365, obs.observation_period_start_date) >= cast('@start_date' as date) THEN dateadd(day, 365, obs.observation_period_start_date)
WHEN cast('@start_date' as date) >= cast(dateadd(year, @min_age, CONCAT(p.year_of_birth,'-01-01')) as date) THEN cast('@start_date' as date)
ELSE cast(dateadd(year, @min_age, CONCAT(p.year_of_birth,'-01-01')) as date)
END as cohort_start_date,

obs.observation_period_end_date as cohort_end_date,

dateadd(day, 365, obs.observation_period_start_date) as year_obs_date,
cast(dateadd(year, @min_age, CONCAT(p.year_of_birth,'-01-01')) as date) as age_min_date,
cast(dateadd(year, @max_age, CONCAT(p.year_of_birth,'-01-01')) as date) as age_max_date,
cast('@start_date' as date) as cal_date

INTO #dates

FROM @schema.person p 
INNER JOIN @schema.observation_period obs
ON p.person_id = obs.person_id
WHERE obs.observation_period_end_date >= dateadd(day, 365, obs.observation_period_start_date)
--AND obs.observation_period_end_date < cast('@end_date' as date)
AND p.gender_concept_id = @gender_code
;"
    
    # execute to get dates
    connectionHandler$executeSql(
      sql,
      start_date = targets[[i]]$startDate,
      end_date = targets[[i]]$endDate,
      gender_code = targets[[i]]$gender,
      min_age = targets[[i]]$minAge,
      max_age = targets[[i]]$maxAge,
      schema = cdmDatabaseSchema
    )
    
    # now find all the period who should be included
    sql <- "SELECT
d.person_id,
d.cohort_start_date

INTO #inclusions

FROM #dates d
WHERE NOT EXISTS (
SELECT * from @cohort_schema.@cohort_table ct
WHERE ct.subject_id = d.person_id 
AND ct.cohort_start_date <= d.cohort_start_date
AND ct.cohort_definition_id in (@exclusion_ids)
)
;
"

# get the included people
connectionHandler$executeSql(
  sql,
  cohort_schema = cohortDatabaseSchema,
  cohort_table = cohortTable,
  exclusion_ids = paste0(targets[[i]]$exclusions, collapse = ',')
)

sql <- "
INSERT INTO @cohort_schema.@cohort_table(subject_id, cohort_definition_id, cohort_start_date, cohort_end_date)

SELECT 
#dates.person_id as subject_id, 
@cohort_definition_id as cohort_definition_id,
min(#dates.cohort_start_date) as cohort_start_date,
min(case when #dates.cohort_end_date < cast('@end_date' as date) then #dates.cohort_end_date 
else cast('@end_date' as date) end) as cohort_end_date

FROM #dates INNER JOIN #inclusions
ON #dates.person_id = #inclusions.person_id
AND #dates.cohort_start_date = #inclusions.cohort_start_date

WHERE #dates.cohort_start_date <= #dates.cohort_end_date 
AND #dates.cohort_start_date <= '@end_date'
AND #dates.cohort_start_date <= #dates.age_max_date

GROUP BY #dates.person_id 
;
"

# now get the first valid date and insert into cohort table
connectionHandler$executeSql(
  sql,
  cohort_definition_id = targets[[i]]$targetId, # qrisk_t_male
  cohort_schema = cohortDatabaseSchema,
  cohort_table = cohortTable,
  end_date = targets[[i]]$endDate
)

# clean up
connectionHandler$executeSql(
  "DROP TABLE #dates;
DROP TABLE #inclusions;
")

  }
}


