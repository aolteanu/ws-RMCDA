checkAndExtractInputs <- function(xmcdaData, programExecutionResult) {
    # all parameters in the order in which the R MCDA function takes them

    performanceTable <- NULL
    categoriesLowerProfiles <- NULL
    categoriesRanks <- NULL
    criteriaWeights <- NULL
    criteriaMinMax <- NULL
    majorityThreshold <- NULL
    criteriaVetos <- NULL
    criteriaDictators <- NULL
    majorityRule <- NULL
    alternativesIDs <- NULL
    criteriaIDs <- NULL
    categoriesIDs <- NULL

    #############################################
    # get criteria
    #############################################
    
    criteriaIDs <- getActiveCriteria(xmcdaData)$criteriaIDs
    
    #############################################
    # get weights
    #############################################
    
    criteriaWeights <- getNumericCriteriaValuesList(xmcdaData)[[1]]
    
    #############################################
    # get preference directions
    #############################################
    
    criteriaMinMax <- getCriteriaPreferenceDirectionsList(xmcdaData)[[1]]
    
    #############################################
    # get categories
    #############################################

    categoriesIDs <- getActiveCategories(xmcdaData)$categoriesIDs

    #############################################
    # get performance table
    #############################################
    
    performanceTableList <- getNumericPerformanceTableList(xmcdaData)
    
    # we assume that the first performance table is the actual performance table
    
    if(length(performanceTableList) == 0)
      stop("Error: no performance table supplied")
    
    performanceTable <- performanceTableList[[1]]
    
    #############################################
    # get alternatives
    #############################################
    
    activeIDs <- getActiveAlternatives(xmcdaData)$alternativesIDs
    
    alternativesIDs <- rownames(performanceTable)[rownames(performanceTable) %in% activeIDs]
    
    #############################################
    # get majority threshold - and assignment rule if it exists
    #############################################
    
    parameters <- getProgramParametersList(xmcdaData)
    
    if(length(parameters) == 0)
      stop("No method parameters provided (majority threshold)")
    else
    {
      for(i in 1:length(parameters))
      {
        param_name <- names(parameters[[i]])[1]
        if(param_name == "majority")
          majorityThreshold <- parameters[[i]]$majority[[1]]
        else if(param_name == "inrule")
          assignmentRule <- parameters[[i]]$inrule[[1]]
      }
    }
    
    # default majority rule to M if one was not provided
    
    if(is.null(majorityRule))
      majorityRule <- 'M'

    #############################################
    # get category profiles
    #############################################
    
    categoriesLowerProfilesPerformanceTableList <- getNumericCategoriesLowerProfilesPerformanceTableList(xmcdaData)
    
    if("bounding" %in% names(categoriesLowerProfilesPerformanceTableList))
      categoriesLowerProfiles <- categoriesLowerProfilesPerformanceTableList$bounding
    else
      stop("Error: no categories lower profiles have been provided (performance table too)")
    
    if("veto" %in% names(categoriesLowerProfilesPerformanceTableList))
      criteriaVetos <- categoriesLowerProfilesPerformanceTableList$veto
    else
    {
      if(assignmentRule %in% c('V','v','dV','Dv','dv'))
        stop("Error: no categories veto profiles have been provided when the provided assignmentRule requires them")
    }
    
    if("dictator" %in% names(categoriesLowerProfilesPerformanceTableList))
      criteriaDictators <- categoriesLowerProfilesPerformanceTableList$dictator
    else
    {
      if(assignmentRule %in% c('D','d','dV','Dv','dv'))
        stop("Error: no categories dictator profiles have been provided when the provided assignmentRule requires them")
    }
    
    #############################################
    # get categories ranks
    #############################################

    categoriesRanks <- getCategoriesValues(xmcdaData)$categoriesValues
    
    if(!all(sort(categoriesRanks) == 1:length(categoriesIDs)))
      stop('Error: categories ranks should be from 1 to the number of categories')
    
    return(list(performanceTable = performanceTable, categoriesLowerProfiles = categoriesLowerProfiles, categoriesRanks = categoriesRanks, criteriaWeights = criteriaWeights, criteriaMinMax = criteriaMinMax, majorityThreshold = majorityThreshold, criteriaVetos = criteriaVetos, criteriaDictators = criteriaDictators, majorityRule = majorityRule, alternativesIDs = alternativesIDs, criteriaIDs = criteriaIDs, categoriesIDs = categoriesIDs))
}
