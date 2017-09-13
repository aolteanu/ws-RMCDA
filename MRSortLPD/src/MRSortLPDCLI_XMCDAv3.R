# usage:
# R --slave --vanilla --file=MRSortLPDCLI_XMCDAv2.R --args "[inDirectory]" "[outDirectory]"

rm(list=ls())

# tell R to use the rJava package and the RXMCDA3 package

library(rJava)
library(XMCDA3)

# cf. http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
script.dir <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript
    return(dirname(normalizePath(sub(needle, "", cmdArgs[match]))))
  } else {
    # 'source'd via R console
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }
}

# load the R files in the script's directory
script.wd <- setwd(script.dir())
source("utils.R")
source("inputsHandler.R")
source("outputsHandler.R")
source("MRSortLPD.R")
# restore the working directory so that relative paths passed as
# arguments work as expected
if (!is.null(script.wd)) setwd(script.wd)

# get the in and out directories from the arguments

inDirectory <- commandArgs(trailingOnly=TRUE)[1]
outDirectory <- commandArgs(trailingOnly=TRUE)[2]

# Override the directories here: uncomment this when testing from inside R e.g.
# (uncomment this when testing from inside R e.g.)

#inDirectory <- "/path/to/MRSortLPD/tests/in1.v2"
#outDirectory <- "/path/to/MRSortLPD/tests/out_tmp/"

# filenames

alternativesFile <- "alternatives.xml"
criteriaFile <- "criteria.xml"
categoriesFile <- "categories.xml"
categoriesRanksFile <- "categoriesRanks.xml"
performanceTableFile <- "performanceTable.xml"
criteriaWeightsFile <- "criteriaWeights.xml"
majorityThresholdFile <- "majorityThreshold.xml"
categoriesProfilesFile <- "categoriesProfiles.xml"
categoriesProfilesPerformanceTableFile <- "categoriesProfilesPerformanceTable.xml"
vetoProfilesFile <- "vetoProfiles.xml"
vetoProfilesPerformanceTableFile <- "vetoProfilesPerformanceTable.xml"
dictatorProfilesFile <- "dictatorProfiles.xml"
dictatorProfilesPerformanceTableFile <- "dictatorProfilesPerformanceTable.xml"
assignmentRuleFile <- "assignmentRule.xml"
alternativesAffectationsFile <- "alternativesAffectations.xml"
messagesFile <- "messages.xml"

# the Java xmcda object for the output messages

xmcdaMessages<-.jnew("org/xmcda/XMCDA")
xmcdaData <- .jnew("org/xmcda/XMCDA")

loadXMCDAv3(xmcdaData, inDirectory, alternativesFile, mandatory = TRUE, xmcdaMessages, "alternatives")
loadXMCDAv3(xmcdaData, inDirectory, criteriaFile, mandatory = TRUE, xmcdaMessages, "criteriaScales")
loadXMCDAv3(xmcdaData, inDirectory, categoriesFile, mandatory = TRUE, xmcdaMessages, "categories")
loadXMCDAv3(xmcdaData, inDirectory, categoriesRanksFile, mandatory = TRUE, xmcdaMessages, "categoriesValues")
loadXMCDAv3(xmcdaData, inDirectory, performanceTableFile, mandatory = TRUE, xmcdaMessages, "performanceTable")
loadXMCDAv3(xmcdaData, inDirectory, criteriaWeightsFile, mandatory = FALSE, xmcdaMessages, "criteriaValues")
loadXMCDAv3(xmcdaData, inDirectory, majorityThresholdFile, mandatory = FALSE, xmcdaMessages, "programParameters")
loadXMCDAv3(xmcdaData, inDirectory, categoriesProfilesFile, mandatory = TRUE, xmcdaMessages, "categoriesProfiles")

nbProfiles <- 1

if(xmcdaData$categoriesProfilesList$size() == nbProfiles)
{
  categoryProfiles <- xmcdaData$categoriesProfilesList$get(as.integer(nbProfiles - 1))
  
  categoryProfiles$setMcdaConcept("bounding")
  
  xmcdaData$categoriesProfilesList$set(as.integer(nbProfiles - 1), categoryProfiles)

  nbProfiles <- nbProfiles + 1
}

loadXMCDAv3(xmcdaData, inDirectory, categoriesProfilesPerformanceTableFile, mandatory = TRUE, xmcdaMessages, "performanceTable")
loadXMCDAv3(xmcdaData, inDirectory, vetoProfilesFile, mandatory = FALSE, xmcdaMessages, "categoriesProfiles")

if(xmcdaData$categoriesProfilesList$size() == nbProfiles)
{
  categoryProfiles <- xmcdaData$categoriesProfilesList$get(as.integer(nbProfiles - 1))
  
  categoryProfiles$setMcdaConcept("veto")
  
  xmcdaData$categoriesProfilesList$set(as.integer(nbProfiles - 1), categoryProfiles)
  
  nbProfiles <- nbProfiles + 1
}

loadXMCDAv3(xmcdaData, inDirectory, vetoProfilesPerformanceTableFile, mandatory = FALSE, xmcdaMessages, "performanceTable")
loadXMCDAv3(xmcdaData, inDirectory, dictatorProfilesFile, mandatory = FALSE, xmcdaMessages, "categoriesProfiles")

if(xmcdaData$categoriesProfilesList$size() == nbProfiles)
{
  categoryProfiles <- xmcdaData$categoriesProfilesList$get(as.integer(nbProfiles - 1))
  
  categoryProfiles$setMcdaConcept("dictator")
  
  xmcdaData$categoriesProfilesList$set(as.integer(nbProfiles - 1), categoryProfiles)
  
  nbProfiles <- nbProfiles + 1
}

loadXMCDAv3(xmcdaData, inDirectory, dictatorProfilesPerformanceTableFile, mandatory = FALSE, xmcdaMessages, "performanceTable")
loadXMCDAv3(xmcdaData, inDirectory, assignmentRuleFile, mandatory = TRUE, xmcdaMessages, "programParameters")

# if we have problem with the inputs, it is time to stop

if (xmcdaMessages$programExecutionResultsList$size() > 0){
  if (xmcdaMessages$programExecutionResultsList$get(as.integer(0))$isError()){
    writeXMCDA(xmcdaMessages, paste(outDirectory,messagesFile, sep="/"))
    stop(paste("An error has occured while loading the input files. For further details, see ", messagesFile, sep=""))
  }
}

# let's check the inputs and convert them into our own structures

inputs <- handleException(
  function() return(
    checkAndExtractInputs(xmcdaData, programExecutionResult)
  ),
  xmcdaMessages,
  humanMessage = "Errors in the input data, reason: "
)

if (xmcdaMessages$programExecutionResultsList$size()>0){
  if (xmcdaMessages$programExecutionResultsList$get(as.integer(0))$isError()){
    writeXMCDA(xmcdaMessages, paste(outDirectory,messagesFile, sep="/"))
    stop(paste("An error has occured while checking and extracting the inputs. For further details, see ", messagesFile, sep=""))
  }
}

# here we know that everything was loaded as expected

# now let's call the calculation method

results <- handleException(
  function() return(
    MRSortLPD(inputs)
  ),
  xmcdaMessages,
  humanMessage = "The calculation could not be performed, reason: "
)

if (is.null(results)){
  writeXMCDA(xmcdaMessages, paste(outDirectory,messagesFile, sep="/"))
  stop("Could not calculate MRSortLPD.")
}

# fine, now let's put the results into XMCDA structures

xResults = convert(results, xmcdaMessages)

if (is.null(xResults)){
  writeXMCDA(xmcdaMessages, paste(outDirectory,messagesFile, sep="/"))
  stop("Could not convert MRSortLPD results into XMCDA")
}

# and last, write them onto the disk

for (i in 1:length(xResults)){
  outputFilename = paste(outDirectory, paste(names(xResults)[i],".xml",sep=""), sep="/")
  tmp <- handleException(
    function() return(
      writeXMCDA(xResults[[i]], outputFilename, xmcda_v3_tag(names(xResults)[i]))
    ),
    xmcdaMessages,
    humanMessage = paste("Error while writing ", outputFilename,", reason :")
  )

  if (is.null(tmp)){
    writeXMCDA(xmcdaMessages, paste(outDirectory,messagesFile, sep="/"))
    stop("Error while writing ",outputFilename,sep="")
  }
}

# then the messages file
# TODO faire autrement qu'en ajoutant une info vide pour faire un <status>ok</status>

tmp <- handleException(
  function() return(
    putProgramExecutionResult(xmcdaMessages, infos="")
  ),
  xmcdaMessages
)

if (is.null(tmp)){
  writeXMCDA(xmcdaMessages, paste(outDirectory,messagesFile, sep="/"))
  stop("Could not add methodExecutionResult to tree.")
}

tmp <- handleException(
  function() return(
    writeXMCDA(xmcdaMessages, paste(outDirectory,messagesFile, sep="/"))
  ),
  xmcdaMessages
)

if (is.null(tmp)){
  writeXMCDA(xmcdaMessages, paste(outDirectory,messagesFile, sep="/"))
  stop("Error while writing messages file.")
}
