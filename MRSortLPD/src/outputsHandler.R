# TODO depending on whether the file was generated from a description based on
# XMCDA v2 or v3, only one list is correct, either XMCDA_v2_TAG_FOR_FILENAME
# or XMCDA_v3_TAG_FOR_FILENAME: check them to determine which one should be
# adapted.

XMCDA_v2_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v2 tag
  alternativesAffectations = "alternativesAffectations",
  messages = "methodMessages"
)

XMCDA_v3_TAG_FOR_FILENAME <- list(
  # output name -> XMCDA v3 tag
  alternativesAffectations = "alternativesAffectations",
  messages = "programExecutionResult"
)

xmcda_v3_tag <- function(outputName){
  return (XMCDA_v3_TAG_FOR_FILENAME[[outputName]])
}

xmcda_v2_tag <- function(outputName){
  return (XMCDA_v2_TAG_FOR_FILENAME[[outputName]])
}


convert <- function(results, programExecutionResult) {
  
  xmcdaAlternativesAssignments<-.jnew("org/xmcda/XMCDA")  
  
  tmp<-handleException(
    function() return(
      putAlternativesAssignments(xmcdaAlternativesAssignments,results$assignments)
    ),
    programExecutionResult,
    humanMessage = "Could not put assignments in tree, reason: "
  )
  
  # if an error occurs, return null, else a dictionnary "xmcdaTag -> xmcdaObject"
  
  if (is.null(tmp)){
    return(null)
  } else{
    return (list(alternativesAssignments = xmcdaAlternativesAssignments))
  }
  

}
