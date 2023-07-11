#----------------Module: UI ---------------------------

# Guane: Shiny app for phylogenetic comparative methods
# Author: Viviana Romero 
# Creation Date: July 7 2023 
# Generic Fuctions

#----------------Module: UI ---------------------------

is.nexus<- function(x){
  pathDiver <- file(description = x,open = "r")
  first_lineDiver <- readLines(pathDiver,n = 2)
  findNexusDiver <- grep(pattern = '#NEXUS|#Nexus|#nexus',x = first_lineDiver )
  close(pathDiver)
  if (length(findNexusDiver) == 1){
    return(TRUE)
  }else{return(FALSE)}
}