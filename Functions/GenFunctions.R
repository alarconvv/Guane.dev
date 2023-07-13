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



#Function messages
#Modified from https://gitlab.com/hedsnz/hedsnz-public/-/blob/main/r/ggplot-shiny-messages.R
messageCatch <- function(objectReact){
  
  # try to print x() -- this generates messages if any are produced
  # define global lists for messages,
  # warnings and errors
  msg_list <-err_list <- wrn_list <- list() 
  
  # define global counters for messages,
  # warnings and errors
  err_num <- wrn_num <- msg_num <- 1
  all_msgs <- NULL
  
  
  value <- tryCatch(objectReact,
                    warning = function(w) {
                      if (w$message == "") {
                        return(NULL)
                      } else {
                        wrn_time <- paste0(Sys.time())
                        wrn_list[[wrn_time]] <<- paste0("Warning ", wrn_num, ": ", w$message)
                        wrn_num <<- wrn_num + 1
                        all_msgs <<- c(wrn_list,err_list)
                        return(all_msgs[rev(order(as.POSIXct(names(all_msgs))))])
                      }
                      
                    }, error = function(e) {
                      if (e$message == "") {
                        return(NULL)
                      } else {
                        err_time <- paste0(Sys.time())
                        err_list[[err_time]] <<- paste0("Error ", err_num, ": ", e$message)
                        err_num <<- err_num + 1
                        all_msgs <<- c(wrn_list,err_list)
                        return(all_msgs[rev(order(as.POSIXct(names(all_msgs))))])
                      }
                    },
                    msg = function(m) {
                      if (m$message == "") {
                        return(NULL)
                      } else {
                        msg_time <- paste0(Sys.time())
                        msg_list[[msg_time]] <<- paste0("msg", msg_num, ": ", m$message)
                        msg_num <<- msg_num + 1
                        all_msgs <<- c(wrn_list,err_list)
                        return(all_msgs[rev(order(as.POSIXct(names(all_msgs))))])
                      }
                      
                    }
                    
  )
  
  
  errorlist <- list(value= value, errorMessage= all_msgs)
  
  if(is.null(all_msgs)){
    return("No errors")
  }else{
    return(print(errorlist$errorMessage))
  }
  
}
