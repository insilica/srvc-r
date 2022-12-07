
globals <- new.env()

#' @description get the configuration for the current stage
#' @export
config <- \(){ jsonlite::fromJSON(Sys.getenv("SR_CONFIG")) }

#' @description initialize an SRVC stage
#' @export
initialize <- function(){
  mksock <- purrr::compose(~ socketConnection(.[[1]],.[[2]],blocking=T), ~ strsplit(.,":")[[1]])
  globals$sr_output <- mksock(Sys.getenv("SR_OUTPUT"),":") 
  globals$sr_input <- mksock(Sys.getenv("SR_INPUT"),":") 
  withr::defer_parent({close(globals$sr_output); close(globals$sr_input)})
}

#' @description get an event from the SRVC flow
#' @param predicate predicate function for filtering events, events that match predicate are returned
#' @export
next_event <- function(predicate){
  safejs <- purrr::possibly(jsonlite::parse_json,otherwise=NULL)

  while(TRUE){
    line  <- readLines(globals$sr_input,n=1)  
    event <- safejs(line,auto_unbox=T)
    if(!is.null(event) && predicate(event)){
      write_event(line)
      return(event) 
    }
  }
}

#' @description write an event to the SRVC flow
#' @param event event to write
#' @export
write_event <- function(event){
  writeLines(event, con=globals$sr_output)
}