
cfg <- \(){ jsonlite::fromJSON(Sys.getenv("SR_CONFIG")) }

mksock    <- purrr::compose(~ socketConnection(.[[1]],.[[2]],blocking=T), ~ strsplit(.,":")[[1]])
sr_output <- \(){ mksock(Sys.getenv("SR_OUTPUT"),":") }
sr_input  <- \(){ mksock(Sys.getenv("SR_INPUT"),":") }


flow_next <- function(type){
  safejs <- purrr::possibly(jsonlite::parse_json,otherwise=NULL)

  while(TRUE){
    line  <- readLines(sr_input,n=1)  
    event <- safejs(line,auto_unbox=T)
    if(!is.null(event) && event$type==type){
      writeLines(line, con=sr_output)
      return(event) 
    }
  }
}