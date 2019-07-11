slowExec <- function(pause, f,  ...){

  function(...){

    Sys.sleep(pause)

    f(...)

  }

}
