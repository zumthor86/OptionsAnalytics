get_option_epic <- function(strike, option_type = 'C', expiry){

  glue::glue("OP.D.{.codes[expiry]}.{strike}{option_type}.IP")

}
