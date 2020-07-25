#' Collapse a one level list into a string
#'
#' @param l list to be collapsed
#' @param sep same as in paste
#' @param collapse same as in paste

list2str <- function(l, sep=':', collapse= ' ') {
  paste(names(l), l, sep=':', collapse = ' ')
}


#' Create a parameter dataframe
#'
#' @param k the number of studies in the MA (NULL if you w)
#' @param delta the true effect (or the average of the true effects if heterogeneity exists)
#' @param tau the SD around the true effect
#'
#' @export
#'

create_paramdf <- function(k, delta, tau, fixed.n) {
  param_df <- expand_grid(k, delta, tau, fixed.n)
}

#' create a dummy parameter dataframe
#'
#' @export

dummy_paramdf <- function() {
  create_paramdf(k=c(1,2,3), delta=c(0,1), tau=c(0), fixed.n=NA)
}

#' Export variables to launch a simple script
#'
#' @export
#'

dummy_env <- function() {
  k<<-2
  delta<<-1
  tau<<-1
  fixed.n<<-NA
}

#' Create one MA data
#'
#' @export

dummy_MA <- function() {
  genMA(3,0,1,NA)
}


#' Set verbose mode
#'
#' @export

set_verbose <- function(verbose=F) {
  if(verbose){
    logging::basicConfig(level=logging::loglevels['DEBUG'])
  }else{
    logging::basicConfig(level=logging::loglevels['INFO'])
  }
}


