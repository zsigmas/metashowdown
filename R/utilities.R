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
#' @param weigth_stat probability of each type, pass as vector inside list list(c(1,2,3), c(3,2,4))
#' @param seq_prop_stop proportion at each peek, pass as vector inside list list(c(1,2,3), c(3,2,4))
#' @param seq_alpha_stop criticial alpha at each peek, pass as vector inside list list(c(1,2,3), c(3,2,4))
#'
#' @export
#'

create_paramdf <- function(k, delta, tau, fixed.n, weight_stat, seq_prop_stop, seq_alpha_stop) {
  param_df <- expand_grid(k, delta, tau, fixed.n, weight_stat, seq_prop_stop, seq_alpha_stop)

}

#' create a dummy parameter dataframe
#'
#' @export

dummy_paramdf <- function() {
  create_paramdf(k=c(10,5,20), delta=c(0,1), tau=c(0), fixed.n=NA,
                 weight_stat = list(c(1,2,0),c(4,5,0)),
                 seq_prop_stop = list(c(.5,1),c(.25,1)),
                 seq_alpha_stop = list(c(.005,.05),c(.01,.05))
                 )
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

dummy_MA <- function(k=3) {
  genMA(k,0,1,NA)
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

#' Dummy study
#'
#' @export

dummy_study <- function(delta=0) {
  study <-  c(rnorm(30), rnorm(30)+delta)
}


