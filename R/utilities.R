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

create_paramdf <- function(k, delta, tau) {
  param_df <- expand_grid(k, delta, tau)
}

#' create a dummy parameter dataframe
#'
#' @export
#'

create_dummy_paramdf <- function() {
  create_paramdf(k=c, delta)
}
