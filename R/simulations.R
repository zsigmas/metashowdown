#' Main simulation function
#'
#' @param k the number of studies in the MA
#' @param delta the true effect (or the average of the true effects if heterogeneity exists)
#' @param tau the SD around the true effect
#' @param censor The censoring function - either "none", "med" (medium publication bias), "high" (high publication bias), or a vector of 3 values for the censoring function (posSign_NS_baseRate, negSign_NS_baseRate, counterSig_rate)
#' @param qrpEnv the qrp environment that produced the literature: 'none', 'low', 'med', 'high'
#' @param seq_t A list with a field called pr, which contains which proportion of the studies in the metanalisys used a sequential analisys technique and, a seq param df with two fields, pv the critical p value of each peeking and pr the proportion of observations of each peek.
#'
#' @export


simulate <- function(k, delta, tau,
                     qrpEnv = c("none", "low", "medium", "high"),
                     censorFunc = c("none", "medium", "high"),
                     verbose = FALSE,
                     fixed.n = NULL,
                     seq_t = list(pr, seq_param)
                     ){
  this_MA = genMA(k, delta, tau, fixed.n)

  #TODO Calculate stats, tvalue, seq_tvalue, bayesian
  #TODO Add Questionable Research Practices
  #TODO Add Publication Bias

}
