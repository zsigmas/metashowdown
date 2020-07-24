#' Main simulation call
#'
#' @param param_df, dataframe containing the parameters to call simulate_oneMA
#' @param production a boolean flag to disthinguish when  we are in production or testing mode
#'
#' @export

run_simulation <- function(param_df, production = T) {

  loginfo('Lanzando simulación', logger='run_simulation')

  pbapply::pbmapply(simulate_oneMA,
                    k=param_df[['k']],
                    delta=param_df[['delta']],
                    tau=param_df[['tau']]
  )

  loginfo('Finalizando simulación', logger='run_simulation')
}


#' Simulate one MA
#'
#' @param k the number of studies in the MA
#' @param delta the true effect (or the average of the true effects if heterogeneity exists)
#' @param tau the SD around the true effect
#' @param censor The censoring function - either "none", "med" (medium publication bias), "high" (high publication bias), or a vector of 3 values for the censoring function (posSign_NS_baseRate, negSign_NS_baseRate, counterSig_rate) *NOT SUPPORTED*
#' @param qrpEnv the qrp environment that produced the literature: 'none', 'low', 'med', 'high'  *NOT SUPPORTED*
#' @param seq_t A list with a field called pr, which contains which proportion of the studies in the metanalisys used a sequential analisys technique and, a seq param df with two fields, pv the critical p value of each peeking and pr the proportion of observations of each peek. *NOT SUPPORTED*
#'
#' @export


simulate_oneMA <- function(k, delta, tau,
                           qrpEnv = c("none", "low", "medium", "high"),
                           censorFunc = c("none", "medium", "high"),
                           verbose = FALSE,
                           fixed.n = NULL,
                           seq_t = list(pr, seq_param)
){
  logdebug(glue::glue('Parameters: {params}', params=list2str(as.list(match.call()))),
                      logger='simulate_oneMA')
  logdebug('Generating MA studies')
  this_MA <- genMA(k, delta, tau, fixed.n)

  r <- compute_t_test(this_MA)

  #TODO seq_tvalue
  #TODO bayesian
  #TODO NOT NOW Add Questionable Research Practices
  #TODO NOT NOW Add Publication Bias

  return(r)
}
