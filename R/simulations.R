#' Main simulation call
#'
#' @param param_df, dataframe containing the parameters to call simulate_oneMA
#' @param production a boolean flag to disthinguish when we are in production or testing mode
#'
#' @export

run_simulation <- function(param_df, production = T) {

  loginfo('Lanzando simulación', logger='run_simulation')
  r <- bind_rows(pbapply::pbmapply(simulate_oneMA,
                         k=param_df[['k']],
                         delta=param_df[['delta']],
                         tau=param_df[['tau']],
                         fixed.n = param_df[['fixed.n']],
                         id = 1:nrow(param_df),
                         MoreArgs = list(drop_sample=T),
                         SIMPLIFY = F
  ))

  return(r)
}


#' Simulate one MA
#'
#' @param k the number of studies in the MA
#' @param delta the true effect (or the average of the true effects if heterogeneity exists)
#' @param tau the SD around the true effect
#' @param censor The censoring function - either "none", "med" (medium publication bias), "high" (high publication bias), or a vector of 3 values for the censoring function (posSign_NS_baseRate, negSign_NS_baseRate, counterSig_rate) *NOT SUPPORTED*
#' @param fixed.n number of participants *PER GROUP* (groups are always balanced), if NA it is extracted from a distribution (see getN)
#' @param qrpEnv the qrp environment that produced the literature: 'none', 'low', 'med', 'high'  *NOT SUPPORTED*
#' @param seq_t A list with a field called pr, which contains which proportion of the studies in the metanalisys used a sequential analisys technique and, a seq param df with two fields, pv the critical p value of each peeking and pr the proportion of observations of each peek. *NOT SUPPORTED*
#' @param id an optional numerical id for the metanalysis, by default NA
#' @param drop_sample an optional flag to drop or retain the sample in each study
#'
#'
#' @export


simulate_oneMA <- function(k, delta, tau,
                           #qrpEnv = c("none", "low", "medium", "high"),
                           #censorFunc = c("none", "medium", "high"),
                           #verbose = FALSE,
                           fixed.n = NA,
                           #seq_t = list(pr, seq_param)
                           id=NA,
                           drop_sample=F
){

  logdebug(glue::glue('Parameters: {params}', params=list2str(list(k=k,delta=delta,
                                                                   tau=tau,fixed.n=fixed.n,
                                                                   drop_sample=drop_sample))),
           logger='simulate_oneMA')

  logdebug('Generating MA studies')
  this_MA <- genMA(k, delta, tau, fixed.n)

  stat_type = 't_test'

  r <- run_stats(this_MA, stat_type)

  if(drop_sample){
    logdebug('Dropping Sample')
    r <- select(r, -s)} #Drop sample information

  r <- mutate(r, id=id)

  #TODO seq_tvalue
  #TODO bayesian
  #TODO NOT NOW Add Questionable Research Practices
  #TODO NOT NOW Add Publication Bias

  return(r)
}

#' Run stats on each study
#'
#' @param ma_df ma_df over which we want to apply the stats
#' @param stat_type type of stats we want to apply c=('t_test')
#'
#' @export

run_stats <- function(ma_df, stat_type=c('t_test')){
  logdebug(glue::glue('Selected: {stat_type}', stat_type=stat_type),
           logger='run_stats')

  stat_list = list(t_test = run_t_test)

  r <- stat_list[[stat_type]](ma_df)
  r <- mutate(r, stat_type = stat_type)
}

#' Run the test for all the studies in a single metaanalysis
#'
#' @param ma_df dataframe containing the metanalysis
#'
#' @export

run_t_test <- function(ma_df) {
  logdebug('Running single_t_test',
           logger='run_t_test')
  mutate(ma_df, r = (lapply(s, function(y){compute_t_test(y)}))) %>% unnest_wider(r)
}
