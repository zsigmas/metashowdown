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
                         weight_stat = param_df[['weight_stat']],
                         seq_prop_stop = param_df[['seq_prop_stop']],
                         seq_alpha_stop = param_df[['seq_alpha_stop']],
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
#' @param seq_prop_stop a vector with the proportions at which we will peek in the case of a sequential t test (Single value)
#' @param seq_prop_alpha a vector with the critical alpha value at each peek in the case of a sequential t test (Single Value)
#'
#'
#' @export


simulate_oneMA <- function(k, delta, tau,
                           #qrpEnv = c("none", "low", "medium", "high"),
                           #censorFunc = c("none", "medium", "high"),
                           #verbose = FALSE,
                           fixed.n = NA,
                           #seq_t = list(pr, seq_param)
                           weight_stat,
                           seq_prop_stop = NA,
                           seq_alpha_stop = NA,
                           id=NA,
                           drop_sample=F
){
  logdebug(glue::glue('Parameters: {params}', params=list2str(list(k=k,delta=delta,
                                                                   tau=tau,fixed.n=fixed.n,
                                                                   drop_sample=drop_sample,
                                                                   weight_stat=weight_stat,
                                                                   seq_prop_stop=seq_prop_stop,
                                                                   seq_alpha_stop=seq_alpha_stop))),
           logger='simulate_oneMA')

  logdebug('Generating MA studies')
  this_MA <- genMA(k, delta, tau, fixed.n)

  this_MA <- assign_stat(this_MA, weight_stat, seq_prop_stop, seq_alpha_stop)
  r <- run_stats(this_MA)

  #TODO put together and check that we can bind df with differents rows


  if(drop_sample){
    logdebug('Dropping Sample')
    r <- select(r, -s)} #Drop sample information

  r <- mutate(r, id=id)

  #TODO bayesian
  #TODO NOT NOW Add Questionable Research Practices
  #TODO NOT NOW Add Publication Bias

  return(r)
}

#' Run stats on each study
#'
#' @param ma_df_stat list of studies in the metanalysis with their stat assigned
#'
#' @export

run_stats <- function(ma_df_stat){
  logdebug(glue::glue('Running stats'),
           logger='run_stats')

  stat_list = list(t_test = run_t_test,
                   seq_t_test = run_seq_t_test)

  split_ma_df_stat <- split(ma_df_stat, ma_df_stat$stat_type)

  r <- lapply(1:length(split_ma_df_stat), c) # Strange way of initializing

  for(i in 1:length(split_ma_df_stat)){
    stat_type <- names(split_ma_df_stat)[i]
    r[[i]] <- stat_list[[stat_type]](split_ma_df_stat[[i]])
    r[[i]] <- mutate(r[[i]], stat_type = stat_type)
  }
r <- bind_rows(r)

}

#' Run the t-test for the studies in the dataframe
#'
#' @param ma_df dataframe containing the metanalysis
#'
#' @export

run_t_test <- function(ma_df) {
  logdebug('Running t_test',
           logger='run_t_test')
  mutate(ma_df, r = (lapply(s, function(y){compute_t_test(y)}))) %>% unnest_wider(r)
}


#' Run the seq test for all the studies in the dataframe
#'
#' @param ma_df dataframe containing the metanalysis and the prop_stop and alpha_stop per study
#'
#' @export

run_seq_t_test <- function(ma_df) {
  logdebug('Running seq_t_test',
           logger='run_seq_t_test')
  mutate(ma_df, r = mapply(compute_seq_t_test, study=s, prop_stop=seq_prop_stop, alpha_stop=seq_alpha_stop, SIMPLIFY = F)) %>% unnest_wider(r)
}


#' Assign stat
#'
#' @param ma_df dataframe with the studies
#' @param weight_stat a vector with the expected weight for t_test, seq_t_test and bayes for each test in the metaanalysis
#' @param seq_prop_stop a vector with the proportions at which we will peek in the case of a sequential t test (Single value)
#' @param seq_prop_alpha a vector with the critical alpha value at each peek in the case of a sequential t test (Single Value)
#'
#' For non-sequential t-test seq* parameters are set to NA

assign_stat <- function(ma_df, weight_stat, seq_prop_stop=NULL, seq_alpha_stop=NULL) {

  assertthat::assert_that(weight_stat[3]==0, msg = 'No bayes please! Not implemented!')
  assertthat::assert_that(weight_stat[2]==0 | (weight_stat[2]!=0 & !is.null(seq_prop_stop) & !is.null(seq_alpha_stop)),
                          msg = 'If sequential t_test then seq_prop_stop and seq_alpha_stop must not be null')

  if(is.null(seq_prop_stop)){seq_prop_stop=NA}
  if(is.null(seq_alpha_stop)){seq_alpha_stop=NA}

  available_stats = c('t_test', 'seq_t_test', 'bayes')
  ma_df <- mutate(ma_df,
                  stat_type=sample(available_stats, size = nrow(ma_df), replace=TRUE, prob=weight_stat),
                  seq_prop_stop = lapply(stat_type, function(st){if(st=='seq_t_test'){!!seq_prop_stop}else{NA}}),
                  seq_alpha_stop = lapply(stat_type, function(st){if(st=='seq_t_test'){!!seq_alpha_stop}else{NA}}))
}
