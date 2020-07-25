#' Compute a two-sample t-test
#'
#' We don't use the one by R to speed up the computation, it only computes the alternative of the difference being greater than 0 (i.e. one-tailed)
#'
#' @param study a vector representing a study with 2n entries, n for each of the groups
#' @param tails number of tails (TODO)
#'
#' @export

compute_t_test <- function(study, tails=1){
  #TODO add tails

  # Compute values for test
  n1 <-  length(study)/2
  n2 <-  n1
  idx1 <-  1:n1
  idx2 <-  (n1+1):(n1+n2)
  m1 <-  mean(study[idx1])
  m2 <-  mean(study[idx2])
  s1 <-  sd(study[idx1])
  s2 <-  sd(study[idx2])
  v1 <-  var(study[idx1])
  v2 <-  var(study[idx2])
  df <-  2*n1-2

  # Compute test
  sdp = sqrt((s1^2/n1)+(s2^2/n2))
  t = (m1-m2)/sdp
  p = pt(t, df = df, lower.tail = F)

  #Power and Effect Size

  d = compute_power(m1, m2, n1, n2, v1, v2, df)

  return(list(p=p,t=t, d=d, df=df))
}


#' Compute stats for posterior statistical tests
#'
#' @param m1 mean of the group 1
#' @param m2 mean of the group 2
#' @param n1 number of participants in group 1
#' @param n2 number of participants in group 2
#' @param v1 variance of group 1
#' @param v2 variance of group 2
#' @param df degrees of freedom

compute_power <- function(m1, m2, n1, n2, v1, v2, df) {
  d  <-  (m1-m2)/sqrt(((n1 - 1) * v1 + (n2 - 1) * v2) / df)
}


#' Compute a sequential t-test with a pre specified number of stops and a given alpha
#'
#' @param study a vector representing a study with 2n entries, n for each of the groups
#' @param prop_stop at which proportion of all data will we peek
#' @param alpha_stop alpha in each of the peeks
#'
#' In case the proportion goes below one it automatically chooses one more
#'
#' @export

compute_seq_t_test <- function(study, prop_stop, alpha_stop){

  logdebug('Computing Sequential t test', logger = 'compute_seq_t_test')

  n1 <-  length(study)/2
  n2 <-  n1

  logdebug(glue::glue('Size per group total:{length(study)}, n1:{n1}, n2:{n2}'), logger = 'compute_seq_t_test')

  for(i in 1:length(prop_stop)){
    beg1 <- 1
    beg2 <- (n1+1)
    end1 <- max(2,round(n1*prop_stop[i])) # Max to avoid picking just one subject
    end2 <- beg2 + max(2, round(n2*prop_stop[i])) - 1 # Otherwise we take one extra #Max same as above
    this_study <- c(study[beg1:end1],study[beg2:end2])
    r <- compute_t_test(this_study)

    logdebug(glue::glue('{i} stop'), logger = 'compute_seq_t_test')
    logdebug(glue::glue('Group1 [{beg1}:{end1}]'), logger = 'compute_seq_t_test')
    logdebug(glue::glue('Group1 [{beg2}:{end2}]'), logger = 'compute_seq_t_test')
    logdebug(glue::glue('Result p:{r$p}, d:{r$d}, df:{r$df}, prop:{prop_stop[i]}'), logger = 'compute_seq_t_test')


    if(r$p<alpha_stop[i]){
      break
    }
  }

  return(c(r, prop=prop_stop[i]))
}

