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

  return(list(p=p,t=t, d=d))
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

