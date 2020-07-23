#' Compute a two-sample t-test
#'
#' We don't use the one by R to speed up the computation, it only computes the alternative of the difference being greater than 0 (i.e. one-tailed)
#'
#' @param study a study with one of the samples
#' @param alpha alpha of the test
#' @param tails number of tails
#'
#' @export

compute_t_test <- function(study, tails=1){

  #TODO add tails
  #TODO power should be refactored in an independent function to be reused

  # Compute values for test
  n1 = length(study)/2
  n2 = n1
  idx1 = 1:n1
  idx2 = (n1+1):(n1+n2)
  m1 = mean(study[idx1])
  m2 = mean(study[idx2])
  s1 = sd(study[idx1])
  s2 = sd(study[idx2])
  v1 = var(study[idx1])
  v2 = var(study[idx2])
  df = 2*n1-2

  # Compute test
  sdp = sqrt((s1^2/n1)+(s2^2/n2))
  t = (m1-m2)/sdp
  p = pt(t, df = df, lower.tail = F)

  #Power and Effect Size
  vp = sqrt(((n1 - 1) * v1 + (n2 - 1) * v2) / df)
  d = (m1-m2)/vp

  return(list(p=p,t=t))
}


