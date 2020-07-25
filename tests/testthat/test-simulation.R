test_that("run simulation", {

  #TODO Create one data from which we know the results and work from there (Maybe fix the random seed)

  param_df <- dummy_paramdf()

  r <- run_simulation(param_df)

  expect_equal(nrow(r), sum(param_df$k))
  expect_false(all(is.na(r$id)))

  expect_true(length(unique(r$ns))!=1)

  comp_r <- r %>% group_by(id) %>% summarise(k = length(id),
                                             delta = mean(opt_delta),
                                             tau = mean(opt_tau)) %>%
    select(-id)

  expect_equal(comp_r, select(param_df, -fixed.n))


})

test_that("simulate one MA", {

  set.seed(1)

  expect_cols = c('ns', 'opt_delta', 'opt_tau', 'emp_tau', 'emp_delta','s', 'p', 'd', 't', 'df','stat_type', 'id')

  k=2
  d=0
  tau=1
  fixed.n = NA
  id=NA
  drop_sample=F

  r_MA <- simulate_oneMA(k,d,tau,fixed.n,id,drop_sample)

  expect_true("s" %in% colnames(simulate_oneMA(k,d,tau,fixed.n,id,F)))
  expect_false("s" %in% colnames(simulate_oneMA(k,d,tau,fixed.n,id,T)))
  expect_equal(simulate_oneMA(k,d,tau,fixed.n,1,F)[[1, 'id']],1)
  expect_equal(simulate_oneMA(k,d,tau,fixed.n,NA,F)[[1, 'id']],NA)
  expect_equal(nrow(r_MA),k)
  expect_equal(sort(expect_cols), sort(colnames(simulate_oneMA(k,d,tau,fixed.n,id,F))))

  #Hand calculated tests it will fail in the future when we add new tests.

  r <- r_MA[1,]
  s <- unlist(r[[1,'s']])
  sl <- length(s)
  s1 <- s[1:(sl/2)]
  s2 <- s[(1+(sl/2)):sl]
  t = t.test(s1,s2, alternative = 'greater', var.equal = T)
  expect_equal(t$p.value, r$p)
  expect_equal(as.vector(t$statistic), r$t)

  r <- r_MA[2,]
  s <- unlist(r[[1,'s']])
  sl <- length(s)
  s1 <- s[1:(sl/2)]
  s2 <- s[(1+(sl/2)):sl]
  t = t.test(s1,s2, alternative = 'greater', var.equal = T)
  expect_equal(t$p.value, r$p)
  expect_equal(as.vector(t$statistic), r$t)

  #TODO Create one data from which we know the results and work from there (Maybe fix the random seed)

})

test_that("run_t_test", {
  s <- genMA(1,0,0,NA)
  comp <- compute_t_test(unlist(s[[1,'s']]))

  r <- run_t_test(s) %>% pivot_longer(c('p', 't', 'd', 'df'),names_to = 'n', values_to = 'v') %>% select(n, v)
  c <- r[['v']]
  names(c) <- r[['n']]

  expect_equivalent(comp,c)

  s <- genMA(2,0,0,NA)
  comp1 <- compute_t_test(unlist(s[[1,'s']]))
  comp2 <- compute_t_test(unlist(s[[2,'s']]))

  r <- run_t_test(s) %>% pivot_longer(c('p', 't', 'd', 'df'),names_to = 'n', values_to = 'v') %>% select(n, v)
  c <- r[['v']]
  names(c) <- r[['n']]

  expect_equivalent(c(comp1, comp2),c)
})

test_that("run_stat", {

  set.seed(1)

  stat_types_true = c('t_test')
  stat_types_error = c('bayes_factor', 'seq_ttest')
  s <- genMA(1,0,0,NA)

  for(st in stat_types_true){
    r <- run_stats(s, st)
    expect_true(all(r[['stat_type']]==st))
  }

  for(st in stat_types_error){
    expect_error(run_stats(s, st))
  }
})




