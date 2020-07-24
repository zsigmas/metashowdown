test_that("run simulation", {

  #Create one data from which we know the results and work from there (Maybe fix the random seed)

})

test_that("simulate one MA", {

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




