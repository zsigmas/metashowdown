test_that("genMA returns the correct number of studies", {
  expect_equal(nrow(genMA(0,0,0,3)), 0)
  expect_equal(nrow(genMA(1,0,0,3)), 1)
  expect_equal(nrow(genMA(10,0,0,3)), 10)
  expect_equal(nrow(genMA(0,0,0,NA)), 0)
  expect_equal(nrow(genMA(1,0,0,NA)), 1)
  expect_equal(nrow(genMA(10,0,0,NA)), 10)
})

test_that("genMA returns the correct number of participants per study", {
  n=0
  expect_equal(length(genMA(1,0,0,n)[[1,'s']][[1]]), 2*n)
  n=10
  expect_equal(length(genMA(1,0,0,n)[[1,'s']][[1]]), 2*n)
})



test_that("genMA distribution is correct (Fails probabilistically 5% of the time)", {
  n=1e4
  np = 1e3
  d=0
  t=0

  p <- genMA(n,d,t,np) %>%
    mutate(id=1:n) %>%
    unnest(s) %>%
    group_by(id) %>%
    summarise(s=mean(s), emp_delta=mean(emp_delta)) %>%
    summarise(avg=mean(s), sd=sd(s))

  expect_equal(p[['avg']], d, tolerance=1e-1)
  expect_equal(p[['sd']], t, tolerance=1e-1)
  ###################

  d=0
  t=3

  p <- genMA(n,d,t,np) %>%
    mutate(id=1:n) %>%
    unnest(s) %>%
    group_by(id) %>%
    summarise(s=mean(s), emp_delta=mean(emp_delta)) %>%
    summarise(avg=mean(s), sd=sd(s))

  expect_equal(p[['avg']], d, tolerance=t*2.58/sqrt(n)) #95% capture this is not well calculated but it works...
  expect_equal(p[['sd']], t, tolerance=1e-1)

})
