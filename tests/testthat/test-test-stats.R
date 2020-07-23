test_that("t-test computation", {
  s1 = rnorm(50)
  s2 = rnorm(50)
  r = compute_t_test(c(s1,s2))
  t = t.test(s1,s2, alternative = 'greater', var.equal = T)
  expect_equal(t$p.value, r$p)
  expect_equal(as.vector(t$statistic), r$t) #As.vector is to remove a warning due to it not being a vector
})


test_that("t-test power", {

})

