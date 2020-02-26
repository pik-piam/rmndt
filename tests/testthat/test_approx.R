test_that("Extrapolation works as expected", {
  dt <- as.data.table(ChickWeight)
  dt[Chick == 1 & Time > 0, weight := NA]

  extra <- approx_dt(
    dt, 0:21, "Time", "weight", idxcols=c("Chick", "Diet"), extrapolate = TRUE)

  expect_equal(nrow(extra[Chick == 1]), 22)
  expect_true(all(extra[Chick == 1]$weight == extra[Chick == 1 & Time == 0]$weight))

})

test_that("Interpolation works as expected", {
  dt <- data.table(x=c(1,3,5), y=c(1,3,7))

  extra <- approx_dt(dt, 1:5, "x", "y")

  expect_equal(extra[x == 2]$y, 2)
  expect_equal(extra[x == 4]$y, 5)

})
