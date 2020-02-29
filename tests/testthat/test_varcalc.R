test_that("Check if vertical calculation along a specific column works.", {
  totest <- data.table(a=LETTERS[1:10], b=1:10, c=1:10, d=c(rep("X", 5), rep("Y", 5)))
  totest <- melt(totest, id.vars=c("a", "d"))

  result1 <- varcalc_dt(totest, "variable", "value", "b+c", "b+c")
  expect_equal(result1[variable == "b+c"]$value, 1:10*2)
  
})

test_that("Vertical calculation with using by.", {
  totest <- data.table(a=LETTERS[1:10], b=1:10, c=1:10, d=c(rep("X", 5), rep("Y", 5)))
  totest <- melt(totest, id.vars=c("a", "d"))

  result2 <- varcalc_dt(totest, "variable", "value", "sum(b)", "sumb", by="d")

  expect_equal(result2[variable == "sumb" & d == "X"]$value, rep(15, 5))
  expect_equal(result2[variable == "sumb" & d == "Y"]$value, rep(40, 5))
})

test_that("Vertical calculation with only two columns does not work.", {
  totest <- data.table(a=LETTERS[1:10], b=1:10)
  expect_error(varcalc_dt(totest, "a", "b", "A+B", "A+B"))
  
})
