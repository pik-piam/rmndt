test_that("writeDfToGDXparameter round-trips a data.table to a GDX parameter", {
  skip_if_not_installed("gamstransfer")

  # EUR appears twice, so the region set must collapse it to a single element
  dt <- data.table(
    region = c("EUR", "USA", "CHA", "EUR"),
    year = c(2020L, 2020L, 2020L, 2050L),
    value = c(1.5, 2.5, 3.5, 4.5)
  )

  gdxPath <- tempfile(fileext = ".gdx")
  on.exit(unlink(gdxPath), add = TRUE)

  writeDfToGDXparameter(dt, gdxPath, "p_test", description = "unit test")

  expect_true(file.exists(gdxPath))

  m <- gamstransfer::Container$new(gdxPath)

  # Parameter and its domain sets are written as symbols
  expect_true(all(c("p_test", "region", "year") %in% m$listSymbols()))

  # The domain sets hold the unique elements of their column, in order of appearance
  expect_equal(as.character(m["region"]$records$uni), unique(dt$region))
  expect_equal(as.character(m["year"]$records$uni), unique(as.character(dt$year)))

  param <- m["p_test"]
  expect_equal(param$description, "unit test")
  expect_equal(param$domain, c("region", "year"))
  expect_equal(param$numberRecords, nrow(dt))

  # gamstransfer regroups records, so match on the domain keys instead of on row order
  rec <- as.data.table(param$records)
  rec[, `:=`(region = as.character(region), year = as.character(year))]
  expected <- copy(dt)[, year := as.character(year)]

  both <- merge(rec, expected, by = c("region", "year"), suffixes = c(".gdx", ".in"))
  expect_equal(nrow(both), nrow(dt))
  expect_equal(both$value.gdx, both$value.in)
})

test_that("writeDfToGDXparameter errors on a missing column", {
  dt <- data.table(region = "EUR", value = 1)

  gdxPath <- tempfile(fileext = ".gdx")
  on.exit(unlink(gdxPath), add = TRUE)

  expect_error(
    writeDfToGDXparameter(dt, gdxPath, "p_test", domainCols = c("region", "year")),
    "not found"
  )
})
