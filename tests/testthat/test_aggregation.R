test_that("Aggregation and dis-aggregation without weights on data with multiple dimensions", {
    ## to iso countries, without weights (value reproduced)
    FEiso <- disaggregate_dt(REMIND_FinalEnergy, REMIND_RegionMap, datacols=c("se", "fe", "te"))

    ## check for one line
    nrand <- sample(1:nrow(REMIND_RegionMap), 1)
    reg <- REMIND_RegionMap$region[nrand]
    cty <- REMIND_RegionMap$iso[nrand]

    expect_equal(REMIND_FinalEnergy[region == reg, value], FEiso[iso == cty, value])

    ## re-aggregate and sum
    FEsum <- aggregate_dt(FEiso, REMIND_RegionMap, datacols=c("se", "fe", "te"))

    ## n countries in region
    fac <- nrow(REMIND_RegionMap[region == reg])

    ## the sum has to be equal to the number or countries in the region
    expect_equal(FEsum[region == reg, value], FEiso[iso == cty, value] * fac)
})

test_that("Disaggregation with GDP weights on data with multiple dimensions", {
    ## to iso countries, distribute by GDP

    FEiso <- disaggregate_dt(REMIND_FinalEnergy, REMIND_RegionMap,
                             datacols=c("se", "fe", "te"),
                             weights=REMIND_GDP)

    ## check for one line
    nrand <- sample(1:nrow(REMIND_RegionMap), 1)
    reg <- REMIND_RegionMap$region[nrand]
    cty <- REMIND_RegionMap$iso[nrand]

    ## summing should reproduce regional data
    FEsum <- aggregate_dt(FEiso, REMIND_RegionMap, datacols=c("se", "fe", "te"))

    ## the sum has to be equal to the number or countries in the region
    expect_equal(FEsum[region == reg, value], REMIND_FinalEnergy[region == reg, value])
})

test_that("Aggregation with GDP weights on data with multiple dimensions", {
    ## to iso countries, distribute by GDP

    FEiso <- disaggregate_dt(REMIND_FinalEnergy, REMIND_RegionMap,
                      datacols=c("se", "fe", "te"),
                      weights=REMIND_GDP)

    ## weighted average
    FEavg <- aggregate_dt(FEiso, REMIND_RegionMap,
                      datacols=c("se", "fe", "te"),
                      weights=REMIND_GDP)

    ## dont know how to do a good check, lets compare size
    expect_equal(nrow(FEavg), nrow(REMIND_FinalEnergy))
    ## any NAs?
    expect_false(any(is.na(FEavg$value)))

})
