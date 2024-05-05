context("Get Hotelling's T2 statistics for two indepependent (small) samples")

test_that("get_hotellings_succeeds", {
  l_res <- get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", 3:10]),
                          m2 = as.matrix(dip1[dip1$type == "T", 3:10]),
                          signif = 0.1)

  # <-><-><-><->

  expect_equal(signif(l_res$Parameters[["dm"]], 7), 26.48562)
  expect_equal(round(l_res$Parameters[["df1"]], 0), 8)
  expect_equal(round(l_res$Parameters[["df2"]], 0), 3)
  expect_equal(signif(l_res$Parameters[["K"]], 7), 0.1125000)
  expect_equal(round(l_res$Parameters[["k"]], 0), 3)
  expect_equal(signif(l_res$Parameters[["T2"]], 7), 2104.464)
  expect_equal(signif(l_res$Parameters[["F"]], 7), 78.91739)
  expect_equal(signif(l_res$Parameters[["F.crit"]], 7), 5.251671)
  expect_equal(signif(l_res$Parameters[["p.F"]], 7), 0.002116258)
})

test_that("get_hotelling_successfully_calculates_CIs", {
  l_res <- get_hotellings(
    m1 = as.matrix(dip11[dip11$Status == "counterfeit", 2:7]),
    m2 = as.matrix(dip11[dip11$Status == "genuine", 2:7]),
    signif = 0.05)

  # <-><-><-><->

  expect_equal(round(l_res[["Parameters"]][["df1"]], 0), 6)
  expect_equal(round(l_res[["Parameters"]][["df2"]], 0), 193)
  expect_equal(signif(l_res[["Parameters"]][["T2"]], 7), 2412.451)
  expect_equal(signif(l_res[["Parameters"]][["F"]], 7), 391.9217)
  expect_equal(signif(l_res[["Parameters"]][["p.F"]], 7), 0.0000000)
  expect_equal(signif(l_res[["Parameters"]][["F.crit"]], 7), 2.145801)
  expect_equal(signif(l_res[["Parameters"]][["t.crit"]], 7), 2.665026)

  # <-><-><-><->

  expect_equal(signif(l_res[["CI"]][["Hotelling"]][, "LCL"], 7),
               c(-0.04432667, -0.5185652, -0.64159649,
                 -2.698094, -1.295233, 1.807197))
  expect_equal(signif(l_res[["CI"]][["Hotelling"]][, "UCL"], 7),
               c(0.3363267, -0.1954348, -0.3044035,
                 -1.751906, -0.6347669, 2.326803))

  # <-><-><-><->

  expect_equal(signif(l_res[["CI"]][["Bonferroni"]][, "LCL"], 7),
               c(0.006434909, -0.4754745, -0.5966305,
                 -2.571916, -1.207157, 1.876489))
  expect_equal(signif(l_res[["CI"]][["Bonferroni"]][, "UCL"], 7),
               c(0.2855651, -0.2385255, -0.3493695,
                 -1.878084, -0.7228426, 2.257511))
})

test_that("get_hotellings_fails", {
  expect_error(
    get_hotellings(m1 = dip1[dip1$type == "R", 3:10],
                   m2 = as.matrix(dip1[dip1$type == "T", 3:10]),
                   signif = 0.05),
    "m1 must be provided as matrix")
  expect_error(
    get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", 3:10]),
                   m2 = dip1[dip1$type == "T", 3:10],
                   signif = 0.05),
    "m2 must be provided as matrix")
  expect_error(
    get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", 3:9]),
                   m2 = as.matrix(dip1[dip1$type == "T", 3:10]),
                   signif = 0.05),
    "matrices m1 and m2 must have the same number of columns")
  expect_error(
    get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", 3:10]),
                   m2 = as.matrix(dip1[dip1$type == "T", 3:9]),
                   signif = 0.05),
    "matrices m1 and m2 must have the same number of columns")
  expect_error(
    get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", 3:10]),
                   m2 = as.matrix(dip1[dip1$type == "T", 3:10]),
                   signif = -1),
    "specify signif")
  expect_error(
    get_hotellings(m1 = as.matrix(dip1[dip1$type == "R", 3:10]),
                   m2 = as.matrix(dip1[dip1$type == "T", 3:10]),
                   signif = 9),
    "specify signif")
})
