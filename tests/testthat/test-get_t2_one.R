context("Get Hotelling's T2 statistics for one (small) sample")

test_that("get_t2_one", {
  res <-
    get_t2_one(m = as.matrix(dip12), mu = c(1000, 15, 60, 800, 75),
               signif = 0.05)

  # <-><-><-><->

  expect_equal(round(res[["Parameters"]][["df1"]], 0), 5)
  expect_equal(round(res[["Parameters"]][["df2"]], 0), 732)
  expect_equal(signif(res[["Parameters"]][["T2"]], 7), 1758.541)
  expect_equal(signif(res[["Parameters"]][["F"]], 7), 349.7968)
  expect_equal(signif(res[["Parameters"]][["p.F"]], 7), 0.0000000)
})

test_that("get_hotelling_successfully_calculates_CIs", {
  l_res <-
    get_t2_one(m = as.matrix(dip12), mu = c(1000, 15, 60, 800, 75),
                          signif = 0.05)

  # <-><-><-><->

  expect_equal(round(l_res[["Parameters"]][["df1"]], 0), 5)
  expect_equal(round(l_res[["Parameters"]][["df2"]], 0), 732)
  expect_equal(signif(l_res[["Parameters"]][["T2"]], 7), 1758.541)
  expect_equal(signif(l_res[["Parameters"]][["F"]], 7), 349.7968)
  expect_equal(signif(l_res[["Parameters"]][["p.F"]], 7), 0.0000000)
  expect_equal(signif(l_res[["Parameters"]][["F.crit"]], 7), 2.226340)
  expect_equal(signif(l_res[["Parameters"]][["t.crit"]], 7), 2.582526)

  # <-><-><-><->

  expect_equal(signif(l_res[["CI"]][["Hotelling"]][, "LCL"], 7),
               c(575.0912, 10.39244, 62.03547, 638.3278, 69.85901))
  expect_equal(signif(l_res[["CI"]][["Hotelling"]][, "UCL"], 7),
               c(673.0073, 11.86735, 69.57141, 1040.943, 87.99788))

  # <-><-><-><->

  expect_equal(signif(l_res[["CI"]][["Bonferroni"]][, "LCL"], 7),
               c(586.2568, 10.56063, 62.89481, 684.2391, 71.92743))
  expect_equal(signif(l_res[["CI"]][["Bonferroni"]][, "UCL"], 7),
               c(661.8417, 11.69917, 68.71207, 995.0316, 85.92946))
})

test_that("get_hotellings_fails", {
  expect_error(
    get_t2_one(m = dip12, mu = c(1000, 15, 60, 800, 75),
                          signif = 0.05),
    "m must be a matrix")
  expect_error(
    get_t2_one(m = as.matrix(dip12),
                          mu = as.character(c(1000, 15, 60, 800, 75)),
                          signif = 0.05),
    "mu must be a numeric vector")
  expect_error(
    get_t2_one(m = as.matrix(dip12), mu = c(1000, 15, 60, 800),
                          signif = 0.05),
    "number of columns in m")
  expect_error(
    get_t2_one(m = as.matrix(dip12), mu = c(1000, 15, 60, 800, 75),
                          signif = -1),
    "specify signif")
  expect_error(
    get_t2_one(m = as.matrix(dip12), mu = c(1000, 15, 60, 800, 75),
                          signif = 9),
    "specify signif")
})
