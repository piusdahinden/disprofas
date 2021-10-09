context("Generic summary and print (and plot) functions")

test_that("plot.bootstrap_f2_succeeds", {
  re <- bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                     tcol = 5:8, grouping = "batch",
                     R = 200, new_seed = 421, useEMA = "no")

  # <-><-><-><->

  expect_s3_class(expect_output(plot(re), "Shah"), "bootstrap_f2")
  expect_output(plot(re, digits = 5), "48.646")
})

test_that("summary.bootstrap_f2_succeeds", {
  re <- bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                     tcol = 5:8, grouping = "batch",
                     R = 200, new_seed = 421, useEMA = "no")

  # <-><-><-><->

  expect_s3_class(expect_output(summary(re), "Shah"), "bootstrap_f2")
  expect_output(summary(re), "STRATIFIED BOOTSTRAP")
  expect_output(summary(re), "BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS")

  expect_output(summary(re, digits = 5), "50.072")
  expect_output(summary(re, digits = 5), "48.646")
})

test_that("print.bootstrap_f2_succeeds", {
  re <- bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b4"), ],
                     tcol = 5:8, grouping = "batch",
                     R = 200, new_seed = 421, useEMA = "no")

  # <-><-><-><->

  expect_s3_class(expect_output(print(re), "Shah"), "bootstrap_f2")
  expect_output(print(re), "STRATIFIED BOOTSTRAP")
  expect_output(print(re), "BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS")

  expect_output(print(re, digits = 5), "50.072")
  expect_output(print(re, digits = 5), "48.646")
})

test_that("summary.mimcr_succeeds", {
  re <- mimcr(data = dip3, tcol = 4:6, grouping = "batch", fit_n_obs = FALSE,
              mtad = 10, signif = 0.05, max_trial = 50, lorellim = 1,
              uprellim = 85, tol = 1e-9)

  # <-><-><-><->

  expect_s3_class(expect_output(summary(re), "MIMCR"), "mimcr")
  expect_output(summary(re), "Yes")
  expect_output(summary(re), "Similar")

  expect_output(summary(re, digits = 5), "1.5438")
  expect_output(summary(re, digits = 5), "2.8908e-08")
})

test_that("print.mimcr_succeeds", {
  re <- mimcr(data = dip3, tcol = 4:6, grouping = "batch", fit_n_obs = FALSE,
              mtad = 10, signif = 0.05, max_trial = 50, lorellim = 1,
              uprellim = 85, tol = 1e-9)

  # <-><-><-><->

  expect_s3_class(expect_output(print(re), "MIMCR"), "mimcr")
  expect_output(print(re), "Yes")
  expect_output(print(re), "Similar")

  expect_output(print(re, digits = 5), "1.5438")
  expect_output(print(re, digits = 5), "2.8908e-08")
})

test_that("summary.mztia_succeeds", {
  re <- mztia(data = dip1, shape = "wide", tcol = 3:10, grouping = "type",
              reference = "R", alpha = 0.05, P = 0.99, cap = FALSE)

  # <-><-><-><->

  expect_s3_class(expect_output(summary(re), "Martinez & Zhao"), "mztia")
  expect_output(summary(re), "Time")
  expect_output(summary(re), "S2.UTL")

  expect_output(summary(re, digits = 5), "15")
  expect_output(summary(re, digits = 5), "92.646")
})

test_that("print.mztia_succeeds", {
  re <- mztia(data = dip1, shape = "wide", tcol = 3:10, grouping = "type",
              reference = "R", alpha = 0.05, P = 0.99, cap = FALSE)

  # <-><-><-><->

  expect_s3_class(expect_output(print(re), "Martinez & Zhao"), "mztia")
  expect_output(print(re), "Time")
  expect_output(print(re), "S2.UTL")

  expect_output(print(re, digits = 5), "15")
  expect_output(print(re, digits = 5), "92.646")
})
