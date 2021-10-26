context("Model-Independent Multivariate Confidence Region Estimation")

# The tests use the values published by Hoffelder et al. (Hoffelder 2015),
# i.e. the values in Table 1, the results of measuring dissolution [%] after
# 15, 20 and 25 minutes of two different capsule formulations with different
# color (white or blue), in Hoffelder, T., Goessl, R., and Wellek, S.
# Multivariate equivalence tests for use in pharmaceutical development.
# J Biopharm Stat (2015) 25(3): 417-437.

test_that("mimcr_results_match_Hoffelder_2015", {
  l_res <-
    mimcr(data = dip3, tcol = 4:6, grouping = "batch", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, lorellim = 1,
          uprellim = 85, tol = 1e-9)

  # <-><-><-><->

  expect_equal(signif(l_res$Parameters[["DM"]], 7), 0.2384023)
  expect_equal(round(l_res$Parameters[["df1"]], 0), 3)
  expect_equal(round(l_res$Parameters[["df2"]], 0), 20)
  expect_equal(round(l_res$Parameters[["alpha"]], 2), 0.05)
  expect_equal(signif(l_res$Parameters[["K"]], 7), 1.818182)
  expect_equal(round(l_res$Parameters[["k"]], 0), 6)
  expect_equal(signif(l_res$Parameters[["T2"]], 7), 0.3410141)
  expect_equal(signif(l_res$Parameters[["F"]], 7), 0.1033376)
  expect_equal(signif(l_res$Parameters[["ncp.Hoffelder"]], 7), 30.32296)
  expect_equal(signif(l_res$Parameters[["F.crit"]], 7), 3.098391)
  expect_equal(signif(l_res$Parameters[["F.crit.Hoffelder"]], 7), 4.899274)
  expect_equal(signif(l_res$Parameters[["p.F"]], 7), 0.9571526)
  expect_equal(signif(l_res$Parameters[["p.F.Hoffelder"]], 7), 2.890827e-08)
  expect_equal(signif(l_res$Parameters[["Sim.Limit"]], 7), 2.248072)
  expect_equal(signif(l_res$Parameters[["Obs.L"]], 7), 1.067015)
  expect_equal(signif(l_res$Parameters[["Obs.U"]], 7), 1.543820)

  # The results correspond to the output obtained with the function
  # T2EQ.dissolution.profiles.hoffelder() from the R package T2EQ from
  # Thomas Hoffelder.
  # Note that in the publication cited above, i.e. Hoffelder (2015), D_crit
  # is defined as 0.74^2 instead of
  # D_crit = sqrt(t(D_glob) %*% solve(t_S) %*% D_glob)
  # where D_glob = rep(mtad, times = n_tp), mtad = 10 (e.g.) and n_tp the
  # number of time points.
  #
  # library(T2EQ) # mtad = 10
  # T2EQ.dissolution.profiles.hoffelder(
  #   X = as.matrix(dip3[dip3$type == "ref", 4:6]),
  #   Y = as.matrix(dip3[dip3$type == "test", 4:6]), alpha = 0.05)
  # l_res$Parameters
  # l_res$Parameters[["DM"]]^2; l_res$Parameters[["Sim.Limit"]]^2
  #
  # Excerpt of the output obtained with
  # T2EQ.dissolution.profiles.hoffelder(
  #   X = as.matrix(dip3[dip3$type == "ref", 4:6]),
  #   Y = as.matrix(dip3[dip3$type == "test", 4:6]), alpha = 0.05)
  #
  # Estimated Mahalanobis distance: 	     0.05683568
  # Equivalence margin: 			             5.053827
  # Hotelling's T2: 			                 0.3410141
  # Noncentrality parameter: 		          30.32296
  # Significance level: 			             0.05
  # Teststatistic: 			                   0.1033376
  # Quantile of noncent. F-distribution: 	 4.899274
  # Decision in favor (1) or against (0) equivalence/similarity of
  #   dissolution profiles:  1
  # 	 p-value of the T2-test for equivalence: p = 2.890827e-08

  # Note that the value provided under "Estimated Mahalanobis distance" is the
  # squared Mahalanobis distance and thus equal to l_res$Parameters[["DM"]]^2.
  # Accordingly, the value under "Equivalence margin" corresponds to
  # l_res$Parameters[["Sim.Limit"]]^2.
})

# The tests use the values published by Hoffelder, T. (Hoffelder 2016), i.e.
# the values underlying Figure 1, the results of measuring dissolution [%]
# after 10, 20 and 30 minutes of two different unknown formulations, from
# Figure 1 in Hoffelder, T. Highly Variable Dissolution Profiles. Comparison
# of T2-Test for Equivalence and f2 Based Methods. Pharm Ind (2016) 78(4):
# 587-592.

test_that("mimcr_results_match_Hoffelder_2016", {
  l_res <- mimcr(data = dip4, tcol = 2:4, grouping = "type", fit_n_obs = FALSE,
                 mtad = 10, signif = 0.05, max_trial = 50, lorellim = 1,
                 uprellim = 85, tol = 1e-9)

  # <-><-><-><->

  expect_equal(signif(l_res$Parameters[["DM"]], 7), 2.823976)
  expect_equal(round(l_res$Parameters[["df1"]], 0), 3)
  expect_equal(round(l_res$Parameters[["df2"]], 0), 20)
  expect_equal(round(l_res$Parameters[["alpha"]], 2), 0.05)
  expect_equal(signif(l_res$Parameters[["K"]], 7), 1.818182)
  expect_equal(round(l_res$Parameters[["k"]], 0), 6)
  expect_equal(signif(l_res$Parameters[["T2"]], 7), 47.84903)
  expect_equal(signif(l_res$Parameters[["F"]], 7), 14.49970)
  expect_equal(signif(l_res$Parameters[["ncp.Hoffelder"]], 7), 1770.045)
  expect_equal(signif(l_res$Parameters[["F.crit"]], 7), 3.098391)
  expect_equal(signif(l_res$Parameters[["F.crit.Hoffelder"]], 7), 373.4880)
  expect_equal(signif(l_res$Parameters[["p.F"]], 7), 0.00003002744)
  expect_equal(signif(l_res$Parameters[["p.F.Hoffelder"]], 7), 8.427879e-110)
  expect_equal(signif(l_res$Parameters[["Sim.Limit"]], 7), 17.17578)
  expect_equal(signif(l_res$Parameters[["Obs.L"]], 7), 1.518558)
  expect_equal(signif(l_res$Parameters[["Obs.U"]], 7), 4.129393)

  # The results correspond to the output obtained with the example shown in the
  # documentation of the T2EQ.dissolution.profiles.hoffelder() function in the
  # R package T2EQ from Thomas Hoffelder.
  #
  # library(T2EQ) # mtad = 10
  # T2EQ.dissolution.profiles.hoffelder(
  #   X = as.matrix(dip4[dip4$type == "ref", 2:4]),
  #   Y = as.matrix(dip4[dip4$type == "test", 2:4]), alpha = 0.05)
  # l_res$Parameters
  # l_res$Parameters[["DM"]]^2; l_res$Parameters[["Sim.Limit"]]^2
  #
  #  Excerpt of the output obtained with
  #  T2EQ.dissolution.profiles.hoffelder(X = REF_pharmind, Y = TEST_pharmind)
  #
  # Estimated Mahalanobis distance:        7.974838
  # Equivalence margin: 			           295.0074
  # Hotelling's T2: 			                47.84903
  # Noncentrality parameter: 		        1770.045
  # Significance level: 			             0.05
  # Teststatistic: 			                  14.4997
  # Quantile of noncent. F-distribution: 373.488
  # Decision in favor (1) or against (0) equivalence/similarity of
  #   dissolution profiles:  1
  # p-value of the T2-test for equivalence: p = 8.427879e-110

  # Note that the value provided under "Estimated Mahalanobis distance" is the
  # squared Mahalanobis distance and thus equal to l_res$Parameters[["DM"]]^2.
  # Accordingly, the value under "Equivalence margin" corresponds to
  # l_res$Parameters[["Sim.Limit"]]^2.
})

# The tests use the values published by Tsong et al. (Tsong 1996), i.e. the
# values in Table 1, dissolution data of a reference and a test batch.
# Tsong, Y., Hammerstrom, T., Sathe, P., and Shah, V.P. Statistical
# Assessment of Mean Differences Between Two Dissolution Data Sets.
# Drug Inf J (1996) 30: 1105-1112.

test_that("mimcr_results_match_Tsong_1996_two", {
  l_res <-
    suppressWarnings(mimcr(data = dip1[, c("type", "tablet", "t.15", "t.90")],
                           tcol = 3:4, grouping = "type", fit_n_obs = FALSE,
                           mtad = 15, signif = 0.1, max_trial = 50,
                           lorellim = 1, uprellim = 100, tol = 1e-9))

  # <-><-><-><->

  expect_equal(signif(l_res$Parameters[["DM"]], 7), 10.44045)
  expect_equal(round(l_res$Parameters[["df1"]], 0), 2)
  expect_equal(round(l_res$Parameters[["df2"]], 0), 9)
  expect_equal(round(l_res$Parameters[["alpha"]], 2), 0.1)
  expect_equal(signif(l_res$Parameters[["K"]], 7), 1.350000)
  expect_equal(signif(l_res$Parameters[["T2"]], 7), 327.0089)
  expect_equal(signif(l_res$Parameters[["F.crit"]], 7), 3.006452)
  expect_equal(signif(l_res$Parameters[["Sim.Limit"]], 7), 9.630777)
  expect_equal(signif(l_res$Parameters[["Obs.L"]], 7), 8.948135)
  expect_equal(signif(l_res$Parameters[["Obs.U"]], 7), 11.93276)

  expect_equivalent(signif(l_res[["NR.CI"]][["CI"]][, 1], 7),
                    c(-15.03433, 2.902591))
  expect_equivalent(signif(l_res[["NR.CI"]][["CI"]][, 2], 7),
                    c(-20.04900, 3.870743))

  # The results correspond to the value shown in Tsong (1996):
  # df1 (P) = 2
  # df2 (n1 + n2 - p - 1) = 9
  # K = 1.35
  # D.M = 10.44
  # D.M.l = 8.94
  # D.M.u = 11.93
  # Points on CR: (-15.03, 2.90) and (-20.05, 3.87)

  # Note that for the comparison with the T2EQ.dissolution.profiles.hoffelder()
  # function from the R package T2EQ from Thomas Hoffelder 'mtad' must be 10.
  #
  # library(T2EQ) # mtad = 10
  # T2EQ.dissolution.profiles.hoffelder(
  #   X = as.matrix(dat[dat$type == "R", 3:4]),
  #   Y = as.matrix(dat[dat$type == "T", 3:4]), alpha = 0.05)
  # l_res <-
  #   suppressWarnings(mimcr(data = dat, tcol = 3:4, grouping = "type",
  #                        mtad = 10, signif = 0.05, max_trial = 50,
  #                        lorellim = 1, uprellim = 100, tol = 1e-9))
  # l_res$Parameters
  # l_res$Parameters[["DM"]]^2; l_res$Parameters[["Sim.Limit"]]^2
  #
  # Estimated Mahalanobis distance: 	    109.003
  # Equivalence margin: 			             41.22305
  # Hotelling's T2: 			                327.0089
  # Noncentrality parameter: 		          123.6691
  # Significance level: 			              0.05
  # Teststatistic: 			                  147.154
  # Quantile of noncent. F-distribution: 	 30.8798
  # Decision in favor (1) or against (0) equivalence/similarity of
  #   dissolution profiles:  0
  # p-value of the T2-test for equivalence: p = 0.915731

  # Note that the value provided under "Estimated Mahalanobis distance" is the
  # squared Mahalanobis distance and thus equal to l_res$Parameters[["DM"]]^2.
  # Accordingly, the value under "Equivalence margin" corresponds to
  # l_res$Parameters[["Sim.Limit"]]^2.
})

test_that("mimcr_results_match_Tsong_1996_all", {
  l_res <-
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.1, lorellim = 1, uprellim = 100, tol = 1e-9)

  # <-><-><-><->

  expect_equal(signif(l_res$Parameters[["DM"]], 7), 26.48562)
  expect_equal(round(l_res$Parameters[["df1"]], 0), 8)
  expect_equal(round(l_res$Parameters[["df2"]], 0), 3)
  expect_equal(round(l_res$Parameters[["alpha"]], 2), 0.1)
  expect_equal(signif(l_res$Parameters[["K"]], 7), 0.1125000)
  expect_equal(signif(l_res$Parameters[["T2"]], 7), 2104.464)
  expect_equal(signif(l_res$Parameters[["F.crit"]], 7), 5.251671)
  expect_equal(signif(l_res$Parameters[["Sim.Limit"]], 7), 19.42719)
  expect_equal(signif(l_res$Parameters[["Obs.L"]], 7), 19.65323)
  expect_equal(signif(l_res$Parameters[["Obs.U"]], 7), 33.31800)

  expect_equivalent(signif(l_res[["NR.CI"]][["CI"]][, 1], 7),
                    c(-17.58002, -15.22654, -13.01651, -10.97963, -7.432708,
                      -0.5169504, 2.513022, 3.708933))
  expect_equivalent(signif(l_res[["NR.CI"]][["CI"]][, 2], 7),
                    c( -29.80331, -25.81346, -22.06682, -18.61370, -12.60063,
                       -0.8763830, 4.260311, 6.287733))

  # The results correspond to the value shown in Tsong (1996):
  # df1 (P) = 8
  # df2 (n1 + n2 - p - 1) = 3
  # K = 0.1125
  # T2 = 2104.46
  # D.M = 26.49
  # D.M.l = 19.65
  # D.M.u = 33.32

  # Note that for the comparison with the T2EQ.dissolution.profiles.hoffelder()
  # function from the R package T2EQ from Thomas Hoffelder 'mtad' must be 10.
  #
  # library(T2EQ) # mtad = 10
  # T2EQ.dissolution.profiles.hoffelder(
  #   X = as.matrix(dip1[dip1$type == "R", 3:10]),
  #   Y = as.matrix(dip1[dip1$type == "T", 3:10]), alpha = 0.05)
  # l_res <-
  #   suppressWarnings(mimcr(data = dip1, tcol = 3:10, grouping = "type",
  #                        mtad = 10, signif = 0.05, max_trial = 50,
  #                        lorellim = 1, uprellim = 100, tol = 1e-9))
  # l_res$Parameters
  # l_res$Parameters[["DM"]]^2; l_res$Parameters[["Sim.Limit"]]^2
  #
  # Estimated Mahalanobis distance: 	    701.4879
  # Equivalence margin: 			            377.4157
  # Hotelling's T2: 			               2104.464
  # Noncentrality parameter: 		         1132.247
  # Significance level: 			              0.05
  # Teststatistic: 			                   78.91739
  # Quantile of noncent. F-distribution: 	 54.39054
  # Decision in favor (1) or against (0) equivalence/similarity of dissolution
  #   profiles:  0
  # p-value of the T2-test for equivalence: p = 0.1449045

  # Note that the value provided under "Estimated Mahalanobis distance" is the
  # squared Mahalanobis distance and thus equal to l_res$Parameters[["DM"]]^2.
  # Accordingly, the value under "Equivalence margin" corresponds to
  # l_res$Parameters[["Sim.Limit"]]^2.
})

test_that("mimcr_fails", {
  tmp0 <- dip1
  tmp0$t.5 <- as.factor(tmp0$t.5)

  tmp1 <- dip1
  tmp1$type <-  as.character(tmp1$type)

  tmp2 <- dip1[1:11, ]

  # <-><-><->

  expect_error(
    mimcr(data = as.matrix(dip1[, 3:10]), tcol = 3:10, grouping = "type",
          fit_n_obs = FALSE, mtad = 10, signif = 0.05, max_trial = 50,
          lorellim = 1, uprellim = 85, tol = 1e-9),
    "data must be provided as data frame")
  expect_error(
    mimcr(data = dip1, tcol = "tico", grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50,
          lorellim = 1, uprellim = 85, tol = 1e-9),
    "tcol must be an integer")
  expect_error(
    suppressWarnings(mimcr(data = dip1, tcol = 3, grouping = "type",
                           fit_n_obs = FALSE, mtad = 10, signif = 0.05,
                           max_trial = 50, lorellim = 1, uprellim = 85,
                           tol = 1e-9)),
    "tcol must be an integer")
  expect_error(
    mimcr(data = dip1, tcol = 3:10 + 0.1, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50,  lorellim = 1,
          uprellim = 85, tol = 1e-9),
    "tcol must be an integer")
  expect_error(
    mimcr(data = dip1, tcol = 7:11, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50,  lorellim = 1,
          uprellim = 85, tol = 1e-9),
    "Some columns specified by tcol were not found")
  expect_error(
    mimcr(data = dip1, tcol = 2:6, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50,  lorellim = 1,
          uprellim = 85, tol = 1e-9),
    "Some names of columns specified by tcol")
  expect_error(
    mimcr(data = tmp0, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50,  lorellim = 1,
          uprellim = 85, tol = 1e-9),
    "Some columns specified by tcol are not numeric")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = 5, fit_n_obs = FALSE, mtad = 10,
          signif = 0.05, max_trial = 50, lorellim = 1, uprellim = 85,
          tol = 1e-9),
    "grouping must be string")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "lot", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, lorellim = 1, uprellim = 85,
          tol = 1e-9),
    "grouping variable was not found")
  expect_error(
    mimcr(data = tmp1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, lorellim = 1, uprellim = 85,
          tol = 1e-9),
    "grouping variable's column in data")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "tablet", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, lorellim = 1, uprellim = 85,
          tol = 1e-9),
    "number of levels in column")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = "FALSE",
          mtad = 10, signif = 0.05, max_trial = 50, lorellim = 1, uprellim = 85,
          tol = 1e-9),
    "fit_n_obs must be a logical")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = c(T, F),
          mtad = 10, signif = 0.05, max_trial = 50, lorellim = 1, uprellim = 85,
          tol = 1e-9),
    "fit_n_obs must be a logical")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = -5, signif = 0.05, max_trial = 50, lorellim = 1, uprellim = 85,
          tol = 1e-9),
    "specify mtad")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 55, signif = 0.05, max_trial = 50, lorellim = 1, uprellim = 85,
          tol = 1e-9),
    "specify mtad")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = -1, max_trial = 50, lorellim = 1, uprellim = 85,
          tol = 1e-9),
    "specify signif")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 9, max_trial = 50, lorellim = 1, uprellim = 85,
          tol = 1e-9),
    "specify signif")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = "max_trial", lorellim = 1,
          uprellim = 85, tol = 1e-9),
    "max_trial must be a positive integer")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = c(50, 100), lorellim = 1,
          uprellim = 85, tol = 1e-9),
    "max_trial must be a positive integer")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50.5, lorellim = 1,
          uprellim = 85, tol = 1e-9),
    "max_trial must be a positive integer")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = -10, lorellim = 1,
          uprellim = 85, tol = 1e-9),
    "max_trial must be a positive integer")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, lorellim = "lorel",
          uprellim = 85, tol = 1e-9),
    "lorellim must be single number >= 0 and < uprellim")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, lorellim = 85,
          uprellim = 1, tol = 1e-9),
    "lorellim must be single number >= 0 and < uprellim")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, lorellim = 1,
          uprellim = "uprel", tol = 1e-9),
    "uprellim must be a single number <= 100 and > lorellim")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, lorellim = 1,
          uprellim = 85, tol = "1e-9"),
    "tol must be a non-negative numeric")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, lorellim = 1,
          uprellim = 85, tol = rep(1e-9, 2)),
    "tol must be a non-negative numeric")
  expect_error(
    mimcr(data = dip1, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, lorellim = 1,
          uprellim = 85, tol = -1e-9),
    "tol must be a non-negative numeric")
  expect_error(
    mimcr(data = tmp2, tcol = 3:10, grouping = "type", fit_n_obs = FALSE,
          mtad = 10, signif = 0.05, max_trial = 50, lorellim = 1,
          uprellim = 85, tol = 1e-9),
    "The treatments to be tested")
})

test_that("mimcr_warns", {
  tmp <- rbind(dip2[dip2$batch == "b0", ],
               dip2[dip2$batch == "b4" & dip2$tablet %in% as.character(1:6), ])

  # <-><-><-><->

  expect_warning(
    mimcr(data = tmp, tcol = 4:8, grouping = "type", fit_n_obs = TRUE,
          mtad = 10, signif = 0.1, max_trial = 50, lorellim = 1, uprellim = 85,
          tol = 1e-9),
    "Rows from the group with redundant observations")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  expect_warning(
    mimcr(data = dip3, tcol = 4:6, grouping = "type", fit_n_obs = TRUE,
          mtad = 10, signif = 0.1, max_trial = 50, lorellim = 1, uprellim = 55,
          tol = 1e-9),
    "The profiles should comprise a minimum of 3 time points")
  expect_warning(
    mimcr(data = dip3, tcol = 4:6, grouping = "type", fit_n_obs = TRUE,
          mtad = 10, signif = 0.1, max_trial = 50, lorellim = 1, uprellim = 85,
          tol = 1),
    "The points found by the Newton-Raphson search")
})
