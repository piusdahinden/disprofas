# Estimation of the parameters for Hotelling's one-sample T2 statistic
# (for small samples)
res <-
  get_t2_one(m = as.matrix(dip1[dip1$type == "R", c("t.15", "t.90")]),
             mu = c(0, 0),
             signif = 0.1)
res$cov
res$Parameters

# Expected results in res$S.pool
#          t.15      t.90
# t.15  3.16435 -0.295340
# t.90 -0.29534  4.856147

# Expected results in res$Parameters
# $Parameters
#           dm          df1          df2       signif            K
# 5.756774e+01 2.000000e+00 4.000000e+00 1.000000e-01 4.000000e-01
#            k           T2            F       F.crit       t.crit
# 6.000000e+00 1.988427e+04 1.325618e+03 4.324555e+00 2.570582e+00
