\dontrun{
  get_jackknife_values(grouping = "batch", stat_fun = get_f2,
                       data = dip2[dip2$batch %in% c("b0", "b4"), ], tcol = 5:8)
}

# (Expected) results in l_jack[1:4]
# $theta.hat
# [1] 50.07187
#
# $theta.jack
# [1] 50.05253
#
# $jack.se
# [1] 0.9203128
#
# $jack.bias
# [1] -0.01933776
