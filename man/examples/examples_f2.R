tmp1 <- f2(data = dip2[dip2$batch %in% c("b0", "b1"), ], tcol = 5:8,
           grouping = "batch", useEMA = "yes")
tmp2 <- f2(data = dip2[dip2$batch %in% c("b0", "b1"), ], tcol = 5:8,
           grouping = "batch", useEMA = "no", uprellim = 100)

# (Expected) results in tmp1
# $f2
# 57.46831
# $Profile.TP
# [1] 30 60 90

# (Expected) results in tmp2
# $f2
# 60.02899
# $Profile.TP
# [1]  30  60  90 180
