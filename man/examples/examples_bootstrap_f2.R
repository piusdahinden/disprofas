\dontrun{
  bsf2 <- bootstrap_f2(data = dip2[dip2$batch %in% c("b0", "b1"), ],
                       tcol = 5:8, grouping = "batch")
}

# (Expected) results in bsf2[c("Boot", "BCa_CI", "ShahBCa_CI")]
# Bootstrap Statistics of stratified bootstrap:
#     original      bias  std. error
# t1* 57.46831 0.3984379     4.84336
#
# $BCa_CI
# [1] 50.73110 67.06992
#
# $ShahBCa_CI
# [1] 49.61195 65.16257
