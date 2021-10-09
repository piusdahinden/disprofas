l_res <- mimcr(data = dip3, tcol = 4:6, grouping = "batch")
l_res[c("Similarity")]
l_res[c("Parameters")]

# (Expected) results in l_res[c("Similarity")]
# $Similarity
#     Tsong Hoffelder
# "Similar" "Similar"

# (Expected) results in l_res[c("Parameters")]
# $Parameters
#            DM              df1              df2            alpha
#  2.384023e-01     3.000000e+00     2.000000e+01     5.000000e-02
#             K                k               T2                F
#  1.818182e+00     6.000000e+00     3.410141e-01     1.033376e-01
# ncp.Hoffelder           F.crit F.crit.Hoffelder              p.F
#  3.032296e+01     3.098391e+00     4.899274e+00     9.571526e-01
# p.F.Hoffelder             MTAD        Sim.Limit            Obs.L
#  2.890827e-08     1.000000e+01     2.248072e+00     1.067015e+00
#         Obs.U
#  1.543820e+00
