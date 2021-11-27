# Dissolution data of one reference batch and one test batch of n = 6
# tablets each:
str(dip1)

# 'data.frame':	12 obs. of  10 variables:
# $ type  : Factor w/ 2 levels "R","T": 1 1 1 1 1 1 2 2 2 2 ...
# $ tablet: Factor w/ 6 levels "1","2","3","4",..: 1 2 3 4 5 6 1 2 3 4 ...
# $ t.5   : num  42.1 44.2 45.6 48.5 50.5 ...
# $ t.10  : num  59.9 60.2 55.8 60.4 61.8 ...
# $ t.15  : num  65.6 67.2 65.6 66.5 69.1 ...
# $ t.20  : num  71.8 70.8 70.5 73.1 72.8 ...
# $ t.30  : num  77.8 76.1 76.9 78.5 79 ...
# $ t.60  : num  85.7 83.3 83.9 85 86.9 ...
# $ t.90  : num  93.1 88 86.8 88 89.7 ...
# $ t.120 : num  94.2 89.6 90.1 93.4 90.8 ...

# Assessment of data by aid of the mimcr() function
res1 <- mimcr(data = dip1, tcol = 3:10, grouping = "type")

# Print of a summary of the assessment
print(res1)

# Results of Model-Independent Multivariate Confidence Region (MIMCR)
# approach to assess equivalence of highly variable in-vitro
# dissolution profiles of two drug product formulations
#
# Did the Newton-Raphson search converge? Yes
#
# Parameters (general):
#   Significance level:                 0.05
# Degrees of freedom (1):               7
# Degrees of freedom (2):               4
# Mahalanobis distance (MD):            25.72
# (F) scaling factor K:                 0.1714
# (MD) scaling factor k:                3
# Hotelling's T2:                       1984
#
# Parameters specific for Tsong (1996) approach:
# Maximum tolerable average difference: 10
# Similarity limit:                     11.33
# Observed upper limit:                 31.68
#
# Parameters specific for Hoffelder (2016) approach:
# Noncentrality parameter:              385
# Critial F (Hoffelder):                23.16
# Probability p (Hoffelder):            0.7402
#
# Conclusions:
#       Tsong (1996):  Dissimilar
#   Hoffelder (2016):  Dissimilar

# Taking only the 15 and 90 minutes testing points into account produces a
# warning because profiles should comprise a minimum of three testing points.
\dontrun{
  res2 <- mimcr(data = dip1, tcol = c(5, 9), grouping = "type", mtad = 15,
                signif = 0.1)
  print(res2)
}

# Warning:
#   In mimcr(data = dip1, tcol = c(5, 9), grouping = "type", mtad = 15,  :
# The profiles should comprise a minimum of 3 time points. The actual profiles
# comprise 2 points only.

# Results of Model-Independent Multivariate Confidence Region (MIMCR)
# approach to assess equivalence of highly variable in-vitro
# dissolution profiles of two drug product formulations
#
# Did the Newton-Raphson search converge? Yes
#
# Parameters (general):
#   Significance level:                 0.1
# Degrees of freedom (1):               2
# Degrees of freedom (2):               9
# Mahalanobis distance (MD):            10.44
# (F) scaling factor K:                 1.35
# (MD) scaling factor k:                3
# Hotelling's T2:                       327
#
# Parameters specific for Tsong (1996) approach:
# Maximum tolerable average difference: 15
# Similarity limit:                     9.631
# Observed upper limit:                 11.93
#
# Parameters specific for Hoffelder (2016) approach:
# Noncentrality parameter:              278.3
# Critial F (Hoffelder):                83.57
# Probability p (Hoffelder):            0.4823
#
# Conclusions:
#       Tsong (1996):  Dissimilar
#   Hoffelder (2016):  Dissimilar

# Dissolution data  of one reference batch and one test batch of n = 12
# capsules each:
str(dip3)

# 'data.frame':	24 obs. of  6 variables:
# $ cap  : Factor w/ 12 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ batch: Factor w/ 2 levels "blue","white": 2 2 2 2 2 2 2 2 2 2 ...
# $ type : Factor w/ 2 levels "ref","test": 1 1 1 1 1 1 1 1 1 1 ...
# $ x.15 : num  49 15 56 57 6 62 23 11 9 42 ...
# $ x.20 : num  86 59 84 87 58 90 71 64 61 81 ...
# $ x.25 : num  98 96 96 99 90 97 97 92 88 96 ...

# A successful comparison:
res3 <- mimcr(data = dip3, tcol = 4:6, grouping = "batch")
print(res3)

# Results of Model-Independent Multivariate Confidence Region (MIMCR)
# approach to assess equivalence of highly variable in-vitro
# dissolution profiles of two drug product formulations
#
# Did the Newton-Raphson search converge? Yes
#
# Parameters (general):
#   Significance level:                 0.05
# Degrees of freedom (1):               3
# Degrees of freedom (2):               20
# Mahalanobis distance (MD):            0.2384
# (F) scaling factor K:                 1.818
# (MD) scaling factor k:                6
# Hotelling's T2:                       0.341
#
# Parameters specific for Tsong (1996) approach:
# Maximum tolerable average difference: 10
# Similarity limit:                     2.248
# Observed upper limit:                 1.544
#
# Parameters specific for Hoffelder (2016) approach:
# Noncentrality parameter:              30.32
# Critial F (Hoffelder):                4.899
# Probability p (Hoffelder):            2.891e-08
#
# Conclusions:
#       Tsong (1996):  Similar
#   Hoffelder (2016):  Similar