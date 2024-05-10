# Determination of the profile portion that corresponds to the EMA rules
# in a data set with dissolution profiles with eight different time points
res1 <- get_profile_portion(
  data = dip1, tcol = 3:10, groups = (dip1$type == "R"), use_ema = "yes")
res1

# Expected result in res1
#  t.5  t.10  t.15  t.20  t.30  t.60  t.90 t.120
# TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE

# Application on the same data set but using the parameter setting
# use_ema = "no"
res2 <- get_profile_portion(
  data = dip1, tcol = 3:10, groups = (dip1$type == "R"), use_ema = "no",
  bounds = c(1, 80))
res2

# Expected result in res2
#  t.5  t.10  t.15  t.20  t.30  t.60  t.90 t.120
# TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE

# Note that if use_ema = "yes" then any setting the bounds parameter
# is ignored.
res3 <- get_profile_portion(
  data = dip1, tcol = 3:10, groups = (dip1$type == "R"), use_ema = "yes",
  bounds = c(1, 80))
res3

# Expected result in res3
#  t.5  t.10  t.15  t.20  t.30  t.60  t.90 t.120
# TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE

# If use_ema = "ignore" is set, a vector of the length of the tcol
# parameter is returned with all items set as TRUE
res4 <- get_profile_portion(
  data = dip1, tcol = 3:10, groups = (dip1$type == "R"), use_ema = "ignore",
  bounds = c(1, 80))
res4

# Expected results in res4
#  t.5  t.10  t.15  t.20  t.30  t.60  t.90 t.120
# TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE

# Longer dissolution profiles may have kinks at the later time points. Although
# indiviudal time points might be compliant with the EMA rules, these time
# points are not taken into account.
res5 <- get_profile_portion(
  data = dip6, tcol = 3:31, groups = (dip6$type == "R"), use_ema = "yes",
  bounds = c(1, 80))
res5

# Expected results in res5
#   t.0   t.5  t.10  t.15  t.20  t.25  t.30  t.35  t.40  t.45  t.50  t.55  t.60
# FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
#  t.65  t.70  t.75  t.80  t.85  t.90  t.95 t.100 t.105 t.110 t.115 t.120 t.125
#  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
# t.130 t.135 t.140
# FALSE FALSE FALSE

# Time point 120 is FALSE because it is surrounded by FALSE elements
colMeans(dip6[dip6$type == "R", 3:31])
apply(dip6[dip6$type == "R", 3:31], 2, sd) /
  apply(dip6[dip6$type == "R", 3:31], 2, mean) * 100
