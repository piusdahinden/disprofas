tmp <- mztia(data = dip5, shape = "long", tcol = 3, grouping = "type",
             reference = "reference", cap = FALSE)

# (Expected) results in tmp[["Data"]][101:107, ]
#      frame  grouping  type type2 time response
# 101 limits reference  Mean  Mean    1 12.00930
# 102 limits reference    TL   LTL    1 11.87152
# 103 limits reference    TL   UTL    1 12.14708
# 104 limits reference TL.S1   LTL    1  6.87152
# 105 limits reference TL.S1   UTL    1 17.14708
# 106 limits reference TL.S2   LTL    1 -3.12848
# 107 limits reference TL.S2   UTL    1 27.14708
