context("f1")

test_that("f1_results_match", {
  time_points <- suppressWarnings(as.numeric(gsub("([^0-9.])([^0-9])", "",
                                                    colnames(dip2))))
  tico <- which(!is.na(time_points))
  tcol <- tico[2:5]

  # <-><-><-><->

  l_res1 <- f1(data = dip2[dip2$batch %in% c("b0", "b1"), ],
               tcol = tcol, grouping = "batch", useEMA = "no", uprellim = 100)
  l_res2 <- f1(data = dip2[dip2$batch %in% c("b0", "b2"), ],
               tcol = tcol, grouping = "batch", useEMA = "no", uprellim = 100)
  l_res3 <- f1(data = dip2[dip2$batch %in% c("b0", "b3"), ],
               tcol = tcol, grouping = "batch", useEMA = "no", uprellim = 100)
  l_res4 <- f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
               tcol = tcol, grouping = "batch", useEMA = "no", uprellim = 100)
  l_res5 <- f1(data = dip2[dip2$batch %in% c("b0", "b5"), ],
               tcol = tcol, grouping = "batch", useEMA = "no", uprellim = 100)

  # <-><-><-><->

  expect_equal(signif(l_res1$f1, 7), 8.729032)
  expect_equal(signif(l_res2$f1, 7), 13.20579)
  expect_equal(signif(l_res3$f1, 7), 13.67089)
  expect_equal(signif(l_res4$f1, 7), 7.379616)
  expect_equal(signif(l_res5$f1, 7), 13.94996)
})

test_that("f1_fails", {
  time_points <- suppressWarnings(as.numeric(gsub("([^0-9.])([^0-9])", "",
                                                  colnames(dip2))))
  tico <- which(!is.na(time_points))
  tcol <- tico[2:5]

  tmp0 <- dip2
  tmp0$t.30 <- as.factor(tmp0$t.30)

  tmp1 <- dip2
  tmp1$batch <- as.character(tmp1$batch)

  tmp2 <- rbind(dip2[dip2$batch == "b0", ],
               dip2[dip2$batch == "b4" & dip2$tablet %in% as.character(1:6), ])

  # <-><-><-><->

  expect_error(
    f1(data = as.matrix(dip2[dip2$batch %in% c("b0", "b4"), tcol]),
       tcol = tcol[1:2], grouping = "batch", useEMA = "no",
       lorellim = 1, uprellim = 85),
    "data must be provided as data frame")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = "tcol", grouping = "batch", useEMA = "no",
       lorellim = 1, uprellim = 85),
    "tcol must be an integer vector")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = tcol[1:2], grouping = "batch", useEMA = "no",
       lorellim = 1, uprellim = 85),
    "tcol must be an integer vector")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = tcol + 0.1, grouping = "batch", useEMA = "no",
       lorellim = 1, uprellim = 85),
    "tcol must be an integer vector")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 5:9, grouping = "batch", useEMA = "no",
       lorellim = 1, uprellim = 85),
    "Some columns specified by tcol")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = 3:8, grouping = "batch", useEMA = "no",
       lorellim = 1, uprellim = 85),
    "Some names of columns specified by tcol")
  expect_error(
    f1(data = tmp0[tmp0$batch %in% c("b0", "b4"), ],
       tcol = tcol, grouping = "batch", useEMA = "no",
       lorellim = 1, uprellim = 85),
    "Some columns specified by tcol are not numeric")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = tcol, grouping = 5, useEMA = "no",
       lorellim = 1, uprellim = 85),
    "grouping must be string")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = tcol, grouping = "lot", useEMA = "no",
       lorellim = 1, uprellim = 85),
    "grouping variable was not found")
  expect_error(
    f1(data = tmp1[tmp1$batch %in% c("b0", "b4"), ],
       tcol = tcol, grouping = "batch", useEMA = "no",
       lorellim = 1, uprellim = 85),
    "grouping variable's column in data")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b3", "b4"), ],
       tcol = tcol, grouping = "batch", useEMA = "no",
       lorellim = 1, uprellim = 85),
    "number of levels in column")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = tcol, grouping = "batch", useEMA = "maybe",
       lorellim = 1, uprellim = 85),
    "specify useEMA either as \"yes\" or \"no\"")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = tcol, grouping = "batch", useEMA = 0,
       lorellim = 1, uprellim = 85),
    "specify useEMA either as \"yes\" or \"no\" or \"ignore\"")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = tcol, grouping = "batch", useEMA = "no",
       lorellim = "lorel", uprellim = 85),
    "lorellim must be single number >= 0 and < uprellim")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = tcol, grouping = "batch", useEMA = "no",
       lorellim = 85, uprellim = 1),
    "lorellim must be single number >= 0 and < uprellim")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b4"), ],
       tcol = tcol, grouping = "batch", useEMA = "no",
       lorellim = 1, uprellim = "uprel"),
    "uprellim must be a single number <= 100 and > lorellim")
  expect_error(
    f1(data = tmp2,
       tcol = tcol, grouping = "batch", useEMA = "yes",
       lorellim = 1, uprellim = 85),
    "The two groups to be compared")
  expect_error(
    f1(data = dip2[dip2$batch %in% c("b0", "b1"), ],
       tcol = 6:8, grouping = "batch", useEMA = "yes",
       lorellim = 1, uprellim = 85),
    "According to EMA the profiles")
})

test_that("f1_warns", {
  time_points <- suppressWarnings(as.numeric(gsub("([^0-9.])([^0-9])", "",
                                                  colnames(dip2))))
  tico <- which(!is.na(time_points))
  tcol <- tico[2:5]

  tmp <- rbind(dip2[dip2$batch == "b0", ],
               dip2[dip2$batch == "b4" & dip2$tablet %in% as.character(1:6), ])

  # <-><-><-><->

  expect_warning(
    f1(data = tmp,
       tcol = tcol, grouping = "batch", useEMA = "no",
       lorellim = 1, uprellim = 85),
    "The two groups to be compared")
  expect_error(
    expect_warning(
      f1(data = dip2[dip2$batch %in% c("b0", "b1"), ],
         tcol = tcol[2:4], grouping = "batch", useEMA = "no",
         lorellim = 1, uprellim = 85),
      "According to EMA the two profiles"),
  "The parameter tcol must be an integer")
})

## <><><><><><><><><><><><><><><><><><><><><><><><><><><><>
