library(testthat)
source("functions.R")

v1 = c(1, 2, 3, 4, 5)
v2 = c(NA, 0, -1, 5, 10)
v2_no_NA = c(0, -1, 5, 10)
v3 = c(-5, -4, 0, 1, 3, 7)
v4 = c(NA, NA, 3, 2, 8)
v4_no_NA = c(3, 2, 8)

# Testing remove_missing
context("Testing remove_missing")
expect_equal(remove_missing(v4), (v4_no_NA))
expect_equal(remove_missing(v3), (v3))
expect_equal(remove_missing(v2), (v2_no_NA))
expect_equal(remove_missing(v1), (v1))

# Testing get_minimum
context("Testing get_minimum")
expect_equal(get_minimum(v4, T), 2)
expect_equal(get_minimum(v3, T), -5)
expect_equal(get_minimum(v2, F), as.double(NA))
expect_equal(get_minimum(v1, T), 1)

# Testing get_maximum
context("Testing get_maximum")
expect_equal(get_maximum(v4, T), 8)
expect_equal(get_maximum(v3, F), 7)
expect_equal(get_maximum(v2, T), 10)
expect_equal(get_maximum(v1, T), 5)

# Testing get_range
context("Testing get_range")
expect_equal(get_range(v4, T), 6)
expect_equal(get_range(v3, T), 12)
expect_equal(get_range(v2, T), 11)
expect_equal(get_range(v1, T), 4)

# Testing get_median
context("Testing get_median")
expect_equal(get_median(v4, F), median(v4, F))
expect_equal(get_median(v3, T), median(v3, T))
get_median(v3, T)
median(v3, T)
expect_equal(get_median(v2, T), median(v2, T))
expect_equal(get_median(v1, T), median(v1, T))

# Testing get_average
context("Testing get_average")
expect_equal(get_average(v4, F), mean(v4, na.rm = F))
expect_equal(get_average(v3, T), mean(v3, na.rm = T))
expect_equal(get_average(v2, T), mean(v2, na.rm = T))
expect_equal(get_average(v1, T), mean(v1, na.rm = T))

# Testing get_stdev
context("Testing get_stdev")
expect_equal(get_stdev(v4, T), sd(v4, T))
expect_equal(get_stdev(v3, T), sd(v3, T))
expect_equal(get_stdev(v2, F), sd(v2, F))
expect_equal(get_stdev(v1, T), sd(v1, T))

# Testing get_quartile1
context("Testing get_quartile1")
expect_equal(get_quartile1(v4, T), quantile(v4, .25, na.rm = T)[[1]])
expect_equal(get_quartile1(v3, T), quantile(v3, .25, na.rm = T)[[1]])
expect_equal(get_quartile1(v2, T), quantile(v2, .25, na.rm = T)[[1]])
expect_equal(get_quartile1(v1, T), quantile(v1, .25, na.rm = T)[[1]])

# Testing get_quartile3
context("Testing get_quartile3")
expect_equal(get_quartile3(v4, T), quantile(v4, .75, na.rm = T)[[1]])
expect_equal(get_quartile3(v3, T), quantile(v3, .75, na.rm = T)[[1]])
expect_equal(get_quartile3(v2, T), quantile(v2, .75, na.rm = T)[[1]])
expect_equal(get_quartile3(v1, T), quantile(v1, .75, na.rm = T)[[1]])

# Testing get_percentile10
context("Testing get_percentile10")
expect_equal(get_percentile10(v4, T), quantile(v4, .1, na.rm = T)[[1]])
expect_equal(get_percentile10(v3, T), quantile(v3, .1, na.rm = T)[[1]])
expect_equal(get_percentile10(v2, T), quantile(v2, .1, na.rm = T)[[1]])
expect_equal(get_percentile10(v1, T), quantile(v1, .1, na.rm = T)[[1]])

# Testing get_percentile90
context("Testing get_percentile90")
expect_equal(get_percentile90(v4, T), quantile(v4, .9, na.rm = T)[[1]])
expect_equal(get_percentile90(v3, T), quantile(v3, .9, na.rm = T)[[1]])
expect_equal(get_percentile90(v2, T), quantile(v2, .9, na.rm = T)[[1]])
expect_equal(get_percentile90(v1, T), quantile(v1, .9, na.rm = T)[[1]])

# Testing count_missing
context("Testing count_missing")
expect_equal(count_missing(v4), 2)
expect_equal(count_missing(v3), 0)
expect_equal(count_missing(v2), 1)
expect_equal(count_missing(v1), 0)

# Testing summary_stats
context("Testing summary_stats")
v1_summary = list(
  minimum = min(v1, na.rm = T),
  percent10 = quantile(v1, .1, na.rm = T)[[1]],
  quartile1 = quantile(v1, .25, na.rm = T)[[1]],
  median = median(v1, na.rm = T),
  mean = mean(v1, na.rm = T),
  quartile3 = quantile(v1, .75, na.rm = T)[[1]],
  percent90 = quantile(v1, .9, na.rm = T)[[1]],
  maximum = max(v1, na.rm = T),
  range = 4,
  stdev = sd(v1, na.rm = T),
  missing = 0
)
expect_equal(summary_stats(v1), v1_summary)

v2_summary = list(
  minimum = min(v2, na.rm = T),
  percent10 = quantile(v2, .1, na.rm = T)[[1]],
  quartile1 = quantile(v2, .25, na.rm = T)[[1]],
  median = median(v2, na.rm = T),
  mean = mean(v2, na.rm = T),
  quartile3 = quantile(v2, .75, na.rm = T)[[1]],
  percent90 = quantile(v2, .9, na.rm = T)[[1]],
  maximum = max(v2, na.rm = T),
  range = 11,
  stdev = sd(v2, na.rm = T),
  missing = 1
)
expect_equal(summary_stats(v2), v2_summary)

v3_summary = list(
  minimum = min(v3, na.rm = T),
  percent10 = quantile(v3, .1, na.rm = T)[[1]],
  quartile1 = quantile(v3, .25, na.rm = T)[[1]],
  median = median(v3, na.rm = T),
  mean = mean(v3, na.rm = T),
  quartile3 = quantile(v3, .75, na.rm = T)[[1]],
  percent90 = quantile(v3, .9, na.rm = T)[[1]],
  maximum = max(v3, na.rm = T),
  range = 12,
  stdev = sd(v3, na.rm = T),
  missing = 0
)
expect_equal(summary_stats(v3), v3_summary)

v4_summary = list(
  minimum = min(v4, na.rm = T),
  percent10 = quantile(v4, .1, na.rm = T)[[1]],
  quartile1 = quantile(v4, .25, na.rm = T)[[1]],
  median = median(v4, na.rm = T),
  mean = mean(v4, na.rm = T),
  quartile3 = quantile(v4, .75, na.rm = T)[[1]],
  percent90 = quantile(v4, .9, na.rm = T)[[1]],
  maximum = max(v4, na.rm = T),
  range = 6,
  stdev = sd(v4, na.rm = T),
  missing = 2
)
expect_equal(summary_stats(v4), v4_summary)

# Testing rescale100
context("Testing rescale100")
expect_equal(rescale100(c(1, 2, 3), 0, 10), c(10, 20, 30))
expect_equal(rescale100(c(1, 2, 3), 0, 16), c(6.25, 12.5, 18.75))
expect_equal(rescale100(c(1, 2, 3), 0, 20), c(5, 10, 15))
expect_equal(rescale100(c(10, 100, 1000), 0, 100), c(10, 100, 1000))

# Testing drop_lowest
context("Testing drop_lowest")
expect_equal(drop_lowest(v1), c(2, 3, 4, 5))
expect_equal(drop_lowest(v2), c(0, -1, 5, 10))
expect_equal(drop_lowest(v3), c(-4, 0, 1, 3, 7))
expect_equal(drop_lowest(v4), c(NA, 3, 2, 8))

# Testing score_homework
context("Testing score_homework")
expect_equal(score_homework(v1, F), mean(v1, na.rm = T))
expect_equal(score_homework(v2, T), mean(drop_lowest(v2), na.rm = T))
expect_equal(score_homework(v3, T), mean(drop_lowest(v3), na.rm = T))
expect_equal(score_homework(v4, F), mean(v4, na.rm = T))

# Testing score_quiz
context("Testing score_quiz")
expect_equal(score_quiz(v1, T), mean(drop_lowest(v1), na.rm = T))
expect_equal(score_quiz(v2, F), mean(v2, na.rm = T))
expect_equal(score_quiz(v3, F), mean(v3, na.rm = T))
expect_equal(score_quiz(v4, T), mean(drop_lowest(v4), na.rm = T))

# Testing score_lab
context("Testing score_lab")
expect_equal(score_lab(11), 100)
expect_equal(score_lab(7), 20)
expect_equal(score_lab(5), 0)
expect_equal(score_lab(8), 40)
