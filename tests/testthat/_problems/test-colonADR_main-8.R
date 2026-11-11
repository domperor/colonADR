# Extracted from test-colonADR_main.R:8

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "colonADR", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
data<-read.csv("sample_CF_data.csv")
x<-apcadr(data$統計_診断)
expect_equal(as.numeric(format(x$APCstrict, digits = 5)), 0.59596)
expect_equal(as.numeric(format(x$APCloose, digits = 5)), 0.60606)
expect_equal(as.numeric(format(x$ADRstrict, digits = 5)), 0.39394)
expect_equal(as.numeric(format(x$ADRloose, digits = 5)), 0.39394)
expect_equal(as.numeric(format(x$PPC, digits = 5)), 0.60606)
