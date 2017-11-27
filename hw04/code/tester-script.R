# test script
library(testthat)
# source in functions to be tested
source('functions.R')
sink('../output/test-reporter.txt')
test_file('tests.R')
sink()
library(shiny)
# Run an app from a subdirectory in the repo
runGitHub("stat133-hws-fall17", "jessegao", subdir = "hw04/app")
