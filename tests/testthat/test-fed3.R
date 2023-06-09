# Load required libraries
library(testthat)
library(dplyr)
library(fed3)

# Read the test data
data <- duplicate_test_data
data$datetime <- lubridate::parse_date_time(data$datetime, "ymd HMS")

# Define the tests
testthat::test_that("deduplicate_datetime works correctly", {
  # Test 'keep_first' method
  result <- deduplicate_datetime(data, method = 'keep_first')
  testthat::expect_false(any(duplicated(result$datetime)))
  print("Testing deduplicate method = 'keep_first'")
  print(result)

  # Test 'keep_last' method
  result <- deduplicate_datetime(data, method = 'keep_last')
  testthat::expect_false(any(duplicated(result$datetime)))
  print("Testing deduplicate method = 'keep_last'")
  print(result)

  # Test 'remove' method
  result <- deduplicate_datetime(data, method = 'remove')
  testthat::expect_false(any(duplicated(result$datetime)))
  print("Testing deduplicate method = 'remove'")
  print(result)

  # Test 'offset' method
  result <- deduplicate_datetime(data, method = 'offset')
  testthat::expect_false(any(duplicated(result$datetime)))
  print("Testing deduplicate method = 'offset'")
  print(result)

  # Test 'offset' method with NA value(s)
  data_na <- data
  data_na[which(is.na(data_na$datetime)), "datetime"] <- NA
  result <- deduplicate_datetime(data_na, method = 'offset')
  testthat::expect_false(any(duplicated(result$datetime)))
  print("Testing deduplicate method = 'offset' with NAs")
  print(result)

  # Test with `reset_counts` TRUE
  result <- deduplicate_datetime(data, method = 'keep_first', reset_counts = TRUE)
  testthat::expect_true(all(diff(result$Pellet_Count) >= 0))
  testthat::expect_true(all(diff(result$Left_Poke_Count) >= 0))
  testthat::expect_true(all(diff(result$Right_Poke_Count) >= 0))
  print("Testing deduplicate method = 'keep_first', reset_counts = TRUE")
  print(result)
})
