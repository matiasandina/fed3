# Load required libraries
library(lubridate)
library(tibble)

# Define a toy dataset
duplicate_test_data <- tibble(
  datetime = c("2023-06-10 08:00:00", "2023-06-10 08:01:00", "2023-06-10 08:02:00",
               "2023-06-10 08:03:00", "2023-06-10 08:04:00", "2023-06-10 08:05:00",
               "2023-06-10 08:06:00", "2023-06-10 08:06:00", "This will fail", "2023-06-10 08:07:00",
               "2023-06-10 08:08:00", "2023-06-10 08:09:00", "2023-06-10 08:10:00",
               "2023-06-10 08:11:00", "2023-06-10 08:11:00", "2023-06-10 08:12:00",
               "2023-06-10 08:13:00", "This will fail too", "2023-06-10 08:14:00", "2023-06-10 08:15:00"),
  Pellet_Count = c(1, 2, 3, 4, 5, 6, 7, 8, NA, 10, 1, 2, 3, 4, 5, 6, 7, NA, 9, 10),
  Left_Poke_Count = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  Right_Poke_Count = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
)
duplicate_test_data$row_id <- 1:nrow(duplicate_test_data)

# Write the data
usethis::use_data(duplicate_test_data, overwrite = T)
