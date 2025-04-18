# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Wed Apr  2 11:42:35 2025
# ------------------------------------------------- #

remove(list = ls())

# Wrong index
expect_error(ReadAHMD(what = "DxDD"))

# Wrong region
expect_error(ReadAHMD(what    = "Dx",
                      regions = "ACTT"))

# Wrong interval
expect_error(ReadAHMD(what     = "Dx",
                      regions  = "ACT",
                      interval = "1x50"))

# Wrong region for the index
expect_error(
  ReadAHMD(what     = "LT_fc",
           regions  = "TAS_LT_fc",
           interval = "1x1",
           show     = FALSE))

# Wrong interval for the index
expect_error(
  ReadAHMD(what     = "e0",
           regions  = "TAS",
           interval = "5x1",
           show     = FALSE))

expect_output(
  print(AHMD_sample)
)

# expect_error(ReadAHMD(what = "LT_f", regions = "TAS", interval = "1x1"))


# The tests below have been removed because in case the internet source is
# temporary not working the CRAN will consider it as a software failure and will
# demand correction. Unfortunately, we can test only the error messages i.e. the
# automated checks put in place.

# # Test the show arg and print function
# expect_silent(D <- ReadAHMD(what     = "LT_f",
#                             regions  = "ACT",
#                             interval = "5x10",
#                             show     = F))
# expect_output(print(D))
