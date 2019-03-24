
# Documentation for the user-defined function make_date_yw()
#
# This function parses epidemiological weeks into dates. In other words, it
# calculate Gregorian calendar dates based on epidemiological years, week
# numbers, and weekday numbers.
#
# Arguments:
# 
#   year     A vector of epidemiological years (e.g. 2017).
#   week     A vector of epidemiological week numbers (1—53).
#   weekday  A vector of epidemiological weekday numbers (1—7). In the WHO
#            implementation, weeks start on Monday so Mondays have a value of 1
#            and Sundays have a value of 7.
#
# Arguments year, week, and weekday are recycled as necessary. You can, for 
# example, give the function a single year and multiple week numbers.
#
# The function returns a vector of class `Date`.
#
# For a given date, a missing value (NA) in any of the input arguments yields
# an NA for the output.
#

make_date_yw <- function(year = 1970L, week = 1L, weekday = 1L) {
  
  # if all arguments are zero-length, return a zero-length date vector
  lengths <- vapply(list(year, week, weekday), length, 1, USE.NAMES = FALSE)
  if (min(lengths) == 0L) as.Date(integer(), lubridate::origin)
  
  # recycle arguments
  N <- max(lengths)
  y <- rep_len(as.integer(year), N)
  w <- rep_len(as.integer(week), N)
  d <- rep_len(as.integer(weekday), N)
  
  # calculate output vector
  out <-
    ifelse(
      is.na(y) | is.na(w) | is.na(d), NA,
      {
        jan1 <- lubridate::make_date(y, 1, 1)
        wday <- as.integer(lubridate::wday(jan1 - 1))
        to_add <- ifelse(wday <= 4, 1, 8) - wday
        wk1 <- jan1 + to_add
        day1 <- wk1 + (w - 1) * 7
        day1 + d - 1
      }
    )
  
  # ifelse returns numeric not Date objects, so convert it back to Date
  as.Date(out, lubridate::origin)
  
}
