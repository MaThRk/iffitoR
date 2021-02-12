#' function to information about the dates and the temporal information of landslides
#'
#' @importFrom lubridate yday
#'
#' @param df object as returned from \code{iffitoR::make_shapefile}
#'
#' @return a spatial dataframe of class \code{sf}
#'

#' \itemize{
#' \item \code{date_info} indicating the available time information for that movement ("no date", "year", "month", "day")
#' \item \code{date} column of class \code{date} indicating the date (if available)
#' \item \code{year.int} year as integer
#' \item \code{month.int} month as integer
#' \item \code{day.int} day as integer
#' \item \code{year.posix} year (year-01-01) as object of class \code{date}
#' }
#'
#' @export
get_date_information = function(df) {

  df = df %>%
    mutate(
      year_present = if_else(!is.na(anno_min), true = TRUE, false = FALSE),
      month_present = if_else(!is.na(mese_min), true = TRUE, false = FALSE),
      day_present = if_else(!is.na(giorno_min), true = TRUE, false = FALSE)
    )


  df = df %>%
    mutate(date_info = if_else(
      day_present,
      true = "day",
      false = if_else(
        month_present,
        true = "month",
        false = if_else(year_present, true = "year", false =
                          "no date")
      )))


  df = df %>% mutate(date = paste(anno_min, mese_min, giorno_min, sep = "/")) %>%
    mutate(date = as.Date(date),
           year.int = anno_min, # year as int
           month.int = mese_min,
           week = as.integer(paste0(year.int, formatC(lubridate::week(date), flag=0, width=2))),
           day.int = giorno_min,
           doy = lubridate::yday(date),
           year.posix = as.Date(paste0(anno_min, "-01-01")))

  # remove the italian time information
  df = df %>%
    select(-c(anno_min, mese_min, giorno_min))

  cat("\nAdded the columns:\n\n date_info (chr) - (eiher 'year', 'month', 'day' or 'no date')\n year.int (integer)
 date (the day as date-object)
 doy (numeric day of the year)
 month.int (integer of the month)
 week (yyyyww e.g. 200201 for the first week in 2002)
 day.int (integer of the day in the month)
 year.posix (date) (object of class date, referenced to the 1st of January of the year)\n\n")

  return(df)

}
