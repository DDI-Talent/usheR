#' Marking timeline
#'
#' Calculate the marking timeline for a final assesment based on a given course start date.
#' Assumes:
#' - deadline is Monday of week 5
#' - calibration is 2 days after deadline
#' - moderation is 14 days after calibration
#' - release is 21 days after deadline
#'
#' Bit bit of a pointless function. But loads of potential improvements, e.g. param for current week, instead of course start date.
#'
#' @param start Character, or Date; The start date of the course in 'YYYY-MM-DD' format.
#'
#' @returns Dataframe with the marking timeline.
#' @export
#'
#' @examples
#' set_marking_timeline('2025-17-02')
#' set_marking_timeline('2025-04-06')
#'
set_marking_timeline <- function(start) {
  start <- as.Date(start)
  deadline <- start + 49
  calibration <- deadline + 2
  moderation <- calibration + 14
  release <- deadline + 21

  timeline_df <- data.frame(
    event = c('start', 'deadline', 'calibration', 'moderation', 'release'),
    date = c(start, deadline,  calibration,  moderation,  release)
  )

  timeline_df$days_until <- as.numeric(timeline_df$date - Sys.Date())

  timeline_df
}

