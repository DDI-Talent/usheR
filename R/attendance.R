#' Taking student attendance
#'
#' This function creates and optionally saves a record of student attendance for a given session.
#' You provide a class list (as a character vector or data frame with a `name` column), and index
#' which students were present using their row numbers. The result is a tibble containing student names,
#' a logical indicator of presence (`TRUE`/`FALSE`), and a session ID.
#'
#' If no session ID is provided, the function automatically uses the current date and time in the format
#' `"dd-mm-yy_HHMM"`.
#' Optionally saves or appends the attendance to a CSV file via `file_path`.
#'
#' @param full_class A character vector of student names or a data frame with a `name` column.
#' @param present_students A numeric vector of indices corresponding to students who were present.
#'                         These indices should match the row numbers of `full_class`.
#' @param file_path Optional. A file path (e.g., `"attendance.csv"`) where the attendance record will be saved or appended.
#' @param session_id Optional. A label for the session (e.g., `"session_1"`). If `NULL`, the current date and time is used (in `dd-mm-yy_HHMM` format).
#'
#' @returns A tibble with columns: `name` (student name), `present` (logical), and `session` (session ID).
#' @export
#'
#' @examples
#' # Full class list
#' full_class <- LETTERS[1:26]
#' # print full class list to view row indices
#' print(full_class)
#' # suppose students 1:9, 15, 18, 20:22 are present.
#' present <- c(1:9, 15, 18, 20:22)
#' # take attendance
#' attendance <- take_attendance(full_class = full_class,
#'                               present_students = present)
#'
take_attendance <- function(full_class,
                            present_students,
                            file_path = NULL,
                            session_id = NULL) {

  # If it is a data frame, extract the `name` column: character vector of names
  if (is.data.frame(full_class)) {
    full_class <- full_class$name
  }

  # If no session label, use date and time
  if (is.null(session_id)) {
    session_id <- format(Sys.time(), "%d-%m-%y_%H%M")
  }

  # Create logical vector for attendance
  present_logical <- seq_along(full_class) %in% present_students

  # Create attendance tibble
  attendance <- tibble::tibble(
    name = full_class,
    present = present_logical,
    session = session_id
  )

  # Save - default is to append
  attendance <- save_output(attendance, file_path)

  return(attendance)

}




