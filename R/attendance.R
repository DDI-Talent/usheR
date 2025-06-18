#' Get present students
#'
#' Helper function to return the list of present students, based on either those present or absent.
#'
#' @param full_class A character vector of all student names in the class, or a data frame containing a `name` column.
#' @param present Optional character vector of students who are present.
#' @param absent Optional character vector of students who are absent.
#'
#' @returns A character vector of students who are present.
#'
#' @examples
#' # Create full class list as vector input (note can also be done as dataframe)
#' full_class <- LETTERS[1:26]
#' # If only some student are present:
#' present <- get_present_students(full_class, present = c("A", "B", "C"))
#' # If majority student present:
#' present <- get_present_students(full_class, absent = c("D", "E"))
#'
get_present_students <- function(full_class, present = NULL, absent = NULL){

  # If it is a data frame, extract the `name` column
  if (is.data.frame(full_class)) {
    full_class <- full_class$name
  }

  # Return present students
  if (!is.null(present)) {
    return(intersect(full_class, present))
  }

  # Return present students by removing absent
  if (!is.null(absent)) {
    return(setdiff(full_class, absent))
  }

}




#' Taking student attendance
#'
#' This function creates and optionally saves a record of student attendance for a given session.
#' You can specify students who are present or absent. The result is a tibble with student names,
#' a logical indicator of presence, and the session ID.
#'
#' @param full_class A character vector of student names or a data frame with a `name` column.
#' @param present Optional. A character vector of names who are present. If provided, others are marked absent.
#' @param absent Optional. A character vector of names who are absent. If provided, others are marked present.
#' @param file_path Optional. A file path (e.g., `"attendance.csv"`) where the attendance record will be saved or appended.
#' @param session_id Optional. A label for the session (e.g., `"session_1"`). If `NULL`, the current date is used (in `dd-mm-yy` format).
#'
#' @returns A tibble with columns: `name` (student name), `present` (logical), and `session` (session ID).
#' @export
#'
#' @examples
#' full_class <- LETTERS[1:26]
#' attendance <- take_attendance(full_class = class_list_2025, absent = c("B", "M", "O", "Z"))
#' matrix(class_list_2025$name)
#' #index!!
#'
#'
take_attendance <- function(full_class,
                            present = NULL,
                            absent = NULL,
                            file_path = NULL, # FOR EASY APPENDING, DO WE SPECIFY AN ATTENDANCE FILE NAME HERE? LIKE "ATTENDANCE.CSV"??
                            session_id = NULL) {

  # If it is a data frame, extract the `name` column: character vector of names
  if (is.data.frame(full_class)) {
    full_class <- full_class$name
  }

  # If no session label use date
  if (is.null(session_id)) {
    session_id <- format(Sys.Date(), "%d-%m-%y")
  }

  # Get list of present students
  present_students <- get_present_students(full_class,
                                           present,
                                           absent)

  # Create attendance tibble
  attendance <- tibble::tibble(
    name = full_class,
    present = full_class %in% present_students, # doesnt work for us
    session = session_id
  )

  # Save or append
  if (!is.null(file_path)) {
    if (file.exists(file_path)) {
      existing <- readr::read_csv(file_path,
                                  stringsAsFactors = FALSE)
      attendance <- dplyr::bind_rows(existing, attendance)
    }
    # Save updated file
    write.csv(attendance, file_path, row.names = FALSE)
  }

  # 1 function takes attendance
  # 1 function  pairs
  # 1 function that saves
  #

  return(attendance)

}
