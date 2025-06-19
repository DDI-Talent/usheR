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
    session_id <- Sys.time()
  }

  # Create logical vector for attendance
  present_logical <- seq_along(full_class) %in% present_students

  attendance <- tibble::tibble(
    name = full_class,
    present = present_logical,
    session = session_id
  )

  # Save - default is to NOT append
  attendance <- save_output(attendance, file_path)

  return(attendance)

}







#' Save a data frame to CSV
#'
#' Saves or appends a data frame to a specified file path.
#' If `append = TRUE`, combines the new data with any existing data in the file.
#'
#' @param dataframe The data frame to save.
#' @param file_path Path to the CSV file. If `NULL`, nothing is saved.
#' @param append Logical. If TRUE, appends to existing file; if FALSE, overwrites (default: FALSE).
#'
#' @returns Returns the saved data frame.
#' @export
#'
#' @examples
#' # Create a class list
#' class_list <- LETTERS[1:26]
#' # Below is an example for saving
#' \dontrun{
#' # Week 1: take attendance
#' week1 <- take_attendance(full_class = class_list,
#'                          present_students = c(1, 2, 4),
#'                          file_path = "attendance_log_W1.csv")  # saved using save_output()
#'
#' # Week 1: generate student pairs and save
#' pairsW1 <- student_pairs(attendance = week1,
#'                        group_size = 2,
#'                        file_path = "pairs_week1.csv")  # saved using save_output()
#'
#' # Week 2: take attendance and append to same file
#' week2 <- take_attendance(full_class = class_list,
#'                          present_students = c(1, 3, 4, 5, 7, 10, 15, 17),
#'                          file_path = "attendance_log_W2.csv")
#'
#' # Week 2: generate student pairs and save
#' pairsW2 <- student_pairs(attendance = week2,
#'                        pair_history = pairsW1,
#'                        group_size = 2,
#'                        file_path = "pairs_week2.csv")
#'
#'}
save_output <- function(dataframe,
                        file_path,
                        append = FALSE) {

  if (!is.null(file_path)) {
    if (append && file.exists(file_path)) {
      existing <- readr::read_csv(file_path, show_col_types = FALSE)
      dataframe <- dplyr::bind_rows(existing, dataframe)
    }

    write.csv(dataframe, file_path, row.names = FALSE)

  }

  return(dataframe)

}
