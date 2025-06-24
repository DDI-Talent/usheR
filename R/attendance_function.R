#' Taking student attendance
#'
#' This function creates and optionally saves a record of student attendance for a given session.
#' You provide a class list (as a character vector or data frame with a `name` column), and index
#' which students were present using their row numbers. The result is a tibble containing student names,
#' a logical indicator of presence (`TRUE`/`FALSE`), and a session ID.
#'
#' If no session ID is provided, the function automatically uses the current date and time in the format
#' `"dd-mm-yy_HHMM"`. You can optionally write the output to a CSV file using the file_path argument.
#' If append = FALSE (default), the file will be created or overwritten.
#' If append = TRUE, the new attendance will be added to the bottom of an existing attendance log.
#' This allows you to accumulate attendance across sessions in a single file.
#'
#' @param full_class A character vector of student names or a data frame with a `name` column.
#' @param present_students A numeric vector of indices corresponding to students who were present.
#'                         These indices should match the row numbers of `full_class`.
#' @param file_path Optional. A file path (e.g., `"attendance.csv"`) where the attendance record will be saved or appended.
#' @param session_id Optional. A label for the session (e.g., `"week1"`). If `NULL`, the current date and time is used.
#' @param append Logical. If TRUE, appends to an existing file specified in `file_path`. If FALSE, overwrites the file. Default is `FALSE`.
#'
#' @returns A tibble with columns: `name` (student name), `present` (logical), and `session` (session ID).
#' @export
#'
#' @examples
#' # Load USJudgeRatings dataset and use as full class list
#' class_list <- data.frame(name = rownames(USJudgeRatings))
#'
#' # print full class list to view row indices
#' print(class_list$name)
#'
#' # week 1: take attendance. Suppose students 1:9, 15, 18, 20:22 are present.
#' attendance_week1 <- take_attendance(full_class = class_list,
#'                               present_students = c(1:9, 15, 18, 20:22),
#'                               session_id = "CourseX_Week1")
#'
#' # week 2: take attendance. Suppose students 1:5, 12, 14 are present.
#' attendance_week2 <- take_attendance(full_class = class_list,
#'                               present_students = c(1:5, 12, 14),
#'                               session_id = "CourseX_Week2")
#'
#' # You can also optionally save the attendance to csv files, or append each week
#' \dontrun{
#' # Save Week 1 to CSV (creates new file)
#' week1 <- take_attendance(full_class = class_list,
#'                          present_students = 1:5,
#'                          session_id = "Week_1",
#'                          file_path = "attendance_log.csv")
#'
#' # Append Week 2 to same file
#' week2 <- take_attendance(full_class = class_list,
#'                          present_students = c(2, 4, 6, 8),
#'                          session_id = "Week_2",
#'                          file_path = "attendance_log.csv",
#'                          append = TRUE)
#' }
#'
#'
take_attendance <- function(full_class,
                            present_students,
                            file_path = NULL,
                            session_id = NULL,
                            append = FALSE) {

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

  # Save output - default is to NOT append
  attendance <- save_output(attendance, file_path, append = append)

  return(attendance)

}







#' Save or append a data frame to CSV
#'
#' This helper function saves a data frame to a specified `.csv` file path, either by overwriting the file
#' or appending to it. If `append = TRUE` and the file already exists, the existing contents
#' are read and combined with the new data before being written back to the file.
#' If `file_path` is `NULL`, the data frame is returned without saving.
#'
#' @param dataframe A data frame to be saved.
#' @param file_path A character string specifying the CSV file path.
#' @param append Logical. If `TRUE`, appends to an existing file if it exists. If `FALSE`, the file is overwritten. Default is `FALSE`.
#'
#' @returns Returns the (optionally saved) data frame.
#'
#' @examples
#' # Load USJudgeRatings dataset and use as full class list
#' class_list <- data.frame(name = rownames(USJudgeRatings))
#'
#' # print full class list to view row indices
#' print(class_list$name)
#'
#' # Below is an example for saving
#' \dontrun{
#' # Week 1: take attendance
#' week1 <- take_attendance(full_class = class_list,
#'                          present_students = c(1, 2, 4),
#'                          file_path = "attendance.csv")  # saved using save_output()
#'
#' # Week 2: take attendance and append to same file
#' week2 <- take_attendance(full_class = class_list,
#'                          present_students = c(1, 3, 4, 5, 7, 10, 15, 17),
#'                          file_path = "attendance.csv",
#'                          append = TRUE)
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
