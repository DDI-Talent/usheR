#' Collapse pairs into single string
#'
#' It is expected that `pair` will be a vector of characters (probably student
#' names), but will also work with a numeric vector.
#'
#' @param pair Vector of elements to combine
#'
#' @return Character vector, length 1
#'
#' @examples
#'
#' \dontrun{
#' sort_and_combine(c('ZZZ', 'AAA'))
#' # [1] "AAA~~ZZZ"
#' }
#'
sort_and_combine <- function(pair) {
  paste(sort(pair), collapse = '~~')
}

#' Assess the "fitness" of a group
#'
#' Counts the number of pairs in the current group that occur in the previous record of groups. Fewer matches means better fitness.
#'
#' @param pairs List of character vectors, where each list element represent the pair two individuals that form a combined pair
#' @param record List of combined pairings
#'
#' @return Integer; the number of duplicate pairs
#'
#' @examples
#' \dontrun{
#'
#' record <- list(
#'               c('AAA~~BBB','WWW~~XXX'),
#'               c('YYY~~ZZZ', 'CCC~~DDD')
#'           )
#'
#' pairs <- list(
#'               c('AAA', 'BBB'),
#'               c('CCC', 'DDD'),
#'               c('EEE', 'FFF')
#'          )
#'
#' fitness_function(pairs, record)
#' }
#'
fitness_function <- function(pairs, record) {
  pairs_strings <- sapply(pairs, sort_and_combine)
  sum(pairs_strings %in% unlist(record))
}


#' Create student pair-groups
#'
#' Generate pair-groups from a list of student names, addressing the difficulty
#' of minimizing duplicate pairings in repeated groupings. #' Finding optimal
#' pairs can be complex, so this function uses a simulation approach inspired by
#' principles of evolutionary computing. It assesses the "fitness" of each
#' proposed group by penalizing duplicate pairings, thereby ensuring that the
#' fewest number of repeated pairs results in the highest fitness score.
#' Additionally, users can supply a record of previous pairings to inform the
#' fitness computation.
#'
#' @param pool Vector of student names
#' @param group_size Integer; Number of students per group
#' @param record List; List of pairings from previous weeks
#' @param population Integer; Number of simulated "organisms" to sample. I.e.
#'   how long to spend searching for a better solution. Default is 1000.
#'
#' @return List of pairs groupings. Each list element is a character vector of
#'   pairs.
#' @export
#'
#' @examples
#' #' ## for 1 week
#' num_students <- 8
#' create_pairs(LETTERS[1:num_students])
#'
#' # ------------------------------------ #
#'
#' ## loop for n_weeks weeks
#' record <- NULL
#' n_weeks <- 5
#' for (i in 1:n_weeks){
#'   # to simulate varying numbers each week
#'   num_students <- sample(7:10, 1)
#'
#'   record <- create_pairs(LETTERS[1:num_students],
#'                          record = record)
#' }
#'
#' record
#'
create_pairs <- function(pool, group_size = 2,
                         record = NULL, population = 1000) {

  pool_idx <- seq_along(pool)
  number_of_groups <- length(pool) %/% group_size
  group_names <- rep(seq_len(number_of_groups),
                     times = group_size + 1)
  group_names <- group_names[pool_idx]

  winning_dups  <- Inf
  how_many_dups <- Inf
  winning_pairs <- c()
  i <- 0
  while (how_many_dups > 0 && i < population) {
    i <- i + 1
    pairs <- split(sample(pool), group_names)

    how_many_dups <- fitness_function(pairs, record)

    if (how_many_dups <= winning_dups) {
      winning_dups  <- how_many_dups
      winning_pairs <- sapply(pairs, sort_and_combine)
    }
  }

  structure(c(list(winning_pairs), record),
            class = c('pairings', 'list'))

}


#' Print method for pair record
#'
#' @param x Pairings; list
#' @param n Number of weeks to show
#' @param ... Not used
#'
#' @return Invisibly returns x
#' @export
#' @keywords internal
#'
print.pairings <- function(x, n = 1, ...) {
  n <- seq_len(n)
  mapply(\(group, week) {
    cat('\n\033[32mWeek', week, '\033[0m')


    lapply(seq_along(group), \(i) {
      cat('\n  \033[34m~ Group', i, '\033[0m\n    - ')
      cat(strsplit(group[i], '~~')[[1]], sep = '\n    - ')
    })


  }, x[n], rev(seq_along(x))[n])
  invisible(x)
}


#' Convert pairings list to tibble
#'
#' A helper function to take a list of pairings returned by `create_pairs()`, and convert it into a tidy tibble
#' with one row per group per week.
#' Optionally, the tibble can be saved as a CSV file if `file_path` is specified.
#'
#' @param pairings A pairings list object returned by `create_pairs()`.
#' @param file_path Optional. A character string specifying a file path to save the output tibble as a CSV file. IF `NULL` (default), no file is saved.
#'
#' @returns A tibble with the columns : `week` (Integer), `group` (Character), and `pairing` (Character).
#'
#' @examples
#' # Create a static roster
#' ## Note this input needs to be a list for `create_pairs()`
#' roster <- LETTERS[1:10]
#'
#' # Generate pairs
#' pairings <- create_pairs(roster)
#'
#' # Convert to tibble
#' data_frame <- convert_to_df(pairings)
#'
#' \dontrun{
#' # Save to CSV
#' data_frame <- convert_to_df(pairings, file_path = "Pairs.csv")
#' }
#'
convert_to_df <- function(pairings,
                          file_path = NULL) {

  data_frame <- dplyr::bind_rows(pairings)

  # Add week labels in reverse so Week 1 is first row group
  data_frame$week <- rev(seq_len(nrow(data_frame)))

  df_pivot <- tidyr::pivot_longer(
    data = data_frame,
    cols = -week,
    names_to = "group",
    values_to = "pairing"
    )

  if(!is.null(file_path)) {
    write.csv(df_pivot, file_path, row.names = FALSE)
  }

  return(df_pivot)

}


#' Convert pairings tibble to list
#'
#' A helper function to convert a pairings tibble (created by `convert_to_df()`) back into a list format compatible with `create_pairs()`.
#' The tibble must contain the columns `week`, `group`, and `pairing`, as produced by `convert_to_df()`.
#'
#' @param df A tibble with columns `week`, `group`, and `pairing`: the output of `convert_to_df()`.
#'
#' @returns A list of named character vectors with class `pairings`, matching the structure expected by `create_pairs()`.
#'
#' @examples
#' # Create a static roster
#' ## Note this input needs to be a list for `create_pairs()`
#' roster <- LETTERS[1:10]
#'
#' # Generate pairs
#' pairings <- create_pairs(roster)
#'
#' # Convert to tibble
#' data_frame <- convert_to_df(pairings)
#'
#' # Convert back to list format
#' roster_list <- convert_to_list(data_frame)
#'
convert_to_list <- function(data_frame) {

  df_wide <- tidyr::pivot_wider(
    data = data_frame,
    names_from = group,
    values_from = pairing
  )

  # Drop the 'week' column as not used in `create_pairs()` list
  df_wide$week <- NULL

  # re-convert each row into a named character vector
  pairings_list <- lapply(seq_len(nrow(df_wide)), function(i) {
    row <- as.character(df_wide[i, ])
    names(row) <- names(df_wide)
    row
  })

  # re-assign class
  class(pairings_list) <- c("pairings", "list")

  return(pairings_list)

}





#' User generation of student pairs based on attendance
#'
#' A wrapper function that generates optimised student pairs from the list of present students
#' using the `create_pairs()` algorithm. This function minimises repeated pairings by incorporating
#' previous pairing history (if provided) and outputs results in tidy data frame format.
#'
#' The pairing history can be supplied as either:
#' - A list with class `pairings` (output of `create_pairs()` or `convert_to_list()`), or
#' - A data frame with columns `week`, `group`, and `pairing` (output of `convert_to_df()`).
#'
#' Optionally, the resulting pairings can be saved to a CSV file via `file_path`.
#'
#'
#' @param attendance A tibble with columns `name` and `present`, as returned by `take_attendance()`.
#' @param group_size Integer; Number of students per group. Default number is 2.
#' @param population Integer; Number of simulated "organisms" to sample. I.e. how long to spend searching for a better solution. Default is 1000.
#' @param pair_history Optional. Pairing history to minimise repeated pairings. Can be a `pairings` list or a data frame.
#' @param file_path Optional. A character string to save the result as a CSV file. If `NULL`, no file is saved.
#'
#' @returns A tibble with columns: `week`, `group`, and `pairing`.
#' @export
#'
#' @examples
#' # Simulate a class list
#' class_list <- data.frame(name = rownames(USJudgeRatings))
#'
#' # Take attendance for week 1 (students 1 to 8 are present)
#' attendance <- take_attendance(full_class = class_list,
#'                               present_students = 1:8,
#'                               session_id = "Week_1")
#'
#' # Generate pairings for Week 1
#' pairs_week1 <- student_pairs(attendance = attendance, group_size = 2)
#'
#' # Take attendance for Week 2
#' attendance2 <- take_attendance(full_class = class_list,
#'                                present_students = c(2, 3, 6, 7, 9, 10),
#'                                session_id = "Week_2")
#'
#' # Generate pairings using Week 1 as history
#' pairs_week2 <- student_pairs(attendance = attendance2,
#'                              pair_history = pairs_week1,
#'                              group_size = 2)
#'
#' \dontrun{
#' # Save to CSV file
#' student_pairs(attendance = attendance,
#'               file_path = "student_pairs.csv")
#' }
student_pairs <- function(attendance,
                          group_size = 2,
                          population = 1000,
                          pair_history = NULL,
                          file_path = NULL){

  # Handle pairing history if supplied as data frame - re-convert to list
  if(!is.null(pair_history)) {
    if(is.data.frame(pair_history)) {
      pair_history <- convert_to_list(pair_history)
      }
  }

  # Extract present students from attendance data
  class_list <- attendance$name[attendance$present]

  new_pairs <- create_pairs(pool = class_list,
                            group_size = group_size,
                            record = pair_history,
                            population = population)

  data_frame <- convert_to_df(new_pairs)

  # In some weeks, there may be fewer students than expected, resulting in underfilled groups.
  # This creates NA entries in the pairing column for groups that couldn't be formed (e.g., group 5 or 6 in a week with only enough students for 4 groups).
  # These rows are removed here to ensure only valid pairings are included.
  data_frame <- data_frame[!is.na(data_frame$pairing), ]

  # Save if file_path is provided
  save_output(data_frame, file_path, append = FALSE)

  return(data_frame)

}


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

