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


#' Convert pairings list to a tibble
#'
#' Converts the output of `create_pairs()` into a tibble with the columns `week` (Integer), `group` (Character), and `pairing` (Character). Optionally saves the tibble as a CSV file if `file_path` is specified.
#'
#' @param pairings A pairings list object returned from `create_pairs()`.
#' @param file_path Optional. A character string specifying a file path to save the output as a CSV file. IF NULL (default), no file is saved.
#'
#' @returns A tibble with the columns `week`, `group`, and `pairing`.
#' @export
#'
#' @examples
#' ## Create a static roster
#' roster <- LETTERS[1:10]
#' ## Generate pairs
#' pairings <- create_pairs(roster)
#' ## Convert to tibble
#' df <- convert_to_df(pairings)
#'
#' \dontrun{
#' # You can provide a file name if you wish to save the data frame as a csv file.
#' df <- convert_to_df(pairings, file_path = "Pairs.csv")
#' }
#'
convert_to_df <- function(pairings, file_path = NULL) {

  df <- dplyr::bind_rows(pairings)

  # add week
  df$week <- rev(seq_len(nrow(df)))

  # Pivot into long format
  df_pivot <- tidyr::pivot_longer(
    data = df,
    cols = -week,
    names_to = "group",
    values_to = "pairing"
    )

  if(!is.null(file_path)) {
    write.csv(df_pivot, file_path, row.names = FALSE)
  }

  return(df_pivot)

}


#' Convert pairings tibble to a list
#'
#' Convert pairings tibble created by `convert _to_df()` back into a list of named character vectors for use in `create_pairs()`.
#'
#' @param df a tibble with columns `week`, `group`, and `pairing`.
#'
#' @returns A list of named character vectors with class `pairings`, matching the necessary format of `create_pairs()`.
#' @export
#'
#' @examples
#' ## Create a static roster
#' roster <- LETTERS[1:10]
#' ## Generate pairs
#' pairings <- create_pairs(roster)
#' ## Convert to tibble
#' df <- convert_to_df(pairings)
#' ## Re-convert back to list
#' list <- convert_to_list(df)
#'
#' \dontrun{
#' # You can provide a file name if you wish to save the data frame as a csv file.
#' df <- convert_to_df(pairings, file_path = "Pairs.csv")
#' }
#'
#'
convert_to_list <- function(df) {

  # convert to wide format
  df_wide <- tidyr::pivot_wider(
    data = df,
    names_from = group,
    values_from = pairing
  )

  # get rid of week column
  df_wide$week <- NULL

  # re-create as list
  pairings_list <- lapply(seq_len(nrow(df_wide)), function(i) {
    row <- as.character(df_wide[i, ])
    names(row) <- names(df_wide)
    row
  })

  # re-assign class
  class(pairings_list) <- c("pairings", "list")

  return(pairings_list)

}





#' User generation of student pairs
#'
#' A wrapper function that takes a list of students and returns optimally paired groups
#' while minimising duplicate pairings from previous sessions.
#'
#' This function automatically handles conversion of pairing history from a data frame (if needed; from `convert_to_list()`), generates new
#' pairings using a simulation algorithm (from `create_pairs()`), and outputs the results as a data frame (from `convert_to_df()`).
#' Optionally saves the results as a CSV file.
#'
#' @param attendance A tibble with columns `name` and `present`, as returned by `take_attendance()`.
#' @param group_size Integer; Number of students per group. Default number is 2.
#' @param population Integer; Number of simulated "organisms" to sample. I.e. how long to spend searching for a better solution. Default is 1000.
#' @param pair_history Optional Record. A pairing history either as a `pairings` list (from `create_pairs()`) or as a data frame (from `convert_to_df()`). If NULL, starts fresh.
#' @param file_path Optional character string. If supplied, the final output will be saved as a CSV file to this path.
#'
#' @returns A data frame (tibble) with the columns: `week`, `group`, and `pairing`.
#' @export
#'
#' @examples
#' # create class roster
#' roster <- LETTERS[1:10]
#' # get who is present
#' present <- 1:8
#' # take attendance
#' attendance <- take_attendance(full_class = roster, present_students = present)
#' # Simulate pairings for week 1
#' week1 <- student_pairs(attendance)
#' # Simulate pairings for week 2, using week 1 as pairs history
#' week2 <- student_pairs(attendance, pair_history = week1)
#'
#' \dontrun{
#' # You can provide a file name if you wish to save the data frame as a csv file.
#' firstWeek <- student_pairs(class_list = roster, group_size = 2, file_path = "Pairs.csv")
#' }
<<<<<<< Updated upstream

student_pairs <- function(class_list, group_size = 2, population = 1000,
=======
student_pairs <- function(attendance,
                          group_size = 2,
                          population = 1000,
>>>>>>> Stashed changes
                          pair_history = NULL,
                          file_path = NULL){

  # convert to list if dataframe
  if(!is.null(pair_history)) {
    if(is.data.frame(pair_history)) {
      pair_history <- convert_to_list(pair_history)
      }
  }

  # generate new pairings
  new_pairs <- create_pairs(pool = class_list,
                            group_size = group_size,
                            record = pair_history,
                            population = population)

  # convert to data frame
  dataframe <- convert_to_df(new_pairs)

  # Remove NA rows (e.g., groups in susequent weeks that cannot be populated)
  dataframe <- dataframe[!is.na(dataframe$pairing), ]

  # save as csv if needed
  save_output(dataframe, file_path, append = FALSE)

  return(dataframe)

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
<<<<<<< Updated upstream
#' @returns A character vector of students who are present.
#' @export
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

=======
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
>>>>>>> Stashed changes
