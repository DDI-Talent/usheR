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
    pairs_strings = sapply(pairs, sort_and_combine)
  list(
    pairs = pairs_strings,
    dup_count = sum(pairs_strings %in% unlist(record))
  )
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

    this_gen <- fitness_function(pairs, record)

    if (how_many_dups <= winning_dups) {
      winning_dups  <- this_gen$dup_count
      winning_pairs <- this_gen$pairs
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
#'
#' @returns A tibble with the columns : `week` (Integer), `group` (Character), and `pairing` (Character).
#'
#' @examples
#' # Create a class list
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
convert_to_df <- function(pairs_list) {

  purrr::map_df(pairs_list, dplyr::tibble, .id = 'week')
}


#' Convert pairings tibble to list
#'
#' A helper function to convert a pairings tibble (created by `convert_to_df()`) back into a list format compatible with `create_pairs()`.
#' The tibble must contain the columns `week`, `group`, and `pairing`, as produced by `convert_to_df()`.
#'
#' @param data_frame A tibble with columns `week`, `group`, and `pairing`: the output of `convert_to_df()`.
#'
#' @returns A list of named character vectors with class `pairings`, matching the structure expected by `create_pairs()`.
#'
#' @examples
#' # Create a class list
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
convert_to_list <- function(pairs_df) {
  structure(split(pairs_df$name, pairs_df$week), class = c('pairings', 'list'))
}





#' Generate optimised student pairs based on latest attendance
#'
#' This wrapper function uses `create_pairs()` to generate optimised student pairs or groups,
#' based on the latest attendance. It automatically attempts to minimise repeat pairings
#' by reading pairing history from `file_path` if the file exists.
#'
#' The function expects a wide-format attendance tibble, as produced by `take_attendance()`,
#' where column 1 is `name`, and column 2 is the most recent session (with logical TRUE/FALSE values for attendance).
#'
#' If a file path is provided and the file exists, it is read and used as pairing history
#' to guide group allocation (via `convert_to_list()`).
#' The resulting pairs are returned as a tidy tibble and can also be saved to the same file path.
#'
#' @param attendance A wide-format tibble returned by `take_attendance()`, with columns `name`, `WeekX`, etc.
#' @param group_size Integer. Number of students per group. Default is 2.
#' @param population Integer. Number of pairing attempts (used to minimise repeats). Default is 1000.
#' @param file_path Optional. Path to a CSV file to load past pairings *and* save new ones.
#' @param seed Optional. Random seed for reproducibility.
#'
#' @return A tibble with columns: `week` (Integer), `group` (Character), and `pairing` (Character).
#' Each row represents one group.
#'
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
                          file_path = NULL,
                          seed = NULL){

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Load pair history automatically from file_path (if exists)
  pair_history <- NULL
  if (!is.null(file_path) && file.exists(file_path)) {
    pair_history_df <- readr::read_csv(file_path, show_col_types = FALSE)
    pair_history <- convert_to_list(pair_history_df)
  }

  # Identify the latest session column
  latest_col <- names(attendance)[2]
  present_logical <- attendance[[latest_col]]
  # Extract present students from attendance data
  class_list <- attendance$name[present_logical]

  new_pairs <- create_pairs(pool = class_list,
                            group_size = group_size,
                            record = pair_history,
                            population = population)


  pairs_df <- convert_to_df(new_pairs)
  # In some weeks, there may be fewer students than expected, resulting in underfilled groups.
  # This creates NA entries in the pairing column for groups that couldn't be formed (e.g., group 5 or 6 in a week with only enough students for 4 groups).
  # These rows are removed here to ensure only valid pairings are included.
  # -- Not sure when this actually happens? --
  # -- Will handle here with a warning till I know otherwise for sure --
  if (anyNA(pairs_df$pairing)) {
    warning('NAs found in pairing colums. Removed in `student_pairs`\n')
    pairs_df[!is.na(pairs_df$pairing), ]
  }
  if (!is.null(file_path)) save_output(pairs_df, file_path)

  pairs_df
}

