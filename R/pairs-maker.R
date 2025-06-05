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
#'               c('AAA-BBB','WWW-XXX'),
#'               c('YYY-ZZZ', 'CCC-DDD')
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
#' ## for 1 week
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
#' ## Convert to tibble and save as CSV
#' df <- Convert_to_df(pairings, "My_Pairs.csv")
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

