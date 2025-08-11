#' Filter admissions xlsx spreadsheat
#'
#' Reads the spreadsheet at the provided path. The spreadsheet should be stored
#' in your personal OneDrive space. The column names are changed to be
#' syntactic. The data are filtered to include only applications to "Data
#' Science for Health and Social Care" programmes (see
#' [](programme_code_lookup).) and only those with a `Qualification Status
#' Decision` equal to "`(SB) Ready for Decision`".
#'
#' No columns are removed, so the function should always work as intended, as
#' long as the `Programme Code` column is correct, and is always called `Programme
#' Code`.
#'
#' @param new_admissions_spreadsheet Character; Path to admissions spreadsheet. Assumed to be the most recent spreadsheet
#' @param reviewed_spreadsheet Character; Path to previous will filter any that have had a note added already.
#' @param decision_col Character; Name of column used for recording decisions. Can be a vector of length two if each spreadsheet has decision col with a different name. Current default is `team_action`
#'
#' @returns Data frame filtered for DSHSC applications. programme applications.
#' @export
#'
#' @examples
#'
#' filter_admissions('~/../OneDrive - University of Edinburgh/.../PG00x PG applicant communications report-online 01-Jan-00.xlsx')
#'
filter_admissions <- function(new_admissions_spreadsheet,
                              reviewed_spreadsheet = NULL,
                              decision_col = 'team_action') {

  decision_col <- tolower(decision_col)

  if (!is.null(reviewed_spreadsheet)) {
    already_reviewed <-  read_admissions_spreadsheet(reviewed_spreadsheet)
    # already_actioned <- filter_actioned(already_reviewed)
    already_actioned <- filter_actioned(already_reviewed, decision_col = decision_col[length(decision_col)])

    ready_for_decision <- dplyr::anti_join(already_reviewed, already_actioned, by = dplyr::join_by(application_id))
  }

  admissions_data <- read_admissions_spreadsheet(new_admissions_spreadsheet)
  ready_for_decision <- filter_new(admissions_data)
  if (!hasName(ready_for_decision, decision_col[[1]])) {
    ready_for_decision[[decision_col[[1]]]] <- NA_character_
  }

  print(ready_for_decision, decision_col = decision_col[[1]])
}


read_admissions_spreadsheet <- function(path) {
  if (!file.exists(path)) {
    stop(paste0('File `', basename(path), '` not found at:\n  `', dirname(path), '`\nCheck your file path is correct'), call. = FALSE)
  }

  switch(tools::file_ext(path),
         csv = d <-readr::read_csv(path, show_col_types = FALSE),
         xlsx = d <- readxl::read_xlsx(path, .name_repair = 'unique_quiet'),
         stop('Only CSV or XLSX files are accepted'))

  names(d) <- gsub('\\W+', '_', tolower(names(d)))

  structure(d, class = c('admissions', class(d)))
}


filter_new <- function(d) {
  d |>
    dplyr::filter(qualification_status_description == '(SB) Ready for Decision') |>
    dplyr::semi_join(programme_code_lookup,
                     by = dplyr::join_by(programme_code)) |>
    dplyr::filter(tolower(status) == 'in progress')

}


filter_actioned <- function(d, decision_col = 'programme_decision') {
  d[!is.na(d[[decision_col]]), ]
}


#' Summary of admissions data
#'
#' Count the number of decision statuses per qualification.
#'
#' @export
#' @keywords internal
#'
summary.admissions <- function(object, decision_col = notes, ...) {
  object |>
    dplyr::mutate(status = sub('.+? ', '', qualification_status_description),
                  qualification = sub(' .+', '', programme_description)) |>
    dplyr::summarise(done  = sum(!is.na({{decision_col}})),
                     to_do = sum(is.na({{decision_col}})),
                     .by = c(qualification, status)) |>
    print(all_cols = TRUE)
}

#' Print method for admissions object
#'
#' By default shows only a selected minimum of columns. To see all columns use
#' `all_cols = TRUE`. Column `decision_col` is placed at the front of the data frame for easy inspection, but is actually the final column of the admissions data set.
#'
#' @param x Data frame of class `admissions`
#' @param all_cols Logical
#' @param ... Ignored
#'
#' @export
#' @keywords internal
#'
print.admissions <- function(x, decision_col = team_action, all_cols = FALSE, ...) {
  if (!all_cols) {
    x <- x |>
      dplyr::select({{decision_col}}, programme_code, uun, application_id, forename, surname, qualification, status) |>
      dplyr::arrange(desc(programme_code))
  }
  NextMethod(x)
}


#' Step through admissions one-by-one
#'
#' Steps through an admissions spreadsheet one row at a time. The relevant
#' information for searching EUCLID is shown in the console, with the prompt
#' `Action?`. The following actions can be taken:
#' - `Enter`: The default action
#'     - A decision was made, and is recorded with reviewer's initials and date stamp, and no further note.
#' - `a`: Accept
#'     - Can add arbitrary note `a A note on acceptance decision`
#' - `r`: Reject
#'     - Can add arbitrary note `a A note on rejection decision`
#' - `n`: add an arbitrary note
#'     - Useful if no decision was made
#'     - E.g. `n A note about why no decision was made`
#' - `#`: Skip
#'     - No decision was made; no comment added; note column remains `NA`.
#' - `u`: Undo
#'     - Rewinds to the previous application.
#' - `x`: Exit
#'     - Stops the review and returns an updated dataframe.
#' - `q`: Quit
#'     - Same as `x`
#'
#' @param d Admissions dataframe.
#' @param reviewer Character; Reviewer initials. Must be provided
#'
#' @returns Admissions dataframe with `team_action` column added.
#' @export
#'
#' @examples
#'   step_through_admissions(admissions_data, 'jw')
#'
step_through_admissions <- function(admissions_data, reviewer, decision_col = 'team_action') {
  if (!nrow(admissions_data)) {
    cat('None left 🥳')
    return(invisible())
  }

  stopifnot('Argument `reviewer` missing\nUse `step_through_admissions(admissions, "Your initials")`' = !missing(reviewer))

  if (!hasName(admissions_data, decision_col)) admissions_data[[decision_col]] <- NA_character_

  to_decide_idx <- which(is.na(admissions_data[[decision_col]]))
  n <- length(to_decide_idx)
  i <- 1

  decision_str <- function(decision = NA) {
    if (!is.na(decision))
      admissions_data[[decision_col]][[row_i]] <<- paste(toupper(reviewer), Sys.Date(), decision)
    i <<- i + 1
  }

  tryCatch({
    while (i <= n) {
      row_i <- to_decide_idx[i]
      action <- display_next(admissions_data[row_i, ], i, n)
      switch(substring(action, 1, 1),
             decision_str('Accept'), # default
             'a' = decision_str(paste('Accept:', substring(action, 3))),
             'r' = decision_str(paste('Reject:', substring(action, 3))),
             'n' = decision_str(paste('Note:', substring(action, 3))), # note added
             '#' = decision_str(), # skipped (change to 's'?)
             'x' = i <- Inf,
             'q' = i <- Inf,
             'u' = {
               i <- i - 1
               last_row <- to_decide_idx[i]
               admissions_data[[decision_col]][[last_row]] <- NA_character_
             }
      )

      cat('\f')
    }
  },
  interrupt = \(e) {
    warning('Interrupted.\nYour progress so far has been saved.\n')
    return(admissions_data)
  },
  finally = progress_summary(admissions_data, decision_col)
  )

  print(admissions_data, decision_col = decision_col[[1]])
}


#' Display next admissions data
#'
#' Handles user interaction. Returns user response.
#'
#' @param d Admissions data frame of one row
#' @param i Integer; Current row number
#' @param n Integer; Total number of rows
#
display_next <- function(d, i, n) {
  codes <- sort(programme_code_lookup$programme_code)
  clrs <- setNames(40 + seq_along(codes), codes)
  cat('\f', i, 'of', n, '\n')
  cat(sprintf('\033[2m%s\033[0m\n', paste(d$forename, d$surname)))
  u <- d$uun
  p <- d$programme_code
  cat(sprintf('\033[30;1;%im (%s) \033[0m\n\033[1m%s\033[0m\n', clrs[p], p, u))

  # could present a menu here?
  readline(paste('Action? '))
}

#' Called for side effect. Returns NULL.
#'
progress_summary <- function(d, decision_col) {
  actioned  <- sum(!is.na(d[[decision_col]]))
  remaining <- sum(is.na(d[[decision_col]]))
  cat(sprintf('Done  : %03s\nTo do : %03s\n',
              actioned, remaining))
  if (!remaining) cat('🎉 🥳 💃 😁\n\n')
}
