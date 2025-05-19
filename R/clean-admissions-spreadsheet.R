#' Flter admissions xlsx spreadsheat
#'
#' Reads the spreadsheet at the provided path. The spreadsheet should be stored
#' in your personal OneDrive space. The column names are changed to be
#' syntactic. The data are filtered to include only applications to "Data
#' Science for Health and Social Care" programmes (see
#' [](programme_code_lookup).) and only those with a `Qualification Status
#' Decision` equal to "`(SB) Ready for Decision`".
#'
#' No columns are removed, so the function should always work as intended, as
#' long as the `Programme Code` column is correct, and always called `Programme
#' Code`.
#'
#' @param spreadsheet_path Character; Path to admissions spreadsheet
#'
#' @returns Data frame filtered for DSHSC applications. programme applications.
#' @export
#'
#' @examples
#'
#' filter_admissions('~/../OneDrive - University of Edinburgh/.../PG00x PG applicant communications report-online 01-Jan-00.xlsx')
#'
filter_admissions <- function(new_admissions_spreadsheet,
                              reviewed_spreadsheet = NULL) {

  if (!is.null(reviewed_spreadsheet)) {
    already_reviewed <-  read_admissions_spreadsheet(reviewed_spreadsheet)
    already_actioned <- filter_actioned(already_reviewed)

    ready_for_decision <- dplyr::anti_join(already_reviewed, already_actioned, by = dplyr::join_by(application_id))
  }

  admissions_data <- read_admissions_spreadsheet(new_admissions_spreadsheet)
  ready_for_decision <- filter_new(admissions_data)
  if (!hasName(ready_for_decision, 'team_action')) {
    ready_for_decision$team_action <- NA_character_
  }

  ready_for_decision
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


filter_actioned <- function(d) {
  dplyr::filter(d, !is.na(team_action))
}


#' Summary of admissions data
#'
#' Count the number of decision statuses per qualification.
#'
#' @export
#' @keywords internal
#'
summary.admissions <- function(object, ...) {
  object |>
    dplyr::mutate(status = sub('.+? ', '', qualification_status_description),
                  qualification = sub(' .+', '', programme_description)) |>
    dplyr::summarise(done  = sum(!is.na(team_action)),
                     to_do = sum(is.na(team_action)),
                     .by = c(qualification, status)) |>
    print(all_cols = TRUE)
}

#' Print method for admissions object
#'
#' By default shows only a selected minimum of columns. To see all columns use
#' `all_cols = TRUE`. Column `team_action` is placed at the front of hte data frame for easy inspection, but is actually the final column of the admissions data set.
#'
#' @param x Data frame of class `admissions`
#' @param all_cols Logical
#' @param ... Ignored
#'
#' @export
#' @keywords internal
#'
print.admissions <- function(x, all_cols = FALSE, ...) {
  if (!all_cols) {
    x <- x |>
      dplyr::select(team_action, programme_code, uun, application_id, forename, surname, qualification, status) |>
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
#'     - A decision was made, and is recorded with reviewer's initials and date stamp.
#' - `#`: Skip
#'     - No decision was made. Can be returned to later.
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
step_through_admissions <- function(admissions_data, reviewer) {
  if (!nrow(admissions_data)) {
    cat('None left 🥳')
    return(invisible())
  }

  stopifnot('Argument `reviewer` missing\nUse `step_through_admissions(admissions, "Your initials")`' = !missing(reviewer))

  if (!hasName(admissions_data, 'team_action')) admissions_data$team_action <- NA_character_

  na_team_action_idx <- which(is.na(admissions_data$team_action))
  n <- length(na_team_action_idx)
  i <- 1
  tryCatch({
    while (i <= n) {
      row_i <- na_team_action_idx[i]
      action <- display_next(admissions_data[row_i, ], i, n)
      switch(action,
             'x' = i <- Inf,
             'q' = i <- Inf,
             '#' = i <- i + 1,
             'u' = {
               i <- i - 1
               last_row <- na_team_action_idx[i]
               admissions_data$team_action[[last_row]] <- NA_character_
             },
             {
               admissions_data$team_action[[row_i]] <- paste(toupper(reviewer), Sys.Date())
               i <- i + 1
             }
      )

      cat('\f')
    }
  },
  interrupt = \(e) {
    warning('Interrupted.\nYour progress so far has been saved.\n')
    return(admissions_data)
  },
  finally = progress_summary(admissions_data)
  )

  admissions_data
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
  cat(sprintf('\033[%im (%s) \033[0m\n\033[1m%s\033[0m\n', clrs[p], p, u))

  # could present a menu here?
  readline(paste('Action? '))
}

#' Called for side effect. Returns NULL.
#'
progress_summary <- function(d) {
  actioned  <- sum(!is.na(d$team_action))
  remaining <- sum(is.na(d$team_action))
  cat(sprintf('Done  : %03s\nTo do : %03s\n',
              actioned, remaining))
  if (!remaining) cat('🎉 🥳 💃 😁')
}
