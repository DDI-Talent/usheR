#' Pull changes from Github
#'
#' For use in Noteable. Students can get the most recent notes into their
#' noteable environment when we release them. Files that have been changed
#' locally, that may cause a conflict, are backed up with a timestamp added to
#' their filename, e.g. `README.md` becomes
#' `README_local-backup_day-xxxxxx_time-zzzzzz.md`
#'
#' @return Invisibly returns names of any files added or changed on the Git
#'   origin
#' @export
#'
update_from_github <- function() {
  modified_filenames <- check_git_for_modified_files()

  if (!is.null(modified_filenames[['M']])) {
    cat('Backing up local changes...\n')
    backup_file_names <- create_dated_backup(modified_filenames[['M']])
  }

  if (any(lengths(modified_filenames[c('A', 'M')]))) {
    bring_files_from_github(unlist(modified_filenames[c('A', 'M')]))

    cat('Completing merge...\n\n')
    git_cleanup(backup_file_names)
  }

  if (length(modified_filenames[['M']]))
    cat('Local files affected', modified_filenames[['M']], sep = '\n  ')

  if (length(modified_filenames[['A']]))
    cat('New files added', modified_filenames[['A']], sep = '\n  ')

  invisible(modified_filenames)
}



call_git <- function(..., stdout = '') {
  system2('git', c(...), stdout = stdout)
}


check_git_for_modified_files <- function() {
  call_git('fetch origin')
  files_affected <- call_git('diff --name-status main origin/main', stdout = TRUE)

  if (!is.null(files_affected)) {
    files_affected <- files_affected |>
      split(substr(files_affected, 1, 1)) |>
      lapply(substring, 3)
  }

  files_affected
}



bring_files_from_github <- function(filenames) {
  quoted_filenames <- shQuote(unlist(filenames))
  call_git('checkout origin/main -- ', quoted_filenames)
  call_git('add', quoted_filenames)
  commit_message <- paste0('"Merging updated files\n  ',
                          paste0(filenames, collapse = '\n  '), '"')
  call_git('commit -m', commit_message, stdout = NULL)
}


create_dated_backup <- function(filenames) {
  time_now <- format(Sys.time(), 'day-%y%m%d_time-%H%M%S')
  replace_this <- '^(.*)(\\.{1}[^.]*)$'
  with_this <- paste0('\\1_local-backup_', time_now, '\\2')
  new_filenames <- sub(replace_this, with_this, filenames)
  file.rename(filenames, new_filenames)

  rename_message <- paste('   ', filenames,
                          '-->', new_filenames,
                          collapse = '\n')

  commit_message <- paste0('"Duplicated\n', rename_message, '"',
                           sep = '', collapse = '')

  call_git('add', paste(new_filenames, collapse = ' '))
  call_git('commit', paste('-m', commit_message), stdout = NULL)
  new_filenames
}



git_cleanup <- function(filenames) {
  call_git('commit -m "Finish a merge"', stdout = NULL)
  # call_git('status')
}
