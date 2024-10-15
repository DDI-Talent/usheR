# problem statement:
# - we want students to be able to get most recent notes into their noteable whenever we release them
# - repo with notes is a public one, and students do not have editing rights (can;t commit and push)
# - in python noteable there is a [+GitRepo] button which (to my understanding) does some magic described below
# - in r we can use terminal, or the github panel, but that requires the ability to log into github, which we do not require from students, and it is possibly impossible/harder any more via username and password?

# What does python's [+GitRepo] bython do to my understanding:
# - without any need to commit/stash it will bring the new files into my cloned repo
# - but it does NOT overwrite/remove work in files I have done so far
# - so with that button student: gets newly added files from github, without losing files they olready edited
# - in a rare occasion of merge conflict where both sides edited the same file: student locally (on noteable) and teacher on public github repo.... it will leave student's copy as it was, and bring the new teacher's copy into a file with an appended date like Badge03_28032024.ipynb

# So the quest(ion) is: can we build a similar github flow in terminal, and then have a R script which runs those terminal scropts. So students would have an R script called RunThisToGetRecentNotes.R - contstrains: students does not need to login to github, teacher repo is public.

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
  call_git('checkout origin/main -- ', filenames)
  call_git('add', filenames)
  commit_message <- paste('"Merging',
                          paste(filenames, collapse = '\n  '),
                          '"',
                          sep = '\n  ')
  call_git('commit -m', commit_message, stdout = NULL)
  invisible()
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
  invisible()
}



#' only function to export
#' update_from_github()
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
      cat('New files added', modified_filenames[['A']] %||% 'None', sep = '\n  ')

    invisible()
}



