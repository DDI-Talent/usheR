# INSTRUCTIONS: to get most recent version of the classnotes, run this whole file
# you can use ctrl+alt+enter (on PC) of cmd+alt+enter (on Mac),
# or use R studio menu: Code > Run Region > Run all

library(magrittr)
# TODO: could this be tidyverse? where do pipes come from

# what's new on local?
system("git status")

# check what's new on the server?
system("git fetch origin")

# which files are affected?
filesAffected <- system("git diff --name-status main origin/main", intern = TRUE)
print("filesAffected")
print(filesAffected)


# separate those files into newly added, and modified:
beginingOfChanged <- '^M\t'
beginingOfNew <- '^A\t'

filesAdded <- filesAffected %>%
  grep(beginingOfNew, ., value = TRUE) %>%
  sub(beginingOfNew, "", .)

print("filesAdded")
print(filesAdded)

filesChanged <- filesAffected %>%
  grep(beginingOfChanged, ., value = TRUE) %>%
  sub(beginingOfChanged, "", .)

print("filesChanged")
print(filesChanged)

# for newly added, just bring them over. For modified, create a backup, then bring them over.

# some helper functions:

# bring new file from git, then add and commit bringing it.
bringFileFromGithub <- function(filename) {
  system(paste0("git checkout origin/main -- ",filename))
  system(paste0("git add ",filename))
  system(paste0("git commit -m  'merging",filename,"'"))
}

# create local copy of a file, with current time at the back of a filename
createABackupCopyOfFileWithDateInName <- function(filename) {
  timeNow <- format(Sys.time(), "day%y%m%d-time%H%M%S")
  replaceThis <- paste0("^(.*)(\\.{1}[^.]*)$")
  withThis <- paste0("\\1-local-backup-",timeNow,"\\2")
  newFilename <- sub(replaceThis, withThis, filename)
  system(paste0("mv ",filename," ",newFilename))
  system(paste0("git add ",newFilename))
  system(paste0("git commit -m  'duplicated ",filename," as ",newFilename,"'"))
}

# create backup on modified ones
for (fileName in filesChanged) {
  createABackupCopyOfFileWithDateInName(fileName)
}

# bring over newest modified and added files
for (fileName in append(filesChanged,filesAdded)) {
  bringFileFromGithub(fileName)
}

# cleanup, so we see less git complaints
system("git commit -m 'finish a merge'")

# check if everything went well
system("git status")
