# usheR

2024-10-12

<!-- Do not manually edit `README.md` -->
<!-- Edit the `README.qmd`, then render. -->
<!-- Also commit and push changes to this and the resulting README.md file. -->
<!-- This makes is easy to incorporate code examples later if we choose. -->

# Some helper functions for teaching.

## Instructions for Package Users: Pair Maker

## Instructions for Package Users: Easier Github Pull-Update Instructions:

### Introduction - Simplified Classnotes Updater (no-drama github pull-and-merge):

This part of the package allows students to ‘update/pull’ the most
recent notes from the teacher’s public course git repository without
using git commands in terminal or RStudio. This is especially useful on
the beginner courses, and in the first weeks where you want to start
using R code quickly, prior to teaching students about git.

### How To use it, As a Teacher:

As a teacher you will include a special file in your classnotes. When
students run that file, they will automatically get most recent
classnotes form github.

1.  This assumes you have all your class notes for students in a github
    repo, which includes the RStudio project
2.  Create the student-facing file (instructions notes below, call it
    `RUN_TO_GET_RECENT_NOTES.Rmd`) from this package in the top folder
    of your teaching-notes repo.
3.  That file installs the UsheR package and runs a function from it.
    That function performs a git commit-merge-pull flow behind the
    scenes (so that students do not need to). Any conflicts are dealt
    with gracefully (by keeping both student and teacher copy, with a
    timestamp).

### How To use it, As a Student:

Students interact with this package only by running a file provided by
their teacher. They will find the file amongst their classnotes in
RStudio.

1.  Once, at the beginning of the course, students will need to clone
    your teaching repo using RStudio menu interface (using
    `File > New Project > Version Control > Git` and use your Repository
    Url).
2.  Whenever students want to have the most recent version of the class
    notes (e.g. every week) they need to open RStudio and run the code
    block in file `RUN_TO_GET_RECENT_NOTES.Rmd`.
3.  This should work both on local RStudio, PositCloud and Noteable.

### How to create that special student facing file:

Copy-paste below code into a file and save it at the top level of your
public repo with notes. Call the file something like
`RUN_TO_GET_RECENT_NOTES.Rmd`.

Whenever students run that file:

- new files from github will be added (pulled) into their project
  directory
- without overwriting their previous notes
- without the need for student to log into github (assuming teacher repo
  is public)

Put ALL of below text in a file `RUN_TO_GET_RECENT_NOTES.Rmd`:

------------------------------------------------------------------------

<!-- start of file RUN_TO_GET_RECENT_NOTES.Rmd-->

Run below code block to get the most recent notes from github.

To run a grey code block below, click anywhere inside it and then click
the green play button in top left corner and click.

```` markdown
```{r echo = T, results = 'hide'}
install.packages("devtools")
devtools::install_github('ddi-talent/usheR')

library(usheR)
update_from_github()
```
````

Once you run it, your R studio will install some packages and bring the
most recent course notes from github to your R project.

------------------------------------------------------------------------

<!-- end of file RUN_TO_GET_RECENT_NOTES.Rmd-->

## For Package Maintainers:

When you add any functions to the package, make your changes in a
separate Git branch, and submit a pull request (PR). As part of your
changes, you should also add your name to the `DESCRIPTION` file (see
below). You can also include a comment with your ORCID number if you
like, in the `comment` field. To add your name use the following
template and paste it into the `Authors@R:` field.

    person(
      given   = "Given name",
      family  = "Family name",
      email   = "email@example.com",
      role = "aut",
      comment = c(ORCID = "Optional - YOUR-ORCID-ID")
    )

For example, inside the `c()` following `Authors@R:`

    Authors@R: c(
        person("John", "Wilson",
        email = "43386915+jmcvw@users.noreply.github.com", role = c("aut", "cre"))
        )

Then, commit and push, and submit your PR. If PRs are new to you, maybe
we can cover this in a future week?

**Extra notes:**

- Please do not use `library()` **ANYWHERE** in the package.
  - in particular don’t do `library(tidyverse)`
  - (For good reasons, not any kind of anti-tidyverse sentiment)
