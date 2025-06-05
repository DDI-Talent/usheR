# usheR

2024-10-12

<!-- Do not manually edit `README.md` -->

<!-- Edit the `README.qmd`, then render. -->

<!-- Also commit and push changes to this and the resulting README.md file. -->

<!-- This makes is easy to incorporate code examples later if we choose. -->

Some helper functions for teaching.

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
