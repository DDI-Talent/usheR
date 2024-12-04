# usheR

2024-10-12

<!-- Do not manually edit `README.md` -->

<!-- Edit the `README.qmd`, then render. -->

<!-- Also commit and push changes to this and the resulting README.md file. -->

<!-- This makes is easy to incorporate code examples later if we choose. -->

Some helper functions for teaching.

When you add any functions to the package, also add your name to the
`DESCRIPTION` file. You can also include a comment with your ORCID
number if you like, in the `comment` field. To add your name use the
following template and paste it into the `Authors@R:` field, inside the
`c()`.

    person(
      given   = "Given name",
      family  = "Family name",
      email   = "email@example.com", role = "aut",
      comment = c(ORCID = "Optional - YOUR-ORCID-ID")
    )

Then, commit and push.

**Extra notes:**

- Please do not use `library()` **ANYWHERE** in the package.
  - in particular don’t do `library(tidyverse)`
