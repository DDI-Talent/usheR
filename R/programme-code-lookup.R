#' Programme code lookup table
#'
#' A table for mapping programme code to programme descriptions. Also contains
#' columns splitting the description into its  component parts.
#'
#' @format ## programme_code_lookup
#'
#' A data frame, 7 rows by 7 columns.
#'
#' __Variable__          | __Type__  | __N unique__ | __Example__
#' ----------------------|-----------|-------------:|-------------------
#' programme_code        | character |            7 | PTMSCDSHSC1P
#' programme_description | character |            7 | Data Science for Health and Social Care....
#' programme_name        | character |            2 | Data Science for Health and Social Care
#' is_online             | logical   |            1 | `TRUE`, `FALSE`
#' qualification         | character |            7 | MSc, PgDip, PgCert, etc.
#' length_years          | integer   |            5 | 1, 2, 3, 4, 6
#' is_part_time          | logical   |            2 | `TRUE`, `FALSE`
#'
'programme_code_lookup'
