#' Create HTML table
#'
#' Creates a complete HTML table skeleton, including the header, body, and
#' column groups. The table is styled using either the default, or user
#' provided, CSS. By default, cells are populated with a default
#' `__PLACEHOLDER__` value. The intention is to provide a template for HTML
#' table that can be easily edited manually, copied, and pasted into a Learn
#' document. There is some minimal ability to customize the table style (see
#' [Adding css rules](#adding-css-rules)). The output is written to an HTML
#' file, which opens for manual editing. Any default `__PLACEHOLDER__` values
#' should be replaced manually. This final table can then be copied and pasted
#' into a Learn page.
#'
#' While there are several packages that can create HTML tables, the HTML output
#' is often complex and difficult to customize. This function aims to provide a
#' simple and straightforward way to create minimal HTML tables, allowing for
#' easy (but minimal) customization.
#'
#' ## Adding css rules
#'
#' New style rules can be added by providing a named vector of CSS declarations.
#' The names are the "selectors", and the values should follow the usual CSS
#' declaration syntax of `property: value`. Selector may include non-syntactic
#' characters such as spaces or dashes, in which case they must be enclosed in
#' backticks or quotes. For example, `selector with spaces` or `.a-class tr >
#' td`, as shown in the examples.
#'
#' Declarations added to existing selectors are appended to the existing
#' declarations for those selectors. Original declarations are not removed.
#' Rules for new selectors are appended to the end of the rules set.
#'
#' @param d Character; Vector of column names, or a data frame.
#' @param n_rows Integer; Number of rows, defaults to 1
#' @param widths Numeric; Vector of column widths
#' @param table_caption Character; Table caption (optional)
#' @param style Character; Named vector of CSS styles where the names are the
#'   selectors and the values are the CSS properties.
#' @param file Character; Name of the file to write the HTML table to. Defaults
#'   to `temp-table.html`
#'
#' @returns If run in an interactive R session, the HTML file generated is
#'   opened for manual editing. Otherwise, returns the path to the created HTML
#'   file.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Create a blank HTML table skeleton
#' # with column names and specicified number of rows
#' do_html_table(c('col_1', 'col_2', 'col_3'), n_rows = 2)
#'
#' # Create HTML table filled with values from a data frame
#' do_html_table(BOD)
#'
#' do_html_table(c('Criteria', 'Description', 'Weight'),
#'               n_rows = 2)
#'
#' do_html_table(c('Criteria', 'Description', 'Weight'),
#'               n_rows = 2,
#'               table_caption = 'This is my caption')
#'
#' # Update the table style
#' new_css <- c(element = 'property: value;',
#'              .row_highlight = 'background-color: #000000;',
#'              `.a-class tr > td` = 'property: value;')
#'
#' do_html_table(c('Criteria', 'Description', 'Weight'),
#'               nrows = 2,
#'               style = new_css)
#' }
#'
do_html_table <- function(d = '', n_rows = 1, widths = NULL,
                          table_caption = NULL, style = NULL,
                          file = 'temp_learn-table.html',
                          interactive = interactive()) {
  n_cols <- length(d)
  table <- paste(
    update_table_style(style),
    '\n<div class=jwTab>',
    '<table>',
    paste0('<caption>', table_caption, '</caption>'),
    col_group(n_cols, widths),
    table_header(d),
    table_body(d, ncl = n_cols, nrw = n_rows),
    '</table>',
    '</div>',
    sep = '\n'
  )
  if (!is.null(file)) cat(table, file= file)

  if (interactive) file.edit(file)
  else table
}








#' Create HTML table header
#'
#' Creates `<thead>` tag and child `<tr>` and `<th>` tags, including the column
#' names.
#' s
#' @param col_names Character; Vector of column names.
#'
#' @returns Character
#'
#' @examples
#' table_header(c('Criteria', 'Description', 'Weight')) |> cat()
#'
table_header <- function(d) {
  if (inherits(d, 'data.frame')) d <- names(d)

  paste0('<!-- Table header -->\n',
         '<thead>\n  <tr>\n',
         paste0('    <th>', d,'</th>\n', collapse = ''),
         '  </tr>\n</thead>\n')
}


#' Create HTML table body
#'
#' Creates `<tbody>` tag and child `<tr>` and `<td>` tags.
#'
#' @param nrw Integer; Number of rows.
#' @param ncl Integer; Number of columns
#'
#' @returns Character
#'
#' @examples
#' table_body() |> cat(sep = '')
#'
table_body <- function(d, nrw, ncl) {

  row_data <- if (inherits(d, 'data.frame')) {
    nrw <- nrow(d)
    apply(d, 1, \(r) paste0(sprintf('    <td> %s </td>\n', r), collapse = ''))
  } else {
    paste0(rep('    <td> __PLACEHOLDER__ </td>\n', ncl), collapse = '')
  }

  rows <- paste0(sprintf('  <!-- Row %s -->\n', seq_len(nrw)),
                 '  <tr>\n', row_data, '  </tr>\n',
                 collapse = '\n')

  paste0('<!-- Table body -->\n<tbody>\n', rows, '</tbody>')

}



#' Create HTML table column group
#'
#' Creates `<colgroup>` tag and child `<col>` tags, for the purpose of setting
#' column widths. If `widths` is `NULL`, the widths are set equally to `96% /
#' ncol`. If too few widths are provided, the table expands to fill the HTML
#' page.
#'
#' @param ncl Integer; Number of columns; derived from the length of the header
#'   row.
#' @param widths Numeric; Vector of column widths.
#'
#' @return Character; HTML `col_group` element
#'
#' @examples
#' col_group(3, c(20, 30, 50)) |> cat(sep = '')
#'
col_group <- function(ncl = NULL, widths = NULL) {
  if (is.null(widths)) {
    widths <- round(rep(96 / ncl, ncl), 2)
  }

  paste0('<col_group>\n',
         paste0('    <col width="', widths,'%">\n', collapse = ''),
         '</col_group>\n')
}




#' Update table CSS styles
#'
#' Creates a `<style>` tag with the provided CSS styles. Default styles are
#' included, and more styles can be added as a named vector. By default new
#' styles are added to the end of the default stylesheet. You can insert them at
#' a specific position using the `after` parameter.
#'
#' Declarations added to existing selectors are appended to the existing
#' declarations for those selectors. Original declarations are not removed.
#' Rules for new selectors are appended to the end of the rules set.
#'
#' @param ... Named Character vector of CSS styles. The names are the selectors,
#'   and the values are the CSS properties Position to insert the new styles
#'
#' @returns Character; HTML `<style>` element
#'
#' @examples
#'
#' # `.row_highlight` already exists in the default stylesheet.
#' # The new declaration is simply appended next to the old.
#' # Selector that do not exist in the default stylesheet are added
#' # to the end of the stylesheet.
#' update_table_style(element = 'property: value;',
#'               .row_highlight = 'background-color: #000000;',
#'               `selector with spaces` = 'property: value;')
#'
update_table_style <- function(...) {
  # cat(table_css)

  default_rules <- c(
    `table` = 'font-size: 1.6rem; font-family: Arial; border-collapse: collapse; border-bottom: 3px solid #6a0051;',
    `caption` = 'text-align: left;',
    `.jwTab th` = 'color: white; background-color: #6a0051; height: 60px; text-align: left; padding-left: 8px;',
    `td` = 'padding: 8px 8px; vertical-align: top;',
    `.row_outer` = 'border-top: 3px solid #6a0051;',
    `.row_inner` = 'border-top: 1px solid #e7d6e3; border-bottom: 1px solid #e7d6e3;',
    `.row_highlight` = 'background-color: #e7d6e3;'
  )
  defalt_rules <- paste('jwTab', default_rules)

  new_rules <- c(...)
  old_names <- names(default_rules)
  new_names <- names(new_rules)
  shared_names <- intersect(old_names, new_names)

  default_rules[shared_names] <- sapply(shared_names, \(n) paste(default_rules[[n]], new_rules[[n]]))
  remaining_names <- setdiff(new_names, old_names)

  updated_rules <- c(default_rules, new_rules[remaining_names])

  paste('<style>',
        paste('  ', names(updated_rules), ' {', updated_rules, '}',
              sep = '', collapse = '\n'),
        '</style>', sep = '\n')
}
