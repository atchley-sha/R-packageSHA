#' Standard Kable Format
#'
#' Create and format a Kable with standard formatting
#' @param df The dataframe to format as a flextable
#' @param caption Caption to add to Kable
#' @param digits The number of digits to round to
#' @param align How to align the table; a vector or string for each column
#' @param linesep Line separation vector
#' @param bootstrap_options Bootstrap options to be used
#' @param latex_options LaTeX options to be used
#' @return A formatted flextable object
#' @export
my_kbl <- function(df,
                   caption = NULL,
                   digits = 2,
                   align = NULL,
                   linesep = "",
                   bootstrap_options = c("striped", "hover", "responsive"),
                   latex_options = c("striped", "repeat_header", "HOLD_position")
) {
  knitr::kable(df, digits = digits, align = align, caption = caption, booktabs = T, linesep = linesep) %>%
    kableExtra::kable_styling(
      bootstrap_options = bootstrap_options,
      latex_options = latex_options,
      full_width = F
    )
}



#' Standard Flextable Format
#'
#' Create and format a flextable with standard formatting
#' @param df The dataframe to format as a flextable
#' @param digits The number of digits `round()` should use
#' @param align How to align the table; one of "left", "center", "right", "justify"
#' @param aln_part Which part to align; one of "header", "body", "footer", "all"
#' @param header Table header to be passed to `add_header_lines()`
#' @param vline Which column(s) to place a vertical line after
#' @param hline Which row(s) to place a horizontal line below
#' @return A formatted flextable object
#' @export
my_flextable <- function(df,
                         digits = 2,
                         align = "center",
                         aln_part = "all",
                         header = NULL,
                         vline = NULL,
                         hline = NULL
){
  dplyr::mutate_if(df, is.numeric,
                   round, digits = digits) %>%
    flextable::flextable() %>%
    flextable::align(align = align, part = aln_part) %>%
    flextable::autofit() %>%
    {
      if(!is.null(vline)) flextable::vline(., j = vline)
      else .
    } %>%
    {
      if(!is.null(hline)) flextable::hline(., i = hline)
      else .
    } %>%
    {
      if(!is.null(header)) flextable::add_header_lines(., header)
      else .
    }
}



#' Enumerate Matrix Rows/Columns
#'
#' Converts a matrix to a tibble and adds row/column names that enumerate the rows/columns
#' @param mat The matrix to enumerate
#' @return A tibble with numbered rows and columns
#' @export
enumerate_matrix <- function(mat){
  mat %>%
    tibble::as_tibble() %>%
    tibble::rowid_to_column() %>%
    `colnames<-`(c(" ", 1:ncol(mat)))
}
