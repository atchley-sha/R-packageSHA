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


#'Write matrix
#'
#'Writes a matrix in Rmd that looks nice. NOTE: use {r results = 'asis'} to get both LaTeX and HTML output.
#'@param mat The matrix to print
#'@param begin What symbol to begin the matrix with
#'@param end What symbol to end the matrix with
#'@return A printed LaTeX/HTML matrix
#'@export
write_mat <- function(mat, begin, end) {
  mat_out <-
    apply(x, 1, function(mat) {
      paste(
        paste(mat, collapse = "&"),
        "\\\\"
      )
    })
  writeLines(c("\\[", begin, "\\begin{matrix}", mat_out, "\\end{matrix}", end, "\\]"))
}
