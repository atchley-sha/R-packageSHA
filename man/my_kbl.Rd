% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_formatting.R
\name{my_kbl}
\alias{my_kbl}
\title{Standard Kable Format}
\usage{
my_kbl(
  df,
  caption = NULL,
  digits = 2,
  align = NULL,
  linesep = "",
  escape = F,
  bootstrap_options = c("striped", "hover", "responsive"),
  latex_options = c("striped", "repeat_header", "HOLD_position")
)
}
\arguments{
\item{df}{The dataframe to format as a flextable}

\item{caption}{Caption to add to Kable}

\item{digits}{The number of digits to round to}

\item{align}{How to align the table; a vector or string for each column}

\item{linesep}{Line separation vector}

\item{escape}{T/F to escape special characters}

\item{bootstrap_options}{Bootstrap options to be used}

\item{latex_options}{LaTeX options to be used}
}
\value{
A formatted flextable object
}
\description{
Create and format a Kable with standard formatting
}
