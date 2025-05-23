% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{reconstruct_bin}
\alias{reconstruct_bin}
\title{Reconstruct Binning Labels}
\usage{
reconstruct_bin(model_obj_tbl)
}
\arguments{
\item{model_obj_tbl}{A tibble with a `bin` column containing binning labels as character strings}
}
\value{
A tibble with the `bin` column modified to show more descriptive labels, such as "Less than X" or "Greater than Y"
}
\description{
This function adjusts bin labels in a model object tibble to make them more readable by converting "-Inf" and "Inf" boundaries into human-friendly phrases.
}
