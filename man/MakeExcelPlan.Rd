% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zigzag-plot.R
\name{MakeExcelPlan}
\alias{MakeExcelPlan}
\title{Makes an excel plan of \code{\link{MakeMainPlan}}.}
\usage{
MakeExcelPlan(
  List,
  DefaultCols = c(`Mb 311` = "#000000"),
  Label = "geno",
  FieldName = "Field",
  user = "user",
  Book = NULL,
  Save = FALSE
)
}
\arguments{
\item{List}{Is the dater frame output from \code{\link{MakeMainPlan}} or something with the same headers}

\item{DefaultCols}{Are a list of key value pair sets for any pre-defined colours to use}

\item{Label}{This is what is to be written on the Field Plan. Options are: 'geno' 'geno+UID' 'UID' 'geno+RC' 'RC'}

\item{FieldName}{This is the name of the field the plan is for eg JKI-43.}

\item{user}{Name of the user}

\item{Book}{If there is allredy a workbook you want to add a sheat to put the object hear, it must be a openxlsx workbook}

\item{Save}{This is if to save the xlsx file in the function}
}
\value{
openxlsx workbook
}
\description{
It is often needed to discuss the field plan or for dead plants etc..
}
