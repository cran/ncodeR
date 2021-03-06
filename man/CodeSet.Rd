% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CodeSet.R
\docType{data}
\name{CodeSet}
\alias{CodeSet}
\title{CodeSet}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
CodeSet
}
\value{
CodeSet object

CodeSet
}
\description{
Object representing a set of codes
}
\section{Fields}{

\describe{
\item{\code{title}}{Title of the CodeSet}

\item{\code{description}}{String description of the set of codes to be included}

\item{\code{excerpts}}{Character vector of text excerpts to code (optional)}

\item{\code{expressions}}{Codes to include in the CodeSet (optional)}
}}

\examples{
data(RS.data)
rs = RS.data
code.set = code.set("Demo RS CodeSet", "CodeSet made for the demo", excerpts = rs$text, codes = c())

}
\keyword{datasets}
