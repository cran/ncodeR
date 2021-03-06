% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/handcode.R
\name{handcode}
\alias{handcode}
\title{Handcode excerpts}
\usage{
handcode(code = NULL, excerpts = NULL, expressions = NULL,
  n = ifelse(is.null(this.set), 10, length(this.set)), baserate = 0.2,
  unseen = F, this.set = NULL, results = NULL)
}
\arguments{
\item{code}{Code object to handcode}

\item{excerpts}{Excerpts to code (optional)}

\item{expressions}{Expressions to code with (options)}

\item{n}{Number of excerpts to handcode}

\item{baserate}{Value between 0 and 1, inflates the baserate chosen excerpts to code, ensuring the number of positive at least equal to n * baserate}

\item{unseen}{Logical or number Indicating additional excerpts with unseen words should be added. If TRUE (default), two words added or by `number`}

\item{this.set}{[TBD]}

\item{results}{[TBD]}
}
\value{
Code
}
\description{
Handcode a set of excerpts using a vector of expressions
}
\details{
Handcode a set of excerpts using a vector of expressions
}
