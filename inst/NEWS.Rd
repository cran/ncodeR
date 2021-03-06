\name{NEWS}
\title{News for Package \pkg{ncodeR}}

\section{Changes in nodeR version 0.2.0.1 (2019-11-19)}{
  \itemize{
    \item Bugfix: Missing precision and recall calculation with rho/Kappa
    \item Bugfix: Fixed output from \code{autocode()}
  }
}

\section{Changes in nodeR version 0.2.0.0 (2019-10-09)}{
  \itemize{
    \item Restructed statistical history on codes
    \item \code{test()} defaults kappa_threshold to 0.9
    \item Codes now contain a holdout set and a set of touchable excerpts.
    \item Simplified retrieving of handsets for coding, along with randomizing the order
    \item Bugfix: \code{handcode()} bug fixed that didn't show full excerpt when there was a newline.
    \item Bugfix: Fixed bug when retreiving differences in code sets using \code{differences()}
  }
}

\section{Changes in nodeR version 0.1.3.0 (2019-04-09)}{
  \itemize{
    \item \code{handcode()} will now invalidate TestSet if it's already been tested
    \item \code{test()} now keeps a running history of all tests on the \code{Code}
    \item Bugfix: \code{summary()} will again properly show the most statistics results
    \item Bugfix: \code{resolve()} properly shows training kappas
    \item Bugfix: \code{handcode()} will now account for already coded excerpts when pulling more excerpts
  }
}

\section{Changes in nodeR version 0.1.2 (2018-08-29)}{
  \itemize{
    \item \code{autocode()} now has a first parameter \code{x} that accepts either a \code{Code} or a \code{CodeSet} object
    \item Bugfix: Using a \code{CodeSet} to autocode now works again
    \item Bugfix: \code{as.data.frame.CodeSet} now works as intended
  }
}

\section{Changes in nodeR version 0.1.1 (2018-08-17)}{
  \itemize{
    \item Excerpts can now by attached directly to a \code{code}
    \item Coding performance improved, allowing use of larger excerpt files
    \item While resolving differences, only the TrainingSet will be recoded
  }
}

\section{Changes in nodeR version 0.1.0 (2018-06-15)}{
  \itemize{
    \item Initial Release
  }
}