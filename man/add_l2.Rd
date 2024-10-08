% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_l2.R
\name{add_l2}
\alias{add_l2}
\title{Add level-2 data}
\usage{
add_l2(scdf, data_l2, cvar = "case")
}
\arguments{
\item{scdf}{A single-case data frame. See \code{\link[=scdf]{scdf()}} to learn about this
format.}

\item{data_l2}{A level 2 dataset.}

\item{cvar}{Character string with the name of the "case" variable in the L2
dataset (default is 'case').}
}
\value{
An scdf
}
\description{
Merges variables with corresponding case names from a data.frame with an scdf
file
}
\details{
This function is mostly used in combination with the \code{\link[=hplm]{hplm()}} function.
}
\examples{
Leidig2018 |> add_l2(Leidig2018_l2)
}
\seealso{
\code{\link[=hplm]{hplm()}}

Other data manipulation functions: 
\code{\link{as.data.frame.scdf}()},
\code{\link{as_scdf}()},
\code{\link{fill_missing}()},
\code{\link{moving_median}()},
\code{\link{outlier}()},
\code{\link{ranks}()},
\code{\link{rescale}()},
\code{\link{scdf}()},
\code{\link{select_cases}()},
\code{\link{set_vars}()},
\code{\link{shift}()},
\code{\link{smooth_cases}()},
\code{\link{standardize}()},
\code{\link{truncate_phase}()}
}
\concept{data manipulation functions}
\keyword{transform}
