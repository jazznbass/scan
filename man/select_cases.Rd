% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/select_cases.R
\name{select_cases}
\alias{select_cases}
\title{Select a subset of cases}
\usage{
select_cases(scdf, ...)
}
\arguments{
\item{scdf}{A single-case data frame. See \code{\link[=scdf]{scdf()}} to learn about this
format.}

\item{...}{Selection criteria. Either numeric, objectnames, or as characters.}
}
\value{
An scdf with a subset of cases
}
\description{
Select a subset of cases
}
\examples{
select_cases(exampleAB, Johanna, Karolina)
select_cases(exampleAB, c(Johanna, Karolina))
select_cases(exampleAB, 1,2)
select_cases(exampleAB, 1:2)
select_cases(exampleAB, -Johanna)
select_cases(exampleAB, -c(Johanna, Karolina))
v <- c("Moritz", "Jannis")
select_cases(exampleA1B1A2B2, v)
}
\seealso{
Other data manipulation functions: 
\code{\link{add_l2}()},
\code{\link{as.data.frame.scdf}()},
\code{\link{as_scdf}()},
\code{\link{fill_missing}()},
\code{\link{moving_median}()},
\code{\link{outlier}()},
\code{\link{ranks}()},
\code{\link{rescale}()},
\code{\link{scdf}()},
\code{\link{set_vars}()},
\code{\link{shift}()},
\code{\link{smooth_cases}()},
\code{\link{standardize}()},
\code{\link{truncate_phase}()}
}
\concept{data manipulation functions}
