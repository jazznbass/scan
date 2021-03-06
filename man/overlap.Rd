% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/overlap.R
\name{overlap}
\alias{overlap}
\alias{overlapSC}
\title{Overlap indices for single-case data}
\usage{
overlap(data, dvar, pvar, mvar, decreasing = FALSE, phases = c(1, 2))

overlapSC(...)
}
\arguments{
\item{data}{A single-case data frame. See \code{\link{scdf}} to learn about this format.}

\item{dvar}{Character string with the name of the dependent variable. Defaults to the attributes in the scdf file.}

\item{pvar}{Character string with the name of the phase variable. Defaults to the attributes in the scdf file.}

\item{mvar}{Character string with the name of the measurement time variable. Defaults to the attributes in the scdf file.}

\item{decreasing}{If you expect data to be lower in the B phase, set
\code{decreasing = TRUE}. Default is \code{decreasing = FALSE}.}

\item{phases}{A vector of two characters or numbers indicating the two
phases that should be compared. E.g., \code{phases = c("A","C")} or
\code{phases = c(2,4)} for comparing the second to the fourth phase. Phases
could be combined by providing a list with two elements. E.g., \code{phases
= list(A = c(1,3), B = c(2,4))} will compare phases 1 and 3 (as A) against 2
and 4 (as B). Default is \code{phases = c("A","B")}.}

\item{...}{Further arguments passed to the function.}
}
\value{
\item{overlap}{A data frame consisting of the following indices for
each single-case for all cases: PND, PEM, PET, NAP, PAND, Tau-U (A vs. B -
Trend A), Diff_mean, Diff_trend, SMD.}
\item{phases.A}{Selection for A phase.}
\item{phases.B}{Selection for B phase.}
\item{design}{Phase design.}
}
\description{
The \code{overlap} function provides the most common overlap indices for
single-case data and some additional statistics.
}
\examples{

## Display overlap indices for one single-case
overlap(Huitema2000, decreasing = TRUE)

## Display overlap indices for six single-cases
overlap(GruenkeWilbert2014)

## Combining phases for analyszing designs with more than two phases   
overlap(exampleA1B1A2B2, phases = list(c("A1","A2"), c("B1","B2")))

}
\seealso{
Other overlap functions: 
\code{\link{corrected_tau}()},
\code{\link{nap}()},
\code{\link{pand}()},
\code{\link{pem}()},
\code{\link{pet}()},
\code{\link{pnd}()},
\code{\link{tau_u}()}
}
\author{
Juergen Wilbert
}
\concept{overlap functions}
