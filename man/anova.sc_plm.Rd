% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anova.R
\name{anova.sc_plm}
\alias{anova.sc_plm}
\alias{anova.sc_hplm}
\title{ANOVA Table for Piecewise Linear Models}
\usage{
\method{anova}{sc_plm}(object, ...)

\method{anova}{sc_hplm}(object, ...)
}
\arguments{
\item{object}{an object containing the results returned by a plm().}

\item{...}{additional plm objects.}
}
\description{
ANOVA Table for Piecewise Linear Models
}
\examples{
mod1 <- plm(exampleAB$Johanna)
mod2 <- plm(exampleAB$Johanna, level = FALSE, slope = FALSE)
anova(mod1, mod2)

## For multilevel models:
# mod1 <- hplm(exampleAB_50)
# mod2 <- hplm(exampleAB_50, slope = FALSE)
# anova(mod1, mod2)
}
