\name{rosnerFEV}
\alias{rosnerFEV}
\docType{data}
\title{
Rosner's FEV data.
}
\description{
This data set consists of 654 observations on youths aged 3 to 19 from East Boston recorded duing the middle to late 1970's. 
Forced expiratory volume (FEV), a measure of lung capacity, is the variable of interest. 
We slightly modified the original data, this version only includes the covariates used in our paper.}

\usage{data(rosnerFEV)}
\format{
  A data frame with 654 observations on the following 3 variables.
  \describe{
    \item{\code{x}}{Age (years).}
    \item{\code{z}}{A binary variable indicating whether or not the youth smokes. Nonsmoker is 0. Smoker is 1.}
    \item{\code{y}}{Forced expiratory volume (liters). Roughly the amount of air an individual can exhale in the first second of a forceful breath.}
  }
}

\references{
Rosner, B. (1995) "Fundamentals of biostatistics".  Duxbury Press:  New York.
}
\keyword{datasets}
