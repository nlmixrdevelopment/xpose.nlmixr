\name{theo_sd_fit}
\alias{theo_sd_fit}
\docType{data}
\title{
  Single-dose theophylline PK data fit
}
\description{
  Single-dose theophylline PK data fit using the first-order conditional estimation method with interaction (FOCEI) in 'nlmixr', a modified tibble.
}
\usage{data("theo_sd_fit")}
\format{
  A \code{\link[tibble]{tibble}} with 132 observations and 22 variables, and an additional 13 properties.
  \describe{
    \item{\code{ID}}{Individual identifier, a factor}
    \item{\code{TIME}}{Time in hours, a numeric vector}
    \item{\code{DV}}{Theophylline concentration, a numeric vector}
    \item{\code{EVID}}{Event identifier, a numeric vector}
    \item{\code{PRED}}{Population predictions, a numeric vector}
    \item{\code{RES}}{Residuals, a numeric vector}
    \item{\code{WRES}}{Weighted residuals, a numeric vector}
    \item{\code{IPRED}}{Individual predictions, a numeric vector}
    \item{\code{IRES}}{Individual residuals, a numeric vector}
    \item{\code{IWRES}}{Individual weighted residuals, a numeric vector}
    \item{\code{CPRED}}{Conditional predictions, a numeric vector}
    \item{\code{CRES}}{Conditional residuals, a numeric vector}
    \item{\code{CWRES}}{Conditional weighted residuals, a numeric vector}
    \item{\code{eta.ka}}{Interindividual variability in ka, a numeric vector}
    \item{\code{eta.cl}}{Interindividual variability in CL/F, a numeric vector}
    \item{\code{eta.v}}{Interindividual variability in V/F, a numeric vector}
    \item{\code{ka}}{Absorption rate in /h, a numeric vector}
    \item{\code{cl}}{Apparent clearance in L/h, a numeric vector}
    \item{\code{v}}{Apparent volume of distribution in L, a numeric vector}
    \item{\code{cp}}{Theophylline concentration, a numeric vector}
    \item{\code{depot}}{Amount of theophylline in the depot compartment, a numeric vector}
    \item{\code{center}}{Amount of theophylline in the central compartment, a numeric vector}
    \item{\code{omega}}{Omega matrix}
    \item{\code{omegaR}}{Omega Correlation matrix}
    \item{\code{shrink}}{Shrinkage table, includes skewness, kurtosis, and eta p-values}
    \item{\code{parFixed}}{Fixed Effect Parameter Table}
    \item{\code{theta}}{Fixed Parameter Estimates}
    \item{\code{eta}}{Individual Parameter Estimates}
    \item{\code{seed}}{Seed (if applicable)}
    \item{\code{coefficients}}{Fixed and random coefficients}
    \item{\code{meta}}{Model meta information environment}
    \item{\code{modelName}}{Model name (from R function)}
    \item{\code{dataName}}{Name of R data input}
    \item{\code{simInfo}}{RxODE list for simulation}
    \item{\code{sigma}}{List of sigma components and their values}
  }
}
\details{
  This dataset is an \code{nlmixr} fit object for demonstrating the use of \code{xpose.nlmixr}.
}
\source{
  NONMEM/nlme.
}
\examples{
data(theo_sd_fit)
str(theo_sd_fit)
}
\keyword{datasets}
