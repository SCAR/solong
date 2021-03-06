% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allometry.R
\name{strip_units}
\alias{strip_units}
\title{Remove the units from an object}
\usage{
strip_units(x)
}
\arguments{
\item{x}{object: with units}
}
\value{
x, with units removed
}
\description{
A convenience function to remove the units assigned to an object.
}
\examples{
x <- data.frame(LRL=c(11.3,13.9),species=c("Architeuthis dux"),
  stringsAsFactors=FALSE)
x$LRL <- sol_set_property(x$LRL,"lower rostral length")
## apply an allometric equation
xa <- sol_allometry(x,c("342218_ML_Roel2000"))

strip_units(xa$allometric_value)


}
