\name{sosConvertString}

\alias{sosConvertString}
\alias{sosConvertDouble}
\alias{sosConvertTime}
\alias{sosConvertLogical}

\title{SOS Conversion functions for Observation Results}
\description{
These functions are called by the parsers of om:Observation and om:Measurement documents to convert the actual values to the correct classes.
}

\usage{
sosConvertString(x, sos)
sosConvertDouble(x, sos)
sosConvertTime(x, sos)
sosConvertLogical(x, sos)
}

\arguments{
  \item{x}{The object to be converted.}
  \item{sos}{An object of class \code{SOS}, whose settings, like formating information, can be utilitzed.}
}

\details{
The methods are automatically called from the given SOS's list of conversion functions. This is either default or can be set manually on creation.

If you want to provide you own conversion functions, follow the example below. Always include the common parameters \code{x} and \code{sos}.

There are functions to access the converters of a SOS (\code{\link{sosDataFieldConverters-methods}}) and to combine default and your own converters (\code{\link{SosDataFieldConvertingFunctions}}).

}

\value{An object of the respective class converted from the parameter \code{x}.}

\author{Daniel Nuest \email{daniel.nuest@uni-muenster.de}}

\seealso{\code{\link{sosDataFieldConverters-methods}}, \code{\link{SosDataFieldConvertingFunctions}}}

\examples{

\dontrun{
sos <- SOS(url = SosExampleServices()[[2]])
one <- sosConvertDouble("1", sos)
class(one)

# add conversion rules, also possible to override default ones
myConverters <- SosDataFieldConvertingFunctions(
	"C" = sosConvertDouble,
	"S/m" = sosConvertDouble)
sos <- SOS(url = SosExampleServices()[[2]], dataFieldConverters = myConverters)

# show converters
sosDataFieldConverters(sos)
}

}

\keyword{utilities}
