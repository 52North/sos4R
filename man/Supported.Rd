\name{Supported}

\alias{SosSupported}
\alias{SosSupportedComparisonOperators}
\alias{SosSupportedBindings}
\alias{SosSupportedGeometryOperands}
\alias{SosSupportedResponseFormats}
\alias{SosSupportedResponseModes}
\alias{SosSupportedResultModels}
\alias{SosSupportedSpatialOperators}
\alias{SosSupportedTemporalOperators}
\alias{SosSupportedServiceVersions}
\alias{SosSupportedOperations}

\title{Functions to Access Supported Features of the Current sos4R Implementation}

\description{
These functions can be used to access the supported parameters for a range of settings of a SOS connection.
}

\usage{
SosSupportedOperations(version = sos100_version)
SosSupportedComparisonOperators()
SosSupportedBindings()
SosSupportedGeometryOperands()
SosSupportedResponseFormats()
SosSupportedResponseModes()
SosSupportedResultModels()
SosSupportedSpatialOperators()
SosSupportedTemporalOperators()
SosSupportedServiceVersions()
}

\arguments{
  \item{version}{The SOS specification version.}
}

\details{
\strong{Supported features}, like connection methods and supported response modes, are accessible by functions starting with "SosSupported". See the examples section for a complete list of these functions.

It is encouraged to rather use these methods than manually set character values for compatibility with future versions, e.g. \code{SosSupportedBindings()[[1]]} instead of directly writing \code{"GET"}.
}

\value{A list of supported values for the respective parameter.}

\author{Daniel Nuest \email{daniel.nuest@uni-muenster.de}}

\seealso{See \code{\link{Defaults}} for default values of parameters.}

\examples{
# The supported operations of the specification
SosSupportedOperations()

# HTTP connection methods supported by this sos4R implementation
SosSupportedBindings()
myBinding <- SosSupportedBindings()[[1]]
myBinding

# Formats, modes and models that can be processed by this implementation
SosSupportedResponseFormats()
SosSupportedResultModels()
SosSupportedResponseModes()

# Operators and operands for filtering in a GetObservation request
SosSupportedTemporalOperators()
SosSupportedSpatialOperators()
SosSupportedGeometryOperands()
SosSupportedComparisonOperators()
}

\keyword{utilities}
