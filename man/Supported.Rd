\name{Supported}

\alias{SosSupported}
\alias{SosSupportedComparisonOperators}
\alias{SosSupportedConnectionMethods}
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
SosSupportedOperations()
SosSupportedComparisonOperators()
SosSupportedConnectionMethods()
SosSupportedGeometryOperands()
SosSupportedResponseFormats()
SosSupportedResponseModes()
SosSupportedResultModels()
SosSupportedSpatialOperators()
SosSupportedTemporalOperators()
SosSupportedServiceVersions()
}

\details{
\strong{Supported features}, like connection methods and supported response modes, are accessible by functions starting with "SosSupported". See the examples section for a complete list of these functions.

It is encouraged to rather use these methods than manually set character values for compatibility with future versions, e.g. \code{SosSupportedConnectionMethods()[[1]]} instead of directly writing \code{"GET"}.
}

\value{A list of supported values for the respective parameter.}

%\references{}

\author{Daniel Nüst \email{daniel.nuest@uni-muenster.de}}

%\note{}
%\section{Warning }{}

\seealso{See \code{\link{Defaults}} for default values of parameters.}

\examples{
# The supported operations of the specification
SosSupportedOperations()

# HTTP connection methods supported by this sos4R implementation
SosSupportedConnectionMethods()
myConnectionType <- SosSupportedConnectionMethods()[[1]]
myConnectionType

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
