\name{checkRequest-methods}
\docType{methods}

\alias{checkRequest}
\alias{checkRequest-methods}
\alias{checkRequest,SOS,SosDescribeSensor,logical-method}
\alias{checkRequest,SOS,SosGetObservationById,logical-method}
\alias{checkRequest,SOS,SosGetObservation,logical-method}
\alias{checkRequest,SOS,OwsGetCapabilities_1.1.0,logical-method}
\alias{checkRequest,SOS,OwsGetCapabilities_2.0.0,logical-method}

% Version 2.0.0
\alias{checkRequest,SOS_2.0.0,SosGetFeatureOfInterest_2.0.0,logical-method}
\alias{checkRequest,SOS_2.0.0,SosGetObservation,logical-method}
\alias{checkRequest,SOS_2.0.0,SosGetDataAvailability_1.0.0,logical-method}


\title{Methods for Function checkRequest}

\description{
A function to check request prior to sending them to a service. This function is autmatically called during the request process and can be used to check the request for consistency with ifself as well as with available metadata, and also perform additional validity checks that might not be possible with class validation.
}

\section{Methods}{
\describe{
	\item{\code{signature(service = "SOS", operation = "DescribeSensor", verbose = "logical")}}{
	Checking a DescribeSensor request that is send to a SOS. This method currently checks the following elements:
		\itemize{
			\item{\code{operation@service} attribute must be \verb{SOS}}
			\item{\code{operation@request} attribute must be \verb{DescribeSensor}}
			\item{\code{operation@procedure} must be listed in the given service's capabilities}
			\item{\code{operation@outputFormat} must be supported by the operations capabilities description}
			\item{\code{operation@binding} must be supported by the package implementations. See \link{SosBindings}.}
		}
	}
	\item{\code{signature(service = "SOS", operation = "SosGetObservationById", verbose = "logical")}}{Checking a GetObservationById request. \strong{Warning: Function not implemented yet.}}
	\item{\code{signature(service = "SOS", operation = "SosGetObservation", verbose = "logical")}}{Checking a GetObservation request. \strong{Warning: Function not implemented yet.}}
	\item{\code{signature(service = "SOS", operation = "OwsGetCapabilities_1.1.0", verbose = "logical")}}{Checking a GetCapabilities request. \strong{Warning: Function not implemented yet.}}
	\item{\code{signature(service = "SOS", operation = "OwsGetCapabilities_2.0.0", verbose = "logical")}}{Checking a GetCapabilities request. \strong{Warning: Function not implemented yet.}}
}

}

\keyword{methods}
