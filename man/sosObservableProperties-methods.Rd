\name{sosObservableProperties-methods}

\docType{methods}

\alias{sosObservableProperties}
\alias{sosObservableProperties-methods}
\alias{sosObservableProperties,list-method}
\alias{sosObservableProperties,OmObservation-method}
\alias{sosObservableProperties,OmObservationCollection-method}
\alias{sosObservableProperties,SOS-method}
\alias{sosObservableProperties,SosObservationOffering_2.0.0-method}
\alias{sosObservableProperties,SweCompositePhenomenon-method}
\alias{sosObservableProperties,SwePhenomenonProperty-method}

\title{Methods for Function \code{sosObservableProperties} in Package \pkg{sos4R}}

\description{Methods for function \code{sosObservableProperties} in package \pkg{sos4R}. The function allows to retrieve observable properties, e.g. air temperature, wind speed, etc., listed in the capabilities of a Sensor Observation Service and other classes.}
\section{Methods}{
  \describe{
    \item{\code{signature(obj = "list")}}{List of objects to retreive properties from.}
    \item{\code{signature(obj = "OmObservation")}}{Get observable properties from an object of class \code{OmObservation}.}
    \item{\code{signature(obj = "OmObservationCollection")}}{Get observable properties from an object of class \code{OmObservationCollection}, namely the items in the collection.}
    \item{\code{signature(obj = "SOS")}}{Get observable properties for a whole connection to an SOS.}
    \item{\code{signature(obj = "SosObservationOffering_2.0.0")}}{Get observable properties from an object of class \code{SosObservationOffering_2.0.0}, needed internally for getting properties for a whole SOS.}
    \item{\code{signature(obj = "SweCompositePhenomenon")}}{Get observable properties from an object of class \code{SweCompositePhenomenon}.}
    \item{\code{signature(obj = "SwePhenomenonProperty")}}{Get observable properties from an object of class \code{SwePhenomenonProperty}.}
  }

}

\keyword{methods}
