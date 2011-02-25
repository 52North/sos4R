\name{OWS}
\Rdversion{1.1}

\docType{class}

\alias{OwsCapabilities_1.1.0-class}
\alias{OwsCapabilities_2.0.0-class}
\alias{OwsCapabilities-class}
\alias{OwsContents-class}
\alias{OwsContentsOrNULL-class}
\alias{OwsException-class}
\alias{OwsExceptionReport}
\alias{OwsExceptionReport-class}
\alias{OwsGetCapabilities_1.1.0-class}
\alias{OwsGetCapabilities_2.0.0-class}
\alias{OwsGetCapabilities-class}
\alias{OwsOperation-class}
\alias{OwsOperationsMetadata-class}
\alias{OwsOperationsMetadataOrNULL-class}
\alias{OwsRange-class}
\alias{OwsServiceIdentification-class}
\alias{OwsServiceIdentificationOrNULL-class}
\alias{OwsServiceOperation-class}
\alias{OwsServiceProvider-class}
\alias{OwsServiceProviderOrNULL-class}

\alias{print,OwsCapabilities-method}
\alias{print,OwsCapabilities_1.1.0-method}
\alias{print,OwsCapabilities_2.0.0-method}
\alias{print,OwsContents-method}
\alias{print,OwsException-method}
\alias{print,OwsExceptionReport-method}
\alias{print,OwsGetCapabilities-method}
\alias{print,OwsGetCapabilities_1.1.0-method}
\alias{print,OwsGetCapabilities_2.0.0-method}
\alias{print,OwsOperation-method}
\alias{print,OwsOperationsMetadata-method}
\alias{print,OwsRange-method}
\alias{print,OwsServiceIdentification-method}
\alias{print,OwsServiceOperation-method}
\alias{print,OwsServiceProvider-method}
\alias{print.summary.OwsRange}

\alias{show,OwsCapabilities_1.1.0-method}
\alias{show,OwsCapabilities_2.0.0-method}
\alias{show,OwsCapabilities-method}
\alias{show,OwsContents-method}
\alias{show,OwsException-method}
\alias{show,OwsExceptionReport-method}
\alias{show,OwsGetCapabilities_1.1.0-method}
\alias{show,OwsGetCapabilities_2.0.0-method}
\alias{show,OwsGetCapabilities-method}
\alias{show,OwsOperation-method}
\alias{show,OwsOperationsMetadata-method}
\alias{show,OwsRange-method}
\alias{show,OwsServiceIdentification-method}
\alias{show,OwsServiceOperation-method}
\alias{show,OwsServiceProvider-method}
\alias{toString,OwsCapabilities-method}
\alias{toString,OwsCapabilities_1.1.0-method}
\alias{toString,OwsCapabilities_2.0.0-method}
\alias{toString,OwsContents-method}
\alias{toString,OwsException-method}
\alias{toString,OwsExceptionReport-method}
\alias{toString,OwsGetCapabilities-method}
\alias{toString,OwsGetCapabilities_1.1.0-method}
\alias{toString,OwsGetCapabilities_2.0.0-method}
\alias{toString,OwsOperation-method}
\alias{toString,OwsOperationsMetadata-method}
\alias{toString,OwsRange-method}
\alias{toString,OwsServiceIdentification-method}
\alias{toString,OwsServiceOperation-method}
\alias{toString,OwsServiceProvider-method}

\alias{OwsCapabilities}
\alias{OwsContents}
\alias{OwsException}
\alias{OwsGetCapabilities}
\alias{OwsOperation}
\alias{OwsOperationsMetadata}
\alias{OwsRange}
\alias{OwsServiceIdentification}
\alias{OwsServiceProvider}

\alias{sosResult,OwsExceptionReport-method}

\alias{sosTitle,OwsServiceIdentification-method}
\alias{sosAbstract,OwsServiceIdentification-method}

\alias{summary.OwsRange}

\title{Classes and Construction Functions for Elements of the OWS Namespace}

\description{These classes represent elements from the OGC Web Services Common Specification and the OGC Web Services Common Standard.}

\section{Objects from the Class}{
Objects can be created by calling the construction functions, e.g. in the form \code{OwsCapabilities_1.1.0(...)}, \code{OwsContents(...)} or \code{OwsException(...)}.

The following classes are virtual and no objects may be created from it: \code{OwsContentsOrNULL}, \code{OwsServiceIdentificationOrNULL}, \code{OwsServiceProviderOrNULL}, \code{OwsOperationsMetadataOrNULL}.
}

\usage{
OwsCapabilities(version, updateSequence = NA, owsVersion = sosDefaultGetCapOwsVersion, identification = NULL, provider = NULL, operations = NULL, contents = NULL, languages = NULL)
OwsException(exceptionCode, exceptionText = c(), locator = as.character(NA))
OwsExceptionReport(version, lang = as.character(NA), exceptions = list(NA))
OwsGetCapabilities(service, acceptVersions, sections = sosDefaultGetCapSections, acceptFormats = sosDefaultGetCapAcceptFormats, updateSequence = c(as.character(NA)), owsVersion = sosDefaultGetCapOwsVersion, acceptLanguages = c(NA))
OwsOperation(name, DCPs, parameters = list(NA), constraints = list(NA), metadata = list(NA))
OwsOperationsMetadata(operations, parameters = list(NA), constraints = list(NA), extendedCapabilities = xmlNode(NA))
OwsRange(minimumValue = as.character(NA), maximumValue = as.character(NA), rangeClosure = as.character(NA), spacing = as.character(NA))
OwsServiceProvider(providerName, providerSite = as.character(NA), serviceContact = xmlNode(NA))
OwsServiceIdentification(serviceType, serviceTypeVersion, profile = c(NA), title, abstract = c(NA), keywords = c(NA), fees = as.character(NA), accessConstraints = c(NA))
}

\section{Slots}{
Capabilities:
  \describe{
    \item{\code{contents}:}{Object of class \code{"OwsContentsOrNULL"}, the contents section of a capabilities document.}
    \item{\code{identification}:}{Object of class \code{"OwsServiceIdentificationOrNULL"}, the identification section of a capabilities document.}
    \item{\code{languages}:}{Object of class \code{"XMLAbstractNode"}, the languages section of a capabilities document, only OWS 2.0.0!}
    \item{\code{operations}:}{Object of class \code{"OwsOperationsMetadataOrNULL"}, the operations section of capabilities document.}
    \item{\code{owsVersion}:}{Object of class \code{"character"}, the used version of OWS.}
    \item{\code{provider}:}{Object of class \code{"OwsServiceProviderOrNULL"}, the provider section of a capabilities document.}
    \item{\code{updateSequence}:}{Object of class \code{"character"}, the updateSequence attribute of a capabilities document.}
    \item{\code{version}:}{Object of class \code{"character"}, the version of the described service in a capabilities document.}
    \item{\code{exceptionCode}:}{Object of class \code{"character"}, the code attribute of an OWS Exception, see \code{\link{OwsExceptionsData}}.}
    \item{\code{exceptionText}:}{Object of class \code{"vector"}, the text of an OWS Exception, see \code{\link{OwsExceptionsData}}.}
    \item{\code{locator}:}{Object of class \code{"character"}, the locator attribute of an OWS Exception, see \code{\link{OwsExceptionsData}}.}
    \item{\code{version}:}{Object of class \code{"character"}, the version of an OWS Exception, see \code{\link{OwsExceptionsData}}.}
    \item{\code{lang}:}{Object of class \code{"character"}, the code attribute of an OWS Exception, see \code{\link{OwsExceptionsData}}.}
    \item{\code{exceptions}:}{Object of class \code{"list"}, the list of \code{OwsException} in a \code{OwsExceptionReport}.}
    \item{\code{sections}:}{Object of class \code{"vector"}, unordered character vector of zero or more names of sections of service metadata document to be returned in service metadata document.}
    \item{\code{acceptFormats}:}{Object of class \code{"vector"}, unordered character vector of zero or more response formats desired by client, with preferred formats listed first.}
    \item{\code{updateSequence}:}{Object of class \code{"vector"}, service metadata document version.}
    \item{\code{acceptVersions}:}{Object of class \code{"character"}, comma-separated prioritized sequence of one or more specification versions accepted by client, with preferred versions listed first.}
    \item{\code{service}:}{Object of class \code{"character"}, the name of the service.}
    \item{\code{request}:}{Object of class \code{"character"}, the name of the operation/request.}
    \item{\code{acceptLanguages}:}{Object of class \code{"vector"}, an unordered character vector of zero or more languages desired by client, with preferred formats listed first. Only OWS 2.0.0!}
    \item{\code{name}:}{Object of class \code{"character"}, name of an operation (request) (for example, GetCapabilities).}
    \item{\code{DCPs}:}{Object of class \code{"list"}, information for a Distributed Computing Platform (DCP) supported for an operation.}
    \item{\code{parameters}:}{Object of class \code{"list"}, parameter valid domain that applies to an operation which a server implements.}
    \item{\code{constraints}:}{Object of class \code{"list"}, Constraint on valid domain of a non-parameter quantity that applies to an operation which a server implements.}
    \item{\code{metadata}:}{Object of class \code{"list"}, metadata about an operation and its implementation.}
    \item{\code{operations}:}{Object of class \code{"list"}, a list of objects of class \code{OwsOperation} in a \code{OperationsMetadata} object.}
    \item{\code{extendedCapabilities}:}{Object of class \code{"XMLAbstractNode"}, the possible contents of the ExtendedCapabilities subsection.}
    \item{\code{minimumValue}:}{Object of class \code{"character"}, minimum value of a range (numeric parameter).}
    \item{\code{maximumValue}:}{Object of class \code{"character"}, maximum value of a range (numeric parameter).}
    \item{\code{rangeClosure}:}{Object of class \code{"character"}, specifies which of minimum and maximum values are included in this range.}
    \item{\code{spacing}:}{Object of class \code{"character"}, regular distance or spacing between allowed values in this range; included when range is not continuous.}
    \item{\code{serviceType}:}{Object of class \code{"character"}, the service type name from registry of services.}
    \item{\code{serviceTypeVersion}:}{Object of class \code{"vector"}, version of a service type implemented by the server.}
    \item{\code{profile}:}{Object of class \code{"vector"}, identifier of OGC Web Service (OWS) Application Profile.}
    \item{\code{title}:}{Object of class \code{"vector"}, title of the server, normally used for display to a human.}
    \item{\code{abstract}:}{Object of class \code{"vector"}, brief narrative description of this server, normally available for display to a human.}
    \item{\code{keywords}:}{Object of class \code{"vector"}, unordered list of one or more commonly used or formalised word(s) or phrase(s) used to describe a server.}
    \item{\code{fees}:}{Object of class \code{"character"}, fees and terms for using a server, including the monetary units as specified in ISO 4217.}
    \item{\code{accessConstraints}:}{Object of class \code{"vector"}, access constraints that should be observed to assure the protection of privacy or intellectual property, and any other restrictions on retrieving or using data from or otherwise using a server.}
    \item{\code{providerName}:}{Object of class \code{"character"}, unique identifier for service provider organization.}
    \item{\code{providerSite}:}{Object of class \code{"character"}, reference to the most relevant web site of a service provider.}
    \item{\code{serviceContact}:}{Object of class \code{"XMLAbstractNode"}, information for contacting service provider.}
  }

}

\section{Extends}{
% TODO explain class hierarchy

\bold{OwsCapabilities_1.1.0:}
Class \code{"\linkS4class{OwsCapabilities}"}, directly.

\bold{OwsCapabilities_2.0.0:}
Class \code{"\linkS4class{OwsCapabilities_1.1.0}"}, directly.
Class \code{"\linkS4class{OwsCapabilities}"}, by class "OwsCapabilities_1.1.0", distance 2.

\bold{OwsGetCapabilities_1.1.0}
Class \code{"\linkS4class{OwsGetCapabilities}"}, directly.
Class \code{"\linkS4class{OwsServiceOperation}"}, by class "OwsGetCapabilities", distance 2.

\bold{OwsGetCapabilities_2.0.0:}
Class \code{"\linkS4class{OwsGetCapabilities_1.1.0}"}, directly.
Class \code{"\linkS4class{OwsGetCapabilities}"}, by class "OwsGetCapabilities_1.1.0", distance 2.
Class \code{"\linkS4class{OwsServiceOperation}"}, by class "OwsGetCapabilities_1.1.0", distance 3.

\bold{OwsServiceIdentification:}
Class \code{"\linkS4class{OwsServiceIdentificationOrNULL}"}, directly.

\bold{OwsServiceProvider:}
Class \code{"\linkS4class{OwsServiceProviderOrNULL}"}, directly.

}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "<NAME OF CLASS>")}: Shows a human readable version of the object.}
    \item{checkRequest}{\code{signature(service = "SOS", operation = "OwsGetCapabilities_1.1.0", verbose = "logical")} or \code{signature(service = "SOS", operation = "OwsGetCapabilities_2.0.0", verbose = "logical")}: Check the given operation for validity and for compliance with the metadata of the given SOS.}
    \item{encodeRequestKVP}{\code{signature(obj = "OwsGetCapabilities")}: See \code{link{encodeRequestKVP}}.}
    \item{encodeRequestSOAP}{\code{signature(obj = "OwsGetCapabilities")}: See \code{link{encodeRequestSOAP}}.}
    \item{encodeRequestXML}{\code{signature(obj = "OwsGetCapabilities")}: See \code{link{encodeRequestXML}}.}
    \item{encodeRequestKVP}{\code{signature(obj = "OwsGetCapabilities_2.0.0")}: See \code{link{encodeRequestKVP}}.}
    \item{encodeRequestKVP}{\code{signature(obj = "OwsGetCapabilities_1.1.0")}: See \code{link{encodeRequestKVP}}.}
    \item{sosRequest}{\code{signature(sos = "SOS", request = "OwsServiceOperation", verbose = "logical", inspect = "logical")}: Send the given operation as a request to the given SOS.}
	 }
}

\arguments{
  \item{abstract}{Brief narrative description of this server, normally available for display to a human.}
  \item{acceptFormats}{Unordered character vector of zero or more response formats desired by client, with preferred formats listed first.}
  \item{acceptLanguages}{Unordered character vector of zero or more languages desired by client, with preferred formats listed first. Only OWS 2.0.0!}
  \item{acceptVersions}{Comma-separated prioritized sequence of one or more specification versions accepted by client, with preferred versions listed first.}
  \item{accessConstraints}{Access constraints that should be observed to assure the protection of privacy or intellectual property, and any other restrictions on retrieving or using data from or otherwise using a server.}
  \item{constraints}{Constraint on valid domain of a non-parameter quantity that applies to an operation which a server implements.}
  \item{contents}{The provider section of a capabilities document, object of class \code{OwsContentsOrNULL}.}
  \item{DCPs}{Information for a Distributed Computing Platform (DCP) supported for an operation.}
  \item{exceptionCode}{The code attribute of an OWS Exception, see \code{\link{OwsExceptionsData}}.}
  \item{exceptions}{The list of \code{OwsException} in a \code{OwsExceptionReport}.}
  \item{exceptionText}{The text element of an OWS Exception, see \code{\link{OwsExceptionsData}}.}
  \item{extendedCapabilities}{The possible contents of the ExtendedCapabilities subsection are not specified by the SOS standard.}
  \item{fees}{Fees and terms for using a server, including the monetary units as specified in ISO 4217.}
  \item{identification}{The identification section of a capabilities document, object of class \code{OwsServiceIdentificationOrNULL}.}
  \item{keywords}{Unordered list of one or more commonly used or formalised word(s) or phrase(s) used to describe a server.}
  \item{lang}{The code attribute of an OWS Exception.}
  \item{languages}{The languages section of a capabilities document, currently an object of class \code{XMLAbstractNode}.}
  \item{locator}{The locator attribute of an OWS Exception, see \code{\link{OwsExceptionsData}}.}
  \item{maximumValue}{Maximum value of a range (numeric parameter).}
  \item{metadata}{Metadata about an operation and its implementation.}
  \item{minimumValue}{Minimum value of a range (numeric parameter).}
  \item{name}{Name of an operation (request) (for example, GetCapabilities).}
  \item{operations}{A list of objects of class \code{OwsOperation} in a \code{OperationsMetadata} object. The provider section of a capabilities document.}
  \item{owsVersion}{The used OWS specification version.}
  \item{parameters}{Parameter valid domain that applies to an operation which a server implements.}
  \item{profile}{Identifier of OGC Web Service (OWS) Application Profile.}
  \item{providerName}{Unique identifier for service provider organization.}
  \item{providerSite}{Reference to the most relevant web site of a service provider.}
  \item{provider}{The provider section of a capabilities document, object of class \code{OwsServiceProviderOrNULL}.}
  \item{rangeClosure}{Specifies which of minimum and maximum values are included in this range; include when not default of \dQuote{closed} range. Possible values are closed, open, open-closed, or closed-open.}
  \item{sections}{Unordered character vector of zero or more names of sections of service metadata document to be returned in service metadata document.}
  \item{serviceContact}{Information for contacting service provider.}
  \item{service}{Service type identifier text.}
  \item{serviceType}{A service type name from registry of services.}
  \item{serviceTypeVersion}{Version of a service type implemented by a server.}
  \item{spacing}{Regular distance or spacing between allowed values in this range; include when range is not continuous.}
  \item{title}{Title of a server, normally used for display to a human.}
  \item{updateSequence}{Service metadata document version, value is “increased” whenever any change is made in complete service metadata document. This can be used to request a certain version of a metadata document. Parameter is found in both request and reponse, but may not be supported by a service.}
  \item{version}{The version of the document.}
}


\details{
\code{OwsServiceOperation} is the top class which is eventually put into the request method, \code{sosRequest(...)}.

Classes ending in \verb{...OrNULL} are used to model optional slots.
}

\references{
Whiteside A. (Ed.), OGC Web Services Common Specification, Open Geospatial Consortium Inc., OGC 06-121r3, Version: 1.1.0 with Corrigendum 1

Whiteside A., Greenwood, J. (Eds.), OGC Web Services Common Standard, Open Geospatial Consortium Inc., OGC 06-121r9, Version: 2.0.0
}

\author{Daniel Nüst \email{daniel.nuest@uni-muenster.de}}

%\note{  }
%\section{Warning }{}
%\seealso{}

\examples{
showClass("OwsCapabilities_1.1.0")
showClass("OwsCapabilities_2.0.0")
showClass("OwsCapabilities")
showClass("OwsContents")
showClass("OwsContentsOrNULL")
showClass("OwsException")
showClass("OwsExceptionReport")
showClass("OwsGetCapabilities_1.1.0")
showClass("OwsGetCapabilities_2.0.0")
showClass("OwsGetCapabilities")
showClass("OwsOperation")
showClass("OwsOperationsMetadata")
showClass("OwsRange")
showClass("OwsServiceIdentification")
showClass("OwsServiceIdentificationOrNULL")
showClass("OwsServiceOperation")
showClass("OwsServiceProvider")
showClass("OwsServiceProviderOrNULL")

# TBD examples for construction functions

}

\keyword{classes}
\keyword{utilities}
