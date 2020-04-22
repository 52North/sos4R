\name{Coercion}

\alias{coerce}
\alias{coerce-sos4R}

\alias{as.SpatialPointsDataFrame.OmMeasurement}
\alias{as.SpatialPointsDataFrame.OmObservation}
\alias{as.SpatialPointsDataFrame.OmObservationCollection}

\alias{as.SpatialPointsDataFrame.SensorML}

\alias{as.SpatialPoints.GmlDirectPosition}
\alias{as.SpatialPolygons.GmlEnvelope}
\alias{as.SpatialPoints.GmlFeatureProperty}
\alias{as.SpatialPoints.GmlPoint}
\alias{as.SpatialPoints.GmlPointProperty}

\alias{as.SpatialPoints.WmlMonitoringPoint}

\alias{as.SpatialPoints.SaSamplingPoint}

\alias{as.SpatialPoints.SamsSamplingFeature}
\alias{as.SpatialPoints.SamsShape}

\alias{as.SpatialPolygons.SosObservationOffering}

\alias{as.data.frame.OmMeasurement}
\alias{as.data.frame.OmObservation}

\alias{as.data.frame.WmlMeasurementTimeseries}

\title{Coercion of SOS objects to Spatial classes}

\description{
The package \pkg{sos4R} comes with several classes that contain spatial information, such as coordinates of sensors or observation data associated with coordinates.
For easy access to these coordinates, there are coercion functions to matching classes from the package \pkg{sp}.
}

\details{
Most coercions, e.g. from \code{Gml..} classes, are used internally during parsing and result extraction.

Objects containing data, e.g. \code{OmObservationCollection}, return a \code{SpatialPointsDataFrame}.

Objects not containing data, e.g. \code{SensorML} instances, return an object of class \code{SpatialPoints}.

To just get the most fitting class, coerce to \code{"Spatial"} as shown in the examples.
}

\examples{
# sensor location
mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/kvp",
             binding = "KVP")
mySensor <- describeSensor(sos = mySOS,
		procedure = sosProcedures(mySOS)[[1]],
		outputFormat = 'text/xml; subtype="sensorML/1.0.1"')
as(mySensor, "Spatial")

# offering bounding box
as(sosOfferings(mySOS)[[1]], "Spatial")
}

\keyword{coercion}
