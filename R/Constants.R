################################################################################
# Copyright (C) 2015 by 52 North                                               #
# Initiative for Geospatial Open Source Software GmbH                          #
#                                                                              #
# Contact: Andreas Wytzisk                                                     #
# 52 North Initiative for Geospatial Open Source Software GmbH                 #
# Martin-Luther-King-Weg 24                                                    #
# 48155 Muenster, Germany                                                      #
# info@52north.org                                                             #
#                                                                              #
# This program is free software; you can redistribute and/or modify it under   #
# the terms of the GNU General Public License version 2 as published by the    #
# Free Software Foundation.                                                    #
#                                                                              #
# This program is distributed WITHOUT ANY WARRANTY; even without the implied   #
# WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU #
# General Public License for more details.                                     #
#                                                                              #
# You should have received a copy of the GNU General Public License along with #
# this program (see gpl-2.0.txt). If not, write to the Free Software           #
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or #
# visit the Free Software Foundation web page, http://www.fsf.org.             #
#                                                                              #
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)                          #
# Created: 2010-06-18                                                          #
# Project: sos4R - visit the project web page: https://github.com/52North/sos4R #
#                                                                              #
################################################################################

#
# SOS ----
#
sos100_version <- "1.0.0"
sosService <- "SOS"
sos100NamespacePrefix <- "sos"
sosIntendedApplicationName <- paste0(sos100NamespacePrefix, ":intendedApplication")
sosTimeName <- paste0(sos100NamespacePrefix, ":time")
sosProcedureName <- paste0(sos100NamespacePrefix, ":procedure")
sosObservedPropertyName <- paste0(sos100NamespacePrefix, ":observedProperty")
sosFeatureOfInterestName <- paste0(sos100NamespacePrefix, ":featureOfInterest")
sosResultModelName <- paste0(sos100NamespacePrefix, ":resultModel")
sosResponseFormatName <- paste0(sos100NamespacePrefix, ":responseFormat")
sosResponseModeName <- paste0(sos100NamespacePrefix, ":responseMode")
sosObservationOfferingName <- paste0(sos100NamespacePrefix, ":ObservationOffering")
sosObservationOfferingListName <- paste0(sos100NamespacePrefix, ":ObservationOfferingList")
sosContentsName <- paste0(sos100NamespacePrefix, ":Contents")
sosFilterCapabilitiesName <- paste0(sos100NamespacePrefix, ":Filter_Capabilities")
sosCapabilitiesName <- paste0(sos100NamespacePrefix, ":Capabilities")
sosEventTimeName <- paste0(sos100NamespacePrefix, ":eventTime")
sosEventTimeLatestValue <- "latest"
sosObjectIDName <- paste0(sos100NamespacePrefix, ":ObjectID")
sosResultName <- paste0(sos100NamespacePrefix, ":result")
sosResultTimeName <- paste0(sos100NamespacePrefix, ":resultTime")
sosPhenomenonTimeName <- paste0(sos100NamespacePrefix, ":phenomenonTime")
sosObservationTypeName <- paste0(sos100NamespacePrefix, ":observationType")
sosFeatureOfInterestTypeName <- paste0(sos100NamespacePrefix, ":featureOfInterestType")
sosProcedureDescriptionFormat <- paste0(sos100NamespacePrefix, ":procedureDescriptionFormat")
sosObservedAreaName <- paste0(sos100NamespacePrefix, ":observedArea")

                              #
# SOS v2.0 ----
#
sos200_version <- "2.0.0"
sos200NamespacePrefix <- "sos20"
sos200ContentsName <- "contents"
sos200FilterCapabilitiesName = "filterCapabilities"
sos200_emptyGetObservationResponseString <-
  paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
         "<sos:GetObservationResponse xmlns:sos=\"http://www.opengis.net/sos/2.0\" ",
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" ",
         "xsi:schemaLocation=\"http://www.opengis.net/sos/2.0 http://schemas.opengis.net/sos/2.0/sosGetObservation.xsd\"/>")
#
# Core Operations Profile ----
#
sosGetCapabilitiesName <- "GetCapabilities"
sosDescribeSensorName <- "DescribeSensor"
sosGetObservationName <- "GetObservation"
sosGetObservationResponseName <- "GetObservationResponse"
#
# Transaction Operations Profile ----
#
sosRegisterSensorName <- "RegisterSensor"
sosInsertObservationName <- "InsertObservation"
#
# Enhanced Operations Profile ----
#
sosGetObservationByIdName <- "GetObservationById"
sosGetResultName <- "GetResult"
sosGetFeatureOfInterestName <- "GetFeatureOfInterest"
sosGetFeatureOfInterestTimeName <- "GetFeatureOfInterestTime"
sosDescribeFeatureTypeName <- "DescribeFeatureType"
sosDescribeObservationTypeName <- "DescribeObservationType"
sosDescribeResultModelName <- "DescribeResultModel"
sosGetFeatureOfInterestResponseName <- "GetFeatureOfInterestResponse"
#
# Hydrology Profile ----
#
sosGetDataAvailabilityName <- "GetDataAvailability"
sosGetDataAvailabilityResponse <- "GetDataAvailabilityResponse"
sosGDAMemberName <- "dataAvailabilityMember"

SosSupportedOperations <- function() {
  .supported <- c(sosGetCapabilitiesName, sosDescribeSensorName,
                  sosGetObservationName ,sosGetObservationByIdName)
  return(.supported)
}

#
# KVP parameter names ----
#
sosKVPParamNameRequest <- "request"
sosKVPParamNameService <- "service"
sosKVPParamNameVersion <- "version"
sosKVPParamNameOffering <- "offering"
sosKVPParamNameObsProp <- "observedProperty"
sosKVPParamNameFoi <- "featureOfInterest"
sosKVPParamNameResponseFormat <- "responseFormat"
sosKVPParamNameSrsName <- "srsName"
sosKVPParamNameEventTime <- "eventTime"
sosKVPParamNameProcedure <- "procedure"
sosKVPParamNameResultModel <- "resultModel"
sosKVPParamNameResponseMode <- "responseMode"
sosKVPParamNameBBOX <- "BBOX"

#
# not exported SOS ----
#
.sosConnectionMethodGet_Deprecated <- "GET"
.sosBindingKVP <- "KVP"
.sosConnectionMethodPost_Deprecated <- "POST"
.sosBindingPOX <- "POX"
.sosBindingSOAP <- "SOAP"

SosSupportedBindings <- function() {
  .supported <- c(.sosConnectionMethodGet_Deprecated,
                  .sosConnectionMethodPost_Deprecated,
                  .sosBindingKVP,
                  .sosBindingPOX)
  names(.supported) <- c("DEPRECATED", "DEPRECATED", "Key-value-pair (GET)",
                         "Plain old XML (POST)")
  return(.supported)
}

mimeTypeCSV <- "text/csv"
mimeTypeXML <- "text/xml"
mimeTypeOM <- 'text/xml;subtype="om/1.0.0"'
mimeTypeSML <- 'text/xml;subtype="sensorML/1.0.1"'
mimeTypeKML <- "application/vnd.google-earth.kml+xml"
mimeSubtypeOM <- '"om/1.0.0"'

.sosSupportedResponseFormats <- c(
  mimeTypeOM,
  mimeTypeSML,
  mimeTypeCSV,
  mimeTypeKML)
SosSupportedResponseFormats <- function() {
  return(.sosSupportedResponseFormats)
}

.sosSupportedResultModels <- c("om:Measurement", "om:Observation")
SosSupportedResultModels <- function() {
  return(.sosSupportedResultModels)
}

.sosSupportedResponseModes <- c("inline")
SosSupportedResponseModes <- function() {
  return(.sosSupportedResponseModes)
}

.sosSupportedServiceVersions <- c(sos100_version, sos200_version)
SosSupportedServiceVersions <- function() {
  return(.sosSupportedServiceVersions)
}

#
# Namespaces ----
#
sos100Namespace <- "http://www.opengis.net/sos/1.0"
xsiNamespace <- "http://www.w3.org/2001/XMLSchema-instance"
owsNamespace <- "http://www.opengis.net/ows/1.1"
ogcNamespace <- "http://www.opengis.net/ogc"
gmlNamespace <- "http://www.opengis.net/gml"
gml32Namespace <- "http://www.opengis.net/gml/3.2"
smlNamespace <- "http://www.opengis.net/sensorML/1.0.1"
sweNamespace <- "http://www.opengis.net/swe/1.0.1"
xlinkNamespace <- "http://www.w3.org/1999/xlink"
.sos100_NamespaceDefinitionsGetObs <- c(ows = "http://www.opengis.net/ows/1.1",
                                        om = "http://www.opengis.net/om/1.0",
                                        ogc = "http://www.opengis.net/ogc",
                                        gml = "http://www.opengis.net/gml")
.sos20_NamespaceDefinitions <- c(sams = "http://www.opengis.net/samplingSpatial/2.0",
                                 sf = "http://www.opengis.net/sampling/2.0",
                                 swes = "http://www.opengis.net/swes/2.0",
                                 om20 = "http://www.opengis.net/om/2.0",
                                 sos = "http://www.opengis.net/sos/2.0")

sos200Namespace = "http://www.opengis.net/sos/2.0"

SosAllNamespaces <- function(version = sos100_version) {
  if (version == sos100_version) {
    .all <- c(sos = sos100Namespace,
              xsi = xsiNamespace,
              sos4R:::.sos100_NamespaceDefinitionsGetObs,
              ows = owsNamespace,
              ogc = ogcNamespace,
              sml = smlNamespace,
              swe = sweNamespace,
              xlink = xlinkNamespace,
              gml = gmlNamespace)
    return(.all[unique(names(.all))])
    } else if (version == sos200_version) {
      .all <- c(sos4R:::.sos20_NamespaceDefinitions)
      return(.all[unique(names(.all))])
  } else {
    stop("Unsupported version", version)
  }
}

.sos100_xsiSchemaLocationAttribute <- c("xsi:schemaLocation" = "http://www.opengis.net/sos/1.0 http://schemas.opengis.net/sos/1.0.0/sosAll.xsd")
.sos20_xsiSchemaLocationAttribute <- c("xsi:schemaLocation" = "http://www.opengis.net/sos/2.0 http://schemas.opengis.net/sos/2.0/sos.xsd")

#
# O&M ----
#
omNamespacePrefix <- "om"
omMeasurementName <- paste0(omNamespacePrefix, ":Measurement")
omMemberName <- paste0(omNamespacePrefix, ":member")
omObservationName <- paste0(omNamespacePrefix, ":Observation")
omObservationCollectionName <- paste0(omNamespacePrefix, ":ObservationCollection")
omFeatureOfInterestName <- paste0(omNamespacePrefix, ":featureOfInterest")
omProcedureName <- paste0(omNamespacePrefix, ":procedure")
omObservedPropertyName <- paste0(omNamespacePrefix, ":observedProperty")
omSamplingTimeName <- paste0(omNamespacePrefix, ":samplingTime")
omResultTimeName <- paste0(omNamespacePrefix, ":resultTime")
omResultName <- paste0(omNamespacePrefix, ":result")
omCategoryObservationName <- paste0(omNamespacePrefix, ":CategoryObservation")
omCountObservationName <- paste0(omNamespacePrefix, ":CountObservation")
omTruthObservationName <- paste0(omNamespacePrefix, ":TruthObservation")
omGeometryObservationName <- paste0(omNamespacePrefix, ":GeometryObservation")
omTemporalObservationName <- paste0(omNamespacePrefix, ":TemporalObservation")
omComplexObservationName <- paste0(omNamespacePrefix, ":ComplexObservation")

#
# O&M 2.0 ----
#
om20NamespacePrefix <- "om20"
om20OM_Observation <- paste0(om20NamespacePrefix, ":OM_Observation")
om20ResultTypeAttributeName <- paste0(om20NamespacePrefix, ":type")
om20ResultMeasureTypeName <- paste0(om20NamespacePrefix, ":MeasureType")
om20PhenomenonTimeName <- paste0(om20NamespacePrefix, ":phenomenonTime")

#
# SA ----
#
saNamespacePrefix <- "sa"
saSamplingPointName <- paste0(saNamespacePrefix, ":SamplingPoint")
saSamplingSurface <- paste0(saNamespacePrefix, ":SamplingSurface")
saPositionName <- paste0(saNamespacePrefix, ":position")
saSampledFeatureName <- paste0(saNamespacePrefix, ":sampledFeature")
saSamplingTimeName <- paste0(saNamespacePrefix, ":samplingTime")

#
# SAMS ----
#
samsNamespacePrefix <- "sams"
samsShapeName <- paste0(samsNamespacePrefix, ":shape")
samsSamplingFeatureName <- paste0(samsNamespacePrefix, ":SF_SpatialSamplingFeature")

#
# SF ----
#
sfNamespacePrefix <- "sf"
sfTypeName <- paste0(sfNamespacePrefix, ":type")
sfSampledFeatureName <- paste0(sfNamespacePrefix, ":sampledFeature")

#
# GML ----
#
gmlNamespacePrefix <- "gml"
gmlPosName <- paste0(gmlNamespacePrefix, ":pos")
gmlPointName <- paste0(gmlNamespacePrefix, ":Point")
gmlTimeInstantName <- paste0(gmlNamespacePrefix, ":TimeInstant")
gmlTimePeriodName <- paste0(gmlNamespacePrefix, ":TimePeriod")
gmlTimePositionName <- paste0(gmlNamespacePrefix, ":timePosition")
gmlRelatedTimeName <- paste0(gmlNamespacePrefix, ":relatedTime")
gmlNameName <- paste0(gmlNamespacePrefix, ":name")
gmlDescriptionName <- paste0(gmlNamespacePrefix, ":description")
gmlBeginName <- paste0(gmlNamespacePrefix, ":begin")
gmlEndName <- paste0(gmlNamespacePrefix, ":end")
gmlBeginPositionName <- paste0(gmlNamespacePrefix, ":beginPosition")
gmlEndPositionName <- paste0(gmlNamespacePrefix, ":endPosition")
gmlFeatureCollectionName <- paste0(gmlNamespacePrefix, ":FeatureCollection")
gmlBoundedByName <- paste0(gmlNamespacePrefix, ":boundedBy")
gmlEnvelopeName <- paste0(gmlNamespacePrefix, ":Envelope")
gmlLowerCornerName <- paste0(gmlNamespacePrefix, ":lowerCorner")
gmlUpperCornerName <- paste0(gmlNamespacePrefix, ":upperCorner")
gmlTimeLengthName <- paste0(gmlNamespacePrefix, ":timeLength")
gmlDurationName <- paste0(gmlNamespacePrefix, ":duration")
gmlTimeIntervalName <- paste0(gmlNamespacePrefix, ":timeInterval")
gmlFeatureMemberName <- paste0(gmlNamespacePrefix, ":featureMember")
gmlIdentifierName <- paste0(gmlNamespacePrefix, ":identifier")

#
# SWE ----
#
sweNamespacePrefix <- "swe"
sweCompositePhenomenonName <- paste0(sweNamespacePrefix, ":CompositePhenomenon")
sweBaseName <- paste0(sweNamespacePrefix, ":base")
sweComponentName <- paste0(sweNamespacePrefix, ":component")
sweDataArrayName <- paste0(sweNamespacePrefix, ":DataArray")
sweElementTypeName <- paste0(sweNamespacePrefix, ":elementType")
sweSimpleDataRecordName <- paste0(sweNamespacePrefix, ":SimpleDataRecord")
sweDataRecordName <- paste0(sweNamespacePrefix, ":DataRecord")
sweFieldName <- paste0(sweNamespacePrefix, ":field")
sweTimeName <- paste0(sweNamespacePrefix, ":Time")
sweQuantityName <- paste0(sweNamespacePrefix, ":Quantity")
sweCategoryName <- paste0(sweNamespacePrefix, ":Category")
sweBooleanName <- paste0(sweNamespacePrefix, ":Boolean")
sweCountName <- paste0(sweNamespacePrefix, ":Count")
sweEncodingName <- paste0(sweNamespacePrefix, ":encoding")
sweTextBlockName <- paste0(sweNamespacePrefix, ":TextBlock")
sweTextEncodingName <- paste0(sweNamespacePrefix, ":TextEncoding")
sweValuesName <- paste0(sweNamespacePrefix, ":values")
sweValueName <- paste0(sweNamespacePrefix, ":value")
sweCodeSpaceName <- paste0(sweNamespacePrefix, ":codeSpace")
sweTextName <- paste0(sweNamespacePrefix, ":Text")
sweUomName <- paste0(sweNamespacePrefix, ":uom")
sweVectorName <- paste0(sweNamespacePrefix, ":Vector")
sweLocationName <- paste0(sweNamespacePrefix, ":location")
sweCoordinateName <- paste0(sweNamespacePrefix, ":coordinate")
swePositionName <- paste0(sweNamespacePrefix, ":Position")
sweLowerCornerName <- paste0(sweNamespacePrefix, ":upperCorner")
sweUpperCornerName <- paste0(sweNamespacePrefix, ":lowerCorner")

#
# SWE Service Model ----
#
swesNamespacePrefix = "swes"
swesOfferingName = paste0(swesNamespacePrefix, ":offering")
swesIdentifierName = paste0(swesNamespacePrefix, ":identifier")
swesNameName = paste0(swesNamespacePrefix, ":name")
swesObservablePropertyName = paste0(swesNamespacePrefix, ":observableProperty")
swesProcedureName = paste0(swesNamespacePrefix, ":procedure")
swesProcedureDescriptionFormatName = paste0(swesNamespacePrefix, ":procedureDescriptionFormat")

#
# WML 2.0 ----
#
wmlMonitoringPointName = "wml:MonitoringPoint"

#
# OGC ----
#
ogcTempOpTMAfterName <- "TM_After"
ogcTempOpTMBeforeName <- "TM_Before"
ogcTempOpTMBeginsName <- "TM_Begins"
ogcTempOpTMBegunByName <- "TM_BegunBy"
ogcTempOpTMContainsName <- "TM_Contains"
ogcTempOpTMDuringName <- "TM_During"
ogcTempOpTMEndedByName <- "TM_EndedBy"
ogcTempOpTMEndsName <- "TM_Ends"
ogcTempOpTMEqualsName <- "TM_Equals"
ogcTempOpTMMeetsName <- "TM_Meets"
ogcTempOpTMMetByName <- "TM_MetBy"
ogcTempOpTMOverlapsName <- "TM_Overalps"
ogcTempOpTMOverlappedBy <- "TM_OverlappedBy"
.ogcSupportedTemporalOps <- list(
  ogcTempOpTMAfterName,
  ogcTempOpTMBeforeName,
  ogcTempOpTMDuringName,
  ogcTempOpTMEqualsName
)
names(.ogcSupportedTemporalOps) <- .ogcSupportedTemporalOps
SosSupportedTemporalOperators <- function() {
  return(.ogcSupportedTemporalOps)
}

ogcNamespacePrefix <- "ogc"
ogcSpatialOpBBOXName <- "BBOX"
ogcSpatialOpContainsName <- "Contains"
ogcSpatialOpIntersectsName <- "Intersects"
ogcSpatialOpOverlapsName <- "Overlaps"
ogcSpatialOpBeyondName <- "Beyond"
ogcSpatialOpCrossesName <- "Crosses"
ogcSpatialOpDWithinName <- "DWithin"
ogcSpatialOpDisjointName <- "Disjoint"
ogcSpatialOpEqualsName <- "Equals"
ogcSpatialOpTouchesName <- "Touches"
ogcSpatialOpWithinName <- "Within"
.ogcSupportedSpatialOps <- list(
  ogcSpatialOpBBOXName,
  ogcSpatialOpContainsName,
  ogcSpatialOpIntersectsName,
  ogcSpatialOpOverlapsName
)
names(.ogcSupportedSpatialOps) <- .ogcSupportedSpatialOps
SosSupportedSpatialOperators <- function() {
  return(.ogcSupportedSpatialOps)
}

ogcGeometryOperandEnvelopeName <- "gml:Envelope"
ogcGeometryOperandPolygonName <- "gml:Polygon"
ogcGeometryOperandPointName <- "gml:Point"
ogcGeometryOperandLineStringName <- "gml:LineString"

.ogcSupportedGeometryOperands <- list(
  ogcGeometryOperandEnvelopeName,
  ogcGeometryOperandPolygonName,
  ogcGeometryOperandPointName,
  ogcGeometryOperandLineStringName
)
names(.ogcSupportedGeometryOperands) <- .ogcSupportedGeometryOperands
SosSupportedGeometryOperands <- function() {
  return(.ogcSupportedGeometryOperands)
}

ogcComparisonOpBetweenName <- "PropertyIsBetween"
ogcComparisonOpEqualToName <- "PropertyIsEqualTo"
ogcComparisonOpGreaterThanName <- "PropertyIsGreaterThan"
ogcComparisonOpGreaterThanOrEqualToName <- "PropertyIsGreaterThanOrEqualTo"
ogcComparisonOpLessThenName <- "PropertyIsLessThan"
ogcComparisonOpLessThanOrEqualToName <- "PropertyIsLessThanOrEqualTo"
ogcComparisonOpIsLikeName <- "PropertyIsLike"
ogcComparisonOpIsNotEqualTo <- "PropertyIsNotEqualTo"
ogcComparisonOpIsNull <- "PropertyIsNull"
.ogcSupportedComparisonOperators <- list()
names(.ogcSupportedComparisonOperators) <- .ogcSupportedComparisonOperators
SosSupportedComparisonOperators <- function() {
  return(.ogcSupportedComparisonOperators)
}

ogcPropertyNameName <- paste0(ogcNamespacePrefix, ":PropertyName")
ogcBBOXName <- paste0(ogcNamespacePrefix, ":BBOX")
ogcContainsName <- paste0(ogcNamespacePrefix, ":Contains")
ogcIntersectsName <- paste0(ogcNamespacePrefix, ":Intersects")
ogcOverlapsName <- paste0(ogcNamespacePrefix, ":Overlaps")
ogcSpatialCapabilitiesName <- paste0(ogcNamespacePrefix, ":Spatial_Capabilities")
ogcTemporalCapabilitiesName <- paste0(ogcNamespacePrefix, ":Temporal_Capabilities")
ogcScalarCapabilitiesName <- paste0(ogcNamespacePrefix, ":Scalar_Capabilities")
ogcIdCapabilities <- paste0(ogcNamespacePrefix, ":Id_Capabilities")
ogcGeometryOperandsName <- paste0(ogcNamespacePrefix, ":GeometryOperands")
ogcGeometryOperandName <- paste0(ogcNamespacePrefix, ":GeometryOperand")
ogcSpatialOperatorsName <- paste0(ogcNamespacePrefix, ":SpatialOperators")
ogcSpatialOperatorName <- paste0(ogcNamespacePrefix, ":SpatialOperator")
ogcTemporalOperandsName <- paste0(ogcNamespacePrefix, ":TemporalOperands")
ogcTemporalOperandName <- paste0(ogcNamespacePrefix, ":TemporalOperand")
ogcTemporalOperatorsName <- paste0(ogcNamespacePrefix, ":TemporalOperators")
ogcTemporalOperatorName <- paste0(ogcNamespacePrefix, ":TemporalOperator")
ogcLogicalOperatorsName <- paste0(ogcNamespacePrefix, ":LogicalOperators")
ogcComparisonOperatorsName <- paste0(ogcNamespacePrefix, ":ComparisonOperators")
ogcArithmeticOperatorsName <- paste0(ogcNamespacePrefix, ":ArithmeticOperators")
ogcEIDName <- paste0(ogcNamespacePrefix, ":EID")
ogcFIDName <- paste0(ogcNamespacePrefix, ":FID")
ogcLiteralName <- paste0(ogcNamespacePrefix, ":Literal")

smlSensorMLName <- "sml:SensorML"

#
# OWS ----
#
owsNamespacePrefix <- "ows"
owsServiceIdentificationName <- paste0(owsNamespacePrefix, ":ServiceIdentification")
owsTitleName <- paste0(owsNamespacePrefix, ":Title")
owsAbstractName <- paste0(owsNamespacePrefix, ":Abstract")
owsAcceptVersionsName <- paste0(owsNamespacePrefix, ":AcceptVersions")
owsKeywordsName <- paste0(owsNamespacePrefix, ":Keywords")
owsKeywordName <- paste0(owsNamespacePrefix, ":Keyword")
owsServiceTypeName <- paste0(owsNamespacePrefix, ":ServiceType")
owsServiceTypeVersionName <- paste0(owsNamespacePrefix, ":ServiceTypeVersion")
owsFeesName <- paste0(owsNamespacePrefix, ":Fees")
owsAccessConstraintsName <- paste0(owsNamespacePrefix, ":AccessConstraints")
owsServiceProviderName <- paste0(owsNamespacePrefix, ":ServiceProvider")
owsOperationsMetadataName <- paste0(owsNamespacePrefix, ":OperationsMetadata")
owsOperationName <- paste0(owsNamespacePrefix, ":Operation")
owsDCPName <- paste0(owsNamespacePrefix, ":DCP")
owsHTTPName <- paste0(owsNamespacePrefix, ":HTTP")
owsGetName <- paste0(owsNamespacePrefix, ":Get")
owsPostName <- paste0(owsNamespacePrefix, ":Post")
owsParameterName <- paste0(owsNamespacePrefix, ":Parameter")
owsAllowedValuesName <- paste0(owsNamespacePrefix, ":AllowedValues")
owsValueName <- paste0(owsNamespacePrefix, ":Value")
owsAnyValueName <- paste0(owsNamespacePrefix, ":AnyValue")
owsRangeName <- paste0(owsNamespacePrefix, ":Range")
owsMinimumValueName <- paste0(owsNamespacePrefix, ":MinimumValue")
owsMaximumValueName <- paste0(owsNamespacePrefix, ":MaximumValue")
owsSpacingName <- paste0(owsNamespacePrefix, ":Spacing")
owsConstraintName <- paste0(owsNamespacePrefix, ":Constraint")
owsMetadataName <- paste0(owsNamespacePrefix, ":Metadata")
owsExceptionReportName <- paste0(owsNamespacePrefix, ":ExceptionReport")
owsExceptionName <- paste0(owsNamespacePrefix, ":Exception")
owsExceptionTextName <- paste0(owsNamespacePrefix, ":ExceptionText")
owsProfileName <- paste0(owsNamespacePrefix, ":Profile")
owsProviderNameName <- paste0(owsNamespacePrefix, ":ProviderName")
owsProviderSiteName <- paste0(owsNamespacePrefix, ":ProviderSite")
owsServiceContactName <- paste0(owsNamespacePrefix, ":ServiceContact")
owsVersionName <- paste0(owsNamespacePrefix, ":Version")
owsSectionsName <- paste0(owsNamespacePrefix, ":Sections")
owsSectionName <- paste0(owsNamespacePrefix, ":Section")
owsAcceptFormatsName <- paste0(owsNamespacePrefix, ":AcceptFormats")
owsOutputFormatName <- paste0(owsNamespacePrefix, ":OutputFormat")
owsAcceptLanguagesName <- paste0(owsNamespacePrefix, ":AcceptLanguages")
owsLanguageName <- paste0(owsNamespacePrefix, ":Language")

#
# KML ----
#
kmlName <- "kml"

#
# OWS exception details ----
#
.owsCodes = c(
  "OperationNotSupported",
  "MissingParameterValue",
  "InvalidParameterValue",
  "VersionNegotiationFailed",
  "InvalidUpdateSequence",
  "OptionNotSupported",
  "NoApplicableCode")
.owsCodeMeanings = c(
  "Request is for an operation that is not supported by this server",
  "Operation request does not include a parameter value, and this server did not declare a default parameter value for that parameter",
  "Operation request contains an invalid parameter value",
  "List of versions in 'AcceptVersions' parameter value in GetCapabilities operation request did not include any version supported by this server",
  "Value of (optional) updateSequence parameter in GetCapabilities operation request is greater than current value of service metadata updateSequence number",
  "Request is for an option that is not supported by this server",
  "No other exceptionCode specified by this service and server applies to this exception")
.owsCodeLocators = c(
  "Name of operation not supported",
  "Name of missing parameter",
  "Name of parameter with invalid value",
  "None, omit 'locator' parameter",
  "None, omit 'locator' parameter",
  "Identifier of option not supported",
  "None, omit 'locator' parameter")
.httpCode = c("501", "400", "400", "400", "400", "501", "3xx, 4xx, 5xx")
.httpMessage = c("Not Implemented", "Bad request", "Bad request", "Bad request",
                 "Bad request", "Not implemented", "Internal Server Error")

.owsStandardExceptions <- data.frame(
  exceptionCode = .owsCodes,
  meaningOfCode = .owsCodeMeanings,
  locator = .owsCodeLocators,
  httpStatusCode = .httpCode,
  httpMessage = .httpMessage,
  check.rows = TRUE, check.names = TRUE)
OwsExceptionsData <- function() {
  return(.owsStandardExceptions)
}

#
# others ----
#
xmlTextNodeName <- "text"

.sosCheatSheetDocumentName <- "sos4r_cheat-sheet.pdf"
sosAttributeFileName <- "savedAsFile"
