############################################################################## #
# Copyright (C) 2019 by 52 North                                               #
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
# Created: 2010-09-15                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

switchSweNamespace <- function(docNamespaces, sos){
  docNs <- unique(docNamespaces)

  if (swe20Namespace %in% docNs) {
    targetNs <- sos@namespaces[names(sos@namespaces) != sweNamespacePrefix]
    targetNs <- c(targetNs, swe = swe20Namespace)
    return(targetNs)
  }

  return(sos@namespaces)
}

#
# optimized for 52N SOS, that means only options used there in OMEncoder are
# handled here.
#
# For example, swe:elementCount can also have an attribut ref, but this is not
# checked here, and swe:Count is actually a swe:AbstractDataComponentType, but
# here it is just looked for a child element swe:value.
#
parseDataArray <- function(obj, sos, verbose = FALSE) {
  namespaces <- switchSweNamespace(xml2::xml_ns(obj), sos)

  elementCount <- xml2::xml_text(xml2::xml_find_first(x = obj,
                                                      xpath = "./swe:elementCount/swe:Count/swe:value",
                                                      ns = namespaces)
                                  )
  if (verbose) cat("[parseDataArray] Parsing DataArray with", elementCount, "elements.\n")

  elementTypeParser <- sosParsers(sos)[[sweElementTypeName]]
  elementTypeXml <- xml2::xml_child(x = obj, search = sweElementTypeName, ns = namespaces)
  fieldDescriptions <- elementTypeParser(obj = elementTypeXml,
                                sos = sos,
                                verbose = verbose)
  if (verbose) cat("[parseDataArray] Parsed field descriptions:", toString(fieldDescriptions), "\n")

  encodingParser <- sosParsers(sos)[[sweEncodingName]]
  encodingXml <- xml2::xml_child(x = obj,
                                  search = sweEncodingName,
                                  ns = namespaces)
  encoding <- encodingParser(obj = encodingXml, sos = sos, verbose = verbose)

  if (verbose) cat("[parseDataArray] Parsed encoding description:", toString(encoding), "\n")

  valueParser <- sosParsers(sos)[[sweValuesName]]
  parsedValues <- valueParser(values = xml2::xml_child(x = obj,
                                                 search = sweValuesName,
                                                 ns = namespaces),
                        fields = fieldDescriptions,
                        encoding = encoding,
                        sos = sos,
                        verbose = verbose)

  return(parsedValues)
}


#
# values is XML and encoding holds a SweTextBlock with the required separators.
#
parseValues <- function(values, fields, encoding, sos, verbose = FALSE) {
  if (verbose) cat("[parseValues] Parsing swe:values using", toString(encoding), "and",
                  length(fields), "fields:", toString(names(fields)), "\n")
  if (!(inherits(encoding, "SweTextBlock") || inherits(encoding, "SweTextEncoding"))) {
    stop("Handling for given encoding not implemented!")
  }

  converters <- sosDataFieldConverters(sos)

  blocks <- strsplit(x = xml2::xml_text(x = values),
                          split = encoding@blockSeparator)
  tokens <- sapply(blocks, strsplit,
                        split = encoding@tokenSeparator)

  if (verbose)
    cat("[parseValues] Parsing values from lines: ", toString(tokens), "\n")

  # data frame of correct length to be able to use cbind for first column
  tempId = "tempID"
  valuesDataFrame <- data.frame(seq(1,length(tokens)))
  names(valuesDataFrame) <- tempId

  # do following for all fields
  fieldCount <- length(fields)
  for (currentFieldIndex in seq(1,fieldCount)) {
    if (verbose) cat("[parseValues] Processing field index", currentFieldIndex , "of", fieldCount,"\n")

    # create list for each variable
    currentValues <- sapply(tokens, "[[", currentFieldIndex)
    if (verbose)
      cat("[parseValues] Current values: ", toString(currentValues), "\n")
    currentField <- fields[[currentFieldIndex]]

    if (verbose)
      cat("[parseValues] Parsing field", paste(currentField), "\n")

    # convert values to the correct types
    fieldDefinition <- currentField[["definition"]]
    valueConverter <- converters[[fieldDefinition]]

    if (is.null(valueConverter)) {
      # could still be a unit of measurement given, use as
      if (!is.na(currentField["uom"])) {
        valueConverter <- converters[[currentField[["uom"]]]]
      }
      if (is.null(valueConverter)) {
        # fallback option
        # TODO clarify, if the if should be removed or not
        if (verbose) {
          cat(paste0("[parseValues] No converter for values with the definition '",
                         currentField[["definition"]],
                         "'! Trying a default for R class '",
                         currentField[["rClass"]],
                         "' or the default fallback '",
                         "sosConvertDouble", # manually sync with Defaults.R
                         "', but you can add one when creating a SOS using ",
                         "SosDataFieldConvertingFunctions() if the converted ",
                         "values are not as expected.\n"))
        }
        if (is.null(currentField[["rClass"]])) {
          valueConverter <- converters[["fallBack"]]
        } else {
          valueConverter <- converters[[currentField[["rClass"]]]]
        }
      }
    }
    if (verbose) {
      cat("[parseValues] Using converter:\n")
      print(valueConverter)
    }

    # do the conversion
    currentValues <- valueConverter(x = currentValues, sos = sos)

    # bind new and existing data:
    if (verbose) cat("[parseValues] Binding additional data.frame for",
                    currentField[["name"]],
                    "-- value range", toString(range(currentValues)), "\n")
    newValueDataFrame <- data.frame(currentValues, stringsAsFactors = FALSE)

    # create the names of the new data:
    .newDataName <- currentField[["name"]]
    names(newValueDataFrame) <- .cleanupColumnName(.newDataName)

    if (verbose) cat("[parseValues] Added column name:", names(newValueDataFrame), "\n")

    # bind existing and new data column
    valuesDataFrame <- cbind(valuesDataFrame, newValueDataFrame)

    if (verbose) {
      cat("[parseValues] The new bound data frame (one variable the a temp id):\n")
      utils::str(valuesDataFrame)
    }

    # add field information as attributes to the new column using human
    # readable names
    .addAttrs <- as.list(currentField)
    names(.addAttrs) <- .sosParseFieldReadable[names(currentField)]

    .lastColumn <- dim(valuesDataFrame)[[2]]
    .oldAttrs <- attributes(valuesDataFrame[,.lastColumn])

    attributes(valuesDataFrame[,.lastColumn]) <- c(as.list(.oldAttrs), .addAttrs)

    if (verbose) cat("[parseValues] Added attributes to new data:",
                    toString(.addAttrs),
                    "[ names: ", toString(names(.addAttrs)), "]",
                    "\n[parseValues] Old attributes list is",
                    toString(.oldAttrs),
                    "\n[parseValues] New attributes list is",
                    toString(attributes(valuesDataFrame[,.lastColumn])),
                    "\n")
  }

  # remove id column
  if (verbose) cat("[parseValues] Removing temporary first column\n")
  valuesDataFrame <- valuesDataFrame[,!colnames(valuesDataFrame) %in% tempId, drop = FALSE]

  if (verbose) {
    cat("[parseValues] returning final data frame:\n")
    utils::str(valuesDataFrame)
  }
  return(valuesDataFrame)
}

#
# Creates list of named character vectors with the information from swe:field Elements.
#
parseElementType <- function(obj, sos, verbose = FALSE) {
  namespaces <- switchSweNamespace(xml2::xml_ns(obj), sos)

  elementTypeHref <- stringr::str_remove_all(xml2::xml_attr(x = obj, attr = "href"), "#")
  if (verbose) cat("[parseElementType] Got child", xml2::xml_name(xml2::xml_children(obj)),
                   "and id", elementTypeHref, "for object", xml2::xml_name(obj), "\n")

  if (is.na(elementTypeHref)) {
    elementType <- obj
  }
  else {
    root <- xml2::xml_root(obj)
    elementType <- xml2::xml_parent(
      xml2::xml_find_first(x = root, xpath = paste0("//*[@gml:id='", elementTypeHref, "']"))
    )

    if (is.na(elementType)) {
      stop("Got ", sweElementTypeName," with a reference (href) but cannot find definition - cannot parse!",
           toString(obj))
    } else  {
      if (verbose) cat("[parseDataArray] Found elementType via reference", elementTypeHref, "\n")
    }
  }

  simpleDataRecord <- xml2::xml_child(x = elementType, search = sweSimpleDataRecordName, ns = namespaces)
  dataRecord <- xml2::xml_child(x = elementType, search = sweDataRecordName, ns = namespaces)

  if (!is.na(simpleDataRecord) || !is.na(dataRecord)) {
    # pick the existing one
    if (!is.na(simpleDataRecord)) dr <- simpleDataRecord
    else dr <- dataRecord

    fields <- xml2::xml_find_all(x = dr,
                                 xpath = sweFieldName,
                                 ns = namespaces)

    if (verbose) cat("[parseElementType] Got data record with", length(fields), "fields. \n")

    # extract the fields, naming with attribute 'name'
    parsedFields <- lapply(X = fields,
                           FUN = parseField,
                           sos = sos,
                           verbose = verbose)
    names <- sapply(parsedFields, "[", "name")
    names(parsedFields) <- names

    if (verbose) cat("[parseElementType] Names of parsed fields:", names(fields), "\n")

    return(parsedFields)
  }
  else {
    stop(paste("Cannot parse swe:elementType, only children of type",
               sweSimpleDataRecordName, "and", sweDataRecordName,
               "are supported!"))
  }
}

#
# swe:encoding
#
parseEncoding <- function(obj, sos, verbose = FALSE) {
  namespaces <- switchSweNamespace(xml2::xml_ns(obj), sos)

  .textBlock <- xml2::xml_child(x = obj, search = sweTextBlockName, ns = namespaces)

  .textEncoding <- xml2::xml_child(x = obj, search = sweTextEncodingName, ns = namespaces)

  if (!(is.na(.textBlock))) {
    .tb <- parseTextBlock(.textBlock)
    return(.tb)
  }
  else if (!(is.na(.textEncoding))) {
    .tb <- parseTextEncoding(.textEncoding)
    return(.tb)
  }
  else {
    stop(paste("Cannot parse swe:encoding, only", sweTextBlockName, "and", sweTextEncodingName, "are supported!"))
  }
}

#
# human readable versions of field names ----
#
.sosParseFieldReadable <- list(
  "name",
  "definition",
  "unit of measurement",
  "category name",
  "category value",
  "category code space",
  "R class of values")
names(.sosParseFieldReadable) <- list(
  "name",
  "definition",
  "uom",
  "category",
  "value",
  "codeSpace",
  "rClass")

#
# Function creates a named character vector (using field names from variables
# like ".sosParseFieldXYZ") with all information stored in a swe:field.
#
parseField <- function(obj, sos, verbose = FALSE) {
  namespaces <- switchSweNamespace(xml2::xml_ns(obj), sos)

  field <- NULL
  name <- xml2::xml_attr(x = obj, attr = "name", ns = namespaces)
  if (verbose) cat("[parseField] Parsing field description of ", name, "\n")

  innerField <- xml2::xml_child(x = obj)
  innerFieldName <- xml2::xml_name(x = innerField, ns = namespaces)

  # Available options: Time, Text, Quantity, Category, Boolean, Count
  # The parsed elements and fields are closely bound to 52N SOS (OMEncoder.java)
  if (innerFieldName == sweTimeName) {
    def <- xml2::xml_attr(x = innerField, attr = "definition")
    uom <- NA
    #
    # handle special case of no children available
    # see https://github.com/r-lib/xml2/issues/264
    #
    if (length(xml2::xml_children(innerField)) > 0) {
      child <- xml2::xml_child(innerField)
      if (sweUomName == xml2::xml_name(child, ns = namespaces)) {
        uom <- xml2::xml_attr(x = child, attr = "href")
      }
    }
    field <- c(name = name, definition = def, uom = uom, rClass = "POSIXct")
  }
  else if (innerFieldName == sweTextName) {
    def <- xml2::xml_attr(x = innerField, attr = "definition")
    field <- c(name = name, definition = def, rClass = "character")
  }
  else if (innerFieldName == sweQuantityName) {
    def <- xml2::xml_attr(x = innerField, attr = "definition", ns = namespaces)

    child <- xml2::xml_child(x = innerField)
    uom <- NA
    if (sweUomName == xml2::xml_name(child, ns = namespaces)) {
      uom <- xml2::xml_attr(x = child, attr = "code")
    }
    field <- c(name = name, definition = def, uom = uom, rClass = "numeric")
  }
  else if (innerFieldName == sweCategoryName) {
    catName <- xml2::xml_attr(x = innerField, attr = "name")
    definition <- xml2::xml_attr(x = innerField, attr = "definition")
    value <- xml2::xml_text(x = xml2::xml_child(x = innerField,
                                                search = sweValueName,
                                                ns = namespaces))
    codeSpace <- xml2::xml_attr(x = xml2::xml_child(x = innerField,
                                                    search = sweCodeSpaceName,
                                                    ns = namespaces),
                                attr = "xlink:href",
                                ns = namespaces)

    field <- c(name = name,
               category = catName,
               definition = definition,
               value = value,
               codeSpace = codeSpace,
               rClass = "factor")
  }
  else if (innerFieldName == sweBooleanName) {
    def <- xml2::xml_attr(x = innerField, attr = "definition")
    field <- c(name = name, definition = def, rClass = "logical")
  }
  else if (innerFieldName == sweCountName) {
    def <- xml2::xml_attr(x = innerField, attr = "definition")
    field <- c(name = name, definition = def, rClass = "integer")
  }
  else if (name == sweDataRecordName) {
    stop("Parsing of nested swe:DataRecords is not supported!")
  }

  if (verbose) cat("[parseField] Parsed field:", toString(field), "\n")

  return(field)
}

parseTextBlock <- function(obj) {
  .id <- xml2::xml_attr(x = obj, attr = "id", default = NA_character_)
  .tS <- xml2::xml_attr(x = obj, attr = "tokenSeparator")
  .bS <- xml2::xml_attr(x = obj, attr = "blockSeparator")
  .dS <- xml2::xml_attr(x = obj, attr = "decimalSeparator")

  .tb <- SweTextBlock(tokenSeparator = .tS,
                      blockSeparator = .bS,
                      decimalSeparator = .dS,
                      id = .id)
  return(.tb)
}

parseTextEncoding <- function(obj) {
  .id <- xml2::xml_attr(x = obj, attr = "id", default = NA_character_)
  .tS <- xml2::xml_attr(x = obj, attr = "tokenSeparator")
  .bS <- xml2::xml_attr(x = obj, attr = "blockSeparator")
  .dS <- xml2::xml_attr(x = obj, attr = "decimalSeparator", default = NA_character_)

  .tb <- SweTextEncoding(tokenSeparator = .tS,
                         blockSeparator = .bS,
                         decimalSeparator = .dS,
                         id = .id)
  return(.tb)
}

parsePhenomenonProperty <- function(obj, sos, verbose = FALSE) {
  obsProp <- NULL

  # check if reference or inline phenomenon
  href <- xml2::xml_attr(x = obj, attr = "href")
  if (!is.na(href)) {
    if (verbose) cat("[parsePhenomenonProperty] with reference", href, "\n")
    obsProp <- SwePhenomenonProperty(href = href)
  }
  else {
    compPhen <- xml2::xml_child(x = obj)
    # 52N SOS only returns swe:CompositePhenomenon
    name <- xml2::xml_name(x = compPhen, ns = sos@namespaces)
    if (verbose) cat("[parsePhenomenonProperty] inline with name", name, "\n")

    if (name == sweCompositePhenomenonName) {
      phen <- parseCompositePhenomenon(compPhen,  sos = sos, verbose = verbose)
      obsProp <- SwePhenomenonProperty(phenomenon = phen)
    }
    else {
      warning(paste("[parsePhenomenonProperty] Unsupported observed property: ",
                    name, "\n"))
    }
  }

  return(obsProp)
}

parseCompositePhenomenon <- function(obj, sos, verbose = FALSE) {
  .id <- xml2::xml_attr(x = obj, attr = "id", default = NA_character_)
  if (verbose) cat("[parseCompositePhenomenon] with id", .id, "\n")

  .dimension <- as.integer(xml2::xml_attr(x = obj, attr = "dimension", default = NA_character_))
  .name <- xml2::xml_text(xml2::xml_child(x = obj, search = gmlNameName, ns = sos@namespaces))
  if (verbose) cat("[parseCompositePhenomenon] parsed name '", .id,
                  "' and dimension '", .dimension, "'\n", sep = "")

  .components <- lapply(X = xml2::xml_find_all(x = obj, xpath = sweComponentName),
                        FUN = parseComponent,
                        sos = sos,
                        verbose = verbose)

  if (verbose) cat("[parseCompositePhenomenon]", length(.components),
                  "components parsed.\n")

  # optional:
  .description <- NA_character_
  if (!is.na(xml2::xml_child(x = obj, search = gmlDescriptionName, ns = sos@namespaces))) {
    .description <- parsePhenomenonProperty(xml2::xml_child(x = obj, search = gmlDescriptionName, ns = sos@namespaces))
  }
  .base <- NULL
  if (!is.na(xml2::xml_child(x = obj, search = sweBaseName, ns = sos@namespaces))) {
    .base <- parsePhenomenonProperty(xml2::xml_child(x = obj, search = sweBaseName, ns = sos@namespaces))
  }

  .compPhen <- SweCompositePhenomenon(id = .id, name = .name,
                                      description = .description, dimension = .dimension,
                                      components = .components, base = .base)

  return(.compPhen)
}

parseComponent <- function(obj, sos, verbose = FALSE) {
  if (verbose) cat("[parseComponent] ", as(obj, "character"), "\n")
  # 52N SOS only sets the href property on swe components, but still reuse function
  .component <- parsePhenomenonProperty(obj, sos = sos, verbose = verbose)
  return(.component)
}

parseSwePosition <- function(obj, sos, verbose = FALSE) {
  .rF <- xml2::xml_attr(x = obj, attr = "referenceFrame", ns = sos@namespaces)
  if (verbose) cat("[parseSwePosition] with referenceFrame", .rF, "\n")

  .location <- xml2::xml_child(x = obj, search = sweLocationName, ns = sos@namespaces)
  .parser <- sosParsers(sos)[[sweLocationName]]

  .pos <- .parser(.location, sos = sos, verbose = verbose)

  .oldAttrs <- attributes(.pos)
  attributes(.pos) <- c(.oldAttrs, list(referenceFrame = .rF))

  return(.pos)
}

parseSweLocation <- function(obj, sos, verbose = FALSE) {
  .vector <- xml2::xml_child(x = obj, search = sweVectorName, ns = sos@namespaces)
  .id <- xml2::xml_attr(x = obj, attr = "id")
  if (verbose) cat("[parseSweLocation] with id", .id, "\n")

  .parser <- sosParsers(sos)[[sweVectorName]]
  location <- .parser(.vector, sos = sos, verbose = verbose)

  return(location)
}

parseSweVector <- function(obj, sos, verbose = FALSE) {
  children <- xml2::xml_find_all(x = obj,
                                 xpath = sweCoordinateName,
                                 ns = sos@namespaces)
  parser <- sosParsers(sos)[[sweCoordinateName]]
  vector <- lapply(X = children,
                   FUN = parser,
                   sos = sos,
                   verbose = verbose)
  names(vector) <- sapply(vector, function(current) {return(current$axisID)})
  if (verbose) cat("[parseSweVector] parsed vector with coordinates: ", toString(names(vector)), "\n")

  return(vector)
}

parseSweCoordinate <- function(obj, sos, verbose = FALSE) {
  .name <- xml2::xml_attr(x = obj, attr = "name")
  if (verbose) cat("[parseSweCoordinate] with name", .name, "\n")

  .quantity <- xml2::xml_child(x = obj, search = sweQuantityName, ns = sos@namespaces)
  .axisID <- xml2::xml_attr(x = .quantity, attr = "axisID")
  if (verbose) cat("[parseSweCoordinate] axisID: ", .axisID, "\n")

  .uomNode <- xml2::xml_child(x = .quantity, search = sweUomName, ns = sos@namespaces)
  .uomCode <- xml2::xml_attr(x = .uomNode, attr = "code", ns = sos@namespaces)
  if (verbose) cat("[parseSweCoordinate] uomCode: ", .uomCode, "\n")

  .valueNode <- xml2::xml_child(x = .quantity, search = sweValueName, ns = sos@namespaces)
  .value <- as.double(xml2::xml_text(x = .valueNode))
  if (verbose) cat("[parseSweCoordinate] value: ", .value, "\n")

  return(list(name = .name,
              axisID = .axisID,
              uomCode = .uomCode,
              value = .value))
}
