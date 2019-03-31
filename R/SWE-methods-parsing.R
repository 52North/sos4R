################################################################################
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
################################################################################

#
# optimized for 52N SOS, that means only options used there in OMEncoder are
# handled here.
#
# For example, swe:elementCount can also have an attribut ref, but this is not
# checked here, and swe:Count is actually a swe:AbstractDataComponentType, but
# here it is just looked for a child element swe:value.
#
parseDataArray <- function(obj, sos, verbose = FALSE) {
  .elementCount <- xml2::xml_text(xml2::xml_find_first(x = obj,
                                                       xpath = "./swe:elementCount/swe:Count/swe:value",
                                                       ns = SosAllNamespaces())
                                  )
  if (verbose) cat("[parseDataArray] Parsing DataArray with", .elementCount, "elements.\n")

  .elementTypeParser <- sosParsers(sos)[[sweElementTypeName]]
  .elementTypeXml <- xml2::xml_child(x = obj, search = sweElementTypeName, ns = SosAllNamespaces())
  .fields <- .elementTypeParser(obj = .elementTypeXml,
                                sos = sos,
                                verbose = verbose)
  if (verbose) cat("[parseDataArray] Parsed field descriptions:", toString(.fields), "\n")

  .encParser <- sosParsers(sos)[[sweEncodingName]]
  .encodingXml <- xml2::xml_child(x = obj,
                                  search = sweEncodingName,
                                  ns = SosAllNamespaces())
  .encoding <- .encParser(obj = .encodingXml, sos = sos, verbose = verbose)

  if (verbose) cat("[parseDataArray] Parsed encoding description:", toString(.encoding), "\n")

  .valParser <- sosParsers(sos)[[sweValuesName]]
  .values <- .valParser(values = xml2::xml_child(x = obj,
                                                 search = sweValuesName,
                                                 ns = SosAllNamespaces()),
                        fields = .fields,
                        encoding = .encoding,
                        sos = sos, verbose = verbose)

  return(.values)
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

  .converters <- sosDataFieldConverters(sos)

  .blockLines <- strsplit(x = xml2::xml_text(x = values),
                          split = encoding@blockSeparator)
  .tokenLines <- sapply(.blockLines, strsplit,
                        split = encoding@tokenSeparator)

  if (verbose)
    cat("[parseValues] Parsing values from lines: ", toString(.tokenLines), "\n")

  # data frame of correct length to be able to use cbind for first column
  .tempId = "tempID"
  .data <- data.frame(seq(1,length(.tokenLines)))
  names(.data) <- .tempId

  # do following for all fields
  .fieldCount <- length(fields)
  for (.currentFieldIdx in seq(1,.fieldCount)) {
    if (verbose) cat("[parseValues] Processing field index", .currentFieldIdx , "of", .fieldCount,"\n")

    # create list for each variable
    .currentValues <- sapply(.tokenLines, "[[", .currentFieldIdx)
    if (verbose)
      cat("[parseValues] Current values: ", toString(.currentValues), "\n")
    .currentField <- fields[[.currentFieldIdx]]

    if (verbose)
      cat("[parseValues] Parsing field", paste(.currentField), "\n")

    # convert values to the correct types
    .fieldDefinition <- .currentField[["definition"]]
    .method <- .converters[[.fieldDefinition]]
    if (verbose) {
      cat("[parseValues] Using converter:\n")
      print(.method)
    }

    if (is.null(.method)) {
      # could still be a unit of measurement given, use as
      if (!is.na(.currentField["uom"])) {
        .method <- .converters[[.currentField[["uom"]]]]
        if (is.null(.method)) {
          # fallback option
          warning(paste("No converter for the unit of measurement ",
                        .currentField[["uom"]],
                        " with the definition ",
                        .currentField[["definition"]],
                        "! Trying a default, but you can add one when creating a SOS using",
                        "SosDataFieldConvertingFunctions().\n"))

          .method <- .converters[["fallBack"]]
        }
      }
      else {
        warning(paste("No converter found for the given field", toString(.currentField),
                      "using fallBack converter."))
        .method <- .converters[["fallBack"]]
      }
    }

    if (verbose) {
      cat("[parseValues] Using converter function:\n")
      show(.method)
    }

    # do the conversion
    .currentValues <- .method(x = .currentValues, sos = sos)

    # bind new and existing data:
    if (verbose) cat("[parseValues] Binding additional data.frame for",
                    .currentField[["name"]],
                    "-- value range", toString(range(.currentValues)), "\n")
    .newData <- data.frame(.currentValues)

    # create the names of the new data:
    .newDataName <- .currentField[["name"]]
    names(.newData) <- .cleanupColumnName(.newDataName)

    if (verbose) cat("[parseValues] Added column name:", names(.newData), "\n")

    # bind existing and new data column
    .data <- cbind(.data, .newData)

    if (verbose) {
      cat("[parseValues] The new bound data frame (one variable the a temp id):\n")
      str(.data)
    }

    # add field information as attributes to the new column using human
    # readable names
    .addAttrs <- as.list(.currentField)
    names(.addAttrs) <- .sosParseFieldReadable[names(.currentField)]

    .lastColumn <- dim(.data)[[2]]
    .oldAttrs <- attributes(.data[,.lastColumn])

    attributes(.data[,.lastColumn]) <- c(as.list(.oldAttrs), .addAttrs)

    if (verbose) cat("[parseValues] Added attributes to new data:",
                    toString(.addAttrs),
                    "[ names: ", toString(names(.addAttrs)), "]",
                    "\n[parseValues] Old attributes list is",
                    toString(.oldAttrs),
                    "\n[parseValues] New attributes list is",
                    toString(attributes(.data[,.lastColumn])),
                    "\n")
  }

  # remove id column
  if (verbose) cat("[parseValues] Removing temporary first column\n")
  .data <- .data[,!colnames(.data) %in% .tempId]

  if (verbose) {
    cat("[parseValues] returning final data frame:\n")
    str(.data)
  }
  return(.data)
}

#
# Creates list of named character vectors with the information from swe:field Elements.
#
parseElementType <- function(obj, sos, verbose = FALSE) {
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

  simpleDataRecord <- xml2::xml_child(x = elementType, search = sweSimpleDataRecordName, ns = SosAllNamespaces())
  dataRecord <- xml2::xml_child(x = elementType, search = sweDataRecordName, ns = SosAllNamespaces())

  if (!is.na(simpleDataRecord) || !is.na(dataRecord)) {
    # pick the existing one
    if (!is.na(simpleDataRecord)) dr <- simpleDataRecord
    else dr <- dataRecord

    fields <- xml2::xml_find_all(x = dr,
                                 xpath = sweFieldName,
                                 ns = SosAllNamespaces())

    if (verbose) cat("[parseElementType] Got data record with", length(fields), "fields. \n")

    # extract the fields, naming with attribute 'name'
    parsedFields <- lapply(fields, parseField, sos = sos, verbose = verbose)
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
  .textBlock <- xml2::xml_child(x = obj, search = sweTextBlockName, ns = SosAllNamespaces())

  .textEncoding <- xml2::xml_child(x = obj, search = sweTextEncodingName, ns = SosAllNamespaces())

  if (!(is.na(.textBlock))) {
    .tb <- parseTextBlock(.textBlock)
    return(.tb)
  }
  else if (!(is.na(.textEncoding))) {
    .tb <- parseTextEncoding(.textEncoding)
    return(.tb)
  }
  else {
    stop(paste("Cannot parse swe:encoding, only", sweTextBlockName, "and", sweTextEncodingName,
               "are supported!"))
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
  "category code space")
names(.sosParseFieldReadable) <- list(
  "name",
  "definition",
  "uom",
  "category",
  "value",
  "codeSpace")

#
# Function creates a named character vector (using field names from variables
# like ".sosParseFieldXYZ") with all information stored in a swe:field.
#
parseField <- function(obj, sos, verbose = FALSE) {
  field <- NULL
  name <- xml2::xml_attr(x = obj, attr = "name", ns = SosAllNamespaces())
  if (verbose) cat("[parseField] Parsing field description of ", name, "\n")

  innerField <- xml2::xml_child(x = obj)
  innerFieldName <- xml2::xml_name(x = innerField, ns = SosAllNamespaces())

  # Available options: Time, Text, Quantity, Category, Boolean
  # The parsed elements and fields are closely bound to 52N SOS (OMEncoder.java)
  if (innerFieldName == sweTimeName || innerFieldName == sweTextName) {
    def <- xml2::xml_attr(x = innerField, attr = "definition")
    field <- c(name = name, definition = def)
  }
  else if (innerFieldName == sweQuantityName) {
    def <- xml2::xml_attr(x = innerField, attr = "definition", ns = SosAllNamespaces())

    child <- xml2::xml_child(x = innerField)
    uom <- NA
    if (sweUomName == xml2::xml_name(child, ns = SosAllNamespaces())) {
      uom <- xml2::xml_attr(x = child, attr = "code")
    }
    field <- c(name = name, definition = def, uom = uom)
  }
  else if (innerFieldName == sweCategoryName) {
    catName <- xml2::xml_attr(x = innerField, attr = "name")
    definition <- xml2::xml_attr(x = innerField, attr = "definition")
    value <- xml2::xml_text(x = xml2::xml_child(x = innerField,
                                                search = sweValueName,
                                                ns = SosAllNamespaces()))
    codeSpace <- xml2::xml_attr(x = xml2::xml_child(x = innerField,
                                                    search = sweCodeSpaceName,
                                                    ns = SosAllNamespaces()),
                                attr = "xlink:href",
                                ns = SosAllNamespaces())

    field <- c(name = name,
               category = catName,
               definition = definition,
               value = value,
               codeSpace = codeSpace)
  }
  else if (name == sweCountName) {
    warning("Parsing of the given swe:field ", name, " is not implemented!")
    field <- c(name = innerField)
  }
  else if (name == sweDataRecordName) {
    stop("Parsing of nested swe:DataRecords is not supported!")
  }

  if (verbose) cat("[parseField] Parsed field", toString(field), "\n")

  return(field)
}

parseTextBlock <- function(obj) {
  .id <- xml2::xml_attr(x = obj, attr = "id", default = NA_character_)
  .tS <- xml2::xml_attr(x = obj, attr = "tokenSeparator")
  .bS <- xml2::xml_attr(x = obj, attr = "blockSeparator")
  .dS <- xml2::xml_attr(x = obj, attr = "decimalSeparator")

  .tb <- SweTextBlock(tokenSeparator = .tS, blockSeparator = .bS,
                      decimalSeparator = .dS, id = .id)
  return(.tb)
}

parseTextEncoding <- function(obj) {
  .id <- xml2::xml_attr(x = obj, attr = "id", default = NA_character_)
  .tS <- xml2::xml_attr(x = obj, attr = "tokenSeparator")
  .bS <- xml2::xml_attr(x = obj, attr = "blockSeparator")
  .dS <- xml2::xml_attr(x = obj, attr = "decimalSeparator", default = NA_character_)

  .tb <- SweTextEncoding(tokenSeparator = .tS, blockSeparator = .bS,
                         decimalSeparator = .dS, id = .id)
  return(.tb)
}

parsePhenomenonProperty <- function(obj, verbose = FALSE) {
  .obsProp <- NULL

  # check if reference or inline phenomenon
  .href <- xml2::xml_attr(x = obj, attr = "href")
  if (!is.na(.href)) {
    if (verbose) cat("[parsePhenomenonProperty] with reference", .href, "\n")
    .obsProp <- SwePhenomenonProperty(href = .href)
  }
  else {
    .compPhen <- xml2::xml_child(x = obj)
    # 52N SOS only returns swe:CompositePhenomenon
    .name <- xml2::xml_name(x = .compPhen, ns = SosAllNamespaces())
    if (verbose) cat("[parsePhenomenonProperty] inline with name", .name, "\n")

    if (.name == sweCompositePhenomenonName) {
      .phen <- parseCompositePhenomenon(.compPhen, verbose = verbose)
      .obsProp <- SwePhenomenonProperty(phenomenon = .phen)
    }
    else {
      warning(paste("[parsePhenomenonProperty] Unsupported observed property: ",
                    .name, "\n"))
    }
  }

  return(.obsProp)
}

parseCompositePhenomenon <- function(obj, verbose = FALSE) {
  .id <- xml2::xml_attr(x = obj, attr = "id", default = NA_character_)
  if (verbose) cat("[parseCompositePhenomenon] with id", .id, "\n")

  .dimension <- as.integer(xml2::xml_attr(x = obj, attr = "dimension", default = NA_character_))
  .name <- xml2::xml_text(xml2::xml_child(x = obj, search = gmlNameName, ns = SosAllNamespaces()))
  if (verbose) cat("[parseCompositePhenomenon] parsed name '", .id,
                  "' and dimension '", .dimension, "'\n", sep = "")

  .components <- lapply(xml2::xml_find_all(x = obj, xpath = sweComponentName),
                        parseComponent, verbose = verbose)

  if (verbose) cat("[parseCompositePhenomenon]", length(.components),
                  "components parsed.\n")

  # optional:
  .description <- NA_character_
  if (!is.na(xml2::xml_child(x = obj, search = gmlDescriptionName, ns = SosAllNamespaces()))) {
    .description <- parsePhenomenonProperty(xml2::xml_child(x = obj, search = gmlDescriptionName, ns = SosAllNamespaces()))
  }
  .base <- NULL
  if (!is.na(xml2::xml_child(x = obj, search = sweBaseName, ns = SosAllNamespaces()))) {
    .base <- parsePhenomenonProperty(xml2::xml_child(x = obj, search = sweBaseName, ns = SosAllNamespaces()))
  }

  .compPhen <- SweCompositePhenomenon(id = .id, name = .name,
                                      description = .description, dimension = .dimension,
                                      components = .components, base = .base)

  return(.compPhen)
}

parseComponent <- function(obj, verbose = FALSE) {
  if (verbose) cat("[parseComponent] ", as(obj, "character"), "\n")
  # 52N SOS only sets the href property on swe components, but still reuse function
  .component <- parsePhenomenonProperty(obj, verbose)
  return(.component)
}

parseSwePosition <- function(obj, sos, verbose = FALSE) {
  .rF <- xml2::xml_attr(x = obj, attr = "referenceFrame", ns = SosAllNamespaces())
  if (verbose) cat("[parseSwePosition] with referenceFrame", .rF, "\n")

  .location <- xml2::xml_child(x = obj, search = sweLocationName, ns = SosAllNamespaces())
  .parser <- sosParsers(sos)[[sweLocationName]]

  .pos <- .parser(.location, sos = sos, verbose = verbose)

  .oldAttrs <- attributes(.pos)
  attributes(.pos) <- c(.oldAttrs, list(referenceFrame = .rF))

  return(.pos)
}

parseSweLocation <- function(obj, sos, verbose = FALSE) {
  .vector <- xml2::xml_child(x = obj, search = sweVectorName, ns = SosAllNamespaces())
  .id <- xml2::xml_attr(x = obj, attr = "id")
  if (verbose) cat("[parseSweLocation] with id", .id, "\n")

  .parser <- sosParsers(sos)[[sweVectorName]]
  location <- .parser(.vector, sos = sos, verbose = verbose)

  return(location)
}

parseSweVector <- function(obj, sos, verbose = FALSE) {
  .children <- xml2::xml_find_all(x = obj,
                                  xpath = sweCoordinateName,
                                  ns = SosAllNamespaces())

  .parser <- sosParsers(sos)[[sweCoordinateName]]
  .vector <- lapply(X = .children, FUN = .parser, sos = sos, verbose = verbose)
  names(.vector) <- sapply(.vector, function(current) {return(current$axisID)})
  if (verbose) cat("[parseSweVector] parsed vector with coordinates: ", toString(names(.vector)), "\n")

  return(.vector)
}

parseSweCoordinate <- function(obj, sos, verbose = FALSE) {
  .name <- xml2::xml_attr(x = obj, attr = "name")
  if (verbose) cat("[parseSweCoordinate] with name", .name, "\n")

  .quantity <- xml2::xml_child(x = obj, search = sweQuantityName, ns = SosAllNamespaces())
  .axisID <- xml2::xml_attr(x = .quantity, attr = "axisID")
  if (verbose) cat("[parseSweCoordinate] axisID: ", .axisID, "\n")

  .uomNode <- xml2::xml_child(x = .quantity, search = sweUomName, ns = SosAllNamespaces())
  .uomCode <- xml2::xml_attr(x = .uomNode, attr = "code", ns = SosAllNamespaces())
  if (verbose) cat("[parseSweCoordinate] uomCode: ", .uomCode, "\n")

  .valueNode <- xml2::xml_child(x = .quantity, search = sweValueName, ns = SosAllNamespaces())
  .value <- as.double(xml2::xml_text(x = .valueNode))
  if (verbose) cat("[parseSweCoordinate] value: ", .value, "\n")

  return(list(name = .name,
              axisID = .axisID,
              uomCode = .uomCode,
              value = .value))
}
