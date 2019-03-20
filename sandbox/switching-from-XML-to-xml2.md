# Switching from `XML` to `xml2`

## Rationale

The R package `XML` for parsing and manipulation of XML documents in R is not actively maintained anymore, but used by many:

The R package `xml2` is an actively maintained, more recent alternative.

This file documents useful resources and steps for moving from `XML` to `xml2`.

## Resources

- Collected at https://github.com/r-lib/xml2/issues/246
  - [x] https://github.com/jennybc/googlesheets/pull/102
  - [x] https://github.com/quanteda/quanteda/pull/1364/files
  - [x] https://github.com/quanteda/readtext/pull/128/files
  - [x] https://github.com/andrie/sss/commit/8787dd01dc6784bf1fc0f0301ce5d318adad7f70
  - [x] https://github.com/cpsievert/rdom/pull/14/files
  - [x] https://github.com/USGS-R/geoknife/pull/362/files and https://github.com/USGS-R/geoknife/commit/76b4b47297b7e2f2cd96fb783ec2f35db2ac2f8b
  - [x] https://github.com/mikldk/ryacas/commit/e31954259b4ee9e23b0566566b94a9cad7e32ab8 (pretty straightforward example, regex-able)
  - [x] https://github.com/cloudyr/aws.alexa/commit/5f5441f1ac8bd4f1f409e2e61ae3ab154962f6ec

## Finding usage

The `itdepends` package helps with finding all usages of XML, see https://speakerdeck.com/jimhester/it-depends?slide=38

> devtools::install_github("jimhester/itdepends")
> library("itdepends")
> itdepends::dep_locate("XML")

## Mapping functions

### Decoding

| `XML` | `xml2` | Comment |
|-------|--------|---------|
| `XML::getNodeSet(doc = <document object>, path = "<XPath expression>")` or `XML::xpathApply(...)` | `xml2::xml_find_all(..)` and `xml2::xml_find_one(..)` with `x = <node>, xpath = "<XPath 1.0 expression>"` | Find matching nodes value of a node's attribute |
| `XML::htmlTreeParse(<path>, asText = <treat file as text>)` | `xml2::read_html(<path, URL, connection, or literal xml>)` |  parse HTML document |
| `XML::isXMLString("<string>")` | No direct equivalent, can try to parse... | Heuristically determine if string is XML |
| `XML::toString.XMLNode(<node>)` | `as.character(<document or node>)` |  object to character |
| `XML::xmlAttrs(node = <node object>)` | `xml2::xml_attrs(x = <document, node, or node set>)` | Get the attributes of a node, both return a named character vector. |
| `XML::xmlApply(X = <node>)` and `XML::xmlSApply(..)` | functions `xml2::xml_attrs(..)` and `xml2::xml_contents(..)` are vectorized | Apply function to each child of a node |
| `XML::xmlChildren(x = <node object>)[["<name of the sub-node>"]]` | `xml2::xml_child(x = <node>, search = <number, or name of the sub-node>)` (only elements) and `xml2::xml_contents(..)` for all nodes | Get sub-nodes of a node |
| `XML::xmlElementsByTagName(el = <node object>, name = "<name to match>")` | `xml2::xml_find_all(x = <document, node, node set>, xpath = "<name to match>")` | Retrieve children matching tag name (children/sub-elements) |
| `XML::xmlGetAttr(node = <node object>, name = "<attribute name>", default = "<default>")` | `xml2::xml_attr(x = <document, node, or node set>, attr = "<attribute name>")` | Get value of a node's attribute |
| `XML::xmlName(node = <node object>)` | `xml2::xml_name(x = <document, node, or node set>)` | Get name of a node |
| `XML::xmlParse(..)` | `xml2::read_xml(..)` | Unexposed method in `XML` ? |
| `XML::xmlParseDoc(file = <file name> or "<xml content>", asText = !file.exists(file))` | `xml2::read_xml(x = <string, connection, URL, or raw vector>)` | parse XML document |
| `XML::xmlParseString(content = "<string>")` | `xml2::read_xml(x = <string, connection, URL, or raw vector>)` | convenience function XML to node/tree |
| `XML::xmlRoot(x = <node object>)` | `xml2::xml_root(x = <document, node, or node set>` | Get top-level node |
| `XML::xmlSize(obj = <node or document object>)` | `xml2::xml_length()` | Note that `xml_length(..)` does not need to go to the root first, i.e. `XML::xmlSize(XML::xmlRoot(old)) == xml2::xml_length(new)`  |
| `XML::xmlToList(node = <xml node or document>)` | `xml2::as_list(x = <document, node, or node set>)` | convert to R-like list; **difference**: `as_list` does not drop the root element |
| `XML::xmlTreeParse(file = <file name> or "<xml content>", asText = !file.exists(file))` | | parse XML document |
| `if(!is.null(<node object>[["<child name>"]])) {` | `(inherits(xml_child(<node object>, "<child name>"), "xml_missing")` | Checking for child node existence |
| `XML::xmlValue(<node object>)` | `xml2::xml_text(x = <document, node, or node set>)` | Get/Set contents of a leaf node |

**Common snippets**

| `XML` | `xml2` | Comment |
|-------|--------|---------|
| `if (!is.null(XML::xmlChildren(x = obj)[[<node name>]]))` | `if (!inherits(xml2::xml_find_first(x = obj, xpath = <node name>), "xml_missing")` | Check if element exists. |
| `if(!is.null(XML::xmlAttrs(node = obj)[["href"]]))` | `if(!is.na(xml2::xml_attr(x = obj, attr = "href")))` | Checking for potentiall non-existing attribute |

### Encoding

| `XML` | `xml2` | Comment |
|-------|--------|---------|
| `XML::addAttributes(node = <node object>, ..., .attrs = <character vector with attribute names>, append = <replace or add>)` | `xml2::xml_set_attrs(x = <document, node, node set>, value = <named character vector>)` to set multiple attributes and overwrite existing ones, or `xml2::xml_set_attr(x = <node>, attr = <name>, value = <value>)` to append a single attribute | Add attributes to a node; in `xml2` no re-assigning the object is needed, i.e. no `doc <- XML::addAttributes(node = doc, ...)` |
| `XML::addChildren(node = <node object>, kids = list())` | `xml2::xml_add_child(.x = <document or nodeset>, .value = <document, node or nodeset>)` | Add child nodes to a node |
| `XML::saveXML(doc = <xml document object>, file = "<file name>")` | `xml2::write_xml(x = <document or node>, file = "<path or connection">)` | Write XML document to string or file |
| `XML::xmlNamespaceDefinitions(x = <node>)` | `xml2::xml_ns(x = <document, node, or node set>)` | Get namespace definitions from a node |
| `XML::xmlNode(name = "<node name>")` | `xml2::xml_new_document %>% xml2::xml_add_child("<node name>")` or (preferred in docs) `xml_new_root("<node name>")` | Create a new node |
| `XML::xmlValue()` | `xml2::xml_text(x = <document, node, or node set>)` | Get/Set contents of a leaf node |

### Classes

| `XML` | `xml2` | Comment |
|-------|--------|---------|
| `XMLAbstractDocument` | `xml_document` | .. |
| `XMLAbstractNode`, `XMLCommentNode`, `XMLTextNode`, ... | `xml_node` | .. |
| ? | `xml_missing` | .. |

## Process

The following steps were applied in switching from `XML` to `xml2` for the package [`sos4R`](https://github.com/52North/sos4R/issues/42).
This is not a "clean" process, but hopefully provides useful input for other's doing the switch.
Ideally the lessons learned on what can be "regex-ed" and what needs manual interaction go into the above tables at a later stage.

1. Make sure all functions use named parameters and package prefix with the following regular expressions
  - `addAttributes\((?!node)` replaced with `XML::addAttributes(node = `
  - `addChildren\(node` replaced with `XML::addChildren(node`
  - `getNodeSet\((?!doc)` replaced with `XML::getNodeSet(doc = `
  - `isXMLString\((?!str)` replaced with `XML::isXMLString(str = `
  - `saveXML\((?!doc)` replaced with `XML::saveXML(doc = `
  - `xmlAttrs\((?!node)` replaced with `XML::xmlAttrs(node = `
  - `xmlChildren\((?!x)` replaced with `XML::xmlChildren(x = `
  - `xmlElementsByTagName` replaced with `XML::xmlElementsByTagName`
  - `xmlGetAttr\((?!node)` replaced with `XML::xmlGetAttr(node = `
  - `xmlName\((?!node)` replaced with `XML::xmlName(node = `
  - `xmlNode\((?!name)` and `xmlNode\(name =` replaced with `XML::xmlNode(name = `
  - `xmlParse\(` replaced with `XML::xmlParse(file = `
  - `xmlParseDoc\((?!file)` replaced with `XML::xmlParseDoc(file = `
  - `xmlParseString\(` replaced with `XML::xmlParseString(content = `
  - `xmlRoot\((?!x)` replaced with `XML::xmlRoot(x = `
  - `xmlSize\(` replaced with `XML::xmlSize(obj = `
  - `xmlToList\(` replaced with `XML::xmlToList(node = `
  - `xmlTreeParse\(` replaced with `XML::xmlTreeParse(file = `
  - `xmlValue\((?!x)` replaced with `XML::xmlValue(x = `
1. `Imports:` XML instead of `Depends:`
1. Run tests - skip the ones unrelated to XML handling
1. Commit: 
1. **Do the switch** (parsing functions first, all searches in files `*.R`, files in `/sandbox/` ignored for manual corrections; order driven by running a basic parsing test and see where it fails next)
  - `XML::xmlParseDoc`
    - Replace `XML::xmlParseDoc(file = ` with `xml2::read_xml(x = ` (26 occurrences)
    - Fix parameters
      - replace `, asText = TRUE` with `` (blank, 11 occurrences)
      - turn `options` into vector with strings
      - replace `c(XML::NOERROR, XML::RECOVER)` with `SosDefaultParsingOptions()`
      - use `xmlParseOptions` everywhere
    - _Refactor:_ have only one call in `SOS-methods-util.R` in internal function `.internalXmlRead(..)`., drop parameter `xmlParseOptions` from all functions
  - `XML::xmlParseString`
    - Replaced manually by simplifying the implementation of `encodeXML` for signature `"character"`
  - `XML::xmlParse`
    - Replace single occurrence manually and refactored method `parseFile`
  - `XML::xmlRoot`
    - Replace `XML::xmlRoot` with `xml2::xml_root` (25 occurrences)
  - `XML::xmlName`
    - Replace `XML::xmlName(node =` with `xml2::xml_name(x =` (30 occurrences)
    - Manually added `, ns = SosAllNamespaces()` later to have names with prefix
  - `XML::xmlAttrs`
    - Replace `XML::xmlAttrs(node = ` with `xml2::xml_attrs(x = ` (3 occurrences)
    - Fix further occurrences manually by searching for `xmlAttrs` (must have slipped by before)
    - `xml2::xml_attrs(x = obj)[["href"]]` does not work because if attribute href does not exist there will be a "subscript out of bounds" error. Need to use 
  - Search for `xml2::xml_attrs\(x = (.*)\[\[` and fix manually to `xml2::xml_attrs(x = obj, attr = "<attribute name>")` and update subsequent `is.null(..)` checks to use `is.na(..)`
  - `XML::xmlGetAttr`
    - Replace `XML::xmlGetAttr\(node = (.*), name = ` with `xml2::xml_attr(x = $1, attr = ` (55 occurrences)
    - Manually fix the ones with spread across multiple lines and with missing `name = `, can also fix indentation then or remove newline
    - Manually fix where `xmlGetAttr` was used withn `lapply(..)` or `sapply(..)`
  - `XML::xmlValue`
    - Replace `XML::xmlValue\(x = ` with `xml2::xml_text(x = ` (45 occurrences)
  - `XML::xmlChildren`
    - Replace `XML::xmlChildren\(x = ` with `xml2::xml_children(x = ` (22 occurrences)
    - The common pattern `XML::xmlChildren(x = obj)[[gmlTimeInstantName]]` does not work because `xml2::xml_children(..)` does not return a named list. Need to run `xml2::xml_find_all(x = obj, xpath = gmlTimeInstant)` or `xml2::xml_find_first(..)` then. Search for `xml2::xml_children\(x = (.*)\[\[` to fix those manually (10 results)
      - `..find_first` returns missing node: `is.na(xml2::xml_find_first(x, "f"))` or `inherits(xml2::xml_find_first(x, "f"), "xml_missing")`
      - `..find_all` returns (potentially empty) nodeset: `length(xml2::xml_find_all(x, "f"))`
  - Replaced occurrences of class `XMLAbstractNode` and `XMLInternalDocument` for slots in S4 classes with `ANY` and the default prototype to `xml2::xml_missing()`, will have to handle stuff manually around these classes
    - Opened issue about this in `xml2` repo: https://github.com/r-lib/xml2/issues/248
  - Add `SosAllNamespaces()` and add namespaces to all the `xxxName` constants in `R/Constants.R`
  - `test_exceptionreports.R` complete
  - `test_sams.R` added and parsing fixed
  - `XML::getNodeSet` manually switched to `xml2::xml_find_all(..)` and `xml2::xml_find_one(..)`, because XPath-based getting of sub-nodes with `xml2` also requires proper namespaces and some handling can be simplified because of vectorised `xml2::xml_text(..)`.
  - `XML::xmlSize`
    - Updated single occurrence manually
  - `XML::saveXML`
    - Replaced `XML::saveXML(doc = ` with `xml2::write_xml(x = ` (6 occurrences), no parameters in `saveXML` besides `doc` and `file` were used
  - Update `NAMESPACE` to import `xml2` and not `XML`
  - Parsing tests of `test_sensors.R` work
  - `XML::isXMLString`
    - Replace with simple regex test: `grepl("^<(.*)>$", "...")`
  - get rid of `.filterXmlChildren` and `.filterXmlOnlyNoneTexts` _manually_ using `xml2::xml_child(..)`, `xml2::xml_find_first(..)` or `xml2::xml_find_all(..)`
    - also remove all `".noneText"` objects (and by that fix all occurrences of `xmlTagName`)
    - `is.na(xml2::` > fix using `is.na(..)` (regex, 16 occurrences)
  - must fix all `obj[[` because subsetting with `[[` does not work with XML (107 occurrences at this point!)
    - trying to automate by replacing `obj\[\[(.*?)\]\]` with `xml2::xml_child(x = obj, search = $1, ns = SosAllNamespaces())`
    - does not work for multiple subsets, e.g. `obj[["elementCount"]][["Count"]][["value"]]` > search for `SosAllNamespaces())[[` and fix manually to use XPath (4 occurrences)
    - re-check occurrences of `.children[[`
    - New test for...
      - `parseOwsRange`







      - `parseSosFilter_Capabilities`
      - `parseOwsServiceIdentification`


      - `parseTimeInstantProperty`
      - `parseTimePeriod`


    


  - **[Encoding functions]**
  - `XML::addAttributes`
    - switched manually because sometimes `.attrs` is used, which is replaced with `xml2::xml_set_attrs()`, and sometimes not (single `...`), which is replaced with `xml2::xml_set_attr()`, the `_set_attr` variants operate directly on the object (no need to re-assign), and often statements are multi-line (18 occurrences)




TODO:

  - `XML::addChildren`

= "ANY"

  

  - `is.null\(\.` with some XML object, should be `is.na(..)` which picks up on `"xml_missing"` objects
  


  



_Limitations of regexes_ for the actual switch are due to multi-line statements and the result of functions not being the same.
Especially the subsetting with `[[` used extensively does not work the same way anymore.

<!-- Uploaded with

gist -c -f switching-from-XML-to-xml2.md -d "Switching an R package from XML to xml2" switching-from-XML-to-xml2.md

Update with

gist -u https://gist.github.com/3ed3b0057713eb4f4d75d11bb62f2d66 -f switching-from-XML-to-xml2.md switching-from-XML-to-xml2.md

--> 