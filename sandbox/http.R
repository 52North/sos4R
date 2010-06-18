################################################################################
# Copyright (C) 2010 by 52 North											   #
# Initiative for Geospatial Open Source Software GmbH						   #
# 																			   #
# Contact: Andreas Wytzisk													   #
# 52 North Initiative for Geospatial Open Source Software GmbH				   #
# Martin-Luther-King-Weg 24													   #
# 48155 Muenster, Germany													   #
# info@52north.org															   #
#																			   #
# This program is free software; you can redistribute and/or modify it under   #
# the terms of the GNU General Public License version 2 as published by the    #
# Free Software Foundation.													   #
#																			   #
# This program is distributed WITHOUT ANY WARRANTY; even without the implied   #
# WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU #
# General Public License for more details.									   #
#																			   #
# You should have received a copy of the GNU General Public License along with #
# this program (see gpl-2.0.txt). If not, write to the Free Software		   #
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or #
# visit the Free Software Foundation web page, http://www.fsf.org.			   #
#																			   #
# Author: Daniel Nüst (daniel.nuest@uni-muenster.de)                           #
# Created: 2010-06-18														   #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #                                              #
#                                                                              #
################################################################################

#####################################################
# built-in download.file
download.file(url="http://giv-sos.uni-muenster.de:8080/52nSOSv3/sos?request=GetCapabilities&version=1.0.0&service=SOS", destfile="text.xml")
# works, but requires saving the file locally... 

#####################################################
# http://cran.r-project.org/web/packages/httpRequest/
library("httpRequest")

# GetCapabilities request with all sections
getCapRequest <- '<?xml version="1.0" encoding="UTF-8"?><GetCapabilities xmlns="http://www.opengis.net/sos/1.0" xmlns:ows="http://www.opengis.net/ows/1.1" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.opengis.net/sos/1.0 http://schemas.opengis.net/sos/1.0.0/sosGetCapabilities.xsd" service="SOS"><ows:AcceptVersions><ows:Version>1.0.0</ows:Version></ows:AcceptVersions><ows:Sections><ows:Section>OperationsMetadata</ows:Section><ows:Section>ServiceIdentification</ows:Section><ows:Section>ServiceProvider</ows:Section><ows:Section>Filter_Capabilities</ows:Section><ows:Section>Contents</ows:Section></ows:Sections></GetCapabilities>'

# variables
port <- 8080
host <- "http://giv-sos.uni-muenster.de"
host2 <- "http://mars.uni-muenster.de"
path <- "/52nSOSv3/sos?request=GetCapabilities&version=1.0.0&service=SOS"
path2 <- "OWS5SOS"
referer <- "52north.org/sos4r"

# getToHost
getcap <- getToHost(host=host, path=path, referer=referer, port=port)
# DOES NOT WORK...
# Error in make.socket(host = host, port = port, server = FALSE) : socket not established

# getToHost2
getcap <- getToHost2(host=host, path=path, referer=referer, port=port)
# Error in socketConnection(host = host, port = port, open = "a+b", blocking = TRUE) : 
#		cannot open the connection
# In addition: Warning message:
# In socketConnection(host = host, port = port, open = "a+b", blocking = TRUE) :
#  http://giv-sos.uni-muenster.de:8080 cannot be opened


# simplePostToHost
path3 <- "/52nSOSv3/sos"
data <- "request=GetCapabilities&version=1.0.0&service=SOS"
getCap <- simplePostToHost(host=host, path=path3, referer=NULL, data = data, port=port)

# postToHost
datalist <- list("request"="GetCapabilities", "version"="1.0.0", "service"="SOS")
getCap <- postToHost(host=host, path=path3, data.to.send=datalist, referer=NULL, port=port)
# Error in make.socket(host = host, port = port, server = FALSE) : socket not established




############################################
# http://www.omegahat.org/RCurl/
# install.packages(packageName, repos = "http://www.omegahat.org/R")
library("RCurl")
temp <- getURL("www.google.de")

# getForm
temp <- getURL(host, port=port, referer=referer, verbose=TRUE)
temp <- getURL("http://giv-sos.uni-muenster.de:8080/52nSOSv3/sos?request=GetCapabilities&version=1.0.0&service=SOS")
# GEHT!

temp <- getForm("http://giv-sos.uni-muenster.de:8080/52nSOSv3/sos", request="GetCapabilities", version="1.0.0", service="SOS")
# GEHT AUCH! Ist auch sauberer.


sosUrl = "http://giv-sos.uni-muenster.de:8080/52nSOSv3/sos"
myCurlOptions = c(verbose = TRUE)
myParams <- list("request"="GetCapabilities", "version"="1.0.0", "service"="SOS")

temp <- postForm(sosUrl, .params = myParams)
# liefer nur raw[0]
rawToChar(temp)
# ""

# postForm
temp <- postForm("http://giv-sos.uni-muenster.de:8080/52nSOSv3/sos", request="GetCapabilities", version="1.0.0", service="SOS")
# liefert exception report... unexpected element CDATA

# need to use getCapRequest
temp <- postForm("http://giv-sos.uni-muenster.de:8080/52nSOSv3/sos", "request"=getCapRequest)
# encoding error


############################################
# http://www.omegahat.org/SSOAP/
library("SSOAP")
