title: Data Processing (I)
link: http://www.xn--nrdholmen-07a.net/sos4r/2010/04/data-processing-i/
author: daniel
description: 
post_id: 66
created: 2010/04/27 09:00:28
created_gmt: 2010/04/27 07:00:28
comment_status: open
post_name: data-processing-i
status: publish
post_type: post

# Data Processing (I)

Since the last post I mostly worked on processing the ds570.0 dataset. I implement a parser in Java to feed the ClimateSOS (<http://giv-sos.uni-muenster.de:8080/ClimateSOS/>) via the transactional profile - meaning I user the SOS operations RegisterSensor and InsertObservation via [http POST](http://en.wikipedia.org/wiki/POST_\(HTTP\)) to insert data. I base the code on the transactional feeder from the [OX-Framework](http://52north.org/maven/project-sites/swe/oxf/index.html), and will also make the programme available for download here (see below).