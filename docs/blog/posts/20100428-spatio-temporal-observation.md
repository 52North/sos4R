title: Spatio Temporal Observation
link: http://www.xn--nrdholmen-07a.net/sos4r/2010/04/spatio-temporal-observation/
author: daniel
description: 
post_id: 108
created: 2010/04/28 23:58:23
created_gmt: 2010/04/28 21:58:23
comment_status: open
post_name: spatio-temporal-observation
status: publish
post_type: post

# Spatio Temporal Observation

The main component of sos4R will be the encoders and decoders for the request and responses that are sent to respectively received from the SOS. The SOS uses Observations & Measurements as the format to communicate the stored observations. But since O&M is a very broad specification, it is necessary to define a certain subset which is supported by sos4R. On top of that, the software structure will make the decoders for these elements easily interchangeable.