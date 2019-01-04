title: cross building
link: http://www.xn--nrdholmen-07a.net/sos4r/2010/10/cross-building/
author: daniel
description: 
post_id: 446
created: 2010/10/21 08:42:26
created_gmt: 2010/10/21 06:42:26
comment_status: open
post_name: cross-building
status: publish
post_type: post

# cross building

I tried to find a nice solution for cross building my package for different platforms - well, for Windows mostly. I do not want to dig into R source code or compile R myself and stuff like that. Luckily, I found a good looking explanation shot explanation on the page for [contributed documentation](http://cran.r-project.org/other-docs.html), but sadly **I did not (!) get it to work**. It is called **"Building Microsoft Windows Versions of R and R packages under Intel Linuxâ€** by Jun Yan and A. J. Rossini, and comes with a PDF and a makefile. But the latter relies on resources that are no longer available online. That would/could have been great... I wrote an Email to the authors, but I did not get any response in the last two weeks. I don't recommend to spend time on that.

## Comments

**[Henning](#28 "2010-10-23 19:32:43"):** Which resources are not available anymore?

**[daniel](#29 "2010-10-24 13:10:33"):** Files that the makefile wants to download are not available:

