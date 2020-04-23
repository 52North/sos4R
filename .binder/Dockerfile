FROM rocker/binder:3.6.2
LABEL maintainer='Daniel Nüst <daniel.nuest@uni-muenster.de>'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}

RUN wget https://github.com/nuest/sos4R/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2020-04-16/')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0
