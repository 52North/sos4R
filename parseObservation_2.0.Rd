\name{parseObservation_2.0}
\alias{parseObservation_2.0}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
parseObservation_2.0(obj, sos, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{sos}{
%%     ~~Describe \code{sos} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (obj, sos, verbose = FALSE) 
{
    obj <- obj[[om20OM_Observation]]
    .id <- xmlGetAttr(node = obj, name = "id", default = NA_character_)
    if (verbose) 
        cat("[parseObservation]", .id, "\n")
    .procedure <- xmlGetAttr(node = obj[[omProcedureName]], name = "href", 
        default = NA_character_)
    .observedProperty <- parsePhenomenonProperty(obj[[omObservedPropertyName]], 
        sos = sos, verbose = verbose)
    if (!is.null(obj[[omPhenomenonTimeName]])) {
        .phenomenonTime <- parseTime(obj = obj[[omPhenomenonTimeName]], 
            format = sosTimeFormat(sos = sos), verbose = verbose)
    }
    else {
        warning("om:phenomenonTime is mandatory in om:Observation, but is missing!")
        .phenomenonTime <- NULL
    }
    if (!is.null(obj[[omFeatureOfInterestName]])) {
        .featureOfInterest <- parseFOI(obj[[omFeatureOfInterestName]], 
            sos = sos, verbose = verbose)
        if (!is.null(.featureOfInterest@href)) {
            if (verbose) 
                cat("[trying to get referenced featureOfInterest]\n")
            foiList <- getFeatureOfInterest(sos = sos, featureOfInterest = .featureOfInterest@href)
            if (length(foiList) > 0) {
                .featureOfInterest <- foiList[[1]]
            }
        }
    }
    else {
        warning("om:featureOfInterest is mandatory in om:Observation, but is missing!")
        .featureOfInterest <- NULL
    }
    .resultParsingFunction <- sosParsers(sos)[[omResultName]]
    .result <- .resultParsingFunction(obj[[omResultName]], sos, 
        verbose)
    if (!is.null(obj[[omResultTimeName]])) {
        .resultTime <- parseSamplingTime(obj = obj[[omResultTimeName]], 
            format = sosTimeFormat(sos = sos), verbose = verbose)
    }
    else {
        .resultTime <- NULL
    }
    .obs <- OmOM_Observation(phenomenonTime = .phenomenonTime, 
        procedure = .procedure, observedProperty = .observedProperty, 
        featureOfInterest = .featureOfInterest, result = .result)
    return(.obs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
