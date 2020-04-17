#
# This is a Shiny web application to illustrate the use of the sos4R package
#
# author: Benedikt Graeler, 52North GmbH

library("shiny")
library("leaflet")
library("xts")
library("rgdal")
library("sos4R")

# sample SOS server hosted by the Wupperverband (wupperverband.de)
fluggs <- SOS(url = "https://fluggs.wupperverband.de/sos2/service",
              binding = "KVP",
              version = "2.0.0")

# Define the UI for the application
ui <- fluidPage(
    titlePanel("Shiny sos4R"),
    p("A demo app retrieving data from the Wupperverband SOS service at ",
      a("https://fluggs.wupperverband.de/sos2/service"), " using the wrapper functions for convenient data access."),

    sidebarLayout(
        sidebarPanel(
            sliderInput("time",
                        "time window",
                        min = Sys.time()-14*24*3600,
                        max = Sys.time(),
                        value = Sys.time()-c(72,24)*3600),
            uiOutput("phenSelector")
        ),

        mainPanel(
            leafletOutput("mymap"),
            p(),
            plotOutput("timeplot")
        )
    )
)

# Define the server logic
server <- function(input, output) {

    sit <- reactive({
        sites <- sites(sos = fluggs,
                       begin = input$time[1], end = input$time[2],
                       includePhenomena = TRUE)
        spTransform(sites, CRS("+init=epsg:4326"))})

    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addMarkers(data = sit(),
                       popup = sit()$siteID,
                       layerId = sit()$siteID)
    })

    observeEvent(input$mymap_marker_click, {
        p <- input$mymap_marker_click
        output$phenSelector <- renderUI({
            choices <- colnames(sit()@data)[-1][which(t(sit()[sit()$siteID == p$id ,]@data[,-1]))]
            selectInput("phen",
                        paste0("Phenomena at ", p$id),
                        choices = choices,
                        selected = choices[1],
                        multiple = TRUE)
        })

        output$timeplot <- renderPlot({
            if(!is.null(input$phen)) {
                dat <- getData(sos = fluggs,
                               sites = p$id,
                               phenomena = input$phen,
                               begin = as.POSIXct(input$time[1]),
                               end = as.POSIXct(input$time[2]))
                if(any(input$phen %in% colnames(dat))) {
                    dat <- xts::xts(dat[,input$phen], dat$timestamp)

                    plot(dat, main="Time series plot")
                    xts::addLegend("topright", on=1,
                              legend.names = input$phen,
                              lty=c(1, 1), lwd=c(2, 1),
                              col=1:length(input$phen))
                }
            }
        })
    })

}

# Run the application
shinyApp(ui = ui, server = server)
