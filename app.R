

#######################################################################################
# LOAD REQUIRED R PACKAGES                                                            #
#######################################################################################

library(shiny)
library(ggplot2)
library(SPARQL) 
library(dplyr)
library(shinyWidgets)
library(DT)


#######################################################################################
# RETRIEVE DATA FROM STATISTICS.GOV.SCOT                                              #
#######################################################################################

# Define the statistics.gov.scot endpoint
endpoint <- "http://statistics.gov.scot/sparql"

# create query statement
query <-
    "PREFIX qb: <http://purl.org/linked-data/cube#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  SELECT ?Year ?Sector ?Pollutant ?Emissions
  WHERE {
  ?s qb:dataSet <http://statistics.gov.scot/data/greenhouse-gas-emissions-by-source-sector>;
  <http://statistics.gov.scot/def/dimension/greenhouseGasSourceSector> ?SectorURI;
  <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?YearURI;
  <http://statistics.gov.scot/def/dimension/pollutant> ?PollutantURI;
  <http://statistics.gov.scot/def/measure-properties/count> ?Emissions.
  ?SectorURI rdfs:label ?Sector.
  ?YearURI rdfs:label ?Year.
  ?PollutantURI rdfs:label ?Pollutant.
  }
  ORDER BY ?Year ?Sector ?Pollutant"

# Use SPARQL package to submit query and save results to a data frame
    qdata <- SPARQL(endpoint,query)
    GHGdata <- qdata$results
    
    
#######################################################################################
# MANIPULATE DATA                                                                     #
#######################################################################################

# Calculate all pollutants and all sources categories and append to GHGdata
    tmp1 <- aggregate(Emissions ~ Year + Pollutant, GHGdata, sum)
    tmp1$Sector <- "(TOTAL - ALL SECTORS)"
    tmp2 <- aggregate(Emissions ~ Year + Sector, GHGdata, sum)
    tmp2$Pollutant <- "(TOTAL - ALL POLLUTANTS)"
    tmp3 <- aggregate(Emissions ~ Year, GHGdata, sum)
    tmp3$Pollutant <- "(TOTAL - ALL POLLUTANTS)"
    tmp3$Sector <- "(TOTAL - ALL SECTORS)"
    GHGdata <- rbind(GHGdata,tmp1,tmp2,tmp3)

# automatically define periods, sectors and pollutants for chart options
    minyear <- as.numeric(min(GHGdata[,1]))
    maxyear <- as.numeric(max(GHGdata[,1]))
    sectorlist <- as.list(distinct(select(GHGdata,Sector)))
    pollutantlist <- as.list(distinct(select(GHGdata,Pollutant)))

# Set up custom theme for ggplot from the ggthemes package
    theme_set(theme_grey(base_size = 16))



#######################################################################################
# SERVER CODE                                                                         #
#######################################################################################

server<-function(input, output) {
  
  # Step 3 Set it up for the check boxes for source sectors
  data_1=reactive({
    return(GHGdata[GHGdata$Sector%in%input$source_choose&GHGdata$Pollutant%in%input$pollutant_choose,])
  })
  
  # output filtered data table
  output$filtered_data = DT::renderDataTable({
    data=subset(data_1(),  Year>= input$range[1] & Year<= input$range[2], options = list(lengthMenu = c(25, 50, 100), pageLength = 25))
  })
  
  # Downloadable csv of selected dataset ----
      #  output$downloadData <- downloadHandler(content = subset(data_1(),  Year>= input$range[1] & Year<= input$range[2], filename="downloadData"))
      # here a tutorial to make this work
      ### https://shiny.rstudio.com/articles/download.html
  
    # Step 4 Use ggplot2 to draw a basic linechart
  output$plot <- renderPlot({
    
    # Step 5 Set it up so the year chooser updates the chart
    g <- ggplot(data=subset( data_1(),  Year>= input$range[1] & Year<= input$range[2]), aes(x=Year, y=Emissions, group=interaction(Sector, Pollutant))) +     geom_line(aes(color=interaction(Sector, Pollutant)), size=1.5)+ theme(legend.position="bottom")
    g <- g + labs (x="Year", y="Emissions (MtCO2e)") 
    g <- g + guides(col = guide_legend(nrow = 5),byrow=TRUE)  + theme(legend.title=element_blank())
    
    print(g)
    
  })
}


#######################################################################################
# USER INTERFACE CODE                                                                 #
#######################################################################################

ui<-fluidPage(
  
  # Title
  tags$head(HTML("<title>GHG emissions by source sector and pollutant, Scotland</title>")),
  titlePanel(h1("GHG emissions by source sector and pollutant, Scotland")),
  
  # Sidebar 
  sidebarPanel(
    
    tags$h3("Options"),
    
    # Specification of range within an interval
    sliderInput("range", "Select years:",
                min = minyear, max = maxyear, value = c(1998,maxyear),sep = ""),

    # Insert a picker widget for source sectors
    pickerInput(
      inputId = "source_choose", 
      label = "Select source sectors", 
      choices = sectorlist$Sector, selected=sectorlist$Sector[1:10], options = list(`actions-box` = TRUE), 
      multiple = TRUE
    ),
    
    # Insert a picker widget for pollutants
    pickerInput(
      inputId = "pollutant_choose", 
      label = "Select pollutants", 
      choices = pollutantlist$Pollutant, selected=pollutantlist$Pollutant[8], options = list(`actions-box` = TRUE), 
      multiple = TRUE
    ),
    
    # Download Button
    #downloadButton("downloadData", "Download these data"),
    
    # HTML STUFF - PROBABLY MOVE INTO A TAB ALONG WITH A DESCRIPTION
    tags$br(),
    tags$br(),
    tags$h3("Links"),
    tags$a(href="https://github.com/andrew-mortimer/GHG_shiny_app", "Download the source code"),
    tags$br(),
    tags$a(href="https://statistics.gov.scot/data/greenhouse-gas-emissions-by-source-sector", "See original Data source")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Line chart",
        tags$head(tags$style("#plot{height:94vh !important;}")),
        plotOutput('plot')
        ),
      tabPanel("Table", 
               DT::dataTableOutput('filtered_data')
               ),
      tabPanel("Summary statistics", "This will have growth rates between minyear and maxyear and a few other bits and bobs")
      )
  )
)


#######################################################################################
# RUN APPLICATION                                                                     #
#######################################################################################

shinyApp(ui, server)
