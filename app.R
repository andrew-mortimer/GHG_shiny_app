
library(shiny)
library(ggplot2)
library(SPARQL) 
library(dplyr)

#############################################
# RETRIEVE DATA FROM STATISTICS.GOV.SCOT    #
#############################################

# Step 1 - Set up preliminaries and define query
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

# Set up custom theme for ggplot from the ggthemes package
theme_set(theme_grey(base_size = 16))

# automatically define periods, sectors and pollutants for chart options
minyear <- as.numeric(min(GHGdata[,1]))
maxyear <- as.numeric(max(GHGdata[,1]))
sectorlist <- as.list(distinct(select(GHGdata,Sector)))
pollutantlist <- as.list(distinct(select(GHGdata,Pollutant)))


#######################################
# SERVER CODE                         #
#######################################

server<-function(input, output) {
  
  # Step 3 Set it up for the check boxes for source sectors
  data_1=reactive({
    return(GHGdata[GHGdata$Sector%in%input$source_choose&GHGdata$Pollutant%in%input$pollutant_choose,])
  })
  
# this next bit doesn't work  
#  data_1=reactive({
#    return(data_1[data_1$Pollutant%in%input$pollutant_choose,])
#  })
    
  
  
  # Step 4 Use ggplot2 to draw a basic linechart
  output$plot <- renderPlot({
    
    # Step 5 Set it up so the year chooser updates the chart
    g <- ggplot(data=subset( data_1(),  Year>= input$range[1] & Year<= input$range[2]), aes(x=Year, y=Emissions, group=interaction(Sector, Pollutant))) +     geom_line(aes(color=interaction(Sector, Pollutant)), size=2.0)+ theme(legend.position="bottom")
    g <- g + labs (x="Year", y="Emissions (MtCO2e)") 
    g <- g + guides(col = guide_legend(nrow = 5),byrow=TRUE)  + theme(legend.title=element_blank())
    
    print(g)
    
  })
}

#######################################
# USER INTERFACE                      #
#######################################

ui<-fluidPage(
  
  # Title
  titlePanel("GHG emissions by source sector and pollutant, Scotland"),
  
  # Sidebar 
  sidebarPanel(
#    headerPanel("not currently used"),
    
    # Specification of range within an interval
    sliderInput("range", "Select years:",
                min = minyear, max = maxyear, value = c(1998,maxyear),sep = ""),
    
    # Check boxes for the source sectors
    checkboxGroupInput("source_choose", label = "Select source sectors",
                       choices  = sectorlist$Sector, 
                       selected = sectorlist$Sector
    ),
    # Check boxes for pollutants
    checkboxGroupInput("pollutant_choose", label = "Select pollutants",
                       choices  = pollutantlist$Pollutant, 
                       selected = pollutantlist$Pollutant
    )
  ),
  
  mainPanel(
    tags$head(tags$style("#plot{height:100vh !important;}")),
    plotOutput('plot')
  )
)

#######################################
# RUN SHINYAPP                        #
#######################################

shinyApp(ui, server)
