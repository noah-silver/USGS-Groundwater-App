

library(shiny)

ui <- fluidPage(

    # Application title
    titlePanel("USGS Groundwater Data Plots"),
    tabsetPanel(
      tabPanel("State Plots", fluid = TRUE,
       sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "state",
                      label = "State Name",
                      choices = c(tolower(state.name), "district of columbia"),
                      selected = NULL),
            selectInput(inputId = "fill",
                        label = "Variable of Interest",
                        choices = colnames(readRDS("countydat3.RDS")[11:62]),
                        selected = NULL),
            textInput(inputId = "leg",
                      label = "Legend Title",
                      value = " ",
                      placeholder = NULL),
            selectInput(inputId = "low",
                        label = "Color: Low Variable Level",
                        choice = colors(),
                        selected = "white"),
            selectInput(inputId = "high",
                        label = "Color: High Variable Level",
                        choice = colors(),
                        selected = "red")
        ),

        mainPanel(
            plotOutput("plot")
           
        )
    )
),
  tabPanel("Region Plots", fluid = TRUE,
           
           plotOutput("plot2"),
           hr(),
           
           fluidRow(
             column(3,
                    selectInput(inputId = "state2",
                                label = "State #1",
                                choices = c(tolower(state.name), "district of columbia"),
                                selected = "wyoming"),
                    selectInput(inputId = "state3",
                                label = "State #2",
                                choices = c(tolower(state.name), "district of columbia", NA),
                                selected = "south dakota"),
                    selectInput(inputId = "state4",
                                label = "State #3",
                                choices = c(tolower(state.name), "district of columbia", NA),
                                selected = "nebraska"),
                    selectInput(inputId = "state5",
                                label = "State #4",
                                choices = c(tolower(state.name), "district of columbia", NA),
                                selected = "colorado")),
             column(4, offset = 1,
                    selectInput(inputId = "state6",
                                label = "State #5",
                                choices = c(tolower(state.name), "district of columbia", NA),
                                selected = "kansas"),
                    selectInput(inputId = "state7",
                                label = "State #6",
                                choices = c(tolower(state.name), "district of columbia", NA),
                                selected = "oklahoma"), 
                    selectInput(inputId = "state8",
                                                         label = "State #7",
                                                            choices = c(tolower(state.name), "district of columbia", NA),
                                                            selected = "new mexico"),
                    selectInput(inputId = "state9",
                                label = "State #8",
                                choices = c(tolower(state.name), "district of columbia", NA),
                                selected = "texas")),
             column(4,
                    selectInput(inputId = "state10",
                                label = "State #9",
                                choices = c(tolower(state.name), "district of columbia", NA),
                                selected = NA),
                    selectInput(inputId = "state11",
                                label = "State #10",
                                choices = c(tolower(state.name), "district of columbia", NA),
                                selected = NA),
                    selectInput(inputId = "fill2",
                                label = "Variable of Interest",
                                choices = colnames(readRDS("countydat3.RDS")[11:62]),
                                selected = NULL),
                    textInput(inputId = "leg2",
                              label = "Legend Title",
                              value = " ",
                              placeholder = NULL),
                    selectInput(inputId = "low2",
                                label = "Color: Low Variable Level",
                                choice = colors(),
                                selected = "white"),
                    selectInput(inputId = "high2",
                                label = "Color: High Variable Level",
                                choice = colors(),
                                selected = "red"))
             
           )
        
    )
  ),
)




server <- function(input, output) {
    library(tidyverse)
    library(maps)
    library(mapproj)
    library(readr)
  
    countydat3 <- readRDS("countydat3.RDS")

    output$plot <- renderPlot({
        abbdat <- map_data("county", region = input$state)
        abbdat2 <- left_join(abbdat, countydat3, by = c("subregion","region"))
        ggplot(abbdat2, aes_string(abbdat2$long, abbdat2$lat, group = abbdat2$group, fill = input$fill))+
            geom_polygon(color = "black")+
            scale_fill_gradient2(name = input$leg, low = input$low, high = input$high, na.value = "white")+
            coord_map("polyconic")+
            theme_void()
    })
    
    output$plot2 <- renderPlot({
      abbdatregion <- map_data("county", region = c(input$state2, input$state3, input$state4,
                                                    input$state5, input$state6, input$state7, 
                                                    input$state8, input$state9, input$state10, 
                                                    input$state11))
      abbdatregion2 <- left_join(abbdatregion, countydat3, by = c("subregion","region"))
      ggplot(abbdatregion2, aes_string(abbdatregion2$long, abbdatregion2$lat, group = abbdatregion2$group, fill = input$fill2))+
        geom_polygon(color = "black")+
        scale_fill_gradient2(name = input$leg2, low = input$low2, high = input$high2, na.value = "white")+
        coord_map("polyconic")+
        theme_void()
    })
    
}

shinyApp(ui = ui, server = server)
