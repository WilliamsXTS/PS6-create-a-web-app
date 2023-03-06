library(shiny)
library(tidyverse)
diet <- read_delim("Fat_Supply_Quantity_Data.csv")

ui <- fluidPage(
      titlePanel("COVID-19 Healthy Diet"),
mainPanel(
  tabsetPanel(
    tabPanel("Home", p("Welcome to my home page.", "This is Tianshi Xu.",
                       em("Our target audience would be people who are trying to take preventative measures to protect themselves from getting Covid-19."),
                       strong("The Data for COVID-19 confirmed, deaths, recovered and active cases are obtained from Johns Hopkins Center for Systems Science and Engineering. The USDA Center for Nutrition Policy and Promotion diet intake guideline information can be found in ChooseMyPlate.gov."))),
    tabPanel("Table", 
               sidebarPanel(
                 selectInput(inputId = "country",
                             label = "Select the country:",
                             choices = unique(diet$Country))
      ),
      mainPanel(
        tableOutput("table"),
        textOutput("tableconclusion")
      )
    ),
    tabPanel("Plot",
      sidebarLayout(
        sidebarPanel(
            sliderInput("food",
                        "The meat they eat:",
                        min = 0,
                        max = 30,
                        value = 5),
            radioButtons("color", "Choose color",
                         choices = c("skyblue", "lawngreen", "orangered",
                                              "purple", "gold"))
        ),
        mainPanel(
          plotOutput("plot"),
          textOutput("plotconclusion")
        )
    )
)
     )
  )
)

server <- function(input, output) {
  filter <- reactive({
    diet %>% 
      filter(Country == input$country) %>% 
      select(Country, Meat, Recovered)
  })
  
  output$table <- renderTable({
    filter()
  })
  
  output$tableconclusion <- renderText({
    if(!is.null(input$country)) {
      paste(input$country,"is selected", round((filter()$Recovered *100), 2), "%", "is the recovery rate.")
    }
  })
  
    output$plot <- renderPlot({
        diet %>% 
          sample_n(input$food) %>% 
          ggplot(aes(Meat, Recovered)) +
          geom_line(col=input$color)+
          geom_point()
    })
    
    output$plotconclusion <- renderText({
      paste("You selected", input$food, "values")
    })
}

shinyApp(ui = ui, server = server)
