
library(shiny)
library(tidyverse)

#Data load and tidy
togoi <- read_csv(here::here("togoi.csv"))

togoi$Larva.tmt <- as.factor(
  str_c(togoi$Larva.Temp.C, " ", togoi$Larva.Food))
togoi$Adult.tmt <- as.factor(
  str_c(togoi$Adult.Temp.C, " ", togoi$Adult.Food))

togoi <- togoi %>%
  filter(wing.length != "NA") %>%
  select("Larva.tmt", "sex", "wing.length")

options(shiny.autoreload = TRUE)


ui <- fluidPage(
  titlePanel("Mosquito body size analyses"),
  tags$small("Markus Thormeyer, Phd Student, UBC"),
  tags$br(),
  tags$br(),
  "This app will help you compare how different larval treatments of 
  temperature and nutrition affect adult body size.",
  "The number corresponds to the rearing temperature recorded as degrees 
  Celsius, and the 'Low', 'Med', 'High', correspond to relative nutrition 
  levels",
  tags$br(),
  tags$br(),
  "Please start by selecting any of the following treatments below",
  tags$br(),
  tags$br(),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "my_checkbox_larva.tmt", "Select Larva Treatment Type",
        choices = unique(togoi$Larva.tmt)
        ),
      tags$br(),
      strong("Boxplot sex differentiation"),
      checkboxInput(
        "sex_checkbox", "Click for sex differentiation", 
        value = TRUE),
      tags$br(),
      tags$br(),
      strong("Summary statistics"),
      tableOutput("my_table")
      ),
    mainPanel(
      plotOutput("my_plot")
    )
  )
)

server <- function(input, output) {
  
  #My first feature is this widget which will allow for the selection of 
  #different larval treatment groups for comparison by visual analysis (boxplot),
  #or summary statistics (table). This widget is also capable of splitting the
  #plot into male and female mosquitoes. 
  filtered <- reactive({
    req(input$my_checkbox_larva.tmt)
    togoi %>%
      filter(Larva.tmt == input$my_checkbox_larva.tmt)
  })
  
  #My second feature is this plot which displays the different body sizes of
  #mosquitoes grown at different larval temperatures and nutrition contents
  output$my_plot <- renderPlot(
    if(input$sex_checkbox == TRUE){
      filtered() %>%
        ggplot(aes(x = Larva.tmt, y = wing.length)) +
        geom_jitter(aes(alpha = 0.6), size =3, width = 0.3)+
        geom_boxplot()+
        theme(legend.position = "none")+
        ylab("Wing Length (cm)")+
        facet_wrap(vars(sex))
    }else{
    filtered() %>%
      ggplot(aes(x = Larva.tmt, y = wing.length)) +
      geom_jitter(aes(alpha = 0.6), size =3, width = 0.3)+
      geom_boxplot()+
      theme(legend.position = "none")+
      ylab("Wing Length (cm)")
    }
  )
  
  
  #My third feature is this output table which provides some basic stats 
  #results of the selected mosquito growth treatments.
  output$my_table <- renderTable(
    filtered() %>%
      group_by(Larva.tmt) %>%
      summarize("Mean (cm)" = mean(wing.length),
                "SD" = sd(wing.length))
  )
}

shinyApp(ui = ui, server = server)


