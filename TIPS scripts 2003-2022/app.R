runExample("01_hello")


##
library(shiny)

# Define UI

ui <- fluidPage(
  titlePanel("title panel"),
  
  fluidRow(
    dateRangeInput("daterange3", "Date range:",
                   start = "2003-01-01",
                   end = "2021-01-01",
                   min = "2001-01-01",
                   max = "2021-01-01",
                   format = "mm/dd/yy",
                   separator = " - "),
    
  
    
  sidebarLayout(
                sidebarPanel("sidebar panel"),
                mainPanel("main panel")
  )
  )
                
)

# Define server logic

server <- function(input, output) {}

# RUn the app

shinyApp(ui = ui, server= server)


ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      h1("First level title"),
      h2("Second level title"),
      h3("Third level title"),
      h4("Fourth level title"),
      h5("Fifth level title"),
      h6("Sixth level title")
    )
  )
)

# Define server logic

server <- function(input, output) {}

# RUn the app

shinyApp(ui = ui, server= server)

## star wars  (alignment)

ui <- fluidPage(
  titlePanel("My Star Wars App"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      h6("Episode IV", align = "center"),
      h6("A NEW HOPE", align = "center"),
      h5("It is a period of civil war.", align = "center"),
      h4("Rebel spaceships, striking", align = "center"),
      h3("from a hidden base, have won", align = "center"),
      h2("their first victory against the", align = "center"),
      h1("evil Galactic Empire.")
    )
  )
)

#### formatting text

ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      p("p creates a paragraph of text."),
      p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
      strong("strong() makes bold text."),
      em("em() creates italicized (i.e, emphasized) text."),
      br(),
      code("code displays your text similar to computer code"),
      div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
      br(),
      p("span does the same thing as div, but it works with",
        span("groups of words", style = "color:blue"),
        "that appear inside a paragraph.")
    )
  )
)

server <- function(input, output) {}

# RUn the app

shinyApp(ui = ui, server= server)

#### images

ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      img(src = "rstudio.png", height = 140, width = 400)
    )
  )
)

######## trying to run the doc

source("MasterScript.R")

# Define UI

lib

ui <- fluidPage(
  titlePanel("TIPS, Nominal Treasuries, I Bond, and Inflation Between 2003-2021"),
  
  sidebarLayout(
    sidebarPanel("sidebar panel"),
    mainPanel("main panel")
  )
  
)

# Define server logic

server <- function(input, output) {
  
  
}

# RUn the app

shinyApp(ui = ui, server= server)




####


# Define UI ----
ui <- fluidPage(
  titlePanel("Basic widgets"),
  
  fluidRow(
    
    column(3,
           h3("Buttons"),
           actionButton("action", "Action"),
           br(),
           br(), 
           submitButton("Submit")),
    
    column(3,
           h3("Single checkbox"),
           checkboxInput("checkbox", "Choice A", value = TRUE)),
    
    column(3, 
           checkboxGroupInput("checkGroup", 
                              h3("Checkbox group"), 
                              choices = list("Choice 1" = 1, 
                                             "Choice 2" = 2, 
                                             "Choice 3" = 3),
                              selected = 1)),
    
    column(3, 
           dateInput("date", 
                     h3("Date input"), 
                     value = "2014-01-01"))   
  ),
  
  fluidRow(
    
    column(3,
           dateRangeInput("dates", h3("Date range"))),
    
    column(3,
           fileInput("file", h3("File input"))),
    
    column(3, 
           h3("Help text"),
           helpText("Note: help text isn't a true widget,", 
                    "but it provides an easy way to add text to",
                    "accompany other widgets.")),
    
    column(3, 
           numericInput("num", 
                        h3("Numeric input"), 
                        value = 1))   
  ),
  
  fluidRow(
    
    column(3,
           radioButtons("radio", h3("Radio buttons"),
                        choices = list("Choice 1" = 1, "Choice 2" = 2,
                                       "Choice 3" = 3),selected = 1)),
    
    column(3,
           selectInput("select", h3("Select box"), 
                       choices = list("Choice 1" = 1, "Choice 2" = 2,
                                      "Choice 3" = 3), selected = 1)),
    
    column(3, 
           sliderInput("slider1", h3("Sliders"),
                       min = 0, max = 100, value = 50),
           sliderInput("slider2", "",
                       min = 0, max = 100, value = c(25, 75))
    ),
    
    column(3, 
           textInput("text", h3("Text input"), 
                     value = "Enter text..."))   
  )
  
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
