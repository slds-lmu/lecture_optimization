# Define UI for application that draws a histogram
ui = fluidPage(
  
  # Application title
  titlePanel("Evolutionäre Algorithmen"),
  
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      
      helpText(
        "Finde den Ball mit dem größten Radius, der keinen der grauen Bälle berührt!"
      ),
      tags$div("Hierbei"),
      tags$ul(
        tags$li(HTML("wählen wir Eltern zur Erzeugung von Nachkommen <strong>neutral</strong> aus,")),
        tags$li(HTML("verwenden wir im Mutationsschritt eine <strong>Gauss-Mutation</strong> mit Standardabweichung 2,")), 
        tags$li(HTML("und als Überlebensselektion wählen wir eine <strong>(mu + lambda)-Selektion</strong>."))
      ),
      tags$hr(),
      tags$h4(
        HTML("<strong>Initialisierung</strong>")
      ),
      # Input: number of initials balls
      sliderInput("nballs", paste("Anfangsgrid: "),
        min = 10, max = 50,
        value = 10),
      
      # Input: population size
      sliderInput("mu", paste("Größe der Anfangspopulation mu:"),
        min = 0, max = 50,
        value = 10),
      
      actionButton("create.population", "Initialisiere population"),
      
      br(),
      br(),
      br(),
      
      
      # Input: choose offspring
      sliderInput("lambda", paste("Wähle lambda:"),
        min = 0, max = 30,
        value = 3),
      
      actionButton("variation", "Mutation"),
      actionButton("selection", "Selektion"),
      
      HTML("<br>")),
      
      # Input: Animation with custom interval (in ms) ----
      # to control speed, plus looping

    
    mainPanel(
      sidebarPanel(tags$style(".well {background-color:#FFFFFF;}"),        
        uiOutput("slider_to_anim"),
        uiOutput("speed_value"),
        plotOutput("grid", width = 500, height = 500),
        width = 12
      )
      
    )
  )
)
