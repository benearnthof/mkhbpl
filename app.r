require(shiny) 
require(PlayerRatings) 
require(shinythemes)
ui <- fluidPage(theme = shinytheme("cosmo"),
  sidebarLayout(
    sidebarPanel("MKH-BPL",
      selectInput("winningteam", "Winning Team", 
                  choices = paste(rep("Floor", times = 9), 0:8) ),
      selectInput("losingteam", "Losing Team", 
                  choices = paste(rep("Floor", times = 9), 0:8) ), 
      actionButton("update", "Add Match!", icon = icon("beer")) ), 
    mainPanel(
      plotOutput("ratingsplot"),
      tableOutput("ratings"),
               ) 
    )
  ) 
server <- function(input, output) { 
  tmp <- data.frame(NULL, stringsAsFactors = FALSE) 
  
  matches <- eventReactive(input$update, { 
    if (input$winningteam == input$losingteam) return(tmp) 
    match <- nrow(tmp) + 1 
    hometeam <- input$winningteam 
    awayteam <- input$losingteam 
    score <- 1
    tmp <<- rbind(tmp, c(as.integer(match), hometeam, awayteam, score), 
                  stringsAsFactors = FALSE) 
    colnames(tmp) <- c("index", "hometeam", "awayteam", "score")
    dta <- tmp
    dta$index <- as.numeric(dta$index)
    dta$score <- as.numeric(dta$score)
    ratings <- fide(dta, init = 1200, history = TRUE)
    list(matchdata = tmp, ratings = ratings) 
    }) 
  
  output$ratings <- renderTable({
    matches()$ratings$ratings[,1:6]
    })
  
  output$ratingsplot <- renderPlot({
    if (nrow(matches()$matchdata) < 2) return(NULL)
    plot(matches()$ratings, main = "MKH - BEERPONG RATINGS", lty = 1, 
         lwd = 2)
  })
  
} 

shinyApp(ui, server)
