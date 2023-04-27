library(shiny)

ui <- fluidPage(
  titlePanel("Linear Functions"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("slope", "Slope:",
                  min = 0, max = 10,
                  value = 1),
      sliderInput("intercept", "Intercept:",
                  min = 0, max = 100,
                  value = 20, step = 10),
      sliderInput("xval", "X-value:",
                  min = 0, max = 10,
                  value = 5, step = 1)
    ),
    mainPanel(
      tableOutput("values"),
      "Your function: y = a + x * b",
      h3(textOutput("formula", container = span)),
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  sliderValues <- reactive({
    data.frame(
      Name = c("Slope","Intercept", "X-value"),
      Value = as.character(c(input$slope,input$intercept,input$xval)),
      stringsAsFactors = T)
  })
  
  vals <- reactiveValues()
  observe({
    vals$yval <- input$intercept + input$slope * input$xval
  })
    
      
    output$values <- renderTable({
      sliderValues()
    })
    
    output$formula <- renderText({
      paste(input$intercept + input$slope * input$xval,
            " = ",
            input$intercept,
            " + ",
            input$slope,
            "*",
            input$xval)
    })
    
    output$plot <- renderPlot({
      curve(input$slope*x + input$intercept, xlim=c(0, 10), ylim=c(0,100), xlab = "X", ylab = "Y")
      points(input$xval, vals$yval, pch = 16, cex = 2, col = "red")
    })
  
}

shinyApp(ui = ui, server = server)
