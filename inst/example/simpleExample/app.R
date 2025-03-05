library(shiny)

# Define UI for application
ui <- fluidPage(
    ShinyLocalStorage::useShinyLocalStorage(),
    titlePanel("Simple Shiny App"),
    sidebarLayout(
        sidebarPanel(
            textInput("key", "Enter key:", ""),
            textInput("val", "Enter value:", ""),
            actionButton("store", "Store"),
            actionButton("retrieve", "Retrieve"),
            actionButton("remove", "Remove"),
            actionButton("clear", "Clear")
        ),
        mainPanel(
            h4("Stored Value"),
            textOutput("output")
        )
    )
)

# Define server logic
server <- function(input, output) {
    ShinyLocalStorage::configureShinyLocalStorage("simpleExample")

    observeEvent(input$store, {
        ShinyLocalStorage::store(input$key, input$val)
    })

    observeEvent(input$remove, {
        ShinyLocalStorage::remove(input$key)
    })
    observeEvent(input$retrieve, {
        output$output <- renderText({
            ShinyLocalStorage::retrieve(input$key)
        })
    })

    observeEvent(input$clear, {
        ShinyLocalStorage::clear()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
