
library(shiny)
ui <- fluidPage(
  selectInput("stcd", label = "State Code", choices = st_codes_f(state_only = TRUE)$stcd, selected = "OR", multiple = FALSE),
  verbatimTextOutput("summary"),
  tableOutput("table")
)
server <- function(input, output, session) {
  
  output$table <- renderTable({
    dataset <- pm[["a"]]
    dataset %>% 
      filter(stcd == input$stcd) %>% 
      select(stcd, seat_counter, quota) 
  })
}
shinyApp(ui, server)