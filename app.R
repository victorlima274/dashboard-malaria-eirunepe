library(shiny)

ui <- fluidPage(
  titlePanel("Painel Epidemiológico da Malária — Eirunepé/AM"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filtros"),
      selectInput(
        "ano",
        "Ano:",
        choices = c("2026", "2025"),
        selected = "2026"
      )
    ),
    
    mainPanel(
      h3("Painel de teste"),
      p("O dashboard está funcionando."),
      hr(),
      h4("Próxima etapa"),
      p("Aqui vamos inserir os indicadores e gráficos da análise epidemiológica.")
    )
  )
)

server <- function(input, output, session) {
}

shinyApp(ui = ui, server = server)
