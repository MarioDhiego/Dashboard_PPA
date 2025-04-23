

library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(bslib)
library(dplyr)
library(readxl)
library(DT)

# Carregar os dados
dados <- read_excel("BANCO_PPA.xlsx")

# Interface
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "materia"),
  useShinyjs(),
  titlePanel("PPA - Indicadores Financeiros e Físicos"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("setor", "Setor:", choices = sort(unique(dados$SETOR)), selected = NULL),
      selectInput("ano", "Ano:", choices = sort(unique(dados$ANO)), selected = NULL)
    ),
    
    mainPanel(
      fluidRow(
        column(3, uiOutput("box_fin_prev")),
        column(3, uiOutput("box_fin_real")),
        column(3, uiOutput("box_fis_prev")),
        column(3, uiOutput("box_fis_real"))
      ),
      br(),
      fluidRow(
        column(12, withSpinner(DTOutput("tabela_fisico")))
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  # Dados filtrados
  dados_filtrados <- reactive({
    dados %>%
      filter(
        SETOR == input$setor,
        ANO %in% input$ano
      )
  })
  
  # Notificação caso não haja dados
  observe({
    req(input$setor, input$ano)
    if (nrow(dados_filtrados()) == 0) {
      showNotification("Nenhum dado encontrado para o setor e ano selecionados.", type = "warning")
    }
  })
  
  # Caixas de valor
  output$box_fin_prev <- renderUI({
    total <- sum(dados_filtrados()$FINANCEIRO_PREVISTO, na.rm = TRUE)
    div(class = "card bg-primary text-white p-3 rounded-3 shadow",
        h5("Execução Financeira (Previsto)"),
        h3(formatC(total, format = "f", big.mark = ".", decimal.mark = ","))
    )
  })
  
  output$box_fin_real <- renderUI({
    total <- sum(dados_filtrados()$FINANCEIRO_REALIZADO, na.rm = TRUE)
    div(class = "card bg-success text-white p-3 rounded-3 shadow",
        h5("Execução Financeira (Realizado)"),
        h3(formatC(total, format = "f", big.mark = ".", decimal.mark = ","))
    )
  })
  
  output$box_fis_prev <- renderUI({
    total <- sum(dados_filtrados()$FISICO_PREVISTO, na.rm = TRUE)
    div(class = "card bg-info text-white p-3 rounded-3 shadow",
        h5("Execução Física (Previsto)"),
        h3(formatC(total, format = "f", big.mark = ".", decimal.mark = ",", digits = 0))
    )
  })
  
  output$box_fis_real <- renderUI({
    total <- sum(dados_filtrados()$FISICO_REALIZADO, na.rm = TRUE)
    div(class = "card bg-warning text-dark p-3 rounded-3 shadow",
        h5("Execução Física (Realizado)"),
        h3(formatC(total, format = "f", big.mark = ".", decimal.mark = ",", digits = 0))
    )
  })
  
  # Tabela interativa
  output$tabela_fisico <- renderDT({
    req(nrow(dados_filtrados()) > 0)
    
    dados_fisico <- dados_filtrados() %>%
      mutate(
        PERCENTUAL_FISICO = ifelse(FISICO_PREVISTO > 0, 100 * FISICO_REALIZADO / FISICO_PREVISTO, NA),
        PERCENTUAL_FINANCEIRO = ifelse(FINANCEIRO_PREVISTO > 0, 100 * FINANCEIRO_REALIZADO / FINANCEIRO_PREVISTO, NA)
      ) %>%
      select(
        REGIÃO, 
        FINANCEIRO_PREVISTO,
        FINANCEIRO_REALIZADO,
        PERCENTUAL_FINANCEIRO,
        FISICO_PREVISTO, 
        FISICO_REALIZADO,
        PERCENTUAL_FISICO
      )
    
    datatable(dados_fisico, 
              extensions = 'Buttons',
              options = list(
                pageLength = 12,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
              ),
              colnames = c("Região", 
                           "Financeiro Previsto", "Financeiro Realizado", "Execução Financeira (%)",
                           "Físico Previsto", "Físico Realizado", "Execução Física (%)")) %>%
      formatRound("PERCENTUAL_FINANCEIRO", 0) %>%
      formatRound("PERCENTUAL_FISICO", 0) %>%
      formatStyle(
        "PERCENTUAL_FINANCEIRO",
        background = styleColorBar(range(0, 100), 'lightblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        "PERCENTUAL_FISICO",
        background = styleColorBar(range(0, 100), 'lightgreen'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
}

# Rodar o app
shinyApp(ui, server)
