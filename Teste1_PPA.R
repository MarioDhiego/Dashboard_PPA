

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(DT)
library(readxl)
library(dplyr)

# Carregar os Dados
dados <- read_excel("BANCO_PPA.xlsx", sheet = "geral")
dados2 <- read_excel("BANCO_PPA.xlsx", sheet = "Desdobramento")


# Interface
ui <- dashboardPage(
  dashboardHeader(title = "Indicadores Financeiros e Físicos (PPA)",
                  titleWidth = 400),
  
  dashboardSidebar(
    useShinyjs(),
    selectInput("setor", "Setor:", choices = sort(unique(dados$SETOR)), selected = NULL),
    selectInput("ano", "Ano:", choices = sort(unique(dados$ANO)), selected = NULL),
    actionButton("reset", "Resetar Filtros", icon = icon("redo"))
  ),
  
  dashboardBody(
    fluidRow(
      valueBoxOutput("box_fin_prev", width = 3),
      valueBoxOutput("box_fin_real", width = 3),
      valueBoxOutput("box_fis_prev", width = 3),
      valueBoxOutput("box_fis_real", width = 3)
    ),
    br(),
    fluidRow(
      column(12, withSpinner(DTOutput("tabela_fisico")))
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  # Resetar os filtros
  observeEvent(input$reset, {
    updateSelectInput(session, "setor", choices = sort(unique(dados$SETOR)), selected = NULL)
    updateSelectInput(session, "ano", choices = sort(unique(dados$ANO)), selected = NULL)
  })
  
  
  # Dados filtrados
  dados_filtrados <- reactive({
    req(input$setor, input$ano)
    dados %>%
      filter(
        SETOR == input$setor,
        ANO == input$ano
      )
  })
  
  # Notificação se não houver dados
  observe({
    req(input$setor, input$ano)
    if (nrow(dados_filtrados()) == 0) {
      showNotification("Nenhum dado encontrado para o setor e ano selecionados.", type = "warning")
    }
  })
  
  # Caixas de valor
  output$box_fin_prev <- renderValueBox({
    total <- sum(dados_filtrados()$FINANCEIRO_PREVISTO, na.rm = TRUE)
    icon_color <- ifelse(total > 1000000, "green", "blue")  # Exemplo de mudança de ícone com base no valor
    valueBox(
      subtitle = "Execução Financeira (Previsto)",
      value = paste0("R$ ", formatC(total, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)),
      icon = icon("money-bill", lib = "font-awesome"),
      color = "blue"
    )
  })
  
  output$box_fin_real <- renderValueBox({
    total <- sum(dados_filtrados()$FINANCEIRO_REALIZADO, na.rm = TRUE)
    icon_color <- ifelse(total > 1000000, "green", "blue")  # Exemplo de mudança de ícone com base no valor
    valueBox(
      subtitle = "Execução Financeira (Realizado)",
      value = paste0("R$ ", formatC(total, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)),
      icon = icon("check-circle", lib = "font-awesome"),
      color = "green"
    )
  })
  
  output$box_fis_prev <- renderValueBox({
    total <- sum(dados_filtrados()$FISICO_PREVISTO, na.rm = TRUE)
    valueBox(
      subtitle = "Execução Física (Previsto)",
      value = formatC(total, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
      icon = icon("chart-line", lib = "font-awesome"),
      color = "teal"
    )
  })
  
  output$box_fis_real <- renderValueBox({
    total <- sum(dados_filtrados()$FISICO_REALIZADO, na.rm = TRUE)
    valueBox(
      subtitle = "Execução Física (Realizado)",
      value = formatC(total, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
      icon = icon("chart-bar", lib = "font-awesome"),
      color = "orange"
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
    
    DT::datatable(dados_fisico, 
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
