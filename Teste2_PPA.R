
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(DT)
library(readxl)
library(dplyr)

# Carregar os dados
dados <- read_excel("BANCO_PPA.xlsx", sheet = 1)  # Planilha principal
dados_desdobramento <- read_excel("BANCO_PPA.xlsx", sheet = "Desdobramento")  # Nova aba

# Limpar espaços em branco extras (boa prática)
dados$SETOR <- trimws(dados$SETOR)
dados_desdobramento$SETOR <- trimws(dados_desdobramento$SETOR)
dados_desdobramento$REGIAO <- trimws(dados_desdobramento$REGIAO)

# Interface
ui <- dashboardPage(
  dashboardHeader(title = "PLANO PLURIANUAL 2021-2024", titleWidth = 400),
  
  dashboardSidebar(
                   tags$img(src = "detran1.jpeg", 
                            #src = "Governo_Para.png",
                            width = 230, 
                            heigth = 120),
    useShinyjs(),
    selectInput("setor", "Setor:", choices = sort(unique(dados$SETOR)), selected = NULL),
    selectInput("ano", "Ano:", choices = sort(unique(dados$ANO)), selected = NULL),
    actionButton("reset", "LIMPAR FILTROS", icon = icon("redo"))
  ),
  
  dashboardBody(
    tabsetPanel(
      tabPanel("INDICADORES",
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
      ),
      tabPanel("MUNICÍPIOS ATENDIDOS",
               fluidRow(
                 column(3,
                        selectInput("setor_desdob", "Setor:", choices = NULL, selected = NULL)
                 ),
                 
                 column(3,
                        selectInput("regiao_desdob", "Região:", choices = NULL, selected = NULL)
                 ),
                 column(3,
                        selectInput("ano_desdob", "Ano:", choices = NULL, selected = NULL)
                 ),
                 column(3,
                        actionButton("reset_desdob", "Resetar Filtros Desdobramento", icon = icon("redo"))
                 )
               ),
               br(),
               fluidRow(
                 column(12, withSpinner(DTOutput("tabela_desdobramento")))
               )
      )
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
  
  # Dados filtrados - principal
  dados_filtrados <- reactive({
    req(input$setor, input$ano)
    dados %>%
      filter(SETOR == input$setor, 
             ANO == input$ano)
  })
  
  # Dados filtrados - desdobramento (CORRIGIDO)
  dados_desdobramento_filtrados <- reactive({
    df <- dados_desdobramento
    
    if (!is.null(input$setor_desdob) && input$setor_desdob != "") {
      df <- df %>% filter(SETOR == input$setor_desdob)
    }
    if (!is.null(input$regiao_desdob) && input$regiao_desdob != "") {
      df <- df %>% filter(REGIAO == input$regiao_desdob)
    }
    if (!is.null(input$ano_desdob) && input$ano_desdob != "") {
      df <- df %>% filter(ANO == input$ano_desdob)
    }
    
    df
  })
  
  # Atualizar os filtros de REGIÃO e ANO na aba 'Desdobramento'
  observe({
    updateSelectInput(session, "setor_desdob", 
                      choices = sort(unique(dados_desdobramento$SETOR)), 
                      selected = NULL)
    
    updateSelectInput(session, "regiao_desdob", 
                      choices = sort(unique(dados_desdobramento$REGIAO)), 
                      selected = NULL)
    
    updateSelectInput(session, "ano_desdob", 
                      choices = sort(unique(dados_desdobramento$ANO)), 
                      selected = NULL)
  })
  
  # Resetar os filtros de "Desdobramento"
  observeEvent(input$reset_desdob, {
    updateSelectInput(session, "setor_desdob", choices = sort(unique(dados_desdobramento$SETOR)), selected = NULL)
    updateSelectInput(session, "regiao_desdob", choices = sort(unique(dados_desdobramento$REGIAO)), selected = NULL)
    updateSelectInput(session, "ano_desdob", choices = sort(unique(dados_desdobramento$ANO)), selected = NULL)
  })
  
  # Caixas de valor
  output$box_fin_prev <- renderValueBox({
    total <- sum(dados_filtrados()$FINANCEIRO_PREVISTO, na.rm = TRUE)
    valueBox(
      subtitle = "Execução Financeira - Previsto (R$)",
      value = paste0("R$ ", formatC(total, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)),
      icon = icon("money-bill", lib = "font-awesome"),
      color = "blue"
    )
  })
  
  output$box_fin_real <- renderValueBox({
    total <- sum(dados_filtrados()$FINANCEIRO_REALIZADO, na.rm = TRUE)
    valueBox(
      subtitle = "Execução Financeira - Realizado (R$)",
      value = paste0("R$ ", formatC(total, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)),
      icon = icon("check-circle", lib = "font-awesome"),
      color = "green"
    )
  })
  
  output$box_fis_prev <- renderValueBox({
    total <- sum(dados_filtrados()$FISICO_PREVISTO, na.rm = TRUE)
    valueBox(
      subtitle = "Execução Física - Previsto (un)",
      value = formatC(total, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
      icon = icon("chart-line", lib = "font-awesome"),
      color = "teal"
    )
  })
  
  output$box_fis_real <- renderValueBox({
    total <- sum(dados_filtrados()$FISICO_REALIZADO, na.rm = TRUE)
    valueBox(
      subtitle = "Execução Física - Realizado (un)",
      value = formatC(total, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
      icon = icon("chart-bar", lib = "font-awesome"),
      color = "orange"
    )
  })
  
  # Tabela física principal
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
                           "Financeiro Previsto", 
                           "Financeiro Realizado", 
                           "Execução Financeira (%)",
                           "Físico Previsto", 
                           "Físico Realizado", 
                           "Execução Física (%)")) %>%
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
  
  # Tabela Desdobramento com total
  output$tabela_desdobramento <- renderDT({
    df <- dados_desdobramento_filtrados()
    
    if (nrow(df) == 0) {
      showNotification("NENHUMA INFORMAÇÃO para os filtros selecionados.", type = "warning")
      return(NULL)
    }
    
    # Calcular o total das colunas numéricas
    total_row <- df %>%
      select_if(is.numeric) %>%  
      summarise(across(everything(), ~sum(. , na.rm = TRUE))) %>%
      mutate(SETOR = "Total", 
             REGIAO = "Total", 
             ANO = "Total")
    
    df$ANO <- as.character(df$ANO)
    total_row$ANO <- as.character(total_row$ANO)
    
    df_com_total <- bind_rows(df, total_row)
    
    datatable(df_com_total,
              extensions = 'Buttons',
              options = list(
                pageLength = 20,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                scrollX = TRUE
              ))
  })
  
  
  
  
  
  
}

# Rodar o app
shinyApp(ui, server)
