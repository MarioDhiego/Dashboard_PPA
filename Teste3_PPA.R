


#----------------------------------- PACOTES ----------------------------------#
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(dashboardthemes)
library(DT)
library(readxl)
library(dplyr)
#------------------------------------------------------------------------------#

#---------------------------------- DataFrame ---------------------------------#
dados <- read_excel("BANCO_PPA.xlsx", sheet = 1)             
dados_desdobramento <- read_excel("BANCO_PPA.xlsx", sheet = 2)  
dados_atividade <- read_excel("BANCO_PPA.xlsx", sheet = 3)  
#------------------------------------------------------------------------------#


# Limpar espaços em branco extras
dados$SETOR <- trimws(dados$SETOR)
dados_desdobramento$SETOR <- trimws(dados_desdobramento$SETOR)
dados_desdobramento$REGIAO <- trimws(dados_desdobramento$REGIAO)

# Interface
ui <- dashboardPage(
  dashboardHeader(title = "PLANO PLURIANUAL 2020-2024", titleWidth = 400),
  dashboardSidebar(
    tags$img(src = "detran1.jpeg", width = 230, height = 140),
    useShinyjs(),
    selectInput("setor", "SETOR:", choices = sort(unique(dados$SETOR)), sort(unique(dados$SETOR))[1]),
    selectInput("ano", "ANO:", choices = sort(unique(dados$ANO)), selected = 2024),
    actionButton("reset", "LIMPAR FILTROS", icon = icon("redo"), class = "btn btn-warning")
  ),
  dashboardBody(
    shinyDashboardThemes(theme = "blue_gradient"),
    tabsetPanel(
      tabPanel("INDICADORES", icon = icon("car"),
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
      tabPanel("MUNICÍPIOS ATENDIDOS", icon = icon("globe"),
               fluidRow(
                 column(2, selectInput("setor_desdob", "SETOR:", choices = NULL)),
                 column(3, selectInput("regiao_desdob", "REGIÃO INTEGRAÇÃO:", choices = NULL, selected = 'BAIXO AMAZONAS')),
                 column(2, selectInput("ano_desdob", "ANO:", choices = NULL, selected = 2024)),
                 column(3, actionButton("reset_desdob", "LIMPAR FILTROS", icon = icon("redo")))
               ),
               br(),
               fluidRow(
                 column(12, withSpinner(DTOutput("tabela_desdobramento")))
               )
      ),
      tabPanel("ATIVIDADES", icon = icon("calendar-check"),
               fluidRow(
                 column(2, selectInput("setor_atividade", "SETOR:", choices = NULL)),
                 column(3, selectInput("regiao_atividade", "REGIÃO INTEGRAÇÃO:", choices = NULL, selected = 'BAIXO AMAZONAS')),
                 column(2, selectInput("ano_atividade", "ANO:", choices = NULL, selected = 2024)),
                 column(2, selectInput("municipio_atividade", "MUNICÍPIO:", choices = NULL, selected = 'SANTARÉM')),
                 column(3, actionButton("reset_atividade", "LIMPAR FILTROS", icon = icon("redo")))
               ),
               br(),
               fluidRow(
                 # Linha separada para o valueBox
                 # valueBoxOutput("pessoas_atendidas", width = 12)  # Ocupa toda a linha
               ),
               br(),
               fluidRow(
                 column(12, withSpinner(DTOutput("tabela_atividade")))
               )
      )
      
      
      
      
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  observeEvent(input$reset, {
    updateSelectInput(session, "setor", choices = sort(unique(dados$SETOR)), selected = NULL)
    updateSelectInput(session, "ano", choices = sort(unique(dados$ANO)), selected = 2024)
  })
  
  dados_filtrados <- reactive({
    req(input$setor, input$ano)
    dados %>% filter(SETOR == input$setor, ANO == input$ano)
  })
  
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
  
  observe({
    updateSelectInput(session, "setor_desdob", choices = sort(unique(dados_desdobramento$SETOR)), selected = 'EDUCAÇÃO')
    updateSelectInput(session, "regiao_desdob", choices = sort(unique(dados_desdobramento$REGIAO)), selected = 'BAIXO AMAZONAS')
    updateSelectInput(session, "ano_desdob", choices = sort(unique(dados_desdobramento$ANO)), selected = 2024)
  })
  
  observeEvent(input$reset_desdob, {
    updateSelectInput(session, "setor_desdob", choices = sort(unique(dados_desdobramento$SETOR)), selected = 'EDUCAÇÃO')
    updateSelectInput(session, "regiao_desdob", choices = sort(unique(dados_desdobramento$REGIAO)), selected = 'BAIXO AMAZONAS')
    updateSelectInput(session, "ano_desdob", choices = sort(unique(dados_desdobramento$ANO)), selected = 2024)
  })
  
  #--------------------------------------- CAIXAS DE VALORES ---------------------------------------------------#
  # Cálculo do total de atividades filtradas
  
  #output$total_atividades <- renderValueBox({
  #   total_atividades <- nrow(dados_atividade_filtrados())  # Total de atividades filtradas
  #  valueBox(total_atividades,
  #           "Atividades Realizadas",
  #           icon = icon("calendar-check"),
  #           color = "blue", 
  #           width = 2)  # Ocupa toda a largura disponível
  # })
  
  
  output$box_fin_prev <- renderValueBox({
    total <- sum(dados_filtrados()$FINANCEIRO_PREVISTO, na.rm = TRUE)
    valueBox("R$ " %>% paste0(formatC(total, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)),
             "Execução Financeira - Previsto (R$)", icon = icon("money-bill"), color = "blue")
  })
  
  output$box_fin_real <- renderValueBox({
    total <- sum(dados_filtrados()$FINANCEIRO_REALIZADO, na.rm = TRUE)
    valueBox("R$ " %>% paste0(formatC(total, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)),
             "Execução Financeira - Realizado (R$)", icon = icon("check-circle"), color = "green")
  })
  
  output$box_fis_prev <- renderValueBox({
    total <- sum(dados_filtrados()$FISICO_PREVISTO, na.rm = TRUE)
    valueBox(formatC(total, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
             "Execução Física - Previsto (un)", icon = icon("chart-line"), color = "teal")
  })
  
  output$box_fis_real <- renderValueBox({
    total <- sum(dados_filtrados()$FISICO_REALIZADO, na.rm = TRUE)
    valueBox(formatC(total, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
             "Execução Física - Realizado (un)", icon = icon("chart-bar"), color = "orange")
  })
  #------------------------------------------------------------------------------------------------------------#
  
  
  #----------------- TABELA INDICADORES ---------------------------------------------------------------------#
  output$tabela_fisico <- renderDT({
    req(nrow(dados_filtrados()) > 0)
    dados_filtrados() %>%
      mutate(
        PERCENTUAL_FISICO = ifelse(FISICO_PREVISTO > 0, 100 * FISICO_REALIZADO / FISICO_PREVISTO, NA),
        PERCENTUAL_FINANCEIRO = ifelse(FINANCEIRO_PREVISTO > 0, 100 * FINANCEIRO_REALIZADO / FINANCEIRO_PREVISTO, NA)
      ) %>%
      select(REGIÃO, FINANCEIRO_PREVISTO, FINANCEIRO_REALIZADO, PERCENTUAL_FINANCEIRO,
             FISICO_PREVISTO, FISICO_REALIZADO, PERCENTUAL_FISICO) %>%
      datatable(extensions = 'Buttons',
                options = list(pageLength = 12, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
                colnames = c("Região", "Financeiro Previsto", "Financeiro Realizado", "Execução Financeira (%)",
                             "Físico Previsto", "Físico Realizado", "Execução Física (%)")) %>%
      formatRound("PERCENTUAL_FINANCEIRO", 0) %>%
      formatRound("PERCENTUAL_FISICO", 0) %>%
      formatStyle("PERCENTUAL_FINANCEIRO", background = styleColorBar(range(0, 100), 'lightblue'),
                  backgroundSize = '100% 90%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
      formatStyle("PERCENTUAL_FISICO", background = styleColorBar(range(0, 100), 'lightgreen'),
                  backgroundSize = '100% 90%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center')
  })
  
  #----------------- TABELA MUNICPIPIOS ATENDIDOS -------------------------------------------------------------#
  output$tabela_desdobramento <- renderDT({
    df <- dados_desdobramento_filtrados()
    if (nrow(df) == 0) {
      showNotification("NENHUMA INFORMAÇÃO para os filtros selecionados.", type = "warning")
      return(NULL)
    }
    total_row <- df %>% select_if(is.numeric) %>% summarise(across(everything(), ~sum(., na.rm = TRUE))) %>%
      mutate(SETOR = "Total", REGIAO = "Total", ANO = "Total")
    df$ANO <- as.character(df$ANO)
    total_row$ANO <- as.character(total_row$ANO)
    df_com_total <- bind_rows(df, total_row) %>% select(-SETOR, -ANO)
    datatable(df_com_total, extensions = 'Buttons', options = list(pageLength = 20, dom = 'Bfrtip',
                                                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                                   scrollX = TRUE))
  })
  #-----------------------------------------------------------------------------------------------------------------#
  observe({
    updateSelectInput(session, "setor_atividade", choices = sort(unique(dados_atividade$SETOR)), selected = NULL)
    updateSelectInput(session, "regiao_atividade", choices = sort(unique(dados_atividade$REGIAO)), selected = NULL)
    updateSelectInput(session, "ano_atividade", choices = sort(unique(dados_atividade$ANO)), selected = 2024)
    updateSelectInput(session, "municipio_atividade", choices = sort(unique(dados_atividade$MUNICIPIO)), selected = 'SANTARÉM')  # Novo filtro
  })
  
  observeEvent(input$reset_atividade, {
    updateSelectInput(session, "setor_atividade", choices = sort(unique(dados_atividade$SETOR)), selected = NULL)
    updateSelectInput(session, "regiao_atividade", choices = sort(unique(dados_atividade$REGIAO)), selected = NULL)
    updateSelectInput(session, "ano_atividade", choices = sort(unique(dados_atividade$ANO)), selected = 2024)
    updateSelectInput(session, "municipio_atividade", choices = sort(unique(dados_atividade$MUNICIPIO)), selected = 'SANTARÉM')  # Novo filtro
  })
  
  dados_atividade_filtrados <- reactive({
    df <- dados_atividade
    if (!is.null(input$setor_atividade) && input$setor_atividade != "") {
      df <- df %>% filter(SETOR == input$setor_atividade)
    }
    if (!is.null(input$regiao_atividade) && input$regiao_atividade != "") {
      df <- df %>% filter(REGIAO == input$regiao_atividade)
    }
    if (!is.null(input$ano_atividade) && input$ano_atividade != "") {
      df <- df %>% filter(ANO == input$ano_atividade)
    }
    if (!is.null(input$municipio_atividade) && input$municipio_atividade != "") {  # Filtro de município
      df <- df %>% filter(MUNICIPIO == input$municipio_atividade)
    }
    df
  })
  
  #----------------- TABELA ATIVIDADES ----------------------------------------------------------------------#
  output$tabela_atividade <- renderDT({
    df <- dados_atividade_filtrados()
    if (nrow(df) == 0) {
      showNotification("NENHUMA INFORMAÇÃO para os filtros selecionados.", type = "warning")
      return(NULL)
    }
    
    # Corrigido para ATIVIDADE
    df <- df %>%
      mutate(ATIVIDADE = case_when(
        ATIVIDADE == "Palestra" ~ '<span class="badge badge-primary">Palestra</span>',
        ATIVIDADE == "Curso" ~ '<span class="badge badge-success">Curso</span>',
        ATIVIDADE == "Ação Educativa" ~ '<span class="badge badge-warning">Ação Educativa</span>',
        ATIVIDADE == "Reunião Institucional" ~ '<span class="badge badge-secondary">Reunião</span>',
        TRUE ~ ATIVIDADE
      ))
    
    total_row <- df %>% 
      select_if(is.numeric) %>% 
      summarise(across(everything(), ~sum(., na.rm = TRUE))) %>%
      mutate(SETOR = "Total", REGIAO = "Total", ANO = "Total")
    
    df$ANO <- as.character(df$ANO)
    total_row$ANO <- as.character(total_row$ANO)
    
    df_com_total <- bind_rows(df, total_row) %>% select(-SETOR, -ANO, -REGIAO)
    
    datatable(df_com_total, 
              escape = FALSE, # importante deixar FALSE para renderizar HTML dos badges
              extensions = 'Buttons', 
              options = list(pageLength = 20, dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), 
                             scrollX = TRUE,
                             scrollY = "400px",  # Adicionando rolagem vertical
                             searching = TRUE  # Habilita a pesquisa
              ))
  })
  #------------------------------------------------------------------------------#  
  
  
  
}

# Rodar o app
shinyApp(ui, server)
