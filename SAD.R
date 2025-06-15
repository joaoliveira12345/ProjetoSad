# Pacotes necessários
library(shiny)
library(shinydashboard)
library(randomForest)
library(dplyr)
library(ggplot2)

# Geração de dados realistas
set.seed(123)
n <- 2000

gerar_dados <- function(n) {
  temperatura <- rnorm(n, 15, 8)
  humidade <- pmax(20, pmin(95, rnorm(n, 60, 15)))
  vento <- pmax(0, rnorm(n, 2.5, 1.5))
  radiacao <- pmax(0, rnorm(n, 0.7, 0.4))
  precipitacao <- pmax(0, rnorm(n, 2, 3))
  neve <- ifelse(temperatura < 3, pmax(0, rnorm(n, 1, 2)), 0)
  
  base_rentals <- 200
  temp_effect <- ifelse(temperatura > 25, 1.2,
                        ifelse(temperatura > 15, 1.5,
                               ifelse(temperatura > 5, 1.0, 0.3)))
  humidity_effect <- ifelse(humidade > 80, 0.7, 1.0)
  wind_effect <- ifelse(vento > 4, 0.6, 1.0)
  solar_effect <- 1 + (radiacao * 0.5)
  rain_effect <- pmax(0.2, 1 - (precipitacao * 0.1))
  snow_effect <- pmax(0.1, 1 - (neve * 0.2))
  
  rentals <- base_rentals * temp_effect * humidity_effect *
    wind_effect * solar_effect * rain_effect * snow_effect
  rentals <- pmax(0, round(rentals + rnorm(n, 0, 30)))
  
  data.frame(
    Rented_Bike_Count = rentals,
    Temperature = temperatura,
    Humidity = humidade,
    Wind_speed = vento,
    Solar_Radiation = radiacao,
    Rainfall = precipitacao,
    Snowfall = neve
  )
}

dados <- gerar_dados(n)

# Modelo Random Forest
modelo_rf <- randomForest(
  Rented_Bike_Count ~ .,
  data = dados,
  ntree = 500,
  importance = TRUE
)

# Interface Shiny
ui <- dashboardPage(
  dashboardHeader(title = "Previsão de Alugueres de Bicicletas"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Previsão", tabName = "previsao")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "previsao",
              fluidRow(
                box(title = "Condições Meteorológicas", status = "primary", width = 6,
                    numericInput("temp", "Temperatura (°C):", value = 20, min = -5, max = 38),
                    sliderInput("hum", "Humidade (%):", min = 20, max = 100, value = 60),
                    numericInput("wind", "Velocidade do Vento (km/h):", value = 2, min = 0, max = 40),
                    sliderInput("rad", "Radiação Solar (MJ/m²):", min = 0, max = 15, value = 0.7, step = 0.1),
                    numericInput("rain", "Precipitação (mm):", value = 0, min = 0, max = 20),
                    numericInput("snow", "Neve (cm):", value = 0, min = 0, max = 10),
                    actionButton("prever", "Calcular Previsão", class = "btn-primary")
                ),
                box(title = "Resultado da Previsão", status = "success", width = 6,
                    h3(textOutput("previsao")),
                    h4("Intervalo de Confiança:"),
                    textOutput("intervalo"),
                    br(),
                    plotOutput("grafico_previsao")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  previsao_resultado <- eventReactive(input$prever, {
    novos_dados <- data.frame(
      Temperature = input$temp,
      Humidity = input$hum,
      Wind_speed = input$wind,
      Solar_Radiation = input$rad,
      Rainfall = input$rain,
      Snowfall = input$snow
    )
    
    predicao <- predict(modelo_rf, newdata = novos_dados)
    
    erro_padrao <- sqrt(mean((dados$Rented_Bike_Count - predict(modelo_rf))^2))
    intervalo_inf <- max(0, predicao - 1.96 * erro_padrao)
    intervalo_sup <- predicao + 1.96 * erro_padrao
    
    list(
      predicao = round(predicao),
      intervalo_inf = round(intervalo_inf),
      intervalo_sup = round(intervalo_sup)
    )
  })
  
  output$previsao <- renderText({
    resultado <- previsao_resultado()
    paste("Previsão:", resultado$predicao, "bicicletas")
  })
  
  output$intervalo <- renderText({
    resultado <- previsao_resultado()
    paste("Entre", resultado$intervalo_inf, "e", resultado$intervalo_sup, "bicicletas")
  })
  
  output$grafico_previsao <- renderPlot({
    resultado <- previsao_resultado()
    dados_plot <- data.frame(
      Categoria = c("Mínimo", "Previsão", "Máximo"),
      Valor = c(resultado$intervalo_inf, resultado$predicao, resultado$intervalo_sup)
    )
    
    ggplot(dados_plot, aes(x = Categoria, y = Valor, fill = Categoria)) +
      geom_bar(stat = "identity") +
      labs(title = "Previsão com Intervalo de Confiança", y = "Número de Bicicletas", x = "") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)

