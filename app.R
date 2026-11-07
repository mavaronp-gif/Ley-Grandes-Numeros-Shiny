
# ============================================================
# LEY DE LOS GRANDES NÚMEROS
# Simulación del lanzamiento de un dado
# Aplicación desarrollada con R Shiny
# ============================================================

library(shiny)
library(ggplot2)
library(dplyr)

# ------------------------------------------------------------
# INTERFAZ DE USUARIO
# ------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Ley de los Grandes Números: lanzamiento de un dado"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Configuración de la simulación"),
      
      sliderInput(
        inputId = "n",
        label = "Número de lanzamientos:",
        min = 10,
        max = 100000,
        value = 1000,
        step = 10
      ),
      
      actionButton(
        inputId = "simular",
        label = "Simular lanzamientos",
        icon = icon("play"),
        class = "btn-primary"
      ),
      
      br(), br(),
      
      actionButton(
        inputId = "reiniciar",
        label = "Reiniciar simulación",
        icon = icon("refresh")
      ),
      
      hr(),
      
      h4("Esperanza matemática"),
      
      h3("E(X) = 3.5"),
      
      p("Dado equilibrado de seis caras."),
      
      hr(),
      
      h4("Resultados de la simulación"),
      
      verbatimTextOutput("resumen")
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          "Media acumulada",
          
          br(),
          
          h4("Convergencia de la media muestral"),
          
          plotOutput("grafico_media", height = "420px")
        ),
        
        tabPanel(
          "Frecuencias",
          
          br(),
          
          h4("Frecuencia relativa de cada cara"),
          
          plotOutput("grafico_frecuencias", height = "420px")
        ),
        
        tabPanel(
          "Resultados",
          
          br(),
          
          h4("Resultados de los lanzamientos"),
          
          tableOutput("tabla")
        )
      )
    )
  )
)

# ------------------------------------------------------------
# SERVIDOR
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # Valores iniciales
  resultados <- reactiveVal(NULL)
  
  # Función para simular lanzamientos
  simular_dado <- function(n) {
    
    data.frame(
      Lanzamiento = 1:n,
      Resultado = sample(1:6, size = n, replace = TRUE)
    ) |>
      mutate(
        Media_acumulada = cumsum(Resultado) / Lanzamiento
      )
  }
  
  # Simulación inicial
  observe({
    
    if (is.null(resultados())) {
      resultados(simular_dado(1000))
    }
    
  })
  
  # Ejecutar simulación
  observeEvent(input$simular, {
    
    resultados(simular_dado(input$n))
    
  })
  
  # Reiniciar
  observeEvent(input$reiniciar, {
    
    resultados(simular_dado(1000))
    
    updateSliderInput(
      session,
      "n",
      value = 1000
    )
    
  })
  
  # Datos reactivos
  datos <- reactive({
    req(resultados())
    resultados()
  })
  
  # ----------------------------------------------------------
  # RESUMEN ESTADÍSTICO
  # ----------------------------------------------------------
  
  output$resumen <- renderPrint({
    
    x <- datos()
    
    cat("Número de lanzamientos:", nrow(x), "\n")
    cat("Media muestral:", round(mean(x$Resultado), 4), "\n")
    cat("Esperanza matemática: 3.5\n")
    cat(
      "Diferencia absoluta:",
      round(abs(mean(x$Resultado) - 3.5), 4),
      "\n"
    )
    
    cat(
      "Desviación estándar:",
      round(sd(x$Resultado), 4)
    )
    
  })
  
  # ----------------------------------------------------------
  # GRÁFICO DE LA MEDIA ACUMULADA
  # ----------------------------------------------------------
  
  output$grafico_media <- renderPlot({
    
    x <- datos()
    
    ggplot(x, aes(
      x = Lanzamiento,
      y = Media_acumulada
    )) +
      
      geom_line(
        color = "#2878B5",
        linewidth = 0.7
      ) +
      
      geom_hline(
        yintercept = 3.5,
        color = "red",
        linetype = "dashed",
        linewidth = 1
      ) +
      
      labs(
        title = "Ley de los Grandes Números",
        subtitle = paste(
          "Número de lanzamientos:",
          nrow(x)
        ),
        x = "Número de lanzamientos",
        y = "Media muestral acumulada"
      ) +
      
      scale_y_continuous(
        breaks = seq(1, 6, 0.5)
      ) +
      
      theme_minimal(base_size = 14) +
      
      theme(
        plot.title = element_text(
          face = "bold"
        )
      )
    
  })
  
  # ----------------------------------------------------------
  # GRÁFICO DE FRECUENCIAS RELATIVAS
  # ----------------------------------------------------------
  
  output$grafico_frecuencias <- renderPlot({
    
    x <- datos()
    
    frecuencias <- x |>
      count(Resultado) |>
      mutate(
        Frecuencia_relativa = n / sum(n),
        Cara = factor(Resultado)
      )
    
    ggplot(
      frecuencias,
      aes(
        x = Cara,
        y = Frecuencia_relativa
      )
    ) +
      
      geom_col(
        fill = "#2878B5",
        width = 0.65
      ) +
      
      geom_hline(
        yintercept = 1/6,
        color = "red",
        linetype = "dashed",
        linewidth = 1
      ) +
      
      labs(
        title = "Distribución de las frecuencias relativas",
        subtitle = "La línea roja representa la probabilidad teórica (1/6)",
        x = "Cara del dado",
        y = "Frecuencia relativa"
      ) +
      
      scale_y_continuous(
        labels = scales::percent
      ) +
      
      theme_minimal(base_size = 14)
    
  })
  
  # ----------------------------------------------------------
  # TABLA DE RESULTADOS
  # ----------------------------------------------------------
  
  output$tabla <- renderTable({
    
    head(datos(), 100)
    
  })
  
}

# ------------------------------------------------------------
# EJECUTAR APLICACIÓN
# ------------------------------------------------------------

shinyApp(ui = ui, server = server)
