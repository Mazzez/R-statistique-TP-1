# ============================================================================
# APPLICATION R SHINY - VERSION STYLÉE COMPATIBLE
# ============================================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)  # thèmes Bootstrap simples et stables

# Charger les données
setwd("~/Desktop/R-stat/TP-1")
don <- read.csv2("don_mls.csv", header = TRUE, sep = ";", dec = ",")
names(don) <- c("natalite", "urbanisation")
don$natalite      <- as.numeric(as.character(don$natalite))
don$urbanisation  <- as.numeric(as.character(don$urbanisation))

# ============================================================================
# UI AVEC STYLE MODERNE (THEME FLATLY)
# ============================================================================

ui <- navbarPage(
  title = "📊 Natalité vs Urbanisation",
  theme = shinytheme("cerulean"),  # tu peux le Changer par "flatly", "cosmo", "darkly", "yeti", etc.
  
  # TAB 1 : Dashboard
  tabPanel("🎯 Dashboard",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               tags$h4("🔧 Filtres"),
               tags$hr(),
               
               sliderInput("natalite_range",
                           "Taux de natalité (%)",
                           min = floor(min(don$natalite)),
                           max = ceiling(max(don$natalite)),
                           value = c(floor(min(don$natalite)),
                                     ceiling(max(don$natalite))),
                           step = 1),
               
               sliderInput("urbanisation_range",
                           "Taux d'urbanisation (%)",
                           min = floor(min(don$urbanisation)),
                           max = ceiling(max(don$urbanisation)),
                           value = c(floor(min(don$urbanisation)),
                                     ceiling(max(don$urbanisation))),
                           step = 1),
               
               tags$hr(),
               
               checkboxGroupInput(
                 "plots_to_show", "Graphiques à afficher :",
                 choices = c("Histogrammes"     = "hist",
                             "Boxplots"         = "box",
                             "Nuage de points"  = "scatter",
                             "Régression"       = "reg",
                             "Diagnostics"      = "diag"),
                 selected = c("hist","box","scatter","reg")
               ),
               
               tags$hr(),
               actionButton("reset", "🔄 Réinitialiser", 
                            class = "btn btn-primary btn-block")
             ),
             
             mainPanel(
               width = 9,
               fluidRow(
                 column(6, plotOutput("histogrammes", height = 300)),
                 column(6, plotOutput("boxplots", height = 300))
               ),
               fluidRow(
                 column(6, plotOutput("scatter_plot", height = 300)),
                 column(6, plotOutput("regression_plot", height = 300))
               ),
               fluidRow(
                 column(12, plotOutput("diagnostics_plot", height = 500))
               )
             )
           )
  ),
  
  # TAB 2 : Statistiques
  tabPanel("📈 Statistiques",
           fluidRow(
             column(4,
                    wellPanel(
                      h4("Résumé descriptif"),
                      verbatimTextOutput("descriptive_stats")
                    )
             ),
             column(8,
                    wellPanel(
                      h4("Données filtrées"),
                      tableOutput("donnees_table")
                    )
             )
           )
  ),
  
  # TAB 3 : Modèle
  tabPanel("📐 Modèle",
           fluidRow(
             column(6,
                    wellPanel(
                      h4("Résumé du modèle linéaire"),
                      verbatimTextOutput("model_summary")
                    )
             ),
             column(6,
                    wellPanel(
                      h4("Interprétation"),
                      textOutput("interpretation"),
                      tags$hr(),
                      tags$p("Le modèle ajuste une droite de régression linéaire pour expliquer la natalité par l'urbanisation.")
                    )
             )
           )
  ),
  
  # TAB 4 : Corrélation
  tabPanel("🔗 Corrélation",
           fluidRow(
             column(6,
                    wellPanel(
                      h4("Test de corrélation de Pearson"),
                      verbatimTextOutput("correlation_test")
                    )
             ),
             column(6,
                    wellPanel(
                      h4("Synthèse"),
                      textOutput("correlation_text")
                    )
             )
           )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  # Réinitialiser les filtres
  observeEvent(input$reset, {
    updateSliderInput(session, "natalite_range",
                      value = c(floor(min(don$natalite)), ceiling(max(don$natalite))))
    updateSliderInput(session, "urbanisation_range",
                      value = c(floor(min(don$urbanisation)), ceiling(max(don$urbanisation))))
  })
  
  # Données filtrées réactives
  data_filtered <- reactive({
    don %>%
      filter(natalite >= input$natalite_range[1],
             natalite <= input$natalite_range[2],
             urbanisation >= input$urbanisation_range[1],
             urbanisation <= input$urbanisation_range[2])
  })
  
  # Modèle linéaire réactif
  model_reactive <- reactive({
    if(nrow(data_filtered()) >= 2) {
      lm(natalite ~ urbanisation, data = data_filtered())
    } else {
      NULL
    }
  })
  
  # ========== HISTOGRAMMES ==========
  output$histogrammes <- renderPlot({
    if("hist" %in% input$plots_to_show && nrow(data_filtered()) > 0) {
      par(mfrow=c(1,2), mar=c(4,4,3,2))
      hist(data_filtered()$natalite, main="Natalité", xlab="Taux (%)", 
           col="#2c3e50", border="white", breaks=7)
      hist(data_filtered()$urbanisation, main="Urbanisation", xlab="Taux (%)", 
           col="#18bc9c", border="white", breaks=7)
      par(mfrow=c(1,1))
    }
  })
  
  # ========== BOXPLOTS ==========
  output$boxplots <- renderPlot({
    if("box" %in% input$plots_to_show && nrow(data_filtered()) > 0) {
      par(mfrow=c(1,2), mar=c(4,4,3,2))
      boxplot(data_filtered()$natalite, main="Natalité", ylab="Taux (%)", 
              col="#2c3e50", border="#1a252f")
      boxplot(data_filtered()$urbanisation, main="Urbanisation", ylab="Taux (%)", 
              col="#18bc9c", border="#0f7864")
      par(mfrow=c(1,1))
    }
  })
  
  # ========== NUAGE DE POINTS ==========
  output$scatter_plot <- renderPlot({
    if("scatter" %in% input$plots_to_show && nrow(data_filtered()) > 0) {
      plot(data_filtered()$urbanisation, data_filtered()$natalite,
           main="Natalité vs Urbanisation",
           xlab="Urbanisation (%)", ylab="Natalité (%)",
           pch=21, bg="#2c3e50", col="#1a252f", cex=2)
      grid(col="gray80", lty=2)
    }
  })
  
  # ========== RÉGRESSION ==========
  output$regression_plot <- renderPlot({
    if("reg" %in% input$plots_to_show && nrow(data_filtered()) > 1) {
      plot(data_filtered()$urbanisation, data_filtered()$natalite,
           main="Régression linéaire",
           xlab="Urbanisation (%)", ylab="Natalité (%)",
           pch=21, bg="#2c3e50", col="#1a252f", cex=2)
      grid(col="gray80", lty=2)
      
      if(!is.null(model_reactive())) {
        abline(model_reactive(), col="#e74c3c", lwd=3)
        
        a <- coef(model_reactive())[1]
        b <- coef(model_reactive())[2]
        R2 <- summary(model_reactive())$r.squared
        
        legend("topright",
               legend=paste("y =", round(a,2), "+", round(b,3), "x\nR² =", round(R2,3)),
               col="#e74c3c", lwd=2, bty="o", bg="white", cex=1.1)
      }
    }
  })
  
  # ========== DIAGNOSTICS ==========
  output$diagnostics_plot <- renderPlot({
    if("diag" %in% input$plots_to_show && !is.null(model_reactive()) && nrow(data_filtered()) >= 4) {
      par(mfrow=c(2,2), mar=c(4,4,3,2))
      plot(model_reactive(), which=1:4)
      par(mfrow=c(1,1))
    }
  })
  
  # ========== DONNÉES ==========
  output$donnees_table <- renderTable({
    data_filtered()
  })
  
  # ========== STATS ==========
  output$descriptive_stats <- renderPrint({
    if(nrow(data_filtered()) > 0) {
      cat("=== NATALITÉ ===\n")
      cat("Moyenne :", round(mean(data_filtered()$natalite), 2), "%\n")
      cat("Médiane :", round(median(data_filtered()$natalite), 2), "%\n")
      cat("Écart-type :", round(sd(data_filtered()$natalite), 2), "%\n\n")
      
      cat("=== URBANISATION ===\n")
      cat("Moyenne :", round(mean(data_filtered()$urbanisation), 2), "%\n")
      cat("Médiane :", round(median(data_filtered()$urbanisation), 2), "%\n")
      cat("Écart-type :", round(sd(data_filtered()$urbanisation), 2), "%\n")
    }
  })
  
  # ========== MODÈLE ==========
  output$model_summary <- renderPrint({
    if(!is.null(model_reactive()) && nrow(data_filtered()) >= 2) {
      summary(model_reactive())
    } else {
      cat("Pas assez de données pour ajuster un modèle.\n")
    }
  })
  
  output$interpretation <- renderText({
    if(!is.null(model_reactive()) && nrow(data_filtered()) >= 2) {
      a <- round(coef(model_reactive())[1], 3)
      b <- round(coef(model_reactive())[2], 3)
      R2 <- round(summary(model_reactive())$r.squared, 3)
      
      paste("Équation : natalité =", a, "+", b, "× urbanisation\n",
            "R² =", R2, "(", round(100*R2, 1), "% variance expliquée)\n",
            "Impact : +1% urbanisation →", b, "points de natalité")
    } else {
      "Filtrez les données pour voir le modèle."
    }
  })
  
  # ========== CORRÉLATION ==========
  output$correlation_test <- renderPrint({
    if(nrow(data_filtered()) >= 2) {
      cor.test(data_filtered()$natalite, data_filtered()$urbanisation)
    }
  })
  
  output$correlation_text <- renderText({
    if(nrow(data_filtered()) >= 2) {
      r <- cor(data_filtered()$natalite, data_filtered()$urbanisation)
      paste("Coefficient de Pearson : r =", round(r, 3))
    }
  })
}

# ============================================================================
# LANCER L'APP
# ============================================================================

shinyApp(ui = ui, server = server)
