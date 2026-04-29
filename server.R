# --- server.R ---
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(scales)
library(purrr)
library(tidyverse)


df <- read.csv(file = "data_gps.csv", header = TRUE, stringsAsFactors = FALSE)

function(input, output, session) {
  # Actualisation des données par rapport aux sélections du filtres
  observe({
    updateSelectInput(session, "niveau", choices = c("Tous", sort(unique(df$Niveau)), selected="Tous"))
    updateSelectizeInput(session, "poste", choices = c("Tous", unique(df$POSTE)), selected = "Tous" )
    #updateSliderInput(session, "min_minutes", max = max(df$Min, na.rm = TRUE))
  })
  
  # Création d'un df filtré 
  data_filtered <- reactive({
    req(input$poste, input$niveau) 
    
    # filtre minutes
    data <- df 
    
    # filtre équipe
    if (input$niveau != "Tous") {
      data <- data |> filter(Niveau == input$niveau)
    }
    
    # filtre poste
    if (input$poste != "Tous") {
      data <- data |> filter(POSTE == input$poste) 
    }
    
    return(data)
  })
  
  # Récupération des données lié à un joueur selectionné
  data_j1 <- reactive({
    req(input$joueur1)
    df |> filter(NOM == input$joueur1) |> slice(1) 
  })
  
  # MISE À JOUR DU MENU DU JOUEUR 1
  observe({
    # On récupère les données filtrées selon l'équipe, le poste
    df_f <- data_filtered()
    
    # On récupère la liste des noms valides, s'il n'y en a pas, alors ça n'affiche rien
    if (is.null(df_f) || nrow(df_f) == 0) {
      noms_finale <- character(0)
    } else {
      noms_finale <- sort(unique(df_f$NOM))
    }
    
    # On mémorise le joueur actuel pour ne pas l'effacer
    joueur_actuel <- isolate(input$joueur1) 
    
    #On regarde si après avoir modifié des paramètres, certains joueurs peuvent encore correspondrent au caractéristiques
    selection <- if (!is.null(joueur_actuel) && joueur_actuel %in% noms_finale) {
      joueur_actuel
    } else {
      ""
    }
    
    # création du menu déroulant avec tous les joueurs correspondant 
    updateSelectizeInput(
      session = session,
      inputId = "joueur1",
      choices = c("Choisir un joueur" = "", noms_finale),
      selected = selection,
      server = TRUE
    )
  })
  
  # On refait une selection de joueur qui sera utile dans le cadre de la comparaison 
  output$choix_equipe_j2 <- renderUI({
    req(input$activer_comp)
    req(input$joueur1)
    selectInput("niveau_j2", "Niveau du Joueur 2 :", 
                choices = c("Tous", sort(unique(df$Niveau))))
  })
  
  # 2. MENU : Choix du nom du Joueur 2 (FILTRÉ PAR L'ÉQUIPE !)
  output$select_joueur2 <- renderUI({
    req(input$activer_comp, input$joueur1, input$niveau_j2) 
    
    poste_j1 <- df |> filter(NOM == input$joueur1) |> pull(POSTE) |> first()
    
    # On garde les joueurs du même poste, mais on enlève le Joueur 1 de la liste
    df_comp <- df |> 
      filter(POSTE == poste_j1) |> 
      filter(NOM != input$joueur1)
    
    if (input$niveau_j2 != "Tous") {
      df_comp <- df_comp |> filter(Niveau == input$niveau_j2)
    }
    
    # On extrait les noms restants
    joueurs_comp <- df_comp |> pull(NOM) |> unique() |> sort()
    
    selectizeInput(
      inputId = "joueur2", 
      label = paste("Nom du joueur (", poste_j1, ") :"), 
      choices = c("Choisir un joueur" = "", joueurs_comp),
      options = list(placeholder = 'Tapez un nom...')
    )
  })
  
  #Création de la datatable pour tester
  output$table_donnees <- renderDT({
    datatable(data_filtered(), options = list(pageLength = 10))
  })
  
  
  output$radar_chart <- renderPlotly({
    
    req(input$joueur1)
    
    # 1. Sélection des statistiques
    stats_radar <- c(
      #"Distance totale" = "TOTAL.DISTANCE", 
      "Vitesse max." = "VMAX.KM.H", 
      "Mètre par minute" = "M.MIN", 
      "Nombre de Sprint > 25" = "NB.SPRINT...25", 
      "ACC" = "ACC",
      "DEC" = "DEC"
    )
    
    couleur_j1 <- "#2C3E50" 
    couleur_j2 <- "#e74c3c" 
    
    # ==========================================
    # 2. JOUEUR 1 : Calcul du MAX absolu
    # ==========================================
    donnees_j1 <- df |> 
      filter(NOM == input$joueur1) |> 
      summarise(across(all_of(unname(stats_radar)), ~max(.x, na.rm = TRUE)))
    
    valeurs_j1 <- as.numeric(donnees_j1)
    valeurs_j1 <- c(valeurs_j1, valeurs_j1[1]) # Boucler le polygone
    
    axes <- names(stats_radar)
    axes <- c(axes, axes[1]) 
    
    # On garde en mémoire la plus grande valeur du Joueur 1 pour l'échelle du graphique
    max_global <- max(valeurs_j1, na.rm = TRUE)
    
    # ==========================================
    # 3. CREATION DU GRAPHIQUE DE BASE (Joueur 1)
    # ==========================================
    p <- plot_ly(
      type = 'scatterpolar',
      mode = 'lines+markers'
    ) |> 
      add_trace(
        r = valeurs_j1,
        theta = axes,
        fill = 'toself',
        name = input$joueur1,
        line = list(color = couleur_j1),
        fillcolor = "rgba(44, 62, 80, 0.4)",
        marker = list(color = couleur_j1),
        # NOUVELLE LIGNE POUR L'INFO-BULLE :
        hovertemplate = paste0("<b>", input$joueur1, "</b><br>%{theta} : <b>%{r}</b><extra></extra>")
      )
    
    # ==========================================
    # 4. JOUEUR 2 : Superposition (si activé)
    # ==========================================
    if (input$activer_comp && !is.null(input$joueur2) && input$joueur2 != "") {
      
      # Même logique de calcul du MAX pour le Joueur 2
      donnees_j2 <- df |> 
        filter(NOM == input$joueur2) |> 
        summarise(across(all_of(unname(stats_radar)), ~max(.x, na.rm = TRUE)))
      
      valeurs_j2 <- as.numeric(donnees_j2)
      valeurs_j2 <- c(valeurs_j2, valeurs_j2[1]) # Boucler le polygone
      
      # On met à jour le max_global si le Joueur 2 a fait mieux que le Joueur 1
      max_global <- max(c(max_global, valeurs_j2), na.rm = TRUE)
      
      # Ajout au graphique
      p <- p |> add_trace(
        r = valeurs_j2,
        theta = axes,
        fill = 'toself',
        name = input$joueur2,
        line = list(color = couleur_j2),
        fillcolor = 'rgba(231, 76, 60, 0.5)',
        marker = list(color = couleur_j2),
        # NOUVELLE LIGNE POUR L'INFO-BULLE :
        hovertemplate = paste0("<b>", input$joueur2, "</b><br>%{theta} : <b>%{r}</b><extra></extra>") # Affiche le nom du joueur et la valeur de chaque axe dans l'info-bulle, sans afficher de trace supplémentaire
      )
    }
    
    # ==========================================
    # 5. MISE EN FORME FINALE
    # ==========================================
    p <- p |> layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          # L'échelle s'adapte automatiquement au meilleur des deux joueurs (+ 10% de marge)
          range = c(0, max_global * 1.1),
          showticklabels = FALSE # On cache les chiffres pour l'esthétisme
        )
      ),
      title = list(text = "<b>Comparaison des performances athlétiques (Valeurs Max) </b>", x = 0.5),
      showlegend = TRUE,
      margin = list(t = 80, b = 40),
      paper_bgcolor = 'rgba(0,0,0,0)', 
      plot_bgcolor  = 'rgba(0,0,0,0)',
      font = list(color = "#F8F9FA")
    )
    
    # Affichage
    p
  })
  
  # Statistique individuelle du joueur 1 (vue globale)
  output$stats_saison_j1 <- renderUI({
    req(input$joueur1)
    
    infos <- df |> filter(NOM == input$joueur1)
    
    # Extrait les infos du joueurs
    nom_joueur <- input$joueur1
    poste_joueur <- infos$POSTE[1]
    niveau_joueur <- infos$Niveau[1]
    
    #Calculs et Arrondis propres
    nb_matchs <- nrow(infos)
    temps_jeu <- sum(infos$TEMPS.DE.JEU, na.rm = TRUE)
    
    # Alerte si temps de jeu non significatif
    alerte_temps <- if (temps_jeu < 250) {
      div(
        style = "background-color: #fff3cd !important; color: #856404 !important; 
               border: 1px solid #ffeeba !important; border-radius: 4px; 
               padding: 8px; margin-bottom: 15px; font-size: 0.85rem; display: flex; align-items: center;",
        span("⚠️", style = "margin-right: 8px; font-size: 1.1rem;"),
        "Attention : Temps de jeu faible. Les moyennes et les valeurs max peuvent être peu représentatives."
      )
    } else {
      NULL
    }
    
    # Arrondi à 0 décimale pour les mètres, et formatage des milliers (ex: 11 500)
    dist_moy <- round(mean(infos$TOTAL.DISTANCE, na.rm = TRUE), 0)
    dist_moy_format <- format(dist_moy, big.mark = " ", scientific = FALSE)
    
    # Arrondi à 1 décimale pour les sprints (ex: 14.2)
    sprint_moy <- round(mean(infos$NB.SPRINT...25, na.rm = TRUE), 1)
    
    # Création de la carte (Design blanc, épuré et moderne)
    card(
      class = "shadow-sm border",
      style = "background-color: #ffffff ; border-radius: 8px;", # Forcer le blanc
      
      card_header(
        
        class = "fg-dark border-bottom pt-3 pb-2",
        style= "background-color: #ffffff;",
        
        div(
          class = "d-flex justify-content-between align-items-center",
          h4(nom_joueur, class = "mb-0 fw-bold text-primary"),
          span(poste_joueur, class = "badge bg-dark text-fg rounded-pill px-3 py-2 fs-6"),
          span(niveau_joueur, class = "badge bg-dark text-fg rounded-pill px-3 py-2 fs-6")
        )
      ),
      
      card_body(
        class = "pt-4 pb-4",
        alerte_temps, # Affiche l'alerte si nécessaire
        fluidRow(
          
          column(3, class = "text-center border-end", 
                 p("Matchs joués", class = "text-dark mb-1 fs-6"), 
                 h3(nb_matchs, class = "text-dark fw-bold mb-0")
          ),
          
          column(3, class = "text-center border-end", 
                 p("Temps de jeu", class = "text-dark mb-1 fs-6"), 
                 h3(paste0(temps_jeu, " min"), class = "text-dark fw-bold mb-0")
          ),
         
          column(3, class = "text-center border-end", 
                 p("Distance moy. / match", class = "text-dark mb-1 fs-6"), 
                 h3(paste0(dist_moy_format, " m"), class = "text-dark fw-bold mb-0")
          ),
          
          column(3, class = "text-center", 
                 p("Sprints > 25 / match", class = "text-dark mb-1 fs-6"), 
                 h3(sprint_moy, class = "text-dark fw-bold mb-0")
          )
        )
      )
    )
  })
  
  # Statistique individuelle du joueur 2 (vue globale)
  output$stats_saison_j2 <- renderUI({
    # On s'assure qu'un joueur 2 est sélectionné et que la comparaison est activée
    req(input$joueur1, input$joueur2, input$activer_comp)
    
    # Filtrage des données pour le Joueur 2
    infos <- df |> filter(NOM == input$joueur2)
    
    # Si le joueur n'a pas de données, on s'arrête
    if(nrow(infos) == 0) return(NULL)
    
    # Extraction du profil
    nom_joueur <- input$joueur2
    poste_joueur <- infos$POSTE[1]
    niveau_joueur <- infos$Niveau[1]
    
    # Calculs et Arrondis (Identique au J1 pour une comparaison juste)
    nb_matchs <- nrow(infos)
    temps_jeu <- sum(infos$TEMPS.DE.JEU, na.rm = TRUE)
    
    # Alerte si temps de jeu non significatif
    alerte_temps <- if (temps_jeu < 250) {
      div(
        style = "background-color: #fff3cd !important; color: #856404 !important; 
               border: 1px solid #ffeeba !important; border-radius: 4px; 
               padding: 8px; margin-bottom: 15px; font-size: 0.85rem; display: flex; align-items: center;",
        span("⚠️", style = "margin-right: 8px; font-size: 1.1rem;"),
        "Attention : Temps de jeu faible. Les moyennes et les valeurs max peuvent être peu représentatives.(En effet, si un joueur joue 4 minutes et qu'il se donne à fond, on obtiendra des valeurs élevées en valeurs max)"
      )
    } else {
      NULL
    }
    
    dist_moy <- round(mean(infos$TOTAL.DISTANCE, na.rm = TRUE), 0)
    dist_moy_format <- format(dist_moy, big.mark = " ", scientific = FALSE)
    
    sprint_moy <- round(mean(infos$NB.SPRINT...25, na.rm = TRUE), 1)
  
    card(
      class = "shadow-sm border",
      style = "background-color: #ffffff; border-radius: 8px;",
      
      
      card_header(
        class = "fg-white border-bottom pt-3 pb-2",
        div(
          class = "d-flex justify-content-between align-items-center",
          # On utilise la couleur du j2 rouge
          h4(nom_joueur, class = "mb-0 fw-bold text-danger"), 
          span(poste_joueur, class = "badge bg-light text-secondary border rounded-pill px-3 py-2 fs-6"),
          span(niveau_joueur, class = "badge bg-light text-secondary border rounded-pill px-3 py-2 fs-6")
        )
      ),
      
      
      card_body(
        class = "pt-4 pb-4",
        alerte_temps, # Affiche l'alerte si nécessaire
        fluidRow(
          column(3, class = "text-center border-end", 
                 p("Matchs joués", class = "text-dark mb-1 fs-6"), 
                 h3(nb_matchs, class = "text-dark fw-bold mb-0")
          ),
          column(3, class = "text-center border-end", 
                 p("Temps de jeu", class = "text-dark mb-1 fs-6"), 
                 h3(paste0(temps_jeu, " min"), class = "text-dark fw-bold mb-0")
          ),
          column(3, class = "text-center border-end", 
                 p("Distance moy. / match", class = "text-dark mb-1 fs-6"), 
                 h3(paste0(dist_moy_format, " m"), class = "text-dark fw-bold mb-0")
          ),
          column(3, class = "text-center", 
                 p("Sprints > 25 / match", class = "text-dark mb-1 fs-6"), 
                 h3(sprint_moy, class = "text-dark fw-bold mb-0")
          )
        )
      )
    )
  })
}
