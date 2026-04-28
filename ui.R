# --- ui.R ---
library(shiny)
library(bslib)
library(bsicons)
library(plotly)
library(DT)
library(googlesheets4)

# Définition du thème de l'app
theme_dfco <- bs_theme(
  bg = "#07101f",        # rouge nuit très profond (Fond de l'application)
  fg = "#F8F9FA", 
  "navbar-bg" = "#d40028",
  primary = "#07101f",   # Le rouge du logo du DFCO
  secondary = "#d40028", # 
  success = "#00C853",   # Vert vif pour les stats positives
  danger = "#FF1744",    # Rouge vif pour les alertes
  base_font = font_google("Montserrat"),    # Police
  heading_font = font_google("Montserrat")  # Police
)


page_navbar(
  
  theme = theme_dfco,
  id = "mes_onglets",
  title = tags$span(
    tags$img(
      src = "https://files.memberz.fr/dfco/logo_starter.png", 
      height = "80px", 
      style = "margin-right: 10px;"
    ), 
    "Comparative Dashboard | N3-N1"
 ),
 # BARRE LATÉRALE 
 sidebar = sidebar(
   open = "open",
   width = 350,
   id = "ma_sidebar",
   h4("Filtres de selection"),
   actionButton("refresh", "Actualiser les données", icon = icon("sync")),
   selectizeInput("niveau", "Niveau :", choices = "Chargement..."),
   selectizeInput("poste", "Poste :", choices = "Chargement...", multiple = FALSE),
   #sliderInput("min_minutes", "Minutes jouées minimum :", min = 0,max = 3500, value = 500),
   
   conditionalPanel(
     condition = "input.mes_onglets == 'onglet_stat'",
     selectizeInput("joueur1", "Sélectionnez un joueur :", choices = "Chargement..."),
     hr(), 
     h4("Mode Comparaison"),
     checkboxInput("activer_comp", "Comparer avec un autre joueur", FALSE),
     uiOutput("choix_equipe_j2"), 
     uiOutput("select_joueur2") 
   )
 ),
 nav_panel(
   title = "Accueil",
   
   div(
     class = "text-center",
     style = "max-width: 1200px; margin: auto; margin-top: 120px;", 
     
     h2("Mesurez l'écart entre les joueurs du centre de formation et le haut niveau", 
        class = "fw-bold mb-2", style = "color: #ffffff;"),
     
     p("Explorez, analysez et comparez les performances de nos joueurs pour la saison 2025-2026.", 
       class = "fw-bold mb-5",style = "color: #ffffff;"),
     
     tags$img(
       src = "https://files.memberz.fr/dfco/logo_starter.png", 
       class = "rounded shadow-lg mb-5", 
       style = "max-width: 550px; width: 100%; border: 1px solid #1A2E44;" 
     ),
     
     p("Une solution d'analyse de données pour comparer les statistiques des joueurs de formation avec les professionnels. Identifiez les profils 'prêts pour le haut niveau' et facilitez vos choix stratégiques grâce à une visualisation claire des talents les plus intéressants.", 
       class = "text-muted mb-5", 
       style = "font-size: 1.1rem; line-height: 1.6;"),
     
     hr(style = "border-color: #1A2E44; margin-bottom: 40px;"), 
     
     layout_columns(
       col_widths = c(6, 6),
       div(
         bs_icon("funnel-fill", size = "2.5rem", class = "mb-3", style = "color: #085fff;"),
         h5("1. Filtrer", class = "fw-bold text-white"),
         p("Utilisez le menu latéral pour cibler un joueur ou un poste précis.", class = "text-muted small")
       ),
       #div(
        # bs_icon("graph-up-arrow", size = "2.5rem", class = "mb-3 text-success"),
         #h5("2. Analyser", class = "fw-bold text-white"),
         #p("Repérez les anomalies de marché grâce aux graphiques de performance.", class = "text-muted small")
       #),
       div(
         bs_icon("crosshair", size = "2.5rem", class = "mb-3 text-danger"),
         h5("3. Comparer", class = "fw-bold text-white"),
         p("Mettez deux joueurs face à face dans le Radar de performance.", class = "text-muted small")
       )
     )
   )
 ),
 nav_panel(
   title = "Analyse Comparative",
   value = "onglet_stat",
   
   mainPanel(
     width = 12,
     
     # STAT DES JOUEURS
     uiOutput("layout_profils",class = "bg-primary text-dark"),
     uiOutput("layout_stats"),
     
     br(),
     
     # GRAPHIQUE TOILE D'ARAIGNÉ
     
       card(
         class = "shadow-sm mb-4",
         card_header("🕸️ Radar des performances  ", class= " bg-primary text-darkk"),
         card_body(
           plotlyOutput("radar_chart", height = "500px") 
         )
       ),
     ("le graphique de radar compare les performances d'un joueur sélectionné à celles des joueurs du même poste, en affichant plusieurs statistiques sur un même graphique. Chaque axe du radar représente une statistique différente.")
       
       
       
       # GRAPHIQUE Centiles
       #card(
        # class = "shadow-sm mb-4",
         #card_header("📊 Centiles (Par rapport aux joueurs du même poste)", class = "bg-primary text-dark"),
         #card_body(
           
           # 1. La phrase pour aider à lire le graphique
          # htmlOutput("explication_centiles"),
           #br(), # Petit saut de ligne
           
           # 2. La zone qui va accueillir 1 ou 2 graphiques
          # uiOutput("layout_percentiles")
           
        # )
      # ),
       
       #GRAPHIQUE NUAGE DE POINTS
       #card(
        # class = "shadow-sm mb-4",
         #card_header("Analyse Détaillée", class = "bg-primary text-dark"),
         #card_body(
        #   uiOutput("choix_vars_scatter"),
         #  plotlyOutput("scatter_plot", height = "500px")
         #)
       #)
   ) 
 ),
 
 nav_panel(
   title = "Données brutes",
   value = "onglet_stat",
   
   mainPanel(
     width = 12,
     
     DTOutput("table_donnees")
     )
   ),
 )
