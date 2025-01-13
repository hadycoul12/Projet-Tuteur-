library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(DT)
library(shinyjs)
library(tidyr)
library(factoextra)
library(FactoMineR)
library(missMDA)







ui <- dashboardPage(
  dashboardHeader(title = "Analyse de SIGI"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "home", icon = icon("home")),
      menuItem("Base documentaire", tabName = "docs", icon = icon("book")),
      menuItem("Base de données", tabName = "data", icon = icon("database")),
      menuItem("Statistiques", tabName = "stats", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tabItems(
      tabItem(tabName = "home",
              h2("Bienvenue sur Mon plateforme d'analyse de la discrimination de genre dans le monde"),
              h3("Auteur: Hady COULIBALY - Data Scientist"),
              tags$div(
                tags$img(src = "photo.jpg", 
                         alt = "Image SIGI", 
                         style = "width:100%; max-width:800px; display:block; margin:auto;")
              ),
              p("Mon plateforme explore les inegalités de genre ? travers le monde, en utilisant les données SIGI (Social Institutions and Gender Index).")
      ),
      tabItem(tabName = "docs",
              h2("Base documentaire sur le thème du genre"),
              p("La notion d'égalité hommes/femmes fait référence ? des différences qui sont construites socialement et non ? des différences biologiques, mais englobe les constructions sociales qui attribuent ? chaque sexe des rôles, des statuts et des rapports hi?rarchisés dans la société. Ce concept, communément appelé ? genre ?, met en lumière les normes, les valeurs et les pratiques sociales qui fa?onnent les exp?riences des individus en fonction de leur sexe."),
              p("Ce thème d'étude, plus commun?ment appel? ? genders studies ? s'est progressivement impos? comme l'un des th?mes cl?s pour l'?tude du d?veloppement social. Elles examinent les rapports de genre ? travers une multitude de crit?res, tant au niveau macro-social (syst?me juridique, institutions sociales) que micro-social (sant?, ?ducation, travail, participation politique, autonomie)."),
              p("Dans une soci?t? donn?e, les rapports de genre se manifestent ? travers des aspects tels que la l?gislation familiale, la planification familiale, le droit ? l'avortement, le divorce, ou encore la polygamie. Ils se r?v?lent ?galement dans des domaines plus intimes, comme la r?partition des t?ches domestiques, la prise de d?cision au sein du foyer, ou l'acc?s ? l'?ducation et ? l'emploi."),
              h3("Voici quelques articles recommandés sur le sujet :"),
              tags$ul(
                tags$li(a("ONU Femmes", href = "https://www.unwomen.org/en")),
                tags$li(a("Observatoire de l'égalit? des genres pour l'Am?rique latine et les Cara?bes", href = "https://observatoriodegenero.org.mx/?lang=en")),
                tags$li(a("Institut europ?en pour l'?galit? entre les hommes et les femmes (EIGE)", href = "https://eige.europa.eu/"))
              )
      ),
      tabItem(tabName = "data",
              h2("Accès à la base de données"),
              div(id = "login_page",
                  textInput("username", "Nom d'utilisateur"),
                  passwordInput("password", "Mot de passe"),
                  actionButton("login", "Connexion")
              ),
              hidden(
                div(id = "protected_content",
                    selectInput("dataset", "Choisissez une base de données:",
                                choices = c("SIGI 2019", "SIGI 2023", "IDH 2021")),
                    DTOutput("table"),
                    actionButton("logout", "D?connexion")
                )
              )
      ),
      tabItem(tabName = "stats",
              h2("Statistiques sur la discrimination de genre"),
              selectInput("year", "Choisissez l'année:", choices = c("2019", "2023")),
              selectInput("analysis", "Choisissez l'analyse:",
                          choices = c("Moyenne SIGI par région", 
                                      "Pays les plus discriminants",
                                      "Pays les moins discriminants",
                                      "Niveaux de discrimination par région",
                                      "Carte choroplèthe",
                                      "Evolution temporelle",
                                      "Dendrogramme hiérarchique",
                                      "Plan factoriel",
                                      "Détection d'anomalies")),
              plotOutput("plot")
      )
    )
  )
)

server <- function(input, output, session) {
  valid_credentials <- list(username = "hady", password = "2024")
  
  observeEvent(input$login, {
    if(input$username == valid_credentials$username && input$password == valid_credentials$password) {
      shinyjs::hide("login_page")
      shinyjs::show("protected_content")
    } else {
      showNotification("Identifiants incorrects", type = "error")
    }
  })
  
  observeEvent(input$logout, {
    shinyjs::hide("protected_content")
    shinyjs::show("login_page")
  })
  
  output$table <- renderDT({
    req(input$dataset)
    if(input$dataset == "SIGI 2019") {
      datatable(data19, options = list(pageLength = 10))
    } else if(input$dataset == "SIGI 2023") {
      datatable(data23, options = list(pageLength = 10))
    } else {
      datatable(IDH21, options = list(pageLength = 10))
    }
  })
  
  output$plot <- renderPlot({
    data <- if(input$year == "2019") data19 else data23
    
    if(input$analysis == "Moyenne SIGI par région") {
      mean_data <- data %>%
        group_by(Region) %>%
        summarize(mean_Valeur = mean(Valeur))
      
      ggplot(mean_data, aes(x = reorder(Region, mean_Valeur), y = mean_Valeur, fill = Region)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = paste(round(mean_Valeur), "%")),
                  position = position_dodge(width = 0.9), vjust = -0.5) +
        labs(title = paste("Moyenne SIGI par région en", input$year),
             x = "R?gion", y = "Moyenne SIGI") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        ylim(0, 100)
    } else if(input$analysis == "Pays les plus discriminants") {
      pays_plus_discriminants <- data %>%
        filter(Var_Sigi == "SIGI") %>%
        group_by(Pays) %>%
        summarize(moyenne_discrimination = mean(Valeur)) %>%
        top_n(5, moyenne_discrimination)
      
      ggplot(pays_plus_discriminants, aes(x = reorder(Pays, moyenne_discrimination), y = moyenne_discrimination)) +
        geom_bar(stat = "identity", fill = "maroon") +
        geom_text(aes(label = sprintf("%.1f%%", moyenne_discrimination)),
                  vjust = -0.5, color = "black", size = 3) +
        labs(title = paste("Pays avec les niveaux les plus élevés de discrimination de genre en", input$year),
             x = "Pays", y = "Moyenne de la discrimination") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$analysis == "Pays les moins discriminants") {
      pays_moins_discriminants <- data %>%
        filter(Var_Sigi == "SIGI") %>%
        group_by(Pays) %>%
        summarize(moyenne_discrimination = mean(Valeur)) %>%
        top_n(-5, moyenne_discrimination)
      
      ggplot(pays_moins_discriminants, aes(x = reorder(Pays, -moyenne_discrimination), y = moyenne_discrimination)) +
        geom_bar(stat = "identity", fill = "forestgreen") +
        geom_text(aes(label = sprintf("%.1f%%", moyenne_discrimination)),
                  vjust = -0.5, color = "black", size = 3) +
        labs(title = paste("Pays avec les niveaux les plus bas de discrimination de genre en", input$year),
             x = "Pays", y = "Moyenne de la discrimination") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if(input$analysis == "Niveaux de discrimination par région") {
      region_data <- data %>%
        filter(Var_Sigi == "SIGI") %>%
        group_by(Region) %>%
        summarize(moyenne_discrimination = mean(Valeur, na.rm = TRUE))
      
      ggplot(region_data, aes(x = "", y = moyenne_discrimination, fill = Region)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = paste("Niveaux de discrimination de genre par région en", input$year),
             fill = "R?gion") +
        theme_void() +
        geom_text(aes(label = paste0(round(moyenne_discrimination, 1), "%")),
                  position = position_stack(vjust = 0.5)) +
        scale_fill_brewer(palette = "Set2")
    } else if(input$analysis == "Carte choroplèthe") {
      world <- ne_countries(returnclass = "sf")
      map_data <- left_join(world, data %>% filter(Var_Sigi == "SIGI"), by = c("name" = "Pays"))
      
      ggplot() +
        geom_sf(data = map_data, aes(fill = Valeur)) +
        scale_fill_gradient(name = "Niveau de discrimination ", low = "palegreen", high = "darkred") +
        labs(title = paste("Carte choroplèthe des niveaux de discrimination de genre en", input$year),
             subtitle = "Basé sur les données de discrimination de genre dans différents pays",
             caption = "Source: Ocde (Sigi)") +
        theme_void()
    } else if(input$analysis == "Evolution temporelle") {
      sigi_2019 <- data19 %>% filter(Var_Sigi == "SIGI")
      sigi_2023 <- data23 %>% filter(Var_Sigi == "SIGI")
      
      evolution_data <- inner_join(sigi_2019, sigi_2023, by = "Pays") %>%
        select(Pays, Valeur.x, Valeur.y) %>%
        rename(SIGI_2019 = Valeur.x, SIGI_2023 = Valeur.y)
      
      ggplot(evolution_data, aes(x = SIGI_2019, y = SIGI_2023)) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        labs(title = "?volution des valeurs SIGI entre 2019 et 2023",
             x = "SIGI 2019", y = "SIGI 2023") +
        theme_minimal()
    } else if(input$analysis %in% c("Dendrogramme hiérarchique", "Plan factoriel", "Détection d'anomalies")) {
      data_Europe23 = data23 %>%
        filter(Var_Sigi %in% c("Discrimination in the family", "Restricted access to productive and financial resources", "Restricted civil liberties", "Restricted physical integrity", "SIGI")) %>%
        select(Region, Pays, Var_Sigi, Valeur) %>%
        spread(key = Var_Sigi, value = Valeur) %>%
        filter(Region == "Europe")
      
      data_quant <- data_Europe23[, -c(1, 2)] %>%
        mutate_all(~as.numeric(as.character(.)))
      
      ncp <- estim_ncpPCA(data_quant, ncp.max = 5)
      data_imputed <- imputePCA(data_quant, ncp = ncp$ncp)
      
      data_final <- cbind(data_Europe23[, c("Region", "Pays")], data_imputed$completeObs)
      rownames(data_final) <- data_final$Pays
      
      res.PCA <- PCA(data_final, quali.sup = c(1, 2), graph = FALSE)
      res.HCPC <- HCPC(res.PCA, nb.clust = 4, consol = FALSE, graph = FALSE)
      
      if(input$analysis == "Dendrogramme hiérarchique") {
        fviz_dend(res.HCPC, 
                  rect = TRUE,
                  show_labels = TRUE,
                  cex = 0.7,
                  main = "Dendrogramme hiérarchique des pays d'Europe (2023)")
      } else if(input$analysis == "Plan factoriel") {
        fviz_cluster(res.HCPC, 
                     geom = "text",
                     repel = TRUE,
                     main = "Plan factoriel des pays d'Europe (2023)")
      } else if(input$analysis == "Détection d'anomalies") {
        scores <- res.PCA$ind$coord[,1]
        anomalies <- data.frame(Pays = rownames(data_final), Score = scores)
        anomalies <- anomalies[order(abs(anomalies$Score), decreasing = TRUE),]
        
        ggplot(anomalies, aes(x = reorder(Pays, -abs(Score)), y = Score)) +
          geom_bar(stat = "identity", aes(fill = abs(Score))) +
          coord_flip() +
          labs(title = "Détection d'anomalies pour les pays d'Europe (2023)",
               x = "Pays", y = "Score d'anomalie") +
          theme_minimal()
      }
    }
  })
}


shinyApp(ui, server, options = list(encoding = "UTF-8"))



