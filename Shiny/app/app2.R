library(shiny)
library(tidyverse)
library(scales)
library(DT)

# Importation des données et création des variables objet
donnees <- read_csv2("delitDep.csv", na = "NA", locale = locale(encoding = "latin1"))

delits_long <- donnees %>%
  pivot_longer(-Dpt, names_to = "categorie", values_to = "nombre") %>%
  mutate(
    nombre = as.numeric(nombre),
    categorie = str_replace_all(categorie, "_", " "),
    categorie_principale = case_when(
      str_detect(categorie, regex("vol", ignore_case = TRUE)) ~ "Vols",
      str_detect(categorie, regex("homicide|tentative|meurtre", ignore_case = TRUE)) ~ "Homicides et tentatives",
      str_detect(categorie, regex("coup|blessure|violence", ignore_case = TRUE)) ~ "Violences physiques",
      str_detect(categorie, regex("viol|sexuel", ignore_case = TRUE)) ~ "Violences sexuelles",
      str_detect(categorie, regex("stup|drogue", ignore_case = TRUE)) ~ "Stupéfiants",
      str_detect(categorie, regex("incendie|destruction|dégrad", ignore_case = TRUE)) ~ "Destructions / dégradations",
      str_detect(categorie, regex("escroq|fraude|faux", ignore_case = TRUE)) ~ "Escroqueries / fraudes",
      str_detect(categorie, regex("cambriolage", ignore_case = TRUE)) ~ "Cambriolages",
      TRUE ~ "Autres"
    )
  )

liste_departements <- sort(unique(delits_long$Dpt))
liste_categories   <- sort(unique(delits_long$categorie_principale))

# Création de l'interface
ui <- fluidPage(
  titlePanel("Délits par département (simple)"),
  
  tabsetPanel(
    tabPanel(
      "National",
      fluidRow(
        column(
          4,
          selectInput("cat_nat", "Catégorie :", choices = c("Toutes", liste_categories)),
          sliderInput("top_n", "Top N départements :", min = 5, max = 20, value = 10),
          checkboxInput("log_scale", "Axe en log", value = TRUE)
        ),
        column(8, plotOutput("p_top", height = 350))
      ),
      br(),
      DTOutput("tab_nat")
    ),
    
    tabPanel(
      "Comparaison",
      fluidRow(
        column(
          4,
          selectInput(
            "dpts", "Départements :",
            choices = liste_departements,
            selected = head(liste_departements, 3),
            multiple = TRUE
          ),
          checkboxGroupInput(
            "cats", "Catégories :",
            choices = liste_categories,
            selected = head(liste_categories, 3)
          ),
          actionButton("go_comp", "Mettre à jour")
        ),
        column(8, plotOutput("p_comp", height = 520))
      )
    ),
    
    tabPanel(
      "Département",
      fluidRow(
        column(
          4,
          selectInput("dpt_detail", "Département :", choices = liste_departements, selected = "75"),
          sliderInput("top_k", "Top délits :", min = 5, max = 30, value = 15)
        ),
        column(8, plotOutput("p_detail", height = 450))
      )
    )
  )
)

#Création du côté serveur
server <- function(input, output, session) {
  
  # Top départements
  output$p_top <- renderPlot({
    d <- delits_long
    
    if (input$cat_nat != "Toutes") {
      d <- d %>% filter(categorie_principale == input$cat_nat)
    }
    
    top <- d %>%
      group_by(Dpt) %>%
      summarise(Total = sum(nombre, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total)) %>%
      slice_head(n = input$top_n)
    
    p <- ggplot(top, aes(x = Total, y = reorder(Dpt, Total))) +
      geom_col() +
      labs(x = "Nombre de délits", y = "Département") +
      theme_minimal()
    
    if (isTRUE(input$log_scale)) {
      p + scale_x_log10(labels = label_number(big.mark = " "))
    } else {
      p + scale_x_continuous(labels = label_number(big.mark = " "))
    }
  })
  
  # Tableau de lecture
  output$tab_nat <- renderDT({
    d <- delits_long
    if (input$cat_nat != "Toutes") {
      d <- d %>% filter(categorie_principale == input$cat_nat)
    }
    
    tab <- d %>%
      group_by(Dpt) %>%
      summarise(Total = sum(nombre, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total))
    
    datatable(tab, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Comparaison par département
  comp_data <- eventReactive(input$go_comp, {
    req(input$dpts, input$cats)
    
    delits_long %>%
      filter(Dpt %in% input$dpts, categorie_principale %in% input$cats) %>%
      group_by(Dpt, categorie_principale) %>%
      summarise(total = sum(nombre, na.rm = TRUE), .groups = "drop") %>%
      group_by(Dpt) %>%
      mutate(pct = 100 * total / sum(total)) %>%
      ungroup()
  }, ignoreInit = TRUE)
  
  output$p_comp <- renderPlot({
    comp <- comp_data()
    req(nrow(comp) > 0)
    
    ggplot(comp, aes(x = categorie_principale, y = pct, fill = categorie_principale)) +
      geom_col(show.legend = FALSE) +
      geom_text(
        aes(label = paste0(round(pct, 1), "%")),
        hjust = -0.1,
        size = 3
      ) +
      facet_wrap(~Dpt) +
      coord_flip() +
      scale_y_continuous(
        labels = function(x) paste0(x, "%"),
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.15))
      ) +
      labs(x = NULL, y = "Part (%)") +
      theme_minimal()
  })
  
  # Top délits par département
  output$p_detail <- renderPlot({
    req(input$dpt_detail)
    
    d <- delits_long %>%
      filter(Dpt == input$dpt_detail) %>%
      group_by(categorie) %>%
      summarise(total = sum(nombre, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      slice_head(n = input$top_k)
    
    ggplot(d, aes(x = total, y = reorder(categorie, total))) +
      geom_col() +
      scale_x_continuous(labels = label_number(big.mark = " ")) +
      labs(
        title = paste("Département", input$dpt_detail),
        x = "Nombre",
        y = NULL
      ) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8))
  })
}

shinyApp(ui, server)
