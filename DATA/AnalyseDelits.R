library(tidyverse)

delits <- read_csv2("delitDep.csv", na = "NA", locale = locale(encoding = "latin1"))

newdelit <- delits %>%
  mutate(
    Dpt = Dpt,
    
    'Atteintes aux personnes' = rowSums(across(
      contains("Homicides") |
        contains("Tentatives_homicides") |
        contains("Coups_et_blessures") |
        contains("Autres_coups_et_blessures") |
        contains("Violences__mauvais_traitements") |
        contains("Violences_autorité") |
        contains("Atteintes_dignité") |
        contains("Atteintes_sex") |
        contains("Viols") |
        contains("Harcèlements") |
        contains("Sequestrations") |
        contains("Prises_d_otages") |
        contains("Règlements_compte")
    ), na.rm = TRUE),
    
    'Atteintes aux biens' = rowSums(across(
      contains("Camb") |
        contains("Vols_") |
        contains("Autres_vols") |
        contains("Destructions") |
        contains("dégrada") |
        contains("Incendies") |
        contains("Violations_domicile") |
        contains("Recels")
    ), na.rm = TRUE),
    
    'Éco-financier' = rowSums(across(
      contains("Escroqueries") |
        contains("Banqueroutes") |
        contains("Fraudes_fiscales") |
        contains("Fausse_monnaie") |
        contains("Achats_ventes_sans_factures") |
        contains("Marchandage") |
        contains("Travail_clandestin") |
        contains("Prix_illicittes") |
        contains("Contrefaçons") |
        contains("cartes_crédit") |
        contains("chèques_volés") |
        contains("Infractions_chèques") |
        contains("Autres_délits_éco_financiers")
    ), na.rm = TRUE),
    
    'Stupéfiants' = rowSums(across(contains("stup")), na.rm = TRUE),
    
    'Étrangers et séjour' = rowSums(across(
      contains("étranger") | contains("étrangers")
    ), na.rm = TRUE),
    
    'Administratif, professionnel et réglementaire' = rowSums(across(
      contains("urbanisme") |
        contains("profession_règlementée") |
        contains("Fraudes_alimentaires") |
        contains("santé_publique") |
        contains("alcool_tabac") |
        contains("Chasse_pêche") |
        contains("animaux") |
        contains("courses__jeux") |
        contains("garde_mineurs")
    ), na.rm = TRUE),
    
    'Ordre public, autorité et sûreté' = rowSums(across(
      contains("Outrages") |
        contains("Menaces") |
        contains("Proxénétisme") |
        contains("armes_prohib") |
        contains("Atteintes_aux_intérêts_fondamentaux") |
        contains("Attentats") |
        contains("interdiction_séjour")
    ), na.rm = TRUE),
    
    'Faux et usage de faux' = rowSums(across(
      contains("Faux_") |
        contains("faux_docs") |
        contains("Faux_doc") |
        contains("Autres_faux")
    ), na.rm = TRUE)
  )

newdelit <- newdelit %>% 
  select(
    Dpt,
    `Atteintes aux personnes`,
    `Atteintes aux biens`,
    `Éco-financier`,
    `Stupéfiants`,
    `Étrangers et séjour`,
    `Administratif, professionnel et réglementaire`,
    `Ordre public, autorité et sûreté`,
    `Faux et usage de faux`
  )

# Transformation en format long
delits_long <- newdelit %>% 
  pivot_longer(
    cols = -Dpt,
    names_to = "categorie",
    values_to = "nombre"
  )

library(ggplot2)

# Répartition totale des délits par catégorie
delits_long %>%
  group_by(categorie) %>%
  summarise(total = sum(nombre)) %>%
  
  ggplot() +
  aes(x = total, y = reorder(categorie, total), fill = categorie) +
  
  geom_col() +
  
  scale_x_log10(
    breaks = c(1000, 10000, 50000, 250000, 1000000),
    labels = c("1k", "10k", "50k", "250k", "1M")) +
  
  labs(title = "Répartition des délits en France",
       x = "Catégorie de délit",
       y = "Nombre de délits") +
  
  theme_minimal() +
  theme(legend.position = "none")

# Plot du top 10 des départements
delits_total_dpt <- newdelit %>%
  mutate(Total = rowSums(select(., -Dpt))) %>%
  arrange(desc(Total)) %>%
  slice(1:10)

delits_total_dpt %>%
  ggplot() +
  aes(x = Total, y = reorder(Dpt, Total), fill = Dpt) +
  
  geom_col() +
  
  scale_x_continuous(breaks = c(50000, 75000, 100000, 125000, 150000, 175000, 
                                200000, 225000, 250000, 275000),
                                labels = c("50k", "75k", "100k", "125k", "150k",
                                           "175k", "200k", "225k", "250k", "275k")) +
  
  labs(title = "Top 10 des départements avec le plus de délits", 
       x = "Nombre total de délits", 
       y = "Département") +
  
  theme_minimal() +
  theme(legend.position = "none")

# Calculer le total national
total_national <- newdelit %>%
  mutate(Total = rowSums(select(., -Dpt))) %>%
  summarise(Total_France = sum(Total)) %>%
  pull(Total_France)

# Créer le top des départements avec pourcentage national
top_depts <- newdelit %>%
  mutate(Total = rowSums(select(., -Dpt))) %>%
  mutate(Pct_national = (Total / total_national) * 100) %>%
  arrange(desc(Total)) %>%
  slice(1:9)

# Préparer les données pour facet
facet_data <- newdelit %>%
  filter(Dpt %in% top_depts$Dpt) %>%
  left_join(top_depts %>% select(Dpt, Total, Pct_national), by = "Dpt") %>%
  mutate(Dpt_label = paste0(
    "Département : ", Dpt, " (", round(Pct_national, 1), "% national)",
    "\nTotal des délits recensés : ", format(Total, big.mark = " "))) %>%
  
  mutate(Dpt_label = factor(Dpt_label, levels = unique(Dpt_label[order(-Total)]))) %>%
  
  pivot_longer(-c(Dpt, Total, Pct_national, Dpt_label), names_to = "categorie", values_to = "nombre") %>%
  
  group_by(Dpt) %>%
  
  mutate(Pourcentage = nombre / sum(nombre) * 100) %>%
  
  ungroup()

# Graphique facetté
ggplot(facet_data, aes(x = reorder(categorie, -Pourcentage),
                       y = Pourcentage,
                       fill = categorie)) +
  geom_col(show.legend = FALSE) +
  
  geom_text(aes(label = paste0(round(Pourcentage, 1), "%")),
            hjust = -0.1,
            size = 3,
            color = "black") +
  
  facet_wrap(~Dpt_label, ncol = 3) +
  
  coord_flip() +
  
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),limits = c(0, 110), breaks = seq(0, 100, 10)) +
  
  labs(title = "Profil criminel des 9 départements les plus représentés",
    subtitle = "Répartition en pourcentage par catégorie de délit - Échelle commune 0-100%",
    x = "", y = "Part du total (%)") +
  
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(
      face = "bold",
      size = 10,
      color = "white",
      lineheight = 0.9),
    strip.background = element_rect(fill = "gray20", color = NA),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5),
    axis.text.y = element_text(size = 8),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(20, 20, 20, 20))

library(scales)  # Pour la fonction percent



donnees_empilees <- newdelit %>%
  filter(Dpt %in% top_depts$Dpt) %>%
  left_join(top_depts %>% select(Dpt, Total), by = "Dpt") %>%
  mutate(
    Dpt_label = paste0(Dpt, " (", format(Total, big.mark = " "), ")"),
    Dpt_label = factor(Dpt_label, levels = rev(unique(Dpt_label[order(-Total)])))
  ) %>%
  pivot_longer(
    cols = -c(Dpt, Total, Dpt_label),
    names_to = "categorie",
    values_to = "nombre"
  ) %>%
  mutate(
    # garantit un type numérique (gère aussi espaces/virgules éventuels)
    nombre = suppressWarnings(as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", as.character(nombre)))))
  ) %>%
  filter(!is.na(nombre)) %>%  # évite que des valeurs non convertibles cassent les sommes
  mutate(total_global = sum(nombre, na.rm = TRUE)) %>%
  group_by(categorie) %>%
  mutate(part_globale = sum(nombre, na.rm = TRUE) / first(total_global)) %>%
  ungroup() %>%
  mutate(categorie = if_else(part_globale < 0.01, "Autres", categorie)) %>%
  group_by(Dpt, Total, Dpt_label, categorie) %>%
  summarise(nombre = sum(nombre, na.rm = TRUE), .groups = "drop") %>%
  # optionnel : mettre "Autres" en dernier dans la légende
  mutate(categorie = factor(categorie, levels = c(setdiff(sort(unique(as.character(categorie))), "Autres"), "Autres")))

ggplot(donnees_empilees, aes(x = Dpt_label, y = nombre, fill = categorie)) +
  geom_col(position = "fill", color = "white") +
  coord_flip() +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(labels = percent, expand = c(0, 0)) +
  labs(
    title = "Composition des délits par département",
    subtitle = "Répartition proportionnelle - Top 9 départements",
    x = "Département (total des délits)",
    y = "Proportion (%)",
    fill = "Catégorie de délit"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  guides(fill = guide_legend(ncol = 1))
