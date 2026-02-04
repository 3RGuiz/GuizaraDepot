
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

# Affichage
newdelit

# Transformation en format long (toutes les catégories de délits sauf Dpt)
delits_long <- newdelit %>% 
  pivot_longer(
    cols = -Dpt,
    names_to = "categorie",
    values_to = "nombre"
  )

delits_long

# Retour au format wide
delits_wide <- delits_long %>% 
  pivot_wider(
    names_from = Categorie,
    values_from = Nombre,
    id_cols = Dpt
  )

delits_wide

library(ggplot2)

# Répartition totale des délits par catégorie

delits_long %>%
  group_by(categorie) %>%            #data
  summarise(total = sum(nombre)) %>% #data
  
  ggplot() + #mapping
  aes(x = total, y = reorder(categorie, total), fill = categorie) + #mapping
  
  geom_col() + #layers

  scale_x_log10(
    breaks = c(1000, 10000, 50000, 250000, 1000000),
    labels = c("1k", "10k", "50k", "250k", "1M")) + # scales

  labs(title = "Répartition des délits en France",
       x = "Catégorie de délit",
       y = "Nombre de délits") + #labels
  
  theme_minimal() +               #theme
  theme(legend.position = "none") #theme


#Plot du top 10 des départements les + criminels

delits_total_dpt <- newdelit %>%
  mutate(Total = rowSums(select(., -Dpt))) %>%
  arrange(desc(Total)) %>%
  slice(1:10)

delits_total_dpt %>%
  ggplot() +                                        #data
  aes(x=Total, y=reorder(Dpt, Total),fill = Dpt) + #data
  
  geom_col() + #layers
  
  scale_x_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  
  labs(title = "Top 10 des départements avec le plus de délits", 
       x = "Département", y = "Nombre total de délits") +#Labels
  
  theme_minimal() + #Themes
  coord_cartesian()




#facet_wrap ou facet_grid pour afficher le type de criminalité par département
#avec d'un côté les départements et de l'autre la criminalité


