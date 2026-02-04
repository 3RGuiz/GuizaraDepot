# Chargement des données
library(tidyverse)

delits <- read_csv2("delitDep.csv", na = "NA", locale = locale(encoding = "latin1"))

# Création des nouvelles catégories agrégées
newdelit <- delits %>%
  mutate(
    Dpt = Dpt,  # Conservation de la colonne Département
    
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

# Sélection des colonnes pertinentes
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

# Total par catégorie pour tous les départements
delits_long %>%
  group_by(categorie) %>%            #data
  summarise(total = sum(nombre)) %>% #data
  
  ggplot() + #mapping
  aes(x = total, y = reorder(categorie, total), fill = categorie) + #mapping
  
  geom_col() + #layers

  labs(title = "Répartition des délits en France",
       x = "Catégorie de délit",
       y = "Nombre de délits") + #labels
  
  theme_minimal() +               #theme
  theme(legend.position = "none") #theme









