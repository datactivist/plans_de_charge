# Visualisation des plans de charge

# Packages
library(tidyverse)
library(plotly)
#devtools::install_github('bbc/bbplot')
#library(bbplot)

# Données
data <- read_csv("project.task.csv") |> 
  rename(debut = `Date de début`,
         fin = `Date de fin`,
         tache_parente = `Tâche parente`,
         etat = `Étiquette d'état Kanban`,
         etape = `Étape`) |> 
  mutate(assigne = coalesce(`Assigné à`, `Sous-tâches/Assigné à`),
         heures_prevues = coalesce(`Heures prévues initialement`, `Heures prévues sous-tâches`),
         heures_passees = coalesce(`Heures passées`, `Heures passées sur les sous-tâches`),
         heures_restantes = coalesce(`Heures restantes`, `Sous-tâches/Heures restantes`),
         # test = case_when(!is.na(Titre) & !is.na(`Sous-tâches`) ~ paste(Titre, ":", `Sous-tâches`),
         #                  !is.na(Titre) & is.na(`Sous-tâches`) ~ `Sous-tâches`,
         #                  TRUE ~ NA_character_)) |> 
         type = case_when(!is.na(tache_parente) ~ "Sous-tâche",
                          !is.na(`Sous-tâches`) ~ "Sous-tâche",
                          TRUE ~ "Tâche"),
         taches = coalesce(Titre, `Sous-tâches`)) |> 
  select(Projet, taches, type, tache_parente, Description, debut, fin, etape, etat, assigne:heures_restantes, `Heures passées`:`Sous-tâches`)




#------------ Visualisation n°1



data |> filter(assigne == "Pauline Breton-Chauvet", !is.na(debut)) |> #Pauline Breton-Chauvet
    ggplot() +
    geom_segment(aes(
        x = debut,
        xend = fin,
        y = taches,
        yend = taches,
        lwd = heures_prevues,
        colour = type
    )) +
    ggforce::facet_col(facets = vars(Projet), 
                     scales = "free_y", 
                     space = "free") +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 70)) +
    scale_color_manual(values = c("#7bafc5", "#0073a5")) +
    theme_bw() +
    guides(col = guide_legend(title = "", reverse = TRUE, override.aes = list(lwd=2)),
           lwd = guide_legend(title = "Heures vendues")) +
    geom_vline(aes(xintercept = as.numeric(as.POSIXct(Sys.Date()))), 
               col = "red", size = .35) +
    labs(title = paste("Tâches et sous-tâches de travail renseignées au", 
                       format(as.Date(Sys.Date(), format="%Y-%m-%d %H:%M:%S"),"%d %B %Y")), 
         x = "", y = "") +
    geom_text(aes(x = fin, y = taches, 
                  label = paste0("  ", heures_prevues, "H"), hjust="bottom"),
              size = 3, col = "#333333")


#------------ Visualisation n°2




# Tableau des projets
table_projets <- data |> 
  select(Projet, taches, type, tache_parente, Description, debut, fin, etape, etat, assigne:heures_restantes) |> 
  mutate(heures_prevues = round(heures_prevues, 1),
         heures_passees = round(heures_passees, 1),
         heures_restantes = round(heures_restantes, 1)) |> 
  arrange(Projet)

    group_by(Projet) |> 
    mutate(hprevues = sum(heures_prevues),
           hpassees = sum(heures_passees),
           hrestantes = sum(heures_restantes),
           nb_taches = n()) |> 
    ungroup() |> 
    distinct(Projet, assigne, hprevues, hpassees, hrestantes, debut, fin, nb_taches) |> 
    pivot_longer(cols = c(hprevues, hpassees, hrestantes), 
                 names_to = "periode", 
                 values_to = "heures", 
                 names_prefix = "h") |> 
    mutate(periode = case_when(periode == "passees" ~ "réalisées",
                              periode == "prevues" ~ "prévues",
                              TRUE ~ periode),
           periode = factor(periode, ordered = T, levels = c("prévues", "réalisées", "restantes")),
           Projet = reorder(Projet, heures, desc = T))



        # BROUILLON


# Graph
cumul_projets |> filter(assigne == "Pauline Breton-Chauvet") |> 
      ggplot(aes(x=Projet, y = heures, fill = periode, group = periode)) +
      geom_bar(stat="identity", position = "dodge", width=.6, col = "white", size = .5) +
      labs(title = paste("Heures prévues, réalisées et restantes des missions au", 
                           format(as.Date(Sys.Date(), format="%Y-%m-%d %H:%M:%S"),"%d %B %Y")), 
           y = "Nombre d'heures totales", x = "") +
      # scale_fill_manual(values = c("réalisées" = "#ee7440", 
      #                              "prévues" = "#3368cf",
      #                              "restantes" = "#18ba13")) +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 70)) +
      guides(fill = guide_legend(title = "Heures", reverse = F)) +
      facet_wrap(~Projet, scales = "free_x", labeller = label_wrap_gen(width = 50, multi_line = TRUE)) +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
ggplotly(graph)

# Graph Pauline

# Transformation des données
pauline_ird <- data |> 
    filter(etat != "Pret",
           Projet != "Tâches non facturables à timesheeter") |> 
    group_by(taches) |> 
    mutate(hprevues = sum(heures_prevues),
           hpassees = sum(heures_passees),
           hrestantes = sum(heures_restantes),
           nb_taches = n()) |> 
    ungroup() |> 
    distinct(Projet, taches, assigne, hprevues, hpassees, hrestantes, debut, fin, nb_taches) |> 
    pivot_longer(cols = c(hprevues, hpassees, hrestantes), 
                 names_to = "periode", 
                 values_to = "heures", 
                 names_prefix = "h") |> 
    mutate(periode = case_when(periode == "passees" ~ "réalisées",
                              periode == "prevues" ~ "prévues",
                              TRUE ~ periode),
           periode = factor(periode, ordered = T, levels = c("prévues", "réalisées", "restantes")),
           Projet = reorder(Projet, heures, desc = T))|> 
    filter(assigne == "Pauline Breton-Chauvet",
               Projet == "Etude de cadrage Horizon [IRD]")
      
pauline_ird |> 
  ggplot(aes(x=taches, y = heures, fill = periode, group = periode)) +
      geom_bar(stat="identity", position = "dodge", width=.6, col = "white", size = .5) +
      labs(title = "Heures prévues, réalisées et restantes de la mission au 29/03/2023", 
           y = "Nombre d'heures totales", x = "") +
      # scale_fill_manual(values = c("réalisées" = "#ee7440", 
      #                              "prévues" = "#3368cf",
      #                              "restantes" = "#18ba13")) +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 70)) +
      guides(fill = guide_legend(title = "Heures", reverse = F)) +
      facet_wrap(~taches, scales = "free_x", labeller = label_wrap_gen(width = 50, multi_line = TRUE)) +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) + 
      # geom_text(aes(x = taches, y = heures, label = paste0("  ", heures, "H"), group = periode), 
      #           vjust = -0.5, size = 2.5)
      geom_text(aes(x = taches, y = heures+5, label = paste0("  ", round(heures, 0), "h")), position = position_dodge(width = 0.6), size = 3)

ggplotly(graph)



# Viusalisation n°3
data |> 
    filter(assigne == "Anne-Laure Donzel", 
           etat != "Pret",
           Projet != "Tâches non facturables à timesheeter") |> 
    group_by(Projet) |> 
    mutate(hprevues = sum(heures_prevues),
           hpassees = sum(heures_passees),
           hrestantes = sum(heures_restantes),
           nb_taches = n()) |> 
    ungroup() |> 
    distinct(Projet, hprevues, hpassees, hrestantes, debut, fin, nb_taches) |> 
    pivot_longer(cols = c(hprevues, hpassees, hrestantes), 
                 names_to = "periode", 
                 values_to = "heures", 
                 names_prefix = "h") |> 
    mutate(periode = case_when(periode == "passees" ~ "réalisées",
                              periode == "prevues" ~ "prévues",
                              TRUE ~ periode),
           periode = factor(periode, ordered = T, levels = c("prévues", "réalisées", "restantes")),
           Projet = reorder(Projet, heures, desc = T)) |> #mutate(title_projet = reorder(title_projet, n, desc=T)) %>% 
  ggplot(aes(x=Projet, y = n, fill = Genre, group = Genre, text = paste(part, "%")))+
    geom_bar(stat="identity", position = "dodge", width=.6, col = "white", size = .2) +
    labs(x = "", y = "Nombre d'utilisateurs") +
    theme_classic() +
    coord_flip() +
    scale_fill_manual(values = c("#da4729", "#21468d")) +
    theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(hjust = 1)) +
    guides(fill = guide_legend(title = "", reverse = TRUE))
ggplotly(graph, tooltip = c("text"))




# Visualisation n°4
data(df_konj)
