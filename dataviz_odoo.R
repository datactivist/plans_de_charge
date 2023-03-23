# Visualisation des plans de charge

# Packages
library(tidyverse)
library(plotly)
#devtools::install_github('bbc/bbplot')
#library(bbplot)

# Données
data <- read_csv("project.task.csv")


# Transformation des données
data <- data |> rename(taches = Titre,
                       assigne = `Assigné à`,
                       debut = `Date de début`,
                       fin = `Date de fin`,
                       heures_passees = `Heures passées`,
                       heures_passees_sous_tache = `Heures passées sur les sous-tâches`,
                       heures_prevues = `Heures prévues initialement`,
                       heures_prevues_sous_tache = `Heures prévues sous-tâches`,
                       heures_restantes = `Heures restantes`,
                       tache_parente = `Tâche parente`,
                       assigne_sous_tache = `Sous-tâches/Assigné à`, 
                       etat = `Étiquette d'état Kanban`)
diane <- data |> filter(assigne == "Diane Thierry")
pauline <- data |> filter(assigne == "Pauline Breton-Chauvet")
anne_laure <- data |> filter(assigne == "Anne-Laure Donzel")




#------------ Visualisation n°1



pauline |> filter(!is.na(debut)) |> 
    ggplot() +
    geom_segment(aes(
        x = debut,
        xend = fin,
        y = taches,
        yend = taches,
        lwd = heures_prevues,
        col = heures_prevues
    )) +
    ggforce::facet_col(facets = vars(Projet), 
                     scales = "free_y", 
                     space = "free") +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 70)) +
    scale_color_continuous(high = "#132B43", low = "#56B1F7") +
    theme_bw() +
    # theme(#strip.text.y = element_text(angle = 0),
    #       strip.background = element_blank(),
    #       strip.text.y = element_blank()) +
    guides(col = "none", 
           lwd = guide_legend(title = "Heures vendues")) +
    geom_vline(aes(xintercept = as.numeric(as.POSIXct(Sys.Date()))), 
               col = "red", size = .35) +
    labs(title = paste("Tâches et sous-tâches de travail renseignées au", 
                       format(as.Date(Sys.Date(), format="%Y-%m-%d %H:%M:%S"),"%d %B %Y")), 
         x = "", y = "") +
    geom_text(aes(x = fin, y = taches, 
                  label = paste0("  ", heures_prevues, "H"), hjust="bottom", col = heures_prevues),
              size = 3)
# ggplotly(graph) %>% 
#     layout(legend = list(orientation = "h", x = 0.4, y = -0.2),
#            showlegend = FALSE) 


#------------ Visualisation n°2



# Graph
anne_laure |> 
    filter(etat != "Pret",
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
           Projet = reorder(Projet, heures, desc = T)) |> 
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



# Viusalisation n°3
anne_laure |> 
    filter(etat != "Pret",
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



