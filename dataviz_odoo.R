# Visualisation des plans de charge

# Import
library(tidyverse)
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



# Visualisation
#graph <- 
library(plotly)
diane |>
    ggplot() +
    geom_segment(aes(
        x = debut,
        xend = fin,
        y = taches,
        yend = taches,
        lwd = heures_prevues,
        #fill = Projet
    )) +
    # facet_grid(Projet ~ ., 
    #            scales = "free",
    #            space = "free"
    #            ) +
    facet_wrap(~Projet, 
               scales = "free",
               #space = "free",
               ) +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 70)) +
    theme_bw() +
    theme(strip.text.y = element_text(angle = 0))
ggplotly(graph) %>% 
    layout(legend = list(orientation = "h", x = 0.4, y = -0.2),
           showlegend = FALSE)
library(ggiraph)
girafe(graph)



