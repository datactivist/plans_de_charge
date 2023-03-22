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
pauline <- data |> filter(assigne == "Pauline Breton-Chauvet")
pauline <- data |> filter(assigne == "Anne-Laure Donzel")



# Visualisation n°1
#graph <- 
library(plotly)
graph <- pauline |> filter(!is.na(debut)) |> 
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
    guides(lwd = "none") +
    geom_vline(aes(xintercept = as.numeric(as.POSIXct(Sys.Date()))), 
               col = "red", size = .35)

ggplotly(graph) %>% 
    layout(legend = list(orientation = "h", x = 0.4, y = -0.2),
           showlegend = FALSE) 


# Visualisation n°2

# Data
cumul_projet <- pauline |> 
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
           periode = factor(periode, ordered = T, levels = c("restantes", "réalisées", "prévues")),
           Projet = reorder(Projet, heures, desc = T))


# Graph
cumul_projet |> 
    ggplot(aes(x=Projet, y = heures, fill = periode, group = periode)) +
    geom_bar(stat="identity", position = "dodge", width=.6, col = "white", size = .2) +
    theme_classic() +
    coord_flip() +
    labs(title = "Heures prévues, réalisées et restantes des missions", 
         y = "Nombre d'heures totales", x = "") +
    scale_fill_manual(values = c("réalisées" = "#ee7440", 
                                 "prévues" = "#3368cf",
                                 "restantes" = "#18ba13")) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 70)) +
    guides(fill = guide_legend(title = "Heures", reverse = TRUE))
ggplotly(graph)


graph <- cumul_projet %>% #mutate(title_projet = reorder(title_projet, n, desc=T)) %>% 
    #filter(n > 50) %>% 
  ggplot(aes(x=Projet, y = n, fill = Genre, group = Genre, text = paste(part, "%")))+
    geom_bar(stat="identity", position = "dodge", width=.6, col = "white", size = .2) +
    labs(x = "", y = "Nombre d'utilisateurs", 
         title = paste0("Genre des contributeurs par projet de plus de 50 contributions\n(", (table %>% filter(!is.na(gender)) %>% summarise(n=n()))$n, " genres connus sur ", nrow(table), " contributions)", sep = "")) +
    theme_classic() +
    coord_flip() +
    scale_fill_manual(values = c("#da4729", "#21468d")) +
    theme(legend.position = "right",
        text = element_text(family = "Montserrat", size = 12),
        plot.title = element_text(hjust = 1)) +
    guides(fill = guide_legend(title = "", reverse = TRUE))
ggplotly(graph, tooltip = c("text"))



