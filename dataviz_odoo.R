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
         etape = `Étape`,
         todo = Titre,
         `Heures prévues` = `Heures prévues initialement`) |> 
  mutate(type = case_when(!is.na(tache_parente) ~ "Sous-tâche", .default = "Tâche"),
         taches = coalesce(tache_parente, paste0(todo, 1))) |> 
  select(Projet, todo, type, tache_parente, taches, Description, Progression, debut, fin, etape, etat, `Heures passées`:`Heures restantes`, `Assigné à`) |> 
  arrange(Projet, taches) |> 
  group_by(Projet) |> 
  arrange(desc(taches)) |> 
  mutate(id = row_number()) |> 
  mutate(todo = fct_reorder(todo, -id)) |> 
  ungroup()




#------------ Visualisation n°1



data |> 
  filter(`Assigné à` == "Sarah Bourgouin", !is.na(debut)) |> 
  mutate(todo = fct_reorder(todo, -id)) |> 
    ggplot() +
    geom_segment(aes(
        x = debut,
        xend = fin,
        y = todo,
        yend = todo,
        lwd = `Heures prévues`,
        colour = type
    )) +
    ggforce::facet_col(facets = vars(Projet), 
                     scales = "free_y", 
                     space = "free") +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 70)) +
    scale_color_manual(values = c("#7bafc5", "#0073a5")) +
    theme_bw() +
    guides(col = guide_legend(title = "", reverse = TRUE, override.aes = list(lwd=2)),
           lwd = guide_legend(title = "Heures prévues")) +
    geom_vline(aes(xintercept = as.numeric(as.POSIXct(Sys.Date()))), 
               col = "red", size = .35) +
    labs(title = paste("Tâches et sous-tâches de travail renseignées au", 
                       format(as.Date(Sys.Date(), format="%Y-%m-%d %H:%M:%S"),"%d %B %Y")), 
         x = "", y = "") +
    geom_text(aes(x = fin, y = todo, 
                  label = paste0("  ", `Heures prévues`, "H"), hjust="bottom"),
              size = 3, col = "#333333")


#------------ Visualisation n°2




# Tableau des projets
table_projets <- data |> as.data.frame() |> 
  mutate(`Heures prévues` = round(`Heures prévues`, 1),
         `Heures passées` = round(`Heures passées`, 1),
         `Heures restantes` = round(`Heures restantes`, 1),
         ` ` = ifelse(type == "Tâche", "clipboard-check", "list-check"),
         état = case_when(etat == "En cours" ~ "circle", 
                          etat == "Prêt" ~ "circle-check", 
                          .default = NA_character_)) |> 
  arrange(Projet, taches) |> 
  rename(`heures prévues` = `Heures prévues`,
         `heures réalisées` = `Heures passées`,
         `heures restantes` = `Heures restantes`,
         `assigné` = `Assigné à`) |> 
  mutate(debut = as.Date(debut, format="%Y-%m-%d %H:%M:%S"),"%d-%m-%Y",
         fin = as.Date(fin, format="%Y-%m-%d %H:%M:%S"),"%d-%m-%Y") |> 
  select(Projet, ` `, type, todo, debut, fin, `heures prévues`:`heures restantes`, etape, état, `assigné`, id) |> 
  mutate_all(as.character) |> 
  mutate_at(vars(everything()), replace_na, replace = "") |> 
  mutate(`heures restantes` = as.numeric(`heures restantes`)) |> 
  filter(`assigné` == "Sarah Bourgouin")

# Représentation du tableau
library(gt)
library(gtExtras)
table_projets |> 
  group_by(Projet) |> 
  arrange(id) |> 
  select(-id) |> 
  gt(groupname_col = "Projet") |>
  gt_fa_column(column = ` `, direction = 1) |>  #icones
  gt_fa_column(column = état, direction = 1, height = '15px', prefer_type = "solid", 
               palette = if (all(table_projets$état == "circle")) "#999999" 
               else if (all(table_projets$état == "circle-check")) "green" else c("circle" = "#999999", "circle-check" = "green")) |> 
  gt_theme_nytimes() |> 
  tab_header(title = "Tableau global des tâches et sous-tâches par personne") |>
  tab_style(  #en gras
    style = list(cell_text(weight = "bold")),
    locations = cells_body(
    columns = Projet)) |>
  tab_style(  #en gras
    style = list(cell_text(weight = "bold")),
    locations = cells_body(
    columns = `heures restantes`)) |>
  tab_style(  #en gras
    style = list(cell_text(color = "#f44336")),
    locations = cells_body(
    columns = `heures restantes`,
    rows = `heures restantes` < 0)) |>
  tab_style(  #en gras
    style = list(cell_text(color = "#38761d")),
    locations = cells_body(
    columns = `heures restantes`,
    rows = `heures restantes` >= 0)) |>
  tab_options(table.font.size = 17,
              table.border.bottom.width = 1,
              table.border.bottom.color = "#CCCCCC",
              row_group.background.color = "grey")
  


#------------ Visualisation n°3



# Tableau des heures restantes divisées par semaine jusqu'à fin tache / sous-tache
table_restant_semaine <- data |> 
  filter(`Heures restantes` > 0) |> 
  mutate(today = Sys.Date(),
         nb_week_left = round(as.numeric(difftime(fin, today, units = "weeks")), 0),
         hours_week_left = `Heures restantes` / nb_week_left) |> 
  filter(nb_week_left > 0) |> 
  select(Projet, todo, `Assigné à`, today, fin, nb_week_left, hours_week_left) |> 
  group_by(rn = row_number()) %>% 
  mutate(complete_date = list(seq(as.Date(today), as.Date(fin), by = 7))) |> 
  unnest()|> ungroup() |> select(-rn) |> 
  filter(`Assigné à` == "Sarah Bourgouin") |> 
  mutate(hours_total = ifelse(row_number() == 1, sum(hours_week_left), NA_real_), .by = complete_date) |> 
  arrange(complete_date) |> 
  mutate(hours_total = ifelse(row_number() == 1, hours_total, NA_real_), .by = hours_total) |> 
  mutate(todo = paste0(Projet, "\n", todo))

# Dataviz
graph <- table_restant_semaine |> 
  ggplot(aes(x = complete_date, y = hours_week_left, fill = todo, 
             text = paste0("Todo : ", todo, "\nProjet : ", Projet, "\nTemps : ", round(hours_week_left, 1), 
                           " heures restantes par semaine \n du ", format(today, '%d %B %Y'), " jusqu'au ", format(fin, '%d %B %Y')))) +
  geom_bar(stat = "identity") +
  geom_hline(aes(yintercept = round(32*0.857,0)), col = "red", size = .35) + #round(35*0.857,0) pour un temps plein
  geom_text(aes(x = complete_date, y = hours_total+2, label = paste0(round(hours_total, 0), "H")), 
            size = 3, vjust = 1) +
  labs(title = paste("Répartition des heures restantes par tâches et par semaine -", table_restant_semaine[1,3]),
       y = "Heures restantes vendues par semaine") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  scale_fill_viridis_d() +
  guides(fill = guide_legend(title = "Tâches et sous-tâches")) +
  theme_classic() +
  theme(axis.title.x = element_blank())
ggplotly(graph, tooltip = c("text")) |> 
    layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE))
  

  

# Tableau des heures restantes divisées par mois jusqu'à fin tache / sous-tache
table_restant_mois <- data |> 
  filter(`Heures restantes` > 0) |> 
  mutate(today = Sys.Date(),
         nb_month_left = round(as.numeric(difftime(fin, today, units = "weeks") /4.34524), 0),
         hours_month_left = case_when(nb_month_left >=1 ~ `Heures restantes` / nb_month_left,
                                      nb_month_left < 1 ~ `Heures restantes`)) |> 
  filter(nb_month_left > 0) |> 
  select(Projet, todo, `Assigné à`, today, fin, nb_month_left, hours_month_left) |> 
  group_by(rn = row_number()) %>% 
  mutate(complete_date = list(seq(as.Date(today), as.Date(fin), by = "month"))) |> 
  unnest()|> ungroup() |> select(-rn) |> 
  filter(`Assigné à` == "Sarah Bourgouin") |> 
  mutate(hours_total = ifelse(row_number() == 1, sum(hours_month_left), NA_real_), .by = complete_date) |> 
  arrange(complete_date) |> 
  mutate(hours_total = ifelse(row_number() == 1, hours_total, NA_real_), .by = hours_total) |> 
  mutate(todo = paste0(Projet, "\n", todo))

# Dataviz
graph <- table_restant_mois |> 
  ggplot(aes(x = complete_date, y = hours_month_left, fill = todo, 
             text = paste0("Todo : ", todo, "\nProjet : ", Projet, "\nTemps : ", round(hours_month_left, 1), 
                           " heures restantes par mois \n du ", format(today, '%d %B %Y'), " jusqu'au ", format(fin, '%d %B %Y')))) +
  geom_bar(stat = "identity") +
  geom_hline(aes(yintercept = round(17*0.857*8,0)), col = "red", size = .35) + #round(21*0.857*7,0)
  geom_text(aes(x = complete_date, y = hours_total+2, label = paste0(round(hours_total, 0), "H")), 
            size = 3, vjust = 1) +
  labs(title = paste("Répartition des heures restantes par tâches et par mois au", format(Sys.Date(), '%d %B %Y'), "-", table_restant_mois[1,3]),
       y = "Heures restantes vendues par mois") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  scale_fill_viridis_d() +
  guides(fill = guide_legend(title = "Tâches et sous-tâches")) +
  theme_classic() +
  theme(axis.title.x = element_blank())
ggplotly(graph, tooltip = c("text")) |> 
    layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE))
  


















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
    mutate(hprevues = sum(`Heures prévues`),
           hpassees = sum(`Heures passées`),
           hrestantes = sum(`Heures restantes`),
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
    mutate(hprevues = sum(`Heures prévues`),
           hpassees = sum(`Heures passées`),
           hrestantes = sum(`Heures restantes`),
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
