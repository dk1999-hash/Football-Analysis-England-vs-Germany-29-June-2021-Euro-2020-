install.packages(c("readxl", "dplyr", "ggplot2", "magrittr", "MASS", "skimr", "ggrepel", "scales", "fmsb", "remotes", "knitr", "tidyr"))
remotes::install_github("statsbomb/StatsBombR")
# Install devtools if not installed
if (!requireNamespace("devtools")) install.packages("devtools")

remotes::install_github("Torvaney/ggsoccer", force = TRUE)

library(readxl)
library(dplyr)
library(ggplot2)
library(magrittr)
library(MASS)
library(skimr)
library(ggrepel)
library(scales)
library(fmsb)
library(knitr)
library(ggsoccer)
library(tidyr)
library(StatsBombR)
library(StatsBombR)

comps <- FreeCompetitions()

euro2020 <- comps %>% filter(competition_id == 55, season_id == 43)

matches <- FreeMatches(euro2020)

# Find England vs Germany (29 June 2021)
eng_ger <- matches %>%
  filter(home_team.home_team_name == "England",
         away_team.away_team_name == "Germany",
         match_date == "2021-06-29")
glimpse(eng_ger)

# Pull match_id
match_id <- eng_ger$match_id


events <- get.matchFree(Match = data.frame(match_id = 3794688))


# Check result
glimpse(events)
events_df <- as.data.frame(events)
events_df
names(events_df)


df <- read_excel("Data Insights Task.xlsx")

#Shot map
df_shots <- df %>%
  dplyr::select(Name, Team, xG, `All Goals`, `Average Pressure X`, `LBP F3`) %>%
  rename(
    goal = `All Goals`,
    location.x = `Average Pressure X`,
    location.y = `LBP F3`
  ) %>%
  filter(!is.na(location.x), !is.na(location.y)) %>%
  mutate(
    goal = ifelse(goal > 0, "Goal", "Miss"),
    location.x = scales::rescale(location.x, to = c(0, 120)),
    location.y = scales::rescale(location.y, to = c(0, 80))
  )



df_shots <- df_shots %>%
  mutate(
    location.x = pmin(pmax(location.x, 0), 120),
    location.y = pmin(pmax(location.y, 0), 80)
  )

# Generating pitch theme
theme_pitch <- function() {
  theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_rect(fill = "#4E8C45", colour = NA),
      panel.background = element_rect(fill = "#4E8C45", colour = NA),
      legend.background = element_rect(fill = "#4E8C45", colour = NA),
      legend.key = element_rect(fill = "#4E8C45", colour = NA),
      legend.text = element_text(colour = "white"),
      legend.title = element_text(colour = "white"),
      plot.title = element_text(size = 16, face = "bold", colour = "white"),
      plot.subtitle = element_text(size = 12, colour = "white")
    )
}

p <- ggplot(df_shots) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "#4E8C45") +
  theme_pitch() +
  geom_point(aes(x = location.x, y = location.y, size = xG, colour = goal), alpha = 0.85) +
  geom_text_repel(
    aes(x = location.x, y = location.y, label = Name),
    size = 3, colour = "white", force = 1.2, max.overlaps = 40,
    box.padding = 0.4, point.padding = 0.2
  ) +
  scale_colour_manual(values = c("Goal" = "yellow", "Miss" = "deepskyblue")) +
  scale_size(range = c(3, 8)) +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "Shots – England vs Germany (29 June 2021)",
    subtitle = "From Actual Dataset | xG = point size",
    colour = "goal",
    size = "xG"
  )

print(p)


ggsave("simulated_shot_map.png", p, width = 10.5, height = 6.8, dpi = 300)

if (!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("jogall/soccermatics", force = TRUE)
library(soccermatics)


library(dplyr)

library(ggplot2)
library(soccermatics)
library(dplyr)

library(dplyr)
library(ggplot2)
library(soccermatics)
library(scales)
library(tidyr)
library(viridis)

# Custom pitch theme
theme_pitch <- function() {
  theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_rect(fill = "#1B3B2F", colour = NA),
      panel.background = element_rect(fill = "#1B3B2F", colour = NA),
      legend.background = element_rect(fill = "#1B3B2F"),
      legend.key = element_rect(fill = "#1B3B2F"),
      legend.title = element_text(colour = "white", size = 10),
      legend.text = element_text(colour = "white"),
      plot.title = element_text(size = 14, face = "bold", colour = "white"),
      plot.subtitle = element_text(size = 10, colour = "white")
    )
}
#pressure heatmaps for england vs germany
# Extract and clean pressure event coordinates
prepare_pressure_data <- function(df, team_name) {
  df %>%
    filter(type.name == "Pressure", team.name == team_name) %>%
    filter(!is.na(location)) %>%
    mutate(
      location.x = purrr::map_dbl(location, 1),
      location.y = purrr::map_dbl(location, 2)
    ) %>%
    filter(!is.na(location.x), !is.na(location.y)) %>%
    mutate(
      location.x = pmin(pmax(location.x, 0), 120),
      location.y = pmin(pmax(location.y, 0), 80)
    )
}


# Plot pressure heatmap
plot_pressure_heatmap <- function(data, team_name) {
  ggplot(data, aes(x = location.x, y = location.y)) +
    annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "#1B3B2F") +
    stat_density_2d_filled(
      contour_var = "density",
      alpha = 0.85,
      adjust = 1.2,
      n = 200,
      h = c(6, 6)
    ) +
    scale_fill_viridis_d(option = "C") +
    theme_pitch() +
    labs(
      title = paste(team_name, "Pressure Heatmap – England vs Germany (29 June 2021)"),
      subtitle = "Built using pressure event coordinates from StatsBomb",
      fill = "Density"
    )
}

#Prepare the cleaned pressure data for each team
england_pressure <- prepare_pressure_data(events_df, "England")
germany_pressure <- prepare_pressure_data(events_df, "Germany")
# Display plots
plot_pressure_heatmap(england_pressure, "England")
plot_pressure_heatmap(germany_pressure, "Germany")

#Creating xG and xG Assisted per 90 chart
df_xGA <- df %>%
  mutate(minutes = as.numeric(Minutes)) %>%
  mutate(
    xG = as.numeric(xG),
    xGA = as.numeric(`xG Assisted`)
  ) %>%
  filter(minutes > 0) %>%
  mutate(
    xG_90 = round(xG / minutes * 90, 2),
    xGA_90 = round(xGA / minutes * 90, 2)
  ) %>%
  filter(xG_90 > 0 | xGA_90 > 0)  # <<=== FILTER OUT NON-CONTRIBUTORS

# Pivot for stacked bar plot
tidyr::pivot_longer(
  df_xGA %>%
    dplyr::select(Name, xG_90, xGA_90),
  cols = c(xG_90, xGA_90),
  names_to = "Type",
  values_to = "Per90"
)
library(tidyr)
df_long <- df_xGA %>%
  dplyr::select(Name, xG_90, xGA_90) %>%
  pivot_longer(cols = c(xG_90, xGA_90), names_to = "Type", values_to = "Per90")

# Plot
ggplot(df_long, aes(x = reorder(Name, Per90), y = Per90, fill = Type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("xGA_90" = "#DC2828", "xG_90" = "#3778C2"),
                    labels = c("xG" = "xG", "xGA_90" = "xG Assisted")) +
  labs(
    title = "Expected Goal Contribution",
    subtitle = "England vs Germany, 29 June 2021",
    x = NULL,
    y = "Per 90"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Create radar chart comparing key England players
england_keys <- c("Kieran Trippier", "Declan Rice", "Luke Shaw", "Jack Grealish")


vars <- c("xG", "xG Assisted", "Key Passes", "Shots", "Assists",
          "Dribbles", "Tackles", "Interceptions", "Pressures", "Passes Inside Box")

library(tidyr)

df_radar <- df %>%
  filter(Name %in% england_keys) %>%
  rename(`Key Passes` = `Key Passes`,  # make sure this is accurate
         `xG Assisted` = `xG Assisted`,
         `Passes Inside Box` = `Passes Inside Box`) %>% 
  dplyr::select(Name, all_of(vars)) 
df_radar
df_norm <- df_radar
df_norm[vars] <- lapply(df_norm[vars], rescale)

df_chart <- rbind(
  rep(1, length(vars)),     # Max
  rep(0, length(vars)),     # Min
  df_norm[,-1]              # Player values 
)
rownames(df_chart) <- c("Max", "Min", as.character(df_norm$Name))  # Ensure names are characters

# Step 6: Plot radar chart
colors_border <- c("red", "blue", "green", "purple")
colors_fill <- adjustcolor(colors_border, alpha.f = 0.25)

radarchart(df_chart,
           axistype = 1,
           pcol = colors_border,
           pfcol = colors_fill,
           plwd = 2,
           cglcol = "grey", cglty = 1, axislabcol = "grey", 
           caxislabels = c("0", "0.25", "0.5", "0.75", "1"), 
           cglwd = 0.8,
           vlcex = 0.8)

legend("bottomleft", legend = as.character(df_norm$Name), 
       col = colors_border, lty = 1, lwd = 2, bty = "n")


# Creating table summary
summary_table <- df_shots %>%
  group_by(Team) %>%
  summarise(
    Shots = n(),
    Goals = sum(goal == "Goal"),
    `Average xG` = round(mean(xG, na.rm = TRUE), 2),
    `Key Players (xG)` = paste0(
      paste0(Name[order(-xG)][1:2], " (", round(xG[order(-xG)][1:2], 2), ")"),
      collapse = ", "
    ),
    .groups = "drop"
  )


# Displaying the table
kable(summary_table, caption = "Table 1: Summary of Shot Metrics – England vs Germany (29 June 2021)")



