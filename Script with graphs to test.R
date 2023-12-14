library(groundhog)
library(gridExtra)
library(knitr)
library(tidyr)
library(dplyr)

# reading tables and filtering to relevant data

league_EUW <- read.table(
  file = "C:\\Users\\Matheus Viana\\Documents\\Blackmagic Design\\Final Project\\games (1).csv",
  header = TRUE,
  sep = ','
)

league_BR <- read.table(
  file = "C:\\Users\\Matheus Viana\\Documents\\Blackmagic Design\\Final Project\\full_data_100.csv",
  header = TRUE,
  sep = ','
)
# limit to only the necessary data, which is statistics which are shared between data frames (ex. both data frames have a column to show which team got first blood)
league_BR <- league_BR[, c("blueWin", "blueFirstBlood", "blueDragonKill", "redDragonKill", "blueRiftHeraldKill", "redRiftHeraldKill", "blueBaronKill", "redBaronKill")]

league_EUW <- league_EUW[, c("winner", "firstBlood", "t1_dragonKills", "t2_dragonKills", "t1_riftHeraldKills", "t2_riftHeraldKills", "t1_baronKills", "t2_baronKills")]


league_EUW
league_BR


## My Code ##

# Calculate BR %
br_percentages <- prop.table(table(league_BR$blueWin)) * 100

# Calculate EUW %
euw_percentages <- prop.table(table(factor(league_EUW$winner, levels = c(1, 2)))) * 100


data_plot <- data.frame(
  Category = c("BR_Blue", "BR_Red", "EUW_Blue", "EUW_Red"),
  Percentage = c(br_percentages[2], br_percentages[1], euw_percentages[1], euw_percentages[2])
)


filtered_data <- data_plot[data_plot$Percentage >= 40 & data_plot$Percentage <= 60, ]

# Create a bar plot
ggplot(data_plot, aes(x = Category, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(data = filtered_data, aes(label = paste0(sprintf("%.2f", Percentage), "%")),
            vjust = -0.5, size = 3, color = "black") +
  labs(
    title = "Percentage Distribution: Blue vs. Red",
    x = "Category",
    y = "Percentage"
  ) +
  scale_fill_manual(values = c("#3498db", "#e74c3c", "#3498db", "#e74c3c")) +
  theme_minimal() +
  coord_cartesian(ylim = c(40, 60)) 

# Rift Baron EU

combined_objectives_EUW <- league_EUW %>%
  select(winner, t1_riftHeraldKills, t2_riftHeraldKills, t1_baronKills, t2_baronKills) %>%
  gather(key = "objective", value = "kills", -winner) %>%
  separate(objective, into = c("team", "objective"), sep = "_") %>%
  mutate(team = ifelse(team == "t1", 1, 2))

objectives_wr_EUW <- combined_objectives_EUW %>%
  group_by(kills, winner, team, objective) %>%
  summarise(count = n()) %>%
  group_by(kills, team, objective) %>%
  mutate(win_rate = ifelse(winner == team, count / sum(count) * 100, 0))

plot3 <- ggplot(objectives_wr_EUW, 
                aes(x = kills, 
                    y = win_rate, 
                    fill = factor(winner))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  scale_fill_manual(values = c("#3498db", "#e74c3c"), name = "Winner") +
  labs(title = "Win Rates: Rift Herald, Baron (EUW)",
       x = "Number of Kills",
       y = "Win Rate (%)") +
  facet_wrap(~objective, scales = "free_y", labeller = labeller(objective = c("riftHeraldKills" = "Rift Herald Kills", "baronKills" = "Baron Kills")), ncol = 4) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 100))


#Rift Baron BR
combined_objectives_BR <- league_BR %>%
  select(blueRiftHeraldKill, redRiftHeraldKill, blueBaronKill, redBaronKill) %>%
  pivot_longer(cols = everything(), names_to = "objective", values_to = "kills") %>%
  separate(objective, into = c("team", "objective"), sep = "(?<=blue|red)(?=Rift|Baron)") %>%
  mutate(team = ifelse(team == "blue", TRUE, FALSE))

objectives_wr_BR <- combined_objectives_BR %>%
  group_by(kills, team, objective) %>%
  summarise(count = n()) %>%
  group_by(kills, objective) %>%
  mutate(total_count = sum(count)) %>%
  mutate(win_rate = count / total_count * 100) %>%
  ungroup()  

plot4 <- ggplot(objectives_wr_BR, 
                aes(x = kills, 
                    y = win_rate, 
                    fill = factor(team))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  scale_fill_manual(values = c("#3498db", "#e74c3c"), name = "Winner") +
  labs(title = "Win Rates: Rift Herald, Baron (Brazil)",
       x = "Number of Kills",
       y = "Win Rate (%)") +
  facet_wrap(~objective, scales = "free", labeller = labeller(objective = c("RiftHeraldKill" = "Rift Herald Kills", "BaronKill" = "Baron Kills")), ncol = 4) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 100)) 

grid.arrange(plot3, plot4, ncol = 2)

# table
baron_summary <- objectives_wr_BR %>%
  filter(objective == "BaronKill") %>%
  group_by(kills, team) %>%
  summarise(total_count = sum(count)) %>%
  pivot_wider(names_from = team, values_from = total_count) %>%
  mutate(win_rate_blue = ifelse(!is.na(`TRUE`) & !is.na(`FALSE`), `FALSE` / (`TRUE` + `FALSE`) * 100, 0),
         win_rate_red = ifelse(!is.na(`TRUE`) & !is.na(`FALSE`), `TRUE` / (`TRUE` + `FALSE`) * 100, 0)) %>%
  summarise(avg_win_rate_blue = mean(win_rate_blue),
            avg_win_rate_red = mean(win_rate_red))

kable(baron_summary, format = "markdown", caption = "Average Win Rates for Baron Kills in Brazil")


# Making Tables -MV
table_EUW <- summary_fb_EUW %>%
  select(-firstBlood) %>%
  spread(winner, winrate)

table_BR <- summary_fb_BR %>%
  select(-blueFirstBlood) %>%
  spread(blueWin, winrate)

# Display tables using kable
kable(table_EUW, format = "markdown", caption = "Win Rates based on First Blood (EUW) Note: Blue Team is team 1") 

kable(table_BR, format = "markdown", caption = "Win Rates based on First Blood (Brazil) Note: Blue Team is team 1") 