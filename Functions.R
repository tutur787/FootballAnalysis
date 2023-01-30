library(tidyverse)
library(radarchart)
library(fmsb)
library(plotly)
#library(shiny)
library(ggrepel)

### IMPORTS ###

prem_players_stats <- read_csv("Players.csv")
prem_players_stats <- prem_players_stats[1:507,] %>% filter(Pos != "GK")
prem_players_stats[, 4:15] <- sapply(prem_players_stats[, 4:15], as.numeric)
prem_players_stats$Pos <- substring(prem_players_stats$Pos, 1, 2)

player_names = unique(prem_players_stats$Player)
random_name_1 = sample(player_names, 1)
random_name_2 = sample(player_names, 1)
rand_pos = prem_players_stats %>% filter(Player == random_name_2) %>% pull(Pos)
subset_df = prem_players_stats[prem_players_stats$Pos == rand_pos, ]
random_name_3 = sample(subset_df$Player, 1)
player_names = setNames(player_names, player_names)

prem_team_stats <- read_csv("Teams.csv")

team_names = unique(prem_team_stats$Squad)
random_team = sample(team_names, 1)
team_names = setNames(team_names, team_names)

### FUNCTION ###
#### PLAYER ####
create_graph_player <- function(data, player){
  ## COLORS FOR THE GRAPH ##
  colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) )
  colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) )
  ## SETTING UP ##
  position <- data %>% filter(Player == player) %>% pull(Pos)
  ## SWITCH ##
  switch(position,
         "FW" = {
           data_1 <- data %>% filter(Player %in% c(player,"mean_row")) %>% select(xG, xAG, `Cmp%`, KP, `SoT%`, `Succ%`)
           data_1 <- as.data.frame(data_1)
           scales <- list(
             xG = c(2,0),
             xAG = c(2,0),
             `Cmp%` = c(100,0),
             KP = c(5,0),
             `SoT%` = c(100,0),
             `Succ%` = c(100,0)
           )
           scales <- as.data.frame(scales)
           scales <- mutate(scales, 
                            `Cmp%` = Cmp.,
                            `SoT%` = SoT.,
                            `Succ%` = Succ.) %>% select(xG, xAG, `Cmp%`, KP, `SoT%`, `Succ%`)
           data_sum <- data %>% filter(Pos == "FW") %>% filter(`90s` >= 3) %>% summarise(xG=mean(xG, na.rm=TRUE), xAG=mean(xAG, na.rm=TRUE), `Cmp%`=mean(`Cmp%`, na.rm=TRUE), KP=mean(KP, na.rm=TRUE), `SoT%`=mean(`SoT%`, na.rm=TRUE),`Succ%`=mean(`Succ%`, na.rm=TRUE))
           data_2 <- rbind(scales, data_1, data_sum)
           radarchart(data_2, axistype=1,
                      pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                      cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,5,1.25), cglwd=0.8,
                      vlcex=1.5 )
           legend(x=0.2, y=1.2, legend = c(player, "Prem Average"), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
         },
         "MF" = {
           data_1 <- data %>% filter(Player %in% c(player,"mean_row")) %>% select(xAG, `Cmp%`, KP, `Succ%`, `Tkl+Int`,`Tkl%`)
           data_1 <- as.data.frame(data_1)
           scales <- list(
             xAG = c(2,0),
             `Cmp%` = c(100,0),
             KP = c(5,0),
             `Succ%` = c(100,0),
             `Tkl+Int` = c(20,0),
             `Tkl%` = c(100,0)
           )
           scales <- as.data.frame(scales)
           scales <- mutate(scales, 
                            `Cmp%` = Cmp.,
                            `Succ%` = Succ.,
                            `Tkl%` = Tkl.,
                            `Tkl+Int` = Tkl.Int) %>% select(xAG, `Cmp%`, KP, `Succ%`, `Tkl+Int`,`Tkl%`)
           data_sum <- data %>% filter(Pos == "MF") %>% filter(`90s` >= 3) %>% summarise(xAG=mean(xAG, na.rm=TRUE), `Cmp%`=mean(`Cmp%`, na.rm=TRUE), KP=mean(KP, na.rm=TRUE), `Succ%`=mean(`Succ%`, na.rm=TRUE), `Tkl+Int`=mean(`Tkl+Int`, na.rm=TRUE), `Tkl%`=mean(`Tkl%`, na.rm=TRUE))
           data_2 <- rbind(scales, data_1, data_sum)
           radarchart(data_2, axistype=1,
                      pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                      cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                      vlcex=1.5 )
           legend(x=0.2, y=1.2, legend = c(player, "Prem Average"), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
         },
         "DF" = {
           data_1 <- data %>% filter(Player %in% c(player,"mean_row")) %>% select(`Cmp%`,`Tkl+Int`,`Tkl%`,`Def 3rd`,`Clr`,`CrsPA`)
           data_1 <- as.data.frame(data_1)
           scales <- list(
             `Cmp%` = c(100,0),
             `Tkl+Int` = c(6,0),
             `Tkl%` = c(100,0),
             `Def 3rd` = c(10,0),
             `Clr` = c(20,0),
             `CrsPA` = c(10,0)
           )
           scales <- as.data.frame(scales)
           scales <- mutate(scales, 
                            `Cmp%` = Cmp.,
                            `Tkl%` = Tkl.,
                            `Tkl+Int` = Tkl.Int,
                            `Def 3rd` = Def.3rd,) %>% select(`Cmp%`,`Tkl+Int`,`Tkl%`,`Def 3rd`,`Clr`,`CrsPA`)
           data_sum <- data %>% filter(Pos == "DF") %>% filter(`90s` >= 3) %>% summarise(`Cmp%`=mean(`Cmp%`, na.rm=TRUE), `Tkl+Int`=mean(`Tkl+Int`, na.rm=TRUE), `Tkl%`=mean(`Tkl%`, na.rm=TRUE), `Def 3rd`=mean(`Def 3rd`,na.rm=TRUE), `Clr`=mean(`Clr`,na.rm=TRUE),`CrsPA`=mean(`CrsPA`,na.rm=TRUE))
           data_2 <- rbind(scales, data_1, data_sum)
           radarchart(data_2, axistype=1,
                      pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                      cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                      vlcex=1.5 )
           legend(x=0.2, y=1.2, legend = c(player, "Prem Average"), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
         }
  )
}

#### COMPARISON ####

create_graph_players<- function(data, player1, player2){
  ## COLORS FOR THE GRAPH ##
  colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) )
  colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) )
  ## SETTING UP ##
  position1 <- data %>% filter(Player == player1) %>% pull(Pos)
  position2 <- data %>% filter(Player == player2) %>% pull(Pos)
  if (position1 != position2){
    stop("Select players with the same position!")
  }
  position <- position1
  ## SWITCH ##
  switch(position,
         "FW" = {
           data_1 <- data %>% filter(Player %in% c(player1, player2)) %>% select(xG, xAG, `Cmp%`, KP, `SoT%`, `Succ%`)
           data_1 <- as.data.frame(data_1)
           scales <- list(
             xG = c(2,0),
             xAG = c(2,0),
             `Cmp%` = c(100,0),
             KP = c(5,0),
             `SoT%` = c(100,0),
             `Succ%` = c(100,0)
           )
           scales <- as.data.frame(scales)
           scales <- mutate(scales, 
                            `Cmp%` = Cmp.,
                            `SoT%` = SoT.,
                            `Succ%` = Succ.) %>% select(xG, xAG, `Cmp%`, KP, `SoT%`, `Succ%`)
           data_2 <- rbind(scales, data_1)
           radarchart(data_2, axistype=1,
                      pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                      cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,5,1.25), cglwd=0.8,
                      vlcex=1.5 )
           legend(x=0.2, y=1.2, legend = c(player1, player2), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
         },
         "MF" = {
           data_1 <- data %>% filter(Player %in% c(player1, player2)) %>% select(xAG, `Cmp%`, KP, `Succ%`, `Tkl+Int`,`Tkl%`)
           data_1 <- as.data.frame(data_1)
           scales <- list(
             xAG = c(2,0),
             `Cmp%` = c(100,0),
             KP = c(5,0),
             `Succ%` = c(100,0),
             `Tkl+Int` = c(20,0),
             `Tkl%` = c(100,0)
           )
           scales <- as.data.frame(scales)
           scales <- mutate(scales, 
                            `Cmp%` = Cmp.,
                            `Succ%` = Succ.,
                            `Tkl%` = Tkl.,
                            `Tkl+Int` = Tkl.Int) %>% select(xAG, `Cmp%`, KP, `Succ%`, `Tkl+Int`,`Tkl%`)
           data_2 <- rbind(scales, data_1)
           radarchart(data_2, axistype=1,
                      pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                      cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                      vlcex=1.5 )
           legend(x=0.2, y=1.2, legend = c(player1, player2), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
         },
         "DF" = {
           data_1 <- data %>% filter(Player %in% c(player1, player2)) %>% select(`Cmp%`,`Tkl+Int`,`Tkl%`,`Def 3rd`,`Clr`,`CrsPA`)
           data_1 <- as.data.frame(data_1)
           scales <- list(
             `Cmp%` = c(100,0),
             `Tkl+Int` = c(6,0),
             `Tkl%` = c(100,0),
             `Def 3rd` = c(10,0),
             `Clr` = c(20,0),
             `CrsPA` = c(10,0)
           )
           scales <- as.data.frame(scales)
           scales <- mutate(scales, 
                            `Cmp%` = Cmp.,
                            `Tkl%` = Tkl.,
                            `Tkl+Int` = Tkl.Int,
                            `Def 3rd` = Def.3rd,) %>% select(`Cmp%`,`Tkl+Int`,`Tkl%`,`Def 3rd`,`Clr`,`CrsPA`)
           data_2 <- rbind(scales, data_1)
           radarchart(data_2, axistype=1,
                      pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                      cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                      vlcex=1.5 )
           legend(x=0.2, y=1.2, legend = c(player1, player2), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
           }
  )
}


#### TEAM ####

create_graph_team <- function(data, team, stat) {
  switch(stat,
         "Gls, Ast" = {
           data_1 <- data %>% select(Squad, Gls, Ast)
           data_1$color <- ifelse(data_1$Squad == team, team, "Other teams")
           ggplot(data_1, aes(x = Gls, y = Ast, col = color)) + 
             geom_point(size=3, alpha = 0.4, shape=21) + 
             geom_abline(slope = 1, intercept = 0, colour = "black", linetype=2) +
             geom_label_repel(aes(label = Squad)) +
             scale_x_continuous(limits = c(0,3), name = "Goals/90") +
             scale_y_continuous(limits = c(0,3), "Assist/90")
         },
         "Poss" = {
           data_1 <- data %>% select(Squad, Poss)
           ggplot(data_1, aes(x = Squad, y = Poss)) + 
             geom_bar(stat = "identity", aes(fill = Squad)) + 
             geom_text(aes(label = Poss), vjust = -0.2)+
             scale_fill_manual(values = ifelse(data_1$Squad == team, "red", "midnightblue")) +
             theme(axis.text.x = element_text(angle = 90, hjust = 1))
         },
         "Gls, xG" = {
           data_1 <- data %>% select(Squad, Gls, xG)
           data_1$color <- ifelse(data_1$Squad == team, team, "Other teams")
           ggplot(data_1, aes(x = Gls, y = xG, col = color)) + 
             geom_point(size=3, alpha = 0.4, shape=21) + 
             geom_abline(slope = 1, intercept = 0, colour = "black", linetype=2) +
             geom_label_repel(aes(label = Squad)) +
             scale_x_continuous(limits = c(0,3), name = "Goals/90") +
             scale_y_continuous(limits = c(0,3), "xG/90")
         }
         )
}
