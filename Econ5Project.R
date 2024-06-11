#Omkar Bakshi
#Last Updated: 18 March 2021, 3:20 PM

#Found formattable() from https://www.littlemissdata.com/blog/prettytables. 
#Author: Laura Ellis. 
#Last Published: Sept 24, 2018.
install.packages("formattable")

library(formattable)
library(RColorBrewer)
library(tidyr)
library(ggplot2)
library(dplyr)

getwd()
setwd("/Users/omkarbakshi/Desktop")

nba_data <- read.csv("nba_games.csv")

#Put a numeric value of 1 to every Win and 0 to every Loss. This will make it possible to calculate win ratios.
nba_data <- nba_data %>% mutate(win = as.numeric(WINorLOSS == "W"))
nba_data

#Created and added a new variable "Assist Differential", which measures a given team's assists minus its opponents assists for every game played. 
#Selected only the columns relevant to this project. 
nba_data <- nba_data %>% mutate(assists_diff = Assists - Opp.Assists) %>% 
  select(Game, Team, Assists, Opp.Assists, assists_diff, WINorLOSS, win)
formattable(nba_data)
nba_data

#A boxplot that shows the distribution of assist differentials for each team. 
#Found how to do reorder() from https://www.rpubs.com/dvdunne/reorder_ggplot_barchart_axis. No Author or Date Published found. 
boxplot <- ggplot(nba_data, aes(x=reorder(as.factor(Team), -assists_diff), y=assists_diff)) + 
  geom_boxplot(fill="slateblue") +
  xlab("Team") +
  ylab("Assist Differential Per Game") +
  ggtitle("Assist Differential Per Game for NBA Teams")
boxplot

#This groups data by Team and gives the value of the mean of the Assist Differential associated with that team over 2014-2018. 
#In descending order, we are able to see that GSW has the outright highest mean Assist Differential followed by SAS and WAS.
mean_assist_diff <- nba_data %>% group_by(Team) %>%
  summarise(team_mean_assist_diff = mean(assists_diff)) %>% 
  arrange(desc(team_mean_assist_diff))
mean_assist_diff

#This groups data by Team and gives the win ratio (wins/total games played) associated with that team over 2014-2018. In descending order, we are able to see that GSW has won the most games, followed by SAS and HOU. 
win_ratio <- nba_data %>% group_by(Team) %>% 
  summarise(win_percent = sum(win)/328) %>% 
  arrange(desc(win_percent))
win_ratio

#Using the Mean Assist Differential tibble, a bar graph is constructed (mean assist differentials in descending order)
barplot <- ggplot(mean_assist_diff, aes(x=reorder(as.factor(Team), -team_mean_assist_diff), y=team_mean_assist_diff)) + 
  geom_bar(stat="identity", fill="firebrick3") + 
  xlab("Team") + 
  ylab("Mean Assist Differential") + 
  ggtitle("Mean Assist Differentials of NBA Teams (2014-2018)")
barplot

#Using the Win ratio tibble, a bar graph is constructed (win ratios in descending order). 
barplot2 <- ggplot(win_ratio, aes(x=reorder(as.factor(Team), -win_percent), y=win_percent)) + 
  geom_bar(stat="identity", fill = "orange") +
  xlab("Team") + 
  ylab("Win Ratio") +
  ggtitle("Win Ratios of NBA Teams")
barplot2

#This merges the above two variables into a new dataset. 
#The data is matched by Team. The mean Assist Differential is still in descending order; this will allow us to eye the total win number associated with that team and get a surface-level view of the effect of higher mean assist differentials on win record. 
full_data <- merge(mean_assist_diff, win_ratio, by="Team") %>% 
  arrange(desc(team_mean_assist_diff))
full_data

#This is a simple regression of mean assist differentials and win percentages.
regression <- lm(win_percent ~ team_mean_assist_diff, data=full_data)
summary(regression)

#Using the data from the merged data set, a plot relating Games Won and Mean Assist Differential Per Game can be generated. 
#Because there are 30 teams in the merged dataframe, there are 30 points in this plot. 
plot <- full_data %>% ggplot(aes(x=team_mean_assist_diff, y=win_percent)) +
  geom_point() + 
  geom_smooth(method=lm, color="red") +
  xlim(-4, 7) + 
  ylab("Win Ratio") + 
  xlab("Mean Assist Differential Per Game") + 
  ggtitle("Mean Assist Differential vs. Win Ratio")
plot

#Learned cor() from http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r. No Author or Date listed.
#Correlation coefficient of plot (could have just taken square root of R^2 given in the regression summary as well)
cor(as.vector(full_data$team_mean_assist_diff), as.vector(full_data$win_percent), method=c("pearson", "kendall", "spearman"))

#Author: Maurits Evers
#Last Published: Feb 20, 2019
#https://stackoverflow.com/questions/54782522/remove-outliers-from-data-frame-in-r
#Many of the assist differentials are outliers (as seen in the boxplot), which can cause the mean assist differentials to be larger or smaller than reality. 
#To get a better idea of how the correlation between assist difference and win ratio would look like, we should remove the outliers. 
#These are the outliers present in the Assist Differentials observations.
outliers <- boxplot(nba_data$assists_diff, plot = FALSE)$out
outliers

#This is the dataframe without assist differentials outliers.
no_outliers <- nba_data[!(nba_data$assists_diff %in% outliers), ]
no_outliers

#This is the mean assist differential tibble using the tibble above. 
#Note: mad stands for mean assist differentials
no_outliers_mad <- no_outliers %>% group_by(Team) %>% 
  summarise(no_out_mad = mean(assists_diff)) %>% 
  arrange(desc(no_out_mad))
no_outliers_mad  

#This is the win ratio tibble without outliers using the no_outliers dataframe
win_ratio_no_out <- no_outliers %>% group_by(Team) %>% 
  summarise(win_percent = sum(win)/328) %>% 
  arrange(desc(win_percent))
win_ratio_no_out

#This is the merged dataframe of the variables that did not account in the assist differential outliers.
new_full_data <- merge(no_outliers_mad, win_ratio_no_out, by="Team") %>% 
  arrange(desc(no_out_mad))
new_full_data

#This merged table will help find what how removing outliers from assist differential will change mean assist differentials (percent change). 
see_diff <- merge(new_full_data, full_data, by="Team") %>%
  mutate(per_change = (no_out_mad - team_mean_assist_diff)/team_mean_assist_diff)
see_diff

#This is the average change in mean assist differential after outliers were taken out. Multiplying by 100 will give percent change. 
mean(see_diff$per_change)

#This is a simple regression of non-outlier data from the tibble new_full_data.
regression_2 <- lm(win_percent ~ no_out_mad, data=new_full_data)
summary(regression_2)

#This is the scatterplot of data with outliers omitted from the raw assist differential data.
plot_2 <- new_full_data %>% ggplot(aes(x=no_out_mad, y=win_percent)) +
  geom_point() + 
  geom_smooth(method=lm, color="red") +
  xlim(-4,7) +
  ylab("Win Ratio") + 
  xlab("Mean Assist Differential Per Game") + 
  ggtitle("Mean Assist Differential vs. Win Ratio (Adjusted: No Outliers)")
plot_2

#Correlation coefficient of plot_2
cor(as.vector(new_full_data$no_out_mad), as.vector(new_full_data$win_percent), method=c("pearson", "kendall", "spearman"))

#This is not discussed in the report, but I wanted to try writing a function for regression_2.
#This function takes mean assist differential as the input and returns expected win ratio as its output. 
predicted_win_ratio <- function(r) {
  win_percent <- r*0.04168 + 0.49072
  return(win_percent)
}

#Some random inputs to check if the function is predicting as per regression_2.
predicted_win_ratio(6.65)
predicted_win_ratio(0)
predicted_win_ratio(-3)

#Took a simple random sample of 5 NBA Teams. 
#Categorized the aggregated data into specific Teams. This will be helpful when examining data on an individual team level instead of the league basis.  
det <- nba_data[which(nba_data$Team=="DET"),]
uta <- nba_data[which(nba_data$Team=="UTA"),]
hou <- nba_data[which(nba_data$Team=="HOU"),]
nop <- nba_data[which(nba_data$Team=="NOP"),]
lal <- nba_data[which(nba_data$Team=="LAL"),]

#First team is DET. There are 4 different scenarios that can happen: if they get: 
#positive assist differential, and win the game
#positive assist differential, and lose the game
#negative assist different, and lose the game
#negative assist differential, and win the game
#The goal is to observe the percentage in which each scenario occurs and if there is convincing evidence that a relationship exists. 

positive_assist_diff_wins_det = det[which(det$assists_diff > 0 & det$win==1),]
positive_ad_wins_det <- nrow(positive_assist_diff_wins_det)/nrow(det)

positive_assist_diff_losses_det = det[which(det$assists_diff > 0 & det$win==0),]
positive_ad_losses_det <-nrow(positive_assist_diff_losses_det)/nrow(det)

negative_assist_diff_losses_det = det[which(det$assists_diff < 0 & det$win==0),]
negative_ad_losses_det <- nrow(negative_assist_diff_losses_det)/nrow(det)

negative_assist_diff_wins_det = det[which(det$assists_diff < 0 & det$win==1),]
negative_ad_wins_det <- nrow(negative_assist_diff_wins_det)/nrow(det)

det_data <- c(positive_ad_wins_det, positive_ad_losses_det, negative_ad_wins_det, negative_ad_losses_det)

#Learned how to plot pie charts from https://www.r-graph-gallery.com/131-pie-plot-with-r.html. 
#Author: Yan Holtz.
#Also learned how to plot pie charts from https://www.tutorialspoint.com/r/r_pie_charts.htm
#Author: Not Listed
colors <- brewer.pal(4, "Set1")
pie(det_data, 
    labels=
      c("Positive Assist Differential and Won Game", 
        "Positive Assist Differential and Lost Game", 
        "Negative Assist Differential and Won Game", 
        "Negative Assist Differential and Lost Game"), 
    col=colors, main = "Detroit Pistons: +/- Assist Differentials and Game Result")

#Same process with UTA dataframe
positive_assist_diff_wins_uta = uta[which(uta$assists_diff > 0 & uta$win==1),]
positive_ad_wins_uta <- nrow(positive_assist_diff_wins_uta)/nrow(uta)

positive_assist_diff_losses_uta = uta[which(uta$assists_diff > 0 & uta$win==0),]
positive_ad_losses_uta <-nrow(positive_assist_diff_losses_uta)/nrow(uta)

negative_assist_diff_losses_uta = uta[which(uta$assists_diff < 0 & uta$win==0),]
negative_ad_losses_uta <- nrow(negative_assist_diff_losses_uta)/nrow(uta)

negative_assist_diff_wins_uta = uta[which(uta$assists_diff < 0 & uta$win==1),]
negative_ad_wins_uta <- nrow(negative_assist_diff_wins_uta)/nrow(uta)

uta_data <- c(positive_ad_wins_uta, positive_ad_losses_uta, negative_ad_wins_uta, negative_ad_losses_uta)

pie(uta_data, labels= 
      c("Positive Assist Differential and Won Game", 
        "Positive Assist Differential and Lost Game", 
        "Negative Assist Differential and Won Game", 
        "Negative Assist Differential and Lost Game"), 
    col=colors, main = "Utah Jazz: +/- Assist Differentials and Game Result")

#Same process with HOU datafrane
positive_assist_diff_wins_hou = hou[which(hou$assists_diff > 0 & hou$win==1),]
positive_ad_wins_hou <- nrow(positive_assist_diff_wins_hou)/nrow(hou)

positive_assist_diff_losses_hou = hou[which(hou$assists_diff > 0 & hou$win==0),]
positive_ad_losses_hou <-nrow(positive_assist_diff_losses_hou)/nrow(hou)

negative_assist_diff_losses_hou = hou[which(hou$assists_diff < 0 & hou$win==0),]
negative_ad_losses_hou <- nrow(negative_assist_diff_losses_hou)/nrow(hou)

negative_assist_diff_wins_hou = hou[which(hou$assists_diff < 0 & hou$win==1),]
negative_ad_wins_hou <- nrow(negative_assist_diff_wins_hou)/nrow(hou)

hou_data <- c(positive_ad_wins_hou, positive_ad_losses_hou, negative_ad_wins_hou, negative_ad_losses_hou)

pie(hou_data, labels= 
      c("Positive Assist Differential and Won Game", 
        "Positive Assist Differential and Lost Game", 
        "Negative Assist Differential and Won Game", 
        "Negative Assist Differential and Lost Game"), 
    col=colors, main = "Houston Rockets: +/- Assist Differentials and Game Result")

#Same process with NOP.
positive_assist_diff_wins_nop = nop[which(nop$assists_diff > 0 & nop$win==1),]
positive_ad_wins_nop <- nrow(positive_assist_diff_wins_nop)/nrow(nop)

positive_assist_diff_losses_nop = nop[which(nop$assists_diff > 0 & nop$win==0),]
positive_ad_losses_nop <-nrow(positive_assist_diff_losses_nop)/nrow(nop)

negative_assist_diff_losses_nop = nop[which(nop$assists_diff < 0 & nop$win==0),]
negative_ad_losses_nop <- nrow(negative_assist_diff_losses_nop)/nrow(nop)

negative_assist_diff_wins_nop = nop[which(nop$assists_diff < 0 & nop$win==1),]
negative_ad_wins_nop <- nrow(negative_assist_diff_wins_nop)/nrow(nop)

nop_data <- c(positive_ad_wins_nop, positive_ad_losses_nop, negative_ad_wins_nop, negative_ad_losses_nop)

pie(nop_data, labels= 
      c("Positive Assist Differential and Won Game", 
        "Positive Assist Differential and Lost Game", 
        "Negative Assist Differential and Won Game", 
        "Negative Assist Differential and Lost Game"), 
    col=colors, main = "New Orleans Pelicans: +/- Assist Differentials and Game Result")

#Same Process with LAL.
positive_assist_diff_wins_lal = lal[which(lal$assists_diff > 0 & lal$win==1),]
positive_ad_wins_lal <- nrow(positive_assist_diff_wins_lal)/nrow(lal)

positive_assist_diff_losses_lal = lal[which(lal$assists_diff > 0 & lal$win==0),]
positive_ad_losses_lal <-nrow(positive_assist_diff_losses_lal)/nrow(lal)

negative_assist_diff_losses_lal = lal[which(lal$assists_diff < 0 & lal$win==0),]
negative_ad_losses_lal <- nrow(negative_assist_diff_losses_lal)/nrow(lal)

negative_assist_diff_wins_lal = lal[which(lal$assists_diff < 0 & lal$win==1),]
negative_ad_wins_lal <- nrow(negative_assist_diff_wins_lal)/nrow(lal)

lal_data <- c(positive_ad_wins_lal, positive_ad_losses_lal, negative_ad_wins_lal, negative_ad_losses_lal)

pie(lal_data, labels= 
      c("Positive Assist Differential and Won Game", 
        "Positive Assist Differential and Lost Game", 
        "Negative Assist Differential and Won Game", 
        "Negative Assist Differential and Lost Game"), 
    col=colors, main = "Los Angeles Lakers: +/- Assist Differentials and Game Result")

#End of Code

