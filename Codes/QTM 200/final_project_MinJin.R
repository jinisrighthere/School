#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("tidyverse","knitr","kableExtra","ggpubr","Hmisc","car","ResourceSelection","lmtest"),  pkgTest)

# set working directory
setwd("~/school/Emory/3rd Year S/QTM200/Final_project")

# import data
game <- read.csv("games.csv")

################################################################################################
################################################################################################

# create a new table containing response and predicting variables
game_var <- game%>%
  select(HOME_TEAM_WINS, FT_PCT_home, AST_home, REB_away)
# remove all NA observations
game_var <- game_var%>%
  filter(!is.na(HOME_TEAM_WINS),!is.na(FT_PCT_home), !is.na(AST_home),!is.na(REB_away))
# transform free throw percentage from real values to percentage for better estimation result in later parts
game_var <- game_var%>%
  mutate(FT_PCT_home = FT_PCT_home * 100)

# summary statistics and distribution
summary_table <- data.frame(
  Variables = c("Home team free throw percentage", "Home team assists","Away team rebounds"),
  Min = c(min(game_var$FT_PCT_home), min(game_var$AST_home), min(game_var$REB_away)),
  Median = c(median(game_var$FT_PCT_home), median(game_var$AST_home), median(game_var$REB_away)),
  Mean = c(mean(game_var$FT_PCT_home),mean(game_var$AST_home), mean(game_var$REB_away)),
  Max = c(max(game_var$FT_PCT_home),max(game_var$AST_home), max(game_var$REB_away)),
  Sd = c(sd(game_var$FT_PCT_home),sd(game_var$AST_home), sd(game_var$REB_away))
)

kable(summary_table,digits = 3,booktabs = T, caption = "Summary statistics of independent variables, n = 24096")%>% 
  kable_styling(latex_options = c("hold_position", "striped"))

# distribution graphs for independent variables
ft_distribution <- game_var%>%
  ggplot(aes(x = FT_PCT_home)) + 
  geom_histogram() + 
  labs(title = "Distribution of Home Team Free Throw Percentage", x = "Free Throw Percentage", y = "Frequency")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5,size = 8))
ast_distribution <- game_var%>%
  ggplot(aes(x = AST_home)) + 
  geom_histogram() + 
  labs(title = "Distribution of Home Team Assist", x = "Home Team Assist", y = "Frequency")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5,size = 10))
reb_distribution <- game_var%>%
  ggplot(aes(x = REB_away)) + 
  geom_histogram() + 
  labs(title = "Distribution of Away Team Rebound", x = "Away Team Rebound", y = "Frequency")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5,size = 10))

# distribution graph for the response variable
win_distribution <- game_var%>%
  ggplot(aes(x = HOME_TEAM_WINS)) + 
  geom_bar() + 
  labs(title = "Distribution of Home Team Game Outcome\n 0 = lose, 1 = win", x = "Game Outcome", y = "Frequency")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5,size = 8))+
  scale_x_continuous(breaks = round(seq(min(game_var$HOME_TEAM_WINS), max(game_var$HOME_TEAM_WINS), by = 1),1))

ggarrange(win_distribution,ft_distribution, ast_distribution, reb_distribution,ncol = 2, nrow = 2)

# use correlation table to display correlation among variables
game_var%>%
  select(FT_PCT_home, AST_home, REB_away)%>%
  cor()%>%
  kable(caption = "Corrlation table for the independent variables, n = 24096")%>%
  kable_styling(latex_options = c("hold_position", "striped"))

# construct model1
model1_add <- glm(HOME_TEAM_WINS ~ FT_PCT_home ,data = game_var, family = binomial(link = "logit"))

# check the summary
summary(model1_add)

# calculate exp(beta1) and intercept
exp(0.018465)

#intercept 
exp(-1.025)

# construct model2
model2_add <- glm(HOME_TEAM_WINS ~ FT_PCT_home + AST_home,data = game_var, family = binomial(link = "logit"))

# check the summary
summary(model2_add)


# calculate odds for estimates 
#free throw
exp(0.019570)

#assist 
exp(0.135336)

#intercept 
exp(-4.122167)


# construct model3
model3_add <- glm(HOME_TEAM_WINS ~ FT_PCT_home + AST_home + REB_away,data = game_var, family = binomial(link = "logit"))

# check the summary
summary(model3_add)

# calculate odds for estimates 
#free throw
exp(0.014945)

#assist 
exp(0.132118)

# away team rebounds
exp(-0.077012)

#intercept 
exp(-0.445396)

# interactive term: AST_home * REB_away,data 
model3_co1 <- glm(HOME_TEAM_WINS ~ FT_PCT_home + AST_home + REB_away + AST_home * REB_away,data = game_var, family = binomial(link = "logit"))
summary(model3_co1)

# interactive term: FT_PCT_home * AST_home
model3_co2 <- glm(HOME_TEAM_WINS ~ FT_PCT_home + AST_home + REB_away + FT_PCT_home * AST_home,data = game_var, family = binomial(link = "logit"))
summary(model3_co2)

# interactive term: FT_PCT_home * REB_away
model3_co3 <- glm(HOME_TEAM_WINS ~ AST_home + FT_PCT_home + REB_away + FT_PCT_home * REB_away,data = game_var, family = binomial(link = "logit"))
summary(model3_co3)

# conduct chi-square test and Likelihood ratio test for model fit 
anova(model3_add, model3_co1, test = "Chisq")
lrtest(model3_add, model3_co1)

# conduct hoslem test for model fit
hoslem.test(game_var$HOME_TEAM_WINS, fitted(model3_co1))

# check correlation among variables
vif(model3_co1)

# Plot Residual vs. Fitted, Normal QQ plot, Residual vs. Leverage, and Scale-Location plot  
par(mfrow=c(2,2))
plot(model3_co1)

# create bubble plot to check outliers
dev.new(width=5, height=4)
influencePlot(model3_co1)
