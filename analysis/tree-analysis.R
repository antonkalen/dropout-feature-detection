
# Load packages -----------------------------------------------------------

library(readr)
library(dplyr)
library(here)
library(tidyr)
library(rpart)


# Load data ---------------------------------------------------------------

dropout_data <- read_csv2(here("data", "DO.csv"))


# Data prep ---------------------------------------------------------------
clean_data <- dropout_data %>% 
  transmute(
    dropout = factor(DO, 0:1, c("no dropout", "dropout")),
    age = Age,
    gender = factor(Gender, 1:2, c("male", "female")),
    other_sports = Other_Sport,
    injury_previous_season = factor(Injury_previous_season, 1:2, c("yes", "no")),
    socioeconomic = Socioeconomic,
    intrinsic = Inrinsic,
    ide = IDE,
    int = INT,
    ext = EXT,
    amot = AMOT,
    aut_supp = AutSupp
  ) %>% 
  drop_na()

# Make decision tree ------------------------------------------------------

model1 <- rpart(
  dropout ~ age + gender + other_sports + injury_previous_season + socioeconomic + intrinsic + ide + int + ext + amot + aut_supp,
  data = clean_data,
  method = "class"
)

plot(model1)
printcp(model1) 
summary(model1)





options(java.parameters = "-Xmx5000m")
set_bart_machine_num_cores(4)

tree <- bartMachine(
  X = clean_data[2:12],
  y = clean_data[[1]]
)

investigate_var_importance(tree, num_replicates_for_avg = 20)
summary(tree)
