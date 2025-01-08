Sys.setlocale("LC_ALL", "en_US.UTF-8")

setwd("C:/Users/Casper/Desktop/group (1)/group/Data_Programming_Group_Project.git")

gert::git_pull()

library(tidyverse)
library(ggplot2)

surveys <- readRDS("C:/Users/Casper/Desktop/group (1)/group/Data_Programming_Group_Project.git/data/processed/surveys.rds")



