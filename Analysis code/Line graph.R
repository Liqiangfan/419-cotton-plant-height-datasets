library(tidyverse) 
library(scales) 

setwd("D:/line_graph")

df <- read.table("inputfile.csv", header = T, sep = ",", check.names = F)

head(df, 2)

profile_sig <- c(1, 2, 3, 4, 5)

df <- df %>%
    filter(Profile %in% profile_sig) %>%
    pivot_longer(cols = c("Time1", "Time2", "Time3", "Time4", "Time5"),
                 names_to = "Time",
                 values_to = "Value")

head(df, 2)

df %>%
    ggplot(aes(x = Time, y = Value, group = ID, color = ID)) +
    geom_line() +
    scale_x_discrete(limits = c("Time1", "Time2", "Time3", "Time4", "Time5")) +
    facet_wrap(~ Profile, nrow = 2) +
    theme(legend.position = "none")
	
df %>%
    ggplot(aes(x = Time, y = Value, group = ID, color = ID)) +
    geom_line() +
    scale_x_discrete(limits = c("Time1", "Time2", "Time3", "Time4", "Time5")) +
    facet_wrap(~ factor(Profile, levels = profile_sig), nrow = 2) +
    theme(legend.position = "none")

df2 <- df %>%
    group_by(Profile,Time) %>%
    summarise(Value = mean(Value))

df %>%
    ggplot(aes(x = Time, y = Value)) +
    geom_line(aes(group = ID), color = "red") +
    scale_x_discrete(limits = c("Time1", "Time2", "Time3", "Time4", "Time5")) +
    geom_line(data = df2,
              aes(x = Time, y = Value, group = 1), color = "black") +
    facet_wrap(~ factor(Profile, levels = profile_sig), nrow = 2) +
    theme(legend.position = "none")
