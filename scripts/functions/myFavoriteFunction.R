if(!require(dplyr)) install.packages(dplyr)
library(dplyr)
library(nycflights13)
library(ggplot2)
library(ggrepel)

flights %>% 
  group_by(carrier, dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL") %>%
  ggplot(mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_point(aes(color = carrier)) +
  geom_label_repel(aes(label=ifelse(delay > 25 | delay <= -5 ,as.character(carrier),'')))
  geom_smooth(se = FALSE)
