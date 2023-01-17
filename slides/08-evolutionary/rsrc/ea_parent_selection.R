library(ggplot2)
library(dplyr)

y = round(runif(10, 0, 10), 2)

df = data.frame(probability = 1/y, fitness = y)

df <- df %>% 
  arrange(desc(fitness)) %>%
  mutate(prop = probability / sum(df$probability) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

p = ggplot(df, aes(x="", y=probability, fill=fitness)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + theme_void() + 
  geom_text(aes(label = fitness), color = "white",
            position = position_stack(vjust = 0.5))

ggsave("figure_man/ea_parent_selection.pdf", p, width = 5, height = 5)