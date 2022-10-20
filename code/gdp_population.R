library(tidyverse)

gapminder_1997 <- read_csv("gapminder_1997.csv")
view(gapminder_1997)
Sys.Date() # outputs current date of system
sum(5, 6)

round(pi, 2)

getwd() # gets current working directory

# plotting #

ggplot(gapminder_1997, aes(x = gdpPercap, y = lifeExp, 
                           color = continent, size = pop/1000000)) + 
  labs(x = "GDP Per Capita", y = "Life Expectancy", 
       title = "Do people in wealthy countries live longer?", 
       size = "Population (in millions)", color = "Continent") +
  geom_point() +
  scale_color_brewer(palette = "Set2")
  
  # plotting for data exploration
gapminder_data <- read.csv("gapminder_data.csv")
view(gapminder_data)

ggplot(gapminder_data, aes(x = year, y = lifeExp, color = continent)) + 
  geom_point() + 
  labs(x = "Year", y = "Life Expectancy")

str(gapminder_data)

ggplot(gapminder_data, aes(x = year, y = lifeExp, 
                           color = continent, group = country)) + 
  geom_line() + 
  labs(x = "Year", y = "Life Expectancy")

ggplot(gapminder_data, aes(x = continent, y = lifeExp)) + 
  geom_boxplot() + 
  labs(x = "Continent", y = "Life Expectancy")

ggplot(gapminder_1997, aes(x = continent, y = lifeExp)) + 
  geom_violin(aes(fill = continent)) +
  geom_jitter(aes(alpha = 0.5)) +
  labs(x = "Continent", y = "Life Expectancy")

ggplot(gapminder_1997, aes(x = lifeExp)) +
  geom_density()

# ggplot2 themes

ggplot(gapminder_1997, aes(x = lifeExp)) +
  geom_histogram() +
  coord_flip() +
  theme_minimal()

ggplot(gapminder_1997, aes(x = lifeExp)) +
  geom_histogram() +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 45, vjust = 0.5, hjust = 1))

# facet

ggplot(gapminder_1997, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  facet_wrap(vars(continent))

ggplot(gapminder_1997, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  facet_grid(rows = vars(continent))

# saving plot

ggsave("awesome_plot.jpg", width = 6, height = 4)

violin_plot <- ggplot(gapminder_1997, aes(x = continent, y = lifeExp)) + 
  geom_violin(aes(fill = continent)) +
  geom_jitter(alpha = 0.5) +
  labs(x = "Continent", y = "Life Expectancy")

# does not change object just how it is shown in Rstudio
violin_plot + 
  theme_bw()

# changes object to use theme
Violin_plot <- violin_plot + theme_bw()

ggsave("awesome_violin_plot.jpg", violin_plot,
       width = 6, height = 4)


# animated plots

install.packages(c("gganimate", "gifski"))

library(gganimate)
library(gifski)

ggplot(gapminder_data, aes(x = log(gdpPercap), y = lifeExp, 
                           size = pop/1000000, color = continent)) +
  geom_point()


static_hansplot <- ggplot(gapminder_data, aes(x = log(gdpPercap), y = lifeExp, 
                                              size = pop/1000000, color = continent)) +
                   geom_point(alpha = 0.5) +
                   scale_color_brewer(palette = "Set1") +
                   labs(x = "GDP Per Capita", y = "Life Expectancy", 
                        color = "Continent", size = "Population (in millions)")
                   
animated_hansplot <- static_hansplot +
  transition_states(year, transition_length = 1, state_length = 1) +
  ggtitle("{closest_state}")

animated_hansplot

anim_save("hans_animateplot.gif", 
          plot = animated_hansplot, 
          renderer = gifski_renderer())          
