###==========================================###
###Laboratorio 9###
###Por: Maribel Vizárraga León
###FEcha: 30/11/2019
###=========================================###

coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")

mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")

waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

library(tidyverse)
library(knitr)
library(kableExtra)
library(ggplot2)
library(dyplr)
library(hrbrthemes)

install.packages("hrbrthemes", repos = "https://cinc.rud.is")

### Gráfico y tabla de Coast vs Waste


coast_vs_waste %>% 
  filter(Year == "2010")%>%
  sample_n(150, replace=T) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
                                      "hover",
                                      "condensed"),
                full_width = F,
                fixed_thead = T) %>%
  row_spec(0, color = "blue") %>% 
  scroll_box(height = "250px") 

coast_vs_waste$`Mismanaged plastic waste (tonnes)`



library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)




coast_vs_waste$`Total population (Gapminder)`


library(ggplot2)

options(scipen=999)  # turn-off scientific notation like 1e+48
data("midwest", package = "ggplot2")

coast_vs_waste$`Total population (Gapminder)`

ggplot(coast_vs_waste, aes(`Coastal population`, `Mismanaged plastic waste (tonnes)`)) +
  geom_point(color="steelblue",
             fill="#69b3a2",
             shape=21,
             alpha=0.5,
             size=6,
             stroke = 2) + 
  geom_smooth(method="lm", se=F) +
  labs(subtitle = "¿Las costas producen toda la basura?", 
       y = "Toneladas de plásticos", 
       x = "Población Costera", 
       title = "Relación entre mal manejo de plásticos y población costera", 
       caption = "Fuente: tidytuesday") +
  theme_bw()

coast_vs_waste$`Total population (Gapminder)`

ggplot(coast_vs_waste, aes(`Total population (Gapminder)`, `Mismanaged plastic waste (tonnes)`)) +
  geom_point(color="orange",
             fill="#69b3a2",
             shape=21,
             alpha=0.5,
             size=6,
             stroke = 2) + 
  geom_smooth(method="lm", se=F) +
  labs(subtitle = "Fueron los demás", 
       y = "Toneladas de plásticos", 
       x = "Población", 
       title = "Relación entre mal manejo de plásticos y población total", 
       caption = "Fuente: tidytuesday") +
  theme_bw()




ggplot(coast_vs_waste , aes(x=`Coastal population` , y= `Mismanaged plastic waste (tonnes)` ) ) +
  geom_hex(mapping = NULL, data = NULL, stat = "binhex",
           position = "identity", ..., na.rm = FALSE, show.legend = NA,
           inherit.aes = TRUE) +
  labs(title="Producción de desechos por cantidad de población costera", 
       subtitle=" Para 2010 ", 
       caption="Fuente: pmoracho.github.io", 
       y="Mismanaged plastic waste (tonnes)", 
       x="Coastal population") +
  theme_elegant()

coast_vs_waste$`Mismanaged plastic waste (tonnes)`


### Gráfico y tabla de Mismanaged vs gdp


mismanaged_vs_gdp %>% 
  filter(Year== "2010")%>%
  sample_n(220, replace=T) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
                                      "hover",
                                      "condensed"),
                full_width = F,
                fixed_thead = T) %>%
  row_spec(0, color = "green") %>% 
  scroll_box(height = "250px") 

ggplot (mismanaged_vs_gdp, aes(x= `Per capita mismanaged plastic waste (kilograms per person per day)`, y=`GDP per capita, PPP (constant 2011 international $) (Rate)`) ) +
  geom_hex(bins = 15) +
  scale_fill_continuous(type = "viridis") +
  labs(title="Relación producción de desperdicio vs PIB En el Mundo", 
       subtitle="Para el año 2010", 
       caption="Fuente: pmoracho.github.io", 
       y="PIB en dólares constantes para 2011", 
       x="Kiligramos/Persona/Día") +
  theme_bw()


## Tabla 2 del mismo tema porque no me salieron con la otra base

mismanaged_vs_gdp %>% 
  filter(Year== "2010")%>%
  sample_n(220, replace=T) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
                                      "hover",
                                      "condensed"),
                full_width = F,
                fixed_thead = T) %>%
  row_spec(0, color = "green") %>% 
  scroll_box(height = "250px") 


mismanaged_vs_gdp$`Total population (Gapminder)`
mismanaged_vs_gdp%>%
  filter(Year== 2010)%>%
  arrange(`Per capita mismanaged plastic waste (kilograms per person per day)`)%>%
  print()

ggplot(mismanaged_vs_gdp, aes(x=`Per capita mismanaged plastic waste (kilograms per person per day)`, y=`Total population (Gapminder)`)) +
  geom_point() + 
  geom_segment( aes(x=`Per capita mismanaged plastic waste (kilograms per person per day)`, xend=`Per capita mismanaged plastic waste (kilograms per person per day)`, y=0, yend=`Total population (Gapminder)`))+
  theme_bw()


### Gráfico y trabla del desperdicio contra PIB


waste_vs_gdp %>% 
  filter(Year== "2010")%>%
  sample_n(45, replace=T) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
                                      "hover",
                                      "condensed"),
                full_width = F,
                fixed_thead = T) %>%
  row_spec(0, color = "red") %>% 
  scroll_box(height = "250px") 

