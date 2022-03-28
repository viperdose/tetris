library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)

vaccines <- read_csv("phe_vaccines_age_london_boroughs.csv")
View(vaccines)

vaxdat = vaccines %>% 
  group_by(age_band,dose) %>% 
  filter(date == "2021-10-26") %>% 
  summarise(totalpop = sum(population), prop_doses = round(100*(sum(cum_doses)/sum(population)),digits = 1)) %>% 
  pivot_wider(id_cols = c(age_band,totalpop),names_from = dose,values_from = c("prop_doses")) %>% 
  clean_names() %>% 
  mutate(x1st_dose = x1st_dose - x2nd_dose, unvaxed = 100-(x1st_dose + x2nd_dose)) %>% 
  rename(Unvaccinated = unvaxed, "1st dose" = x1st_dose,  "2nd dose" = x2nd_dose) %>% 
  pivot_longer(!c(age_band,totalpop)) %>% 
  mutate(name = factor(name, levels=c("Unvaccinated","1st dose","2nd dose")),age_band = gsub(age_band,pattern = " |years",replacement = ""))

vaxdat1 = vaccines %>% filter(date == "2021-10-26") %>% group_by(age_band,dose) 
head(vaxdat1)

vaxdat2 = vaxdat1 %>% summarise(totalpop = sum(population), prop_doses = round(100*(sum(cum_doses)/sum(population)),digits = 1))
head(vaxdat2)

vaxdat3 = vaxdat2 %>% pivot_wider(id_cols = c(age_band,totalpop),names_from = dose,values_from = c("prop_doses")) %>% clean_names()
head(vaxdat3)

vaxdat4 = vaxdat3 %>% mutate(x1st_dose = x1st_dose - x2nd_dose, unvaxed = 100-(x1st_dose + x2nd_dose)) %>% rename(Unvaccinated = unvaxed, "1st Dose Only" = x1st_dose,  "1st and 2nd Dose" = x2nd_dose) 
head(vaxdat4)

vaxdat5 = vaxdat4 %>% pivot_longer(!c(age_band,totalpop)) %>% mutate(name = factor(name, levels=c("Unvaccinated","1st Dose Only","1st and 2nd Dose")),age_band = gsub(age_band,pattern = " |years",replacement = "")) 
head(vaxdat5)

plot = ggplot(data = vaxdat,aes(x=age_band,y=value,
                                fill=name,
                                width=totalpop)) +
  geom_bar(position="stack", 
           stat="identity",
           col="white") +
  facet_grid(~age_band,scales = "free_x", 
             space = 'free') +
  theme(panel.spacing = unit(0, "lines")) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.ticks.length.y.left = unit(1.25, "cm")) +
  theme(axis.ticks.length.y.right = unit(1.25, "cm")) +
  scale_y_continuous(labels=scales::percent_format(scale = 1),
                     sec.axis = dup_axis(name=NULL,labels = NULL)) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  theme(legend.direction = "horizontal",
        legend.spacing.x = unit(0.4, 'cm'),
        legend.key.size = unit(0.62, "cm"),
        legend.position = c(0.19,1.08),
        legend.text = element_text(size=12,color="#343C41")) +
  theme(plot.margin = margin(t = 1.3,r = 0.2,b = 0.25,l = 0.25,
                             unit = "cm")) +
  labs(title = "Percentage of the population by age band who have received COVID-19 Vaccinations", 
       subtitle = "Vaccinations administered to London residents up to 27 October 2021\nWidth of blocks is proportional to population size",
       caption = "Source: PHE COVID-19 Dashboard\nGraphic by George Seed (original by GLA City Intelligence)") +
  theme(plot.title = element_text(color = "#343C41", face="bold",
                                  size=18,vjust = 8), 
        plot.subtitle = element_text(color = "#343C41",
                                     size=14,vjust = 10)) +
  theme(axis.ticks = element_line(color = "#D4D4D4",size=0.5)) +
  theme(axis.text.y = element_text(size=12,color= "#343C41")) +
  theme(plot.title.position = "plot") +
  theme(plot.caption.position = "plot", plot.caption = element_text(hjust = 0,size=12.5,color = "#343C41",vjust=0)) +
  theme(axis.ticks.x = element_blank()) + 
  theme(axis.text.x = element_text(size=9,color= "#343C41")) +
  scale_fill_manual(values = c("#CCCCCC","#87A4C4","#4D657F"))
plot
