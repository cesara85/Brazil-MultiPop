##packages
pacman::p_load(tidyverse,devtools,stringr,MSDem,knitr,kableExtra,viridis,wcde,magrittr,readxl)
rm(list=ls())
setwd("C:/Cesar/Bolsa Produtividade/Projecoes/gitbrazil/Brazil-Multistate-projections/")

# Data estimated using Census for 2000 and 2010. Excel used to calculate TFR and make corrections for e1 to e3 groups.
# 2011-2020 used health registries. Trajectories estimated accordingly to states experiences.

##1 BASIC TFR
# TFR for 2000 to 2020
fertility2000_2020 <- read.csv2("C:/Cesar/Bolsa Produtividade/Dados/Fecundidade/fertility2000_2020.csv")
write_csv(fertility2000_2020,"data/fertility/TFR2000_2020.csv")
#registry data only
sinasc <- fertility2000_2020  %>% 
  filter(source =="SINASC"&Name!="Brazil") %>% select(State,year, source, tfr) %>% 
  rename(area = State)%>% mutate(area = as.character(area)) %>% 
  pivot_wider(names_from = year, values_from = tfr)

#basic plots
{
Brazil2000_20<-filter(fertility2000_2020, State=="0") # Brazil trajectory
ggplot(data=Brazil2000_20)+
  geom_point(aes(x = year, 
                 y = tfr,
                 color = source
  ),
  size = 4)+
  scale_color_manual(values=c("midnightblue","orange"))+
  labs(title = 'Adjusted TFR (Census) and registry-based TFR(SINASC), 2000-2020',
       y = 'TFR',
       x = 'year',
       color = "Data Source")+
  theme_get()+
  theme(axis.text.x = element_text(angle=90, hjust=1))

# States TFR - Census SINASC by SSp group
Brazil2000_20<-filter(fertility2000_2020, State!="0" & source =="SINASC")
Brazil2000_20$year<-as.numeric(Brazil2000_20$year)
ggplot(data=Brazil2000_20, aes(x = year, 
                               y = tfr,
                               color = Name))+
  geom_line(linewidth=1.5)+
  facet_wrap(~ssp2)+
  labs(title = 'Registry-based TFR(SINASC), states, 2010-2020',
       y = 'TFR',
       color = "States")+
  scale_x_continuous(name="Year",breaks= c(2010,2012,2014,2016,2018,2020))+
  theme_get()+
  theme(axis.text.x = element_text(angle=0, hjust=1),
        legend.title.align = 0.4,
        legend.position = c(0.85,0.225))

# Map of States TFR in 2020 - SINASC
pacman::p_load(sf,geobr,RColorBrewer)
brazil_states<-read_state(code_state="all", year=2010)
ggplot(brazil_states)+geom_sf()
Brazil2000_20<-filter(fertility2000_2020, year == "2020" & State !="0")
map_fert<-Brazil2000_20 %>% full_join (brazil_states, by = c('State'='code_state'), multiple = "all")
map_fert %>%
  round("TFR", digts="2")
ggplot(map_fert)+
  geom_sf(aes(geometry = geom,
              fill=tfr),
          color="lightgrey")+
  scale_fill_viridis(option= "magma", direction=-1, name = "Total Fertility Rate",
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5))+
  geom_sf_label(aes(geometry = geom,
                    label = tfr),
                size = 3,
                alpha = 0.5)+
  labs(title = 'Registry-based TFR,  2020')+
  theme_get()+
  theme(title = element_text (size=9),
        legend.title = element_text(size=9),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom")

##maps of ssp - option2
ggplot(map_fert)+
  geom_sf(aes(geometry = geom,
              fill=fct_reorder(ssp2, -tfr)),
          color="lightgrey")+
  scale_fill_manual(
    values=magma(5),
    name = "Fertility groups",
    drop = FALSE,
    guide = guide_legend(
      keyheight = unit(4, units = "mm"),
      keywidth = unit(25 / length(labels), units = "mm"),
      title.position = 'top',
      title.hjust = 0.2,
      label.hjust = 1,
      nrow = 5,
      byrow = T,
      reverse = F))+
  labs(title = 'Brazilian States SSP group by TFR Level')+
  theme_get()+
  theme(title = element_text (size=12),
        legend.title = element_text(size=11),
        legend.text = element_text(size=10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
}

#2 ASFR
asfr2000_2010 <- read_excel("C:/Cesar/Bolsa Produtividade/Dados/Fecundidade/adj_asfr_2000_2010.xlsx")
asfr2000_2010 %<>% pivot_longer(cols = c(rep_asfr,adj_asfr), values_to = "asfr", names_to = "variable") %>% 
  mutate(variable = case_when(variable =="rep_asfr"~"Reported",
                              variable =="adj_asfr"~"Adjusted")) %>% 
  rename(age = Idade)

#basic plots
{
  # Brazil trajectory
asfr2000_2010 %>% 
  filter(area=="Brazil"& edu!="all" & variable=="Adjusted") %>% 
ggplot(mapping = aes(x = age, 
                 y = asfr,
                 color = edu))+
  geom_point(size=3)+
  geom_line(aes(group = edu), linewidth =1.2)+
  facet_wrap(~year)+
  scale_color_manual(values =c("#B2182B","#EF8A62","#FDDBC7","#D1E5F0","#67A9CF","#2166AC"))+
  labs(y = 'Age specific fertility ratio',
       x = 'Age group',
       color = "Educational Attainment")+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, hjust=0.5),axis.text.y = element_text(size=12, hjust=1),
        legend.text = element_text(size=12), strip.text = element_text(size=12,face="bold"))
ggsave("plots/ASFRBrazil.jpg", width = 25, height = 20, units = "cm")

# states trajectory by edu
asfr2000_2010 %>% 
  filter(area!="Brazil"& edu!="all" & variable=="Adjusted") %>% 
  ggplot(mapping = aes(x = age, 
                       y = asfr,
                       color = area))+
  geom_point(size=.7)+
  geom_line(aes(group = area), linewidth =.3)+
  facet_grid(~year~edu)+
  labs(y = 'Age specific fertility ratio',
       x = 'Age group',
       color = "Educational Attainment")+
  theme_bw() +
  theme(axis.text.x = element_text(size=12, hjust=0.5, angle = 90),axis.text.y = element_text(size=12, hjust=1),
        legend.text = element_text(size=12), strip.text = element_text(size=12,face="bold"))
ggsave("plots/ASFRstates.jpg", width = 30, height = 20, units = "cm")
  }

#3 adding SSPs groups and estimating future trajectories. Variable for TFR by edu att. Set e6 at 15-19 to 0.
fertility2000_2020 %<>% 
  rename(area = State) %>% 
  mutate(area = as.character(area))%>%
  mutate (area = case_when(area=="0"~"Brazil", TRUE~area))
asfrprojections <- full_join(asfr2000_2010, fertility2000_2020, multiple = "all") %>%
  filter(area!="Brazil"& year!=2000 & variable=="Adjusted"&source=="Census") %>% 
  select(region, area, Name, age, edu,asfr,tfr,ssp2) %>% 
  group_by(area, edu) %>% 
  mutate(tfredu=sum(asfr)*5) %>% 
  ungroup() %>% 
  mutate(asfr = case_when(age=="15-19"&edu=="e6"~0, TRUE~asfr))

#2010-15 and 2015-2020 asfr and TFR
asfrprojections <- full_join(asfrprojections, sinasc, multiple = "all") %>% rename(y2010 = '2010', y2015='2015',y2020='2020') %>% 
  mutate(p2010 = case_when(ssp2=="Medium - declining"~(asfr*y2010/tfr), #ASFR = 2010 Census ASFR multplied by the relation between SINASC value on 2010(y2010) and Census in 2010(tfr)
                              ssp2=="High - declining"~(asfr*y2010/tfr),
                              ssp2=="High - stable"~asfr,
                              ssp2=="Medium - stable"~(asfr*y2010/tfr),
                              ssp2=="Low"~(asfr*y2010/tfr)),
         p2015 = case_when(ssp2=="Medium - declining"~(asfr*y2015/tfr),
                              ssp2=="High - declining"~(asfr*y2015/tfr),
                              ssp2=="High - stable"~asfr,
                              ssp2=="Medium - stable"~(asfr*y2015/tfr),
                              ssp2=="Low"~(asfr*y2015/tfr)),
         p2020 = case_when(ssp2=="Medium - declining"~(asfr*y2020/tfr),
                           ssp2=="High - declining"~(asfr*y2020/tfr),
                           ssp2=="High - stable"~asfr,
                           ssp2=="Medium - stable"~(asfr*y2020/tfr),
                           ssp2=="Low"~(asfr*y2020/tfr)),
         p2025 = p2020,
         p2030 = p2020,
         p2035 = p2020,
         p2040 = p2020,
         p2045 = p2020,
         p2050 = p2020,
         p2055 = p2020)

asfrprojections %<>% #tfr estimation by edu att
  group_by(area, edu) %>% 
  mutate(tfr2010=sum(p2010)*5,
         tfr2015=sum(p2015)*5,
         tfr2020=sum(p2020)*5,
         tfr2025=sum(p2025)*5,
         tfr2030=sum(p2030)*5,
         tfr2035=sum(p2035)*5,
         tfr2040=sum(p2040)*5,
         tfr2045=sum(p2045)*5,
         tfr2050=sum(p2050)*5,
         tfr2055=sum(p2055)*5)%>%  
  ungroup()
#editing final files - for asfr and for tfr
tfrprojections <- asfrprojections %>% 
  filter(age=="15-19") %>% 
  select(region, area,edu,ssp2,tfr2010:tfr2055) %>% 
  pivot_longer(cols = tfr2010:tfr2055, names_to = "year", values_to = "tfr") %>% 
  separate(year, into=c("var","period"),sep = "r") %>% select(-"var")
#write_csv(tfrprojections,"data/fertility/TFR2010_2060.csv")

asfrprojections <- asfrprojections %>% 
  select(region,area,age,edu,ssp2,p2010:p2055) %>% 
  pivot_longer(cols = p2010:p2055, names_to = "year", values_to = "asfr") %>% 
  separate(year, into=c("var","period"),sep = "p") %>% select(-"var")

#write_csv(asfrprojections,"data/fertility/ASFR2010_2060.csv")

#ploting result
#facets result
asfrprojected <- asfrprojections %>% 
  pivot_longer(cols = c(tfr,tfredu, y2010:tfr2015), names_to = "year", values_to = "value")
asfrprojected%>% 
  filter(year=="tfredu"|year=="tfr2010"|year=="tfr2015", age=="15-19") %>% 
  ggplot(mapping = aes(x = fct_relevel(year, c("tfredu","tfr2010","tfr2015")), 
                         y = value,
                         color = area))+
      geom_point(size=2)+
      geom_line(aes(group = area), linewidth =1)+
      facet_grid(~edu~ssp2)+
      labs(y = 'Total fertility rate',
           x = 'Year',
           color = "Area")+
      theme_bw() +
      theme(axis.text.x = element_text(size=12, hjust=0.5),axis.text.y = element_text(size=12, hjust=1),
            legend.text = element_text(size=12), strip.text = element_text(size=12,face="bold"))
#ggsave("plots/TFR2010_20projection.jpg", width = 30, height = 30, units = "cm")

#one graph
asfrprojected <- asfrprojections %>% 
  pivot_longer(cols = c(tfr,tfredu, y2010:tfr2015), names_to = "year", values_to = "value")
asfrprojected%>% 
  filter(year=="tfredu"|year=="tfr2010"|year=="tfr2015", age=="15-19") %>% 
  ggplot(mapping = aes(x = fct_relevel(year, c("tfredu","tfr2010","tfr2015")), 
                       y = value,
                       color = ssp2))+
  geom_point(size=2)+geom_line(aes(group=area))+
  facet_wrap(~edu)+
  labs(y = 'Total fertility rate',
       x = 'Year',
       color = "Area")+
  theme_bw() +
  theme(axis.text.x = element_text(size=12, hjust=0.5),axis.text.y = element_text(size=12, hjust=1),
        legend.text = element_text(size=12), strip.text = element_text(size=12,face="bold"))

asfrprojected%>% 
  filter(year=="tfredu"|year=="tfr2010"|year=="tfr2015", age=="15-19") %>% 
  ggplot(mapping = aes(x = fct_relevel(year, c("tfredu","tfr2010","tfr2015")), 
                       y = value,
                       color = area))+
  geom_point(size=2)+
  geom_line(aes(group = area), linewidth =1)+
  facet_grid(~edu~ssp2)+
  labs(y = 'Total fertility rate',
       x = 'Year',
       color = "Area")+
  theme_bw() +
  theme(axis.text.x = element_text(size=12, hjust=0.5),axis.text.y = element_text(size=12, hjust=1),
        legend.text = element_text(size=12), strip.text = element_text(size=12,face="bold"))
#ggsave("plots/TFR2010_20projection.jpg", width = 30, height = 30, units = "cm")

#e6 group
asfrprojections %>% 
  filter(edu =="e6"&period ==2020) %>% 
  ggplot(mapping = aes(x = age, 
                       y = asfr,
                       color = area))+
  geom_point(size=2)+geom_line(aes(group=area))+
  facet_wrap(~ssp2)+
  labs(y = 'Total fertility rate',
       x = 'Year',
       color = "Area")+
  theme_bw() +
  theme(axis.text.x = element_text(size=12, hjust=0.5),axis.text.y = element_text(size=12, hjust=1),
        legend.text = element_text(size=12), strip.text = element_text(size=12,face="bold"))
