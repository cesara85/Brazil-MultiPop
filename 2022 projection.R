
### EDUCATION STRUCTURE AND ATTAINMENT LEVELS. 

## BASE YEAR - 2022
# Estimation of the proportion value for 2022 from projection.
finalv2<-final %>%
  select(region, Time, sex, edu,agest,pop) %>% 
  filter(agest!=-5&(Time==2020|Time==2025)) %>%
  group_by(region,Time,sex,agest) %>% 
  mutate(sumpop=sum(pop),
         prop=pop/sumpop) %>% select(region, Time, sex, edu,agest,prop) %>% 
  pivot_wider(names_from = Time, values_from = prop) %>% 
  mutate(`2022` = 0.6*`2020`+0.4*`2025`) %>%  
  pivot_longer(cols = `2022`, names_to = "year", values_to ="prop")

# checks
setDT(finalv2)
finalv2[,by=.(agest,sex,region),sum(prop)] %>% spread(agest,V1)

# 2022 IBGE population
CD2022 <- read_excel("../data/education/CD2022.xlsx")
CD2022 %<>% 
  pivot_longer(cols = c(`0`:`90`), names_to = "age", values_to ="pop") %>% 
  rename(region=area,agest=age) %>% mutate(region=as.character(region),agest=as.numeric(agest))

# CALCULATING POPULATION WITH ED. ATTAINMENT
pop2022<- 
  full_join(finalv2,CD2022) %>% 
  mutate(popedu=prop*pop,
         edu=case_when(agest%in%0:10~"e1",TRUE~edu))
setDT(pop2022)
write_csv(pop2022, "../data/population/brazil_pop_2022.csv")


# FINAL CHECKS
pop2022[,sum(popedu),by=.(edu,agest)] %>% spread(edu,V1)
#check age structure
pop2022 %>% 
  mutate(edu=case_when(agest%in%0:10~"Under 15",
                       edu=="e1"~"No Education",
                       edu=="e2"~"Incomplete Primary",
                       edu=="e3"~"Primary",
                       edu=="e4"~"Lower Secondary",
                       edu=="e5"~"Upper Secondary",
                       edu=="e6"~"Post Secondary"),
         pop_pyramid = ifelse(sex == "m",-popedu/1000, popedu/1000)) %>% 
  ggplot(mapping = aes(x=pop_pyramid, y=as.factor(agest), 
                       fill=fct_relevel(edu,"Post Secondary","Upper Secondary","Lower Secondary", "Primary", "Incomplete Primary","No Education","Under 15")))+
  geom_col()+geom_vline(xintercept=0)+scale_fill_manual(values = wic_col6)+scale_x_continuous(labels = abs)+
  labs (x = "População (em milhares)",y = "Grupo etário",fill = "Nível educacional") +
  theme(legend.title = element_text(size=11, face = "bold", hjust = 0.5), legend.text = element_text(size=10), legend.position = "bottom",legend.direction = "horizontal",
        axis.text.y = element_text(size=11),axis.title.y = element_text(size=11, margin = margin(r=5)),
        axis.title.x = element_text(size=11),axis.text.x = element_text(size=11),
        strip.text = element_text(face = "bold", size = 11),
        panel.grid.major = element_blank())+
  guides(fill = guide_legend(title.position = "top", label.hjust = 0))

## projected values

education2010_65 <- read.csv("C:/Cesar/Bolsa Produtividade/Projecoes/gitbrazil/data/education/education2010_65.csv")
education2010_65 %<>% 
  pivot_wider(names_from = year, values_from = prjpropfinal) %>% 
  mutate(`2022` = 0.6*`2020`+0.4*`2025`,
         `2027` = 0.6*`2025`+0.4*`2030`,
         `2032` = 0.6*`2030`+0.4*`2035`,
         `2037` = 0.6*`2035`+0.4*`2040`,
         `2042` = 0.6*`2040`+0.4*`2045`,
         `2047` = 0.6*`2045`+0.4*`2050`,
         `2052` = 0.6*`2050`+0.4*`2055`,
         `2057` = 0.6*`2055`+0.4*`2060`,
         `2062` = 0.6*`2060`+0.4*`2065`) %>% 
  pivot_longer(cols = c(`2022`:`2062`), names_to = "year", values_to ="prop")
education2010_65 %<>%
  select(scenario,age,sex,education,area,state,region,year,prop)
write_csv(education2010_65, "data/education/education2010_62.csv")

#checks
setDT(education2010_65)
education2010_65[,by=.(scenario,age,sex,area,year),sum(prop)] %>% spread(year,V1)

education2010_65 %>% filter(year==2062,sex=="Female") %>% 
  ggplot(mapping = aes(x= prop,y = reorder(state, prop, max),
                       fill= education))+
  geom_col()+
  facet_grid(~scenario~age)+
  labs(title = '2060 Educational attaiment by state, age and SSP scenarios',
       subtitle = "Females, age 15 to 34",y = 'State',x = '%',
       fill = "Educational Attainment")+
  scale_fill_manual(values =c("#B2182B","#EF8A62","#FDDBC7","#D1E5F0","#67A9CF","#2166AC"))+
  theme_bw()+
  theme(title = element_text (size=10),
        axis.text.y = element_text (size=9),
        legend.title = element_text(size=10),legend.position = "bottom",
        panel.grid.major.y = element_line(linetype = "dashed"))+
  guides(fill = guide_legend(title.vjust = 0.5, nrow = 1))


