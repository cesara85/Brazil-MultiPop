## educational attainment using census - 1991 to 2010
rm(list = ls())
pacman::p_load(tidyverse,devtools,MSDem,knitr,kableExtra,viridis,reshape2,magrittr,wcde)
setwd("C:/Cesar/Bolsa Produtividade/Projecoes/gitbrazil/Brazil-Multistate-projections/")
options(digits = 2, scipen = 999)

##1991 census
census_1991 <- read.csv("C:/Cesar/Bolsa Produtividade/Dados/Censo - Leitura no R/1991/1991.csv", header=TRUE)
#age and edu att.
census_1991 %<>%
  rename(year = cem_harm_ano, age = cem_harm_idade, education=cem_harm_niveleducacao, weight = cem_harm_peso, sex=cem_harm_sexo,area=cem_harm_uf_2010) %>% 
  mutate(edu = cut(education, breaks = c(0, 1, 2, 4, 6, 8, 10),
                                                    labels = c("e1", "e2", "e3", "e4", "e5", "e6"),
                                                    include.lowest = T, right = T)) %>% 
  mutate(age_interval = cut(age, breaks = c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, 94, 99, 199),
                                        labels = c("0-4", "05-09", "10-14", "15-19", "20-24", "25-29","30-34", "35-39", "40-44", "45-49","50-54", "55-59","60-64", "65-69","70-74", "75-79","80-84", "85-89","90-94", "95-99","100+"),
                                        include.lowest = T, right = T))
#population by weight
brazil_pop_1991 <- census_1991 %>%
  group_by(area,year,age_interval,edu,sex) %>%
  count(area,wt=weight)
sum(brazil_pop_1991$n)#check if total population is ok
write_csv(brazil_pop_1991, "data/population/brazil_pop_1991.csv")

## 2000 census
census_2000 <- read.csv("C:/Cesar/Bolsa Produtividade/Dados/Censo - Leitura no R/2000/Censo 2000.csv", header = TRUE, sep=";")
#age and edu att.
census_2000 %<>%
  rename(year = cem_harm_ano, age = cem_harm_idade, education=cem_harm_niveleducacao, weight = cem_harm_peso, sex=cem_harm_sexo,area=cem_harm_uf_2010) %>% 
  mutate(edu = cut(education, breaks = c(0, 1, 2, 4, 6, 8, 10),
                   labels = c("e1", "e2", "e3", "e4", "e5", "e6"),
                   include.lowest = T, right = T)) %>% 
  mutate(age_interval = cut(age, breaks = c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, 94, 99, 199),
                            labels = c("0-4", "05-09", "10-14", "15-19", "20-24", "25-29","30-34", "35-39", "40-44", "45-49","50-54", "55-59","60-64", "65-69","70-74", "75-79","80-84", "85-89","90-94", "95-99","100+"),
                            include.lowest = T, right = T))
brazil_pop_2000 <- census_2000 %>%
  group_by(area,year,age_interval,edu,sex) %>%
  count(area,wt=weight)
sum(brazil_pop_2000$n)#check if total population is ok
write_csv(brazil_pop_2000, "data/population/brazil_pop_2000.csv")

## 2010 census
census_2010 <- read.csv("C:/Cesar/Bolsa Produtividade/Dados/Censo - Leitura no R/2010/Censo_2010.csv", header = TRUE, sep=",")
#age and edu att.
census_2010 %<>%
  rename(year = cem_harm_ano, age = cem_harm_idade, education=cem_harm_niveleducacao, weight = cem_harm_peso, sex=cem_harm_sexo,area=cem_harm_uf_2010) %>% 
  mutate(edu = cut(education, breaks = c(0, 1, 2, 4, 6, 8, 10),
                   labels = c("e1", "e2", "e3", "e4", "e5", "e6"),
                   include.lowest = T, right = T)) %>% 
  mutate(age_interval = cut(age, breaks = c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, 94, 99, 199),
                            labels = c("0-4", "05-09", "10-14", "15-19", "20-24", "25-29","30-34", "35-39", "40-44", "45-49","50-54", "55-59","60-64", "65-69","70-74", "75-79","80-84", "85-89","90-94", "95-99","100+"),
                            include.lowest = T, right = T))
brazil_pop_2010 <- census_2010 %>%
  group_by(area,year,age_interval,edu,sex) %>%
  count(area,wt=weight)
sum(brazil_pop_2010$n)#check if total population is ok
write_csv(brazil_pop_2010, "data/population/brazil_pop_2010.csv")

##1991 to 2010 files and analysis
pop1991_2010 <- bind_rows(brazil_pop_1991,brazil_pop_2000,brazil_pop_2010)
write_csv(pop1991_2010, "data/population/pop1991_2010.csv")

##################### basic file - start reading here ###################################
pop1991_2010 <- read.csv("data/population/pop1991_2010.csv", header = TRUE)
#states names and region
pop1991_2010 %<>% 
  mutate(state = case_when(area==11 ~"Rondônia",area==12 ~"Acre",area==13 ~"Amazonas",area==14 ~"Roraima",
                           area==15 ~"Pará",area==16 ~"Amapá",area==17 ~"Tocantins",area==21 ~"Maranhão",
                           area==22 ~"Piauí",area==23 ~"Ceará",area==24 ~"Rio Grande do Norte",area==25 ~"Paraíba",
                           area==26 ~"Pernambuco",area==27 ~"Alagoas",area==28 ~"Sergipe",area==29 ~"Bahia",area==31 ~"Minas Gerais",
                           area==32 ~"Espírito Santo",area==33 ~"Rio de Janeiro",area==35 ~"São Paulo",area==41 ~"Paraná",
                           area==42 ~"Santa Catarina",area==43 ~"Rio Grande do Sul",area==50 ~"Mato Grosso do Sul",area==51 ~"Mato Grosso",
                           area==52 ~"Goiás",area==53 ~"Distrito Federal"),
         region = case_when(area==11 ~"North", area==12 ~"North", area==13 ~"North", area==14 ~"North",
                            area==15 ~"North", area==16 ~"North",area==17 ~"North",
                            area==21 ~"Northeast", area==22 ~"Northeast",area==23 ~"Northeast", area==24 ~"Northeast",
                            area==25 ~"Northeast",area==26 ~"Northeast", area==27 ~"Northeast",area==28 ~"Northeast", area==29 ~"Northeast",
                            area==31 ~"Southeast",area==32 ~"Southeast",area==33 ~"Southeast",area==35 ~"Southeast",
                            area==41 ~"South",area==42 ~"South",area==43 ~"South",
                            area==50 ~"Center-West",area==51 ~"Center-West",area==52 ~"Center-West",area==53 ~"Center-West")) %>% 
  separate(age_interval, c("age","end"))

#percentual edu values by age and sex and change rate
pop1991_2010 %<>% 
  group_by(area, year, age, sex) %>% 
  mutate(pop=sum(n)) %>% 
  ungroup() %>% 
  mutate(propBR=n/pop) %>% 
  mutate(sex = case_when(sex==0~"Female",sex==1~"Male")) %>% 
  replace_na(list(edu="Under 5"))

#just age groups from 15 to 39 and calculating future trends
#wcde proportions and transformations to join with Brazil data
Brazil <- get_wcde(indicator = "prop", scenario = c(1,2,3), 
                   country_name = "Brazil", pop_edu = "six") %>% 
  filter(year%in%2010:2060, 
         sex!="Both", 
         age=="15--19"|age=="20--24"|age=="25--29"|age=="30--34",
         education !="Short Post Secondary"& education!="Bachelor"
         & education!="Master and higher") %>%  
  mutate(education = case_when(education=="No Education"~"e1",
                               education=="Incomplete Primary"~"e2",
                               education=="Primary"~"e3",
                               education=="Lower Secondary"~"e4",
                               education=="Upper Secondary"~"e5",
                               education=="Post Secondary"~"e6")) %>% na.omit() %>% 
  separate(age, c("age","end")) %>%
  select(-country_code, -end)# %>%
  pivot_wider(names_from = "year", values_from = "prop") 
  
#join wcde with brazil data
#eduproportions at 2010
eduproportions<-pop1991_2010 %>% 
  select(area,state,region,year, age, edu, sex,propBR) %>%
  mutate(propBR=propBR*100) %>% rename(education=edu) %>% 
  filter(age%in%15:30,year==2010)
#WIC 2010 estimates
Brazil2010 <- Brazil %>% 
  filter(year==2010) 
#join files, and estimate a coeficient of proportions (Brazil states and WIC values). When e6 at 15years, prop =0.
fulleducation<- full_join(Brazil2010,eduproportions, by=join_by(age, education,sex), multiple="all") %>% 
  mutate(ql = case_when(age=="15" & education=="e6"~0, TRUE~propBR/prop))
#join files and calculate for every year.
Brazil2060 <- Brazil
fulleducation<-full_join(fulleducation,Brazil2060,by=join_by(scenario,age, education,sex), multiple="all") %>% 
  mutate(prjprop = ql*prop.y) %>% 
  group_by(scenario,age,sex,state,year) %>% 
  mutate(prjproptot = sum(prjprop)) %>% 
  ungroup() %>% 
  mutate(prjpropfinal = 100*prjprop/prjproptot)%>% 
  select(scenario,age,sex,education,area,state,region,year,prjpropfinal)
write_csv(fulleducation, "data/education/education2010_60.csv")

############### plots ########################

##main results in 2 plots - sex
#male
fulleducation %>%
  filter(year==2060,sex=="Male") %>% 
  ggplot(mapping = aes(x= prjpropfinal,
                       y = reorder(state, prjpropfinal, max),
                              fill= education))+
  geom_col()+
  facet_grid(~scenario~age)+
  labs(title = '2060 Educational attaiment by state, age and SSP scenarios',
       subtitle = "Males, age 15 to 34",
       y = 'State',
       x = '%',
       fill = "Educational Attainment")+
  scale_fill_manual(values =c("#B2182B","#EF8A62","#FDDBC7","#D1E5F0","#67A9CF","#2166AC"))+
  theme_bw()+
  theme(title = element_text (size=10),
        axis.text.y = element_text (size=9),
        legend.title = element_text(size=10),legend.position = "bottom",
        panel.grid.major.y = element_line(linetype = "dashed"))+
  guides(fill = guide_legend(title.vjust = 0.5, nrow = 1))
ggsave("plots/2060males.jpg", width = 30, height = 25, units = "cm")


#female
fulleducation %>%
  filter(year==2055,sex=="Female") %>% 
  ggplot(mapping = aes(x= prjpropfinal,
                       y = reorder(state, prjpropfinal, max),
                       fill= education))+
  geom_col()+
  facet_grid(~scenario~age)+
  labs(title = '2060 Educational attaiment by state, age and SSP scenarios',
       subtitle = "Females, age 15 to 34",
       y = 'State',
       x = '%',
       fill = "Educational Attainment")+
  scale_fill_manual(values =c("#B2182B","#EF8A62","#FDDBC7","#D1E5F0","#67A9CF","#2166AC"))+
  theme_bw()+
  theme(title = element_text (size=10),
        axis.text.y = element_text (size=9),
        legend.title = element_text(size=10), legend.position = "bottom",
        panel.grid.major.y = element_line(linetype = "dashed"))+
  guides(fill = guide_legend(title.vjust = 0.5, nrow = 1))
ggsave("plots/2060females.jpg", width = 30, height = 25, units = "cm")

#e6 trajectory

fulleducation %>% filter(education =="e6", age!=15,year==2055) %>% 
  ggplot(mapping = aes(x= prjpropfinal,
                       y = reorder(state, prjpropfinal, max),
                       fill= education))+
  geom_col()+
  facet_grid(~sex~scenario~age)#+
    labs(title = 'E6 Population, by State, 1991-2010',
         y = 'State',
         x = '%',
         color = "Year")+
  theme(title = element_text (size=9),
        legend.title = element_text(size=9),
        panel.grid.major.y = element_line(linetype = "dashed"))+
    theme_bw()
ggsave("plots/2055females.jpg", width = 30, height = 25, units = "cm")
  
#wcde plot   
Brazil %>%
  filter(scenario==1) %>% 
  ggplot(mapping = aes(x= prop,
                       y = as.factor(year),
                       fill= education))+
  geom_col()+
  facet_grid(~sex~age)+
  labs(title = 'E6 Population, by State, 1991-2010',
       y = 'State',
       x = '%',
       color = "Year")+
  theme(title = element_text (size=9),
        legend.title = element_text(size=9),
        panel.grid.major.y = element_line(linetype = "dashed"))
  
######### some analysis and plots - 1991 to 2010. Brazil data only.
#reframe to edu values by states and sex
pop1991_2010a <- pop1991_2010 %>% 
  group_by(area,state, region,year,sex,edu) %>% 
  summarise(pop=sum(n)) %>% 
  group_by(area,year,sex) %>%
  mutate(poptotal=sum(pop)) %>% 
  ungroup %>% 
  mutate(proportion=pop/poptotal)

#proportions of e1 to e3 and e4 to 26
pop1991_2010a %>%
  mutate(edu2 = case_when((edu=="e1"|edu=="e2"|edu=="e3")~"e1_e3",
                          (edu=="e4"|edu=="e5"|edu=="e6")~"e4_e6")) %>%
  group_by(state,year,sex,edu2) %>%
  summarize(proportion2 =sum(proportion)) 


#plots e6. 
pop1991_2010a %>% 
  filter(edu=="e6") %>% 
  ggplot(mapping = aes(x= proportion,
                y = reorder(state, proportion, max),
                color = factor(year)))+
  geom_point(aes(color=factor(year)),
             size = 3)+
  geom_line(aes(group = state))+
  facet_wrap(~sex, dir ="v")+
  labs(title = 'E6 Population, by State, 1991-2010',
       y = 'State',
       x = '%',
       color = "Year")+
  theme(title = element_text (size=9),
        legend.title = element_text(size=9),
        panel.grid.major.y = element_line(linetype = "dashed"))
ggsave("plots/e6edu1991_2010.jpg", width = 30, height = 25, units = "cm")

#all edu levels grouped e1-e3 and e4-e6
pop1991_2010a %>%
  filter(edu!="Under 5") %>%
  mutate(edu2 = case_when((edu=="e1"|edu=="e2"|edu=="e3")~"E1 to E3",
                          (edu=="e4"|edu=="e5"|edu=="e6")~"E4 to E6")) %>%
  group_by(state,year,sex,edu2) %>%
  summarize(proportion2 =sum(proportion)) %>% 
  ggplot(mapping = aes(x= proportion2,
                       y = reorder(state, proportion2, max),
                       color = factor(year)))+
  geom_point(aes(color=factor(year)),
             size = 3)+
  geom_line(aes(group = state))+
  facet_grid(~sex~edu2)+
  labs(title = 'Population by educational attainment, by State, 1991-2010',
       y = 'State',
       x = '%',
       color = "Year")+
  theme_bw()+
  theme(title = element_text (size=9),
        legend.title = element_text(size=10),axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12),axis.title.x = element_text(size=12),
        strip.text = element_text(face = "bold", size = 12),
        panel.grid.major.y = element_line(linetype = "dashed"))
ggsave("plots/edugroups1991_2010.jpg", width = 30, height = 25, units = "cm")

##transitions 15 to 34 years
pop1991_2010b <- pop1991_2010 %>%
  filter(age_interval=="15-19"|age_interval=="20-24"|age_interval=="25-29"|age_interval=="30-34") %>% 
  group_by(area,state,region,year,sex,edu) %>% 
  summarize(pop=sum(n)) %>% 
  group_by(area,state,region,year,sex) %>% 
  mutate(poptotal=sum(pop)) %>% 
  ungroup %>% 
  mutate(proportion=pop/poptotal)

#transitions of population from 15 to 34
pop1991_2010b %>%
  ggplot(mapping = aes(x= proportion,
                       y = reorder(state, proportion, max),
                       color = factor(year)))+
  geom_point(aes(color=factor(year)),
             size = 3)+
  geom_line(aes(group = state))+
  facet_grid(~sex~edu)+
  labs(title = 'Population from 15 to 34 years by educational attainment, by State, 1991-2010',
       y = 'State',
       x = '%',
       color = "Year")+
  theme_bw()+
  theme(title = element_text (size=9),
        legend.title = element_text(size=10),axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12),axis.title.x = element_text(size=12),
        strip.text = element_text(face = "bold", size = 12),
        panel.grid.major.y = element_line(linetype = "dashed"))
ggsave("plots/edugroups15_34_1991_2010.jpg", width = 30, height = 25, units = "cm")

#choosing specific age groups
##transitions 35 to 39 years
pop1991_2010_25 <- pop1991_2010 %>%
  filter(age>=25) %>% 
  group_by(area,state,region,year,sex,edu) %>% 
  summarize(pop=sum(n)) %>% 
  group_by(area,state,region,year,sex) %>% 
  mutate(poptotal=sum(pop)) %>% 
  ungroup %>% 
  mutate(proportion=pop/poptotal)
pop1991_2010_25 %>% 
  ggplot(mapping = aes(x= proportion,
                       y = reorder(state, proportion, max),
                       color = factor(year)))+
  geom_point(aes(color=factor(year)),
             size = 3)+
  geom_line(aes(group = state))+
  facet_grid(~sex~edu)+
  labs(title = 'E6 Population with xx years and more by educational attainment, by State, 1991-2010',
       y = 'State',
       x = '%',
       color = "Year")+
  theme_bw()+
  theme(title = element_text (size=9),
        legend.title = element_text(size=10),axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=12),axis.title.x = element_text(size=12),
        strip.text = element_text(face = "bold", size = 12),
        panel.grid.major.y = element_line(linetype = "dashed"))


ggsave("plots/edugroups1991_2010.jpg", width = 30, height = 25, units = "cm")
