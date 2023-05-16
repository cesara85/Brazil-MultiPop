## Population in 2010 - comparison between census and WIC
getwd()
pacman::p_load(readxl,tidyverse, reshape2, wcde, ggforce, flextable, purrr)
rm(list = ls())
rm(wic_brazil2)

# WIC indicators - mortality
#dowloading and filtering data from 2010 only. Last age group = 90+
wic_brazil <- get_wcde(indicator = "pop", scenario = 2, country_name = "Brazil", 
                       pop_age = "all", pop_sex = "all", pop_edu = "six") %>% 
  filter(year == 2010, age !="All", sex!= "Both", education != "Total") %>% 
  pivot_wider(names_from = age, values_from = epop) %>% 
  mutate(`90+` = `90--94`+`95--99`+`100+`)%>% select(-c("90--94","95--99","100+")) %>% 
  pivot_longer(cols = `0--4`:`90+`, names_to = "age") %>% 
  group_by(age, sex)%>%
  mutate(poptotal_wic=sum(value))%>%
  mutate(poprel_wic=(value/poptotal_wic))

# IBGE population
### read 2010census file
census_2010 <- read.csv("data/population/Censo_2010.csv")
## age and education groups variables
census_2010$education <- cut(census_2010$cem_harm_niveleducacao, breaks = c(0, 1, 2, 4, 6, 8, 10),
                                         labels = c("No Education","Incomplete Primary","Primary","Lower Secondary","Upper Secondary", "Post Secondary"),
                                         include.lowest = T, right = T)
census_2010$age <- cut(census_2010$cem_harm_idade, breaks = c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, 199),
                               labels = c("0--4", "5--9", "10--14", "15--19", "20--24", "25--29","30--34", "35--39", "40--44", "45--49","50--54",
                                          "55--59","60--64", "65--69","70--74", "75--79","80--84", "85--89","90+"),
                               include.lowest = T, right = T)

##START POPULATION, BY AGE, SEX AND EDUCATIONAL ATTAINMENT - 2010
brazil_pop_2010 <- census_2010 %>%
  select(cem_harm_ano, cem_harm_peso, cem_harm_sexo, cem_harm_uf_2010, age, education) %>% 
  rename(weight = cem_harm_peso, sex = cem_harm_sexo, area = cem_harm_uf_2010, year =cem_harm_ano ) %>% 
  mutate (sex= case_when(sex==0 ~ "Female",sex==1 ~ "Male")) %>% 
  group_by(sex,age,education,area) %>% 
  summarise(pop=sum(weight))
#write data
#write_csv(brazil_pop_2010, "data/population/brazil_pop_2010.csv")

## changing names and adjusting to compare with WIC
census_2010b <- census_2010 %>%
  select(cem_harm_ano, cem_harm_peso, cem_harm_sexo, cem_harm_uf_2010, age, education) %>% 
  rename(weight = cem_harm_peso, sex = cem_harm_sexo, area = cem_harm_uf_2010, year =cem_harm_ano ) %>% 
  mutate (sex= case_when(sex==0 ~ "Female",sex==1 ~ "Male")) %>% 
  mutate(education = case_when(age=="0--4"|age=="5--9"|age=="10--14"~"Under 15", TRUE~education))

# agreggate table to compare. Calculate proportions of educ. att. by age and sex
ibge_brazil <- census_2010b %>%
  group_by(sex, age,education)%>%
  summarise(pop=sum(weight)) %>% 
  mutate(pop = pop/1000) %>% 
  group_by(age, sex)%>%
  mutate(poptotal_ibge=sum(pop))%>%
  mutate(poprel_ibge=(pop/poptotal_ibge))

#join IBGE and WIC files
pop2010 <- left_join(wic_brazil, ibge_brazil) %>% 
  rename(pop_wic = value, pop_ibge = pop)%>% 
  pivot_longer(cols = pop_wic:poprel_ibge, names_to = "variable")
pop2010$value <- pop2010$value %>% replace_na(0)

#comparing results
#basic plot
pop2010 %>%
  filter (variable=="pop_wic"|variable=="pop_ibge") %>%
  ggplot(mapping = aes(y= value, x = fct_inorder(age), color = variable))+
  geom_point()+
  facet_wrap(~fct_inorder(education))

#edu plot
pop2010 %>% 
  filter (variable=="poprel_wic"|variable=="poprel_ibge", education!="Under 15",
          age!="0--4",age!="5--9",age!="10--14")%>%
  mutate(variable = case_when(variable == "poprel_wic"~"WIC",variable== "poprel_ibge"~"IBGE")) %>% 
  ggplot(mapping = aes(y= value, x = age, fill = variable))+
  geom_col(position = "dodge") +
  facet_wrap(~fct_inorder(education))+
  labs(title = "Population by age and educational attainment (%)",
       y = "%",
       x = 'Age interval',
       fill = "Data Source")+
  theme_bw()+
  theme(title = element_text (size=12),
        legend.title = element_text(size=11),
        axis.text.x = element_text(size=11,angle=90, hjust=1),
        axis.text.y = element_text(size =11),
        strip.text = element_text(size=12))
ggsave("plots/pop_edu_comparison.jpg",width = 25, height = 25, units = "cm")


#basic table
library(knitr)
library(kableExtra)
poptable <- pop2010 %>% 
  filter (variable=="pop_ibge"|variable=="pop_wic")%>%
  group_by(education, variable) %>% 
  summarise(population = sum(value)) %>% 
  group_by(variable) %>% 
  mutate(prop=population/sum(population)) %>%  
  mutate(prop=round(prop*100, 2)) %>% 
  pivot_wider(names_from = variable, values_from = c(population,prop)) %>% 
  rename ("Educational Attainment" = education, "IBGE (abs.)" = population_pop_ibge, "WIC (abs.)" = population_pop_wic,"IBGE (%)" = prop_pop_ibge, "WIC (%)" = prop_pop_wic)
poptable %>%
  kable("html") %>%
  kable_styling(full_width = F)
