#library#
## Estimations of domestic migration using 2010 levels and existing projections assumptions to SSP2
getwd()
pacman::p_load(readxl,tidyverse, reshape2, wcde, ggforce, flextable, purrr, knitr, kableExtra,magrittr)
rm(list = ls())

options(scipen = 999)

##1. WIC values
wic_migration <- get_wcde(indicator = "net", scenario = c(1,2,3),
                          country_name = "Brazil", pop_sex = "both", pop_age = "all", pop_edu = "total",include_scenario_names = TRUE) %>%
  filter(sex=="Both", age =="All") %>% 
  select(scenario_name, period, net) 

#basic migration plot from WIC
wic_migration %>% 
  mutate(year = period) %>% 
  separate(period, c("in_year", "end_year")) %>% 
  filter(in_year >= 2010)%>% 
  ggplot(mapping = aes(
    x = year,
    y = net,
    color = fct_inorder(scenario_name),
    group=scenario_name))+
  geom_point(size=3)+geom_line(linewidth=1)+
  theme_bw()+
  labs(title = "International net migration (000's)",
       subtitle = "Brazil (2010-2100)",
       x = "Period",
       color = "Scenario",
       y = "Net migration(000´s)",
       caption = "WIC(2018)")+
  theme(title = element_text (size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.x=element_text(angle=45,hjust=1),
        legend.position = "bottom")
ggsave("plots/wic_netmigration.jpg", width = 25, height = 15, units = "cm")

##2. Migration file from Census.
# IBGE population and migrants
### read 2010census file
mig_2010 <- read.csv("data/migration/migration2010.csv", sep=";")

## age and education groups variables
mig_2010$education <- cut(mig_2010$cem_harm_niveleducacao, breaks = c(0, 1, 2, 4, 6, 8, 10),
                             labels = c("No Education","Incomplete Primary","Primary","Lower Secondary","Upper Secondary", "Post Secondary"),
                             include.lowest = T, right = T)
mig_2010$age <- cut(mig_2010$cem_harm_idade, breaks = c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, 199),
                       labels = c("0--4", "5--9", "10--14", "15--19", "20--24", "25--29","30--34", "35--39", "40--44", "45--49","50--54",
                                  "55--59","60--64", "65--69","70--74", "75--79","80--84", "85--89","90+"),
                       include.lowest = T, right = T)
##BASIC DOMESTIC MIGRATION, BY AGE, SEX AND EDUCATIONAL ATTAINMENT - 2005-2010
brazil_mig_2010 <- mig_2010 %>%
  select(V6262, cem_harm_peso, cem_harm_sexo, cem_harm_uf_2010, age, education) %>% 
  rename(area= V6262, weight = cem_harm_peso, sex = cem_harm_sexo, area = cem_harm_uf_2010) %>% 
  mutate (sex= case_when(sex==0 ~ "Female",sex==1 ~ "Male")) %>%
  mutate(area = area/100000) %>% 
  group_by(area,sex,age,education,area) %>% 
  summarise(pop=sum(weight))
brazil_mig_2010
write_csv(brazil_mig_2010, "data/migration/migration_2010.csv")
##read file with census estimations done
brazil_mig_2010 <- read_csv("data/migration/migration_2010.csv")

#deleting non-migratns (area and destination equal states) and estimating migration matrix. Adding states names
brazil_mig_2010b <- brazil_mig_2010 %>% 
  filter(origin!=area) %>% 
  group_by(origin, area) %>% 
  summarise(mig=sum(pop))%>%
  mutate(states = case_when(area==11~"RO",area==12~"AC",area==13~"AM",area==14~"RR",area==15~"PA",
                            area==16~"AP",area==17~"TO",area==21~"MA",area==22~"PI",area==23~"CE",
                            area==24~"RN",area==25~"PB",area==26~"PE",area==27~"AL",area==28~"SE",
                            area==29~"BA",area==31~"MG",area==32~"ES",area==33~"RJ",area==35~"SP",
                            area==41~"PR",area==42~"SC",area==43~"RS",area==50~"MS",area==51~"MT",
                            area==52~"GO",area==53~"DF",TRUE~"OTHER"))%>% 
  pivot_wider(names_from = origin, values_from = mig)%>%
  rename(RO="11",AC="12",AM="13",RR="14",PA="15",AP="16",TO="17",MA="21",PI="22",CE="23",RN="24",PB="25",
         PE="26",AL="27",SE="28",BA="29",MG="31",ES="32",RJ="33",SP="35",PR="41",SC="42",RS="43",MS="50",
         MT="51",GO="52",DF="53") %>% 
  select(area, states, RO, everything()) 

brazil_mig_2010b <- mutate_all(brazil_mig_2010b, ~replace_na(.,0))

#outmigrants and inmigrants
#outmigration
brazil_outmig_2010 <- brazil_mig_2010 %>% 
  filter(area!=origin) %>% 
  group_by(origin) %>% 
  summarise(outmigrants=sum(pop)) %>% 
  mutate(outmigrants2 = case_when(origin<=53~outmigrants, TRUE~0)) %>% #without non specified area
  mutate(outprop = outmigrants2/sum(outmigrants2)) %>% 
  mutate(outmigrants_adjusted = outmigrants2+(outprop*((sum(outmigrants)-sum(outmigrants2)))))
sum(brazil_outmig_2010$outmigrants_adjusted)

#inmigration
brazil_inmig_2010 <- brazil_mig_2010 %>% 
  filter(area!=origin) %>% 
  group_by(area) %>% 
  summarise(inmigrants=sum(pop))

sum(brazil_inmig_2010$inmigrants)

#net migration
out_in_mig2010full<-left_join(brazil_outmig_2010, brazil_inmig_2010, by = join_by(origin==area))
out_in_mig2010<- out_in_mig2010full %>%  
  select(origin, outmigrants_adjusted,inmigrants) %>% 
  rename(area=origin, outmig=outmigrants_adjusted, inmig=inmigrants) %>% 
  mutate(netmig = inmig - outmig,
         period = "2005--2009",
         region = case_when(area==11 ~"North", area==12 ~"North", area==13 ~"North", area==14 ~"North",
                            area==15 ~"North", area==16 ~"North",area==17 ~"North",
                            area==21 ~"Northeast", area==22 ~"Northeast",area==23 ~"Northeast", area==24 ~"Northeast",
                            area==25 ~"Northeast",area==26 ~"Northeast", area==27 ~"Northeast",area==28 ~"Northeast", area==29 ~"Northeast",
                            area==31 ~"Southeast",area==32 ~"Southeast",area==33 ~"Southeast",area==35 ~"Southeast",
                            area==41 ~"South",area==42 ~"South",area==43 ~"South",
                            area==50 ~"Center-West",area==51 ~"Center-West",area==52 ~"Center-West",area==53 ~"Center-West"),
                  area = case_when(area==11~"RO",area==12~"AC",area==13~"AM",area==14~"RR",area==15~"PA",
                          area==16~"AP",area==17~"TO",area==21~"MA",area==22~"PI",area==23~"CE",
                          area==24~"RN",area==25~"PB",area==26~"PE",area==27~"AL",area==28~"SE",
                          area==29~"BA",area==31~"MG",area==32~"ES",area==33~"RJ",area==35~"SP",
                          area==41~"PR",area==42~"SC",area==43~"RS",area==50~"MS",area==51~"MT",
                          area==52~"GO",area==53~"DF")) %>% na.omit()

#domestic migration proportions
#these are the files to calculate proportions. Absolute numbers must consider the adjustments made in the previous step.

brazil_mig_2010%<>%
  filter(origin<=53) %>% 
  mutate(origin = case_when(origin==11~"RO",origin==12~"AC",origin==13~"AM",origin==14~"RR",origin==15~"PA",origin==16~"AP",
                            origin==17~"TO",origin==21~"MA",origin==22~"PI",origin==23~"CE",origin==24~"RN",origin==25~"PB",
                            origin==26~"PE",origin==27~"AL",origin==28~"SE",origin==29~"BA",origin==31~"MG",origin==32~"ES",
                            origin==33~"RJ",origin==35~"SP",origin==41~"PR",origin==42~"SC",origin==43~"RS",origin==50~"MS",
                            origin==51~"MT",origin==52~"GO",origin==53~"DF")) %>% 
  mutate(area = case_when(area==11~"RO",area==12~"AC",area==13~"AM",area==14~"RR",area==15~"PA",area==16~"AP",area==17~"TO",
                          area==21~"MA",area==22~"PI",area==23~"CE",area==24~"RN",area==25~"PB",area==26~"PE",area==27~"AL",
                          area==28~"SE",area==29~"BA",area==31~"MG",area==32~"ES",area==33~"RJ",area==35~"SP",area==41~"PR",
                          area==42~"SC",area==43~"RS",area==50~"MS",area==51~"MT",area==52~"GO",area==53~"DF"))


#outmigrants by state
brazil_outmig_2010_prop <- brazil_mig_2010 %>% 
  filter(origin!=area) %>% 
  group_by(origin, sex, age, education) %>%
  summarise(outmig=sum(pop)) %>% 
  group_by(origin) %>% 
  mutate(outmig_total = sum(outmig)) %>% 
  ungroup() %>% 
  mutate(prop_out = outmig/outmig_total)
write_csv(brazil_outmig_2010_prop,"data/migration/outmig_2010_prop.csv")

#inmigrants by state
brazil_inmig_2010_prop <- brazil_mig_2010 %>% 
  filter(area!=origin) %>% 
  group_by(area, sex, age, education) %>% 
  summarise(inmig=sum(pop)) %>% 
  group_by(area) %>% 
  mutate(inmig_total = sum(inmig)) %>% 
  ungroup() %>% 
  mutate(prop_in = inmig/inmig_total)
write_csv(brazil_inmig_2010_prop,"data/migration/inmig_2010_prop.csv")

#in and outigration join
brazil_outmig_2010_prop %<>% rename(area=origin)
full_mig_2010 <- full_join(brazil_inmig_2010_prop, brazil_outmig_2010_prop)
full_mig_2010 %<>%
  pivot_longer(cols = (inmig:prop_out), values_to = "value", names_to = "variable")

#migration profile by state
#outmigration
brazil_outmig_2010_prop %>% 
  group_by(origin, sex, education) %>%
  ggplot(mapping=aes(x=fct_relevel(origin, c("RJ","RS","SC","MG","AM","PR","ES","MS","DF","SP",
                                             "RR","GO","RN","MT","AP","SE","AC","PE","RO","PB",
                                             "CE","TO","PA","PI","BA","AL","MA")),y=prop_out, fill=fct_inorder(education)))+
  geom_col()+
  facet_wrap(~sex, dir="v")+
  labs(x='State',y='Outmigrants (%)', fill='Education Attainment')+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, hjust = 1),
        axis.title.y = element_text(size = 10))

#fullmigration
full_mig_2010 %>% 
  group_by(area, sex, education) %>%
  filter(variable =="prop_in"|variable=="prop_out") %>% 
  mutate(variable = case_when(variable=="prop_in"~"Inmigration",
                              variable=="prop_out"~"Outmigration")) %>% 
  ggplot(mapping=aes(y=fct_relevel(area, c("RJ","RS","SC","MG","AM","PR","ES","MS","DF","SP","RR","GO","RN","MT","AP",
                                           "SE","AC","PE","RO","PB","CE","TO","PA","PI","BA","AL","MA")),x=value, 
                     fill=fct_relevel(education, c("No Education","Incomplete Primary","Primary","Lower Secondary","Upper Secondary", "Post Secondary"))))+
  geom_col()+
  facet_wrap(~fct_rev(variable))+
  labs(x='State',y=' Migrants (%)', fill='Educational Attainment')+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, hjust = 1),axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size=12), legend.text = element_text(size=12))
ggsave("plots/propmigrants.jpg", width = 25, height = 25, units = "cm")

#check order of outmigration
brazil_outmig_2010_prop %>% 
  group_by(origin, education) %>%
  filter(education =="Post Secondary") %>% 
  summarise(outmig=sum(prop_out)) %>% 
  print(n=27)
#end of proportion estimates


#merging data from 2010 based estimatives with IBGE projections for 5 year intervals
## extracting projected migration data from multiple sheets
#function to extract data from multiple sheets
clean_sheets <- function(sheet) {
  
  # take the characters of each sheet to extract area, skip first 4 rows 
  area <- str_sub(sheet)  
  
  migration_projection <- read_excel(
    "data/IBGE_projection.xls", 
    sheet = sheet,
    skip = 4) %>% 
    mutate(area = area) %>% 
    rename(year = 1, net_migration = 7) %>% 
    select(c(1,7,area)) %>% #keep just relevant columns
    filter(year %in% 2010:2060) # delete unused rows
  migration_projection
}
states <- paste(c("RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB","PE","AL","SE","BA","MG","ES","RJ","SP","PR","SC","RS","MS", "MT", "GO", "DF"))
#combining into one dataframe
migration_projection  <- map_df(states, clean_sheets)
#adjust for 5 year
migration_projection5year<-migration_projection %>% 
  mutate(year = case_when(year %in% 2010:2014~"2010--2014",year %in% 2015:2019~"2015--2019",
                          year %in% 2020:2024~"2020--2024",year %in% 2025:2029~"2025--2029",
                          year %in% 2030:2034~"2030--2034",year %in% 2035:2039~"2035--2039",
                          year %in% 2040:2044~"2040--2044",year %in% 2045:2049~"2045--2049",
                          year %in% 2050:2054~"2050--2054",year %in% 2055:2059~"2055--2059"))%>% 
  group_by(year,area) %>% reframe(sum(net_migration))%>%
  rename(netmig="sum(net_migration)") %>% na.omit() %>% 
  pivot_wider(names_from = year, values_from = netmig)
#join files
migration_projection5year<- full_join(migration_projection5year, out_in_mig2010, by = join_by(area))
migration_projection5year<-migration_projection5year %>% 
  rename("2005--2009"=netmig) %>% select(-outmig, - inmig, - period) %>% 
  pivot_longer(cols = ("2010--2014":"2005--2009"), values_to = "netmig", names_to = "year")

write_csv(migration_projection5year,"data/migration/migration_projection5year.csv")
#read data with the migrants estimated with IBGE data for 5 year period
migration_projection5year <- read_csv("data/migration/migration_projection5year.csv")

##3. calculate absolute numbers considering the census and projections estimations
migration_projection5year <- migration_projection5year %>%
  filter(year!="2005--2009")
abs_mig <- full_join(out_in_mig2010, migration_projection5year, by = join_by(area)) %>% 
  rename(netmigcensus= netmig.x, netmigproj = netmig.y) %>% 
  mutate(outmigproj = ((netmigproj/netmigcensus)*outmig),
         inmigproj = ((netmigproj/netmigcensus)*inmig))
write_csv(abs_mig,"data/migration/migration_absolutenumbers5year.csv")

##finally, the numbers for out and inmigrants
full_mig_2010 %<>%
  filter(variable=="prop_in"|variable=="prop_out") %>% 
  pivot_wider(names_from = variable, values_from = value)
full_mig_2060 <- full_join(full_mig_2010, abs_mig, by = join_by(area), multiple = "all") %>% 
  select(area:prop_out, region.x, year,netmigproj:inmigproj) %>% 
  mutate(outmigfinal = prop_out*outmigproj,
         inmigfinal = prop_in*inmigproj)
write_csv(full_mig_2060,"data/migration/full_mig_2060.csv")


##some plots

#projected IBGE migration plot
migration_projection5year %>%
  filter(year!="2005--2009") %>% 
ggplot(mapping=aes(x=year, y = netmig, color=area, group=area))+
  geom_line(linewidth=1)+ geom_point(size=3)+
  facet_wrap(~region, dir = "h")+
  labs(x='Period',y='Net Migration', color='State')+
  theme(axis.text.x = element_text(size=14, angle = 90, hjust = 1),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.85, 0.12),
        legend.text = element_text(size=12),
        legend.title = element_text(hjust = 0.5, size=12))+
  guides(color=guide_legend(ncol=3))

ggsave("plots/IBGE_estimatedmigration.jpg", width = 50, height = 30, units = "cm")


#comparison census and projection
migration_projection5year %>%
  filter(year=="2005--2009"|year=="2010--2014") %>% 
  ggplot(mapping=aes(x=area,y=netmig))+
  geom_col()+
  facet_col(~year)
+
  labs(x='Period',y='Net Migration', color='State')+
  theme(axis.text.x = element_text(size=8, angle = 90, hjust = 1),
        axis.title.y = element_text(size = 10))

#RJ and DF examples
df1<-full_mig_2010 %>% 
  filter(variable =="inmig"|variable=="outmig") %>% filter(area=="DF"|area=="RJ")%>% 
  mutate(value = case_when(sex=="Male"~-value, TRUE~value)) %>% 
  mutate(variable = case_when(variable=="inmig"~"Inmigration",variable=="outmig"~"Outmigration",TRUE~variable)) %>% 
  separate(age, c("age", "end")) %>% mutate(age=as.numeric(age))
df1 <-  mutate_all(df1, ~replace_na(.,0))
df1 %>%
  ggplot(mapping=aes(y=as.factor(age),x=value,
                     fill=fct_relevel(education, c("Post Secondary","Upper Secondary","Lower Secondary","Primary","Incomplete Primary","No Education"))))+
  geom_col()+
  facet_grid(~fct_rev(variable)~area)+
  geom_vline(xintercept = 0)+
  labs(y=NULL, x=NULL, fill='Educational Attainment')+
  theme_bw()+
  scale_x_continuous(labels = abs)+
  scale_fill_manual(values =c("#2166AC","#67A9CF","#D1E5F0","#FDDBC7","#EF8A62","#B2182B"))+
  theme(axis.text.x = element_text(size=10, hjust = 0.5),axis.text.y = element_text(size=10, vjust = 0.5),
        legend.text = element_text(size=11),legend.title = element_text(size=12), legend.position = "bottom",
        strip.text = element_text(size=12,face="bold"))
ggsave("plots/DFandRJ.jpg", width = 35, height = 35, units = "cm")

##output tables
out_in_mig2010 %>%
  select(area, inmig, outmig, netmig) %>%
  kable(col.names = c("State", "Outmigration", "Inmigration", "Net migration"), digits=0, format.args=list(big.mark=".", decimal.mark=",")) %>% 
  kable_classic_2 (full_width = F)
