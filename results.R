#### results plots - share format
pacman::p_load(readxl,tidyverse, reshape2, wcde, ggforce, flextable, purrr,magrittr,data.table,gt)
pacman::p_load(sf,geobr,RColorBrewer, viridis,ggrepel)
brazil_states<-read_state(code_state="all", year=2010)
ggplot(brazil_states)+geom_sf()

options(digits=3,scipen=999)

### colors

brazilcolors <-(c("#2166AC","#67A9CF","#D1E5F0","#FDDBC7","#EF8A62","#B2182B","grey"))
         
### IBGE projection
{
clean_sheet_proj <- function(sheet) {
  
  # take the characters of each sheet to extract area, skip first 7 rows 
  region <- str_sub(sheet)  
  
  projection_table <- read_excel(
    "../data/IBGE_projection.xls", 
    sheet = sheet,
    range = "A5:AK56") %>% 
    mutate(region = region) %>% 
    mutate(uf=region)
  
}
states <- paste(c("Brasil","RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB","PE","AL","SE","BA","MG","ES","RJ","SP","PR","SC","RS","MS", "MT", "GO", "DF"))
states
IBGE_projection  <- map_df(states, clean_sheet_proj)

IBGE_projection %<>%
  rename(Year = ...1,
         Population = Total...2,
         births = ...5,
         deaths = ...6,
         CBR = ...11,
         CDR = ...12)
IBGE_projection2 <- IBGE_projection %>%
  rename(tfr_ibge = ...20, Male = Homens...15, Female = Mulheres...16) %>% 
  select(Year, Population,births,deaths,CBR,CDR,tfr_ibge,Female, Male, region)
setDT(IBGE_projection2)

#deaths
IBGE_projection2[Year%in%2022:2026, by=.(region),deaths5:=sum(deaths)][Year%in%2027:2031, by=.(region),deaths5:=sum(deaths)][
  Year%in%2032:2036, by=.(region),deaths5:=sum(deaths)][Year%in%2037:2041, by=.(region),deaths5:=sum(deaths)][
  Year%in%2042:2046, by=.(region),deaths5:=sum(deaths)][Year%in%2047:2051, by=.(region),deaths5:=sum(deaths)][
  Year%in%2052:2056, by=.(region),deaths5:=sum(deaths)][Year%in%2057:2061, by=.(region),deaths5:=sum(deaths)]

#births
IBGE_projection2[Year%in%2022:2026, by=.(region),births5:=sum(births)][Year%in%2027:2031, by=.(region),births5:=sum(births)][
  Year%in%2032:2036, by=.(region),births5:=sum(births)][Year%in%2037:2041, by=.(region),births5:=sum(births)][
  Year%in%2042:2046, by=.(region),births5:=sum(births)][Year%in%2047:2051, by=.(region),births5:=sum(births)][
  Year%in%2052:2056, by=.(region),births5:=sum(births)][Year%in%2057:2061, by=.(region),births5:=sum(births)]

IBGE_projection2 %<>%
  filter(Year==2022|Year==2027|Year==2032|Year==2037|Year==2042|Year==2047|Year==2052|Year==2057)
write.csv(IBGE_projection2,file = "../data/output/IBGE_projection.csv")
}

###our projection
final1$scenario<-1
write.csv(final1,file = "data/output/final2.csv")
final2$scenario<-2
write.csv(final2,file = "data/output/final1.csv")
final3$scenario<-3
write.csv(final3,file = "data/output/final3.csv")
final2<-read.csv("data/output/final2.csv")
final1<-read.csv("data/output/final1.csv")
final3<-read.csv("data/output/final3.csv")
finalssp<-bind_rows(final1,final2,final3)

finalssp %<>%
  mutate(area= case_when(region==11~"RO",region==12~"AC",region==13~"AM",region==14~"RR",region==15~"PA",
                         region==16~"AP",region==17~"TO",region==21~"MA",region==22~"PI",region==23~"CE",
                         region==24~"RN",region==25~"PB",region==26~"PE",region==27~"AL",region==28~"SE",
                         region==29~"BA",region==31~"MG",region==32~"ES",region==33~"RJ",region==35~"SP",
                         region==41~"PR",region==42~"SC",region==43~"RS",region==50~"MS",region==51~"MT",
                         region==52~"GO",region==53~"DF"))
setDT(finalssp)

### total population
finalssp[agest!=-5,by=.(Time,scenario),sum(pop)] %>% spread(scenario,V1)
final2[agest>=65,by=.(Time),sum(pop)]/final2[agest!=-5,by=.(Time),sum(pop)]
final4<-finalssp %>% filter(agest!=-5) %>%
  mutate(edu=case_when(agest<=10~"Under 15",
                       TRUE~edu)) %>% 
  group_by(Time,edu,scenario) %>% 
  summarise(pop=sum(pop))

# Filtrando e transformando os dados
final4 <- finalssp %>%
  filter(agest != -5) %>%
  mutate(edu = case_when(
    agest <= 10 ~ "Under 15",
    TRUE ~ edu)) %>%
  group_by(Time, edu, scenario) %>%
  summarise(pop = sum(pop), .groups = 'drop')

# Calculando o tempo de população máxima para cada cenário
max_times <- final4 %>%
  group_by(scenario) %>%
  summarise(Time = Time[which.max(sum(pop))], .groups = 'drop')

# Plotando o gráfico
final4 %>%
  ggplot(aes(x = Time, y = pop / 1000000, fill = fct_relevel(edu, "e6", "e5", "e4", "e3", "e2", "e1", "Under 15"))) +
  geom_area() +
  facet_wrap(~scenario) +
  scale_fill_manual(values = brazilcolors) +
  labs(y = "Population (millions)", fill = "Educational attainment", x = "Time") +
  theme_bw() +
  theme(
    legend.title = element_text(size = 11, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    legend.box.spacing = unit(0.1, "cm"),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12, margin = margin(r = 5)),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5),
    strip.text = element_text(size = 12),
    panel.grid.major = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  geom_vline(data = max_times, aes(xintercept = Time), linetype = "dashed", color = "red")

print(totalpopulation)


max_time <- final4 %>%
  group_by(scenario, Time) %>%
  summarise(total_pop = sum(pop)) %>%
  filter(total_pop == max(total_pop)) %>%
  pull(Time,scenario)

totalpopulation<-final4%>%
  ggplot(aes(x=Time, y= pop/1000000, 
             fill = fct_relevel(edu,"e6","e5","e4","e3", "e2","e1","Under 15")))+
  geom_area()+
  facet_wrap(~scenario)+
  scale_fill_manual(values = brazilcolors)+
  labs (y = "Population (millions)", fill="Educational attainment", x = "Time")+
  theme_bw()+
  theme(legend.title = element_text(size=11, hjust = 0.5), legend.text = element_text(size=10),
        legend.position = "bottom",
        legend.box.spacing = unit(0.1, "cm"),
        axis.text.y = element_text(size=12),axis.title.y = element_text(size=12, margin = margin(r=5)),
        axis.title.x = element_text(size=12),axis.text.x = element_text(size=12, angle = 45,vjust=0.5),
        strip.text = element_text(size = 12),
        panel.grid.major = element_blank())+
  guides(fill=guide_legend(nrow = 1))+
  geom_vline(xintercept = max_time, linetype = "dashed", color = "red")
totalpopulation
ggsave("plots/totalpopulation.png", totalpopulation, width = 10, height = 6, dpi = 300)

### total population - portugues
final2[agest!=-5,by=.(Time),sum(pop)]
final2[agest>=65,by=.(Time),sum(pop)]/final2[agest!=-5,by=.(Time),sum(pop)]
final3<-final2 %>% filter(agest!=-5) %>%
  mutate(edu=case_when(agest<=10~"Abaixo de 15 anos",
                       edu=="e1"~"Sem escolaridade",
                       edu=="e2"~"Ensino primário Incompleto",
                       edu=="e3"~"Ensino primário",
                       edu=="e4"~"Ensino Fundamental",
                       edu=="e5"~"Ensino Médio",
                       edu=="e6"~"Ensino superior")) %>% 
  group_by(Time,edu) %>% 
  summarise(pop=sum(pop))
#graph of total population by edu att
final3 %>% 
  ggplot(aes(x=Time, y= pop/1000000, 
             fill = fct_relevel(edu,"Ensino superior","Ensino Médio","Ensino Fundamental",
                                "Ensino primário", "Ensino primário Incompleto","Sem escolaridade","Abaixo de 15 anos")))+
  geom_area()+
  scale_fill_manual(values = brazilcolors)+
  labs (y = "População (em milhões)", fill="Nível educacional", x= "Período")+
  theme_bw()+
  theme(legend.title = element_text(size=11, hjust = 0.5), legend.text = element_text(size=10),
        legend.position = "bottom",
        axis.text.y = element_text(size=11),axis.title.y = element_text(size=11, margin = margin(r=5)),
        axis.title.x = element_text(size=11),axis.text.x = element_text(size=11, angle = 45,vjust=0.5),
        strip.text = element_text(size = 11),
        panel.grid.major = element_blank())
##maps
no_axis <- theme(title = element_text (size=9),
                 legend.title = element_text(size=12),
                 legend.position = "bottom",
                 axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

#map of population growth and edu growth
pop2022 <- finalssp[agest!=-5&Time==2022,by=.(Time,region,scenario),.(pop2022=sum(pop))]
pop2062 <-  finalssp[agest!=-5&Time==2062,by=.(Time,region,scenario),.(pop2062=sum(pop))]
popgrowth <-left_join(pop2022, pop2062, by=join_by(region,scenario))%>%  
  mutate(prop=pop2062/pop2022-1) %>% 
  rename(code_state=region) %>% mutate(code_state=as.numeric(code_state))
popgrowth <-left_join(popgrowth, brazil_states)
popgrowth %>% 
  ggplot()+
  geom_sf(aes(fill=100,geometry=geom))+
  geom_label_repel(aes(label = name_state,geometry=geom),
    stat = "sf_coordinates",
    min.segment.length = 0,
    colour="black",
    segment.colour="white",
    label.size = NA)+
  facet_wrap(~scenario)+
  scale_fill_viridis(option= "magma", direction=-1, 
                     name = "Percentual population growth (2010-2060)",
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5))+
  theme_minimal()

popgrowth %>% 
  ggplot()+
  geom_sf(aes(fill=100*prop,geometry=geom))+
  facet_wrap(~scenario)+
  scale_fill_viridis(option= "magma", direction=-1, 
                     name = "Percentual population growth (2022-2062)",
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5))+
  theme_minimal()+ no_axis

#edu growth - e6
final[agest!=-5&edu=="e6",by=.(Time,agest),.(pop=sum(pop))] %>% spread(Time, pop)
finalssp[agest>=25&edu=="e6"&Time==2062,by=.(region,scenario),.(pop=sum(pop))]/finalssp[agest>=25&Time==2062,by=.(region,scenario),.(pop=sum(pop))]
final[agest>=25&edu=="e6"&sex=="m",by=.(Time),.(pop2010=sum(pop))]/final[agest>=25&sex=="m",by=.(Time),.(pop2010=sum(pop))]
final[agest>=25&edu=="e6"&sex=="f",by=.(Time),.(pop2010=sum(pop))]/final[agest>=25&sex=="f",by=.(Time),.(pop2010=sum(pop))]
final[agest>=25&edu=="e6",by=.(Time),.(pop2010=sum(pop))]/final[agest>=25,by=.(Time),.(pop2010=sum(pop))]
final[agest>=25&edu=="e6",by=.(Time,sex,region),.(pope6=sum(pop))]
finalssp[agest>=25,by=.(Time,sex,region,scenario),.(pop2010=sum(pop))]


final[,region:=as.numeric(region)]

edu2010 <- final2[agest>=25&Time==2010&edu=="e6",by=.(Time,region,edu),.(pop2010=sum(pop))]
edu2060 <-  final2[agest>=25&Time==2060&edu=="e6",by=.(Time,region,edu),.(pop2060=sum(pop))]
e6growth <-left_join(edu2010, edu2060, by=join_by(region))%>%  
  mutate(prop=pop2060/pop2010-1) %>% 
  rename(code_state=region) %>% mutate(code_state=as.numeric(code_state))
e6growth <-left_join(e6growth, brazil_states)
e6growth %>% 
  ggplot(aes(fill=100*prop,geometry=geom))+
  geom_sf()+
  scale_fill_viridis(option= "magma", direction=-1, 
                     name = "Percentual growth of population with Post-secondary Education (2010-2060)",
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5))+
  theme_minimal()+ no_axis
setDT(e6growth)
e6growth[,by=.(name_state),(v1=prop*100)][order(V1)]
#### fertility
#TFR - how many births by mother age?
#brazil
IBGE_tfr<-IBGE_projection2 %>% filter(region=="Brasil") %>%  
  select(Year,tfr_ibge) %>% mutate(edu="IBGE",
                                   scenario=2) %>%
  rename(Time=Year,TFR=tfr_ibge) 

TFRcheck<-finalssp[agest%in% 15:45,by=.(Time,agest,scenario),.(births=sum(births))]
TFRcheckw <- finalssp[agest%in% 15:45 & sex =="f",by=.(Time,agest,scenario),.(popf=sum(pop))] %>% filter(Time!=2062)
ssp2tft<-TFRcheck %>% filter(Time!=2062) %>% 
  mutate(popf = TFRcheckw$popf,
         asfr = births/5/popf) %>%  group_by(Time,scenario) %>% 
  reframe(TFR=sum(asfr)*5) 
setDT(ssp2tft)

ssp2tft[,edu:="All groups"]
ssp2tft[,by=.(Time,scenario),TFR]

TFRcheckedu<-finalssp[agest%in%15:45,by=.(agest,Time,edu,scenario),.(births=sum(births))]
TFRcheckwedu <- finalssp[agest%in% 15:45 & sex =="f",by=.(agest,Time,edu,scenario),.(popf=sum(pop))]
TFRcheckedu %<>% 
  mutate(popf = TFRcheckwedu$popf,
         asfr=ifelse(agest==15&edu== "e6",0, births/5/popf)) %>%  group_by(Time,edu,scenario) %>% 
  reframe(TFR=sum(asfr)*5) %>% filter(Time!=2062)
setDT(TFRcheckedu)
TFRcheckedu[edu=="e1",by=.(edu,Time,scenario),TFR] %>% spread(Time,TFR)
TFRcheckedu[,by=.(edu,Time,scenario),TFR] %>% spread(Time,TFR)

TFRcheckedureg<-finalssp[agest%in%15:45,by=.(agest,Time,edu,region,scenario),.(births=sum(births))]
TFRcheckwedureg <- finalssp[agest%in% 15:45 & sex =="f",by=.(agest,Time,edu,region,scenario),.(popf=sum(pop))]
TFRcheckedureg %<>% 
  mutate(popf = TFRcheckwedureg$popf,
         asfr=ifelse(agest==15&edu== "e6",0, births/5/popf)) %>%  group_by(Time,edu,region,scenario) %>% 
  reframe(TFR=sum(asfr)*5) %>% filter(Time!=2062)
setDT(TFRcheckedureg)
TFRcheckedureg[edu=="e6",by=.(edu,Time,region,scenario),TFR] %>% spread(Time,TFR)
TFRcheckedu[,by=.(edu,Time,scenario),TFR] %>% spread(Time,TFR)

#plot overall tfr
tfr<-bind_rows(ssp2tft,TFRcheckedu,IBGE_tfr) %>% filter(Time!=2062) %>%
  mutate("TFR_by"=case_when(edu=="All groups"~"All groups",
                edu=="e1"~"No Education",
                edu=="e2"~"Incomplete Primary",
                edu=="e3"~"Primary",
                edu=="e4"~"Lower Secondary",
                edu=="e5"~"Upper Secondary",
                edu=="e6"~"Post Secondary",
                edu=="IBGE"~"IBGE")) %>%
  ggplot(mapping = aes(x=Time, y=TFR, color=fct_relevel(TFR_by,"Post Secondary","Upper Secondary","Lower Secondary", "Primary", "Incomplete Primary","No Education","All groups","IBGE")))+
  geom_point()+geom_line()+
  facet_wrap(~scenario)+
  geom_point(size=2)+geom_line(linewidth=0.5)+
  scale_color_manual(values=c("#2166AC","#67A9CF","#D1E5F0","#FDDBC7","#EF8A62","#B2182B","black","green"))+
  labs (y = "Total Fertility Rate", color="Educational attainment of the mother")+
  theme_bw()+
  theme(legend.title = element_text(size=11, hjust = 0.5), legend.text = element_text(size=10), 
        legend.position = "bottom", legend.box.spacing = unit(0.1,"cm"),
        axis.text.y = element_text(size=11),axis.title.y = element_text(size=11, margin = margin(r=5)),
        axis.title.x = element_text(size=11),axis.text.x = element_text(size=11, angle = 45,vjust=0.5),
        strip.text = element_text(size = 11),
        panel.grid.major = element_blank())
tfr
ggsave("plots/tfr.png",tfr,width = 10, height = 6, dpi = 300)
unique(TFRcheckedu$edu)

easfr<-TFRcheckedu %>%
  filter(Time==2010|Time==2055) %>% 
  mutate(edu=case_when(edu=="e1"~"No Education",
                            edu=="e2"~"Incomplete Primary",
                            edu=="e3"~"Primary",
                            edu=="e4"~"Lower Secondary",
                            edu=="e5"~"Upper Secondary",
                            edu=="e6"~"Post Secondary")) %>%
  ggplot(mapping = aes(x=agest,
                       y=asfr*1000, 
                       color= edu,
                       shape = as.character(Time),
                       group = interaction(Time,edu)))+
  geom_point()+geom_line()+
  geom_point(size=2)+geom_line(linewidth=0.5)+
  scale_color_manual(values=c("#2166AC","#67A9CF","#D1E5F0","#FDDBC7","#EF8A62","#B2182B","green","black"))+
  labs (x= "Age group", y = "Educational age specific feritlity rates", color="Ed. Attainment of the mother", shape ="Time")+
  theme_bw()+
  theme(legend.title = element_text(size=14, hjust = 0.5), legend.text = element_text(size=12), 
        legend.position = "bottom", legend.box.spacing = unit(0.1,"cm"),
        axis.text.y = element_text(size=12),axis.title.y = element_text(size=12, margin = margin(r=5)),
        axis.title.x = element_text(size=12),axis.text.x = element_text(size=12),
        panel.grid.major = element_blank())
ggsave("easfr.png",easfr,width = 10, height = 6, dpi = 300)

#portugues
bind_rows(TFRcheck,TFRcheckedu,IBGE_tfr) %>% filter(Time<=2055) %>%
  ggplot(mapping = aes(x=Time, y=TFR, color=edu))+
  geom_point()+geom_line()+
  geom_point(size=2)+geom_line(linewidth=0.5)+
  scale_color_manual(values=c("#2166AC","#67A9CF","#D1E5F0","#FDDBC7","#EF8A62","#B2182B","green","black"))+
  labs (y = "Total Fertility Rate", color="TFR group")+
  theme_bw()+
  theme(legend.title = element_text(size=11, hjust = 0.5), legend.text = element_text(size=10), 
        axis.text.y = element_text(size=11),axis.title.y = element_text(size=11, margin = margin(r=5)),
        axis.title.x = element_text(size=11),axis.text.x = element_text(size=11, angle = 45,vjust=0.5),
        strip.text = element_text(size = 11),
        panel.grid.major = element_blank())


### aging
dep_pop65<-final2[agest>=65,by=.(Time,region),.(popdep=sum(pop))]
work_pop<-final2[agest>=15&agest<=60,by=.(Time,region),.(popwork=sum(pop))]
olddr<-left_join(dep_pop65,work_pop)%>%  
  mutate(tdr=100*popdep/popwork) %>% 
  rename(code_state=region) %>% mutate(code_state=as.numeric(code_state))
options(digits = 4)
olddr <- left_join(brazil_states,olddr)
olddr %>% 
  filter(Time==2010|Time==2060) %>% 
  ggplot(aes(fill=tdr,label=tdr))+
  geom_sf()+
  facet_wrap(~Time)+
  scale_fill_viridis(option= "magma", direction=-1, 
                     name = "Razão de dependência de Idosos ((65+)/(15-64))",
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5))+
  theme_minimal()+ no_axis
setDT(olddr)
olddr[Time==2060,by=.(name_state),tdr][order(tdr)]

dep_pop15<-final2[agest<=10&agest>=0,by=.(Time,region),.(popdep=sum(pop))]
youthdr<-left_join(dep_pop15,work_pop)%>%  
  mutate(tdr=100*popdep/popwork) %>% 
  rename(code_state=region) %>% mutate(code_state=as.numeric(code_state))
ydr <- left_join(brazil_states,youthdr)
ydr %>% 
  filter(Time==2010|Time==2060) %>% 
  ggplot(aes(fill=tdr,label=tdr))+
  geom_sf()+
  facet_wrap(~Time)+
  scale_fill_viridis(option= "magma", direction=-1, 
                     name = "Razão de dependência Jovem ((15-)/(15-64))",
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5))+
  theme_minimal()+ no_axis
setDT(ydr)
ydr[Time==2060,by=.(name_state),tdr][order(tdr)]

#65 proportion
pop65<-final2[agest>=65,by=.(Time,region),.(pop65=sum(pop))]
poptotal<-final2[agest!=-5,by=.(Time,region),.(poptot=sum(pop))]
age65prop<-left_join(pop65,poptotal)%>%  
  mutate(prop65=pop65/poptot) %>% 
  rename(code_state=region) %>% mutate(code_state=as.numeric(code_state))
options(digits = 2)

pop15<-final2[agest>=0&agest<=10,by=.(Time,region),.(pop15=sum(pop))]
age15prop<-left_join(pop15,poptotal)%>%  
  mutate(prop15=pop15/poptot) %>% 
  rename(code_state=region) %>% mutate(code_state=as.numeric(code_state))
options(digits = 2)

ageprop <- left_join(age15prop,age65prop) %>% 
  pivot_longer(cols=c(prop15,prop65), values_to = "prop", names_to = "Age_Group")


ageprop <- left_join(brazil_states,ageprop)
ageprop %>% 
  filter(Time==2010|Time==2060) %>% 
  ggplot(aes(fill=100*prop,label=100*prop))+
  geom_sf()+
  facet_wrap(~Age_Group~Time)+
  scale_fill_viridis(option= "magma", direction=-1, 
                     name = "Population by age group (15 and less and 65 and more)(%)",
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5))+
  theme_minimal()+ no_axis
  
#label
  ggrepel::geom_label_repel(
    aes(label = round(100*prop, digits=1),geometry=geom),
    stat = "sf_coordinates",
    min.segment.length = 0,
    colour="grey",
    segment.colour="white",
    fontface='bold',
    label.size = NA)


#age structure

agestructure <- finalssp %>% filter((Time==2022|Time==2042|Time==2062)&agest!=-5) %>%
  mutate(edu=case_when(agest%in%0:10~"Under 15",
                       edu=="e1"~"No Education",
                       edu=="e2"~"Incomplete Primary",
                       edu=="e3"~"Primary",
                       edu=="e4"~"Lower Secondary",
                       edu=="e5"~"Upper Secondary",
                       edu=="e6"~"Post Secondary"),
         pop_pyramid = ifelse(sex == "m",-pop/1000, pop/1000)) %>% 
  ggplot(mapping = aes(x=pop_pyramid, y=as.factor(agest), 
                       fill=fct_relevel(edu,"Post Secondary","Upper Secondary","Lower Secondary", "Primary", "Incomplete Primary","No Education","Under 15")))+
  geom_col()+geom_vline(xintercept=0)+scale_fill_manual(values = wic_col6)+
  scale_x_continuous(labels = abs)+
  facet_wrap(~scenario~Time)+
  labs (x = "Population (millions)",y = "Age group", fill = "Educational attainment") +
  theme_bw()+
  theme(legend.title = element_text(size=11, face = "bold", hjust = 0.5), 
        legend.text = element_text(size=10), legend.position = "right",legend.direction = "vertical",
        axis.text.y = element_text(size=11),axis.title.y = element_text(size=11, margin = margin(r=5)),
        axis.title.x = element_text(size=12),axis.text.x = element_text(size=11),
        strip.text.x = element_text(size=12),
        panel.grid.major = element_blank())+
  guides(fill = guide_legend(title.position = "top", label.hjust = 0))
agestructure

ggsave("plots/agestr.png", agestructure, width = 10, height = 6, dpi = 300)


### mortality
#ibge data
IBGE_e0<-IBGE_projection2 %>% filter(region=="Brasil",Year!=2062) %>%  
  select(Year,Male,Female) %>% mutate(edu="IBGE") %>% rename(Time=Year,m=Male,f=Female) %>% 
  pivot_longer(cols =c("m","f"), values_to = "ex", names_to = "sex")

#projections data
all_lt<-bind_rows(lt,ltedu)

#joined data
ltIBGESSP1<-all_lt %>% filter(agest==-5) %>% 
  select(Time,edu,sex,ex)
ltIBGESSP<-bind_rows(ltIBGESSP1,IBGE_e0)

#Life expectancy graph
e0<-ltIBGESSP %>% 
  mutate(sex=case_when(sex=="f"~"Female",
                       sex=="m"~"Male"),
         edu=case_when(edu=="IBGE"~"IBGE",
                       edu=="all"~"All groups",
                       edu=="e1"~"No Education",
                       edu=="e2"~"Incomplete Primary",
                       edu=="e3"~"Primary",
                       edu=="e4"~"Lower Secondary",
                       edu=="e5"~"Upper Secondary",
                       edu=="e6"~"Post Secondary")) %>%
  ggplot(mapping = aes(x=Time, y=ex, 
                       color=fct_relevel(edu,"Post Secondary","Upper Secondary","Lower Secondary", "Primary", "Incomplete Primary","No Education","All groups")))+
  geom_point(size=3)+geom_line(linewidth=1)+facet_wrap(~sex)+
  scale_color_manual(values = c("#2166AC","#67A9CF","#D1E5F0","#FDDBC7","#EF8A62","#B2182B","black", "green"))+
  labs (y = "Life expectancy(0)", color="Educational attainment")+
  theme_bw()+
  theme(legend.title = element_text(size=11, hjust = 0.5), legend.text = element_text(size=10), 
        legend.position = "bottom", legend.box.spacing = unit(0.1,"cm"),
        axis.text.y = element_text(size=11),axis.title.y = element_text(size=11, margin = margin(r=5)),
        axis.title.x = element_text(size=11),axis.text.x = element_text(size=11, angle = 45,vjust=0.5),
        strip.text = element_text(size = 11),
        panel.grid.major = element_blank())
ggsave("e0.png", e0, width = 10, height = 6, dpi = 300)

IBGEdeaths<- IBGE_projection2[,by=.(Year,region),deaths5]%>% spread(Year,deaths5) %>% 
  mutate(source="IBGE") %>% rename(area=region)

SSP2deaths<- final2[Time!=2060,by=.(Time,area),sum(deaths)] %>% spread(Time,V1) %>% 
  mutate(source="SSP") 

All_deaths<-bind_rows(IBGEdeaths,SSP2deaths) %>% 
  pivot_longer(cols = 2:11, names_to = "year", values_to = "deaths") %>% 
  pivot_wider(names_from = source, values_from = deaths)

#deaths comparison graph
All_deaths %>% filter(area!="Brasil") %>% 
  ggplot(mapping = aes(x=year, y=IBGE/SSP,color=area))+ 
  geom_point(size=3)+geom_line(aes(group=area))+
  labs (y = "IBGE deaths / SSP2 deaths", color="State")+
  theme_bw()+
  theme(legend.title = element_text(size=11, hjust = 0.5), legend.text = element_text(size=10), 
        axis.text.y = element_text(size=11),axis.title.y = element_text(size=11, margin = margin(r=5)),
        axis.title.x = element_text(size=11),axis.text.x = element_text(size=11, angle = 45,vjust=0.5),
        strip.text = element_text(size = 11),
        panel.grid.major = element_blank())


final[,by=.(Time),1000*sum(deaths)/5/sum(pop)]#CMR
final[Time<=2015,sum(deaths)] 

nmxedu<-ltedu %>%
  filter((Time==2010|Time==2055)&agest<=80) %>% 
  mutate(edu=case_when(edu=="e1"~"No Education",
                       edu=="e2"~"Incomplete Primary",
                       edu=="e3"~"Primary",
                       edu=="e4"~"Lower Secondary",
                       edu=="e5"~"Upper Secondary",
                       edu=="e6"~"Post Secondary"),
         sex=case_when(sex=="f"~"Female",
                       sex=="m"~"Male")) %>%
  ggplot(mapping = aes(x=agest,
                       y=log(nmx), 
                       color= edu,
                       shape = as.character(Time),
                       group = interaction(Time,edu)))+
  facet_wrap(~sex)+
  geom_point()+geom_line()+
  geom_point(size=2)+geom_line(linewidth=0.5)+
  scale_color_manual(values=c("#2166AC","#67A9CF","#D1E5F0","#FDDBC7","#EF8A62","#B2182B","green","black"))+
  labs (x= "Age group", y = "Educational age specific mortality rates", color="Educational Attainment", shape ="Time")+
  theme_bw()+
  theme(legend.title = element_text(size=14, hjust = 0.5), legend.text = element_text(size=12), 
        legend.position = "bottom", legend.box.spacing = unit(0.1,"cm"),
        axis.text.y = element_text(size=12),axis.title.y = element_text(size=12, margin = margin(r=5)),
        axis.title.x = element_text(size=12),axis.text.x = element_text(size=12),
        strip.text = element_text(size=14),
        panel.grid.major = element_blank())
ggsave("nmxedu.png",nmxedu,width = 10, height = 6, dpi = 300)


#migration
final2$region<- as.numeric(final2$region)

final2[,by=.(Time),sum(odom)]
final[,by=.(Time),sum(idom)]
final[,by=.(Time),sum(idom)]-final[,by=.(Time),sum(odom)]

final2[Time=="2030",by=.(Time,region),sum(idom)-sum(odom)]
final2[Time=="2010",by=.(Time,region),sum(idom)-sum(odom)]
final[,by=.(Time,region),sum(odom)] %>% spread(Time,V1) 
final2[,by=.(Time,area),sum(idom)-sum(odom)] %>% spread(Time,area,V1) 

final2[Time=="2010",by=.(area),sum(idom)][order(-V1)] #top1 inmigration state
final2[Time=="2010",by=.(area),sum(odom)][order(-V1)] #top1 outmigration state

mig2050<-final2[Time=="2050",by=.(Time,area),sum(idom)-sum(odom)][order(-V1)] #net migration
migracao<-final2 %>% filter(Time!=2010&Time!=2060) %>% 
  group_by(Time,area,sex) %>% 
  summarise(idom=sum(idom),
            odom=sum(odom)) %>% 
  mutate(netmig=idom-odom,
         sex=case_when(sex=="f"~"Female",TRUE~"Male"))

mig<-migracao %>% 
  filter(Time==2015|Time==2030|Time==2050) %>% 
  ggplot(mapping = aes(y=netmig, 
                       x=fct_reorder(area, -netmig), fill=as.character(Time)))+
  geom_col(position= position_dodge(width=0.8))+
  facet_wrap(~sex)+
  labs (y = "Net migration", fill="Time", x = "State")+
  theme_bw()+
  theme(legend.title = element_text(size=11, hjust = 0.5), legend.text = element_text(size=10), 
                   legend.position = "bottom", legend.box.spacing = unit(0.1,"cm"),
                   axis.text.y = element_text(size=11),axis.title.y = element_text(size=11, margin = margin(r=5)),
                   axis.title.x = element_text(size=11),axis.text.x = element_text(size=11, angle = 45,vjust=0.5),
                   strip.text = element_text(size = 11),
                   panel.grid.major = element_blank())
ggsave("mig.png",mig,width = 10, height = 6, dpi = 300)
mig2050 %>% 
  ggplot(mapping = aes(y=V1, x=area))+ 
  geom_col()
+geom_vline(xintercept=0)+scale_fill_manual(values = wic_col6)+
  scale_x_continuous(labels = abs)+
  labs (x = "População (em milhares)",y = "Grupo etário",fill = "Nível educacional") +
  theme(legend.title = element_text(size=11, face = "bold", hjust = 0.5), legend.text = element_text(size=10), legend.position = "bottom",legend.direction = "horizontal",
        axis.text.y = element_text(size=11),axis.title.y = element_text(size=11, margin = margin(r=5)),
        axis.title.x = element_text(size=11),axis.text.x = element_text(size=11),
        strip.text = element_text(face = "bold", size = 11),
        panel.grid.major = element_blank())+
  guides(fill = guide_legend(title.position = "top", label.hjust = 0))

### arquivo final

Proj_Brasil_v1 <- final2 %>% filter(agest!=-5) %>%
  mutate(edu=case_when(agest<=10~"Abaixo de 15 anos",
                       edu=="e1"~"Sem escolaridade",
                       edu=="e2"~"Ensino primário Incompleto",
                       edu=="e3"~"Ensino primário",
                       edu=="e4"~"Ensino Fundamental",
                       edu=="e5"~"Ensino Médio",
                       edu=="e6"~"Ensino superior")) %>% 
  rename("UF(código)"=region,
         Ano=Time,
         Sexo=sex,
         "Nível educacional" = edu,
         "Grupo etário" = agest,
         População=pop,
         Nascimentos=births,
         Óbitos=deaths) %>% 
  select("UF(código)", Ano, Sexo, "Nível educacional","Grupo etário", População, Nascimentos, Óbitos)
write.csv(Proj_Brasil_v1, "../data/output/Proj_Brasil_v1.csv")
