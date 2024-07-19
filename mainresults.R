pacman::p_load(readxl,tidyverse, reshape2, wcde, ggforce, flextable, purrr,magrittr,data.table,gt)
pacman::p_load(sf,geobr,RColorBrewer, viridis,ggrepel,ggspatial)

brazil_states<-read_state(code_state="all", year=2010)
brazilcolors <-(c("#2166AC","#67A9CF","#D1E5F0","#FDDBC7","#EF8A62","#B2182B","grey"))
IBGE<-read.csv("../data/output/IBGE_projection.csv")

#### Results

###1 Births
###2 Deaths
###3 total population
###4 Age structure
###5 life expectancy
###6 fertility rates
###7 migration
###8 Main results table


## File with 3 scenarios
final1$scenario<-1
final2$scenario<-2
final3$scenario<-3
finalssp<-bind_rows(final1,final2,final3)
setDT(finalssp)
finalssp %<>%
  mutate(area= case_when(region==11~"RO",region==12~"AC",region==13~"AM",region==14~"RR",region==15~"PA",region==16~"AP",
                         region==17~"TO",region==21~"MA",region==22~"PI",region==23~"CE",region==24~"RN",region==25~"PB",
                         region==26~"PE",region==27~"AL",region==28~"SE",region==29~"BA",region==31~"MG",region==32~"ES",
                         region==33~"RJ",region==35~"SP",region==41~"PR",region==42~"SC",region==43~"RS",region==50~"MS",region==51~"MT",region==52~"GO",region==53~"DF"))

#1 births
finalssp[,by=.(Time,scenario),sum(births)] %>% spread(Time,V1)
finalssp[,by=.(Time,scenario),1000*sum(births)/5/sum(pop)] %>% spread(Time,V1)#CBR
finalssp[Time==2022&scenario==2,sum(births)/5] # 12,679,637 #by year = 2,537,255 # SINASC *15-49 (2,547,629)
finalssp[Time==2022&scenario==2&agest%in%15:45,by=.(agest),sum(births)/5]

#2 deaths
finalssp[,by=.(Time,scenario),sum(deaths)]#abs deaths
finalssp[,by=.(Time,scenario),1000*sum(deaths)/5/sum(pop)] %>% spread(Time,V1)#CMR
finalssp[Time==2022&scenario==2,sum(deaths)/5] # 8,730,313 #by year = 1,746,063 # SIM 1,495,668
finalssp[Time==2022,by=.(agest,scenario),sum(deaths)/5]

#3 total population
yearpop<-finalssp[agest!=-5,by=.(Time,scenario),sum(pop)] %>% spread(scenario,V1)
finalssp[agest>=65,by=.(Time,scenario),scenario,sum(pop)]/finalssp[agest!=-5,by=.(Time,scenario),scenario,sum(pop)]
finalssp[, .(sum_65 = sum(pop[agest >= 65]),sum_pop = sum(pop[agest != -5])),by = .(Time, scenario)][,scenario,Time,sum_65/sum_pop]
finalssp[, .(sum_65 = sum(pop[agest >= 65]),sum_pop = sum(pop[agest != -5])), by = .(Time, scenario)][, proportion := sum_65 / sum_pop]

final4<-finalssp %>% filter(agest!=-5) %>%
  mutate(edu=case_when(agest<=10~"Under 15",TRUE~edu)) %>% 
  group_by(Time,edu,scenario) %>% 
  summarise(pop=sum(pop))
setDT(final4)

#graph of total population by edu att
max_time <- final4[, .(total_pop = sum(pop)), by = .(Time, scenario)][order(-total_pop), .SD[1], by = scenario]
final4 <- merge(final4, max_time[, .(scenario, max_time = Time)], by = "scenario", all.x = TRUE)

totalpopulation <- final4 %>%
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
  geom_vline(aes(xintercept = max_time), linetype = "dashed", color = "red")
totalpopulation
ggsave("plots/totalpopulation.png", totalpopulation, width = 10, height = 6, dpi = 300)

#Maps
no_axis <- theme(title = element_text (size=9),
                 legend.title = element_text(size=12),
                 legend.position = "bottom",
                 axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

#Data - population growth
popgrowth <- finalssp[agest != -5 & Time %in% c(2022, 2042, 2062), 
                      .(population = sum(pop)), 
                      by = .(Time, region, scenario)] %>%
  spread(Time, population, sep = "") %>% 
  mutate("2022-42"=(((Time2042/Time2022)^(1/20))-1)*100,
         "2042-62"=(((Time2062/Time2042)^(1/20))-1)*100) %>% 
  rename(code_state=region) %>% mutate(code_state=as.numeric(code_state)) %>% 
  pivot_longer(cols = c("2022-42","2042-62"), names_to = "Time",values_to = "Annual_growth")

popgrowth %<>% mutate(prop_group = cut(Annual_growth, 
                                       breaks = c(-Inf, -0.2, 0, 0.2, 0.4, 0.8, Inf),
                                       labels = c("-0.33 to -0.2%", "-0.2 to 0%", "0 to 0.2%", "0.2 to 0.4%", "0.4 to 0.8%", "0.8 to 1.6%")))
popgrowth <-left_join(popgrowth, brazil_states)
setDT(popgrowth)
popgrowth[Time=="2042-62"&scenario==1,by=.(Time,scenario,name_state),Annual_growth] %>% arrange(desc(Annual_growth))

# Map
ggplot(popgrowth) +
  geom_sf(aes(fill = Annual_growth, geometry = geom)) +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "blue", midpoint = 0) +
  facet_grid(Time~scenario) +
  labs(fill = "Annual growth (%)") +
  theme_minimal()
# label for states
popgrowth <- popgrowth %>%
  mutate(facet_label = if_else(Time == "2022-42" & scenario == 1, 
                               abbrev_state, NA))

ggplot(popgrowth) +
  geom_sf(aes(fill = Annual_growth, geometry = geom)) +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "blue", midpoint = 0) +
  facet_grid(Time ~ scenario) + 
  geom_label_repel(aes(label = facet_label, geometry = geom),
                   stat = "sf_coordinates",
                   min.segment.length = 0,
                   size = 3,              # Ajuste o tamanho do texto
                   alpha = 0.7,           # Ajuste a transparência
                   colour = "black",      # Cor do texto
                   label.size = 0.1,      # Tamanho da borda do rótulo
                   label.r = unit(0.15, "lines"), # Raio da borda
                   box.padding = 0.1,     # Espaçamento do rótulo
                   point.padding = 0.1,   # Espaçamento do ponto
                   segment.color = "black") + # Cor da linha do segmento
  labs(fill = "Annual growth (%)",
       x="",y="") +
  theme_minimal()+
  theme(legend.title = element_text(size=11, face = "bold", hjust = 0.5), 
        legend.text = element_text(size=10),
        legend.position = "bottom",legend.direction = "horizontal",
        axis.text.y = element_blank(),
        axis.text.x = element_blank())
  theme_minimal()

ggplot(popgrowth) +
  geom_sf(aes(fill = prop_group, geometry = geom), color = "black") +
  facet_grid(~scenario~Time) +
  scale_fill_manual(values = c("-0.33 to -0.2%" = "darkred", 
               "-0.2 to 0%" = "red",
               "0 to 0.2%" = "#deebf7",
               "0.2 to 0.4%" = "#9ecae1",
               "0.4 to 0.8%" = "#6baed6",
               "0.8 to 1.6%" = "#3182bd"), drop = FALSE) +
  theme(legend.title = element_text(size = 11, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.position = "right",
    legend.box.spacing = unit(0.1, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank()) +
  coord_sf()

#4 Age structure
finalssp %>% 
  filter((agest!=-5&Time==2022&scenario==2)|(agest!=-5&Time==2062)) %>%
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
  facet_wrap(~Time~scenario,nrow = 1)+
  geom_col()+geom_vline(xintercept=0)+scale_fill_manual(values = wic_col6)+scale_x_continuous(labels = abs)+
  labs (x = "Population (million)",y = "Age group",fill = "Educational attainment") +
  theme(legend.title = element_text(size=11, face = "bold", hjust = 0.5), legend.text = element_text(size=10), legend.position = "bottom",legend.direction = "horizontal",
        axis.text.y = element_text(size=11),axis.title.y = element_text(size=11, margin = margin(r=5)),
        axis.title.x = element_text(size=11),axis.text.x = element_text(size=11),
        strip.text = element_text(face = "bold", size = 11),
        panel.grid.major = element_blank())+
  guides(fill = guide_legend(title.position = "top", label.hjust = 0))


# edu growth - e6
finalssp[agest!=-5&edu=="e6",by=.(Time,scenario),.(pop=sum(pop))] %>% spread(Time, pop)
finalssp[agest>=25&edu=="e6"&Time==2062,by=.(scenario),.(pop2022=sum(pop))]/finalssp[agest>=25&Time==2062,by=.(scenario),.(pop2022=sum(pop))]
finalssp[agest>=25&edu=="e6"&sex=="f",by=.(Time,scenario),.(pop2022=sum(pop))]/finalssp[agest>=25&sex=="f",by=.(Time,scenario),.(pop2022=sum(pop))]
finalssp[agest>=25&edu=="e6",by=.(Time,scenario),.(pope6=sum(pop))]
finalssp[agest>=25,by=.(Time,scenario),.(pop=sum(pop))]
finalssp[,region:=as.numeric(region)]

#growth betwwen 2022 and 2062 by scenario
edu2022 <- finalssp[agest>=25&Time==2022&edu=="e6",by=.(Time,region,scenario,edu),.(pop2022=sum(pop))]
edu2062 <- finalssp[agest>=25&Time==2062&edu=="e6",by=.(Time,region,scenario,edu),.(pop2062=sum(pop))]
e6growth <-left_join(edu2022, edu2062, by=join_by(region,scenario))%>%  
  mutate(prop=(pop2062/pop2022)-1) %>% 
  rename(code_state=region) %>% mutate(code_state=as.numeric(code_state))
e6growth <-left_join(e6growth, brazil_states)
e6growth %>% 
  ggplot(aes(fill=100*prop,geometry=geom))+
  geom_sf()+
  facet_wrap(~scenario,ncol=3)+
  scale_fill_viridis(option= "magma", direction=-1, 
                     name = "Percentual growth of population with Post-secondary Education (2022-2062)",
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5))+
  theme_minimal()+
  theme(legend.position="bottom")+ no_axis
setDT(e6growth)
e6growth[,by=.(name_state,scenario),(v1=prop*100)][order(V1)] %>% spread(scenario,V1)
setDT(finalssp)
#growth betwwen 2022 and 2062 by region
Alledugrowth <- finalssp %>% filter(agest>=25) %>% 
  group_by(region,area,Time,edu,scenario) %>% mutate(pop1=sum(pop)) %>% 
  select(region,area,Time,edu,scenario,pop,pop1) %>% 
  rename(code_state=region) %>% mutate(code_state=as.numeric(code_state))
Alledugrowth <-left_join(Alledugrowth, brazil_states)
Alledugrowth %<>%
  group_by(name_region,Time,edu,scenario) %>% summarise(pop=sum(pop1)) %>% 
  group_by(name_region,Time,scenario) %>% mutate(poptotal=sum(pop),
                                                 prop=pop/poptotal)
#by region and scenario
Alledugrowth %>% 
  ggplot(aes(x=Time,y=100*prop,fill=edu))+
  geom_col()+
  facet_grid(~name_region~scenario)

#6 fertility rates
#brazil tfr
IBGE_tfr<-IBGE %>% filter(region=="Brasil") %>%  
  select(Year,tfr_ibge) %>% mutate(edu="IBGE",scenario=2) %>%
  rename(Time=Year,TFR=tfr_ibge) 

TFRcheck<-finalssp %>% 
  filter(agest%in% 15:45&Time!=2062) %>% 
  group_by(Time,agest,scenario) %>% summarise(births=sum(births))

TFRcheckw <- finalssp %>% 
  filter(agest%in% 15:45 & sex =="f"&Time!=2062) %>% 
  group_by(Time,agest,scenario) %>% summarise(popf=sum(pop))
tfr<-left_join(TFRcheck,TFRcheckw) %>% 
  mutate(asfr = births/5/popf) %>%  group_by(Time,scenario) %>% 
  reframe(TFR=sum(asfr)*5) 
setDT(tfr)
tfr[,edu:="All groups"]
tfr[,by=.(Time,scenario),TFR] %>% spread(scenario,TFR)

#brazil tfr by edu
TFRcheckedu<-finalssp %>%  
  filter(agest%in% 15:45&Time!=2062) %>% 
  group_by(Time,agest,scenario,edu) %>% summarise(births=sum(births))

TFRcheckwedu<-finalssp %>%  
  filter(agest%in% 15:45 & sex =="f"&Time!=2062) %>% 
  group_by(Time,agest,scenario,edu) %>% summarise(popf=sum(pop))

tfredu<-left_join(TFRcheckedu,TFRcheckwedu) %>% 
  mutate(asfr=ifelse(agest==15&edu== "e6",0, births/5/popf)) %>%  group_by(Time,scenario,edu) %>% 
  reframe(TFR=sum(asfr)*5) 
setDT(tfredu)
tfredu[edu=="e6",by=.(edu,Time,scenario),TFR] %>% spread(Time,TFR)
tfredu[,by=.(edu,Time,scenario),TFR] %>% spread(Time,TFR)

#by edu
#plot overall tfr
tfr<-bind_rows(tfr,tfredu,IBGE_tfr) %>% filter(scenario!=3) %>%
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

#7 mortality life expectancy

##life tables
#computing nmx with population, deaths and births
ltbrazil <- finalssp %>% select(Time,sex, edu,agest,scenario,pop,births,deaths) 
setDT(ltbrazil)
lt0b <- ltbrazil[agest<=-5&Time!=2062,by=.(Time,sex,agest,scenario),(sum(deaths)/sum(pop))]
lt1b <- ltbrazil[agest>=0 &Time!=2062,by=.(Time,sex,agest,scenario),(sum(deaths)/5/sum(pop))]
ltb<-  bind_rows(lt0b,lt1b) %>% rename(nmx=V1)
ltb[agest==90,by=.(Time,sex,scenario),nmx] %>% spread(scenario,nmx)

#nax
ltb %<>% 
  group_by(Time, sex,scenario) %>%
  mutate(n = c(diff(agest),0),
         nax = n/2,
         nax = ifelse(agest == -5 & sex == "f", ifelse (nmx >= 0.107, 0.35, 0.053 + 2.8 * nmx), nax),
         nax = ifelse(agest == -5 & sex == "m", ifelse (nmx >= 0.107, 0.33, 0.045 + 2.648 * nmx), nax),
         nax = ifelse(agest == max(agest), 1/nmx,  nax)) %>% ungroup()
# remaining life table variables
ltb %<>% group_by(Time, sex,scenario) %>% 
  mutate(nqx = (n*nmx) / (1+(n-nax)*nmx),
         nqx = ifelse(agest == max(agest), 1, nqx),
         npx = 1-nqx,
         lx = 100000 * cumprod(lag(npx, default  = 1)),
         ndx = lx*nqx,
         Lx = (lx - ndx) * n +  ndx * nax,
         Tx = rev(cumsum(rev(Lx))),
         ex = Tx / lx) %>% 
  ungroup()
setDT(ltb)
ltb[agest==-5&Time==2057,by=.(Time,sex,agest,scenario),ex]
ltb %>% filter(agest==-5) %>%  
  ggplot(mapping = aes(x=Time, y=ex,color=sex,shape=as.character(scenario)))+
  geom_point()+geom_line(aes(group=interaction(sex,scenario)))
ltb[,edu:="all"]

##life tables by edu level
#computing nmx with population, deathss and births
ltedu <- finalssp %>% select(region,Time,sex, edu,agest,scenario,pop,births,deaths) %>% filter(Time!=2062) %>% 
  setDT(ltedu)
ltedu0 <- ltedu[agest=="-5",by=.(Time,sex,agest,edu,scenario),(sum(deaths)/sum(pop))]
ltedu1 <- ltedu[agest>=0,by=.(Time,sex,agest,edu,scenario),(sum(deaths)/5/sum(pop))]
ltedu<-  bind_rows(ltedu0,ltedu1) %>% rename(nmx=V1)

#ltedu<-ltedu[,by=.(Time,sex,agest,edu),.(pop=sum(pop),deaths=sum(deaths),nmx=sum(deaths)/5/sum(pop))] #general population
ltedu %<>% # fill NAs of edu groups with the closest group - in groups with less than 15 years.
  pivot_wider(names_from = edu, values_from = nmx)%>% 
  mutate(e2 = case_when(is.na(e2)~e1, TRUE~e2),
         e3 = case_when(is.na(e3)~e2, TRUE~e3),
         e4 = case_when(is.na(e4)~e3, TRUE~e4),
         e5 = case_when(is.na(e5)~e4, TRUE~e5),
         e6 = case_when(is.na(e6)~e5, TRUE~e6),)
ltedu %<>% pivot_longer(cols= (e1:e6), values_to = "nmx", names_to = "edu")
setDT(ltedu)
ltedu[agest==0&Time==2052,by=.(sex,edu),nmx]
#nax
ltedu %<>% 
  group_by(Time, sex,edu,scenario) %>%
  mutate(n = c(diff(agest),0),
         nax = n/2,
         nax = ifelse(agest == -5, n/10, nax),
         nax = ifelse(agest == max(agest), 1/nmx,  nax)) %>% ungroup()

# calculate remaining life table columns 
ltedu %<>% group_by(Time, sex,edu,scenario) %>% 
  mutate(nqx = (n*nmx) / (1+(n-nax)*nmx),
         nqx = ifelse(agest == max(agest), 1, nqx),
         npx = 1-nqx,
         lx = 100000 * cumprod(lag(npx, default  = 1)),
         ndx = lx*nqx,
         Lx = (lx - ndx) * n +  ndx * nax,
         Tx = rev(cumsum(rev(Lx))),
         ex = Tx / lx) %>% 
  ungroup()

setDT(ltedu)
ltedu[agest==0&Time==2057,by=.(Time,sex,agest,edu,scenario), ex] %>% spread(sex,ex)

#ibge data
IBGE_e0<-IBGE %>% filter(region=="Brasil",Year!=2062) %>%  
  select(Year,Male,Female) %>% mutate(edu="IBGE",scenario=2) %>% rename(Time=Year,m=Male,f=Female) %>% 
  pivot_longer(cols =c("m","f"), values_to = "ex", names_to = "sex")
#projections data
all_lt<-bind_rows(ltb,ltedu)

#joined data
ltIBGESSP<-all_lt %>% filter(agest==-5) %>% 
  select(Time,scenario,edu,sex,ex)
ltIBGESSP<-bind_rows(ltIBGESSP,IBGE_e0)

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
  geom_point(size=3)+geom_line(linewidth=1)+facet_wrap(~sex~scenario)+
  scale_color_manual(values = c("#2166AC","#67A9CF","#D1E5F0","#FDDBC7","#EF8A62","#B2182B","black", "green"))+
  labs (y = "Life expectancy(0)", color="Educational attainment")+
  theme_bw()+
  theme(legend.title = element_text(size=11, hjust = 0.5), legend.text = element_text(size=10), 
        legend.position = "bottom", legend.box.spacing = unit(0.1,"cm"),
        axis.text.y = element_text(size=11),axis.title.y = element_text(size=11, margin = margin(r=5)),
        axis.title.x = element_text(size=11),axis.text.x = element_text(size=11, angle = 45,vjust=0.5),
        strip.text = element_text(size = 11),
        panel.grid.major = element_blank())
e0
ggsave("e0.png", e0, width = 10, height = 6, dpi = 300)
#computing nmx with population, deaths and births by state
lt <- finalssp %>% select(region,area,Time,sex, edu,agest,scenario,pop,births,deaths) 
setDT(lt)
lt0 <- lt[agest<=-5&Time!=2062,by=.(region,area,Time,sex,agest,scenario),(sum(deaths)/sum(pop))]
lt1 <- lt[agest>=0&Time!=2062,by=.(region,area,Time,sex,agest,scenario),(sum(deaths)/5/sum(pop))]
lt<-  bind_rows(lt0,lt1) %>% rename(nmx=V1)
lt[agest==90,by=.(Time,sex,scenario),nmx]

#nax
lt %<>% 
  group_by(region,area,Time, sex,scenario) %>%
  mutate(n = c(diff(agest),0)
         
finalssp[area=="RS"&agest>=25&edu=="e6", by=.(Time,scenario),(V1=sum(pop))] %>% spread(scenario,V1)
finalssp[area=="RS"&agest>=65, by=.(Time,scenario),(V1=sum(pop))] %>% spread(scenario,V1)
