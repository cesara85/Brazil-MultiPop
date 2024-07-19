options(digits=4)
options(OutDec = ",")
library(gt)
#WCDE to compare
library(wcde)
Brazil_WCDE <- get_wcde(indicator = "pop",  
                   country_name = "Brazil", 
                   pop_age = "all", 
                   pop_sex = "both",
                   pop_edu = "six",
                   scenario = c(1,2,3))
setDT(Brazil_WCDE)

#total population
final[agest!=-5,by=.(Time),sum(pop)]
final[,by=.(Time,agest),sum(pop)] %>% spread(Time,V1) 
final[,by=.(Time,sex),sum(pop)] %>% spread(Time,V1)
final[sex=="f"&Time%in%2010:2025&agest%in%15:45,by=.(Time,edu),sum(pop)]%>% spread(edu,V1)  #multiples checks possibilities
Brazil_WCDE[scenario==2 & year%in%2010:2060,by=.(year),sum(pop)] #wcde
12680127/5
#births
final[Time==2022,by=.(Time,region),sum(births)] %>% spread(Time,V1)

final[,by=.(Time),sum(births)]
final[,by=.(Time),1000*sum(births)/5/sum(pop)]#CBR
finalssp[Time==2022&scenario==2,sum(births)/5] # 12,679,637 #by year = 2,535,927. # SINASC *15-49 (2,547,629)
finalssp[Time==2022&scenario==2&agest%in%15:45,by=.(agest),sum(births)/5]

#deaths
finalssp[,by=.(Time,scenario),sum(deaths)]#abs deaths
final[,by=.(Time),1000*sum(deaths)/5/sum(pop)]#CMR
finalssp[Time==2022&scenario==2,sum(deaths)/5] # 7,577,749 #by year = 1,515,550 # SIM 1,495,668
finalssp[Time==2022,by=.(agest,scenario),sum(deaths)/5]
final[Time==2032|Time==2037,by=.(agest,Time),sum(deaths)] %>% spread(Time,V1)


sim_2022 <- read.csv("data/mortality/sim_2022.txt", sep=";")
setDT(sim_2022)
sim_2022 %<>% separate(agest, into = c("agest", "drop")) %>% 
  mutate(agest=as.numeric(agest),
         agest=case_when(agest==0|agest==7|agest==28~-5,
                         agest==1~0,TRUE~agest),
         X2022=as.numeric(X2022)) %>% 
  group_by(agest) %>% summarise(X2022=sum(X2022,NA.RM=TRUE))

if (!"Time" %in% names(finalssp)) {
  stop("The 'Time' column does not exist in the 'finalssp' data table.")
}

# Summarize deaths for the year 2022
deaths_summary <- finalssp[Time <= 2022, .(deaths_sum = sum(deaths)/5), by = .(agest, scenario)]

# Ensure 'X2022' column exists in 'sim_2022'
if (!"X2022" %in% names(sim_2022)) {
  stop("The 'X2022' column does not exist in the 'sim_2022' data table.")
}

# Merge summarized deaths with 'sim_2022' on 'agest'
deathscomp <- merge(deaths_summary, sim_2022[, .(agest, X2022)], by = "agest")

# View the resulting data table
print(deathscomp)

deathscomp <- merge(finalssp[Time <= 2022, by = .(agest,scenario), sum(deaths)/5], sim_2022[by=. (agest, X2022)], by = "agest")

deathscomp[,by=.(agest,scenario),V1/X2022] %>% spread(scenario,V1)

#SCEN2  #12,948,187 SIM 12,479,256 =12948187/12479256 [1] 1.037577
#SCEN3  #13,381,292 SIM 12,479,256 =12948187/12479256 [1] 1.037577

#migration
final[,by=.(Time),sum(odom)]
final[,by=.(Time),sum(idom)]
final[,by=.(Time),sum(idom)]-final[,by=.(Time),sum(odom)]

final[,by=.(Time,region),sum(idom)] %>% spread(Time,V1) 
final[,by=.(Time,region),sum(odom)] %>% spread(Time,V1) 
final[,by=.(Time,region),sum(idom)-sum(odom)] %>% spread(Time,region,V1) 

final[Time=="2010",by=.(region),sum(idom)][order(-V1)] #top1 inmigration state
final[Time=="2010",by=.(region),sum(odom)][order(-V1)] #top1 outmigration state

final[Time=="2055",by=.(Time,region),sum(idom)-sum(odom)][order(-V1)] #net migration
%>% spread(Time,V1)


#eduprop
Brazil_WCDE_edu <- Brazil_WCDE[scenario==2& year%in%2010:2060,by=.(year,education),sum(pop)] #wcde
Brazil_WCDE_edu %>% group_by(year) %>% filter(year==2060) %>% 
  mutate(prop=V1/sum(V1)) %>% ungroup()
educheck<-final
setDT(educheck)
educheck[agest%in%-5:10,edu:="Abaixo de 15 anos"]
educheck<-educheck[,by=.(Time,edu,region),sum(pop)]
educheck %>% 
  group_by(Time) %>% 
  mutate(prop=100*V1/sum(V1)) %>%
  ungroup() %>% filter(edu=="e6") %>%  select(Time, edu, region, prop) %>% spread(Time,prop) %>% print(n=27)
educheck %>% 
  group_by(Time) %>% 
  mutate(prop=100*V1/sum(V1)) %>%
  ungroup() %>% filter(edu=="e6") %>%  select(Time, edu, region, prop) %>% spread(Time,prop) %>% print(n=27)

setDT(final3)
final3[Time==2060,by=.(Time,region),.(edu,pop/sum(pop))] %>% filter(edu=="Post Secondary") %>% spread(edu=="e6",V2)

#agepyramids and structure
Brazil_WCDE %<>% 
  mutate(age=case_when(age=="100+"~"100--999",TRUE~age)) %>% separate(age,c("age","age_end"))
setDT(Brazil_WCDE)
Brazil_WCDE[scenario==2 & year%in%2010:2060 & age>=60,by=.(year),sum(pop)]/Brazil_WCDE[scenario==2 & year%in%2010:2060,by=.(year),sum(pop)]
Brazil_WCDE[scenario==2 & year==2060 & age<=10,by=.(year),sum(pop)]/Brazil_WCDE[scenario==2 & year==2060,by=.(year),sum(pop)]

final[agest<=10,by=.(Time),sum(pop)]/final[agest!=-5,by=.(Time),sum(pop)]
setDT(final)
#2030, 2045, 2060
final %>% 
  filter(agest!=-5) %>%
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
  facet_wrap(~Time,dir="h")+
  geom_col()+geom_vline(xintercept=0)+scale_fill_manual(values = wic_col6)+scale_x_continuous(labels = abs)+
  labs (x = "População (em milhares)",y = "Grupo etário",fill = "Nível educacional") +
  theme(legend.title = element_text(size=11, face = "bold", hjust = 0.5), legend.text = element_text(size=10), legend.position = "bottom",legend.direction = "horizontal",
        axis.text.y = element_text(size=11),axis.title.y = element_text(size=11, margin = margin(r=5)),
        axis.title.x = element_text(size=11),axis.text.x = element_text(size=11),
        strip.text = element_text(face = "bold", size = 11),
        panel.grid.major = element_blank())+
  guides(fill = guide_legend(title.position = "top", label.hjust = 0))

#numbers in the title
final %>% filter(region==14&Time==2020&agest!=-5) %>%
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
  facet_wrap(~Time,dir="h")+
  geom_col()+geom_vline(xintercept=0)+scale_fill_manual(values = wic_col6)+scale_x_continuous(labels = abs)+
  labs (x = "População (em milhares)",y = "Grupo etário",fill = "Nível educacional", 
        title = paste("São Paulo > Projeção:", format(round(final[region==14&Time==2020&agest!=-5,sum(pop)/1000000],1),nsmall=1), "milhões", "/ Censo 2022: 44,4 milhões")) +
  theme(legend.title = element_text(size=11, face = "bold", hjust = 0.5), legend.text = element_text(size=10), legend.position = "bottom",legend.direction = "horizontal",
        axis.text.y = element_text(size=11),axis.title.y = element_text(size=11, margin = margin(r=5)),
        axis.title.x = element_text(size=11),axis.text.x = element_text(size=11),
        strip.text = element_text(face = "bold", size = 11),
        panel.grid.major = element_blank())+
  guides(fill = guide_legend(title.position = "top", label.hjust = 0))

#TFR - how many births by mother age?
#brazil
TFRcheck<-final[agest%in% 15:45,by=.(Time,agest),.(births=sum(births))]
TFRcheckw <- final[agest%in% 15:45 & sex =="f",by=.(Time,agest),.(popf=sum(pop))]
TFRcheck %<>% 
  mutate(popf = TFRcheckw$popf,
        asfr=births/5/popf) %>%  group_by(Time) %>% 
  reframe(TFR=sum(asfr)*5)# %>% ungroup()
setDT(TFRcheck)
TFRcheck[,by=.(Time),TFR]

TFRcheckedu<-final[agest%in%15:45,by=.(agest,Time,edu),.(births=sum(births))]
TFRcheckwedu <- final[agest%in% 15:45 & sex =="f",by=.(agest,Time,edu),.(popf=sum(pop))]
TFRcheckedu %<>% 
  mutate(popf = TFRcheckwedu$popf,
         asfr=ifelse(agest==15&edu== "e6",0, births/5/popf)) %>%  group_by(Time,edu) %>% 
  reframe(TFR=sum(asfr)*5) %>% filter(Time!=2060)
setDT(TFRcheckedu)
TFRcheckedu[edu=="e1",by=.(edu,Time),TFR] %>% spread(Time,TFR)
TFRcheckedu[,by=.(edu,Time),TFR] %>% spread(Time,TFR)
setDT(asfr_input)
asfr_input[edu=="e1"&Time%in%2010:2050,by=.(edu,Time,region),sum(asfr)/200] %>% spread(Time,V1)

#plot edutfr
TFRcheckedu %>% 
  ggplot(mapping = aes(x=Time, y=TFR, color = edu))+
 geom_point()+geom_line()

##life tables
#computing nmx with population, deaths and births
lt <- finalssp %>% select(region,Time,sex, edu,agest,scenario,pop,births,deaths) 
setDT(lt)
lt0 <- lt[agest<=-5&Time!=2062,by=.(Time,sex,agest,scenario),(sum(deaths)/sum(pop))]
lt1 <- lt[agest>=0&Time!=2062,by=.(Time,sex,agest,scenario),(sum(deaths)/5/sum(pop))]
lt<-  bind_rows(lt0,lt1) %>% rename(nmx=V1)

lt[agest==90,by=.(Time,sex,scenario),nmx] %>% spread(scenario,nmx)
     
#nax
lt %<>% 
  group_by(Time, sex,scenario) %>%
  mutate(n = c(diff(agest),0),
         nax = n/2,
         nax = ifelse(agest == -5 & sex == "f", ifelse (nmx >= 0.107, 0.35, 0.053 + 2.8 * nmx), nax),
         nax = ifelse(agest == -5 & sex == "m", ifelse (nmx >= 0.107, 0.33, 0.045 + 2.648 * nmx), nax),
         nax = ifelse(agest == max(agest), 1/nmx,  nax)) %>% ungroup()


# remaining life table variables
lt %<>% group_by(Time, sex,scenario) %>% 
  mutate(nqx = (n*nmx) / (1+(n-nax)*nmx),
         nqx = ifelse(agest == max(agest), 1, nqx),
         npx = 1-nqx,
         lx = 100000 * cumprod(lag(npx, default  = 1)),
         ndx = lx*nqx,
         Lx = (lx - ndx) * n +  ndx * nax,
         Tx = rev(cumsum(rev(Lx))),
         ex = Tx / lx) %>% 
  ungroup()
setDT(lt)
lt[agest==-5&Time!=2062,by=.(Time,sex,agest,scenario), ex] %>% spread(scenario,ex)
lt %>% filter(agest==-5) %>%  
  ggplot(mapping = aes(x=Time, y=ex,color=as.character(scenario),shape=sex))+
  geom_point()+geom_line(aes(group=interaction(scenario,sex)))

lt[,edu:="all"]

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
ltedu[agest==0,by=.(Time,sex,agest,edu,scenario), ex] %>% spread(sex,ex)

#wcde comparisons
Brazil_WCDE_e0 <- get_wcde(indicator = "e0",  
                        country_name = "Brazil", 
                        pop_age = "all", 
                        pop_sex = "both",
                        pop_edu = "six",
                        scenario = c(1,2,3))
ltedu[sex=="f"&agest==-5&Time%in%2010:2020,by=.(edu,Time),ex] 
ltedu %>% filter(agest==50) %>%  
  ggplot(mapping = aes(x=Time, y=ex, color=edu))+
  geom_point()+geom_line()+facet_wrap(~sex~scenario)
