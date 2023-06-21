options(scipen = 999)
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
Brazil_WCDE[scenario==2 & year%in%2010:2060,by=.(year),sum(pop)] #wcde
final[,by=.(Time,agest),sum(pop)] %>% spread(Time,V1) #total decrease from 2055 to 2060.
final[,by=.(Time,sex),sum(pop)] %>% spread(Time,V1) #total decrease from 2055 to 2060.
final[sex=="f",by=.(Time),sum(pop)]  #female or male
final[,by=.(Time),sum(pop)]
#births
final[,by=.(Time),sum(births)] #total decrease from 2055 to 2060.

#deaths
final[,by=.(Time),sum(deaths)]#abs deaths
final[,by=.(Time),1000*sum(deaths)/5/sum(pop)]#CMR

#eduprop
Brazil_WCDE_edu <- Brazil_WCDE[scenario==2& year%in%2010:2060,by=.(year,education),sum(pop)] #wcde
Brazil_WCDE_edu %>% group_by(year) %>% filter(year==2060) %>% 
  mutate(prop=V1/sum(V1)) %>% ungroup()
educheck<-final
educheck[agest%in%-5:10,edu:="Under 15"]
educheck<-educheck[,by=.(Time,edu),sum(pop)]
educheck %>% group_by(Time) %>%  filter(Time==2060) %>% 
  mutate(prop=V1/sum(V1)) %>% ungroup()

#agepyramids 2030, 2045, 2060
final %>% filter(Time==2060) %>% 
  mutate(pop_pyramid = ifelse(sex == "m",
                              -pop, pop)) %>% 
  ggplot(mapping = aes(x=pop_pyramid, y=as.factor(agest), fill=edu))+
  geom_col()+geom_abline(intercept=0)+scale_fill_manual(values = wic_col6)

#TFR - how many births by mother age?
#brazil
TFRcheck<-final[agest%in% 15:45,by=.(Time,agest),.(births=sum(births))]
TFRcheckw <- final[agest%in% 15:45 & sex =="f",by=.(Time,agest),.(popf=sum(pop))]
TFRcheck %<>% 
  mutate(popf = TFRcheckw$popf,
        asfr=births/5/popf) %>%  group_by(Time) %>% 
  mutate(TFR=sum(asfr)*5) %>% ungroup()

##life tables
#computing nmx with population, deathss and births
lt <- final %>% select(region,Time,sex, edu,agest,pop,births,deaths) %>% filter(Time!=2060) %>% 
  setDT(lt)
lt[agest==0,agest:=1][agest==-5,agest:=0]
      #ltbirths <- lt[,by=.(Time,sex),.(births=sum(births))][,agest:=0]
      #ltbirthsdeaths <- lt[agest==0,by=.(Time,sex),.(deaths=sum(deaths))]
      #ltbirths<-ltbirths[,nmx:=(ltbirthsdeaths$deaths/births)]
lt<-lt[,by=.(Time,sex,agest),.(nmx=sum(deaths)/5/sum(pop))] #general population
      #lt<-lt[agest==0,.nmx:=ltbirths$nmx] #0 age - not necessary?

#nax
lt %<>% 
  group_by(Time, sex) %>%
  mutate(n = c(diff(agest),0),
         nax = n/2,
         nax = ifelse(agest == 0 & sex == "f", 
                      ifelse (nmx >= 0.107, 0.35, 0.053 + 2.8 * nmx), nax),
         nax = ifelse(agest == 0 & sex == "m",
                      ifelse (nmx >= 0.107, 0.33, 0.045 + 2.648  * nmx), nax),
         nax = ifelse(agest == max(agest), 1/nmx,  nax)) %>% ungroup()

# remaining life table variables
lt %<>% group_by(Time, sex) %>% 
  mutate(nqx = (n*nmx) / (1+(n-nax)*nmx),
         nqx = ifelse(agest == max(agest), 1, nqx),
         npx = 1-nqx,
         lx = 100000 * cumprod(lag(npx, default  = 1)),
         ndx = lx*nqx,
         Lx = (lx - ndx) * n +  ndx * nax,
         Tx = rev(cumsum(rev(Lx))),
         ex = Tx / lx) %>% 
  ungroup()

##life tables by edu level
#computing nmx with population, deathss and births
ltedu <- final %>% select(region,Time,sex, edu,agest,pop,births,deaths) %>% filter(Time!=2060) %>% 
  setDT(ltedu)
ltedu[agest==0,agest:=1][agest==-5,agest:=0]
#ltbirths <- lt[,by=.(Time,sex),.(births=sum(births))][,agest:=0]
#ltbirthsdeaths <- lt[agest==0,by=.(Time,sex),.(deaths=sum(deaths))]
#ltbirths<-ltbirths[,nmx:=(ltbirthsdeaths$deaths/births)]
ltedu<-ltedu[,by=.(Time,sex,agest,edu),.(pop=sum(pop),deaths=sum(deaths),nmx=sum(deaths)/5/sum(pop))] #general population
ltedu %<>% select(Time, sex, agest, edu, nmx) %>% # fill NAs of edu groups with the closest group - in groups with less than 15 years.
  pivot_wider(names_from = edu, values_from = nmx)%>% 
  mutate(e2 = case_when(is.na(e2)~e1, TRUE~e2),
         e3 = case_when(is.na(e3)~e2, TRUE~e3),
         e4 = case_when(is.na(e4)~e3, TRUE~e4),
         e5 = case_when(is.na(e5)~e4, TRUE~e5),
         e6 = case_when(is.na(e6)~e5, TRUE~e6),)
ltedu %<>% pivot_longer(cols= (e1:e6), values_to = "nmx", names_to = "edu")

ltedu[agest==0&Time==2050,by=.(Time,sex,edu),nmx]
#nax
ltedu %<>% 
  group_by(Time, sex,edu) %>%
  mutate(n = c(diff(agest),0),
         nax = n/2,
         nax = ifelse(agest == 0 & sex == "f", 
                      ifelse (nmx >= 0.107, 0.35, 0.053 + 2.8 * nmx), nax),
         nax = ifelse(agest == 0 & sex == "m",
                      ifelse (nmx >= 0.107, 0.33, 0.045 + 2.648  * nmx), nax),
         nax = ifelse(agest == max(agest), 1/nmx,  nax)) %>% ungroup()

# calculate remaining life table columns 
ltedu %<>% group_by(Time, sex,edu) %>% 
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
#wcde comparisons
Brazil_WCDE_e0 <- get_wcde(indicator = "e0",  
                        country_name = "Brazil", 
                        pop_age = "all", 
                        pop_sex = "both",
                        pop_edu = "six",
                        scenario = c(1,2,3))
ltedu[sex=="f"&agest==0&Time%in%2010:2020,by=.(edu,Time),ex] 
ltedu %>% filter(agest==0) %>%  
  ggplot(mapping = aes(x=Time, y=ex, color=edu))+
  geom_point()+geom_line()+facet_wrap(~sex)


sx_input<-read_csv("../data/mortality/full_mortality_2010-2060.csv")
setDT(sx_input)
# sx_input[sex=="male"&area=="RO"&period==2010&scenario==2&edu=="e1"]
sx_input[age==0,age:=-5][age==1,age:=0]
# sx_input[sex=="male"&area=="RO"&period==2010&scenario==2&edu=="e1"]

sx_input %<>%
  filter(scenario == 2) %>% 
  mutate(region = case_when(area=="RO"~11,area=="AC"~12,area=="AM"~13,area=="RR"~14,area=="PA"~15,area=="AP"~16,area=="TO"~17,
                            area=="MA"~21,area=="PI"~22,area=="CE"~23,area=="RN"~24,area=="PB"~25,area=="PE"~26,area=="AL"~27,area=="SE"~28,area=="BA"~29,
                            area=="MG"~31,area=="ES"~32,area=="RJ"~33,area=="SP"~35,
                            area=="PR"~41,area=="SC"~42,area=="RS"~43,
                            area=="MS"~50,area=="MT"~51,area=="GO"~52,area=="DF"~53)) %>% 
  mutate(sex= case_when(sex=="female"~"f", sex=="male"~"m")) %>% 
  rename(Time = period) %>% rename(agest=age,sx=nSx) %>% 
  mutate(region = as.character(region)) %>% 
  select(region, Time, sex, edu, agest, sx)

sxdt<-full_join(sxdt,sx_input)
sxdt %<>%
  mutate(sx = case_when(sx>=1~1.0000, TRUE~sx))

write_csv(sxdt,"../data/mortality/sxdt_filled.csv")