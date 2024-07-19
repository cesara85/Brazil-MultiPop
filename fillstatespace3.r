library(magrittr)
# Here, we start filling the empty cells. We will first fill it with the WIC2 values and assumptions and update the projection
# dttosave
# [1] "sxdt"   "asfrdt" "emrdt" "imrdt"  "idmrdt" (in) "odmrdt" (out) "popdt"  "propdt" "srbdt"  

### files with data
# read_csv("../data/population/popdt2.csv")
# read_csv("../data/mortality/sxdt2.csv")
# read_csv("../data/fertility/asfrdt2.csv")
# read_csv("../data/education/propdt2.csv")
# read_csv("../data/migration/idmrdt2.csv")
# read_csv("../data/migration/odmrdt2.csv")

if(iscen=="baseline"){
  # How to prepare the education distribution for 2020?
  #?? For countries with no new baseline data, we start the projection from 2015
  ## but use the population distribution from 2020 (agest and sex) from wpp2019?
  ## When will the wpp2022 present?
  # id.cols <- c(names(popdt)[1:5],"tob") #time of birth

##Brazil data
# popdt -------------------------------------------------------------------
id.cols <- names(popdt)[1:5]
  
# base-year population to be update to 2022 - census results
#popdt
data1<-read_csv("../data/population/brazil_pop_2022.csv")
data1 %<>% 
  group_by(region,agest,sex,edu) %>% 
  summarise(pop=sum(popedu)) %>% 
  mutate(Time = 2022,
         region = as.character(region)) %>%
  select(region, Time, sex, edu, agest, pop)

# adjust for WPP 2024  of Brazil
data2<-data1 %>% 
  group_by(agest,sex,Time) %>% 
  summarise(pop=sum(pop))
wpp2022brazil <- read_excel("../data/population/wpp2022brazil.xlsx")
pop_adj <- left_join(data2,wpp2022brazil, by = join_by(agest, sex))
setDT(pop_adj)
# pop_adj[,sum(pop.x)] 203,080,756
# pop_adj[,sum(pop.y)] 210,306,415
pop_adj[,prop:=pop.y/pop.x]
data1<-left_join(data1, pop_adj, by= join_by(agest,sex))
setDT(data1)
data1[,pop:=pop*prop]
# data1[,sum(pop)] 210306415
data1 %<>% 
  mutate(Time = 2022) %>% 
  select(region, Time, sex, edu, agest, pop)
popdt<-left_join(popdt, data1)
setDT(popdt)
# check
# popdt[Time==2022,sum(pop, na.rm=TRUE)]
# popdt[Time==2022&region=="35",by=.(edu),sum(pop)]

# write_csv(popdt,"../data/population/popdt_filled.csv")

# sxdt --------------------------------------------------------------------
##eduspecific nsx

sx_input<-read_csv("../data/mortality/full_mortality_2057v3.csv")
setDT(sx_input)
sx_input[age==0,age:=-5][age==1,age:=0][sx>=1,sx:=1]
sx_input[sex=="female",sex:="f"][sex=="male",sex:="m"]
sx_input[is.na(edu)&age%in%-5:10,edu:="e1"]

 # sx_input[area=="RO"&Time==2022&scenario==2&edu=="e1"]
sx_input %<>%
   mutate(region = case_when(area=="RO"~11,area=="AC"~12,area=="AM"~13,area=="RR"~14,area=="PA"~15,area=="AP"~16,area=="TO"~17,
                             area=="MA"~21,area=="PI"~22,area=="CE"~23,area=="RN"~24,area=="PB"~25,area=="PE"~26,area=="AL"~27,area=="SE"~28,area=="BA"~29,
                             area=="MG"~31,area=="ES"~32,area=="RJ"~33,area=="SP"~35,
                             area=="PR"~41,area=="SC"~42,area=="RS"~43,
                             area=="MS"~50,area=="MT"~51,area=="GO"~52,area=="DF"~53)) %>%
   rename(agest=age) %>%
   mutate(region = as.character(region)) %>%
   filter(scenario==3) %>% 
   select(region, Time, sex, edu, agest, sx)
sxdt <- left_join(sxdt,sx_input, by= join_by(region, Time, sex, edu, agest))

setDT(sxdt)

## sxdt[is.na(sx), sx := sxdt[edu == "e1", sx]]
sx_e1 <- sxdt[edu == "e1", .(sex, agest, region, Time, sx)]
sxdt[is.na(sx), sx := sx_e1[.SD, on = .(sex, agest, region, Time), x.sx]]

# asfrdt ------------------------------------------------------------------

asfr_input<-read_csv("../data/fertility/ASFR2022_2062.csv")
asfr_input %<>%
  filter(scenario==3) %>% 
  rename(agest=age) %>% mutate(agest = as.numeric(agest),
                               region = as.character(region)) %>%
  select(region, Time, edu, agest, asfr)

asfrdt<-full_join(asfrdt,asfr_input)
setDT(asfrdt)
setDT(asfr_input)

# asfr_input[region==11&Time==2022&edu=="e1"][,sum(asfr)/200]#tfr
write_csv(asfrdt,"../data/fertility/asfrdt_filled.csv")

    # #Note: for the first few periods, population by mother's edu is not available
    # #asfrs (7)
    # # asfrdt
    # input <- data1%>%rename(Time=period)%>%
    #             filter(var=="asfr")%>%
    #             gather(region,asfr,contains("_"))%>%select(-var)%>%
    #             mutate(sex=substr(sex,1,1),Time = Time+1)%>%
    #             rename(agest=age)
    # setDT(input)
    # head(input)
    # check
    # xx <- unique(input$region)
    # length(intersect(xx,regions))
    
    #fert ssp correction
    if(F){issp.fert.var <- ssp.var[ssp==substr(SSP.name,4,4),.(region,fert)]
    setnames(issp.fert.var,"fert", "variant")
    
    asfrdt[issp.fert.var,on=.(region),variant:=variant][
      fert.var,on=.(Time,variant),asfr:=asfr*ssp.adj][
        ,`:=`(variant=NULL)]     
    }

    #srb 
    #srbdt 
    srbdt <- srbdt[,srb:=1.05] #check this with the UN values
    
# propdt ------------------------------------------------------------------
prop_input<-read_csv("../data/education/education2022_65.csv")

prop_input %<>%
  filter(scenario==3) %>% 
  mutate(sex= case_when(sex=="Female"~"f", sex=="Male"~"m")) %>% 
  rename(agest=age, Time = year, edu = education) %>% 
  mutate(agest = as.numeric(agest),
         region = as.character(region),
         prop = prop/100 ) %>% 
  select(region, Time, sex, edu, agest, prop)%>%
  setDT(prop_input)

# prop_input[,by=.(region,Time,sex,agest),sum(prop)][V1!=1][,V1]

propdt<-full_join(propdt,prop_input)
setDT(propdt)
# propdt[,by=.(region,Time,sex,agest),sum(prop)][V1!=1][,V1]
# propdt[,by=.(region,Time,sex,agest),prop]

write_csv(propdt,"../data/education/propdt_filled.csv")


# migration ---------------------------------------------------------------
inmig_input<-read_csv("../data/migration/inmig_rates2022.csv")
outmig_input<-read_csv("../data/migration/outmig_rates2022.csv")

#ssp3
setDT(inmig_input)
setDT(outmig_input)
inmig_input %<>%
  mutate(region = as.character(area))%>%
  select(region,Time,sex,edu,agest,value)

outmig_input %<>% 
  mutate(region = as.character(area)) %>% 
  select(region,Time,sex,edu,agest,value)

cenário_alternativo <- 3
setDT(inmig_input)
# Replicar o valor do ano inicial para todos os anos
if (cenário_alternativo == 3) {
  # Obtendo os valores de outmigfinal e inmigfinal do ano inicial (2022)
  valores_iniciais <- inmig_input[Time == 2022, "value"]
  # Atribuindo esses valores para todos os anos
  inmig_input[, c("value") := lapply(valores_iniciais, rep, length.out = .N), by = .(Time)]
}

setDT(outmig_input)
# Replicar o valor do ano inicial para todos os anos
if (cenário_alternativo == 3) {
  # Obtendo os valores de outmigfinal e inmigfinal do ano inicial (2022)
  valores_iniciais <- outmig_input[Time == 2022, "value"]
  # Atribuindo esses valores para todos os anos
  outmig_input[, c("value") := lapply(valores_iniciais, rep, length.out = .N), by = .(Time)]
}

idmrdt <- left_join(idmrdt, inmig_input, by = join_by(region, Time, sex, edu, agest))
idmrdt[is.na(value),value:=0]
#idmrdt[,inmigfinal:=0] #Closed population

odmrdt <- left_join(odmrdt, outmig_input, by = join_by(region, Time, sex, edu, agest))
odmrdt[is.na(value),value:=0][value>=1,value:=1]
#odmrdt[,outmigfinal:=0] #Closed population

ls(pattern = "dt$")


write_csv(idmrdt,"../data/migration/idmrdt_filled.csv")
write_csv(odmrdt,"../data/migration/odmrdt_filled.csv")

# NON Baseline ------------------------------------------------------------


}  else {#end if baseline  
  stop("prepare #203")
  id.cols <- c(names(popdt)[1:5],"tob")
  # popdt - no change required
  
  # New Mort ----------------------------------------------------------------
  
  
  if(newmort){}#newmort
  
  # New Fert ----------------------------------------------------------------
  
  
  if(newfert) {
    print("asfrdt changes required")
  }#newfert  
  # srbdt - no change required
  
  # New Edu -----------------------------------------------------------------
  if(newedu) {
  }#newedu
  
  
  # New Mig -----------------------------------------------------------------
  
  
} #end if baseline  
