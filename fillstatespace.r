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


if(F) {#newSSP
  # stop()
  ssp.var <- read.csv("../data/India SSP variants sub-national, V2.csv")
  setDT(ssp.var)
  ssp.var[variant=="national",variant:= "nat"]
  
  ssp.var <- ssp.var[variant=="new"]
  
  ssp.var[ssp==3&region=="IN.AN_urban"]
  
  
  temp.var <- data.frame(variant=rep("H",18),
                         ssp.adj = c(seq(1.05, 1.2, 0.05), seq(1.2125, 1.25, 0.0125), rep(1.25, 10)),
                         Time = seq(2011,2096,by=5))
  
  fert.var = rbind(temp.var,
                   temp.var%>%mutate(variant="L",ssp.adj = c(seq(0.95, 0.8, -0.05), seq(0.7875, 0.75, -0.0125), rep(0.75, 10))),
                   temp.var%>%mutate(variant="M",ssp.adj = rep(1, 18)))
  
  mort.var = list(h = -0.5,l = 0.5)
  
  intmig.var = rbind(temp.var%>%mutate(variant="H",ssp.adj = c(seq(1.125, 1.5, 0.125), rep(1.5, 14))),
                     temp.var%>%mutate(variant="L",ssp.adj = c(seq(0.875, 0.5, -0.125), rep(0.5, 14))),
                     temp.var%>%mutate(variant="M",ssp.adj = rep(1, 18)))
  
  reclass.var = rbind(temp.var%>%mutate(variant="H",ssp.adj = c(seq(1.125, 1.5, 0.125), rep(1.5, 14))),
                      temp.var%>%mutate(variant="L",ssp.adj = c(seq(0.875, 0.5, -0.125), rep(0.5, 14))),
                      temp.var%>%mutate(variant="M",ssp.adj = rep(1, 18)))

}#when we do SSPs, we will come here

  if(iscen=="baseline"){
    # How to prepare the education distribution for 2020?
    #?? For countries with no new baseline data, we start the projection from 2015
    ## but use the population distribution from 2020 (agest and sex) from wpp2019?
    ## When will the wpp2022 present?
    # id.cols <- c(names(popdt)[1:5],"tob") #time of birth
    
##Brazil data
# popdt -------------------------------------------------------------------
id.cols <- names(popdt)[1:5]
  
# base-year population to be update to 2015
#popdt
data1<-read_csv("../data/population/brazil_pop_2010.csv")
data1 %<>%
  separate(age, c("agest", "end")) %>% mutate(agest = as.numeric(agest)) %>% 
  mutate(edu = case_when(agest==0~"e1",
                         education=="No Education"~"e1",
                         education=="Incomplete Primary"~"e2",
                         education=="Primary"~"e3",
                         education=="Lower Secondary"~"e4",
                         education=="Upper Secondary"~"e5",
                         education=="Post Secondary"~"e6")) %>% 
  mutate(sex= case_when(sex=="Female"~"f", sex=="Male"~"m")) %>% 
  mutate(Time = 2010) %>%
  rename(region = area) %>% mutate(region = as.character(region)) %>% 
  select(region, Time, sex, edu, agest, pop)
popdt<-full_join(popdt, data1)
#check
# popdt[agest==20]
write_csv(popdt,"../data/population/popdt_filled.csv")


# sxdt --------------------------------------------------------------------
##eduspecific nsx
 
#eduspecific nsx v2 
sx_input<-read_csv("../data/mortality/mortality_brazil_nSx.csv")
setDT(sx_input)
# sx_input[sex=="male"&area=="RO"&period==2010&scenario==2&edu=="e1"]
sx_input[age==0,age:=-5][age==1,age:=0]
# sx_input[sex=="male"&area=="RO"&period==2010&scenario==2&edu=="e1"]

sx_input %<>%
  mutate(region = case_when(area=="RO"~11,area=="AC"~12,area=="AM"~13,area=="RR"~14,area=="PA"~15,area=="AP"~16,area=="TO"~17,
                            area=="MA"~21,area=="PI"~22,area=="CE"~23,area=="RN"~24,area=="PB"~25,area=="PE"~26,area=="AL"~27,area=="SE"~28,area=="BA"~29,
                            area=="MG"~31,area=="ES"~32,area=="RJ"~33,area=="SP"~35,
                            area=="PR"~41,area=="SC"~42,area=="RS"~43,
                            area=="MS"~50,area=="MT"~51,area=="GO"~52,area=="DF"~53)) %>% 
  mutate(sex= case_when(sex=="female"~"f", sex=="male"~"m")) %>% 
  rename(Time = year) %>% rename(agest=age,sx=nSx) %>% 
  mutate(region = as.character(region)) %>% 
  mutate(e1="e1",e2="e2",e3="e3",e4="e4",e5="e5",e6="e6") %>% 
  pivot_longer(cols = (e1:e6), names_to = "edu", values_to = "edu2")%>% 
  select(region, Time, sex, edu, agest, sx)
sxdt <- sx_input

setDT(sxdt)
# asfrdt ------------------------------------------------------------------

asfr_input<-read_csv("../data/fertility/ASFR2010_2060.csv")
asfr_input %<>%
  separate(age, c("agest", "end")) %>% mutate(agest = as.numeric(agest)) %>% 
  rename(Time = period) %>% 
  mutate(region = as.character(area)) %>% 
  mutate(asfr=asfr*1000) %>% 
  select(region, Time, edu, agest, asfr)

asfrdt<-full_join(asfrdt,asfr_input)
# asfrdt[region==11&Time==2010&edu=="e1"][,sum(asfr)/200]#tfr
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
prop_input<-read_csv("../data/education/education2010_60.csv")
    prop_input %<>%
  filter(scenario==2) %>% 
  mutate(sex= case_when(sex=="Female"~"f", sex=="Male"~"m")) %>% 
  rename(agest=age) %>% mutate(agest = as.numeric(agest)) %>% 
  rename(Time = year) %>% 
  rename(edu = education) %>% 
  mutate(region = as.character(area),prjpropfinal =prjpropfinal/100 ) %>% 
  select(region, Time, sex, edu, agest, prjpropfinal)%>%
      rename(prop=prjpropfinal)
setDT(prop_input)

# prop_input[,by=.(region,Time,sex,agest),sum(prjpropfinal)][V1!=1][,V1]

propdt<-full_join(propdt,prop_input)


write_csv(propdt,"../data/education/propdt_filled.csv")


# migration ---------------------------------------------------------------
mig_input<-read_csv("../data/migration/full_mig_2060.csv")

mig_input %<>%
  mutate(region = case_when(area=="RO"~11,area=="AC"~12,area=="AM"~13,area=="RR"~14,area=="PA"~15,area=="AP"~16,area=="TO"~17,
                            area=="MA"~21,area=="PI"~22,area=="CE"~23,area=="RN"~24,area=="PB"~25,area=="PE"~26,area=="AL"~27,area=="SE"~28,area=="BA"~29,
                            area=="MG"~31,area=="ES"~32,area=="RJ"~33,area=="SP"~35,
                            area=="PR"~41,area=="SC"~42,area=="RS"~43,
                            area=="MS"~50,area=="MT"~51,area=="GO"~52,area=="DF"~53)) %>% 
  separate(age, c("agest", "end")) %>% mutate(agest = as.numeric(agest)) %>%
  separate(year, c("Time", "endtime")) %>% mutate(Time = as.numeric(Time)) %>%
  mutate(edu = case_when(education=="No Education"~"e1",
                         education=="Incomplete Primary"~"e2",
                         education=="Primary"~"e3",
                         education=="Lower Secondary"~"e4",
                         education=="Upper Secondary"~"e5",
                         education=="Post Secondary"~"e6"))%>% 
  mutate(sex= case_when(sex=="Female"~"f", sex=="Male"~"m"))%>% 
  mutate(region = as.character(region)) %>% 
  select(region, Time, sex, edu, agest, prop_in, prop_out, inmigfinal, outmigfinal)

idmrdt <- full_join(idmrdt, mig_input) %>% #ATTENTION: PROPORTIONS OF MIGRANTS RELATED TO THE TOTAL OF THE AREA. NOT MIGRATION RATES!
  select(region, Time, sex, edu, agest, prop_in, inmigfinal)
idmrdt[is.na(inmigfinal),inmigfinal:=0]

odmrdt <- full_join(odmrdt, mig_input) %>% #ATTENTION: PROPORTIONS OF MIGRANTS RELATED TO THE TOTAL OF THE AREA. NOT MIGRATION RATES!
  select(region, Time, sex, edu, agest, prop_out, outmigfinal)
odmrdt[is.na(outmigfinal),outmigfinal:=0]


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
