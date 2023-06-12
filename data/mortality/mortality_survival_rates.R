## Survival rates for Brazilian states accordingly to education as in WIC projections (Brazil)
getwd()
pacman::p_load(readxl,tidyverse, reshape2, wcde, ggforce, flextable, purrr)

rm(list = ls())

# extracting life tables from IBGE in 2 steps. 3rd step associate with WIC data.
#1st - creating fucntion to read multiple sheets, create year and area variable and clean data

#function to extract data from multiple sheets
clean_sheets <- function(sheet) {
    
    # take the characters of each sheet to extract area, skip first 7 rows 
    area <- str_sub(sheet)  
    
    mortality_table <- read_excel(
      "data/mortality/Tabuas_Mortalidade 2010-2060.xlsx", 
      sheet = sheet,
      skip = 7) %>% 
      mutate(area = area) %>% 
      rename(age_group = 1, male_nLx = 7, female_nLx = 17) %>% 
      select(c(1,7,17,30)) %>% #keep just relevant columns
      filter(age_group != 'NA' & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
             & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
             & age_group!= "Ano:" & age_group!= "Idade") %>% # delete unused rows
      mutate(n = as.numeric(1:1020)) %>% 
      mutate(year = case_when(n== (1:20)~ 2010, n== (21:40)~ 2011,
                              n== (41:60)~ 2012, n== (61:80)~ 2013,
                              n== (81:100)~ 2014, n== (101:120)~ 2015,
                              n== (121:140)~ 2016, n== (141:160)~ 2017,
                              n== (161:180)~ 2018, n== (181:200)~ 2019,
                              n== (201:220)~ 2020, n== (221:240)~ 2021,
                              n== (241:260)~ 2022, n== (261:280)~ 2023,
                              n== (281:300)~ 2024, n== (301:320)~ 2025,
                              n== (321:340)~ 2026, n== (341:360)~ 2027,
                              n== (361:380)~ 2028, n== (381:400)~ 2029,
                              n== (401:420)~ 2030, n== (421:440)~ 2031,
                              n== (441:460)~ 2032,
                              n== (461:480)~ 2033,
                              n== (481:500)~ 2034,
                              n== (501:520)~ 2035,
                              n== (521:540)~ 2036,
                              n== (541:560)~ 2037,
                              n== (561:580)~ 2038,
                              n== (581:600)~ 2039,
                              n== (601:620)~ 2040,
                              n== (621:640)~ 2041,
                              n== (641:660)~ 2042,
                              n== (661:680)~ 2043,
                              n== (681:700)~ 2044,
                              n== (701:720)~ 2045,
                              n== (721:740)~ 2046,
                              n== (741:760)~ 2047,
                              n== (761:780)~ 2048,
                              n== (781:800)~ 2049,
                              n== (801:820)~ 2050,
                              n== (821:840)~ 2051,
                              n== (841:860)~ 2052,
                              n== (861:880)~ 2053,
                              n== (881:900)~ 2054,
                              n== (901:920)~ 2055,
                              n== (921:940)~ 2056,
                              n== (941:960)~ 2057,
                              n== (961:980)~ 2058,
                              n== (981:1000)~ 2059,
                              n== (1001:1020)~ 2060,
                              TRUE ~ NA)) %>% select(-n) #input for correct year variable
    mortality_table <- melt(mortality_table, id = c("age_group", "year", "area"), value.name = "nLx") %>% #long file
      mutate(sex = case_when(variable == "male_nLx"~"male",
                             TRUE~"female")) %>% 
      mutate(nLx = as.numeric(nLx))%>%  select(-variable)
    mortality_table 
}

#vector of sheet names
states <- paste(c("Brasil","RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB","PE","AL","SE","BA","MG","ES","RJ","SP","PR","SC","RS","MS", "MT", "GO", "DF"))
states
#combining into one dataframe
mortality_brazil  <- map_df(states, clean_sheets)
mortality_brazil <- mutate(mortality_brazil, age_group = case_when(age_group=="90+"~"90", TRUE~age_group))

#2 calculate survival rates and add to a specific data_set
mortality_brazil_nSx <- mortality_brazil %>% 
  pivot_wider(names_from = age_group, values_from = nLx) %>% 
  mutate(nSx_0 = (`0`+`1`)/(5*100000)) %>% 
  mutate(nSx_1 = (`5`/(`0`+`1`))) %>% 
  mutate(nSx_5 = (`10`/(`5`))) %>% 
  mutate(nSx_10 = (`15`/(`10`))) %>% 
  mutate(nSx_15 = (`20`/(`15`))) %>% 
  mutate(nSx_20 = (`25`/(`20`))) %>% 
  mutate(nSx_25 = (`30`/(`25`))) %>% 
  mutate(nSx_30 = (`35`/(`30`))) %>% 
  mutate(nSx_35 = (`40`/(`35`))) %>% 
  mutate(nSx_40 = (`45`/(`40`))) %>% 
  mutate(nSx_45 = (`50`/(`45`))) %>% 
  mutate(nSx_50 = (`55`/(`50`))) %>% 
  mutate(nSx_55 = (`60`/(`55`))) %>% 
  mutate(nSx_60 = (`65`/(`60`))) %>% 
  mutate(nSx_65 = (`70`/(`65`))) %>% 
  mutate(nSx_70 = (`75`/(`70`))) %>%
  mutate(nSx_75 = (`80`/(`75`))) %>% 
  mutate(nSx_80 = (`85`/(`80`))) %>% 
  mutate(nSx_85 = (`90`/(`85`+`90`))) %>% 
  mutate(nSx_90 = nSx_85) %>% 
  select(year, area, sex, nSx_0:nSx_90) %>% 
  pivot_longer(cols = (nSx_0:nSx_90),
               names_to = "age",
               values_to = "nSx") %>% 
  separate(age, c("var","age"))

#write_csv(mortality_brazil_nSx, "data/mortality/mortality_brazil_nSx.csv")

#3rd aspect

#IBGE indicators for 2010-2060 by 5 years with reference levels estimation (related to Brazil)
mortality_brazil_nSx <- mortality_brazil_nSx %>% 
  filter(year %in% c("2010","2015","2020","2025","2030","2035","2040","2045","2050","2055","2060")) %>% 
  pivot_wider(names_from = area, values_from = nSx) %>% 
  pivot_longer(cols = RO:DF, names_to = "area", values_to = "nSx2") %>% 
  mutate(ref_level = nSx2/Brasil) %>% #proportional rate of age-year surv. of the state related to Brazil
  rename(period = year) %>% mutate(period = as.character(period)) %>% 
  mutate(scen1 = 1) %>% mutate(scen2 = 2) %>% mutate(scen3=3)%>% #scenario variable  
  pivot_longer(cols = c(scen1, scen2, scen3), values_to = "scenario") %>% select(-name) %>%  
  select(scenario, age, sex, period, area, ref_level) %>% 
  pivot_wider(names_from = area, values_from = "ref_level")

# WIC indicators - mortality
brazil_eassr_ssp <- get_wcde(indicator = "eassr", scenario = c(1,2,3), country_name = "Brazil") %>% 
  select(c(scenario, age, sex, education, period, eassr)) %>% 
  mutate(sex = case_when(sex == "Male"~ "male",sex == "Female"~"female")) %>%
  separate(age, into = c("age","age_end")) %>% select(-age_end) %>% 
  mutate(age = case_when(age == "Newborn"~"0", age == "0"~"1", TRUE ~ age)) %>% 
  mutate(eassr = eassr/100) %>% 
  filter(period %in% c("2015-2020","2020-2025","2025-2030","2030-2035","2035-2040",
                       "2040-2045","2045-2050","2050-2055","2055-2060","2060-2065"))
ssp_wider2010 <- pivot_wider(brazil_eassr_ssp, names_from = education, values_from = eassr) %>% 
  filter(period == "2015-2020") %>% 
  mutate(period = case_when(period=="2015-2020"~"2010-2015")) #calculating 2010-2015
ssp_wider <- pivot_wider(brazil_eassr_ssp, names_from = education, values_from = eassr) %>%
  add_row(ssp_wider2010) %>% 
  separate(period, c("period", "end")) %>% select(-end) %>% #every period - 2010-2060.
  filter(age!="95" & age!="100")


#join files to calculate specific mortality rates by educational attainment
#adjust to brz mortality data

#join IBGE and WIC data and calculate proportions
full_mortality <- full_join(mortality_brazil_nSx, ssp_wider) %>% 
  pivot_longer(cols = (RO:DF), names_to = "area", values_to = "ref_level") %>% 
  mutate(e1 = `No Education`*ref_level,
         e2 = `Incomplete Primary`*ref_level,
         e3 = `Primary`*ref_level,
         e4 = `Lower Secondary`*ref_level,
         e5 = `Upper Secondary`*ref_level,
         e6 = `Post Secondary`*ref_level) %>% 
  select(age, sex, period, scenario, area, e1:e6) %>% 
  pivot_longer(cols = (e1:e6), names_to = "edu", values_to = "nSx")

#write file
#write_csv(full_mortality, "data/mortality/full_mortality_2010-2060.csv")

#basic plots - mortality for 2030 and 2060.
full_mortality %>% 
  filter(period == "2030"|period =="2060") %>% 
  ggplot(mapping = aes(
    x = as.numeric(age),
    y = nSx,
    color = edu))+
  facet_grid(period~sex~scenario)+
  geom_point()+
  theme_bw()+
  labs(title = "Survival ratios by age, sex, SSP scenario and educational attainment",
    subtitle = "Brazil - 2030 and 2060.",
    x = "Age groups",
    color = "Ed. attainment",
    caption = "Own estimations with WIC(2018) and IBGE(2019)")+
  scale_x_continuous(n.breaks = 10) +
  theme(title = element_text (size=12),
      legend.title = element_text(size=11),
      legend.text = element_text(size=10),
      axis.title.x = element_text(size=10),
      axis.title.y = element_text(size=10))

ggsave("plots/nSx2030_60.jpg", width = 25, height = 25, units = "cm")

#newborns
full_mortality %>% 
  filter(age == "0"& period =="2060") %>% 
  ggplot(mapping = aes(
    x = fct_reorder(area, -nSx),
    y = nSx,
    color = scenario,
    shape = sex))+
  facet_wrap(~edu, dir = "v")+
  geom_point()+
  theme_bw()+
  labs(title = "Survival ratios by age, sex, SSP scenario and educational attainment",
       subtitle = "Brazil - 2030 and 2060.",
       x = "States",
       color = "Ed. attainment",
       caption = "Own estimations with WIC(2018) and IBGE(2019)")+
  theme(title = element_text (size=12),
        legend.title = element_text(size=11),
        legend.text = element_text(size=10),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10))

ggsave("plots/nSx2newborns.jpg", width = 25, height = 25, units = "cm")


