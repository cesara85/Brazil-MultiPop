## Survival rates for Brazilian states accordingly to education as in WIC projections (Brazil)
getwd()
library(readxl)
library(tidyverse)
library(reshape2)
rm(list = ls())

# arranging mortality tables in 3 steps
#1. read all mortality tables. Clean file to maintain just states data.
all_mort_tables <- lapply(excel_sheets("data/mortality/Tabuas_Mortalidade 2010-2060.xlsx"), read_excel, path = "data/mortality/Tabuas_Mortalidade 2010-2060.xlsx")
length(all_mort_tables)

#2. create 27 files, for each state. 1st is Brazil example. It will not be used.

#brazil 
mortality_tables_2010_60 <- read_excel("data/mortality/Tabuas_Mortalidade 2010-2060.xlsx") 
brz_mortality <- mortality_tables_2010_60 %>% 
  rename(age_group = (`BRASIL (2010-2060)`),
         male_npx = 5,
         female_npx = 15) %>% 
  select(c(1,5,15)) %>% #keep just relevant coluns
  mutate(area = "Brazil")%>% #define area
  filter(age_group != 'NA' & age_group != "Tábuas homens"  
         & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
         & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
         & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                          TRUE ~ NA)) #input for correct year variable
brz_mortality$n <- NULL 
brz_mortality <- melt(brz_mortality, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
  mutate(sex = case_when((variable == "male_npx")~"male",
                         TRUE~"female")) #create sex variable
brz_mortality$variable <- NULL

#11 rondonia
{
  mortality_ro <- all_mort_tables[[2]] %>% 
  rename(age_group = "Unidade da Federação:",
         male_npx = 5,
         female_npx = 15) %>% 
  select(c(1,5,15)) %>% #keep just relevant coluns
  mutate(area = "Rondonia")%>% #define area
  filter(age_group != 'NA' & age_group != "Tábuas homens"  
         & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
         & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
         & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                          TRUE ~ NA)) #input for correct year variable
mortality_ro$n <- NULL 
mortality_ro <- melt(mortality_ro, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
  mutate(sex = case_when((variable == "male_npx")~"male",
                         TRUE~"female")) #create sex variable
mortality_ro$variable <- NULL
}

#12 acre
{
  mortality_ac <- all_mort_tables[[3]] %>% 
  rename(age_group = "Unidade da Federação:",
         male_npx = 5,
         female_npx = 15) %>% 
  select(c(1,5,15)) %>% #keep just relevant coluns
  mutate(area = "Acre")%>% #define area
  filter(age_group != 'NA' & age_group != "Tábuas homens"  
         & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
         & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
         & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                          TRUE ~ NA)) #input for correct year variable
mortality_ac$n <- NULL 
mortality_ac <- melt(mortality_ac, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
  mutate(sex = case_when((variable == "male_npx")~"male",
                         TRUE~"female")) #create sex variable
mortality_ac$variable <- NULL
}
#13 amazonas
{
  mortality_am <- all_mort_tables[[4]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Amazonas")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_am$n <- NULL 
  mortality_am <- melt(mortality_am, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_am$variable <- NULL
}
#14 RR
{
  mortality_rr <- all_mort_tables[[5]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Roraima")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_rr$n <- NULL 
  mortality_rr <- melt(mortality_rr, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_rr$variable <- NULL
}
#15 pará
{
  mortality_pa <- all_mort_tables[[6]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Para")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_pa$n <- NULL 
  mortality_pa <- melt(mortality_pa, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_pa$variable <- NULL
}
# 16 ap
{
  mortality_ap <- all_mort_tables[[7]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Amapa")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_ap$n <- NULL 
  mortality_ap <- melt(mortality_ap, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_ap$variable <- NULL
}
## 17 to
{
  mortality_to <- all_mort_tables[[8]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Tocantins")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_to$n <- NULL 
  mortality_to <- melt(mortality_to, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_to$variable <- NULL
}
# 21 Ma
{
  mortality_ma <- all_mort_tables[[9]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Maranhao")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_ma$n <- NULL 
  mortality_ma <- melt(mortality_ma, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_ma$variable <- NULL
}
# 22 pi
{
  mortality_pi <- all_mort_tables[[10]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Piaui")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_pi$n <- NULL 
  mortality_pi <- melt(mortality_pi, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_pi$variable <- NULL
}
# 23 ce
{
  mortality_ce <- all_mort_tables[[11]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Ceara")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_ce$n <- NULL 
  mortality_ce <- melt(mortality_ce, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_ce$variable <- NULL
}
# 24 RN
{
  mortality_rn <- all_mort_tables[[12]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Rio Grande do Norte")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_rn$n <- NULL 
  mortality_rn <- melt(mortality_rn, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_rn$variable <- NULL
}
# 25 pb
{
  mortality_pb <- all_mort_tables[[13]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Paraiba")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_pb$n <- NULL 
  mortality_pb <- melt(mortality_pb, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_pb$variable <- NULL
}
# 26 pe
{
  mortality_pe <- all_mort_tables[[14]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Pernambuco")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_pe$n <- NULL 
  mortality_pe <- melt(mortality_pe, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_pe$variable <- NULL
}
# 27 al
{
  mortality_al <- all_mort_tables[[15]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Alagoas")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_al$n <- NULL 
  mortality_al <- melt(mortality_al, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_al$variable <- NULL
}
# 28 se
{
  mortality_se <- all_mort_tables[[16]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Sergipe")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_se$n <- NULL 
  mortality_se <- melt(mortality_se, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_se$variable <- NULL
}
#29 ba
{
  mortality_ba <- all_mort_tables[[17]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Bahia")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_ba$n <- NULL 
  mortality_ba <- melt(mortality_ba, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_ba$variable <- NULL
}
#31 MG
{
  mortality_mg <- all_mort_tables[[18]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Minas Gerais")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_mg$n <- NULL 
  mortality_mg <- melt(mortality_mg, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_mg$variable <- NULL
}
#32 ES
{
  mortality_es <- all_mort_tables[[19]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Espirito Santo")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_es$n <- NULL 
  mortality_es <- melt(mortality_es, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_es$variable <- NULL
}
#33 RJ
{
  mortality_rj <- all_mort_tables[[20]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Rio de Janeiro")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_rj$n <- NULL 
  mortality_rj <- melt(mortality_rj, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_rj$variable <- NULL
}
#35 sp
{
  mortality_sp <- all_mort_tables[[21]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "São Paulo")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_sp$n <- NULL 
  mortality_sp <- melt(mortality_sp, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_sp$variable <- NULL
}
#41 pr
{
  mortality_pr <- all_mort_tables[[22]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Parana")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_pr$n <- NULL 
  mortality_pr <- melt(mortality_pr, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_pr$variable <- NULL
}
#42 sc
{
  mortality_sc <- all_mort_tables[[23]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Santa Catarina")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_sc$n <- NULL 
  mortality_sc <- melt(mortality_sc, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_sc$variable <- NULL
}
#41 rs
{
  mortality_rs <- all_mort_tables[[24]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Rio Ggrande do Sul")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_rs$n <- NULL 
  mortality_rs <- melt(mortality_rs, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_rs$variable <- NULL
}

#50 ms
{
  mortality_ms <- all_mort_tables[[25]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Mato Grosso do Sul")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_ms$n <- NULL 
  mortality_ms <- melt(mortality_ms, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_ms$variable <- NULL
}
#51 mt
{
  mortality_mt <- all_mort_tables[[26]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Mato Grosso")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_mt$n <- NULL 
  mortality_mt <- melt(mortality_mt, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_mt$variable <- NULL
}
#52 go
{
  mortality_go <- all_mort_tables[[27]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Goias")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_go$n <- NULL 
  mortality_go <- melt(mortality_go, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_go$variable <- NULL
}#53 ms
{
  mortality_df <- all_mort_tables[[28]] %>% 
    rename(age_group = "Unidade da Federação:",
           male_npx = 5,
           female_npx = 15) %>% 
    select(c(1,5,15)) %>% #keep just relevant coluns
    mutate(area = "Distrito Federal")%>% #define area
    filter(age_group != 'NA' & age_group != "Tábuas homens"  
           & age_group != "Fonte: IBGE/Diretoria de Pesquisas. Coordenação de População e Indicadores Sociais. Gerência de Estudos e Análises da Dinâmica Demográfica."
           & age_group != "Projeção da população do Brasil e das Unidades da Federação, por sexo e idade."
           & age_group!= "Ano:" & age_group!= "Idade") %>% #exclude irrelevant rows
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
                            TRUE ~ NA)) #input for correct year variable
  mortality_df$n <- NULL 
  mortality_df <- melt(mortality_df, id = c("age_group", "year", "area"), value.name = "npx") %>% #long file
    mutate(sex = case_when((variable == "male_npx")~"male",
                           TRUE~"female")) #create sex variable
  mortality_df$variable <- NULL
}

#3. join files
mortality_brazil <- bind_rows(mortality_ac, mortality_al, mortality_am, mortality_ap, mortality_ba, mortality_ce, mortality_df, mortality_es, mortality_go, 
          mortality_ma, mortality_mg, mortality_ms, mortality_mt, mortality_pa, mortality_pb, mortality_pe, mortality_pi, mortality_pr,
          mortality_rj, mortality_rn, mortality_ro, mortality_rr, mortality_rs, mortality_sc, mortality_se, mortality_sp, mortality_to)


#4 check and plot results
mortality_brazil$npx <- as.numeric(mortality_brazil$npx)
mortality_brazil$age_group <- as.numeric(mortality_brazil$age_group) %>% 
  replace_na(90)
write_csv(mortality_brazil, "data/mortality/mortality_brazil.csv")

#sex and state - no common values
mortality_brazil %>%
  filter(year == 2010 & age_group == "0") %>% 
  ggplot()+
  geom_point(aes(x = reorder(area,-npx), 
                 y = npx,
                 color = sex),
             size = 3)+
  labs(title = 'mortality test0',
       y = 'npx',
       x = 'state',
       color = "sex")+
  theme_get()+
  theme(axis.text.x = element_text(angle=90, hjust=1))

#2010 and 2060
mortality_brazil %>%
  filter(year == 2010 | year == 2060) %>% 
  ggplot()+
  geom_point(aes(x = as.factor(age_group), 
                 y = npx,
                 color = area,
                 shape = as.factor(year)),
             size = 2)+
  facet_grid(~asex)+
  labs(title = 'mortality test0',
       y = 'npx',
       x = 'state',
       color = "sex")+
  theme_get()+
  theme(axis.text.x = element_text(angle=90, hjust=1))

#by states
mortality_brazil %>%
  filter(year == 2010 | year == 2060) %>% 
  ggplot()+
  geom_point(aes(x = as.factor(age_group), 
                 y = npx,
                 color = as.factor(year),
                 shape = sex),
             size = 2)+
  facet_wrap(~area)+
  labs(title = 'mortality test0',
       y = 'npx',
       x = 'age',
       color = "year")+
  theme_get()+
  theme(axis.text.x = element_text(angle=90, hjust=1))
