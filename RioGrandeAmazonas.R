# Rio Grande do Sul and Amazonas
options(digits = 3)
setDT(Mainresults)

library(officer)
library(flextable)
# Pacotes necessários
library(data.table)
library(dplyr)
library(officer)
library(flextable)
library(gt)

# Dados
RSAM <- Mainresults[area %in% c("AM", "RS") & Time %in% c("2022", "2042","2057","2062"), 
                    .(Totalpop = sum(Totalpop), e6 = sum(Edu6Proportion),
                      Under15 = sum(Under15Proportion), Above65 = sum(Above65Proportion),
                      TFR = sum(TFR), E0_Male=sum(E0_Male),E0_Female=sum(E0_Female)), 
                    by = .(area, scenario, Time)]

RS <- RSAM %>%
  fill(TFR,E0_Female,E0_Male) %>% 
  filter(Time!=2057 & area=="RS")

# Criar a tabela com gt
table <- gt(data = RS) %>%
  tab_header(
    title = "RS Table"
  ) %>%
  cols_label(
    area = "Area",
    scenario = "Scenario",
    Time = "Year",
    Totalpop = "Total Population(millions)",
    e6 = "Post-secondary Population(%)",
    Under15 = "Under 15 (%)",
    Above65 = "Above 65 (%)",
    TFR = "Total Fertility Rate",
    E0_Male = "Male life expectancy (e0)",
    E0_Female = "Female life expectancy (e0)"
  ) %>%
  tab_spanner(
    label = "Demographics",
    columns = vars(Totalpop, e6, Under15, Above65)
  ) %>%
  tab_spanner(
    label = "Vital Statistics",
    columns = vars(TFR, E0_Male, E0_Female)
  )

# Exibir a tabela
print(table)
gtsave(table, "RS_table.png")
# Age structure
RSag<-finalssp %>% filter(area=="RS") %>% 
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
  facet_wrap(~Time~scenario,nrow = 2)+
  geom_col()+geom_vline(xintercept=0)+scale_fill_manual(values = wic_col6)+scale_x_continuous(labels = abs)+
  labs (x = "Population (million)",y = "Age group",fill = "Educational attainment") +
  theme(legend.title = element_text(size=11, face = "bold", hjust = 0.5), legend.text = element_text(size=10), legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text.y = element_text(size=11),axis.title.y = element_text(size=11, margin = margin(r=5)),
        axis.title.x = element_text(size=11),axis.text.x = element_text(size=11),
        strip.text = element_text(face = "bold", size = 11),
        panel.grid.major = element_blank())+
  scale_y_discrete(breaks = scales::breaks_pretty(n=10))+
  guides(fill = guide_legend(title.position = "top", label.hjust = 0))
RSag
library(patchwork)
library(webshot)  # Para salvar a tabela como imagem
webshot::install_phantomjs()

table_img <- grid::rasterGrob(png::readPNG("RS_table.png"), interpolate = TRUE)

# Combinar o gráfico e a tabela
combined_plot <- (RSag / table_img) + plot_layout(ncol = 2, heights = c(2, 1))

# Exibir o gráfico combinado
print(combined_plot)

AM <- RSAM %>%
  fill(TFR,E0_Female,E0_Male) %>% 
  filter(Time!=2057 & area=="AM")

# Criar a tabela com gt
table <- gt(data = AM) %>%
  tab_header(
    title = "AM Table"
  ) %>%
  cols_label(
    area = "Area",
    scenario = "Scenario",
    Time = "Year",
    Totalpop = "Total Population(millions)",
    e6 = "Post-secondary Population(%)",
    Under15 = "Under 15 (%)",
    Above65 = "Above 65 (%)",
    TFR = "Total Fertility Rate",
    E0_Male = "Male life expectancy (e0)",
    E0_Female = "Female life expectancy (e0)"
  ) %>%
  tab_spanner(
    label = "Demographics",
    columns = vars(Totalpop, e6, Under15, Above65)
  ) %>%
  tab_spanner(
    label = "Vital Statistics",
    columns = vars(TFR, E0_Male, E0_Female)
  )

# Exibir a tabela
print(table)
gtsave(table, "RS_table.png")
# Age structure
RSag<-finalssp %>% filter(area=="RS") %>% 
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
  facet_wrap(~Time~scenario,nrow = 2)+
  geom_col()+geom_vline(xintercept=0)+scale_fill_manual(values = wic_col6)+scale_x_continuous(labels = abs)+
  labs (x = "Population (million)",y = "Age group",fill = "Educational attainment") +
  theme(legend.title = element_text(size=11, face = "bold", hjust = 0.5), legend.text = element_text(size=10), legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text.y = element_text(size=11),axis.title.y = element_text(size=11, margin = margin(r=5)),
        axis.title.x = element_text(size=11),axis.text.x = element_text(size=11),
        strip.text = element_text(face = "bold", size = 11),
        panel.grid.major = element_blank())+
  scale_y_discrete(breaks = scales::breaks_pretty(n=10))+
  guides(fill = guide_legend(title.position = "top", label.hjust = 0))
RSag
library(patchwork)
library(webshot)  # Para salvar a tabela como imagem
webshot::install_phantomjs()

table_img <- grid::rasterGrob(png::readPNG("RS_table.png"), interpolate = TRUE)

# Combinar o gráfico e a tabela
combined_plot <- (RSag / table_img) + plot_layout(ncol = 2, heights = c(2, 1))

# Exibir o gráfico combinado
print(combined_plot)
