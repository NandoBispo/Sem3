install.packages("readxl")
library(readxl)
library(tidyverse)
library(DT)
library(janitor)
library(patchwork)

#Verificando quantas planilhas há na pasta de trabalho Excel.
excel_sheets("Dados/endometriose.xls")

#Exportando o BD.
BD <- read_excel("Dados/endometriose.xls")

#Identificando as variáveis.
BD %>% names()
glimpse(BD)

#Transformando as variáveis em fator.
BD <- BD %>% mutate(
  Dismenorréia = as_factor(Dismenorréia),
  AFSr = as_factor(AFSr)
) #%>% pull(Dismenorréia) %>% head()

#Identificando os tipos de levels
BD %>% pull(Dismenorréia) %>% head()
BD %>% pull(AFSr) %>% head()

#Reordenando os levels (Teste)
forcats::lvls_revalue(BD$Dismenorréia, new_levels = c("Não tem","Leve","Moderada","Intensa","*"))

forcats::lvls_reorder(BD$Dismenorréia, c(2,1,3,4,5))

#Q2a. VAs AFSr x Dismenorréia------------
#Cabeçalho personalizado: https://rstudio.github.io/DT/
colunas = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Gravidade da Endometriose'),
      th(colspan = 6, 'Níveis de Dor')
    ),
    tr(
    lapply(c("Nenhuma","Leve","Moderada","Intensa","*", "Total de Obs."), th)
    )
  )
))
BD %>%
  # filter(!is.na(AFSr)) %>% 
  group_by(Grupo) %>% 
  mutate(
    AFSr = forcats::lvls_revalue(AFSr, c("Nenhuma", "Mínima", "Média", "Moderada", "Elevada")),
    # Dismenorréia = forcats::lvls_revalue(Dismenorréia, new_levels = c("Dor Leve","Não Tem Dor","Dor Moderada","Dor Intensa","*")),
    Dismenorréia = forcats::lvls_reorder(Dismenorréia, c(2,1,3,4,5))
  ) %>%
  # count(AFSr) %>% 
  tabyl(AFSr, Dismenorréia) %>%
  adorn_totals("col") %>%  #Linha de totais.
  # # adorn_totals(c("row", "col")) %>%  #Linha de totais.
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits=2) %>% 
  # adorn_title("top", row_name = "Gravidade", col_name = "Dor na Menstruação") %>% 
  DT::datatable(
    colnames = c("Gravidade da Endometriose" = 1, "Total de Observações" = 7), 
    container = colunas, 
    rownames = FALSE,
    caption = htmltools::tags$caption(#Possibilita a criação da legenda da tabele.
      style = 'caption-side: bottom; text-align: left;',
      'Tabela 1: ', htmltools::em('Comparativo percentual entre a Gravidade da Endometriose e os Níveis de Dor nos pacientes dos Grupos de Controle e Doentes.')
    )
    )

#----Teste(Não deu certo)----
# BD %>%
#   # filter(!is.na(AFSr)) %>% 
#   group_by(Grupo) %>% 
#   count(AFSr,Dismenorréia) %>% 
#   
#   mutate(
#     AFSr = forcats::lvls_revalue(AFSr, c("Não Tem", "Gravidade Mínima", "Gravidade Média", "Gravidade Moderada", "Gravidade Elevada")),
#     Dismenorréia = forcats::lvls_revalue(Dismenorréia, new_levels = c("Dor Leve","Não Tem Dor","Dor Moderada","Dor Intensa","*")),
#     Dismenorréia = forcats::lvls_reorder(Dismenorréia, c(2,1,3,4,5)),
#     pct = prop.table(n)*100
#   ) %>% DT::datatable()
# tabyl(AFSr, Dismenorréia) %>%
#   adorn_totals("col") %>%  #Linha de totais.
#   # # adorn_totals(c("row", "col")) %>%  #Linha de totais.
#   adorn_percentages("row") %>%
#   adorn_pct_formatting(digits=2)
# # adorn_title("top", row_name = "Gravidade", col_name = "Dor na Menstruação") %>% 
# DT::datatable() 


# BD %>%
#   # filter(!is.na(AFSr)) %>%
#   # count(AFSr) %>%
#   # mutate(
#   #   BD$Dismenorréia = forcats::lvls_revalue(BD$Dismenorréia, new_levels = c("Leve","Não tem","Moderada","Intensa","*")),
#   #   BD$Dismenorréia = forcats::lvls_reorder(BD$Dismenorréia, n)
#   # ) %>% 
#   # mutate(Fr = n/sum(n)*100) %>%
#   ggplot() +
#   geom_point(aes(x = AFSr,y = Dismenorréia,  color = Dismenorréia))
# 
# BD %>% filter(!is.na(AFSr)) %>% count(AFSr)


#----Q2b----
anyNA(BD$Idade)
anyNA(BD$PCRa)

Rot_Idade = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Grupo de Pacientes'),
      th(colspan = 8, 'Idade dos Pacientes')
    ),
    tr(
      lapply(c("Min","Q1","Média","Md",  "Q3", "Max", "Var", "DP"), th)
    )
  )
))
BD %>% 
  group_by(Grupo) %>% 
  summarise(
    Min = min(Idade), 
    Q1 = quantile(Idade, .25),
    Media = round(mean(Idade),2), 
    Md = median(Idade), 
    Q3 = quantile(Idade, .75), 
    Max = max(Idade), 
    Var = round(var(Idade),2), 
    DP = round(sd(Idade),2)
  ) %>% 
  DT::datatable(
    rownames = FALSE, 
    container = Rot_Idade,
    caption = htmltools::tags$caption(#Possibilita a criação da legenda da tabele.
      style = 'caption-side: bottom; text-align: left;',
      'Tabela 2: ', htmltools::em('Medidas resumo e variabilidade da idade dos pacientes.')
    )
                )
    
summary(BD$Idade)

P1 <- BD %>% #Histograma
  ggplot()+
  geom_histogram(
    aes(x = Idade), 
    binwidth = 2, 
    color = "white"
    ) + labs(
      y = "Frequência",
      title = "Histograma",
      subtitle = "Distribuição de freqência das idades dos pacientes."
      )

P1

P2 <- BD %>% #Boxplot
  group_by(Grupo) %>% 
  ggplot() +
  geom_boxplot(
    aes(
      y = Idade, 
      x = Grupo)
    ) + coord_cartesian(
      ylim = c(0,45)
      ) + labs(
        title = "Gráfico de Caixas",
        subtitle = "Comparativo entre as idades dos pacientes dos grupos de Controle e Doenetes."
      )
P2
#-------Visualizando e testando as modificações---------------------------

BD %>% 
  mutate(
    PCRa2 = stringr::str_replace(PCRa, "<", ""),
    PCRa2 = stringr::str_replace(PCRa2, ",", "."),
    PCRa2 = as.double(PCRa2), 
    PCRa2 = if_else(PCRa2 == 0.5, 0.25, PCRa2)
  ) %>% 
  select(PCRa, PCRa2) %>% 
  view()

#-------Implementando as modificações---------------------------
BD <- 
  BD %>% 
    mutate(#Modificando os dados da VA PCRa
      PCRa2 = stringr::str_replace(PCRa, "<", ""),
      PCRa2 = stringr::str_replace(PCRa2, ",", "."),
      PCRa2 = as.double(PCRa2),
      PCRa2 = if_else(PCRa2 == 0.5, 0.25, PCRa2)
    )
#--------------------------------------------------------------
glimpse(BD)

#----VA PCRa----
Rot_PCRa = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Grupo de Pacientes'),
      th(colspan = 8, 'Concentração de Proteína C-reativa')
    ),
    tr(
      lapply(c("Min","Q1","Média","Md",  "Q3", "Max", "Var", "DP"), th)
    )
  )
))
BD %>% #Extraindo medidas resumo.
  group_by(Grupo) %>% 
  summarise(
    Min = min(PCRa2), 
    Q1 = quantile(PCRa2, .25),
    Media = round(mean(PCRa2),2), 
    Md = median(PCRa2),
    Q3 = quantile(PCRa2, .75),
    Max = max(PCRa2),
    Var = round(var(PCRa2),2), 
    DP = round(sd(PCRa2),2)
  ) %>% DT::datatable(
    rownames = FALSE, 
    container = Rot_PCRa,
    caption = htmltools::tags$caption(#Possibilita a criação da legenda da tabele.
      style = 'caption-side: bottom; text-align: left;',
      'Tabela 3: ', htmltools::em('Medidas resumo e variabilidade do Nível de Concentração de Proteína C-reativa dos pacientes.')
    )
  )

P3 <- 
  BD %>% 
    ggplot()+
    geom_histogram(
      aes(x = PCRa2), #fill = "blue", 
      binwidth = 3, 
      color = "white"
      ) + labs(
        x = "PCRa",
        y = "Frequência",
        title = "Histograma",
        subtitle = "Concentração de Proteína C-reativa"
      )
P3

P4 <- 
  BD %>% 
    group_by(Grupo) %>% 
    ggplot() +
    geom_boxplot(
      aes(
        x = Grupo, 
        y = PCRa2)
      ) + coord_cartesian(
        ylim = c(0,45)
        ) + labs(
          y = "PCRa",
          title = "Gráfico de Caixas",
          subtitle = "Comparativo da Concentração de Proteína C-reativa entre os pacientes dos grupos de Controle e Doenetes."
        )
P4
#----Gráfico de Dispersão entre as VAs Idade e PCRa a fim de identificar correlação.----
BD %>% 
  filter(Grupo %in%  c("Doente", "Controle")) %>% 
  ggplot() +
  geom_point(aes(x = Idade, y = PCRa2, color = Idade)) + 
labs(x = "Idade dos Paciente", #Insere os rótulos e títulos no gráfico
     y =  "Concentração de Proteína C-reativa",
     title = "Gráfico de dispersão",
     subtitle = "PCR x Idade dos pacientes por Grupo."
     ) +
coord_cartesian(ylim = c(0, 35)) + #Controla a escala do eixo y em coordenadas cartesianas.
facet_wrap(~Grupo, ncol = 2) #Possibilita colocar 2 gráficos por tela.

cor(BD$Idade, BD$PCRa2)

#---- Criando conta Git
usethis::create_project(caminho)
caminho <- getwd()
