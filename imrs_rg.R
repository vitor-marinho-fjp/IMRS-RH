library(tidyverse)
library(readxl)

# Bases -------------------------------------------------------------------------


municipio <- read_excel("RH.xlsx", sheet = "mun")
View(RH)

cras <- read_excel("1_CRAS/Censo_SUAS_2022_CRAS_RH.xlsx")



creas <- read_excel("2 - CREAS/Censo_SUAS_ 2022_CREAS_RH.xlsx")


gestao_mun <- read_excel("9 - GESTÃO MUNICIPAL/Censo_SUAS_2022_Gestão_Municipal_RH.xlsx")

pop_2022 <- read_excel("POP FAIXAS ETÁRIAS Totais - Homens e Mulheres 2022.xlsx", 
                       skip = 3) %>% 
  select(1:3) %>% rename(IBGE7=1,
                         populacao = 3)




# Somar emprego total -----------------------------------------------------


df_cras_renamed <- cras %>%
  rename(
    nivel_fundamental = d70_9bin1_sum,
    nivel_medio = d70_9bin2_sum,
    nivel_superior = d70_9bin3_sum,
    pedagogos = d70_10bin1_sum,
    assis_social = d70_10bin2_sum,
    antropologo= d70_10bin3_sum,
    adivogados = d70_10bin4_sum,
    psicologos = d70_10bin5_sum,
    serv_estatutario= d70_11bin1_sum,
    empregatos_clt = d70_11bin2_sum,
    comissionados = d70_11bin3_sum,
    outros_vinculos = d70_11bin4_sum,
    nu_trabalhador=nu_trabalhador)


##creas

df_creas_renamed <- creas %>%
  rename(
    nivel_fundamental1 = d_62_8bin1_sum,
    nivel_medio1 = d_62_8bin2_sum,
    nivel_superior1 = d_62_8bin3_sum,
    pedagogos1 = d_62_9bin1_sum,
    assis_social1 = d_62_9bin2_sum,
    antropologo1= d_62_9bin3_sum,
    adivogados1 = d_62_9bin4_sum,
    psicologos1 = d_62_9bin5_sum,
    serv_estatutario1= d_62_10bin1_sum,
    empregatos_clt1 = d_62_10bin2_sum,
    comissionados1 = d_62_10bin3_sum,
    outros_vinculos1 = d_62_10bin4_sum,
    nu_trabalhador1=nu_trabalhador
  )



###gestão

df_gestao_mun <- gestao_mun %>%
  rename(
    nivel_fundamental2 = d81_9bin1_sum,
    nivel_medio2 = d81_9bin2_sum,
    nivel_superior2 = d81_9bin3_sum,
    pedagogos2 = d81_10bin1_sum,
    assis_social2 = d81_10bin2_sum,
    antropologo2= d81_10bin3_sum,
    adivogados2 = d81_10bin4_sum,
    psicologos2 = d81_10bin5_sum,
    serv_estatutario2= d81_11bin1_sum,
    empregatos_clt2 = d81_11bin2_sum,
    comissionados2 = d81_11bin3_sum,
    outros_vinculos2 = d81_11bin4_sum,
    nu_trabalhador2=nu_trabalhador
  )




df <-  left_join(df_cras_renamed, df_creas_renamed)

df <-  left_join(df, df_gestao_mun)





 df2 <-  df %>% 
    group_by(IBGE7) %>%
    summarise(
      nivel_fundamental = sum(nivel_fundamental, nivel_fundamental1, nivel_fundamental2, na.rm = TRUE),
      nivel_medio = sum(nivel_medio, nivel_medio1, nivel_medio2, na.rm = TRUE),
      nivel_superior = sum(nivel_superior, nivel_superior1, nivel_superior2, na.rm = TRUE),
      pedagogos = sum(pedagogos, pedagogos1, pedagogos2, na.rm = TRUE),
      assis_social = sum(assis_social, assis_social1, assis_social2, na.rm = TRUE),
      antropologo = sum(antropologo, antropologo1, antropologo2, na.rm = TRUE),
      adivogados = sum(adivogados, adivogados1, adivogados2, na.rm = TRUE),
      psicologos = sum(psicologos, psicologos1, psicologos2, na.rm = TRUE),
      serv_estatutario = sum(serv_estatutario, serv_estatutario1, serv_estatutario2, na.rm = TRUE),
      empregatos_clt = sum(empregatos_clt, empregatos_clt1, empregatos_clt2, na.rm = TRUE),
      comissionados = sum(comissionados, comissionados1, comissionados2, na.rm = TRUE),
      outros_vinculos = sum(outros_vinculos, outros_vinculos1, outros_vinculos2, na.rm = TRUE),
      nu_trabalhador = sum(nu_trabalhador, nu_trabalhador1, nu_trabalhador2, na.rm = TRUE)
    ) %>%
    select(
      IBGE7,
      nivel_fundamental,
      nivel_medio,
      nivel_superior,
      pedagogos,
      assis_social,
      antropologo,
      adivogados,
      psicologos,
      serv_estatutario,
      empregatos_clt,
      comissionados,
      outros_vinculos,
      nu_trabalhador
    )
  

 
df2 <-  left_join(municipio, df2)


# calcualar indicadores ---------------------------------------------------



df2$IBGE7 <- as.character(df2$IBGE7)

df2 <-  left_join(df2, pop_2022)




IMRS_RH <-  df2 %>%
  group_by(IBGE7) %>%
  summarise(
    B_RHTOTALA = sum(nu_trabalhador, na.rm = TRUE),
    B_RHAS = sum(assis_social, na.rm = TRUE),
    B_RHASPS = (sum(assis_social, na.rm = TRUE) / sum(nivel_superior, na.rm = TRUE)) * 100,
    B_RHPSI = sum(psicologos, na.rm = TRUE),
    B_RHPSIPS = (sum(psicologos, na.rm = TRUE) / sum(nivel_superior, na.rm = TRUE)) * 100,
    B_RHENME = sum(nivel_medio, na.rm = TRUE),
    B_RHCURSU = sum(nivel_superior, na.rm = TRUE),
    B_RHTOEST = sum(serv_estatutario, na.rm = TRUE),
    B_RHTOCEL = sum(empregatos_clt, na.rm = TRUE),
    B_RHGIPOAS = (sum(nivel_fundamental) * 0.33 +
                    sum(nivel_medio) * 0.66 +
                    sum(nivel_superior) * 1 +
                    sum(nivel_superior) * 1.25) / sum(nu_trabalhador, na.rm = TRUE),
    B_RHVPOAAS = ((sum(serv_estatutario, na.rm = TRUE) + sum(empregatos_clt, na.rm = TRUE)) / sum(nu_trabalhador, na.rm = TRUE)) * 100,
    B_RHVINEST = (sum(serv_estatutario, na.rm = TRUE) / sum(nu_trabalhador, na.rm = TRUE)) * 100,
    B_RHTPOASPH = sum(nu_trabalhador, na.rm = TRUE) / (populacao / 10000) 
  )








