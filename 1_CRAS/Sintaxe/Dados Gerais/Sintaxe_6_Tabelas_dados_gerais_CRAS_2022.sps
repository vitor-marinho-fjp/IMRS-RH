* Encoding: UTF-8.

**CENSO SUAS 2021 - CRAS**


FREQUENCIES VARIABLES=Regiao Porte_pop2010 UF
  /ORDER=ANALYSIS.

**BLOCO 2 - ESTRUTURA F�SICA DO CRAS**

FREQUENCIES VARIABLES=q1   q2_1  q2_2 d_2 q3  q4
  /ORDER=ANALYSIS.




MULT RESPONSE GROUPS=$mr.5 'Tipo de unidade com a qual o CRAS compartilha seu im�vel'
(q5_1 q5_2 q5_3 q5_4 q5_5 q5_6 q5_7 q5_8 q5_9 q5_10 q5_11  q5_12 q5_13  q5_99 (1))
/FREQUENCIES=$mr.5.


MULT RESPONSE GROUPS=$mr.6 'Espa�os do im�vel que s�o compartilhados entre o CRAS e a outra unidade'
(q6_1  q6_2  q6_3  q6_4  q6_5  q6_6  q6_7  q6_8  q6_9  q6_10  q6_11  q6_12 (1))
/FREQUENCIES=$mr.6.

FREQUENCIES VARIABLES= d_7_atend_categoria  d_7_adm_atend_categoria  d_7_PBF_categoria  d_7_PCF_categoria   d_7_8_banheiros    q7_9  q7_10 q7_11  q7_12  q7_13
  /ORDER=ANALYSIS.



CTABLES
  /VLABELS VARIABLES=q8_1 q8_2 q8_3 q8_4 DISPLAY=LABEL
  /TABLE q8_1 [C][COUNT F40.0, ROWPCT.VALIDN PCT40.1] + q8_2 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1] + q8_3 [C][COUNT F40.0, ROWPCT.VALIDN PCT40.1] + q8_4 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1]
  /CLABELS ROWLABELS=OPPOSITE
  /CATEGORIES VARIABLES=q8_1 [1, 2, 0, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q8_2 [1, 2, 0, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q8_3 [1, 2, 0, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q8_4 [1, 2, 0, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
/TITLES
 TITLE= 'q_8.Condi��es de acessibilidade para pessoas com defici�ncia e pessoas idosas'.


MULT RESPONSE GROUPS=$mr.9 'q11.Outras adapta��es para assegurar acessibilidade'
(q9_1  q9_2 q9_3  q9_4  q9_5  q9_6  q9_7  q9_0 (1))
/FREQUENCIES=$mr.9.


MULT RESPONSE GROUPS=$mr.10 'q11.Equipamentos e materiais dispon�veis para o desenvolvimento dos servi�os no CRAS'
(q10_1 q10_2 q10_3 q10_4 q10_5 q10_6 q10_7 q10_8 q10_9 q10_10 q10_11 q10_12 q10_13 q10_14 q10_15 q10_16  q10_17 (1))
/FREQUENCIES=$mr.10.


FREQUENCIES VARIABLES=d_11_1  d_11_2
  /ORDER=ANALYSIS. 


CTABLES
  /VLABELS VARIABLES=q12_1 q12_2 q12_3 q12_4 DISPLAY=LABEL
  /TABLE q12_1 [C][COUNT F40.0, ROWPCT.VALIDN PCT40.1] + q12_2 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1] + q12_3 [C][COUNT F40.0, ROWPCT.VALIDN PCT40.1] + q12_4 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1]
  /CLABELS ROWLABELS=OPPOSITE
  /CATEGORIES VARIABLES=q12_1 [1, 2, 3, 4,5, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q12_2 [1, 2, 3, 4,5, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q12_3 [1, 2, 3, 4,5, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q12_4 [1, 2, 3, 4,5, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
/TITLES
 TITLE= 'q_12.Em rela��o a Internet, informe com que frequ�ncia a equipe'.


**BLOCO 3 - SERVI�O DE PROTE��O E ATENDIMENTO INTEGRAL � FAM�LIA***

MULT RESPONSE GROUPS=$mr.13 'q13.A��es e atividades desenvolvidas no �mbito do PAIF'
(q13_1 q13_2 q13_3 q13_4 q13_5 q13_6 q13_7 q13_8 q13_9 q13_10 q13_11 q13_12 q13_13 q13_14 q13_15 q13_16 q13_17 q13_18 q13_19 q13_20 q13_21 q13_22 q13_23 q13_24 q13_25  q13_26 q13_27 q13_28 q13_29  q13_30 q13_99 (1))
/FREQUENCIES=$mr.13.


FREQUENCIES q14_categoria q14_98
  /ORDER=ANALYSIS. 


MULT RESPONSE GROUPS=$mr.15 'q_15.Principais tematicas abordadas no atendimento coletivo realizado pelo PAIF'
(q15_1 q15_2 q15_3 q15_4 q15_5 q15_6 q15_7 q15_8 q15_9  q15_10 q15_11 q15_12 q15_13 q15_14 q15_15 q15_16 q15_17  q15_18 q15_19 q15_20 q15_21  q15_22 q15_23 q15_24 q15_25 q15_26 q15_27 q15_99 (1))
/FREQUENCIES=$mr.15.


MULT RESPONSE GROUPS=$mr.16 'q_16.a��es e/ou estrat�gias de potencial preventivo e informativo (campanhas sociais e de conscientiza��o) este CRAS  participa  ou realiza no territ�rio'
( q16_1 q16_2 q16_3 q16_4 q16_5 q16_6 q16_7 q16_8 q16_9 q16_10 q16_11 q16_12 q16_13  q16_14 q16_99 (1))
/FREQUENCIES=$mr.16.


**BLOCO 4 - SCFV

FREQUENCIES q17
  /ORDER=ANALYSIS. 


CTABLES
  /VLABELS VARIABLES=q18_1 q18_2 q18_3 q18_4 q18_5 q18_6 DISPLAY=LABEL
  /TABLE q18_1 [COUNT F40.0, COLPCT.COUNT PCT40.1] + q18_2 [COUNT F40.0, COLPCT.COUNT PCT40.1] +
    q18_3 [COUNT F40.0, COLPCT.COUNT PCT40.1] + q18_4 [COUNT F40.0, COLPCT.COUNT PCT40.1] + q18_5
    [COUNT F40.0, COLPCT.COUNT PCT40.1] + q18_6 [COUNT F40.0, COLPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES= q18_1 q18_2 q18_3 q18_4 q18_5 q18_6  ORDER=A KEY=VALUE EMPTY=INCLUDE
  /TITLES
    TITLE='q_18. Esta unidade oferta diretamente o Servi�o de Conviv�ncia e Fortalecimento de V�nculos  '.


MULT RESPONSE GROUPS=$mr.19 'q19. Este CRAS possui rede referenciada para a oferta Servi�o de Conviv�ncia e Fortalecimento de V�nculos?'
(q19_1 q19_2 q19_3 q19_0 (1))
/FREQUENCIES=$mr.19.


CTABLES
  /VLABELS VARIABLES=q20_1 q20_2 q20_3 q20_4 q20_5 q20_6 DISPLAY=LABEL
  /TABLE q20_1 [COUNT F40.0, COLPCT.COUNT PCT40.1] + q20_2 [COUNT F40.0, COLPCT.COUNT PCT40.1] +
    q20_3 [COUNT F40.0, COLPCT.COUNT PCT40.1] + q20_4 [COUNT F40.0, COLPCT.COUNT PCT40.1] + q20_5
    [COUNT F40.0, COLPCT.COUNT PCT40.1] + q20_6 [COUNT F40.0, COLPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES= q20_1 q20_2 q20_3 q20_4 q20_5 q20_6   ORDER=A KEY=VALUE EMPTY=INCLUDE
  /TITLES
    TITLE='q_20. A rede referenciada a este CRAS oferta Servi�o de Conviv�ncia e Fortalecimento de V�nculos  '.


FREQUENCIES q21 q22 q23 
  /ORDER=ANALYSIS. 


**BLOCO 5 - PSB NO DOMICILIO**

FREQUENCIES  q24  d25_1 d25_2 d25_3 d25_4
  /ORDER=ANALYSIS. 


MULT RESPONSE GROUPS=$mr.26 'q26. Este CRAS possui rede referenciada para oferta do Servi�o de Prote��o Social B�sica no Domic�lio para Pessoas com defici�ncia e idosas?'
(q26_1 q26_2 q26_3 q26_0 (1))
/FREQUENCIES=$mr.26.


FREQUENCIES d27_1 d27_2 d27_3 d27_4
  /ORDER=ANALYSIS. 

MULT RESPONSE GROUPS=$mr.28 'q28.  Informe as a��es e atividades realizadas no �mbito do Servi�o no Domic�lio, oferta direta ou referenciada ao CRAS'
(q28_1 q28_2 q28_3 q28_4 q28_5 q28_6 q28_7 q28_8 q28_9 q28_99 q28_0(1))
/FREQUENCIES=$mr.28.


FREQUENCIES q29
  /ORDER=ANALYSIS. 


**BLOCO 6 - EQUIPE VOLANTE**

FREQUENCIES q30 q31
  /ORDER=ANALYSIS. 

MULT RESPONSE GROUPS=$mr.32 'q32.Como ocorre  o deslocamento  da equipe  volante '
(q32_1 q32_2 q32_3 q32_4 q32_5 q32_6 q32_7 q32_99(1))
/FREQUENCIES=$mr.32.


MULT RESPONSE GROUPS=$mr.33 'q33.Indique os tipos de atendimento realizados pela equipe volante nas comunidades  '
(q33_1 q33_2 q33_3 q33_4 q33_5 q33_6 q33_7 q33_8 q33_9 q33_10 q33_11 q33_12 q33_99(1))
/FREQUENCIES=$mr.33.


**BLOCO 7 - BENEF�CIOS SOCIOASSISTENCIAIS **

FREQUENCIES q34
  /ORDER=ANALYSIS. 

MULT RESPONSE GROUPS=$mr.35 'q35.Quais s�o os Benef�cios Eventuais ofertados (concedido/entregue) neste CRAS?  '
(q35_1 q35_2 q35_3 q35_4(1))
/FREQUENCIES=$mr.35.


MULT RESPONSE GROUPS=$mr.36 'q36. informe caso o munic�pio conceda/entrega Beneficio Eventual em situa��o de calamidade  '
(q36_1 q36_2 q36_3  q36_4  q36_5  q36_6  q36_7  q36_8  q36_9  q36_10  q36_11  q36_12  q36_99(1))
/FREQUENCIES=$mr.36.

MULT RESPONSE GROUPS=$mr.37 'q37.Caso o munic�pio conceda Beneficio Eventual em situa��o de vulnerabilidade social tempor�ria, informe quais  '
(q37_1 q37_2 q37_3 q37_4 q37_5 q37_6 q37_7 q37_8 q37_9 q37_99(1))
/FREQUENCIES=$mr.37.


MULT RESPONSE GROUPS=$mr.38 'q38. Com rela��o ao Benef�cio de Presta��o Continuada, este CRAS faz  '
(q38_1 q38_2 q38_3 q38_4 q38_5 q38_6 q38_99 q38_0(1))
/FREQUENCIES=$mr.38.


**BLOCO 8 -CADASTRO �NICO**

FREQUENCIES q39
  /ORDER=ANALYSIS. 

MULT RESPONSE GROUPS=$mr.40 'q40.Indique as a��es e atividades desenvolvidas pela equipe desta Unidade no �mbito do Cadastro �nico  '
(q40_1 q40_2 q40_3 q40_4 q40_5 q40_6 q40_7 q40_8 q40_9 q40_10 q40_11 q40_12 q40_13 q40_14 q40_15 q40_16 q40_17 q40_18 q40_99(1))
/FREQUENCIES=$mr.40.


FREQUENCIES  d41_1 d41_2  d41_3  d42_1  d42_2 d42_3  d42_4 d42_5
  /ORDER=ANALYSIS. 


FREQUENCIES  d42_1  d42_2 d42_3  d42_4 d42_5
  /ORDER=ANALYSIS. 


MULT RESPONSE GROUPS=$mr.43 'q43.q43_1_v15406_1_1 - A��es espec�ficas s�o feitas para atender demandas de averigua��o e revis�o cadastral  '
(q43_1 q43_2 q43_3 q43_99 q43_0  (1))
/FREQUENCIES=$mr.43.



CTABLES
  /VLABELS VARIABLES=q44_1 q44_2 q44_3 q44_4 q44_5  q44_99 DISPLAY=LABEL
  /TABLE q44_1 [C][COUNT F40.0, ROWPCT.VALIDN PCT40.1] + q44_2 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1] + q44_3 [C][COUNT F40.0, ROWPCT.VALIDN PCT40.1] + q44_4 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1]  + q44_5 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1]  + q44_99 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1]
  /CLABELS ROWLABELS=OPPOSITE
  /CATEGORIES VARIABLES=q44_1 [1, 2, 0, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q44_2 [1, 2, 0, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q44_3 [1, 2, 0, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q44_4 [1, 2, 0, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q44_5 [1, 2, 0, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q44_99 [1, 2, 0 OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
/TITLES
 TITLE= 'q_44.Indique em quais situa��es e com qual frequ�ncia  a equipe faz entrevistas domiciliares'.


**BLOCO 9 - PROGRAMA BOLSA FAM�LIA  **

MULT RESPONSE GROUPS=$mr.45 'q45.Indique as a��es e atividades desenvolvidas pela equipe desta Unidade em rela��o � gest�o de benef�cios do Programa Bolsa Fam�lia  '
( q45_0 q45_1 q45_2 q45_3 q45_4 q45_5 q45_6 q45_7 q45_8 q45_9 q45_10 q45_11 q45_12 q45_13 q45_99(1)) 
/FREQUENCIES=$mr.45.


MULT RESPONSE GROUPS=$mr.46 'q46.Indique quais s�o as dificuldades enfrentadas pelo CRAS em rela��o � gest�o de benef�cios do Programa Bolsa Fam�lia  '
(q46_1 q46_2 q46_3 q46_4 q46_5 q46_6 q46_7 q46_8  q46_9 q46_10 q46_11 q46_12 q46_13 q46_14 q46_15 q46_16 q46_99(1))
/FREQUENCIES=$mr.46.


FREQUENCIES q47
  /ORDER=ANALYSIS. 


MULT RESPONSE GROUPS=$mr.48 'q48.Indique as a��es e atividades desenvolvidas pela equipe desta Unidade em rela��o �s fam�lias em descumprimento de condicionalidades '
(q48_0 q48_1 q48_2 q48_3 q48_4 q48_5 q48_6 q48_7 q48_8 q48_9 q48_10 q48_11 q48_12 q48_13 q48_14 q48_15 q48_99(1))
/FREQUENCIES=$mr.48.

MULT RESPONSE GROUPS=$mr.49 'q49.S�o feitas a��es espec�ficas para atender as fam�lias em fase de suspens�o por descumprimento de condicionalidades '
(q49_1 q49_2 q49_3 q49_4 q49_99 q49_0 q49_98(1))
/FREQUENCIES=$mr.49.


MULT RESPONSE GROUPS=$mr.50 'q50.Este CRAS recebe den�ncia de fraudes de recebimento de benef�cios do Programa Bolsa Fam�lia, qual o tratamento dado?   '
(q50_1 q50_2 q50_99 q50_0 q50_98(1))
/FREQUENCIES=$mr.50.

**BLOCO 10 - � FUNCIONAMENTO DO CRAS DURANTE A PANDEMIA DE COVID19   **


FREQUENCIES q51_0 
  /ORDER=ANALYSIS. 


CTABLES
  /VLABELS VARIABLES= q51_1  q51_2 q51_3  q51_4  q51_5  q51_6  q51_7  q51_8  q51_9  q51_10  q51_99 DISPLAY=LABEL
  /TABLE q51_1 [C][COUNT F40.0, ROWPCT.VALIDN PCT40.1] +  q51_2 [C][COUNT F40.0, ROWPCT.VALIDN PCT40.1] + q51_3[C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1] + q51_4 [C][COUNT F40.0, ROWPCT.VALIDN PCT40.1] + q51_5 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1]  + q51_6 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1]  + q51_7   [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1]  + q51_8 [C][COUNT F40.0, ROWPCT.VALIDN PCT40.1] + q51_9 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1]  + q51_10 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1]   + q51_99   [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1]
  /CLABELS ROWLABELS=OPPOSITE
   /CATEGORIES VARIABLES=q51_1 [1, 2, 3, 4, 5,  OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
   /CATEGORIES VARIABLES=q51_2 [1, 2, 3, 4, 5,  OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q51_3 [1, 2, 3, 4, 5, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q51_4 [1, 2, 3, 4, 5,OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q51_5 [1, 2, 3, 4, 5,OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q51_6 [1, 2, 3, 4, 5, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
   /CATEGORIES VARIABLES=q51_7 [1, 2, 3, 4, 5,  OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q51_8 [1, 2, 3, 4, 5, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q51_9 [1, 2, 3, 4, 5, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q51_10[1, 2, 3, 4, 5, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
 /CATEGORIES VARIABLES=q51_99 [1, 2, 3, 4, 5, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
/TITLES
 TITLE= 'q_51.Como se deu o funcionamento deste CRAS durante a pandemia do Coronav�rus e a frequ�ncia com isto ocorreu'.




MULT RESPONSE GROUPS=$mr.52 'q52.Como estava a jornada de trabalho das(os) profissionais durante a pandemia ?    '
(q52_1 q52_2 q52_3 q52_99(1))
/FREQUENCIES=$mr.52.

FREQUENCIES q53
  /ORDER=ANALYSIS. 


MULT RESPONSE GROUPS=$mr.54 'q54. Esta unidades realizou algumas destas atividades em rela��o ao Aux�lio Emergencial '
(q54_1 q54_2 q54_3 q54_4 q54_5 q54_99(1))
/FREQUENCIES=$mr.54.

**BLOCO 11 - PROGRAMA CRIAN�A FELIZ   **

FREQUENCIES q55 
  /ORDER=ANALYSIS. 

MULT RESPONSE GROUPS=$mr.56 'q56. O Programa Crian�a Feliz tem acesso aos seguintes equipamentos deste CRAS '
(q56_1 q56_2 q56_3 q56_4 q56_5 q56_0 (1))
/FREQUENCIES=$mr.56.

FREQUENCIES q57 
  /ORDER=ANALYSIS. 


CTABLES
  /VLABELS VARIABLES= q58_1 q58_2 q58_3 q58_4 q58_5 DISPLAY=LABEL
  /TABLE q58_1 [C][COUNT F40.0, ROWPCT.VALIDN PCT40.1] + q58_2 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1] + q58_3 [C][COUNT F40.0, ROWPCT.VALIDN PCT40.1] + q58_4 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1]  + q58_5 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1]  
  /CLABELS ROWLABELS=OPPOSITE
  /CATEGORIES VARIABLES=q58_1 [1, 2, 3, 4, 5, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q58_2 [1, 2, 3, 4, 5,  OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q58_3 [1, 2, 3, 4, 5,  OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q58_4 [1, 2, 3, 4, 5,  OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q58_5 [1, 2, 3, 4, 5,  OTHERNM]  EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
/TITLES
 TITLE= 'q_58.Com que periodicidade s�o realizadas as seguintes atividades de integra��o do Programa Crian�a Feliz com este CRAS.'.

MULT RESPONSE GROUPS=$mr.59 'q59. No que se refere �s dificuldades em realizar as visitas domiciliares, assinale as tr�s principais alternativas apresentadas pela equipe'
(q59_0 q59_1 q59_2 q59_3 q59_4 q59_5 q59_6 q59_7 q59_8 q59_9 q59_10 q59_11 q59_99 (1))
/FREQUENCIES=$mr.59.


**BLOCO 12 - GEST�O E TERRIT�RIO***


CTABLES
  /VLABELS VARIABLES= q60_1 q60_2 q60_3 q60_4 q60_5  q60_6  q60_7  DISPLAY=LABEL
  /TABLE q60_1 [C][COUNT F40.0, ROWPCT.VALIDN PCT40.1] + q60_2 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1] + q60_3 [C][COUNT F40.0, ROWPCT.VALIDN PCT40.1] + q60_4 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1]  + q60_5 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1]  + q60_6 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1] + q60_7 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1] 
  /CLABELS ROWLABELS=OPPOSITE
  /CATEGORIES VARIABLES=q60_1 [1, 2, 3, 4, 5, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q60_2 [1, 2, 3, 4, 5,  OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q60_3 [1, 2, 3, 4, 5,  OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q60_4 [1, 2, 3, 4, 5,  OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q60_5 [1, 2, 3, 4, 5,  OTHERNM]  EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q60_6 [1, 2, 3, 4, 5,  OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q60_7 [1, 2, 3, 4, 5,  OTHERNM]  EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
/TITLES
 TITLE= 'q_60.Com que periodicidade, em m�dia, s�o realizadas as seguintes atividades de gest�o'.

FREQUENCIES q61 q62
  /ORDER=ANALYSIS. 

MULT RESPONSE GROUPS=$mr.63 'q63.Quais mecanismos de participa��o s�o utilizados nesta unidade'
(q63_1 q63_2 q63_3 q63_4 q63_99 (1))
/FREQUENCIES=$mr.63.


MULT RESPONSE GROUPS=$mr.64 'q64. No territ�rio de abrang�ncia deste CRAS, h� presen�a de'
(q64_1 q64_2 q64_3 q64_4 q64_5 q64_6 q64_7 q64_8 q64_9 q64_10 q64_11  q64_12 q64_13 q64_14 q64_99 (1))
/FREQUENCIES=$mr.64.

MULT RESPONSE GROUPS=$mr.65_1 'q65.1 Quais atendimentos a Grupos Tradicionais e Espec�ficos foram realizados neste CRAS nos �ltimos 12 meses para Povos Ind�genas '
(q65_1_1 q65_1_2 q65_1_3 q65_1_4 q65_1_99 q65_1_0 q65_1_5(1))
/FREQUENCIES=$mr.65_1.

MULT RESPONSE GROUPS=$mr.65_2 'q65.2 Quais atendimentos a Grupos Tradicionais e Espec�ficos foram realizados neste CRAS nos �ltimos 12 meses para Comunidade Quilombola '
(q65_2_1 q65_2_2 q65_2_3 q65_2_4 q65_2_99 q65_2_0 q65_2_5(1))
/FREQUENCIES=$mr.65_2.

MULT RESPONSE GROUPS=$mr.65_3 'q65.3 Quais atendimentos a Grupos Tradicionais e Espec�ficos foram realizados neste CRAS nos �ltimos 12 meses para Comunidade Ribeirinha '
(q65_3_1 q65_3_2 q65_3_3 q65_3_4 q65_3_99 q65_3_0 q65_3_5(1))
/FREQUENCIES=$mr.65_3.

MULT RESPONSE GROUPS=$mr.65_4 'q65.4 Quais atendimentos a Grupos Tradicionais e Espec�ficos foram realizados neste CRAS nos �ltimos 12 meses para Povos Ciganos '
(q65_4_1 q65_4_2 q65_4_3 q65_4_4 q65_4_99 q65_4_0 q65_4_5(1))
/FREQUENCIES=$mr.65_4.

MULT RESPONSE GROUPS=$mr.65_5 'q65.5 Quais atendimentos a Grupos Tradicionais e Espec�ficos foram realizados neste CRAS nos �ltimos 12 meses para Povos de Matriz Africana e de terreiro '
(q65_5_1 q65_5_2 q65_5_3 q65_5_4 q65_5_99 q65_5_0 q65_5_5(1))
/FREQUENCIES=$mr.65_5.

MULT RESPONSE GROUPS=$mr.65_6 'q65.6 Quais atendimentos a Grupos Tradicionais e Espec�ficos foram realizados neste CRAS nos �ltimos 12 meses para  Extrativistas '
(q65_6_1 q65_6_2 q65_6_3 q65_6_4 q65_6_99 q65_6_0 q65_6_5(1))
/FREQUENCIES=$mr.65_6.

MULT RESPONSE GROUPS=$mr.65_7 'q65.7 Quais atendimentos a Grupos Tradicionais e Espec�ficos foram realizados neste CRAS nos �ltimos 12 meses para Pescadores artesanais  '
(q65_7_1 q65_7_2 q65_7_3 q65_7_4 q65_7_99 q65_7_0 q65_7_5(1))
/FREQUENCIES=$mr.65_7.

MULT RESPONSE GROUPS=$mr.65_8 'q65.8 Quais atendimentos a Grupos Tradicionais e Espec�ficos foram realizados neste CRAS nos �ltimos 12 meses para Ribeirinhas (fam�lias em calhas de rios)'
(q65_8_1 q65_8_2 q65_8_3 q65_8_4 q65_8_99 q65_8_0 q65_8_5(1))
/FREQUENCIES=$mr.65_8.

MULT RESPONSE GROUPS=$mr.65_9 'q65.9 Quais atendimentos a Grupos Tradicionais e Espec�ficos foram realizados neste CRAS nos �ltimos 12 meses para Outros Grupos tradicionais e espec�ficos  '
(q65_9_1 q65_9_2 q65_9_3 q65_9_4 q65_9_99 q65_9_0 q65_9_5(1))
/FREQUENCIES=$mr.65_9.



MULT RESPONSE GROUPS=$mr.65_10 'q65.10 Quais atendimentos a Grupos Tradicionais e Espec�ficos foram realizados neste CRAS nos �ltimos 12 meses para para Trabalho Infantil '
(q65_10_1 q65_10_2 q65_10_3  q65_10_4  q65_10_99  q65_10_0  q65_10_5 (1))
/FREQUENCIES=$mr.65_10.



MULT RESPONSE GROUPS=$mr.65_11 'q65.11 Quais atendimentos a Grupos Tradicionais e Espec�ficos foram realizados neste CRAS nos �ltimos 12 meses para Atingidos por desastre  '
(q65_11_1 q65_11_2 q65_11_3 q65_11_4 q65_11_99 q65_11_0 q65_11_5(1))
/FREQUENCIES=$mr.65_11.

MULT RESPONSE GROUPS=$mr.65_12 'q65.12 Quais atendimentos a Grupos Tradicionais e Espec�ficos foram realizados neste CRAS nos �ltimos 12 meses para Familiares de presos e egressos do sistema carcer�rio  '
(q65_12_1 q65_12_2 q65_12_3 q65_12_4 q65_12_99 q65_12_0 q65_12_5(1))
/FREQUENCIES=$mr.65_12.

MULT RESPONSE GROUPS=$mr.65_13 'q65.13 Quais atendimentos a Grupos Tradicionais e Espec�ficos foram realizados neste CRAS nos �ltimos 12 meses para Medida Socioeducativa'
(q65_13_1 q65_13_2 q65_13_3 q65_13_4 q65_13_99 q65_13_0 q65_13_5(1))
/FREQUENCIES=$mr.65_13.

MULT RESPONSE GROUPS=$mr.65_14 'q65.14 Quais atendimentos a Grupos Tradicionais e Espec�ficos foram realizados neste CRAS nos �ltimos 12 meses para Pessoas em situa��o de rua e/ou sem teto '
(q65_14_1 q65_14_2 q65_14_3 q65_14_4 q65_14_99 q65_14_0 q65_14_5(1))
/FREQUENCIES=$mr.65_14.

MULT RESPONSE GROUPS=$mr.65_15 'q65.15 Quais atendimentos a Grupos Tradicionais e Espec�ficos foram realizados neste CRAS nos �ltimos 12 meses para Pessoas imigrantes (internacional) e ou refugiados  '
(q65_15_1 q65_15_2 q65_15_3 q65_15_4 q65_15_99 q65_15_0 q65_15_5(1))
/FREQUENCIES=$mr.65_15.


MULT RESPONSE GROUPS=$mr.65_16 'q65.16 Quais atendimentos a Grupos Tradicionais e Espec�ficos foram realizados neste CRAS nos �ltimos 12 meses para Transexuais/transg�nero/travestis/l�sbicas/gays/bissexua is/intersexuais  '
(q65_16_1 q65_16_2 q65_16_3 q65_16_4 q65_16_99 q65_16_0 q65_16_5(1))
/FREQUENCIES=$mr.65_16.


CTABLES
  /VLABELS VARIABLES= q66_1 q66_2 q66_3  q66_4  q66_5  q66_6  q66_7  q66_8  q66_9  q66_10   DISPLAY=LABEL
  /TABLE  q66_1 [C][COUNT F40.0, ROWPCT.VALIDN PCT40.1] +  q66_2 [C][COUNT F40.0, ROWPCT.VALIDN PCT40.1] + q66_3[C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1] + q66_4 [C][COUNT F40.0, ROWPCT.VALIDN PCT40.1] + q66_5 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1]  + q66_6 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1]  + q66_7   [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1]  + q66_8 [C][COUNT F40.0, ROWPCT.VALIDN PCT40.1] + q66_9 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1]  + q66_10 [C][COUNT F40.0, ROWPCT.VALIDN 
    PCT40.1]  
  /CLABELS ROWLABELS=OPPOSITE
   /CATEGORIES VARIABLES=q66_1 [0, 1, 2, 3,  OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
   /CATEGORIES VARIABLES=q66_2 [0, 1, 2, 3,  OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q66_3 [0, 1, 2, 3, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q66_4 [0, 1, 2, 3, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q66_5 [0, 1, 2, 3, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q66_6 [0, 1, 2, 3, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
   /CATEGORIES VARIABLES=q66_7 [0, 1, 2, 3,  OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q66_8 [0, 1, 2, 3, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q66_9 [0, 1, 2, 3, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=q66_10[0, 1, 2, 3, OTHERNM] EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
/TITLES
 TITLE= 'q_66.Indique a quais sistemas informatizados do Governo Federal esta Unidade tem acesso'.


MULT RESPONSE GROUPS=$mr.67_1 'q67.1 Indique as a��es de articula��o deste CRAS com os seguintes servi�os, programas ou institui��es existentes no munic�pio - Unidades P�blicas da Rede de Prote��o Social B�sica'
(q67_1_1 q67_1_2  q67_1_3  q67_1_4 q67_1_5  q67_1_6 q67_1_7 q67_1_8 q67_1_0 q67_1_9 (1))
/FREQUENCIES=$mr.67_1.

MULT RESPONSE GROUPS=$mr.67_2 'q67.2 Indique as a��es de articula��o deste CRAS com os seguintes servi�os, programas ou institui��es existentes no munic�pio - Unidades Conveniadas da Rede de Prote��o Social B�sica'
(q67_2_1 q67_2_2  q67_2_3  q67_2_4 q67_2_5  q67_2_6 q67_2_7 q67_2_8 q67_2_0 q67_2_9 (1))
/FREQUENCIES=$mr.67_2.

MULT RESPONSE GROUPS=$mr.67_3 'q67.3 Indique as a��es de articula��o deste CRAS com os seguintes servi�os, programas ou institui��es existentes no munic�pio - Centro de Refer�ncia Especializado em Assist�ncia Social - CREAS'
(q67_3_1 q67_3_2  q67_3_3  q67_3_4 q67_3_5  q67_3_6 q67_3_7 q67_3_8 q67_3_0 q67_3_9 (1))
/FREQUENCIES=$mr.67_3.

MULT RESPONSE GROUPS=$mr.67_4 'q67.4Indique as a��es de articula��o deste CRAS com os seguintes servi�os, programas ou institui��es existentes no munic�pio - Outras Unidades da rede de prote��o social especial '
(q67_4_1 q67_4_2  q67_4_3  q67_4_4 q67_4_5  q67_4_6 q67_4_7 q67_4_8 q67_4_0 q67_4_9 (1))
/FREQUENCIES=$mr.67_4.

MULT RESPONSE GROUPS=$mr.67_5 'q52.5  Indique as a��es de articula��o deste CRAS com os seguintes servi�os, programas ou institui��es existentes no munic�pio - Servi�os de sa�de '
(q67_5_1 q67_5_2  q67_5_3  q67_5_4  q67_5_5  q67_5_6 q67_5_7 q67_5_8 q67_5_0 q67_5_9 (1))
/FREQUENCIES=$mr.67_5.


MULT RESPONSE GROUPS=$mr.67_6 'q67.6  Indique as a��es de articula��o deste CRAS com os seguintes servi�os, programas ou institui��es existentes no munic�pio - Servi�os de educa��o '
(q67_6_1 q67_6_2  q67_6_3  q67_6_4 q67_6_5  q67_6_6 q67_6_7 q67_6_8 q67_6_0 q67_6_9 (1))
/FREQUENCIES=$mr.67_6.


MULT RESPONSE GROUPS=$mr.67_7 'q67.7  Indique as a��es de articula��o deste CRAS com os seguintes servi�os, programas ou institui��es existentes no munic�pio - �rg�os/Servi�os relacionados a trabalho e emprego  '
(q67_7_1 q67_7_2  q67_7_3  q67_7_4 q67_7_5  q67_7_6 q67_7_7 q67_7_8 q67_7_0 q67_7_9 (1))
/FREQUENCIES=$mr.67_7.

MULT RESPONSE GROUPS=$mr.67_8 'q67.8 Indique as a��es de articula��o deste CRAS com os seguintes servi�os, programas ou institui��es existentes no munic�pio - Movimentos sociais locais/Associa��es comunit�rias'
(q67_8_1 q67_8_2  q67_8_3  q67_8_4 q67_8_5  q67_8_6 q67_8_7 q67_8_8 q67_8_0 q67_8_9 (1))
/FREQUENCIES=$mr.67_8.


MULT RESPONSE GROUPS=$mr.67_9 'q67.9  Indique as a��es de articula��o deste CRAS com os seguintes servi�os, programas ou institui��es existentes no munic�pio - Conselho Tutelar  '
(q67_9_1 q67_9_2  q67_9_3  q67_9_4 q67_9_5  q67_9_6  q67_9_7  q67_9_8  q67_9_0  q67_9_9 (1))
/FREQUENCIES=$mr.67_9.


MULT RESPONSE GROUPS=$mr.67_10 'q67.10 Indique as a��es de articula��o deste CRAS com os seguintes servi�os, programas ou institui��es existentes no munic�pio - Judici�rio '
(q67_10_1  q67_10_2  q67_10_3  q67_10_4 q67_10_5  q67_10_6  q67_10_7  q67_10_8  q67_10_0  q67_10_9 (1))
/FREQUENCIES=$mr.67_10.



MULT RESPONSE GROUPS=$mr.68 'q68.Como se d� a refer�ncia e contra-refer�ncia para dos casos de viol�ncia e viola��es de direitos entre o CRAS e o CREAS'
(q68_0 q68_1 q68_2 q68_3 q68_4 q68_99 (1))
/FREQUENCIES=$mr.68.


**BLOCO 13 - GEST�O DE PESSOAS**


FREQUENCIES q69 
  /ORDER=ANALYSIS.   

