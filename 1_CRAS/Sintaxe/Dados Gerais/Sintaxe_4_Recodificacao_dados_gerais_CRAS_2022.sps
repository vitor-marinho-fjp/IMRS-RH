
*****recodificar

*q4*********************************

compute q5_1_velha = q5_1. 
compute q5_2_velha = q5_2. 
compute q5_3_velha = q5_3. 
compute q5_4_velha = q5_4. 
compute q5_5_velha = q5_5. 
compute q5_6_velha = q5_6. 
compute q5_7_velha = q5_7. 
compute q5_8_velha = q5_8. 
compute q5_9_velha = q5_9. 
compute q5_10_velha = q5_10. 
compute q5_11_velha = q5_11. 
compute q5_12_velha = q5_12. 
compute q5_13_velha = q5_13. 
compute q5_99_velha = q5_99. 
compute q6_1_velha = q6_1. 
compute q6_2_velha = q6_2. 
compute q6_3_velha = q6_3. 
compute q6_4_velha = q6_4. 
compute q6_5_velha = q6_5. 
compute q6_6_velha = q6_6. 
compute q6_7_velha = q6_7. 
compute q6_8_velha = q6_8. 
compute q6_9_velha = q6_9. 
compute q6_10_velha = q6_10. 
compute q6_11_velha = q6_11. 
compute q6_12_velha = q6_12. 
execute.
 


DO IF (q4=0).
RECODE q5_1 (ELSE=SYSMIS).
RECODE q5_2 (ELSE=SYSMIS).
RECODE q5_3 (ELSE=SYSMIS).
RECODE q5_4 (ELSE=SYSMIS).
RECODE q5_5 (ELSE=SYSMIS).
RECODE q5_6 (ELSE=SYSMIS).
RECODE q5_7 (ELSE=SYSMIS).
RECODE q5_8 (ELSE=SYSMIS).
RECODE q5_9 (ELSE=SYSMIS).
RECODE q5_10 (ELSE=SYSMIS).
RECODE q5_11 (ELSE=SYSMIS).
RECODE q5_12 (ELSE=SYSMIS).
RECODE q5_13 (ELSE=SYSMIS).
RECODE q5_99 (ELSE=SYSMIS).
RECODE q6_1  (ELSE=SYSMIS).
RECODE q6_2  (ELSE=SYSMIS).
RECODE q6_3  (ELSE=SYSMIS).
RECODE q6_4 (ELSE=SYSMIS).
RECODE q6_5 (ELSE=SYSMIS).
RECODE q6_6 (ELSE=SYSMIS).
RECODE q6_7 (ELSE=SYSMIS).
RECODE q6_8 (ELSE=SYSMIS).
RECODE q6_9 (ELSE=SYSMIS).
RECODE q6_10 (ELSE=SYSMIS).
RECODE q6_11 (ELSE=SYSMIS).
RECODE q6_12 (ELSE=SYSMIS).
END IF.
EXECUTE.


**q6*****************

DO IF (q6_1=1).
RECODE q6_2 (ELSE=SYSMIS).
RECODE q6_3 (ELSE=SYSMIS).
RECODE q6_4 (ELSE=SYSMIS).
RECODE q6_5 (ELSE=SYSMIS).
RECODE q6_6 (ELSE=SYSMIS).
RECODE q6_7 (ELSE=SYSMIS).
RECODE q6_8 (ELSE=SYSMIS).
RECODE q6_9 (ELSE=SYSMIS).
RECODE q6_10(ELSE=SYSMIS).
RECODE q6_11 (ELSE=SYSMIS).
RECODE q6_12 (ELSE=SYSMIS).
END IF.
EXECUTE.


**q9*****************

compute q9_1_velha = q9_1. 
compute q9_2_velha = q9_2. 
compute q9_3_velha = q9_3. 
compute q9_4_velha = q9_4. 
compute q9_5_velha = q9_5. 
compute q9_6_velha = q9_6. 
compute q9_7_velha = q9_6. 
compute q9_0_velha = q9_0. 
execute. 


DO IF (q9_0=1).
RECODE q9_1 (ELSE=SYSMIS).
RECODE q9_2 (ELSE=SYSMIS).
RECODE q9_3 (ELSE=SYSMIS).
RECODE q9_4 (ELSE=SYSMIS).
RECODE q9_5 (ELSE=SYSMIS).
RECODE q9_6 (ELSE=SYSMIS).
RECODE q9_7 (ELSE=SYSMIS).
END IF.
EXECUTE.

*****q14***************************************************************************************************************


RECODE q14_98 (1=1) (0=SYSMIS) (SYSMIS=SYSMIS).
EXECUTE.

compute q14_velha = q14 .
compute q14_1_1_velha = q14_1_1.
compute q14_1_2_velha = q14_1_2.
compute q14_1_3_velha = q14_1_3.
compute q14_2_1_velha = q14_2_1.
compute q14_2_2_velha = q14_2_2.
compute q14_2_3_velha = q14_2_3.
execute. 

DO IF (q14_98=1).
RECODE q14 (ELSE=SYSMIS).
RECODE q14_1_1 (ELSE=SYSMIS).
RECODE q14_1_2 (ELSE=SYSMIS).
RECODE q14_1_3 (ELSE=SYSMIS).
RECODE q14_2_1 (ELSE=SYSMIS).
RECODE q14_2_2 (ELSE=SYSMIS).
RECODE q14_2_3 (ELSE=SYSMIS).
END IF.
EXECUTE.

compute q14_testesoma=  SUM (q14_1_1,q14_1_2,q14_1_3,q14_2_1,q14_2_2,q14_2_3).
execute. 

compute q14= q14_testesoma.
execute. 


DO IF (MISSING(q14)).
RECODE q14_98 (1=1).
END IF.
EXECUTE.


DELETE VARIABLES q14_testesoma.

****q15*******************************************

compute q15_1_velha = q15_1. 
compute q15_2_velha = q15_2. 
compute q15_3_velha =q15_3. 
compute q15_4_velha = q15_4. 
compute q15_5_velha = q15_5. 
compute q15_6_velha = q15_6. 
compute q15_7_velha = q15_7. 
compute q15_8_velha = q15_8. 
compute q15_9_velha = q15_9. 
compute q15_10_velha =q15_10. 
compute q15_11_velha = q15_11. 
compute q15_12_velha = q15_12. 
compute q15_13_velha = q15_13.
compute q15_14_velha = q15_14. 
compute q15_15_velha = q15_15.
compute q15_16_velha = q15_16. 
compute q15_17_velha = q15_17. 
compute q15_18_velha =q15_18. 
compute q15_19_velha = q15_19. 
compute q15_20_velha = q15_20. 
compute q15_21_velha = q15_21. 
compute q15_22_velha = q15_22. 
compute q15_23_velha = q15_23. 
compute q15_24_velha = q15_24. 
compute q15_25_velha =q15_25. 
compute q15_26_velha = q15_26. 
compute q15_27_velha = q15_27. 
compute q15_99_velha = q15_99.
execute. 

DO IF (q15_1=1).
RECODE q15_2 (ELSE=SYSMIS).
RECODE q15_3 (ELSE=SYSMIS).
RECODE q15_4 (ELSE=SYSMIS).
RECODE q15_5 (ELSE=SYSMIS).
RECODE q15_6 (ELSE=SYSMIS).
RECODE q15_7 (ELSE=SYSMIS).
RECODE q15_8 (ELSE=SYSMIS).
RECODE q15_9 (ELSE=SYSMIS).
RECODE q15_10(ELSE=SYSMIS).
RECODE q15_11 (ELSE=SYSMIS).
RECODE q15_12 (ELSE=SYSMIS).
RECODE q15_13 (ELSE=SYSMIS).
RECODE q15_14 (ELSE=SYSMIS).
RECODE q15_15 (ELSE=SYSMIS).
RECODE q15_16 (ELSE=SYSMIS).
RECODE q15_17 (ELSE=SYSMIS).
RECODE q15_18 (ELSE=SYSMIS).
RECODE q15_19 (ELSE=SYSMIS).
RECODE q15_20 (ELSE=SYSMIS).
RECODE q15_21 (ELSE=SYSMIS).
RECODE q15_22 (ELSE=SYSMIS).
RECODE q15_23 (ELSE=SYSMIS).
RECODE q15_24 (ELSE=SYSMIS).
RECODE q15_25 (ELSE=SYSMIS).
RECODE q15_26 (ELSE=SYSMIS).
RECODE q15_27 (ELSE=SYSMIS).
RECODE q15_99 (ELSE=SYSMIS).
END IF.
EXECUTE.



*****q16**************

compute q16_1_velha = q16_1. 
compute q16_2_velha = q16_2. 
compute q16_3_velha =q16_3. 
compute q16_4_velha = q16_4. 
compute q16_5_velha = q16_5. 
compute q16_6_velha = q16_6. 
compute q16_7_velha = q16_7. 
compute q16_8_velha = q16_8. 
compute q16_9_velha = q16_9. 
compute q16_10_velha =q16_10. 
compute q16_11_velha = q16_11. 
compute q16_12_velha = q16_12. 
compute q16_13_velha = q16_13.
compute q16_14_velha = q16_14.
compute q16_99_velha = q16_99. 
execute. 


DO IF (q16_1=1).
RECODE q16_2 (ELSE=SYSMIS).
RECODE q16_3 (ELSE=SYSMIS).
RECODE q16_4 (ELSE=SYSMIS).
RECODE q16_5 (ELSE=SYSMIS).
RECODE q16_6 (ELSE=SYSMIS).
RECODE q16_7 (ELSE=SYSMIS).
RECODE q16_8 (ELSE=SYSMIS).
RECODE q16_9 (ELSE=SYSMIS).
RECODE q16_10(ELSE=SYSMIS).
RECODE q16_11 (ELSE=SYSMIS).
RECODE q16_12 (ELSE=SYSMIS).
RECODE q16_13 (ELSE=SYSMIS).
RECODE q16_14 (ELSE=SYSMIS).
RECODE q16_99 (ELSE=SYSMIS).
END IF.
EXECUTE.

****q17*************************************************

compute q18_1_velha = q18_1. 
compute q18_2_velha = q18_2. 
compute q18_3_velha =q18_3. 
compute q18_4_velha = q18_4. 
compute q18_5_velha = q18_5. 
compute q18_6_velha = q18_6. 
execute. 

DO IF (q17=0).
RECODE q18_1 (ELSE=SYSMIS).
RECODE q18_2(ELSE=SYSMIS).
RECODE q18_3 (ELSE=SYSMIS).
RECODE q18_4 (ELSE=SYSMIS).
RECODE q18_5 (ELSE=SYSMIS).
RECODE q18_6 (ELSE=SYSMIS).
END IF.
EXECUTE.


****q19*************************************************

compute q19_1_velha = q19_1. 
compute q19_2_velha = q19_2. 
compute q19_3_velha =q19_3. 
compute q19_0_velha = q19_0. 
execute. 


DO IF (q19_0=1).
RECODE q19_1 (ELSE=SYSMIS).
RECODE q19_2(ELSE=SYSMIS).
RECODE q19_3 (ELSE=SYSMIS).
END IF.
EXECUTE.



compute q20_1_velha = q20_1. 
compute q20_2_velha = q20_2. 
compute q20_3_velha =q20_3. 
compute q20_4_velha = q20_4. 
compute q20_5_velha =q20_5. 
compute q20_6_velha = q20_6. 
compute q21_velha = q21. 
compute q22_velha = q22. 
execute. 


DO IF (q19_0=1).
RECODE q20_1 (ELSE=SYSMIS).
RECODE q20_2(ELSE=SYSMIS).
RECODE q20_3 (ELSE=SYSMIS).
RECODE q20_4 (ELSE=SYSMIS).
RECODE q20_5 (ELSE=SYSMIS).
RECODE q20_6 (ELSE=SYSMIS).
RECODE q21 (ELSE=SYSMIS).
RECODE q22 (ELSE=SYSMIS).
END IF.
EXECUTE.




***q24******************************************************************************************

compute q25_1_velha = q25_1. 
compute q25_2_velha = q25_2. 
compute q25_3_velha =q25_3. 
compute q25_4_velha = q25_4. 
execute. 


DO IF (q24=0).
RECODE q25_1 (ELSE=SYSMIS).
RECODE q25_2(ELSE=SYSMIS).
RECODE q25_3 (ELSE=SYSMIS).
RECODE q25_4 (ELSE=SYSMIS).
END IF.
EXECUTE.

***q26***********************************************************

compute q26_1_velha = q26_1. 
compute q26_2_velha = q26_2. 
compute q26_3_velha =q26_3. 
compute q26_0_velha = q26_0. 
execute. 

DO IF (q26_0=1).
RECODE q26_1 (ELSE=SYSMIS).
RECODE q26_2(ELSE=SYSMIS).
RECODE q26_3 (ELSE=SYSMIS).
END IF.
EXECUTE.


compute q27_1_velha = q27_1. 
compute q27_2_velha = q27_2. 
compute q27_3_velha =q27_3. 
compute q27_4_velha = q27_4. 
execute. 


DO IF (q26_0=1).
RECODE q27_1 (ELSE=SYSMIS).
RECODE q27_2(ELSE=SYSMIS).
RECODE q27_3 (ELSE=SYSMIS).
RECODE q27_4 (ELSE=SYSMIS).
END IF.
EXECUTE.


***q28***********************************************************

compute q28_1_velha = q28_1. 
compute q28_2_velha = q28_2. 
compute q28_3_velha =q28_3. 
compute q28_4_velha = q28_4. 
compute q28_5_velha = q28_5. 
compute q28_6_velha =q28_6. 
compute q28_7_velha =q28_7. 
compute q28_8_velha =q28_8. 
compute q28_9_velha = q28_9. 
compute q28_0_velha = q28_0. 
compute q28_99_velha = q28_99. 
execute. 



DO IF (q28_0=1).
RECODE q28_1 (ELSE=SYSMIS).
RECODE q28_2(ELSE=SYSMIS).
RECODE q28_3 (ELSE=SYSMIS).
RECODE q28_4 (ELSE=SYSMIS).
RECODE q28_5 (ELSE=SYSMIS).
RECODE q28_6 (ELSE=SYSMIS).
RECODE q28_7 (ELSE=SYSMIS).
RECODE q28_8 (ELSE=SYSMIS).
RECODE q28_9 (ELSE=SYSMIS).
RECODE q28_99 (ELSE=SYSMIS).
END IF.
EXECUTE.


DO IF (q24=0   & q26_0=1).
RECODE q28_1 (ELSE=SYSMIS).
RECODE q28_2(ELSE=SYSMIS).
RECODE q28_3 (ELSE=SYSMIS).
RECODE q28_4 (ELSE=SYSMIS).
RECODE q28_5 (ELSE=SYSMIS).
RECODE q28_6 (ELSE=SYSMIS).
RECODE q28_7 (ELSE=SYSMIS).
RECODE q28_8 (ELSE=SYSMIS).
RECODE q28_9 (ELSE=SYSMIS).
RECODE q28_99 (ELSE=SYSMIS).
RECODE q28_0 (ELSE=SYSMIS).
END IF.
EXECUTE.


********q29** precisa incluir pulo a quest�o 29

compute q29_velha = q29. 

DO IF (q24=0).
RECODE q29  (ELSE=SYSMIS).
END IF.
EXECUTE.


**q30*************************************************************************************************

compute q31_velha = q31. 
compute q32_1_velha =q32_1. 
compute q32_2_velha = q32_2. 
compute q32_3_velha = q32_3. 
compute q32_4_velha =q32_4. 
compute q32_5_velha =q32_5. 
compute q32_6_velha = q32_6. 
compute q32_7_velha = q32_7. 
compute q32_99_velha =q32_99. 
compute q33_1_velha =q33_1. 
compute q33_2_velha = q33_2. 
compute q33_3_velha = q33_3. 
compute q33_4_velha =q33_4. 
compute q33_5_velha =q33_5. 
compute q33_6_velha = q33_6. 
compute q33_7_velha = q33_7. 
compute q33_8_velha =q33_8. 
compute q33_9_velha = q33_9. 
compute q33_10_velha = q33_10. 
compute q33_11_velha = q33_11. 
compute q33_12_velha = q33_12. 
compute q33_99_velha =q33_99.
execute. 


DO IF (q30=0).
RECODE q31 (ELSE=SYSMIS).
RECODE q32_1 (ELSE=SYSMIS).
RECODE q32_2 (ELSE=SYSMIS).
RECODE q32_3 (ELSE=SYSMIS).
RECODE q32_4 (ELSE=SYSMIS).
RECODE q32_5 (ELSE=SYSMIS).
RECODE q32_6 (ELSE=SYSMIS).
RECODE q32_7 (ELSE=SYSMIS).
RECODE q32_99 (ELSE=SYSMIS).
RECODE q33_1 (ELSE=SYSMIS).
RECODE q33_2 (ELSE=SYSMIS).
RECODE q33_3 (ELSE=SYSMIS).
RECODE q33_4 (ELSE=SYSMIS).
RECODE q33_5 (ELSE=SYSMIS).
RECODE q33_6 (ELSE=SYSMIS).
RECODE q33_7 (ELSE=SYSMIS).
RECODE q33_8 (ELSE=SYSMIS).
RECODE q33_9 (ELSE=SYSMIS).
RECODE q33_10 (ELSE=SYSMIS).
RECODE q33_11 (ELSE=SYSMIS).
RECODE q33_12 (ELSE=SYSMIS).
RECODE q33_99 (ELSE=SYSMIS).
END IF.
EXECUTE.



DO IF (q33_99 =1).
RECODE q33_1 (ELSE=SYSMIS).
RECODE q33_2 (ELSE=SYSMIS).
RECODE q33_3 (ELSE=SYSMIS).
RECODE q33_4 (ELSE=SYSMIS).
RECODE q33_5 (ELSE=SYSMIS).
RECODE q33_6 (ELSE=SYSMIS).
RECODE q33_7 (ELSE=SYSMIS).
RECODE q33_8 (ELSE=SYSMIS).
RECODE q33_9 (ELSE=SYSMIS).
RECODE q33_10 (ELSE=SYSMIS).
RECODE q33_11 (ELSE=SYSMIS).
RECODE q33_12 (ELSE=SYSMIS).
END IF.
EXECUTE.

**q34*************************************************************************************************

compute q35_1_velha =q35_1. 
compute q35_2_velha = q35_2. 
compute q35_3_velha = q35_3. 
compute q35_4_velha =q35_4. 
compute q36_1_velha =q36_1. 
compute q36_2_velha = q36_2. 
compute q36_3_velha = q36_3. 
compute q36_4_velha =q36_4. 
compute q36_5_velha =q36_5. 
compute q36_6_velha = q36_6. 
compute q36_7_velha =q36_7. 
compute q36_8_velha =q36_8. 
compute q36_9_velha = q36_9. 
compute q36_10_velha = q36_10. 
compute q36_11_velha =q36_11. 
compute q36_12_velha =q36_12. 
compute q36_99_velha =q36_99. 
compute q37_1_velha =q37_1. 
compute q37_2_velha = q37_2. 
compute q37_3_velha = q37_3. 
compute q37_4_velha =q37_4. 
compute q37_5_velha =q37_5. 
compute q37_6_velha = q37_6. 
compute q37_7_velha =q37_7. 
compute q37_8_velha =q37_8. 
compute q37_9_velha = q37_9. 
compute q37_99_velha =q37_99. 
compute q38_1_velha =q38_1. 
compute q38_2_velha = q38_2. 
compute q38_3_velha = q38_3. 
compute q38_4_velha =q38_4. 
compute q38_5_velha =q38_5. 
compute q38_6_velha = q38_6. 
compute q38_0_velha = q38_0. 
compute q38_99_velha =q38_99. 
compute q39_velha = q39. 
execute. 


DO IF (q34=0).
RECODE q35_1 (ELSE=SYSMIS).
RECODE q35_2 (ELSE=SYSMIS).
RECODE q35_3 (ELSE=SYSMIS).
RECODE q35_4 (ELSE=SYSMIS).
RECODE q36_1 (ELSE=SYSMIS).
RECODE q36_2 (ELSE=SYSMIS).
RECODE q36_3 (ELSE=SYSMIS).
RECODE q36_4 (ELSE=SYSMIS).
RECODE q36_5 (ELSE=SYSMIS).
RECODE q36_6 (ELSE=SYSMIS).
RECODE q36_7 (ELSE=SYSMIS).
RECODE q36_8 (ELSE=SYSMIS).
RECODE q36_9 (ELSE=SYSMIS).
RECODE q36_10 (ELSE=SYSMIS).
RECODE q36_11 (ELSE=SYSMIS).
RECODE q36_12 (ELSE=SYSMIS).
RECODE q36_99 (ELSE=SYSMIS).
RECODE q37_1 (ELSE=SYSMIS).
RECODE q37_2 (ELSE=SYSMIS).
RECODE q37_3 (ELSE=SYSMIS).
RECODE q37_4 (ELSE=SYSMIS).
RECODE q37_5 (ELSE=SYSMIS).
RECODE q37_6 (ELSE=SYSMIS).
RECODE q37_7 (ELSE=SYSMIS).
RECODE q37_8 (ELSE=SYSMIS).
RECODE q37_9 (ELSE=SYSMIS).
RECODE q37_99 (ELSE=SYSMIS).
END IF.
EXECUTE.



**q35**********************************************************************************************************
*****quem responde Benef�cio Eventual em situa��o de calamidade (inclui desastres e emerg�ncias) preenche q36

DO IF (q35_3=0).
RECODE q36_1 (ELSE=SYSMIS).
RECODE q36_2 (ELSE=SYSMIS).
RECODE q36_3 (ELSE=SYSMIS).
RECODE q36_4 (ELSE=SYSMIS).
RECODE q36_5 (ELSE=SYSMIS).
RECODE q36_6 (ELSE=SYSMIS).
RECODE q36_7 (ELSE=SYSMIS).
RECODE q36_8 (ELSE=SYSMIS).
RECODE q36_9 (ELSE=SYSMIS).
RECODE q36_10 (ELSE=SYSMIS).
RECODE q36_11 (ELSE=SYSMIS).
RECODE q36_12 (ELSE=SYSMIS).
RECODE q36_99 (ELSE=SYSMIS).
END IF.
EXECUTE.


****quem responde Benef�cio Eventual em situa��o de vulnerabilidade tempor�ria preenche q37

DO IF (q35_4=0).
RECODE q37_1 (ELSE=SYSMIS).
RECODE q37_2 (ELSE=SYSMIS).
RECODE q37_3 (ELSE=SYSMIS).
RECODE q37_4 (ELSE=SYSMIS).
RECODE q37_5 (ELSE=SYSMIS).
RECODE q37_6 (ELSE=SYSMIS).
RECODE q37_7 (ELSE=SYSMIS).
RECODE q37_8 (ELSE=SYSMIS).
RECODE q37_9 (ELSE=SYSMIS).
RECODE q37_99 (ELSE=SYSMIS).
END IF.
EXECUTE.



**q38**********************************************************************************************************

DO IF (q38_0=1).
RECODE q38_1 (ELSE=SYSMIS).
RECODE q38_2 (ELSE=SYSMIS).
RECODE q38_3 (ELSE=SYSMIS).
RECODE q38_4 (ELSE=SYSMIS).
RECODE q38_5 (ELSE=SYSMIS).
RECODE q38_6 (ELSE=SYSMIS).
RECODE q38_99 (ELSE=SYSMIS).
END IF.
EXECUTE.

**q39**********************************************************************************************************

compute q40_1_velha =q40_1. 
compute q40_2_velha = q40_2. 
compute q40_3_velha = q40_3. 
compute q40_4_velha =q40_4. 
compute q40_5_velha =q40_5. 
compute q40_6_velha = q40_6. 
compute q40_7_velha =q40_7. 
compute q40_8_velha = q40_8. 
compute q40_9_velha = q40_9. 
compute q40_10_velha =q40_10. 
compute q40_11_velha =q40_11. 
compute q40_12_velha = q40_12. 
compute q40_13_velha =q40_13. 
compute q40_14_velha =q40_14. 
compute q40_15_velha = q40_15. 
compute q40_16_velha =q40_16. 
compute q40_17_velha =q40_17. 
compute q40_18_velha = q40_18. 
compute q40_99_velha =q40_99. 
compute q41_1_velha =q41_1. 
compute q41_2_velha = q41_2. 
compute q41_3_velha = q41_3. 
compute q41_98_velha = q41_98. 
compute q42_1_velha =q42_1. 
compute q42_1_1_velha = q42_1_1. 
compute q42_2_velha =q42_2. 
compute q42_2_1_velha = q42_2_1. 
compute q42_3_velha =q42_3. 
compute q42_3_1_velha = q42_3_1. 
compute q42_4_velha =q42_4. 
compute q42_4_1_velha = q42_4_1. 
compute q42_5_velha =q42_5. 
compute q42_5_1_velha = q42_5_1. 
compute q43_0_velha =q43_0. 
compute q43_1_velha =q43_1. 
compute q43_2_velha = q43_2. 
compute q43_3_velha = q43_3. 
compute q43_99_velha =q43_99. 
compute q44_1_velha =q44_1. 
compute q44_2_velha = q44_2. 
compute q44_3_velha = q44_3. 
compute q44_4_velha =q44_4. 
compute q44_5_velha =q44_5. 
compute q44_99_velha = q44_99.
execute. 

DO IF (q39=0).
RECODE q40_1 (ELSE=SYSMIS).
RECODE q40_2 (ELSE=SYSMIS).
RECODE q40_3 (ELSE=SYSMIS).
RECODE q40_4 (ELSE=SYSMIS).
RECODE q40_5 (ELSE=SYSMIS).
RECODE q40_6 (ELSE=SYSMIS).
RECODE q40_7 (ELSE=SYSMIS).
RECODE q40_8 (ELSE=SYSMIS).
RECODE q40_8 (ELSE=SYSMIS).
RECODE q40_10 (ELSE=SYSMIS).
RECODE q40_11 (ELSE=SYSMIS).
RECODE q40_12 (ELSE=SYSMIS).
RECODE q40_13 (ELSE=SYSMIS).
RECODE q40_14 (ELSE=SYSMIS).
RECODE q40_15 (ELSE=SYSMIS).
RECODE q40_16 (ELSE=SYSMIS).
RECODE q40_17 (ELSE=SYSMIS).
RECODE q40_18 (ELSE=SYSMIS).
RECODE q40_99 (ELSE=SYSMIS).
RECODE q41_1 (ELSE=SYSMIS).
RECODE q41_2 (ELSE=SYSMIS).
RECODE q41_3 (ELSE=SYSMIS).
RECODE q41_98 (ELSE=SYSMIS).
RECODE q42_1 (ELSE=SYSMIS).
RECODE q42_1_1 (ELSE=SYSMIS).
RECODE q42_2 (ELSE=SYSMIS).
RECODE q42_2_1 (ELSE=SYSMIS).
RECODE q42_3 (ELSE=SYSMIS).
RECODE q42_3_1 (ELSE=SYSMIS).
RECODE q42_4 (ELSE=SYSMIS).
RECODE q42_4_1 (ELSE=SYSMIS).
RECODE q42_5 (ELSE=SYSMIS).
RECODE q42_5_1 (ELSE=SYSMIS).
RECODE q43_0 (ELSE=SYSMIS).
RECODE q43_1 (ELSE=SYSMIS).
RECODE q43_2 (ELSE=SYSMIS).
RECODE q43_3 (ELSE=SYSMIS).
RECODE q43_99 (ELSE=SYSMIS).
RECODE q44_1 (ELSE=SYSMIS).
RECODE q44_2 (ELSE=SYSMIS).
RECODE q44_3 (ELSE=SYSMIS).
RECODE q44_4 (ELSE=SYSMIS).
RECODE q44_5 (ELSE=SYSMIS).
RECODE q44_99 (ELSE=SYSMIS).
END IF.
EXECUTE.



***q43************************************************************************************

DO IF (q43_0=1).
RECODE q43_1 (ELSE=SYSMIS).
RECODE q43_2 (ELSE=SYSMIS).
RECODE q43_3 (ELSE=SYSMIS).
RECODE q43_99 (ELSE=SYSMIS).
END IF.
EXECUTE.

**q45************************************************************************************

compute q45_0_velha = q45_0. 
compute q45_1_velha =q45_1. 
compute q45_2_velha = q45_2. 
compute q45_3_velha = q45_3. 
compute q45_4_velha =q45_4. 
compute q45_5_velha =q45_5. 
compute q45_6_velha = q45_6. 
compute q45_7_velha =q45_7. 
compute q45_8_velha = q45_8. 
compute q45_9_velha = q45_9. 
compute q45_10_velha =q45_10. 
compute q45_11_velha =q45_11. 
compute q45_12_velha = q45_12. 
compute q45_13_velha =q45_13. 
compute q45_99_velha =q45_99. 
execute. 


DO IF (q45_0=1).
RECODE q45_1 (ELSE=SYSMIS).
RECODE q45_2 (ELSE=SYSMIS).
RECODE q45_3 (ELSE=SYSMIS).
RECODE q45_4 (ELSE=SYSMIS).
RECODE q45_5 (ELSE=SYSMIS).
RECODE q45_6 (ELSE=SYSMIS).
RECODE q45_7 (ELSE=SYSMIS).
RECODE q45_8 (ELSE=SYSMIS).
RECODE q45_9 (ELSE=SYSMIS).
RECODE q45_10 (ELSE=SYSMIS).
RECODE q45_11 (ELSE=SYSMIS).
RECODE q45_12 (ELSE=SYSMIS).
RECODE q45_13 (ELSE=SYSMIS).
RECODE q45_99 (ELSE=SYSMIS).
END IF.
EXECUTE.


***q46*******************************************************************************************

compute q46_1_velha =q46_1. 
compute q46_2_velha = q46_2. 
compute q46_3_velha = q46_3. 
compute q46_4_velha =q46_4. 
compute q46_5_velha =q46_5. 
compute q46_6_velha = q46_6. 
compute q46_7_velha =q46_7. 
compute q46_8_velha = q46_8. 
compute q46_9_velha = q46_9. 
compute q46_10_velha =q46_10. 
compute q46_11_velha =q46_11. 
compute q46_12_velha = q46_12. 
compute q46_13_velha =q46_13. 
compute q46_14_velha =q46_14. 
compute q46_15_velha =q46_15. 
compute q46_16_velha =q46_16. 
compute q46_99_velha =q46_99. 
execute. 


DO IF (q46_1=1).
RECODE q46_2 (ELSE=SYSMIS).
RECODE q46_3 (ELSE=SYSMIS).
RECODE q46_4 (ELSE=SYSMIS).
RECODE q46_5 (ELSE=SYSMIS).
RECODE q46_6 (ELSE=SYSMIS).
RECODE q46_7 (ELSE=SYSMIS).
RECODE q46_8 (ELSE=SYSMIS).
RECODE q46_9 (ELSE=SYSMIS).
RECODE q46_10 (ELSE=SYSMIS).
RECODE q46_11 (ELSE=SYSMIS).
RECODE q46_12 (ELSE=SYSMIS).
RECODE q46_13 (ELSE=SYSMIS).
RECODE q46_14 (ELSE=SYSMIS).
RECODE q46_15 (ELSE=SYSMIS).
RECODE q46_16 (ELSE=SYSMIS).
RECODE q46_99 (ELSE=SYSMIS).
END IF.
EXECUTE.

***q48*******************************************************************************************

compute q48_0_velha = q48_0. 
compute q48_1_velha =q48_1. 
compute q48_2_velha = q48_2. 
compute q48_3_velha = q48_3. 
compute q48_4_velha =q48_4. 
compute q48_5_velha =q48_5. 
compute q48_6_velha = q48_6. 
compute q48_7_velha =q48_7. 
compute q48_8_velha = q48_8. 
compute q48_9_velha = q48_9. 
compute q48_10_velha =q48_10. 
compute q48_11_velha =q48_11. 
compute q48_12_velha = q48_12. 
compute q48_13_velha =q48_13. 
compute q48_14_velha =q48_14. 
compute q48_15_velha =q48_15. 
compute q48_99_velha =q48_99. 
compute q49_1_velha =q49_1. 
compute q49_2_velha = q49_2. 
compute q49_3_velha = q49_3. 
compute q49_4_velha =q49_4. 
compute q49_99_velha =q49_99. 
compute q49_0_velha = q49_0. 
compute q49_98_velha =q49_98. 
compute q50_1_velha =q50_1. 
compute q50_2_velha = q50_2. 
compute q50_99_velha =q50_99. 
compute q50_0_velha = q50_0. 
compute q50_98_velha = q50_98. 
execute. 


DO IF (q48_0=1).
RECODE q48_1 (ELSE=SYSMIS).
RECODE q48_2 (ELSE=SYSMIS).
RECODE q48_3 (ELSE=SYSMIS).
RECODE q48_4 (ELSE=SYSMIS).
RECODE q48_5 (ELSE=SYSMIS).
RECODE q48_6 (ELSE=SYSMIS).
RECODE q48_7 (ELSE=SYSMIS).
RECODE q48_8 (ELSE=SYSMIS).
RECODE q48_9 (ELSE=SYSMIS).
RECODE q48_10 (ELSE=SYSMIS).
RECODE q48_11 (ELSE=SYSMIS).
RECODE q48_12 (ELSE=SYSMIS).
RECODE q48_13 (ELSE=SYSMIS).
RECODE q48_14 (ELSE=SYSMIS).
RECODE q48_15 (ELSE=SYSMIS).
RECODE q48_99 (ELSE=SYSMIS).
END IF.
EXECUTE.



DO IF (q48_0=1).
RECODE q49_1 (ELSE=SYSMIS).
RECODE q49_2 (ELSE=SYSMIS).
RECODE q49_3 (ELSE=SYSMIS).
RECODE q49_4 (ELSE=SYSMIS).
RECODE q49_99 (ELSE=SYSMIS).
RECODE q49_0 (ELSE=SYSMIS).
RECODE q49_98 (ELSE=SYSMIS).
RECODE q50_1 (ELSE=SYSMIS).
RECODE q50_2 (ELSE=SYSMIS).
RECODE q50_99 (ELSE=SYSMIS).
RECODE q50_0 (ELSE=SYSMIS).
RECODE q50_98 (ELSE=SYSMIS).
END IF.
EXECUTE.



*q51****************************************************************************************************

compute q51_0_velha =q51_0. 
compute q51_1_velha =q51_1. 
compute q51_2_velha = q51_2. 
compute q51_3_velha = q51_3. 
compute q51_4_velha =q51_4. 
compute q51_5_velha =q51_5. 
compute q51_6_velha = q51_6. 
compute q51_7_velha =q51_7. 
compute q51_8_velha = q51_8. 
compute q51_9_velha = q51_9. 
compute q51_10_velha =q51_10. 
compute q51_99_velha =q51_99. 
compute q52_1_velha =q52_1. 
compute q52_2_velha = q52_2. 
compute q52_3_velha = q52_3. 
compute q52_99_velha =q52_99. 
compute q53_velha =q53. 
compute q54_1_velha =q54_1. 
compute q54_2_velha = q54_2. 
compute q54_3_velha = q54_3. 
compute q54_4_velha =q54_4. 
compute q54_5_velha =q54_5. 
compute q54_99_velha = q54_99. 
execute. 



DO IF (q51_0=1).
RECODE q51_1 (ELSE=SYSMIS).
RECODE q51_2 (ELSE=SYSMIS).
RECODE q51_3 (ELSE=SYSMIS).
RECODE q51_4 (ELSE=SYSMIS).
RECODE q51_5 (ELSE=SYSMIS).
RECODE q51_6 (ELSE=SYSMIS).
RECODE q51_7 (ELSE=SYSMIS).
RECODE q51_8 (ELSE=SYSMIS).
RECODE q51_9 (ELSE=SYSMIS).
RECODE q51_10 (ELSE=SYSMIS).
RECODE q51_99 (ELSE=SYSMIS).
RECODE q52_1 (ELSE=SYSMIS).
RECODE q52_2 (ELSE=SYSMIS).
RECODE q52_3 (ELSE=SYSMIS).
RECODE q52_99 (ELSE=SYSMIS).
RECODE q53 (ELSE=SYSMIS).
RECODE q54_1 (ELSE=SYSMIS).
RECODE q54_2 (ELSE=SYSMIS).
RECODE q54_3 (ELSE=SYSMIS).
RECODE q54_4 (ELSE=SYSMIS).
RECODE q54_5 (ELSE=SYSMIS).
RECODE q54_99 (ELSE=SYSMIS).
END IF.
EXECUTE.

*q54******************************************************************************************************************
****(Admite m�ltiplas respostas, exceto se marcar a primeira op��o) - n�o a �ltima

DO IF (q54_1=1).
RECODE q54_2 (ELSE=SYSMIS).
RECODE q54_3 (ELSE=SYSMIS).
RECODE q54_4 (ELSE=SYSMIS).
RECODE q54_5 (ELSE=SYSMIS).
RECODE q54_99 (ELSE=SYSMIS).
END IF.
EXECUTE.



***q55**********programa crian�a feliz***********bloco 11*******************************************************

compute q56_1_velha =q56_1. 
compute q56_2_velha =q56_2. 
compute q56_3_velha =q56_3. 
compute q56_4_velha =q56_4. 
compute q56_5_velha =q56_5. 
compute q56_0_velha =q56_0. 
compute q57_velha =q57. 
compute q58_1_velha =q58_1. 
compute q58_2_velha =q58_2. 
compute q58_3_velha =q58_3. 
compute q58_4_velha =q58_4. 
compute q58_5_velha =q58_5. 
compute q59_0_velha =q59_0. 
compute q59_1_velha =q59_1. 
compute q59_2_velha =q59_2. 
compute q59_3_velha =q59_3. 
compute q59_4_velha =q59_4. 
compute q59_5_velha =q59_5. 
compute q59_6_velha =q59_6. 
compute q59_7_velha =q59_7. 
compute q59_8_velha =q59_8. 
compute q59_9_velha =q59_9. 
compute q59_10_velha =q59_10. 
compute q59_11_velha =q59_11. 
compute q59_99_velha = q59_99. 
execute. 


DO IF (q55=0).
RECODE q56_1 (ELSE=SYSMIS).
RECODE q56_2 (ELSE=SYSMIS).
RECODE q56_3 (ELSE=SYSMIS).
RECODE q56_4 (ELSE=SYSMIS).
RECODE q56_5 (ELSE=SYSMIS).
RECODE q56_0 (ELSE=SYSMIS).
RECODE q57 (ELSE=SYSMIS).
RECODE q58_1 (ELSE=SYSMIS).
RECODE q58_2 (ELSE=SYSMIS).
RECODE q58_3 (ELSE=SYSMIS).
RECODE q58_4 (ELSE=SYSMIS).
RECODE q58_5 (ELSE=SYSMIS).
RECODE q59_0 (ELSE=SYSMIS).
RECODE q59_1 (ELSE=SYSMIS).
RECODE q59_2 (ELSE=SYSMIS).
RECODE q59_3 (ELSE=SYSMIS).
RECODE q59_4 (ELSE=SYSMIS).
RECODE q59_5 (ELSE=SYSMIS).
RECODE q59_6 (ELSE=SYSMIS).
RECODE q59_7 (ELSE=SYSMIS).
RECODE q59_8 (ELSE=SYSMIS).
RECODE q59_9 (ELSE=SYSMIS).
RECODE q59_10 (ELSE=SYSMIS).
RECODE q59_11 (ELSE=SYSMIS).
RECODE q59_99 (ELSE=SYSMIS).
END IF.
EXECUTE.



DO IF (q59_0 =1).
RECODE q59_1 (ELSE=SYSMIS).
RECODE q59_2 (ELSE=SYSMIS).
RECODE q59_3 (ELSE=SYSMIS).
RECODE q59_4 (ELSE=SYSMIS).
RECODE q59_5 (ELSE=SYSMIS).
RECODE q59_6 (ELSE=SYSMIS).
RECODE q59_7 (ELSE=SYSMIS).
RECODE q59_8 (ELSE=SYSMIS).
RECODE q59_9 (ELSE=SYSMIS).
RECODE q59_10 (ELSE=SYSMIS).
RECODE q59_11 (ELSE=SYSMIS).
RECODE q59_99 (ELSE=SYSMIS).
END IF.
EXECUTE.





********q62**************gest�o e territorio******bloco 12***********************************************************************************************************************

compute q63_1_velha =q63_1. 
compute q63_2_velha =q63_2. 
compute q63_3_velha =q63_3. 
compute q63_4_velha =q63_4. 
compute q63_99_velha =q63_99. 
execute. 



DO IF (q62=0).
RECODE q63_1 (ELSE=SYSMIS).
RECODE q63_2 (ELSE=SYSMIS).
RECODE q63_3 (ELSE=SYSMIS).
RECODE q63_4 (ELSE=SYSMIS).
RECODE q63_99 (ELSE=SYSMIS).
END IF.
EXECUTE.

********q65***********************************************************************************************************


compute q65_1_1_velha =q65_1_1. 
compute q65_1_2_velha = q65_1_2. 
compute q65_1_3_velha = q65_1_3. 
compute q65_1_4_velha =q65_1_4. 
compute q65_1_99_velha =q65_1_99. 
compute q65_1_0_velha = q65_1_0. 
compute q65_1_5_velha =q65_1_5. 
compute q65_2_1_velha =q65_2_1. 
compute q65_2_2_velha = q65_2_2. 
compute q65_2_3_velha = q65_2_3. 
compute q65_2_4_velha =q65_2_4. 
compute q65_2_99_velha =q65_2_99. 
compute q65_2_0_velha = q65_2_0. 
compute q65_2_5_velha =q65_2_5. 
compute q65_3_1_velha =q65_3_1. 
compute q65_3_2_velha = q65_3_2. 
compute q65_3_3_velha = q65_3_3. 
compute q65_3_4_velha =q65_3_4. 
compute q65_3_99_velha =q65_3_99. 
compute q65_3_0_velha = q65_3_0. 
compute q65_3_5_velha =q65_3_5. 
compute q65_4_1_velha =q65_4_1. 
compute q65_4_2_velha = q65_4_2. 
compute q65_4_3_velha = q65_4_3. 
compute q65_4_4_velha =q65_4_4. 
compute q65_4_99_velha =q65_4_99. 
compute q65_4_0_velha = q65_4_0. 
compute q65_4_5_velha =q65_4_5. 
compute q65_5_1_velha =q65_5_1. 
compute q65_5_2_velha = q65_5_2. 
compute q65_5_3_velha = q65_5_3. 
compute q65_5_4_velha =q65_5_4. 
compute q65_5_99_velha =q65_5_99. 
compute q65_5_0_velha = q65_5_0. 
compute q65_5_5_velha =q65_5_5. 
compute q65_6_1_velha =q65_6_1. 
compute q65_6_2_velha = q65_6_2. 
compute q65_6_3_velha = q65_6_3. 
compute q65_6_4_velha =q65_6_4. 
compute q65_6_99_velha =q65_6_99. 
compute q65_6_0_velha = q65_6_0. 
compute q65_6_5_velha =q65_6_5. 
compute q65_7_1_velha =q65_7_1. 
compute q65_7_2_velha = q65_7_2. 
compute q65_7_3_velha = q65_7_3. 
compute q65_7_4_velha =q65_7_4. 
compute q65_7_99_velha =q65_7_99. 
compute q65_7_0_velha = q65_7_0. 
compute q65_7_5_velha =q65_7_5. 
compute q65_1_1_velha =q65_1_1. 
compute q65_1_2_velha = q65_1_2. 
compute q65_1_3_velha = q65_1_3. 
compute q65_1_4_velha =q65_1_4. 
compute q65_1_99_velha =q65_1_99. 
compute q65_1_0_velha = q65_1_0. 
compute q65_1_5_velha =q65_1_5. 
compute q65_8_1_velha =q65_8_1. 
compute q65_8_2_velha = q65_8_2. 
compute q65_8_3_velha = q65_8_3. 
compute q65_8_4_velha =q65_8_4. 
compute q65_8_99_velha =q65_8_99. 
compute q65_8_0_velha = q65_8_0. 
compute q65_8_5_velha =q65_8_5. 
compute q65_9_1_velha =q65_9_1. 
compute q65_9_2_velha = q65_9_2. 
compute q65_9_3_velha = q65_9_3. 
compute q65_9_4_velha =q65_9_4. 
compute q65_9_99_velha =q65_9_99. 
compute q65_9_0_velha = q65_9_0. 
compute q65_9_5_velha =q65_9_5. 
compute q65_10_1_velha =q65_10_1. 
compute q65_10_2_velha = q65_10_2. 
compute q65_10_3_velha = q65_10_3. 
compute q65_10_4_velha =q65_10_4. 
compute q65_10_99_velha =q65_10_99. 
compute q65_10_0_velha = q65_10_0. 
compute q65_10_5_velha =q65_10_5. 
compute q65_11_1_velha =q65_11_1. 
compute q65_11_2_velha = q65_11_2. 
compute q65_11_3_velha = q65_11_3. 
compute q65_11_4_velha =q65_11_4. 
compute q65_11_99_velha =q65_11_99. 
compute q65_11_0_velha = q65_11_0. 
compute q65_11_5_velha =q65_11_5. 
compute q65_12_1_velha =q65_12_1. 
compute q65_12_2_velha = q65_12_2. 
compute q65_12_3_velha = q65_12_3. 
compute q65_12_4_velha =q65_12_4. 
compute q65_12_99_velha =q65_12_99. 
compute q65_12_0_velha = q65_12_0. 
compute q65_12_5_velha =q65_12_5. 
compute q65_13_1_velha =q65_13_1. 
compute q65_13_2_velha = q65_13_2. 
compute q65_13_3_velha = q65_13_3. 
compute q65_13_4_velha =q65_13_4. 
compute q65_13_99_velha =q65_13_99. 
compute q65_13_0_velha = q65_13_0. 
compute q65_13_5_velha =q65_13_5. 
compute q65_14_1_velha =q65_14_1. 
compute q65_14_2_velha = q65_14_2. 
compute q65_14_3_velha = q65_14_3. 
compute q65_14_4_velha =q65_14_4. 
compute q65_14_99_velha =q65_14_99. 
compute q65_14_0_velha = q65_14_0. 
compute q65_14_5_velha =q65_14_5. 
compute q65_15_1_velha =q65_15_1. 
compute q65_15_2_velha = q65_15_2. 
compute q65_15_3_velha = q65_15_3. 
compute q65_15_4_velha =q65_15_4. 
compute q65_15_99_velha =q65_15_99. 
compute q65_15_0_velha = q65_15_0. 
compute q65_15_5_velha =q65_15_5. 
compute q65_16_1_velha =q65_16_1. 
compute q65_16_2_velha = q65_16_2. 
compute q65_16_3_velha = q65_16_3. 
compute q65_16_4_velha =q65_16_4. 
compute q65_16_99_velha =q65_16_99. 
compute q65_16_0_velha = q65_16_0. 
compute q65_16_5_velha =q65_16_5. 
execute. 


DO IF (q65_1_0=1 | q65_1_5=1).
RECODE q65_1_1 (ELSE=SYSMIS).
RECODE q65_1_2 (ELSE=SYSMIS).
RECODE q65_1_3 (ELSE=SYSMIS).
RECODE q65_1_4 (ELSE=SYSMIS).
RECODE q65_1_99 (ELSE=SYSMIS).
END IF.
EXECUTE.



DO IF (q65_2_0=1 | q65_2_5=1).
RECODE q65_2_1 (ELSE=SYSMIS).
RECODE q65_2_2 (ELSE=SYSMIS).
RECODE q65_2_3 (ELSE=SYSMIS).
RECODE q65_2_4 (ELSE=SYSMIS).
RECODE q65_2_99 (ELSE=SYSMIS).
END IF.
EXECUTE.


DO IF (q65_3_0=1 | q65_3_5=1).
RECODE q65_3_1 (ELSE=SYSMIS).
RECODE q65_3_2 (ELSE=SYSMIS).
RECODE q65_3_3 (ELSE=SYSMIS).
RECODE q65_3_4 (ELSE=SYSMIS).
RECODE q65_3_99 (ELSE=SYSMIS).
END IF.
EXECUTE.


DO IF (q65_4_0=1 | q65_4_5=1).
RECODE q65_4_1 (ELSE=SYSMIS).
RECODE q65_4_2 (ELSE=SYSMIS).
RECODE q65_4_3 (ELSE=SYSMIS).
RECODE q65_4_4 (ELSE=SYSMIS).
RECODE q65_4_99 (ELSE=SYSMIS).
END IF.
EXECUTE.


DO IF (q65_5_0=1 | q65_5_5=1).
RECODE q65_5_1 (ELSE=SYSMIS).
RECODE q65_5_2 (ELSE=SYSMIS).
RECODE q65_5_3 (ELSE=SYSMIS).
RECODE q65_5_4 (ELSE=SYSMIS).
RECODE q65_5_99 (ELSE=SYSMIS).
END IF.
EXECUTE.


DO IF (q65_6_0=1 | q65_6_5=1).
RECODE q65_6_1 (ELSE=SYSMIS).
RECODE q65_6_2 (ELSE=SYSMIS).
RECODE q65_6_3 (ELSE=SYSMIS).
RECODE q65_6_4 (ELSE=SYSMIS).
RECODE q65_6_99 (ELSE=SYSMIS).
END IF.
EXECUTE.

DO IF (q65_7_0=1 | q65_7_5=1).
RECODE q65_7_1 (ELSE=SYSMIS).
RECODE q65_7_2 (ELSE=SYSMIS).
RECODE q65_7_3 (ELSE=SYSMIS).
RECODE q65_7_4 (ELSE=SYSMIS).
RECODE q65_7_99 (ELSE=SYSMIS).
END IF.
EXECUTE.


DO IF (q65_8_0=1 | q65_8_5=1).
RECODE q65_8_1 (ELSE=SYSMIS).
RECODE q65_8_2 (ELSE=SYSMIS).
RECODE q65_8_3 (ELSE=SYSMIS).
RECODE q65_8_4 (ELSE=SYSMIS).
RECODE q65_8_99 (ELSE=SYSMIS).
END IF.
EXECUTE.


DO IF (q65_9_0=1 | q65_9_5=1).
RECODE q65_9_1 (ELSE=SYSMIS).
RECODE q65_9_2 (ELSE=SYSMIS).
RECODE q65_9_3 (ELSE=SYSMIS).
RECODE q65_9_4 (ELSE=SYSMIS).
RECODE q65_9_99 (ELSE=SYSMIS).
END IF.
EXECUTE.


DO IF (q65_10_0=1 | q65_10_5=1).
RECODE q65_10_1 (ELSE=SYSMIS).
RECODE q65_10_2 (ELSE=SYSMIS).
RECODE q65_10_3 (ELSE=SYSMIS).
RECODE q65_10_4 (ELSE=SYSMIS).
RECODE q65_10_99 (ELSE=SYSMIS).
END IF.
EXECUTE.


DO IF (q65_11_0=1 | q65_11_5=1).
RECODE q65_11_1 (ELSE=SYSMIS).
RECODE q65_11_2 (ELSE=SYSMIS).
RECODE q65_11_3 (ELSE=SYSMIS).
RECODE q65_11_4 (ELSE=SYSMIS).
RECODE q65_11_99 (ELSE=SYSMIS).
END IF.
EXECUTE.


DO IF (q65_12_0=1 | q65_12_5=1).
RECODE q65_12_1 (ELSE=SYSMIS).
RECODE q65_12_2 (ELSE=SYSMIS).
RECODE q65_12_3 (ELSE=SYSMIS).
RECODE q65_12_4 (ELSE=SYSMIS).
RECODE q65_12_99 (ELSE=SYSMIS).
END IF.
EXECUTE.

DO IF (q65_13_0=1 | q65_13_5=1).
RECODE q65_13_1 (ELSE=SYSMIS).
RECODE q65_13_2 (ELSE=SYSMIS).
RECODE q65_13_3 (ELSE=SYSMIS).
RECODE q65_13_4 (ELSE=SYSMIS).
RECODE q65_13_99 (ELSE=SYSMIS).
END IF.
EXECUTE.


DO IF (q65_14_0=1 | q65_14_5=1).
RECODE q65_14_1 (ELSE=SYSMIS).
RECODE q65_14_2 (ELSE=SYSMIS).
RECODE q65_14_3 (ELSE=SYSMIS).
RECODE q65_14_4 (ELSE=SYSMIS).
RECODE q65_14_99 (ELSE=SYSMIS).
END IF.
EXECUTE.


DO IF (q65_15_0=1 | q65_15_5=1).
RECODE q65_15_1 (ELSE=SYSMIS).
RECODE q65_15_2 (ELSE=SYSMIS).
RECODE q65_15_3 (ELSE=SYSMIS).
RECODE q65_15_4 (ELSE=SYSMIS).
RECODE q65_15_99 (ELSE=SYSMIS).
END IF.
EXECUTE.


DO IF (q65_16_0=1 | q65_16_5=1).
RECODE q65_16_1 (ELSE=SYSMIS).
RECODE q65_16_2 (ELSE=SYSMIS).
RECODE q65_16_3 (ELSE=SYSMIS).
RECODE q65_16_4 (ELSE=SYSMIS).
RECODE q65_16_99 (ELSE=SYSMIS).
END IF.
EXECUTE.






********q67*******************************************************************************************************************************************

compute q67_1_0_velha =q67_1_0. 
compute q67_1_1_velha =q67_1_1. 
compute q67_1_2_velha = q67_1_2. 
compute q67_1_3_velha = q67_1_3. 
compute q67_1_4_velha =q67_1_4. 
compute q67_1_5_velha =q67_1_5. 
compute q67_1_6_velha = q67_1_6. 
compute q67_1_7_velha =q67_1_7. 
compute q67_1_8_velha =q67_1_8. 
compute q67_1_9_velha =q67_1_9. 
compute q67_2_0_velha =q67_2_0. 
compute q67_2_1_velha =q67_2_1. 
compute q67_2_2_velha = q67_2_2. 
compute q67_2_3_velha = q67_2_3. 
compute q67_2_4_velha =q67_2_4. 
compute q67_2_5_velha =q67_2_5. 
compute q67_2_6_velha = q67_2_6. 
compute q67_2_7_velha =q67_2_7. 
compute q67_2_8_velha =q67_2_8. 
compute q67_2_9_velha =q67_2_9. 
compute q67_3_0_velha =q67_3_0. 
compute q67_3_1_velha =q67_3_1. 
compute q67_3_2_velha = q67_3_2. 
compute q67_3_3_velha = q67_3_3. 
compute q67_3_4_velha =q67_3_4. 
compute q67_3_5_velha =q67_3_5. 
compute q67_3_6_velha = q67_3_6. 
compute q67_3_7_velha =q67_3_7. 
compute q67_3_8_velha =q67_3_8. 
compute q67_3_9_velha =q67_3_9. 
compute q67_4_0_velha =q67_4_0. 
compute q67_4_1_velha =q67_4_1. 
compute q67_4_2_velha = q67_4_2. 
compute q67_4_3_velha = q67_4_3. 
compute q67_4_4_velha =q67_4_4. 
compute q67_4_5_velha =q67_4_5. 
compute q67_4_6_velha = q67_4_6. 
compute q67_4_7_velha =q67_4_7. 
compute q67_4_8_velha =q67_4_8. 
compute q67_4_9_velha =q67_4_9. 
compute q67_5_0_velha =q67_5_0. 
compute q67_5_1_velha =q67_5_1. 
compute q67_5_2_velha = q67_5_2. 
compute q67_5_3_velha = q67_5_3. 
compute q67_5_4_velha =q67_5_4. 
compute q67_5_5_velha =q67_5_5. 
compute q67_5_6_velha = q67_5_6. 
compute q67_5_7_velha =q67_5_7. 
compute q67_5_8_velha =q67_5_8. 
compute q67_5_9_velha =q67_5_9. 
compute q67_6_0_velha =q67_6_0. 
compute q67_6_1_velha =q67_6_1. 
compute q67_6_2_velha = q67_6_2. 
compute q67_6_3_velha = q67_6_3. 
compute q67_6_4_velha =q67_6_4. 
compute q67_6_5_velha =q67_6_5. 
compute q67_6_6_velha = q67_6_6. 
compute q67_6_7_velha =q67_6_7. 
compute q67_6_8_velha =q67_6_8. 
compute q67_6_9_velha =q67_6_9. 
compute q67_7_0_velha =q67_7_0. 
compute q67_7_1_velha =q67_7_1. 
compute q67_7_2_velha = q67_7_2. 
compute q67_7_3_velha = q67_7_3. 
compute q67_7_4_velha =q67_7_4. 
compute q67_7_5_velha =q67_7_5. 
compute q67_7_6_velha = q67_7_6. 
compute q67_7_7_velha =q67_7_7. 
compute q67_7_8_velha =q67_7_8. 
compute q67_7_9_velha =q67_7_9. 
compute q67_8_0_velha =q67_8_0. 
compute q67_8_1_velha =q67_8_1. 
compute q67_8_2_velha = q67_8_2. 
compute q67_8_3_velha = q67_8_3. 
compute q67_8_4_velha =q67_8_4. 
compute q67_8_5_velha =q67_8_5. 
compute q67_8_6_velha = q67_8_6. 
compute q67_8_7_velha =q67_8_7. 
compute q67_8_8_velha =q67_8_8. 
compute q67_8_9_velha =q67_8_9. 
compute q67_9_0_velha =q67_9_0. 
compute q67_9_1_velha =q67_9_1. 
compute q67_9_2_velha = q67_9_2. 
compute q67_9_3_velha = q67_9_3. 
compute q67_9_4_velha =q67_9_4. 
compute q67_9_5_velha =q67_9_5. 
compute q67_9_6_velha = q67_9_6. 
compute q67_9_7_velha =q67_9_7. 
compute q67_9_8_velha =q67_9_8. 
compute q67_9_9_velha =q67_9_9. 
compute q67_10_0_velha =q67_10_0. 
compute q67_10_1_velha =q67_10_1. 
compute q67_10_2_velha = q67_10_2. 
compute q67_10_3_velha = q67_10_3. 
compute q67_10_4_velha =q67_10_4. 
compute q67_10_5_velha =q67_10_5. 
compute q67_10_6_velha = q67_10_6. 
compute q67_10_7_velha =q67_10_7. 
compute q67_10_8_velha =q67_10_8. 
compute q67_10_9_velha =q67_10_9. 
execute. 


DO IF (q67_1_0=1 | q67_1_9=1).
RECODE q67_1_1 (ELSE=SYSMIS).
RECODE q67_1_2 (ELSE=SYSMIS).
RECODE q67_1_3 (ELSE=SYSMIS).
RECODE q67_1_4 (ELSE=SYSMIS).
RECODE q67_1_5 (ELSE=SYSMIS).
RECODE q67_1_6(ELSE=SYSMIS).
RECODE q67_1_7 (ELSE=SYSMIS).
RECODE q67_1_8 (ELSE=SYSMIS).
END IF.
EXECUTE.



DO IF (q67_2_0=1 | q67_2_9=1).
RECODE q67_2_1 (ELSE=SYSMIS).
RECODE q67_2_2 (ELSE=SYSMIS).
RECODE q67_2_3 (ELSE=SYSMIS).
RECODE q67_2_4 (ELSE=SYSMIS).
RECODE q67_2_5 (ELSE=SYSMIS).
RECODE q67_2_6(ELSE=SYSMIS).
RECODE q67_2_7 (ELSE=SYSMIS).
RECODE q67_2_8 (ELSE=SYSMIS).
END IF.
EXECUTE.


DO IF (q67_3_0=1 | q67_3_9=1).
RECODE q67_3_1 (ELSE=SYSMIS).
RECODE q67_3_2 (ELSE=SYSMIS).
RECODE q67_3_3 (ELSE=SYSMIS).
RECODE q67_3_4 (ELSE=SYSMIS).
RECODE q67_3_5 (ELSE=SYSMIS).
RECODE q67_3_6(ELSE=SYSMIS).
RECODE q67_3_7 (ELSE=SYSMIS).
RECODE q67_3_8 (ELSE=SYSMIS).
END IF.
EXECUTE.

DO IF (q67_4_0=1 | q67_4_9=1).
RECODE q67_4_1 (ELSE=SYSMIS).
RECODE q67_4_2 (ELSE=SYSMIS).
RECODE q67_4_3 (ELSE=SYSMIS).
RECODE q67_4_4 (ELSE=SYSMIS).
RECODE q67_4_5 (ELSE=SYSMIS).
RECODE q67_4_6(ELSE=SYSMIS).
RECODE q67_4_7 (ELSE=SYSMIS).
RECODE q67_4_8 (ELSE=SYSMIS).
END IF.
EXECUTE.


DO IF (q67_5_0=1 | q67_5_9=1).
RECODE q67_5_1 (ELSE=SYSMIS).
RECODE q67_5_2 (ELSE=SYSMIS).
RECODE q67_5_3 (ELSE=SYSMIS).
RECODE q67_5_4 (ELSE=SYSMIS).
RECODE q67_5_5 (ELSE=SYSMIS).
RECODE q67_5_6(ELSE=SYSMIS).
RECODE q67_5_7 (ELSE=SYSMIS).
RECODE q67_5_8 (ELSE=SYSMIS).
END IF.
EXECUTE.


DO IF (q67_6_0=1 | q67_6_9=1).
RECODE q67_6_1 (ELSE=SYSMIS).
RECODE q67_6_2 (ELSE=SYSMIS).
RECODE q67_6_3 (ELSE=SYSMIS).
RECODE q67_6_4 (ELSE=SYSMIS).
RECODE q67_6_5 (ELSE=SYSMIS).
RECODE q67_6_6(ELSE=SYSMIS).
RECODE q67_6_7 (ELSE=SYSMIS).
RECODE q67_6_8 (ELSE=SYSMIS).
END IF.
EXECUTE.



DO IF (q67_7_0=1 | q67_7_9=1).
RECODE q67_7_1 (ELSE=SYSMIS).
RECODE q67_7_2 (ELSE=SYSMIS).
RECODE q67_7_3 (ELSE=SYSMIS).
RECODE q67_7_4 (ELSE=SYSMIS).
RECODE q67_7_5 (ELSE=SYSMIS).
RECODE q67_7_6(ELSE=SYSMIS).
RECODE q67_7_7 (ELSE=SYSMIS).
RECODE q67_7_8 (ELSE=SYSMIS).
END IF.
EXECUTE.



DO IF (q67_8_0=1 | q67_8_9=1).
RECODE q67_8_1 (ELSE=SYSMIS).
RECODE q67_8_2 (ELSE=SYSMIS).
RECODE q67_8_3 (ELSE=SYSMIS).
RECODE q67_8_4 (ELSE=SYSMIS).
RECODE q67_8_5 (ELSE=SYSMIS).
RECODE q67_8_6(ELSE=SYSMIS).
RECODE q67_8_7 (ELSE=SYSMIS).
RECODE q67_8_8 (ELSE=SYSMIS).
END IF.
EXECUTE.


DO IF (q67_9_0=1 | q67_9_9=1).
RECODE q67_9_1 (ELSE=SYSMIS).
RECODE q67_9_2 (ELSE=SYSMIS).
RECODE q67_9_3 (ELSE=SYSMIS).
RECODE q67_9_4 (ELSE=SYSMIS).
RECODE q67_9_5 (ELSE=SYSMIS).
RECODE q67_9_6(ELSE=SYSMIS).
RECODE q67_9_7 (ELSE=SYSMIS).
RECODE q67_9_8 (ELSE=SYSMIS).
END IF.
EXECUTE.

DO IF (q67_10_0=1 | q67_10_9=1).
RECODE q67_10_1 (ELSE=SYSMIS).
RECODE q67_10_2 (ELSE=SYSMIS).
RECODE q67_10_3 (ELSE=SYSMIS).
RECODE q67_10_4 (ELSE=SYSMIS).
RECODE q67_10_5 (ELSE=SYSMIS).
RECODE q67_10_6(ELSE=SYSMIS).
RECODE q67_10_7 (ELSE=SYSMIS).
RECODE q67_10_8 (ELSE=SYSMIS).
END IF.
EXECUTE.

***q68**********************************************************************************************************************************************************************************

compute q68_0_velha =q68_0. 
compute q68_1_velha =q68_1. 
compute q68_2_velha =q68_2. 
compute q68_3_velha =q68_3. 
compute q68_4_velha =q68_4. 
compute q68_99_velha =q68_99. 
execute. 


DO IF (q68_0=1).
RECODE q68_1 (ELSE=SYSMIS).
RECODE q68_2 (ELSE=SYSMIS).
RECODE q68_3 (ELSE=SYSMIS).
RECODE q68_4 (ELSE=SYSMIS).
RECODE q68_99 (ELSE=SYSMIS).
END IF.
EXECUTE.









VARIABLE LABELS
q5_1_velha'q5_1_v10367_1_1: Secretaria da Assist�ncia Social ou cong�nere'
q5_2_velha'q5_2_v10367_2_2: Outra unidade administrativa (Sede de Prefeitura, Administra��o Regional, Sub-prefeitura, etc)'
q5_3_velha'q5_3_v10367_3_3: Conselho Municipal de Assist�ncia Social'
q5_4_velha'q5_4_v10367_4_4: Conselho Tutelar'
q5_5_velha'q5_5_v10367_5_5: CREAS � Centro de Refer�ncia Especializado de Assist�ncia Social'
q5_6_velha'q5_6_v10367_6_6: Outra unidade p�blica de servi�os da Assist�ncia Social'
q5_7_velha'q5_7_v10367_7_7: Unidade de Sa�de'
q5_8_velha'q5_8_v10367_8_8: Escola'
q5_9_velha'q5_9_v10367_9_9: Associa��o Comunit�ria'
q5_10_velha'q5_10_v10367_10_10: Entidades ou Organiza��o da Sociedade Civil de Assist�ncia Social'
q5_11_velha'q5_11_v10367_11_11: Organiza��es da sociedade civil de outras �reas'
q5_12_velha'q5_12_v10367_12_12: Esta��o Cidadania/Pra�a do CEU (esporte e/ou cultura)'
q5_13_velha'q5_13_v10367_13_13: Conselho Municipal de Direitos da Crian�a e do Adolescente � CMDCA'
q5_99_velha'q5_99_v10367_99_99: Outros'
q6_1_velha'q6_1_v10369_1_1: Apenas o endere�o � o mesmo, mas todos os espa�os do CRAS s�o independentes e de uso exclusivo, inclusive a entrada'
q6_2_velha'q6_2_v10369_2_2: Entrada / Porta de Acesso'
q6_3_velha'q6_3_v10369_3_3: Recep��o'
q6_4_velha'q6_4_v10369_4_4: Cozinha'
q6_5_velha'q6_5_v10369_5_5: Algumas salas de atendimento particularizado'
q6_6_velha'q6_6_v10369_6_6: Todas as salas de atendimento particularizado'
q6_7_velha'q6_7_v10369_7_7: Algumas salas de atendimento coletivo'
q6_8_velha'q6_8_v10369_8_8: Todas as salas de atendimento coletivo'
q6_9_velha'q6_9_v10369_9_9: Salas administrativas'
q6_10_velha'q6_10_v10369_10_10: Banheiros'
q6_11_velha'q6_11_v10369_11_11: �rea Externa'
q6_12_velha'q6_12_v10369_12_12: Almoxarifado ou similar'
q9_1_velha'q9_1_v12572_1_1: Sim, suporte de profissional com conhecimento em LIBRAS'
q9_2_velha'q9_2_v12572_2_2: Sim, suporte de material em braille'
q9_3_velha'q9_3_v12572_3_3: Sim, suporte para leitores de telas de computador para pessoas com defici�ncia visual'
q9_4_velha'q9_4_v12572_4_4: Sim, h� outras adapta��es ou estrat�gias para assegurar acessibilidade �s pessoas com defici�ncia auditiva/surdas e pessoas com defici�ncia visual neste CRAS'
q9_5_velha'q9_5_v12572_5_5: Sim, outras adapta��es e tecnologias assistivas para defici�ncia f�sica'
q9_6_velha'q9_6_v12572_6_6: Sim, outras adapta��es e tecnologias assistivas para defici�ncia intelectual e autismo'
q9_7_velha'q9_7_v12572_7_7: Sim, pisos especiais com relevos para sinaliza��o voltados pessoa com defici�ncia visual'
q9_0_velha'q9_0_v12572_0_0: N�o h� outras adapta��es'
q14_velha'q14_v14349_14. Quantidade Total de pessoas que participaram regularmente dos atendimentos coletivos realizados pelo PAIF_m�s de refer�ncia: agosto de 2020'
q14_1_1_velha'q14_1_1_v14350_14.1.1. Do total de pessoas que participaram regularmente dos atendimentos coletivos realizados pelo PAIF quantos s�o - Masculino 0 a 18 anos'
q14_1_2_velha'q14_1_2_v14351_14.1.2. Do total de pessoas que participaram regularmente dos atendimentos coletivos realizados pelo PAIF quantos s�o - Masculino 19 a 59 anos'
q14_1_3_velha'q14_1_3_v14348_14.1.3. Do total de pessoas que participaram regularmente dos atendimentos coletivos realizados pelo PAIF quantos s�o - Masculino 60 anos ou mais'
q14_2_1_velha'q14_2_1_v14353_14.2.1. Do total de pessoas que participaram regularmente dos atendimentos coletivos realizados pelo PAIF quantos s�o - Feminino 0 a 18 anos'
q14_2_2_velha'q14_2_2_v14352_14.2.2. Do total de pessoas que participaram regularmente dos atendimentos coletivos realizados pelo PAIF quantos s�o - Feminino 19 a 59 anos'
q14_2_3_velha'q14_2_3_v14354_14.2.3. Do total de pessoas que participaram regularmente dos atendimentos coletivos realizados pelo PAIF quantos s�o - Feminino 60 anos ou mais'
q15_1_velha'q15_1_v15380_1_1: N�o h� atendimento coletivo no PAIF - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_2_velha'q15_2_v15380_2_2: Direito � transfer�ncia de renda e benef�cios assistenciais - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_3_velha'q15_3_v15380_3_3: Direito a Documenta��o Civil B�sica (certid�o de nascimento, CPF, RG, t�tulo eleitoral) - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_4_velha'q15_4_v15380_4_4: Direito a cultura e lazer - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_5_velha'q15_5_v15380_5_5: Direito das Mulheres - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_6_velha'q15_6_v15380_6_6: Direitos das pessoas com defici�ncia - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_7_velha'q15_7_v15380_7_7: Direito � Alimenta��o - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_8_velha'q15_8_v15380_8_8: Os direitos das fam�lias - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_9_velha'q15_9_v15380_9_9: Primeira inf�ncia � direitos e cuidados - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_10_velha'q15_10_v15380_10_10: Adolesc�ncia e gravidez - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_11_velha'q15_11_v15380_11_11: Direito das pessoas idosas - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_12_velha'q15_12_v15380_12_12: PBF e Condicionalidades - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_13_velha'q15_13_v15380_13_13: Preven��o ao Trabalho escravo e tr�fico de pessoas - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_14_velha'q15_14_v15380_14_14: Preven��o ao trabalho infantil - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_15_velha'q15_15_v15380_15_15: Conviv�ncia familiar filhos com m�es encarceradas - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_16_velha'q15_16_v15380_16_16: Cuidar de quem cuida - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_17_velha'q15_17_v15380_17_17: O uso de �lcool e/ou outras drogas na fam�lia - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_18_velha'q15_18_v15380_18_18: Problemas e solu��es do territ�rio - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_19_velha'q15_19_v15380_19_19: Diversidade cultural, �tnica e social - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_20_velha'q15_20_v15380_20_20: Viol�ncia dom�stica contra mulheres - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_21_velha'q15_21_v15380_21_21: Viol�ncia dom�stica contra crian�as e adolescentes - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_22_velha'q15_22_v15380_22_22: Viol�ncia dom�stica contra pessoas idosas - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_23_velha'q15_23_v15380_23_23: Sa�de mental e preven��o ao suic�dio - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_24_velha'q15_24_v15380_24_24: Sexualidade - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_25_velha'q15_25_v15380_25_25: Planejamento familiar/gravidez na adolesc�ncia - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_26_velha'q15_26_v15380_26_26: Suic�dio - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_27_velha'q15_27_v15380_27_27: Educa��o Financeira - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q15_99_velha'q15_99_v15380_99_99: Outro - principais tem�ticas abordadas nos atendimentos coletivos realizados pelo PAIF'
q16_1_velha'q16_1_v15381_1_1: N�o realiza nenhuma a��o e/ou atividade no territ�rio'
q16_2_velha'q16_2_v15381_2_2: Campanha de Preven��o e Combate ao Trabalho Escravo ( 28 de janeiro)'
q16_3_velha'q16_3_v15381_3_3: Campanha de Preven��o a Gravidez na Adolesc�ncia (1 de fevereiro)'
q16_4_velha'q16_4_v15381_4_4: Campanha de Preven��o e Elimina��o da Discrimina��o Racial ( 21 de mar�o)'
q16_5_velha'q16_5_v15381_5_5: Campanha de Preven��o e Combate � Explora��o Sexual de Crian�as e Adolescentes (18 de maio)'
q16_6_velha'q16_6_v15381_6_6: Campanha de Preven��o e enfrentamento do Trabalho Infantil ( 12 de junho)'
q16_7_velha'q16_7_v15381_7_7: Campanha de Preven��o e Combate ao Tr�fico de Pessoas (30 de julho)'
q16_8_velha'q16_8_v15381_8_8: Campanha de Preven��o e Combate � Viol�ncia contra a pessoa idosa ( 16 de junho)'
q16_9_velha'q16_9_v15381_9_9: Campanha de Preven��o e Combate a Viol�ncia contra a mulher ( 25 de novembro)'
q16_10_velha'q16_10_v15381_10_10: Campanha de enfrentamento � discrimina��o e � intoler�ncia �s diversidades (de g�nero, racial/�tnica, de orienta��o sexual e identidade de g�nero, de condi��es de defici�ncia, de religi�o ou cren�a, etc.)'
q16_11_velha'q16_11_v15381_11_11: Campanhas de preven��o ao suic�dio e automutila��o'
q16_12_velha'q16_12_v15381_12_12: Campanha de sensibiliza��o sobre a hansen�ase'
q16_13_velha'q16_13_v15381_13_13: Campanha de sensibiliza��o sobre infec��es sexualmente transmiss�veis'
q16_14_velha'q16_14_v15381_14_14: Mobiliza��es intrasetorial e intersetorial para idenfica��o de sinais e sintomas de turbeculose e enfrentamento de preconceito e estimas em rela��o as pessoas com a doen�a'
q16_99_velha'q16_99_v15381_99_99: Outro'
q18_1_velha'q18_1_v10394_18.1. Esta unidade oferta diretamente o Servi�o de Conviv�ncia e Fortalecimento de V�nculos para os seguintes p�blicos - Crian�as de 0 a 6 anos de idade - (M�s de refer�ncia: agosto/2021)'
q18_2_velha'q18_2_v10395_18.2. Esta unidade oferta diretamente o Servi�o de Conviv�ncia e Fortalecimento de V�nculos para os seguintes p�blicos - Crian�as e adolescentes de 7 a 14 anos de idade - (M�s de refer�ncia: agosto/2021)'
q18_3_velha'q18_3_v10396_18.3. Esta unidade oferta diretamente o Servi�o de Conviv�ncia e Fortalecimento de V�nculos para os seguintes p�blicos - Adolescentes e jovens de 15 a 17 anos de idade - (M�s de refer�ncia: agosto/2021)'
q18_4_velha'q18_4_v10397_18.4. Esta unidade oferta diretamente o Servi�o de Conviv�ncia e Fortalecimento de V�nculos para os seguintes p�blicos - Jovens de 18 a 29 anos de idade - (M�s de refer�ncia: agosto/2021)'
q18_5_velha'q18_5_v10398_18.5. Esta unidade oferta diretamente o Servi�o de Conviv�ncia e Fortalecimento de V�nculos para os seguintes p�blicos - Adultas(os) de 30 a 59 anos de idade - (M�s de refer�ncia: agosto/2021)'
q18_6_velha'q18_6_v10402_18.6. Esta unidade oferta diretamente o Servi�o de Conviv�ncia e Fortalecimento de V�nculos para os seguintes p�blicos - Idosas(os) (60 anos ou mais) - (M�s de refer�ncia: agosto/2021)'
q19_1_velha'q19_1_v10403_1_1: Sim, realizado por outra unidade p�blica'
q19_2_velha'q19_2_v10403_2_2: Sim, realizado por entidade conveniada'
q19_3_velha'q19_3_v10403_3_3: Sim, realizado por entidade n�o conveniada'
q19_0_velha'q19_0_v10403_0_0: N�o'
q20_1_velha'q20_1_v10406_20.1. A rede referenciada a este CRAS oferta Servi�o de Conviv�ncia e Fortalecimento de V�nculos para os seguintes p�blicos - Crian�as de 0 a 6 anos de idade - (M�s de refer�ncia: agosto/2021)'
q20_2_velha'q20_2_v10407_20.2. A rede referenciada a este CRAS oferta Servi�o de Conviv�ncia e Fortalecimento de V�nculos para os seguintes p�blicos - Crian�as e adolescentes de 7 a 14 anos de idade - (M�s de refer�ncia: agosto/2021)'
q20_3_velha'q20_3_v10408_20.3. A rede referenciada a este CRAS oferta Servi�o de Conviv�ncia e Fortalecimento de V�nculos para os seguintes p�blicos - Adolescentes e jovens de 15 a 17 anos de idade - (M�s de refer�ncia: agosto/2021)'
q20_4_velha'q20_4_v10410_20.4. A rede referenciada a este CRAS oferta Servi�o de Conviv�ncia e Fortalecimento de V�nculos para os seguintes p�blicos - Jovens de 18 a 29 anos de idade - (M�s de refer�ncia: agosto/2021)'
q20_5_velha'q20_5_v10411_20.5. A rede referenciada a este CRAS oferta Servi�o de Conviv�ncia e Fortalecimento de V�nculos para os seguintes p�blicos - Adultas(os) de 30 a 59 anos de idade - (M�s de refer�ncia: agosto/2021)'
q20_6_velha'q20_6_v10412_20.6. A rede referenciada a este CRAS oferta Servi�o de Conviv�ncia e Fortalecimento de V�nculos para os seguintes p�blicos - Idosas(os) (60 anos ou mais) - (M�s de refer�ncia: agosto/2021)'
q21_velha'q21_v12576_21. Em rela��o �s vagas de SCFV da rede referenciada, informe se:'
q22_velha'q22_v15383_22. O PAIF realiza acompanhamento das fam�lias de usu�rios atendidos no SCFV da rede referenciada?'
q25_1_velha'q25_1_v14314_25.1. Total de indiv�duos atendidos pelo Servi�o de PSB no Domic�lio neste CRAS no m�s de agosto de 2021. - Total de indiv�duos atendidos'
q25_2_velha'q25_2_v14356_25.2. Do total de indiv�duos atendidos pelo Servi�o de PSB no Domic�lio neste CRAS no m�s de agosto de 2021 quantos s�o o total de idosas(os) atendidos - Total de IDOSAS(OS) atendidas(os)'
q25_3_velha'q25_3_v14357_25.3. Do total de indiv�duos atendidos pelo Servi�o de PSB no Domic�lio neste CRAS no m�s de agosto de 2021 quantos s�o o total de pessoas com defici�ncia- Total de PESSOAS COM DEFICI�NCIA'
q25_4_velha'q25_4_v14358_25.4. Do total de pessoas com defici�ncia atendidos pelo Servi�o de PSB no Domic�lio neste CRAS no m�s de agosto de 2021, quantas possuem de 0 a 6 anos- Total de PESSOAS COM DEFICI�NCIA entre 0 e 6 anos de idade'
q26_1_velha'q26_1_v14315_1_1: Sim, realizado por outra unidade p�blica'
q26_2_velha'q26_2_v14315_2_2: Sim, realizado por entidade conveniada'
q26_3_velha'q26_3_v14315_3_3: Sim, realizado por entidade n�o conveniada'
q26_0_velha'q26_0_v14315_0_0: N�o'
q27_1_velha'q27_1_v14318_27.1. Total de indiv�duos atendidos pelo Servi�o de PSB no Domic�lio na rede referenciada no m�s de agosto de 2021. - Total de indiv�duos atendidos'
q27_2_velha'q27_2_v14361_27.2. Do total de indiv�duos atendidos pelo Servi�o de PSB no Domic�lio na rede referenciada no m�s de agosto de 2021 quantos s�o o total de idosas(os) atendidos - Total de IDOSAS(OS) atendidas(os)'
q27_3_velha'q27_3_v14360_27.3. Do total de indiv�duos atendidos pelo Servi�o de PSB no Domic�lio na rede referenciada no m�s de agosto de 2021 quantos s�o o total de pessoas com defici�ncia- Total de PESSOAS COM DEFICI�NCIA'
q27_4_velha'q27_4_v14359_27.4. Do total de pessoas com defici�ncia atendidos pelo Servi�o de PSB no Domic�lio na rede referenciada no m�s de agosto de 2021, quantas possuem de 0 a 6 anos- Total de PESSOAS COM DEFICI�NCIA entre 0 e 6 anos de idade'
q28_1_velha'q28_1_v14363_1_1: Acolhida inicial no domic�lio do usu�rio'
q28_2_velha'q28_2_v14363_2_2: Elabora��o do Plano de Desenvolvimento do Usu�rio � PDU'
q28_3_velha'q28_3_v14363_3_3: Visita domiciliar para orienta��o e suporte aos cuidados familiares no domic�lio'
q28_4_velha'q28_4_v14363_4_4: Rodas de di�logos peri�dicas com a fam�lia no domic�lio'
q28_5_velha'q28_5_v14363_5_5: Encontros territoriais peri�dicos com grupos multifamiliares'
q28_6_velha'q28_6_v14363_6_6: A��es de mobiliza��o para a cidadania no territ�rio'
q28_7_velha'q28_7_v14363_7_7: Difus�o e est�mulo � participa��o das(os) usu�rias(os) e suas fam�lias em conselhos de controle social'
q28_8_velha'q28_8_v14363_8_8: Reuni�es e encontros peri�dicos com a rede local para a discuss�o de casos/situa��es das(os) usu�rias(os) do Servi�o'
q28_9_velha'q28_9_v14363_9_9: Encaminhamentos monitorados de usu�rios � rede socioassistencial e intersetorial'
q28_0_velha'q28_0_v14363_0_0: Nenhum dos itens anteriores'
q28_99_velha'q28_99_v14363_99_99: Outras atividades'
q31_velha'q31_v10423_31. O(s) territ�rio(s) atendido(s) por esta equipe volante est�o localizados em:'
q32_1_velha'q32_1_v15384_1_1: Ve�culo utilizado pelo CRAS'
q32_2_velha'q32_2_v15384_2_2: Usa a Lancha da Assistente Social'
q32_3_velha'q32_3_v15384_3_3: Usa embarca��es locais'
q32_4_velha'q32_4_v15384_4_4: Transporte p�blico terrestre de linha'
q32_5_velha'q32_5_v15384_5_5: Compartilhamento de transporte fluvial'
q32_6_velha'q32_6_v15384_6_6: Compartilhamento de transporte terrestre'
q32_7_velha'q32_7_v15384_7_7: Deslocamentos complementares a p� para visitar fam�lias'
q32_99_velha'q32_99_v15384_99_99: Outro'
q33_1_velha'q33_1_v15386_1_1: Atendimento particularizado de fam�lias ou indiv�duos'
q33_2_velha'q33_2_v15386_2_2: Atendimento em atividade coletiva de car�ter continuado -Oficinas do PAIF)'
q33_3_velha'q33_3_v15386_3_3: Oferta do SCFV por meio de atividade coletivas de car�ter continuado'
q33_4_velha'q33_4_v15386_4_4: Atendimento em atividade coletiva de car�ter n�o-continuado (Palestras)'
q33_5_velha'q33_5_v15386_5_5: Cadastramento/Atualiza��o cadastral'
q33_6_velha'q33_6_v15386_6_6: Gest�o de benef�cios do Programa Bolsa Fam�lia (esclarecimentos de d�vidas e a��es de bloqueio, desbloqueios, cancelamentos e revers�es de cancelamento de benef�cios)'
q33_7_velha'q33_7_v15386_7_7: Gest�o de condicionalidades (recursos, acompanhamento de condicionalidades, etc)'
q33_8_velha'q33_8_v15386_8_8: Concess�o de benef�cio eventual'
q33_9_velha'q33_9_v15386_9_9: A��es para obten��o de documenta��o pessoal'
q33_10_velha'q33_10_v15386_10_10: Encaminhamento para o BPC'
q33_11_velha'q33_11_v15386_11_11: Encaminhamentos para rede socioassistencial e intersetorial'
q33_12_velha'q33_12_v15386_12_12: Visitas domiciliares'
q33_99_velha'q33_99_v15386_99_99: Outros'
q35_1_velha'q35_1_v15387_1_1: Benef�cio Eventual em situa��o de morte'
q35_2_velha'q35_2_v15387_2_2: Benef�cio Eventual por nascimento'
q35_3_velha'q35_3_v15387_3_3: Benef�cio Eventual em situa��o de calamidade (inclui desastres e emerg�ncias)'
q35_4_velha'q35_4_v15387_4_4: Benef�cio Eventual em situa��o de vulnerabilidade social tempor�ria'
q36_1_velha'q36_1_v15388_1_1: Colch�es/colchonetes'
q36_2_velha'q36_2_v15388_2_2: Cobertores/len�ois/roupas de cama'
q36_3_velha'q36_3_v15388_3_3: Vestimentas/Roupas'
q36_4_velha'q36_4_v15388_4_4: Material de limpeza e higiene'
q36_5_velha'q36_5_v15388_5_5: �gua'
q36_6_velha'q36_6_v15388_6_6: Aux�lio relacionado � alimenta��o (cesta b�sica, entre outros)'
q36_7_velha'q36_7_v15388_7_7: Aux�lio para deslocamento/passagens'
q36_8_velha'q36_8_v15388_8_8: Aux�lio/isen��o para retirada de documenta��o'
q36_9_velha'q36_9_v15388_9_9: Pagamento de Aluguel'
q36_10_velha'q36_10_v15388_10_10: Material de constru��o'
q36_11_velha'q36_11_v15388_11_11: Pagamento/isen��o de contas de �gua e luz'
q36_12_velha'q36_12_v15388_12_12: Aux�lio g�s'
q36_99_velha'q36_99_v15388_99_99: Outros'
q37_1_velha'q37_1_v12582_1_1: Aux�lio relacionado � alimenta��o (cesta b�sica, leite, entre outros)'
q37_2_velha'q37_2_v12582_2_2: Aux�lio para deslocamento/passagens (vale-transporte)'
q37_3_velha'q37_3_v12582_3_3: Aux�lio/Inse��o para retirada de documenta��o (inclui aux�lio para retirada de fotos)'
q37_4_velha'q37_4_v12582_4_4: Pagamento de aluguel'
q37_5_velha'q37_5_v12582_5_5: Material de Constru��o'
q37_6_velha'q37_6_v12582_6_6: Pagamento/Isen��o de contas de �gua e luz'
q37_7_velha'q37_7_v12582_7_7: Aux�lio g�s'
q37_8_velha'q37_8_v12582_8_8: Vestimentas/ Roupas'
q37_9_velha'q37_9_v12582_9_9: M�veis e Eletrodom�sticos'
q37_99_velha'q37_99_v12582_99_99: Outros'
q38_1_velha'q38_1_v12592_1_1: Identifica��o de poss�veis benefici�rias(os) do BPC'
q38_2_velha'q38_2_v12592_2_2: Busca ativa de benefici�rias(os) do BPC para inclus�o no Cad�nico'
q38_3_velha'q38_3_v12592_3_3: Orienta��o/acompanhamento para inser��o no BPC/ Encaminhamento ao INSS'
q38_4_velha'q38_4_v12592_4_4: Atendimento e Acompanhamento das pessoas com defici�ncia do BPC com idade escolar que estejam fora da escola (incluindo BPC escola)'
q38_5_velha'q38_5_v12592_5_5: Atendimento e acompanhamento das pessoas com defici�ncia do BPC em idade produtiva para o mundo do trabalho (incluindo BPC trabalho)'
q38_6_velha'q38_6_v12592_6_6: Inser��o das(os) Benefici�rias(os) nos servi�os socioassistenciais (PAIF, SCFV, entre outros)'
q38_0_velha'q38_0_v12592_0_0: Nenhuma das atividades acima'
q38_99_velha'q38_99_v12592_99_99: Outros'
q39_velha'q39_v10432_39. Este CRAS realiza cadastramento ou atualiza��o do Cad�nico?'
q40_1_velha'q40_1_v15390_1_1: Divulga��o das a��es de cadastramento'
q40_2_velha'q40_2_v15390_2_2: Realiza��o de parcerias com outros �rg�os'
q40_3_velha'q40_3_v15390_3_3: Busca ativa (entrevista domiciliar, mutir�o e a��es itinerantes) com a finalidade de inclus�o e atualiza��o cadastral'
q40_4_velha'q40_4_v15390_4_4: A��o para prestar informa��es �s fam�lias sobre o Cadastro �nico (Reuni�es de acolhida, palestra etc)'
q40_5_velha'q40_5_v15390_5_5: Agendamento pr�vio para a realiza��o da entrevista para inclus�o/atualiza��o cadastral'
q40_6_velha'q40_6_v15390_6_6: Triagem de atendimento � identifica��o de demandas e/ou documenta��o civil antes da entrevista'
q40_7_velha'q40_7_v15390_7_7: Entrevista para inclus�o cadastral'
q40_8_velha'q40_8_v15390_8_8: Entrevista para atualiza��o cadastral'
q40_9_velha'q40_9_v15390_9_9: Fornecimento de comprovante de cadastramento por demanda das fam�lias'
q40_10_velha'q40_10_v15390_10_10: Esclarecimento de d�vidas da popula��o sobre o Cadastro �nico e os programas, servi�os e benef�cios que as fam�lias participam ou buscam participar'
q40_11_velha'q40_11_v15390_11_11: Encaminhamento para obten��o de Documenta��o pessoal'
q40_12_velha'q40_12_v15390_12_12: Encaminhamento de fam�lias ou indiv�duos para a rede de servi�o socioassistencial'
q40_13_velha'q40_13_v15390_13_13: Encaminhamento de fam�lias ou indiv�duos para programas usu�rios do Cadastro �nico, para acesso a benef�cios ou outras pol�ticas p�blicas (educa��o, habita��o, trabalho, etc.)'
q40_14_velha'q40_14_v15390_14_14: Emiss�o de registro t�cnico (parecer, relat�rio, formul�rio etc) no processo de averigua��o cadastral ou apura��o de den�ncias'
q40_15_velha'q40_15_v15390_15_15: Confer�ncia/ cr�tica dos formul�rios preenchidos no papel'
q40_16_velha'q40_16_v15390_16_16: Verifica��o, corre��o ou encaminhamento de pend�ncias no cadastro das fam�lias e pessoas apontadas pelo Sistema de Cadastro �nico'
q40_17_velha'q40_17_v15390_17_17: Arquivamento de formul�rios'
q40_18_velha'q40_18_v15390_18_18: Recebimento e tratamento de den�ncias de presta��o de informa��o inver�dica'
q40_99_velha'q40_99_v15390_99_99: Outras'
q41_1_velha'q41_1_v15392_41.1. Total de pessoas atendidos para o Cadastro �nico - volume mensal de pessoas atendidas para o Cadastro �nico nesta Unidade durante o m�s de refer�ncia: (m�s de refer�ncia: agosto de 2021)'
q41_2_velha'q41_2_v15393_41.2. Destes, qual o total de pessoas realizaram entrevista para inclus�o cadastral - pessoas atendidas para o Cadastro �nico'
q41_3_velha'q41_3_v15394_41.3. Destes, qual o total de pessoas realizaram entrevista para atualiza��o cadastral- pessoas atendidas para o Cadastro �nico'
q41_98_velha'q41_98_v15395_98_41.98. N�o sabe informar - pessoas atendidas para o Cadastro �nico nesta Unidade durante o m�s de refer�ncia: (m�s de refer�ncia: agosto de 2021)'
q42_1_velha'q42_1_v15396_42.1. Entre o agendamento e a realiza��o do atendimento - Em m�dia, quanto tempo dura: em dias'
q42_1_1_velha'q42_1_1_v15397_1_42.1.1. N�o realiza - agendamento e a realiza��o do atendimento'
q42_2_velha'q42_2_v15398_42.2. Ap�s a entrevista, para que os dados coletados em formul�rio em papel sejam inseridos no sistema - Em m�dia, quanto tempo dura: em dias'
q42_2_1_velha'q42_2_1_v15402_1_42.2.1. N�o realiza - Ap�s a entrevista, para que os dados coletados em formul�rio em papel sejam inseridos no sistema'
q42_3_velha'q42_3_v15399_42.3. A espera de uma fam�lia nesta Unidade at� que o atendimento seja iniciado - Em m�dia, quanto tempo dura: em minutos'
q42_3_1_velha'q42_3_1_v15403_1_42.3.1. N�o sabe - A espera de uma fam�lia nesta Unidade at� que o atendimento seja iniciado'
q42_4_velha'q42_4_v15400_42.4. A primeira entrevista de uma fam�lia para o Cadastro �nico (inclus�o cadastral) - Em m�dia, quanto tempo dura: em minutos'
q42_4_1_velha'q42_4_1_v15404_1_42.4.1. N�o sabe - A primeira entrevista de uma fam�lia para o Cadastro �nico (inclus�o cadastral)'
q42_5_velha'q42_5_v15401_42.5. A atualiza��o cadastral de uma fam�lia no Cadastro �nico - Em m�dia, quanto tempo dura: em minutos'
q42_5_1_velha'q42_5_1_v15405_1_42.5.1. N�o sabe- A atualiza��o cadastral de uma fam�lia no Cadastro �nico'
q43_0_velha'q43_0_v15406_0_0: N�o h� a��es espec�ficas para atender o p�blico de averigua��o e revis�o cadastral'
q43_1_velha'q43_1_v15406_1_1: Sim, as fam�lias de averigua��o e revis�o cadastral s�o alvo de busca ativa'
q43_2_velha'q43_2_v15406_2_2: Sim, as fam�lias de averigua��o e revis�o cadastral fazem parte do p�blico priorit�rio de atendimento na unidade'
q43_3_velha'q43_3_v15406_3_3: Sim, h� mutir�es em dias espec�ficos para atender o p�blico de averigua��o e revis�o cadastral'
q43_99_velha'q43_99_v15406_99_99: Sim, outros'
q44_1_velha'q44_1_v15408_44.1. Para apurar den�ncias - quais situa��es e com qual frequ�ncia a equipe faz entrevistas domiciliares'
q44_2_velha'q44_2_v15409_44.2. Para fam�lias inclu�das em Averigua��o Cadastral - quais situa��es e com qual frequ�ncia a equipe faz entrevistas domiciliares'
q44_3_velha'q44_3_v15410_44.3. Para inclus�o ou atualiza��o de dados de fam�lias com benefici�rios do Benef�cio de Presta��o Continuada (BPC) - quais situa��es e com qual frequ�ncia a equipe faz entrevistas domiciliares'
q44_4_velha'q44_4_v15411_44.4. Para inclus�o ou atualiza��o de dados de fam�lias pertencentes � Grupos Populacionais Tradicionais ou Espec�ficos (GPTEs) - quais situa��es e com qual frequ�ncia a equipe faz entrevistas domiciliares'
q44_5_velha'q44_5_v15412_44.5. Para inclus�o ou atualiza��o de dados de fam�lias que moram em locais de dif�cil acesso - quais situa��es e com qual frequ�ncia a equipe faz entrevistas domiciliares'
q44_99_velha'q44_99_v15413_44.99. Outros - quais situa��es e com qual frequ�ncia a equipe faz entrevistas domiciliares'
q45_0_velha'q45_0_v15414_0_0: N�o realiza a��es e atividades relacionadas � gest�o de benef�cios do Programa Bolsa Fam�lia'
q45_1_velha'q45_1_v15414_1_1: Informa��o individualizada sobre regras do Programa (valores de benef�cios, regras de concess�o, bloqueio, suspens�o e cancelamento de benef�cios e condicionalidades)'
q45_2_velha'q45_2_v15414_2_2: Informa��o coletiva �s fam�lias sobre regras do Programa (Reuni�es de acolhida, palestra etc)'
q45_3_velha'q45_3_v15414_3_3: Emiss�o de declara��o de troca de respons�vel familiar para recebimento do benef�cio'
q45_4_velha'q45_4_v15414_4_4: Emiss�o de declara��o especial para recebimento do benef�cio (quando em situa��es de emerg�ncia e estado de calamidade p�blico)'
q45_5_velha'q45_5_v15414_5_5: Esclarecimentos sobre o conte�do de mensagens no extrato de pagamentos, recebida pelo benefici�rio'
q45_6_velha'q45_6_v15414_6_6: Esclarecimentos sobre informa��es de pagamento do Programa: entrega, desbloqueio e ativa��o do Cart�o Bolsa Fam�lia e calend�rio de pagamentos'
q45_7_velha'q45_7_v15414_7_7: Realiza manuten��o de benef�cios (bloqueios, desbloqueios, cancelamentos e revers�es de cancelamento) diretamente no SIBEC'
q45_8_velha'q45_8_v15414_8_8: Registro no Formul�rio Padr�o de Gest�o de Benef�cios (FPGB) e seu arquivamento'
q45_9_velha'q45_9_v15414_9_9: Solicita��o de manuten��o de benef�cios (bloqueios, desbloqueios, cancelamentos e revers�es de cancelamento) pelo m�dulo de Administra��o Off-line do SigPBF'
q45_10_velha'q45_10_v15414_10_10: Elabora��o de of�cios � Secretaria Nacional de Renda de Cidadania solicitando a manuten��o de benef�cios'
q45_11_velha'q45_11_v15414_11_11: Orienta��o sobre como denunciar irregularidades no pagamento e no atendimento dos canais da CAIXA'
q45_12_velha'q45_12_v15414_12_12: Registro de den�ncias de recebimento indevido de benef�cios'
q45_13_velha'q45_13_v15414_13_13: Apura��o de den�ncias de recebimento indevido de benef�cios'
q45_99_velha'q45_99_v15414_99_99: Outros'
q46_1_velha'q46_1_v15416_1_1: N�o h� dificuldades'
q46_2_velha'q46_2_v15416_2_2: Falta de capacita��o da equipe do CRAS sobre as regras do Programa'
q46_3_velha'q46_3_v15416_3_3: As informa��es disponibilizadas pelo Minist�rio da Cidadania s�o de dif�cil compreens�o (Instru��es Operacionais, Informes, Comunicados)'
q46_4_velha'q46_4_v15416_4_4: As informa��es disponibilizadas pelo Minist�rio da Cidadania s�o de dif�cil acesso'
q46_5_velha'q46_5_v15416_5_5: Falta de informa��es sobre as regras do Programa'
q46_6_velha'q46_6_v15416_6_6: Falta de material informativo para disponibilizar no CRAS (cartazes, folders, filipetas)'
q46_7_velha'q46_7_v15416_7_7: Baixa qualidade da internet'
q46_8_velha'q46_8_v15416_8_8: Falta de acesso � internet'
q46_9_velha'q46_9_v15416_9_9: Equipe do CRAS n�o tem acesso ao SIBEC'
q46_10_velha'q46_10_v15416_10_10: Instabilidade do SIBEC'
q46_11_velha'q46_11_v15416_11_11: Equipe do CRAS n�o tem acesso ao SigPBF'
q46_12_velha'q46_12_v15416_12_12: Instabilidade do SigPBF'
q46_13_velha'q46_13_v15416_13_13: Grande volume de atendimentos devido � averigua��o e revis�o cadastral'
q46_14_velha'q46_14_v15416_14_14: Grande volume de atendimentos devido �s opera��es de bloqueios e suspens�es de condicionalidades e de n�o localizados no sistema de ensino'
q46_15_velha'q46_15_v15416_15_15: Grande volume de atendimentos em decorr�ncia das suspens�es pelo recebimento do seguro-defeso'
q46_16_velha'q46_16_v15416_16_16: N�mero reduzido de pessoas na equipe para suprir a demanda de atendimento'
q46_99_velha'q46_99_v15416_99_99: Outros'
q48_0_velha'q48_0_v15419_0_0: N�o realiza atividades referente ao descumprimento de condicionalidades'
q48_1_velha'q48_1_v15419_1_1: Esclarecimentos �s fam�lias sobre as regras de condicionalidades durante o atendimento particularizado'
q48_2_velha'q48_2_v15419_2_2: Intera��o com as escolas dos benefici�rios em descumprimento'
q48_3_velha'q48_3_v15419_3_3: Intera��o com a equipe de sa�de no munic�pio respons�vel pelo acompanhamento das condicionalidades de sa�de'
q48_4_velha'q48_4_v15419_4_4: A��o para prestar informa��es �s fam�lias sobre as condicionalidades (Reuni�es de acolhida, palestras, oficinas em grupo, etc)'
q48_5_velha'q48_5_v15419_5_5: Busca ativa (visita domiciliar, envio de cartas ou SMS, e a��es itinerantes) com a finalidade de facilitar a apresenta��o de recurso pelas fam�lias'
q48_6_velha'q48_6_v15419_6_6: Emiss�o de laudo ou relat�rio social para subsidiar a fam�lia atendida/acompanhada pelo CRAS na apresenta��o do recurso'
q48_7_velha'q48_7_v15419_7_7: Fornecimento de comprovante do registro de recurso para as fam�lias'
q48_8_velha'q48_8_v15419_8_8: Arquivamento da documenta��o comprobat�ria apresentada pela fam�lia no registro do recurso'
q48_9_velha'q48_9_v15419_9_9: Acesso � lista de fam�lias em fase de suspens�o por descumprimento de condicionalidades no territ�rio abrangido pelo CRAS'
q48_10_velha'q48_10_v15419_10_10: Busca ativa das fam�lias (visita domiciliar e a��es itinerantes) com a finalidade de inclus�o nos servi�os socioassistencias para supera��o da situa��o que gerou o descumprimento'
q48_11_velha'q48_11_v15419_11_11: Prioriza��o das fam�lias em fase de suspens�o por descumprimento de condicionalidades nas a��es do PAIF'
q48_12_velha'q48_12_v15419_12_12: Registro no Sicon dos atendimentos/acompanhamentos realizados no �mbito do PAIF com as fam�lias PBF em descumprimento de condicionalidades'
q48_13_velha'q48_13_v15419_13_13: Ativa��o da interrup��o tempor�ria dos efeitos do descumprimento no Sicon'
q48_14_velha'q48_14_v15419_14_14: Encaminhamento de fam�lias ou indiv�duos em descumprimento de condicionalidades para outros servi�os, programas ou benef�cios socioassistenciais'
q48_15_velha'q48_15_v15419_15_15: Encaminhamento de fam�lias ou indiv�duos em descumprimento de condicionalidades para outras pol�ticas p�blicas (educa��o, sa�de, habita��o, trabalho, etc.)'
q48_99_velha'q48_99_v15419_99_99: Outros'
q49_1_velha'q49_1_v15421_1_1: Sim, as fam�lias em fase de suspens�o s�o alvo de busca ativa'
q49_2_velha'q49_2_v15421_2_2: Sim, as fam�lias em fase de suspens�o fazem parte do p�blico priorit�rio de atendimento na unidade'
q49_3_velha'q49_3_v15421_3_3: Sim, as fam�lias em fase de suspens�o s�o inclu�das em acompanhamento familiar'
q49_4_velha'q49_4_v15421_4_4: Sim, para as fam�lias em fase de suspens�o � ativada a interrup��o tempor�ria dos efeitos de descumprimento de condicionalidades quando avaliado necess�rio pela equipe do CRAS'
q49_99_velha'q49_99_v15421_99_99: Sim, outros'
q49_0_velha'q49_0_v15421_0_0: N�o h� a��es espec�ficas para atender as fam�lias em fase de suspens�o'
q49_98_velha'q49_98_v15421_98_98: N�o sabe informar'
q50_1_velha'q50_1_v15423_1_1: Sim, as den�ncias s�o tratadas aqui e depois encaminhadas � Secretaria Municipal'
q50_2_velha'q50_2_v15423_2_2: Sim, as den�ncias s�o encaminhadas para tratamento pela Secretaria Municipal'
q50_99_velha'q50_99_v15423_99_99: Outros'
q50_0_velha'q50_0_v15423_0_0: N�o recebemos den�ncias'
q50_98_velha'q50_98_v15423_98_98: N�o sei informar'
q51_0_velha'q51_0_v15425_51.0. Este CRAS ficou fechado em todo tempo da pandemia - Como se deu o funcionamento deste CRAS durante a pandemia do Coronav�rus e a frequ�ncia com isto ocorreu?'
q51_1_velha'q51_1_v15426_51.1. Ofertou atendimento presencial ao p�blico em geral - Como se deu o funcionamento deste CRAS durante a pandemia do Coronav�rus e a frequ�ncia com isto ocorreu?'
q51_2_velha'q51_2_v15427_51.2. Ofertou apenas os atendimentos presenciais agendados - Como se deu o funcionamento deste CRAS durante a pandemia do Coronav�rus e a frequ�ncia com isto ocorreu?'
q51_3_velha'q51_3_v15428_51.3. Ofertou atendimentos/acompanhamento apenas para o p�blico priorit�rio - Como se deu o funcionamento deste CRAS durante a pandemia do Coronav�rus e a frequ�ncia com isto ocorreu?'
q51_4_velha'q51_4_v15429_51.4. Ofertou os atendimento das demandas de benef�cios eventuais - Como se deu o funcionamento deste CRAS durante a pandemia do Coronav�rus e a frequ�ncia com isto ocorreu?'
q51_5_velha'q51_5_v15430_51.5. Ofertou os atendimento das demandas do Cadastro �nico - Como se deu o funcionamento deste CRAS durante a pandemia do Coronav�rus e a frequ�ncia com isto ocorreu?'
q51_6_velha'q51_6_v15431_51.6. Ofertou os atendimento das demandas do Aux�lio Emergencial - Como se deu o funcionamento deste CRAS durante a pandemia do Coronav�rus e a frequ�ncia com isto ocorreu?'
q51_7_velha'q51_7_v15432_51.7. Ofertou os atendimentos das demandas do Programa Bolsa Fam�lia - Como se deu o funcionamento deste CRAS durante a pandemia do Coronav�rus e a frequ�ncia com isto ocorreu?'
q51_8_velha'q51_8_v15433_51.8. Ofertou teleatendimento por telefone e aplicativos de videoconfer�ncia - Como se deu o funcionamento deste CRAS durante a pandemia do Coronav�rus e a frequ�ncia com isto ocorreu?'
q51_9_velha'q51_9_v15434_51.9. Ofertou atividades coletivas ( oficinas, grupo, a��es comunit�rias) - Como se deu o funcionamento deste CRAS durante a pandemia do Coronav�rus e a frequ�ncia com isto ocorreu?'
q51_10_velha'q51_10_v15435_51.10. Realizou visitas domiciliares - Como se deu o funcionamento deste CRAS durante a pandemia do Coronav�rus e a frequ�ncia com isto ocorreu?'
q51_99_velha'q51_99_v15436_51.99. Outro - Como se deu o funcionamento deste CRAS durante a pandemia do Coronav�rus e a frequ�ncia com isto ocorreu?'
q52_1_velha'q52_1_v15438_1_1: Teve que ampliar o n�mero de profissionais'
q52_2_velha'q52_2_v15438_2_2: Com jornada de trabalho normal (semelhante a jornada anterior a pandemia)'
q52_3_velha'q52_3_v15438_3_3: Com jornada de trabalho reduzida'
q52_99_velha'q52_99_v15438_99_99: Outros.'
q53_velha'q53_v15441_53. Os trabalhadores deste CRAS tiveram acesso a Equipamentos de Prote��o Individual?'
q54_1_velha'q54_1_v15442_1_1: N�o realizou nenhuma atividade em rela��o ao Aux�lio Emergencial'
q54_2_velha'q54_2_v15442_2_2: Prestou orienta��o �s fam�lias j� atendidas/acompanhadas por este CRAS'
q54_3_velha'q54_3_v15442_3_3: Prestou orienta��es �s fam�lias da comunidades em geral'
q54_4_velha'q54_4_v15442_4_4: Auxiliou as fam�lias no preenchimento das solicita��es'
q54_5_velha'q54_5_v15442_5_5: Realizou busca ativa de fam�lias para preenchimento das solicita��es'
q54_99_velha'q54_99_v15442_99_99: Outros'
q56_1_velha'q56_1_v15444_1_1: Computadores - O Programa Crian�a Feliz tem acesso aos seguintes equipamentos deste CRAS'
q56_2_velha'q56_2_v15444_2_2: Ve�culos - O Programa Crian�a Feliz tem acesso aos seguintes equipamentos deste CRAS'
q56_3_velha'q56_3_v15444_3_3: Arquivos  - O Programa Crian�a Feliz tem acesso aos seguintes equipamentos deste CRAS'
q56_4_velha'q56_4_v15444_4_4: Brinquedos, materiais pedag�gicos, culturais e esportivos  - O Programa Crian�a Feliz tem acesso aos seguintes equipamentos deste CRAS'
q56_5_velha'q56_5_v15444_5_5: Biblioteca - O Programa Crian�a Feliz tem acesso aos seguintes equipamentos deste CRAS'
q56_0_velha'q56_0_v15444_0_0: N�o tem acesso a equipamentos do CRAS - O Programa Crian�a Feliz'
q57_velha'q57_v15446_57. Com que frequ�ncia s�o realizadas as reuni�es entre os visitadores e os supervisores deste CRAS?'
q58_1_velha'q58_1_v15447_58.1. Reuni�es de planejamento entre a equipe do Programa Crian�a Feliz e a coordena��o do CRAS - Com que periodicidade s�o realizadas as seguintes atividades de integra��o do Programa Crian�a Feliz com este CRAS:'
q58_2_velha'q58_2_v15448_58.2. A equipe do Programa Crian�a Feliz realiza reuni�o com a equipe do PAIF para discuss�o de casos - Com que periodicidade s�o realizadas as seguintes atividades de integra��o do Programa Crian�a Feliz com este CRAS'
q58_3_velha'q58_3_v15449_58.3. Realiza o planejamento das visitas domiciliares - Com que periodicidade s�o realizadas as seguintes atividades de integra��o do Programa Crian�a Feliz com este CRAS:'
q58_4_velha'q58_4_v15450_58.4. Os visitadores/supervisores do PCF encaminham as fam�lias para os servi�os socioassistencias deste CRAS - Com que periodicidade s�o realizadas as seguintes atividades de integra��o do PCF com este CRAS'
q58_5_velha'q58_5_v15451_58.5. Os visitados/supervisores do PCF encaminham as fam�lias para cadastramento/atualiza��o cadastral - Com que periodicidade s�o realizadas as seguintes atividades de integra��o do Programa Crian�a Feliz com este CRAS'
q59_0_velha'q59_0_v15452_0_0: Nenhuma - No que se refere �s dificuldades em realizar as visitas domiciliares'
q59_1_velha'q59_1_v15452_1_1: Dificuldade de locomo��o da equipe para realizar as visitas - No que se refere �s dificuldades em realizar as visitas domiciliares'
q59_2_velha'q59_2_v15452_2_2: Equipe PCF n�o contratada ou contratada parcialmente - No que se refere �s dificuldades em realizar as visitas domiciliares'
q59_3_velha'q59_3_v15452_3_3: Equipe n�o capacitada - No que se refere �s dificuldades em realizar as visitas domiciliares'
q59_4_velha'q59_4_v15452_4_4: Equipe capacitada parcialmente - No que se refere �s dificuldades em realizar as visitas domiciliares'
q59_5_velha'q59_5_v15452_5_5: N�o consegue encontrar a quantidade de indiv�duos para atingir a meta pactuada - No que se refere �s dificuldades em realizar as visitas domiciliares'
q59_6_velha'q59_6_v15452_6_6: N�o consegue realizar o n�mero m�nimo de visitas preconizado - No que se refere �s dificuldades em realizar as visitas domiciliares'
q59_7_velha'q59_7_v15452_7_7: N�o consegue aplicar m�todo CDC nas visitas para o p�blico BPC - No que se refere �s dificuldades em realizar as visitas domiciliares'
q59_8_velha'q59_8_v15452_8_8: N�o consegue aplicar m�todo CDC nas visitas para o p�blico 0 a 3 anos - No que se refere �s dificuldades em realizar as visitas domiciliares'
q59_9_velha'q59_9_v15452_9_9: N�o consegue realizar atividade voltada ao rec�m nascido - No que se refere �s dificuldades em realizar as visitas domiciliares'
q59_10_velha'q59_10_v15452_10_10: Desconhece a quantidade de visitas necess�ria para o p�blico alvo - No que se refere �s dificuldades em realizar as visitas domiciliares'
q59_11_velha'q59_11_v15452_11_11: Resist�ncia das fam�lias em aceitar a Visita Domiciliar - No que se refere �s dificuldades em realizar as visitas domiciliares'
q59_99_velha'q59_99_v15452_99_99: Outra - No que se refere �s dificuldades em realizar as visitas domiciliares'
q63_1_velha'q63_1_v15462_1_1: As(os) usu�rias(os) e/ou familiares participam das reuni�es de planejamento desta unidade'
q63_2_velha'q63_2_v15462_2_2: As(os) usu�rias(os) contam com representante que participa do planejamento desta unidade'
q63_3_velha'q63_3_v15462_3_3: As(Os) usu�rias(os) escolhem os temas a serem trabalhados nas atividades coletivas (oficinas/palestras) da unidade'
q63_4_velha'q63_4_v15462_4_4: A equipe t�cnica disponibiliza outros meios para avalia��o da oferta (question�rio de satisfa��o, pesquisa de opini�o, urna de sugest�es)'
q63_99_velha'q63_99_v15462_99_99: Outros.'
q65_1_1_velha'q65_1_1_v15466_1_1: Atendimentos do PAIF - Povos Ind�genas'
q65_1_2_velha'q65_1_2_v15466_2_2: Atendimentos da Equipe Volante - Povos Ind�genas'
q65_1_3_velha'q65_1_3_v15466_3_3: Atendimento do Cadastro �nico - Povos Ind�genas'
q65_1_4_velha'q65_1_4_v15466_4_4: Visitas da equipe do PCF - Povos Ind�genas'
q65_1_99_velha'q65_1_99_v15466_99_99: Outros Atendimentos - Povos Ind�genas'
q65_1_0_velha'q65_1_0_v15466_0_0: N�o atende - Povos Ind�genas'
q65_1_5_velha'q65_1_5_v15466_5_5: N�o h� presen�a deste grupo no territ�rio - Povos Ind�genas'
q65_2_1_velha'q65_2_1_v15467_1_1: Atendimentos do PAIF - Comunidade Quilombola'
q65_2_2_velha'q65_2_2_v15467_2_2: Atendimentos da Equipe Volante IF - Comunidade Quilombola'
q65_2_3_velha'q65_2_3_v15467_3_3: Atendimento do Cadastro �nico IF - Comunidade Quilombola'
q65_2_4_velha'q65_2_4_v15467_4_4: Visitas da equipe do PCF IF - Comunidade Quilombola'
q65_2_99_velha'q65_2_99_v15467_99_99: Outros Atendimentos IF - Comunidade Quilombola'
q65_2_0_velha'q65_2_0_v15467_0_0: N�o atende IF - Comunidade Quilombola'
q65_2_5_velha'q65_2_5_v15467_5_5: N�o h� presen�a deste grupo no territ�rio IF - Comunidade Quilombola'
q65_3_1_velha'q65_3_1_v15469_1_1: Atendimentos do PAIF - Comunidade Ribeirinha'
q65_3_2_velha'q65_3_2_v15469_2_2: Atendimentos da Equipe Volante - Comunidade Ribeirinha'
q65_3_3_velha'q65_3_3_v15469_3_3: Atendimento do Cadastro �nico - Comunidade Ribeirinha'
q65_3_4_velha'q65_3_4_v15469_4_4: Visitas da equipe do PCF - Comunidade Ribeirinha'
q65_3_99_velha'q65_3_99_v15469_99_99: Outros Atendimentos - Comunidade Ribeirinha'
q65_3_0_velha'q65_3_0_v15469_0_0: N�o atende - Comunidade Ribeirinha'
q65_3_5_velha'q65_3_5_v15469_5_5: N�o h� presen�a deste grupo no territ�rio - Comunidade Ribeirinha'
q65_4_1_velha'q65_4_1_v15470_1_1: Atendimentos do PAIF - Povos Ciganos'
q65_4_2_velha'q65_4_2_v15470_2_2: Atendimentos da Equipe Volante - Povos Ciganos'
q65_4_3_velha'q65_4_3_v15470_3_3: Atendimento do Cadastro �nico - Povos Ciganos'
q65_4_4_velha'q65_4_4_v15470_4_4: Visitas da equipe do PCF - Povos Ciganos'
q65_4_99_velha'q65_4_99_v15470_99_99: Outros Atendimentos - Povos Ciganos'
q65_4_0_velha'q65_4_0_v15470_0_0: N�o atende - Povos Ciganos'
q65_4_5_velha'q65_4_5_v15470_5_5: N�o h� presen�a deste grupo no territ�rio - Povos Ciganos'
q65_5_1_velha'q65_5_1_v15471_1_1: Atendimentos do PAIF - Povos de Matriz Africana e de terreiro'
q65_5_2_velha'q65_5_2_v15471_2_2: Atendimentos da Equipe Volante - Povos de Matriz Africana e de terreiro'
q65_5_3_velha'q65_5_3_v15471_3_3: Atendimento do Cadastro �nico - Povos de Matriz Africana e de terreiro'
q65_5_4_velha'q65_5_4_v15471_4_4: Visitas da equipe do PCF - Povos de Matriz Africana e de terreiro'
q65_5_99_velha'q65_5_99_v15471_99_99: Outros Atendimentos - Povos de Matriz Africana e de terreiro'
q65_5_0_velha'q65_5_0_v15471_0_0: N�o atende - Povos de Matriz Africana e de terreiro'
q65_5_5_velha'q65_5_5_v15471_5_5: N�o h� presen�a deste grupo no territ�rio - Povos de Matriz Africana e de terreiro'
q65_6_1_velha'q65_6_1_v15472_1_1: Atendimentos do PAIF - Extrativistas'
q65_6_2_velha'q65_6_2_v15472_2_2: Atendimentos da Equipe Volante - Extrativistas'
q65_6_3_velha'q65_6_3_v15472_3_3: Atendimento do Cadastro �nico - Extrativistas'
q65_6_4_velha'q65_6_4_v15472_4_4: Visitas da equipe do PCF - Extrativistas'
q65_6_99_velha'q65_6_99_v15472_99_99: Outros Atendimentos - Extrativistas'
q65_6_0_velha'q65_6_0_v15472_0_0: N�o atende - Extrativistas'
q65_6_5_velha'q65_6_5_v15472_5_5: N�o h� presen�a deste grupo no territ�rio - Extrativistas'
q65_7_1_velha'q65_7_1_v15473_1_1: Atendimentos do PAIF - Pescadores artesanais'
q65_7_2_velha'q65_7_2_v15473_2_2: Atendimentos da Equipe Volante - Pescadores artesanais'
q65_7_3_velha'q65_7_3_v15473_3_3: Atendimento do Cadastro �nico - Pescadores artesanais'
q65_7_4_velha'q65_7_4_v15473_4_4: Visitas da equipe do PCF - Pescadores artesanais'
q65_7_99_velha'q65_7_99_v15473_99_99: Outros Atendimentos - Pescadores artesanais'
q65_7_0_velha'q65_7_0_v15473_0_0: N�o atende - Pescadores artesanais'
q65_7_5_velha'q65_7_5_v15473_5_5: N�o h� presen�a deste grupo no territ�rio'
q65_8_1_velha'q65_8_1_v15474_1_1: Atendimentos do PAIF - Ribeirinhas (fam�lias em calhas de rios)'
q65_8_2_velha'q65_8_2_v15474_2_2: Atendimentos da Equipe Volante - Ribeirinhas (fam�lias em calhas de rios)'
q65_8_3_velha'q65_8_3_v15474_3_3: Atendimento do Cadastro �nico - Ribeirinhas (fam�lias em calhas de rios)'
q65_8_4_velha'q65_8_4_v15474_4_4: Visitas da equipe do PCF - Ribeirinhas (fam�lias em calhas de rios)'
q65_8_99_velha'q65_8_99_v15474_99_99: Outros Atendimentos - Ribeirinhas (fam�lias em calhas de rios)'
q65_8_0_velha'q65_8_0_v15474_0_0: N�o atende - Ribeirinhas (fam�lias em calhas de rios)'
q65_8_5_velha'q65_8_5_v15474_5_5: N�o h� presen�a deste grupo no territ�rio - Ribeirinhas (fam�lias em calhas de rios)'
q65_9_1_velha'q65_9_1_v15475_1_1: Atendimentos do PAIF - Outros Grupos tradicionais e espec�ficos'
q65_9_2_velha'q65_9_2_v15475_2_2: Atendimentos da Equipe Volante - Outros Grupos tradicionais e espec�ficos'
q65_9_3_velha'q65_9_3_v15475_3_3: Atendimento do Cadastro �nico - Outros Grupos tradicionais e espec�ficos'
q65_9_4_velha'q65_9_4_v15475_4_4: Visitas da equipe do PCF - Outros Grupos tradicionais e espec�ficos'
q65_9_99_velha'q65_9_99_v15475_99_99: Outros Atendimentos - Outros Grupos tradicionais e espec�ficos'
q65_9_0_velha'q65_9_0_v15475_0_0: N�o atende - Outros Grupos tradicionais e espec�ficos'
q65_9_5_velha'q65_9_5_v15475_5_5: N�o h� presen�a deste grupo no territ�rio - Outros Grupos tradicionais e espec�ficos'
q65_10_1_velha'q65_10_1_v15476_1_1: Atendimentos do PAIF - Trabalho Infantil'
q65_10_2_velha'q65_10_2_v15476_2_2: Atendimentos da Equipe Volante  - Trabalho Infantil'
q65_10_3_velha'q65_10_3_v15476_3_3: Atendimento do Cadastro �nico  - Trabalho Infantil'
q65_10_4_velha'q65_10_4_v15476_4_4: Visitas da equipe do PCF  - Trabalho Infantil'
q65_10_99_velha'q65_10_99_v15476_99_99: Outros Atendimentos  - Trabalho Infantil'
q65_10_0_velha'q65_10_0_v15476_0_0: N�o atende  - Trabalho Infantil'
q65_10_5_velha'q65_10_5_v15476_5_5: N�o h� presen�a deste grupo no territ�rio  - Trabalho Infantil'
q65_11_1_velha'q65_11_1_v15477_1_1: Atendimentos do PAIF -  Atingidos por desastre'
q65_11_2_velha'q65_11_2_v15477_2_2: Atendimentos da Equipe Volante -  Atingidos por desastre'
q65_11_3_velha'q65_11_3_v15477_3_3: Atendimento do Cadastro �nico -  Atingidos por desastre'
q65_11_4_velha'q65_11_4_v15477_4_4: Visitas da equipe do PCF -  Atingidos por desastre'
q65_11_99_velha'q65_11_99_v15477_99_99: Outros Atendimentos -  Atingidos por desastre'
q65_11_0_velha'q65_11_0_v15477_0_0: N�o atende -  Atingidos por desastre'
q65_11_5_velha'q65_11_5_v15477_5_5: N�o h� presen�a deste grupo no territ�rio -  Atingidos por desastre'
q65_12_1_velha'q65_12_1_v15478_1_1: Atendimentos do PAIF - Familiares de presos e egressos do sistema carcer�rio (em situa��o de priva��o de liberdade/encarcerados)'
q65_12_2_velha'q65_12_2_v15478_2_2: Atendimentos da Equipe Volante  - Familiares de presos e egressos do sistema carcer�rio (em situa��o de priva��o de liberdade/encarcerados)'
q65_12_3_velha'q65_12_3_v15478_3_3: Atendimento do Cadastro �nico  - Familiares de presos e egressos do sistema carcer�rio (em situa��o de priva��o de liberdade/encarcerados)'
q65_12_4_velha'q65_12_4_v15478_4_4: Visitas da equipe do PCF  - Familiares de presos e egressos do sistema carcer�rio (em situa��o de priva��o de liberdade/encarcerados)'
q65_12_99_velha'q65_12_99_v15478_99_99: Outros Atendimentos  - Familiares de presos e egressos do sistema carcer�rio (em situa��o de priva��o de liberdade/encarcerados)'
q65_12_0_velha'q65_12_0_v15478_0_0: N�o atende  - Familiares de presos e egressos do sistema carcer�rio (em situa��o de priva��o de liberdade/encarcerados)'
q65_12_5_velha'q65_12_5_v15478_5_5: N�o h� presen�a deste grupo no territ�rio  - Familiares de presos e egressos do sistema carcer�rio (em situa��o de priva��o de liberdade/encarcerados)'
q65_13_1_velha'q65_13_1_v15479_1_1: Atendimentos do PAIF -  Medida Socioeducativa'
q65_13_2_velha'q65_13_2_v15479_2_2: Atendimentos da Equipe Volante  -  Medida Socioeducativa'
q65_13_3_velha'q65_13_3_v15479_3_3: Atendimento do Cadastro �nico  -  Medida Socioeducativa'
q65_13_4_velha'q65_13_4_v15479_4_4: Visitas da equipe do PCF  -  Medida Socioeducativa'
q65_13_99_velha'q65_13_99_v15479_99_99: Outros Atendimentos  -  Medida Socioeducativa'
q65_13_0_velha'q65_13_0_v15479_0_0: N�o atende  -  Medida Socioeducativa'
q65_13_5_velha'q65_13_5_v15479_5_5: N�o h� presen�a deste grupo no territ�rio  -  Medida Socioeducativa'
q65_14_1_velha'q65_14_1_v15480_1_1: Atendimentos do PAIF - Pessoas em situa��o de rua e/ou sem teto'
q65_14_2_velha'q65_14_2_v15480_2_2: Atendimentos da Equipe Volante - Pessoas em situa��o de rua e/ou sem teto'
q65_14_3_velha'q65_14_3_v15480_3_3: Atendimento do Cadastro �nico - Pessoas em situa��o de rua e/ou sem teto'
q65_14_4_velha'q65_14_4_v15480_4_4: Visitas da equipe do PCF - Pessoas em situa��o de rua e/ou sem teto'
q65_14_99_velha'q65_14_99_v15480_99_99: Outros Atendimentos - Pessoas em situa��o de rua e/ou sem teto'
q65_14_0_velha'q65_14_0_v15480_0_0: N�o atende - Pessoas em situa��o de rua e/ou sem teto'
q65_14_5_velha'q65_14_5_v15480_5_5: N�o h� presen�a deste grupo no territ�rio - Pessoas em situa��o de rua e/ou sem teto'
q65_15_1_velha'q65_15_1_v15481_1_1: Atendimentos do PAIF - Pessoas imigrantes (internacional) e ou refugiados'
q65_15_2_velha'q65_15_2_v15481_2_2: Atendimentos da Equipe Volante - Pessoas imigrantes (internacional) e ou refugiados'
q65_15_3_velha'q65_15_3_v15481_3_3: Atendimento do Cadastro �nico - Pessoas imigrantes (internacional) e ou refugiados'
q65_15_4_velha'q65_15_4_v15481_4_4: Visitas da equipe do PCF - Pessoas imigrantes (internacional) e ou refugiados'
q65_15_99_velha'q65_15_99_v15481_99_99: Outros Atendimentos - Pessoas imigrantes (internacional) e ou refugiados'
q65_15_0_velha'q65_15_0_v15481_0_0: N�o atende - Pessoas imigrantes (internacional) e ou refugiados'
q65_15_5_velha'q65_15_5_v15481_5_5: N�o h� presen�a deste grupo no territ�rio - Pessoas imigrantes (internacional) e ou refugiados'
q65_16_1_velha'q65_16_1_v15482_1_1: Atendimentos do PAIF - Transexuais/transg�nero/travestis/l�sbicas/gays/bissexuais/intersexuais'
q65_16_2_velha'q65_16_2_v15482_2_2: Atendimentos da Equipe Volante - Transexuais/transg�nero/travestis/l�sbicas/gays/bissexuais/intersexuais'
q65_16_3_velha'q65_16_3_v15482_3_3: Atendimento do Cadastro �nico - Transexuais/transg�nero/travestis/l�sbicas/gays/bissexuais/intersexuais'
q65_16_4_velha'q65_16_4_v15482_4_4: Visitas da equipe do PCF - Transexuais/transg�nero/travestis/l�sbicas/gays/bissexuais/intersexuais'
q65_16_99_velha'q65_16_99_v15482_99_99: Outros Atendimentos - Transexuais/transg�nero/travestis/l�sbicas/gays/bissexuais/intersexuais'
q65_16_0_velha'q65_16_0_v15482_0_0: N�o atende - Transexuais/transg�nero/travestis/l�sbicas/gays/bissexuais/intersexuais'
q65_16_5_velha'q65_16_5_v15482_5_5: N�o h� presen�a deste grupo no territ�rio - Transexuais/transg�nero/travestis/l�sbicas/gays/bissexuais/intersexuais'
q67_1_0_velha'q67_1_0_v10463_0_0: N�o tem nenhuma articula��o  - Unidades P�blicas da Rede de Prote��o Social B�sica'
q67_1_1_velha'q67_1_1_v10463_1_1: Possui dados de localiza��o (endere�o, telefone, etc.) - Unidades P�blicas da Rede de Prote��o Social B�sica'
q67_1_2_velha'q67_1_2_v10463_2_2: Recebe usu�rias(os) encaminhados por este CRAS  - Unidades P�blicas da Rede de Prote��o Social B�sica'
q67_1_3_velha'q67_1_3_v10463_3_3: Encaminha usu�rias(os) para este CRAS  - Unidades P�blicas da Rede de Prote��o Social B�sica'
q67_1_4_velha'q67_1_4_v10463_4_4: Acompanha os encaminhamentos  - Unidades P�blicas da Rede de Prote��o Social B�sica'
q67_1_5_velha'q67_1_5_v10463_5_5: Realiza reuni�es peri�dicas  - Unidades P�blicas da Rede de Prote��o Social B�sica'
q67_1_6_velha'q67_1_6_v10463_6_6: Troca informa��es  - Unidades P�blicas da Rede de Prote��o Social B�sica'
q67_1_7_velha'q67_1_7_v10463_7_7: Realiza estudos de caso em conjunto  - Unidades P�blicas da Rede de Prote��o Social B�sica'
q67_1_8_velha'q67_1_8_v10463_8_8: Desenvolve ativiades em parceria  - Unidades P�blicas da Rede de Prote��o Social B�sica'
q67_1_9_velha'q67_1_9_v10463_9_9: Servi�o ou institui��o n�o existente no Munic�pio ou no DF  - Unidades P�blicas da Rede de Prote��o Social B�sica'
q67_2_0_velha'q67_2_0_v10464_0_0: N�o tem nenhuma articula��o  -  Unidades Conveniadas da Rede de Prote��o Social B�sica'
q67_2_1_velha'q67_2_1_v10464_1_1: Possui dados de localiza��o (endere�o, telefone, etc.) -  Unidades Conveniadas da Rede de Prote��o Social B�sica'
q67_2_2_velha'q67_2_2_v10464_2_2: Recebe usu�rias(os) encaminhados por este CRAS -  Unidades Conveniadas da Rede de Prote��o Social B�sica'
q67_2_3_velha'q67_2_3_v10464_3_3: Encaminha usu�rias(os) para este CRAS -  Unidades Conveniadas da Rede de Prote��o Social B�sica'
q67_2_4_velha'q67_2_4_v10464_4_4: Acompanha os encaminhamentos -  Unidades Conveniadas da Rede de Prote��o Social B�sica'
q67_2_5_velha'q67_2_5_v10464_5_5: Realiza reuni�es peri�dicas -  Unidades Conveniadas da Rede de Prote��o Social B�sica'
q67_2_6_velha'q67_2_6_v10464_6_6: Troca informa��es -  Unidades Conveniadas da Rede de Prote��o Social B�sica'
q67_2_7_velha'q67_2_7_v10464_7_7: Realiza estudos de caso em conjunto -  Unidades Conveniadas da Rede de Prote��o Social B�sica'
q67_2_8_velha'q67_2_8_v10464_8_8: Desenvolve ativiades em parceria -  Unidades Conveniadas da Rede de Prote��o Social B�sica'
q67_2_9_velha'q67_2_9_v10464_9_9: Servi�o ou institui��o n�o existente no Munic�pio ou no DF -  Unidades Conveniadas da Rede de Prote��o Social B�sica'
q67_3_0_velha'q67_3_0_v15493_0_0: N�o tem nenhuma articula��o - Centro de Refer�ncia Especializado em Assist�ncia Social - CREAS'
q67_3_1_velha'q67_3_1_v15493_1_1: Possui dados de localiza��o (endere�o, telefone, etc.) - Centro de Refer�ncia Especializado em Assist�ncia Social - CREAS'
q67_3_2_velha'q67_3_2_v15493_2_2: Recebe usu�rias(os) encaminhados por este CRAS - Centro de Refer�ncia Especializado em Assist�ncia Social - CREAS'
q67_3_3_velha'q67_3_3_v15493_3_3: Encaminha usu�rias(os) para este CRAS - Centro de Refer�ncia Especializado em Assist�ncia Social - CREAS'
q67_3_4_velha'q67_3_4_v15493_4_4: Acompanha os encaminhamentos - Centro de Refer�ncia Especializado em Assist�ncia Social - CREAS'
q67_3_5_velha'q67_3_5_v15493_5_5: Realiza reuni�es peri�dicas - Centro de Refer�ncia Especializado em Assist�ncia Social - CREAS'
q67_3_6_velha'q67_3_6_v15493_6_6: Troca informa��es - Centro de Refer�ncia Especializado em Assist�ncia Social - CREAS'
q67_3_7_velha'q67_3_7_v15493_7_7: Realiza estudos de caso em conjunto - Centro de Refer�ncia Especializado em Assist�ncia Social - CREAS'
q67_3_8_velha'q67_3_8_v15493_8_8: Desenvolve ativiades em parceria - Centro de Refer�ncia Especializado em Assist�ncia Social - CREAS'
q67_3_9_velha'q67_3_9_v15493_9_9: Servi�o ou institui��o n�o existente no Munic�pio ou no DF - Centro de Refer�ncia Especializado em Assist�ncia Social - CREAS'
q67_4_0_velha'q67_4_0_v15503_0_0: N�o tem nenhuma articula��o - Outras Unidades da rede de prote��o social especial'
q67_4_1_velha'q67_4_1_v15503_1_1: Possui dados de localiza��o (endere�o, telefone, etc.) - Outras Unidades da rede de prote��o social especial'
q67_4_2_velha'q67_4_2_v15503_2_2: Recebe usu�rias(os) encaminhados por este CRAS - Outras Unidades da rede de prote��o social especial'
q67_4_3_velha'q67_4_3_v15503_3_3: Encaminha usu�rias(os) para este CRAS - Outras Unidades da rede de prote��o social especial'
q67_4_4_velha'q67_4_4_v15503_4_4: Acompanha os encaminhamentos - Outras Unidades da rede de prote��o social especial'
q67_4_5_velha'q67_4_5_v15503_5_5: Realiza reuni�es peri�dicas - Outras Unidades da rede de prote��o social especial'
q67_4_6_velha'q67_4_6_v15503_6_6: Troca informa��es - Outras Unidades da rede de prote��o social especial'
q67_4_7_velha'q67_4_7_v15503_7_7: Realiza estudos de caso em conjunto - Outras Unidades da rede de prote��o social especial'
q67_4_8_velha'q67_4_8_v15503_8_8: Desenvolve ativiades em parceria - Outras Unidades da rede de prote��o social especial'
q67_4_9_velha'q67_4_9_v15503_9_9: Servi�o ou institui��o n�o existente no Munic�pio ou no DF - Outras Unidades da rede de prote��o social especial'
q67_5_0_velha'q67_5_0_v10466_0_0: N�o tem nenhuma articula��o - Servi�os de Sa�de'
q67_5_1_velha'q67_5_1_v10466_1_1: Possui dados de localiza��o (endere�o, telefone, etc.) - Servi�os de Sa�de'
q67_5_2_velha'q67_5_2_v10466_2_2: Recebe usu�rias(os) encaminhados por este CRAS - Servi�os de Sa�de'
q67_5_3_velha'q67_5_3_v10466_3_3: Encaminha usu�rias(os) para este CRAS - Servi�os de Sa�de'
q67_5_4_velha'q67_5_4_v10466_4_4: Acompanha os encaminhamentos - Servi�os de Sa�de'
q67_5_5_velha'q67_5_5_v10466_5_5: Realiza reuni�es peri�dicas - Servi�os de Sa�de'
q67_5_6_velha'q67_5_6_v10466_6_6: Troca informa��es - Servi�os de Sa�de'
q67_5_7_velha'q67_5_7_v10466_7_7: Realiza estudos de caso em conjunto - Servi�os de Sa�de'
q67_5_8_velha'q67_5_8_v10466_8_8: Desenvolve ativiades em parceria - Servi�os de Sa�de'
q67_5_9_velha'q67_5_9_v10466_9_9: Servi�o ou institui��o n�o existente no Munic�pio ou no DF - Servi�os de Sa�de'
q67_6_0_velha'q67_6_0_v10467_0_0: N�o tem nenhuma articula��o - Servi�os de Educa��o'
q67_6_1_velha'q67_6_1_v10467_1_1: Possui dados de localiza��o (endere�o, telefone, etc.) - Servi�os de Educa��o'
q67_6_2_velha'q67_6_2_v10467_2_2: Recebe usu�rias(os) encaminhados por este CRAS - Servi�os de Educa��o'
q67_6_3_velha'q67_6_3_v10467_3_3: Encaminha usu�rias(os) para este CRAS - Servi�os de Educa��o'
q67_6_4_velha'q67_6_4_v10467_4_4: Acompanha os encaminhamentos - Servi�os de Educa��o'
q67_6_5_velha'q67_6_5_v10467_5_5: Realiza reuni�es peri�dicas - Servi�os de Educa��o'
q67_6_6_velha'q67_6_6_v10467_6_6: Troca informa��es - Servi�os de Educa��o'
q67_6_7_velha'q67_6_7_v10467_7_7: Realiza estudos de caso em conjunto - Servi�os de Educa��o'
q67_6_8_velha'q67_6_8_v10467_8_8: Desenvolve ativiades em parceria - Servi�os de Educa��o'
q67_6_9_velha'q67_6_9_v10467_9_9: Servi�o ou institui��o n�o existente no Munic�pio ou no DF - Servi�os de Educa��o'
q67_7_0_velha'q67_7_0_v10468_0_0: N�o tem nenhuma articula��o - �rg�os/Servi�os relacionados a Trabalho e Emprego'
q67_7_1_velha'q67_7_1_v10468_1_1: Possui dados de localiza��o (endere�o, telefone, etc.) - �rg�os/Servi�os relacionados a Trabalho e Emprego'
q67_7_2_velha'q67_7_2_v10468_2_2: Recebe usu�rias(os) encaminhados por este CRAS - �rg�os/Servi�os relacionados a Trabalho e Emprego'
q67_7_3_velha'q67_7_3_v10468_3_3: Encaminha usu�rias(os) para este CRAS - �rg�os/Servi�os relacionados a Trabalho e Emprego'
q67_7_4_velha'q67_7_4_v10468_4_4: Acompanha os encaminhamentos - �rg�os/Servi�os relacionados a Trabalho e Emprego'
q67_7_5_velha'q67_7_5_v10468_5_5: Realiza reuni�es peri�dicas - �rg�os/Servi�os relacionados a Trabalho e Emprego'
q67_7_6_velha'q67_7_6_v10468_6_6: Troca informa��es - �rg�os/Servi�os relacionados a Trabalho e Emprego'
q67_7_7_velha'q67_7_7_v10468_7_7: Realiza estudos de caso em conjunto - �rg�os/Servi�os relacionados a Trabalho e Emprego'
q67_7_8_velha'q67_7_8_v10468_8_8: Desenvolve ativiades em parceria - �rg�os/Servi�os relacionados a Trabalho e Emprego'
q67_7_9_velha'q67_7_9_v10468_9_9: Servi�o ou institui��o n�o existente no Munic�pio ou no DF - �rg�os/Servi�os relacionados a Trabalho e Emprego'
q67_8_0_velha'q67_8_0_v10477_0_0: N�o tem nenhuma articula��o - Movimentos Sociais locais/Associa��es Comunit�rias'
q67_8_1_velha'q67_8_1_v10477_1_1: Possui dados de localiza��o (endere�o, telefone, etc.) - Movimentos Sociais locais/Associa��es Comunit�rias'
q67_8_2_velha'q67_8_2_v10477_2_2: Recebe usu�rias(os) encaminhados por este CRAS - Movimentos Sociais locais/Associa��es Comunit�rias'
q67_8_3_velha'q67_8_3_v10477_3_3: Encaminha usu�rias(os) para este CRAS - Movimentos Sociais locais/Associa��es Comunit�rias'
q67_8_4_velha'q67_8_4_v10477_4_4: Acompanha os encaminhamentos - Movimentos Sociais locais/Associa��es Comunit�rias'
q67_8_5_velha'q67_8_5_v10477_5_5: Realiza reuni�es peri�dicas - Movimentos Sociais locais/Associa��es Comunit�rias'
q67_8_6_velha'q67_8_6_v10477_6_6: Troca informa��es - Movimentos Sociais locais/Associa��es Comunit�rias'
q67_8_7_velha'q67_8_7_v10477_7_7: Realiza estudos de caso em conjunto - Movimentos Sociais locais/Associa��es Comunit�rias'
q67_8_8_velha'q67_8_8_v10477_8_8: Desenvolve ativiades em parceria - Movimentos Sociais locais/Associa��es Comunit�rias'
q67_8_9_velha'q67_8_9_v10477_9_9: Servi�o ou institui��o n�o existente no Munic�pio ou no DF - Movimentos Sociais locais/Associa��es Comunit�rias'
q67_9_0_velha'q67_9_0_v12605_0_0: N�o tem nenhuma articula��o - Conselho Tutelar'
q67_9_1_velha'q67_9_1_v12605_1_1: Possui dados de localiza��o (endere�o, telefone, etc.) - Conselho Tutelar'
q67_9_2_velha'q67_9_2_v12605_2_2: Recebe usu�rias(os) encaminhados por este CRAS - Conselho Tutelar'
q67_9_3_velha'q67_9_3_v12605_3_3: Encaminha usu�rias(os) para este CRAS - Conselho Tutelar'
q67_9_4_velha'q67_9_4_v12605_4_4: Acompanha os encaminhamentos - Conselho Tutelar'
q67_9_5_velha'q67_9_5_v12605_5_5: Realiza reuni�es peri�dicas - Conselho Tutelar'
q67_9_6_velha'q67_9_6_v12605_6_6: Troca informa��es - Conselho Tutelar'
q67_9_7_velha'q67_9_7_v12605_7_7: Realiza estudos de caso em conjunto - Conselho Tutelar'
q67_9_8_velha'q67_9_8_v12605_8_8: Desenvolve ativiades em parceria - Conselho Tutelar'
q67_9_9_velha'q67_9_9_v12605_9_9: Servi�o ou institui��o n�o existente no Munic�pio ou no DF - Conselho Tutelar'
q67_10_0_velha'q67_10_0_v12606_0_0: N�o tem nenhuma articula��o  - Judici�rio'
q67_10_1_velha'q67_10_1_v12606_1_1: Possui dados de localiza��o (endere�o, telefone, etc.) - Judici�rio'
q67_10_2_velha'q67_10_2_v12606_2_2: Recebe usu�rias(os) encaminhados por este CRAS  - Judici�rio'
q67_10_3_velha'q67_10_3_v12606_3_3: Encaminha usu�rias(os) para este CRAS  - Judici�rio'
q67_10_4_velha'q67_10_4_v12606_4_4: Acompanha os encaminhamentos  - Judici�rio'
q67_10_5_velha'q67_10_5_v12606_5_5: Realiza reuni�es peri�dicas  - Judici�rio'
q67_10_6_velha'q67_10_6_v12606_6_6: Troca informa��es  - Judici�rio'
q67_10_7_velha'q67_10_7_v12606_7_7: Realiza estudos de caso em conjunto  - Judici�rio'
q67_10_8_velha'q67_10_8_v12606_8_8: Desenvolve ativiades em parceria  - Judici�rio'
q67_10_9_velha'q67_10_9_v12606_9_9: Servi�o ou institui��o n�o existente no Munic�pio ou no DF  - Judici�rio'
q68_0_velha'q68_0_v15504_0_0: N�o h� CREAS de refer�ncia para este CRAS'
q68_1_velha'q68_1_v15504_1_1: Fluxo de trabalho definido entre as equipes que realizam o atendimento de PSB e PSE'
q68_2_velha'q68_2_v15504_2_2: Responsabilidade compartilhada entre servi�os e realiza��o de reuni�es peri�dicas entre as equipes'
q68_3_velha'q68_3_v15504_3_3: Exist�ncia de planejamento conjunto das a��es preventivas e de atendimento a viola��es de direitos'
q68_4_velha'q68_4_v15504_4_4: H� dificuldades estruturais, territoriais ou de cobertura que impedem a realiza��o de trabalho articulado, entre CRAS e CREAS'
q68_99_velha'q68_99_v15504_99_99: Outros'
.









 













