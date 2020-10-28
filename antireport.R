## BIOL355 LAB REPORT-2212165-Friday/Morning 

data.anti= data.frame(Organisms=c("S.aureus","S.aureus","S.aureus","S.aureus","S.aureus","S.aureus","S.aureus","S.aureus","S.aureus","E.coli","E.coli","E.coli","E.coli","E.coli","E.coli","E.coli","E.coli","E.coli"),
                      APR15=c(13,12,12,11,11,11,14,12,12,17,15,20,15,15,15,16,16,15),
                      TOB10=c(15,17,13,14,19,14,15,17,18,18,16,16,17,16,15,15,17,16),
                      C30=c(25,18,18,19,22,21,22,22,25,8,8,8,7,7,8,8,9,8),
                      CN10=c(14,12,10,15,11,13,12,18,15,20,16,16,16,16,15,12,15,15),
                      S10=c(18,15,20,16,15,15,16,15,15,12,11,10,11,12,12,12,13,12),
                      TE30=c(28,13,25,21,22,25,25,19,26,16,23,22,11,22,22,18,21,16),
                      N30=c(12,13,13,11,12,15,12,19,12,19,15,16,17,17,17,18,17,16),
                      AK30=c(17,13,13,18,12,14,18,17,14,21,17,18,18,18,18,19,20,20),
                      KF30=c(26,30,28,25,25,23,30,24,28,10,12,12,23,10,11,11,12,12),
                      P10=c(11,13,11,10,10,12,12,17,10,6,6,6,6,6,6,6,6,6),
                      Control=c(6,6,6,6,6,6,11,6,6,6,6,6,6,6,6,6,6,6),
                      mouthwash=c(6,6,7,9,8,6,6,8,6,6,6,6,15,6,6,6,6,6),
                      detergent=c(6,7.5,8,6,9,7,7,9,6,6,6,6,6,6,6,6,6,6),
                      soap=c(6,6,6,11,6,7,14,6,6,6,6,6,6,6,6,6,7,6),
                      bleach=c(8,10,15,7,9,9,16,15,15,6,7,6,6,6,6,12,12,10),
                      alch70=c(7,6,6,6,7,7,6,7,6,6,7,6,6,6,6,6,6,6),
                      alc96=c(8,6,7,6,9,9,6,10,7,6,7,7,9,7,6,7,7,10),
                      zefiran=c(24,20,23,23,27,24,25,25,20,19,16,19,16,15,14,14,13,30))

s_aureus= subset(data.anti, subset = Organisms == "S.aureus")
e_coli= subset(data.anti, subset = Organisms== "E.coli")
##S.aureus Tests

shapiro.test(s_aureus$Control)#not normal
wilcox.test(s_aureus$APR15,s_aureus$Control)#0.0003204 < 0.05. 

wilcox.test(s_aureus$TOB10,s_aureus$Control)#0.0002137 < 0.05

wilcox.test(s_aureus$C30,s_aureus$Control)#0.0002085 < 0.05

wilcox.test(s_aureus$CN10,s_aureus$Control)#0.0003661

wilcox.test(s_aureus$S10, s_aureus$Control)#0.0001836 < 0.05

wilcox.test(s_aureus$TE30, s_aureus$Control)#0.000212 < 0.05

wilcox.test(s_aureus$N30,s_aureus$Control)#0.0002381 < 0.05

wilcox.test(s_aureus$AK30,s_aureus$Control)#0.000212

wilcox.test(s_aureus$KF30,s_aureus$Control)#0.0002137

wilcox.test(s_aureus$P10,s_aureus$Control)#0.0008277

##Disinfectans
wilcox.test(s_aureus$mouthwash, s_aureus$Control)#0.2186 p-value>0.05

wilcox.test(s_aureus$detergent, s_aureus$Control)#0.05602

wilcox.test(s_aureus$soap, s_aureus$Control)#0.3023 p-value>0.05

wilcox.test(s_aureus$bleach, s_aureus$Control)#0.001182

wilcox.test(s_aureus$alch70, s_aureus$Control)#0.2151 > 0.05

wilcox.test(s_aureus$alc96, s_aureus$Control)#0.05602 > 0.05

wilcox.test(s_aureus$zefiran, s_aureus$Control)#0.000212

##anova table of antibiotics

s_aureus_siganti= data.frame(Antibiotics= c("APR15", "APR15","APR15","APR15","APR15","APR15","APR15","APR15","APR15",
                                            "TOB10","TOB10","TOB10","TOB10","TOB10","TOB10","TOB10","TOB10","TOB10",
                             "C30","C30","C30","C30", "C30","C30","C30","C30","C30",
                             "CN10", "CN10","CN10","CN10","CN10","CN10","CN10","CN10","CN10",
                             "S10","S10","S10","S10","S10","S10","S10","S10","S10",
                             "TE30","TE30","TE30","TE30","TE30","TE30","TE30","TE30","TE30",
                             "N30","N30","N30","N30","N30","N30","N30","N30","N30",
                             "AK30","AK30","AK30","AK30","AK30","AK30","AK30","AK30","AK30",
                             "KF30","KF30","KF30", "KF30","KF30","KF30","KF30","KF30","KF30",
                             "P10","P10","P10","P10","P10","P10","P10","P10","P10"),
ClearZoneDiameter = c(13,12,12,11,11,11,14,12,12,15,17,13,14,19,14,15,17,18,25,18,18,19,22,21,22,22,25,14,12,10,15,11,13,12,18,15,18,15,20,16,15,15,16,15,15,28,13,25,21,22,25,25,19,26,
                                                   12,13,13,11,12,15,12,19,12,17,13,13,18,12,14,18,17,14,26,30,28,25,25,23,30,24,28,11,13,11,10,10,12,12,17,10))

kruskal.test(ClearZoneDiameter~Antibiotics, data = s_aureus_siganti)
s.siganti.aov= aov(ClearZoneDiameter~Antibiotics, data = s_aureus_siganti)
s.siganti.anova= anova(s.siganti.aov)
##Groups means are different. F=34.9692

#Tukey comparison of antibiotics
s.tukey.anti= TukeyHSD(s.siganti.aov)
s.tukey.anti
## The effectiveness of antibiotics
### KF30>TE30>C30>S10>AK30>TOB10>CN10>N30>APR15>P10

#bleach and zefiran sign. small than 0.05
shapiro.test(s_aureus$bleach)# 0.05053, normal
shapiro.test(s_aureus$zefiran)#0.3964, normal
var.test(s_aureus$zefiran,s_aureus$bleach)
t.test(s_aureus$bleach,s_aureus$zefiran, var.equal = T, alternative = "l")#zefiran is more effective

#E.coli Tests
shapiro.test(e_coli$Control)#identical, normal
shapiro.test(e_coli$APR15)#not normal, 0.0009768
wilcox.test(e_coli$APR15,e_coli$Control)#0.0001329

shapiro.test(e_coli$TOB10)#0.2729,normal
var.test(e_coli$TOB10,e_coli$Control)#p-value < 2.2e-16
t.test(e_coli$TOB10,e_coli$Control, var.equal = F)# p-value = 1.107e-09

shapiro.test(e_coli$C30)# 0.01221,normal
wilcox.test(e_coli$C30, e_coli$Control)#p-value = 0.0001152


shapiro.test(e_coli$CN10)#0.05025
var.test(e_coli$CN10, e_coli$Control)#p-value < 2.2e-16
t.test(e_coli$CN10,e_coli$Control, var.equal = F)#p-value = 6.333e-07

shapiro.test(e_coli$S10)#0.1318
var.test(e_coli$S10, e_coli$Control)#p-value < 2.2e-16
t.test(e_coli$S10,e_coli$Control, var.equal = F)#p-value = 4.718e-08

shapiro.test(e_coli$TE30)#0.09807
var.test(e_coli$TE30, e_coli$Control)#p-value < 2.2e-16
t.test(e_coli$TE30,e_coli$Control, var.equal = F)#p-value = 1.086e-05

shapiro.test(e_coli$N30)#0.5948
var.test(e_coli$N30, e_coli$Control)#p-value < 2.2e-16
t.test(e_coli$N30, e_coli$Control, var.equal = F)#p-value = 2.858e-09

shapiro.test(e_coli$AK30)#0.2574
var.test(e_coli$AK30, e_coli$Control)#p-value < 2.2e-16
t.test(e_coli$AK30, e_coli$Control, var.equal = F)#p-value = 1.916e-09

shapiro.test(e_coli$KF30)#5.011e-05, not normal
var.test(e_coli$KF30, e_coli$Control)# p-value < 2.2e-16
t.test(e_coli$KF30, e_coli$Control, var.equal = F)#p-value = 0.001175

shapiro.test(e_coli$P10)#normal, identical
wilcox.test(e_coli$P10, e_coli$Control)#NA

##Disinfectans
shapiro.test(e_coli$mouthwash)#p-value = 3.217e-07
wilcox.test(e_coli$mouthwash, e_coli$Control)#p-value = 0.3741

shapiro.test(e_coli$detergent)#identical,normal
var.test(e_coli$detergent, e_coli$Control)#p-value = NA
t.test(e_coli$detergent, e_coli$Control, var.equal = T)#constant

shapiro.test(e_coli$soap)#p-value = 3.217e-07
wilcox.test(e_coli$soap,e_coli$Control)#p-value = 0.3741

shapiro.test(e_coli$bleach)#p-value = 0.001972
wilcox.test(e_coli$bleach,e_coli$Control)# p-value = 0.03369

shapiro.test(e_coli$alch70)#p-value = 3.217e-07
wilcox.test(e_coli$alch70,e_coli$Control)#p-value = 0.3741

shapiro.test(e_coli$alc96)#p-value =0.01606
wilcox.test(e_coli$alc96,e_coli$Control)#p-value = 0.0016

shapiro.test(e_coli$zefiran)#p-value = 0.005199 
wilcox.test(e_coli$zefiran,e_coli$Control)#p-value = 0.0001567

##anova table of antibiotics
e_coli_siganti= data.frame(Antibiotics= c("APR15", "APR15","APR15","APR15","APR15","APR15","APR15","APR15","APR15",
                                          "TOB10","TOB10","TOB10","TOB10","TOB10","TOB10","TOB10","TOB10","TOB10",
                                          "C30","C30","C30","C30", "C30","C30","C30","C30","C30",
                                          "CN10", "CN10","CN10","CN10","CN10","CN10","CN10","CN10","CN10",
                                          "S10","S10","S10","S10","S10","S10","S10","S10","S10",
                                          "TE30","TE30","TE30","TE30","TE30","TE30","TE30","TE30","TE30",
                                          "N30","N30","N30","N30","N30","N30","N30","N30","N30",
                                          "AK30","AK30","AK30","AK30","AK30","AK30","AK30","AK30","AK30",
                                          "KF30","KF30","KF30", "KF30","KF30","KF30","KF30","KF30","KF30"),
                           ClearZoneDiameter= c(17,15,20,15,15,15,16,16,15,18,16,16,17,16,15,15,17,16,8,8,8,7,7,8,8,9,8,20,16,16,16,16,15,12,15,15,12,11,10,11,12,12,12,13,12,16,23,22,11,22,22,18,21,16,19,15,16,17,17,17,18,17,16,21,17,18,18,18,18,19,20,20,10,12,12,23,10,11,11,12,12))

kruskal.test(ClearZoneDiameter~Antibiotics, data=e_coli_siganti)#p-value = 2.7e-09
e.siganti.aov=aov(ClearZoneDiameter~Antibiotics, data=e_coli_siganti)
e.siganti.anova=anova(e.siganti.aov)
e.siganti.anova
##Tukey comparison of antibiotics
e.tukey.anti= TukeyHSD(e.siganti.aov)
e.tukey.anti
### The effectiveness of antibiotics
### AK30>TE30>N30>TOB10>APR15>CN10>KF30>C30

##anova of disinfectants, alch96, bleach, zefiran
e_coli_dis= data.frame(Disinfectants=c("bleach","bleach","bleach","bleach","bleach","bleach","bleach","bleach","bleach",
                                       "Alc96","Alc96","Alc96","Alc96","Alc96","Alc96","Alc96","Alc96","Alc96",
                                       "Zefiran","Zefiran","Zefiran","Zefiran","Zefiran","Zefiran","Zefiran","Zefiran","Zefiran"),
                       ClearZoneDiameter= c(6,7,6,6,6,6,12,12,10,6,7,7,9,7,6,7,7,10,19,16,19,16,15,14,14,13,30))

kruskal.test(ClearZoneDiameter~Disinfectants, data=e_coli_dis)
e.sigdis.aov= aov(ClearZoneDiameter~Disinfectants, data=e_coli_dis)
e.sigdis.anova=anova(e.sigdis.aov)
e.sigdis.anova#F>1, mean of disinfectants vary among each other.
##Tukey comparison of antibiotics
e.tukey.dis= TukeyHSD(e.sigdis.aov)
e.tukey.dis
##Zefiran > Alcohol96 > Bleach

##Friday-Morning, Group 5, S.aureus
benchsdata= data.frame(Antibiotics= c("APR15","TOB10","C30","CN10","S10","TE30","N30","AK30","KF30","P10",
                                      "Control","mouth-wash","detergent","soap","bleach","70%EtOH","96%EtOH","Zephiran"),
                       ClearZoneDiameter= c(12,13,18,10,20,25,13,13,28,11,6,7,8,6,15,6,7,23))
##comparison with S.aureus datas

##means of agents from S_aureus data and the benchs data differences. One sample t-test might be applied.
##if we state null hypothesis mean(mu) as the benchs data, we can find out the propability of 
#the differences between the bench data and all groups data.  
##Almost every value from the bench are overlapping with the all data from the groups.


##comparison of E.coli and S.aureus
#H0 states that there is no mean differences between S.aureus and E.coli agents.
#HA states that there is a difference between agents on two organisms. 
##Two sample t-test can be applied to test differences between diameter of agents
## on both S.aureus and E.coli.
shapiro.test(s_aureus$APR15)#normal,p-value = 0.08099
shapiro.test(e_coli$APR15)#not normal, 0.0009768
var.test(s_aureus$APR15,e_coli$APR15)#p-value = 0.1739
t.test(s_aureus$APR15,e_coli$APR15,var.equal = T)#p-value = 1.28e-05
#APR15 affects E.coli more.

shapiro.test(s_aureus$TOB10)#p-value = 0.5734
shapiro.test(e_coli$TOB10)#p-value = 0.2729, both are normal
var.test(s_aureus$TOB10,e_coli$TOB10)#p-value = 0.04975
t.test(s_aureus$TOB10,e_coli$TOB10,var.equal = F)#p-value = 0.5679
#TOB10 acts successfully on both organisms. In other words, both organisms
#dont have any resistance to TOB10.

shapiro.test(s_aureus$C30)#p-value = 0.2253
shapiro.test(e_coli$C30)# p-value = 0.01221, not normal
wilcox.test(s_aureus$C30, e_coli$C30)#p-value = 0.0003048
#C30 has more power on S.aureus; but, data from E.coli isn't normal so that not significant.
##p-value indicates C30 has different effects on the organisms. 
##Therefore, one of them might have resistance.

shapiro.test(s_aureus$CN10)#p-value = 0.807
shapiro.test(e_coli$CN10)#p-value = 0.05025
var.test(s_aureus$CN10,e_coli$CN10)#p-value = 0.6372
t.test(s_aureus$CN10,e_coli$CN10, var.equal = T)#p-value = 0.04399
#CN10 has almost the same effect on both organism.

shapiro.test(s_aureus$S10)#p-value = 0.001956
shapiro.test(e_coli$S10)#p-value = 0.1318
wilcox.test(s_aureus$S10,e_coli$S10)#p-value = 0.0003048
##S10 doesnot have same effectivenes on organisms. 
##From data, it seems S.aureus is affected by S10 more.

shapiro.test(s_aureus$TE30)#p-value = 0.2161
shapiro.test(e_coli$TE30)#p-value = 0.09807
var.test(s_aureus$TE30,e_coli$TE30)#p-value = 0.7379
t.test(s_aureus$TE30,e_coli$TE30, var.equal = T)#p-value = 0.08938
#TE30 is effective on both species.

shapiro.test(s_aureus$N30)#p-value = 0.005543
shapiro.test(e_coli$N30)#p-value = 0.5948
wilcox.test(s_aureus$N30, e_coli$N30)#p-value = 0.005573
#N30 has no equal power on the organisms.
##From the data, S.aureus is assumed to have resistance to S10. However, those values arenot normal.

shapiro.test(s_aureus$AK30)#p-value = 0.1087
shapiro.test(e_coli$AK30)#p-value = 0.2574
var.test(s_aureus$AK30,e_coli$AK30)#p-value = 0.1102
t.test(s_aureus$AK30,e_coli$AK30,var.equal = T)#p-value = 0.0008914
##AK30 does not have equal effectiveness on species. 
#From data, we might assume S.aureus has little more resistance to AK30.

shapiro.test(s_aureus$KF30)#p-value = 0.4257
shapiro.test(e_coli$KF30)#p-value = 5.011e-05
wilcox.test(s_aureus$KF30,e_coli$KF30)#p-value = 0.0004363
## KF30 has different effect on the organisms.
##If we assume the data from E.coli is normal, we can say E.coli has resistance to KF30.

shapiro.test(s_aureus$P10)#p-value = 0.0151
shapiro.test(e_coli$P10)#identical
wilcox.test(s_aureus$P10,e_coli$P10)#p-value = 0.0001526
##Penicillin has different power on both organisms. 
##From the E.coli tests part, we can observe E.coli has resistance to P10

shapiro.test(s_aureus$mouthwash)#p-value = 0.0107
shapiro.test(e_coli$mouthwash)# p-value = 3.217e-07
wilcox.test(s_aureus$mouthwash, e_coli$mouthwash)#p-value = 0.2186
##P-value indicates Mouth wash has same effectiveness

shapiro.test(s_aureus$detergent)#p-value = 0.1446
shapiro.test(e_coli$detergent)#identical
var.test(s_aureus$detergent,e_coli$detergent)#p-value < 2.2e-16
t.test(s_aureus$detergent,e_coli$detergent,var.equal = F)#p-value = 0.01281
##Detergent has different effectiveness on both organisms, 
#also E.coli showed no change compared to control.

shapiro.test(s_aureus$soap)#p-value = 0.0001871
shapiro.test(e_coli$soap)#p-value = 3.217e-07
wilcox.test(s_aureus$soap,e_coli$soap)#p-value = 0.2489
#Soap has the same effectiveness.

shapiro.test(s_aureus$bleach)#p-value = 0.05053
shapiro.test(e_coli$bleach)# p-value = 0.001972
wilcox.test(s_aureus$bleach,e_coli$bleach)#p-value = 0.02508
###Bleach doesnot have the same effectiveness, but we cannot decide since values arenot normal.

shapiro.test(s_aureus$alch70)#p-value = 0.0004194
shapiro.test(e_coli$alch70)#p-value = 3.217e-07
wilcox.test(s_aureus$alch70,e_coli$alch70)#p-value = 0.1396
###70% alcohol has the same effect on the organisms, however organisms seem to have resistance.

shapiro.test(s_aureus$alc96)#p-value = 0.1796
shapiro.test(e_coli$alc96)# p-value = 0.01606
wilcox.test(s_aureus$alc96,e_coli$alc96)#p-value = 0.89
##96%alcohol has the same effectiveness.


shapiro.test(s_aureus$zefiran)#p-value = 0.3946
shapiro.test(e_coli$zefiran)#p-value = 0.005199
wilcox.test(s_aureus$zefiran,e_coli$zefiran)#p-value = 0.006009
##Zefiran doesnot show the same effect on organism, possibly due to non-normal values.
##However, from the data we might assume E.coli have more resistance.

#The bench data are mostly consistent with these results. 
##For Grop5(S.aureus), S10, CN10 and bleach are different from the results above.
##For Group6(E.coli), APR15 and zefiran vary from others.








