#carregabdo biblioteca e abrindo base de dados#
rm(list = ls())
options( survey.lonely.psu = "adjust" )
options(OutDec=",") # Troca . por , nos outputs
library(PNADcIBGE)
library(survey)
library(srvyr)
library(tidyverse)
#dados brutos da PNADC2019#
pnadc_dat<-get_pnadc(2019, interview = 1, #informa ano e trmestre de interesse
            vars = NULL,labels = F,              #informa quais e como ver as variáveis
            design = F                       #informa o objeto para análise (base de microdados ou objeto com desenho amostral) 
            )
                     ## PONDERAÇÃO ## 
# Adiciona coluna de 1's ao arquivo de microdados
pnadc_dat$one <- 1
# Conta o numero de pessoas da amostra
sum(pnadc_dat$one) 

#Declara estrutura do plano amostral complexo
pnadc_plano <-     
  svydesign(            
    ids = ~ UPA ,        # Declara a unidade amostral 
    strata = ~ Estrato , # Declara a variável que contém os estratos (As 2 primeiras posições representam o código da Unidade da Federação)
    weights = ~ V1030 ,  # Declara variável com pesos (projeção da população 1t-4t)
    data = pnadc_dat ,  # Declara base de microdados
    nest = TRUE          # Declara que os estratos podem conter identificações identicas para UPA's distintas
  )

summary(pnadc_plano)
# Tabela com frequencias populacionais (estimativas IBGE para calibração)
df_pos <- data.frame( posest = unique( pnadc_dat$posest ), Freq = unique(pnadc_dat$V1030 ))
df_pos
# Calibrando pesos 
pnadc_calib <- postStratify( pnadc_plano , ~ posest , df_pos )
# Obtém fatores de calibração dos pesos da amostra
pnadc_fatores = weights(pnadc_calib) / weights(pnadc_plano)
boxplot(pnadc_fatores, horizontal = TRUE, xlab="Fatores de calibração")

#Finalmente, o objeto pnadc_calib pode ser utilizado para construir estimativas a cerca da população brasileira. 
#Uma forma de validar os procedimentos adotados até aqui é comparar a estimativa populacional encontrada utilizando o objeto pnadc_calib com 
#a estimativa divulgada pelo IBGE

                      # Valida estimativas populacionais
svytotal( ~ one , pnadc_calib )                 #estimativa populacional

#estimativa BPC total# 
svytotal( ~V5001A %in% 1, pnadc_calib )   

# Avalia Efeito do Plano Amostral
# Comparando as estimativas de segurados BPC>65 com e sem plano (+ calculo do EPA de Kish)
round(svymean( ~ V5001A2 , subset(pnadc_plano, V2009>=65 & V5001A %in% 1) , na.rm = TRUE ),2)

round(svymean( ~ V5001A2 , subset(pnadc_calib, V2009>=65 & V5001A %in% 1) , na.rm = TRUE, deff="replace"),2)


# Salva objeto final
saveRDS(pnadc_calib,"pnadc_calib_2019")

# Limpa objetos da memória
rm(pnadc_dat,pnadc_plano)

                                      ##Estimando estatísticas de interesse## 
# Limpando objetos da memória
rm(list = ls()) 
# Carregando dados da PNADC 
pnadc_calib <- readRDS(file="pnadc_calib_2019")

# Modificando objeto para permitir sintaxe tipo tidyverse
pnadc_calib <- as_survey_design(pnadc_calib)

#Preparando as variáveis para cálculo de estimativas#

pnadc_calib <-  update(pnadc_calib, nivel_renda = factor( 1 + findInterval( VD4019 , seq( 5 , 60 , 5 ))),
                       sexo = as.numeric( V2007 == 1 ), idade65 = as.numeric(V2009>=65), analfabeto = 1*(V3001==2),
                       BPC65 = ifelse( idade65 == 1 , as.numeric( V5001A%in% 1 ) , NA ) ,
                       sembpc65 = ifelse( idade65 == 1 , as.numeric( V5001A %in% 2 ) , NA ) ,
                       # (rendimento habitual do trabalho principal)#
                       rendadossembpcVD4016 = ifelse( V5001A %in% 2 & VD4015 %in% 1 , VD4016 , NA ) ,
                       # (rendimento habitual de todos os trabalhos)#
                       rendadossembpcVD4019 = ifelse( V5001A %in% 2 & VD4015 %in% 1 , VD4019 , NA ) ,
                       # (rendimento efetivo do todos os trabalhos)#
                       rendadossembpcVD4020 = ifelse( V5001A %in% 2 & VD4015 %in% 1 , VD4020 , NA ) ,
                       #indicador de nível superior
                       superior = 1*( VD3004 == 7)
                       )


                                # Estimativas pontuais#

#calculando estibativa do número total de BPC65 por sexo#

#totalBPC65<-svymean( ~ rendadossembpcVD4020 + rendadossembpcVD4019, 
                     # subset(pnadc_calib, V5001A %in% 2) ,
                      #na.rm = TRUE ) 
#round(coef(rendasemBPC)




# Calcula estimativas do rendimento médio nominal de todos os SEM BPC -

rendasemBPC<-svymean( ~ rendadossembpcVD4020 + rendadossembpcVD4019, 
                   subset(pnadc_calib, V5001A %in% 2) ,
                   na.rm = TRUE ) 
round(coef(rendasemBPC))
# Calcula estimativas do rendimento dos BPC65-
rendaBPC65<-svymean( ~V5001A2, 
                      subset(pnadc_calib, BPC65 %in% 1) ,
                      na.rm = TRUE ) 
round(coef(rendaBPC65))

#Calcula estimativas do rendimento BPC65 por sexo# 

rend_BPC65sexo <- svyby( ~ V5001A2,
                     ~ sexo , 
                     subset(pnadc_calib, BPC65 %in% 1), 
                     svymean , na.rm = TRUE )
round(rend_BPC65sexo)

# estimativa da renda dos sem BPC por sexo #
rend_semBPCsexo <- svyby(  ~ rendadossembpcVD4020 + rendadossembpcVD4019,
                     ~ sexo , 
                     subset(pnadc_calib, V5001A %in% 2), 
                     svymean , na.rm = TRUE )

round(rend_semBPCsexo)

# Calculando estimativa da Taxa de Analfabetismo dos BPC65# 
Taxa_analfBPC65 <- svymean(~analfabeto,
                      subset(pnadc_calib, BPC65 %in% 1), 
                      na.rm = TRUE)

round(100*coef(Taxa_analfBPC65),2)
round(100*SE(Taxa_analfBPC65),2)
# Calculando estimativa da Taxa de Analfabetismo dos com BPC em geral# 
Taxa_analfBPC<- svymean(~analfabeto,
                           subset(pnadc_calib, V5001A%in%1), 
                           na.rm = TRUE)

round(100*coef(Taxa_analfBPC),2)
round(100*SE(Taxa_analfBPC),2)
# Calculando estimativa da Taxa de Analfabetismo dos sem BPC# 
Taxa_analfsemBPC <- svymean(~analfabeto,
                           subset(pnadc_calib,V5001A %in% 2), 
                           na.rm = TRUE)
round(100*coef(Taxa_analfsemBPC),2)
round(100*SE(Taxa_analfsemBPC),2)
# Calcula estimativa da Taxa de Analfabetismo dos indivíduos da base#
Taxa_analf <- svymean(~analfabeto,
                      subset(pnadc_calib, V2009 >=0), 
                      na.rm = TRUE)
round(100*coef(Taxa_analf),2)
round(100*SE(Taxa_analf),2)
                                        
