#limpando a instancia
rm(list = ls())
gc()
#bibliotecas
library(dplyr)
library(readxl)
library(psych)
library(data.table)
library(bit64)
library(PNADcIBGE)
library(survey)
library(srvyr)
library(tidyverse)
#definindo fonte da busca
setwd("D:/basededadosBPC")
dir()
#defininco as variáveis que serão importadas

var = c("Ano", "Trimestre", "UF", "Capital", "RM_RIDE", "UPA", "Estrato", "V1008", "V1014", "V1022", "V1023", "V1030", "V1031","V1032", "posest",
        'V2005','V2007','V2009','V2010', #caracteristicas do indivíduo
        "V2001", # quantidade de pessoas no domicilio
        'V5002A','V5002A2', #PBF
        'V3001','V3002','V3003A', #Escolaridade
        'VD4002',"VD4009",'V4029','V4012','V4009','V4019','V4032','VD4005','V4071', #Variaveis de emprego
        'V4001', #trabalhou ao menos uma hora em atividade remunerada
        'V4073', #Embora não tenha tomado providência para conseguir trabalho, gostaria de ter trabalhado ?
        'V4006A', #motivo do afastamento do trabalho
        'V4039','V4039C', #horas trabalhadas trab principal
        'VD4001', #dentro ou fora da força de trabalho
        "V5001A2", "V5001A", #BPC
        'VD4046','VD4047', #Rendimentos 
        'VD5003', #Faixas de rendimento domiciliar per capita
        "VD5007",'VD5008', "VD5010", # Rendimentos domiciliar
        "VD4012") #contribui com a previdencia

#importando base
dados_pnadc <- read_pnadc("PNADC_2019_visita1.txt", "input_PNADC_2019_visita1_20200826.txt", vars = var)

#estudando e tratando as vvariáveis VD4009 E VD4012
head(dados_pnadc)
table(dados_pnadc$VD4005, dados_pnadc$VD4002, exclude = NULL)
table(dados_pnadc$VD4005, dados_pnadc$VD4001, exclude = NULL)
prop.table(table(dados_pnadc$VD4009,exclude = NULL))
prop.table(table(dados_pnadc$VD4012,exclude = NULL))

prop.table(table(dados_pnadc$VD4009,dados_pnadc$V4009,exclude = NULL)) # os NAS são indivíduos desempregados
prop.table(table(dados_pnadc$VD4009,dados_pnadc$V4012,exclude = NULL))
prop.table(table(dados_pnadc$VD4009,dados_pnadc$V4001,exclude = NULL)) # os NAS de VD4009 são indivíduos que não trabalharam, aqui se confirma isso mais uma vez
prop.table(table(dados_pnadc$VD4012,dados_pnadc$V4009,exclude = NULL))# os NAS de VD4012 tbm são indivíduos desocupados

unique(dados_pnadc$VD4009) 
typeof(dados_pnadc$VD4009)

# forca trabalho
dados_pnadc$Tipo=NA
dados_pnadc$Tipo[dados_pnadc$VD4009=="01"]="Privado_Formal"
dados_pnadc$Tipo[dados_pnadc$VD4009=="02" & dados_pnadc$VD4012==1]="Privado_Informal"
dados_pnadc$Tipo[dados_pnadc$VD4009=="02" & dados_pnadc$VD4012==2]="Privado_Informal"
dados_pnadc$Tipo[dados_pnadc$VD4009=="03"]="Domestico_Formal"
dados_pnadc$Tipo[dados_pnadc$VD4009=="04" & dados_pnadc$VD4012==1]="Domestico_Informal"
dados_pnadc$Tipo[dados_pnadc$VD4009=="04" & dados_pnadc$VD4012==2]="Domestico_Informal"
dados_pnadc$Tipo[dados_pnadc$VD4009=="05"]="Publico_Formal"
dados_pnadc$Tipo[dados_pnadc$VD4009=="06" & dados_pnadc$VD4012==1]="Publico_Informal"
dados_pnadc$Tipo[dados_pnadc$VD4009=="06" & dados_pnadc$VD4012==2]="Publico_Informal"
dados_pnadc$Tipo[dados_pnadc$VD4009=="07"]="Militar e Estat"
dados_pnadc$Tipo[dados_pnadc$VD4009=="08" & dados_pnadc$VD4012==1]="Empregador_Informal"
dados_pnadc$Tipo[dados_pnadc$VD4009=="08" & dados_pnadc$VD4012==2]="Empregador_Informal"
dados_pnadc$Tipo[ dados_pnadc$VD4009=="09" & dados_pnadc$VD4012==1]="Conta-Própria"
dados_pnadc$Tipo[dados_pnadc$VD4009=="09" & dados_pnadc$VD4012==2]="Conta-Própria"
dados_pnadc$Tipo[ dados_pnadc$VD4009=="10" & dados_pnadc$VD4012==1]="Trabalhador Fam Aux."
dados_pnadc$Tipo[ dados_pnadc$VD4009=="10" & dados_pnadc$VD4012==2]="Trabalhador Fam Aux."
dados_pnadc$Tipo[!is.na(dados_pnadc$VD4001) & dados_pnadc$VD4001==1 & dados_pnadc$VD4002==2]="Desocupado" 

dados_pnadc$Tipo[dados_pnadc$V2009<14] = "Menor de 14"
dados_pnadc$Tipo[dados_pnadc$VD4001==2] = "Fora da forca"

table(dados_pnadc$Tipo, dados_pnadc$VD4009, exclude = NULL)

summary(dados_pnadc$VD5007)

dados_pnadc$VD5007[is.na(dados_pnadc$VD5007)] = 0

summary(dados_pnadc$VD5007)
dados_pnadc$Faixa = cut(dados_pnadc$VD5007, breaks = c(-0.01, 500, 1000, 3500000), labels = c("Ate 500", "500-1000", "+1000"))
table(dados_pnadc$Faixa, exclude = NULL)

aggregate(VD5007 ~Faixa, data = dados_pnadc, FUN = max)
aggregate(VD5007 ~Faixa, data = dados_pnadc, FUN = min)

dados_pnadc$Ate500 = ifelse(dados_pnadc$Faixa=="Ate 500", 1, 0)
table(dados_pnadc$Ate500, dados_pnadc$Faixa, exclude = NULL)



table(dados_pnadc$V5001A, exclude = NULL)
#verificando se alguem respondeu q nao recebia o BPC e depois informava o valor q recebia do beneficio
summary(dados_pnadc$V5001A2[dados_pnadc$V5001A==2])

dados_pnadc$V5001A2[is.na(dados_pnadc$V5001A2)] = 0
#agrupando
BPC_dom = dados_pnadc %>%
  group_by(UPA, V1008, V1014) %>%
  summarise(Total_BPC = sum(V5001A2))
(BPC_dom)
# Excluindo todos aqueles que são 17,18 e 19. 


dados_pnadc = full_join(dados_pnadc, BPC_dom, by=c("UPA", "V1008", "V1014"))
# rm(BPC_dom)

## Renda (bruta total(VD5007 e VD50010(considera rendimentos em cartão/tíquete transporte ou alimentação)) - BPC(V5001A2)/ n pessas na família(V2001)
# apos analisar os dados verificou-se que existem valores negativos nas vars VD5007 E VD5010 e isso era causado 
# pessoas 17,18 e 19 na posicao familiar, como elas não são importantes para a nossa análise iremos retira-las da base

dados_pnadc = subset(dados_pnadc, !V2005%in%c(17, 18, 19)) 
dados_pnadc$VD5007[is.na(dados_pnadc$VD5007)] = 0
# gerando a variável
dados_pnadc$Renda_exc_BPC1 = dados_pnadc$VD5007 - dados_pnadc$Total_BPC
summary(dados_pnadc$Renda_exc_BPC1)

# considerando rendimentos em cartão/tíquete transporte ou alimentação
dados_pnadc$VD5007[is.na(dados_pnadc$VD5010)] = 0

dados_pnadc$Renda_exc_BPC2 = dados_pnadc$VD5010 - dados_pnadc$Total_BPC
summary(dados_pnadc$Renda_exc_BPC2)

# renda exc percapita

dados_pnadc$Renda_exc_percapita1 = dados_pnadc$Renda_exc_BPC1 / dados_pnadc$V2001 
summary(dados_pnadc$Renda_exc_percapita1)


# considerando rendimentos em cartão/tíquete transporte ou alimentação

dados_pnadc$Renda_exc_percapita2 = dados_pnadc$Renda_exc_BPC2 / dados_pnadc$V2001 
summary(dados_pnadc$Renda_exc_percapita2)
#g014))

#dummies para posição no domicilio 
# dummies para chefe, conjuge, filhos do chefe e outros parentes
# primeiro vamos tratar a variável V2005
summary(dados_pnadc$V2005)
typeof(dados_pnadc$V2005)
dados_pnadc$V2005 <- as.numeric(dados_pnadc$V2005)
summary(dados_pnadc$V2005)

summary(dados_pnadc$V2005)
dados_pnadc$VD5007[is.na(dados_pnadc$V2005)] = 0

dados_pnadc$Faixaposicao = cut(dados_pnadc$V2005, breaks = c(0,1,3,5, 20), labels = c("Chefe", "conjuge", "filhos",'outros_parentes'))
table(dados_pnadc$Faixaposicao, exclude = NULL)
table(dados_pnadc$V2005, exclude = NULL)

aggregate(V2005 ~Faixaposicao, data = dados_pnadc, FUN = max)
aggregate(V2005 ~Faixaposicao, data = dados_pnadc, FUN = min)

#DUMMIE DE CHEFE
dados_pnadc$chefe = ifelse(dados_pnadc$Faixaposicao=="Chefe", 1, 0)
table(dados_pnadc$chefe, dados_pnadc$Faixaposicao, exclude = NULL)
table(dados_pnadc$V2005, exclude = NULL)
#DUMMIE DE CONJUGE
dados_pnadc$conjuge = ifelse(dados_pnadc$Faixaposicao=="conjuge", 1, 0)
table(dados_pnadc$conjuge, dados_pnadc$Faixaposicao, exclude = NULL)
table(dados_pnadc$V2005,dados_pnadc$Faixaposicao, exclude = NULL)
#DUMMIE DE FILHOS
dados_pnadc$filhos = ifelse(dados_pnadc$Faixaposicao=="filhos", 1, 0)
table(dados_pnadc$filhos, dados_pnadc$Faixaposicao, exclude = NULL)
table(dados_pnadc$V2005,dados_pnadc$Faixaposicao, exclude = NULL)

#DUMMIE DE OUTROS FAMILIARES
dados_pnadc$outros_parentes = ifelse(dados_pnadc$Faixaposicao=="outros_parentes", 1, 0)
table(dados_pnadc$outros_parentes, dados_pnadc$Faixaposicao, exclude = NULL)
table(dados_pnadc$V2005,dados_pnadc$Faixaposicao, exclude = NULL)


## dummies para idade e escolaridade
#peimeiro para idade
summary(dados_pnadc$V2009)
typeof(dados_pnadc$V2005)
table(dados_pnadc$V2009, exclude = NULL)


dados_pnadc$Faixaidade = cut(dados_pnadc$V2009, breaks = c(-0.1,9,14,17, 115), labels = c("ate9", "idade10a14", "idade15a17",'idademaisdq17'))
table(dados_pnadc$Faixaidade, exclude = NULL)
table(dados_pnadc$V2009, exclude = NULL)

aggregate(V2009 ~Faixaidade, data = dados_pnadc, FUN = min)
aggregate(V2009 ~Faixaidade, data = dados_pnadc, FUN = max)

# idadede10a14 
dados_pnadc$idade10a14 = ifelse(dados_pnadc$Faixaidade=="idade10a14", 1, 0)
table(dados_pnadc$idade10a14, dados_pnadc$Faixaidade, exclude = NULL)
table(dados_pnadc$V2009,dados_pnadc$idade10a14, exclude = NULL)
#idade_15a17
dados_pnadc$idade15a17 = ifelse(dados_pnadc$Faixaidade=="idade15a17", 1, 0)
table(dados_pnadc$idade15a17, dados_pnadc$Faixaidade, exclude = NULL)
table(dados_pnadc$V2009, dados_pnadc$idade15a17, exclude = NULL)

#frequenta_escola
summary(dados_pnadc$V3002)
table(dados_pnadc$V3002, exclude = NULL)


#dentro ou fora da força (Condição em relação à força de trabalho na semana de referência para pessoas de 14 anos ou mais de idade)
prop.table(table(dados_pnadc$VD4001,exclude = NULL))

#onde estão os 20% de NAs e por que ? 
table(dados_pnadc$VD4001,dados_pnadc$Faixaposicao,exclude = NULL)

table(dados_pnadc$VD4001,dados_pnadc$Faixaidade,exclude = NULL) #estão na faixa de idade de menores do que 14 anos, logo são fora da PEA
dados_pnadc$VD4001[is.na(dados_pnadc$VD4001)] = 0
prop.table(table(dados_pnadc$VD4001,exclude = NULL)) # temos agr 3 categorias,
# onde 0 são pessoas abaixo da idade de trabalhar
# 1 são pessoas na força e 2 são pessoas fora da força 

#idade PEA #
dados_pnadc$IdadePEA = cut(dados_pnadc$V2009, breaks = c(-0.1,15, 115), labels = c("idade0a15", "idade16oumais"))
table(dados_pnadc$IdadePEA, exclude = NULL)
table(dados_pnadc$V2009,dados_pnadc$IdadePEA, exclude = NULL)

# idadede10a14 
dados_pnadc$idade0a15 = ifelse(dados_pnadc$IdadePEA =="idade0a15", 1, 0)
#verificando
table(dados_pnadc$idade0a15, dados_pnadc$IdadePEA , exclude = NULL)
table(dados_pnadc$V2009,dados_pnadc$idade0a15, exclude = NULL)
#idade_15a17
dados_pnadc$idade16oumais = ifelse(dados_pnadc$IdadePEA =="idade16oumais", 1, 0)
#verificando
table(dados_pnadc$idade16oumais, dados_pnadc$IdadePEA , exclude = NULL)
table(dados_pnadc$V2009, dados_pnadc$idade16oumais, exclude = NULL)

###############################3

table(dados_pnadc$VD4001,dados_pnadc$Tipo,exclude = NULL)

table(dados_pnadc$idade15a17,dados_pnadc$Tipo,exclude = NULL)

#A PEA é obtida pela soma da população ocupada e desocupada com 16 anos ou mais de idade.
#Foram classificadas como ocupadas na semana de referência as pessoas que exerceram trabalho, 
# remunerado ou sem remuneração, durante pelo menos uma hora completa na semana de referência ou que tinham 
# trabalho remunerado do qual estavam temporariamente afastadas nessa semana.
# pessoas desocupadas foram consideradas as que procuraram emprego 

## estudando a variável V4071 (procurou emprego?)
table(dados_pnadc$V4071,dados_pnadc$Faixaidade,exclude = NULL)
table(dados_pnadc$V4071,dados_pnadc$V4073,exclude = NULL)
table(dados_pnadc$V4071,dados_pnadc$V4009,exclude = NULL)
table(dados_pnadc$V4071,dados_pnadc$VD4001,exclude = NULL)
prop.table(table(dados_pnadc$V4071,dados_pnadc$VD4001,exclude = NULL)) #0,19% das variáveis são pessoas abaixo da idade de trabalhar

table(dados_pnadc$V4071,dados_pnadc$Tipo,dados_pnadc$VD4001,exclude = NULL)
# 84790NAs sao pessoas com >14 anos (fora de idade de trabalhar)
# os outros são indivíduos dentro da força e empregados de forma formal ou informal
# como essas pessoas estão empregadas, provavelmente não procuraram emprego, então vamos definir NA=2 
dados_pnadc$V4071[is.na(dados_pnadc$V4071)] = 2
#verificando
prop.table(table(dados_pnadc$V4071,dados_pnadc$VD4001,exclude = NULL))
table(dados_pnadc$V4071,dados_pnadc$Tipo,dados_pnadc$VD4001,exclude = NULL) # vemos que a maioria dos desocupados procuraram emprego 
prop.table(table(dados_pnadc$V4071,dados_pnadc$Tipo,exclude = NULL))
prop.table(table(dados_pnadc$V4071,dados_pnadc$Tipo=='Desocupado',exclude = NULL))
prop.table(table(dados_pnadc$V4071,exclude = NULL))

prop.table(table(dados_pnadc$Faixaidade,dados_pnadc$Tipo=='Desocupado',exclude = NULL))
prop.table(table(dados_pnadc$Trimestre,dados_pnadc$Tipo=='Desocupado',dados_pnadc$idade16oumais,exclude = NULL))
table(dados_pnadc$Tipo, dados_pnadc$VD4001,exclude = NULL)

# criando as definições de PEA 
# definindo NA = 0 
dados_pnadc$V4006A[is.na(dados_pnadc$V4006A)] = 0
dados_pnadc$PEA= NA
dados_pnadc$PEA[dados_pnadc$Tipo!='Desocupado' & dados_pnadc$V2009>13]="Ocupado14"                 
dados_pnadc$PEA[dados_pnadc$Tipo!="Desocupado" & dados_pnadc$V2009>13 & dados_pnadc$V4006A!=0]="Afastado14"
dados_pnadc$PEA[dados_pnadc$V4006A== 0 & dados_pnadc$Tipo=='Desocupado' & dados_pnadc$V2009>13]="Desocupado" 
dados_pnadc$PEA[dados_pnadc$V2009<14] = "Menor de 14"


table(dados_pnadc$PEA,dados_pnadc$Tipo,exclude = NULL)

table(dados_pnadc$Tipo,dados_pnadc$V2009>13,exclude = NULL)
table(dados_pnadc$PEA,dados_pnadc$idade10a14,dados_pnadc$V2009>13,exclude = NULL)
dados_pnadc$PEA[is.na(dados_pnadc$PEA)] = 'Desocupado'
table(dados_pnadc$PEA,dados_pnadc$Tipo,exclude = NULL)

#distribuição da PEA por tipo
table(dados_pnadc$PEA,dados_pnadc$Tipo,exclude = NULL)

#Dentro ou fora da PEA

dados_pnadc$PEAOUNAO = ifelse(dados_pnadc$PEA =="Desocupado"| dados_pnadc$PEA =="Ocupado14" | dados_pnadc$PEA =="Afastado14" , 1, 0)
table(dados_pnadc$PEAOUNAO, exclude = NULL)
prop.table(table(dados_pnadc$PEAOUNAO))
table(dados_pnadc$PEAOUNAO,dados_pnadc$Trimestre)
table(dados_pnadc$PEAOUNAO,dados_pnadc$Tipo)



