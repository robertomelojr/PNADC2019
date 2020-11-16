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
        'VD4004A','VD4005',#SUBOCUPADAS E Desalentados
        'V4056','V4062','V4063A','V4064A',      #referente a horas trabalhadas
        'V4039','V4039C', #horas trabalhadas trab principal
        'V4001', #trabalhou ao menos uma hora em atividade remunerada
        'V4073', #Embora não tenha tomado providência para conseguir trabalho, gostaria de ter trabalhado ?
        'V4006A', #motivo do afastamento do trabalho
        'VD4001', #dentro ou fora da força de trabalho
        "V5001A2", "V5001A", #BPC
        'VD4046','VD4047', #Rendimentos 
        'VD5003', #Faixas de rendimento domiciliar per capita
        "VD5007",'VD5008', "VD5010", # Rendimentos domiciliar
        "VD4012") #contribui com a previdencia

#importando as bases #
# 2017
pnadc2017 <- read_pnadc("PNADC_2017_visita1.txt", "input_PNADC_2017_visita1.txt", vars = var)
#2018
pnadc2018 <- read_pnadc("PNADC_2018_visita1.txt", "input_PNADC_2018_visita1.txt", vars = var)
#2019
pnadc2019 <- read_pnadc("PNADC_2019_visita1.txt", "input_PNADC_2019_visita1_20200826.txt", vars = var)

# juntando as bases # Empilhando duas bases uma sobre a outra com rbind
dados_pnadc = rbind(pnadc2017,pnadc2018,pnadc2019)

table(dados_pnadc$V5001A, exclude = NULL)

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
#### #############################################3mercado de trabalho ###################################################

##############3 forca trabalho ##################

dados_pnadc$Tipo=NA
dados_pnadc$Tipo[dados_pnadc$VD4009=="01"]="Privado_Formal"
dados_pnadc$Tipo[dados_pnadc$VD4009=="02" & dados_pnadc$VD4012==1]="Privado_Informal"
dados_pnadc$Tipo[dados_pnadc$VD4009=="02" & dados_pnadc$VD4012==2]="Privado_Informal"
dados_pnadc$Tipo[dados_pnadc$VD4009=="03"]="Domestico_Formal"
dados_pnadc$Tipo[dados_pnadc$VD4009=="04" & dados_pnadc$VD4012==1]="Domestico_formal"
dados_pnadc$Tipo[dados_pnadc$VD4009=="04" & dados_pnadc$VD4012==2]="Domestico_Informal"
dados_pnadc$Tipo[dados_pnadc$VD4009=="05"]="Publico_Formal"
dados_pnadc$Tipo[dados_pnadc$VD4009=="06" & dados_pnadc$VD4012==1]="Publico_formal"
dados_pnadc$Tipo[dados_pnadc$VD4009=="06" & dados_pnadc$VD4012==2]="Publico_Informal"
dados_pnadc$Tipo[dados_pnadc$VD4009=="07"]="Militar e Estat"
dados_pnadc$Tipo[dados_pnadc$VD4009=="08" & dados_pnadc$VD4012==1]="Empregador_formal"
dados_pnadc$Tipo[dados_pnadc$VD4009=="08" & dados_pnadc$VD4012==2]="Empregador_Informal"
dados_pnadc$Tipo[ dados_pnadc$VD4009=="09" & dados_pnadc$VD4012==1]="Conta-Própria"
dados_pnadc$Tipo[dados_pnadc$VD4009=="09" & dados_pnadc$VD4012==2]="Conta-Própria_Informal"
dados_pnadc$Tipo[ dados_pnadc$VD4009=="10" & dados_pnadc$VD4012==1]="Trabalhador Fam Aux."
dados_pnadc$Tipo[ dados_pnadc$VD4009=="10" & dados_pnadc$VD4012==2]="Trabalhador Fam Aux."
dados_pnadc$Tipo[!is.na(dados_pnadc$VD4001) & dados_pnadc$VD4001==1 & dados_pnadc$VD4002==2]="Desocupado" 
dados_pnadc$Tipo[dados_pnadc$V2009<14] = "Menor de 14"
dados_pnadc$Tipo[dados_pnadc$VD4001==2] = "Fora da forca"

#TIPO2
dados_pnadc$Tipo2=NA
###informalidade: Empregado no setor privado (sem carteira assinada) ou Trabalhador por Conta-Própria 
#(sem CNPJ e Sem Contribuição para Previdência Oficial) ou Empregadores (sem CNPJ e Sem Contribuição para Previdência 
#Oficial) ou Trabalhador doméstico (sem carteira de trabalho assinada) ou Trabalhador familiar auxiliar
#privado sem carteira assinada
dados_pnadc$Tipo2[dados_pnadc$VD4009=='02']="Informal" 
dados_pnadc$Tipo2[dados_pnadc$VD4009=='01'& dados_pnadc$VD4012==2]= 'Informal'
#conta propria sem CNPJ q nao contribui p prev social
dados_pnadc$Tipo2[dados_pnadc$VD4009=='09'& dados_pnadc$VD4012==2]='Informal' 
dados_pnadc$Tipo2[dados_pnadc$VD4009=='09'& dados_pnadc$V4019==2]='Informal'
# empregadores sem CNPJ e sem PREV 
dados_pnadc$Tipo2[dados_pnadc$VD4009=='08'& dados_pnadc$VD4012==2]='Informal'
dados_pnadc$Tipo2[dados_pnadc$VD4009=='08'& dados_pnadc$V4019==2]='Informal'
# Trabalhador doméstico (sem carteira de trabalho assinada) ou Trabalhador familiar auxiliar)
dados_pnadc$Tipo2[dados_pnadc$VD4009=="04" & dados_pnadc$VD4012==2]="Informal"
dados_pnadc$Tipo2[dados_pnadc$VD4009=="10"]='Informal'
#########################33desocupados##############
dados_pnadc$Tipo2[!is.na(dados_pnadc$VD4001) & dados_pnadc$VD4001==1 & dados_pnadc$VD4002==2]="Desocupados"
############################################subocupados##################
#varivaeis 'V4039','V4039C','V4056','V4062','V4063A','V4064A',
dados_pnadc$Tipo2[dados_pnadc$VD4004A=='1']='Subocupados'
###desalentados###
dados_pnadc$Tipo2[dados_pnadc$VD4005=='1']='Desalentados'
########################fora da forca#################
dados_pnadc$Tipo2[dados_pnadc$VD4001=='2'] = "Fora da forca"
table(dados_pnadc$Tipo2,exclude = NULL)
table(dados_pnadc$Tipo2,dados_pnadc$VD4002,exclude = NULL) # disso se observa que os NAS = Pessoas ocupadas  são os ocupados
###ocupados
dados_pnadc$Tipo2[is.na(dados_pnadc$Tipo2) & dados_pnadc$VD4002==1]='Ocupados'
#Fora da idade de trabalhar 
dados_pnadc$Tipo2[is.na(dados_pnadc$Tipo2) & dados_pnadc$V2009<=14]='Fora da idade de trabalhar'
table(dados_pnadc$Tipo2,exclude = NULL)

#A PEA é obtida pela soma da população ocupada e desocupada com 14 anos ou mais de idade.
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
dados_pnadc$PEA[dados_pnadc$VD4001==2 & dados_pnadc$V2009>13]="Ocupado14"                 
dados_pnadc$PEA[dados_pnadc$Tipo!="Desocupado" & dados_pnadc$V2009>13 & dados_pnadc$V4006A!=0]="Afastado14"
dados_pnadc$PEA[dados_pnadc$V4006A== 0 & dados_pnadc$Tipo=='Desocupado'& dados_pnadc$V4071==1 & dados_pnadc$V2009>13]="Desocupado" 
dados_pnadc$PEA[dados_pnadc$V2009<14] = "Menor de 14"


table(dados_pnadc$PEA,dados_pnadc$Tipo,exclude = NULL)

table(dados_pnadc$Tipo,dados_pnadc$V2009>13,exclude = NULL)
table(dados_pnadc$PEA,dados_pnadc$idade10a14,dados_pnadc$V2009>13,exclude = NULL)
dados_pnadc$PEA[is.na(dados_pnadc$PEA)] = 'Desocupado'
table(dados_pnadc$PEA,dados_pnadc$Tipo,exclude = NULL)

#PEA
dados_pnadc$FAIXAPEA = NA
dados_pnadc$FAIXAPEA[dados_pnadc$VD4001==2]="PEA"                 
dados_pnadc$FAIXAPEA[dados_pnadc$VD4001==1]="NAOPEA"
dados_pnadc$FAIXAPEA[is.na(dados_pnadc$FAIXAPEA)] = 'Fora_da_idade_de_trabalhar'
table(dados_pnadc$FAIXAPEA,exclude = NULL)
prop.table(table(dados_pnadc$FAIXAPEA))

#DUMMIE PEA
dados_pnadc$PEA = NA
dados_pnadc$PEA[dados_pnadc$VD4001==2 ] = 1                
dados_pnadc$PEA[dados_pnadc$VD4001==1 ] = 0
dados_pnadc$PEA[is.na(dados_pnadc$PEA)] = 0
table(dados_pnadc$PEA,dados_pnadc$FAIXAPEA)
table(dados_pnadc$PEA)
table(dados_pnadc$FAIXAPEA,dados_pnadc$PEA,exclude= NULL)

#Dentro ou fora DA IDADE DE TRABALHAR

dados_pnadc$IDADETRABALHAROUNAO = ifelse(dados_pnadc$PEA =="Desocupado"| dados_pnadc$PEA =="Ocupado14" | dados_pnadc$PEA =="Afastado14" , 1, 0)
table(dados_pnadc$IDADETRABALHAROUNAO, exclude = NULL)
prop.table(table(dados_pnadc$PEAOUNAO))
table(dados_pnadc$IDADETRABALHAROUNAO,dados_pnadc$Trimestre)
table(dados_pnadc$IDADETRABALHAROUNAO,dados_pnadc$VD4001)

########## domicilios e BPC#############
#verificando se alguem respondeu q nao recebia o BPC e depois informava o valor q recebia do beneficio
summary(dados_pnadc$V5001A2[dados_pnadc$V5001A==2])
dados_pnadc$V5001A2[is.na(dados_pnadc$V5001A2)] = 0

# Recebe BPC com mais dq 65 anos#
dados_pnadc$BPC65 = NA
dados_pnadc$BPC65 = ifelse(dados_pnadc$V5001A==1 & dados_pnadc$V2009 >=65 ,1,0)

dados_pnadc$BPC65[dados_pnadc$V5001A==1 & dados_pnadc$V2009 >=65 ] = 1                
prop.table(table(dados_pnadc$BPC65,exclude = NULL))
prop.table(table(dados_pnadc$V5001A,dados_pnadc$V2009 >=65,exclude = NULL))
## RECEBE OU NÃO BPC##
dados_pnadc$BPC = NA
dados_pnadc$BPC[dados_pnadc$V5001A==1]=1
dados_pnadc$BPC[is.na(dados_pnadc$BPC)] = 0
table(dados_pnadc$BPC,dados_pnadc$V5001A, exclude = NULL) # está correta a variável!
###################### agrupando os domicilíos e botando o número de pessoas com BPC por domicílio #######################

NUM_BPC_dom = dados_pnadc %>%
        group_by(UPA, V1008, V1014) %>%
        summarise(NUMTotal_BPC = sum(V5001A==1))
describe(NUM_BPC_dom)
summary(NUM_BPC_dom, exclude =NULL)
# colocando a variável na base 
dados_pnadc = full_join(dados_pnadc, NUM_BPC_dom, by=c("UPA", "V1008", "V1014"))
## proporção do número de pessoas com BPC por domicílio
#estatísticas básicas$$
table(dados_pnadc$NUMTotal_BPC,exclude = NULL)
prop.table(table(dados_pnadc$NUMTotal_BPC,exclude = NULL))
describe(dados_pnadc$NUMTotal_BPC)
summary(dados_pnadc$NUMTotal_BPC)
table(dados_pnadc$V5001A,dados_pnadc$NUMTotal_BPC,exclude = NULL)
table(dados_pnadc$BPC,dados_pnadc$NUMTotal_BPC,exclude = NULL)
prop.table(table(dados_pnadc$BPC,dados_pnadc$NUMTotal_BPC,exclude = NULL)) 

# agrupando os domicílios e pessoas para calcular a renda EXC#
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

############### RENDA EXC ###############333

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
#primeiro para idade
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

table(dados_pnadc$NUMTotal_BPC,dados_pnadc$Tipo2)
table(dados_pnadc$BPC,dados_pnadc$V2009>=65)
table(dados_pnadc$NUMTotal_BPC,dados_pnadc$Faixaposicao,dados_pnadc$V2009>=65)

#### RODANDO O RDD #### 
# install.packages('rdd')
library('rdd')
library('rddensity')

RDestimate(BPC ~ V2009,dados_pnadc,cutpoint = 65)



#RDestimate(dados_pnadc$BPC ~ )














