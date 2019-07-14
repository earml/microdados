# http://leg.ufpr.br/~elias/ensino/ce223/aula18.R
# https://drive.google.com/drive/folders/0B_OX1FlzVrSxZ1JzV3lOWUcxRlU
# http://rcoster.blogspot.com.br/2014/02/lendo-grandes-bancos-de-dados.html
# https://sites.google.com/site/marcosfs2006/microdados
# https://sites.google.com/site/marcosfs2006/textos
# https://sites.google.com/site/marcosfs2006/r_taac_blog/leiturademicrodadoscomor
# https://pt.stackoverflow.com/questions/154724/como-ler-os-microdados-do-enem-no-r/154743
# https://pt.stackoverflow.com/questions/154724/como-ler-os-microdados-do-enem-no-r/154743
# https://analisereal.com/2013/12/19/analisando-microdados-do-ibge-com-o-r/
# http://metodologiapolitica.com/microdados-no-r/#sthash.gJbXru6o.pOC1CXXk.dpbs
# http://metodologiapolitica.com/microdados-no-r/#sthash.tP706YrZ.dpbs

# http://www.rmining.net/index.html
# http://www.rmining.net/2016/03/01/regras-de-associacao-vendas-cruzadas-e-recomendacao/index.html


# Pretende-se apresentar sete formas distintas de extrair informações de dados disponibilizados em 
# arquivos de texto. Sendo oportuno salientar que cada função do R utilizada tem uma finalidade 
# específica, e que conhecer diversas formas de importar dados é importante para resolver problemas 
# distintos. Permite ainda resolver o mesmo problema de diversas formas, adequando há sua capacidade 
# computacional.

# Análise baseada no blog "Metodologia em Ciência Política" e no Canal do youtube "metodologiabrasil" do 
# Roney Fraga Souza
# Tópico da aula : Trabalhar com Microdados usando o R - Parte 1
# Passo a passo
# 1- Acesse o site : http://www.ibge.gov.br/home/
# 2- Clique na aba "População"
# 3- Procure na opção "População" por "PNAD" e clique em "Pesquisa Nacional por Amostra de Domicílios"
# 4- Clique do lado esquerdo em "Microdados" ou se quiser dados de anos anteriores clique na opção
# "Pesquisas Anteriores" o ano procurado.
# 5- Baixe todos os dados > (Dados , Dicionarios , Leia me PNAD 2011 ,
# Leitura em R , Marca de Imputação , Metodologia , Questionario)


### Importando os dados  ###
## OBS: Antes de iniciar a importação dos dados, sugiro que estude o dicionário e escolha quais variáveis
## você pretende analisar. Isso reduz o tempo de processamento e ocupa pouco menos espaço de memária.

## 1 - limpo a memória do R 
rm(list=ls(all=TRUE))

# 2 - Indique o diretório de trabalho 
setwd('C://seplag//Dados//')

# 3 - Mostra até 15 casas decimais 
options("scipen" = 15) 

# 4 - Listar todos os arquivos do diretório de trabalho 
dir() 

# 5 - Informações a serem analisadas da PNAD 2011
# ---------------------------------------------- 
# Após ler o dicionário em .xls encontrei o nome da variável, onde ela inicia e o tamanho. Isso é fundamental
# para conhecer a posição de cada variável
#
# Nome    Inicio	    Tamanho	    Descrição 
# V0101   1	          4           Ano de referencia 
# UF      5	          2	          Unidade da Federação 
# V0302	  18	        1	          Sexo 
# V8005	  27	        3	          Idade em anos 
# V0404	  33	        1	          Cor ou raça 
# V4011	  43	        1	          Estado civil 
# V06111	86	        1	          Internet 
# V06112	87	        1	          Internet móvel 
# V9906	  153	        4	          Código da ocupação 
# V9008	  162	        2	          Posição na ocupação 
# V9532	  321	        12	        Rendimento mensal 
# V4729	  751	        5	          Peso pessoa 
#
# Para facilitar a importação, eu defino um intervalo "um pulo", que exclui todos os espaço vagos entre as
# variáveis que eu tenho interesse, sobrando apenas as variáveis que eu quero. Isso é mais prático, pois 
# ao invés de retirar as que eu quero, eu excluo as que eu não quero
# Nome	    Inicio	    Tamanho	
# V0101     1	          4 
# UF        5	          2	
# controle	            11 
# V0302	    18	        1 
# skip2	                8	** A distância, conforme o dicionário, da variável V0302 para a V8005, é de 8 colunas, pois os valores (ou colunas) que estiverem neste intervalo serão lidas como apenas uma coluna (isso reduz tempo de importação), depois é só excluir estas variável (a variável V0302 ocupa a posição 18 e a V8005 ocupa a posição 27. Observe que os extremos são exclusos - não fazem parte do intervalo)
# V8005	    27	        3 
# skip3	                3	** A distância, conforme o dicionário, da variável V0302 para a V0404, é de 3 colunas, pois os valores (ou colunas) que estiverem neste intervalo serão lidas como apenas uma coluna (isso reduz tempo de importação), depois é só excluir estas variável (a variável V8005 ocupa a posição 27 e a V0404 ocupa a posição 33). OBS: Não existe a posição 28, 29, logo a distância entre 27 e 33 é de 3 (posição: 30,31 e 32)
# V0404	    33	        1	
# skip4	                9 
# V4011	    43	        1 
# skip5	                42	        
# V06111	  86	        1	
# V06112	  87	        1 
# skip6	                65 
# V9906	    153	        4 
# skip7	                5	
# V9008	    162	        2 
# skip8	                157	        
# V9532	    321	        12	
# skip9	                418 
# V4729	    751	        5 
# skip10	              1 
# ---------------------------------------------- 
## Usei a função "read.fwf" para ler o arquivo txt##
## 5- Como tenho interesse na base "PES2011" pessoas 2011 faço, e excluo as colunas não desejadas com o parâmetro "widths"
pes2011 <- read.fwf(file='PES2011.txt', widths=c(4,2),buffersize=100)	
#pes2011 <- read.fwf(file='PES2011.txt', widths=c(4,2,11,1,8,3,3,1,9,1,42,1,1,65,4,5,2,157,12,418,5,1),buffersize=1000)
# 6- dimensão da base de dados (tabela) 
dim(pes2011) 

# 7- nomes das variáveis 
names(pes2011) 

# 8- Apresenta as primeiras 20 linhas 
head(pes2011,20) 

# 9- Renomeia as variáveis
#names(pes2011) <- c("V0101","UF","controle","V0302","skip2","V8005","skip3","V0404","skip4","V4011","skip5","V06111","V06112","skip6","V9906","skip7","V9008","skip8","V9532","skip9","V4729","skip10") 
names(pes2011) <- c("V0101","UF","controle","V0302","skip2","V8005","skip3","V0404","skip4")

# 10- Comando para selecionar apenas algumas variáveis da base de dados 
pes2011b <- subset(pes2011, select=c("V0101","UF","controle","V0302","V8005","V0404","V4011","V06111","V06112","V9906","V9008","V9532","V4729")) 

# 11- Apresenta as primeiras 20 linhas 
head(pes2011b,20) 

# 12- Estrutura das variáveis 
str(pes2011b) 

# 13- selecionar apenas os arquitetos 
pes2011c <- subset(pes2011b, V9906==2141) 


# 14- dimensão da tabela 
dim(pes2011c) 

# 15- listar os itens da memória do R 
ls() 

# 15- limpar a memória do R as bases de dados "pes2011" e"pes2011c"
rm(pes2011,pes2011b) 

# 16- estatísticas descritivas básicas 
summary(pes2011c) 

# 17- eliminar o rendimento V9532=999999999999 sem declaração 
pes2011d <- subset(pes2011c, V9532!=999999999999) 

# 18- dimensão da tabela 
dim(pes2011d) 

# 19- estatísticas descritivas básicas 
summary(pes2011d$V9532) 

# tabelas 
table(pes2011d[["V9532"]],pes2011d[["V0302"]]) 
table(pes2011d[["V0404"]],pes2011d[["V0302"]]) 
table(pes2011d[["V8005"]],pes2011d[["V0404"]]) 


### A forma anterior de importar os dados é muito demorada, sugiro que utilizar as próximas, que são
### bem mais rápidas

##########################
##    *** Forma 2  ***  ##
## É muito lenta também ##
##########################

## As funções do pacote SAScii, elaborado por Anthony Damico,  uma forma rápida e fácil para importar 
## dados armazenados em arquivos de texto que tenham dicionário em formato do software SAS. A maioria 
## dos dados disponibilizados pelo IBGE e pelo Inep podem ser importados utilizando as funções desse 
## pacote.

## Importando dados com a função *** read.SAScii ***  ##

# 1- instalar o pacote 'SAScii' elaborado por Anthony Joseph Damico 

install.packages('SAScii') 

# 2- carregar o pacote 
library(SAScii) 

# 3- ler dicionário da PNAD 2011 pessoas 
parse.SAScii( "input PES2011.sas") 

# 4- exemplo de como abir os dados 
x <- read.SAScii( "PES2011.txt" , "input PES2011.sas" , beginline=1) 

head( x ) 

#######################
##  *** Forma 3  *** ##
#######################

# segundo exemplo de como abrir os dados 
# 1- indicar o caminho do dicionário em formato .txt (o ideal é você retirar os @ antes de importar. Sugiro que copie o arquivo para o excel e faça isso, ou use algum editor)
PESdicionario <- "C://seplag//Dados//input PES2011.txt" 

# 2- indicar caminho dos dados em formato .txt
PESdados <- "C://seplag//Dados//PES2011.txt" 

# 3- lendo o arquivo txt. OBS: O parâmetro "zipped" deve ser colocado como True quando você estiver importando direto do site ou estiver em seu pc zipado.
pes2011 <- read.SAScii(PESdados, PESdicionario, zipped=F, beginline=1) 



#######################
##  *** Forma 4  *** ##
#######################

# 1- lendo PNAD 2011 domicílios 
# verificar dicionário. A função "beginline" quer dizer que a leitura inicia-se na linha 11
#parse.SAScii('2011_DOM_input.TXT', beginline=11) 
parse.SAScii('input DOM2011.TXT', beginline=11) 

# 2- Elimine o comentários do arquivo dicionário do SAS, antes de importar, para não ter erro (Só para a variável "PES2011" que é necessário)
#parse.SAScii('2011_DOM_input2.TXT', beginline=11) 
#parse.SAScii('input2 PES2011.txt', beginline=1) 

# 3- ler o arquivo txt. O comando "buffersize" serve para determinar a quantidade de linha que deve ser importada por vez
#dom2011 <- read.SAScii('2011_DOM.TXT', '2011_DOM_input2.TXT', beginline=11, buffersize=1000) 
dom2011 <- read.SAScii('DOM2011.TXT', 'input DOM2011.TXT', beginline=1, buffersize=80000) 

# estrutura dos dados 
str(dom2011) 

# mostrar as primeiras 20 linhas 
dom2011[1:25,]

##     AULA 4   ##
# Para aqrquivos grandes #

#read.SAScii.sqlite()
#Convertendo arquivos txt em csv usando as funções criadas por Marcos F Silva 
# carregar o dicionário disponibilizado pelo IBGE 
load('dicPNAD2011.RData') 

# listar itens da memória 
ls() 

# analisar as primeiras 15 linhas dos dicionários 
head(dicdom2011, 15) 
head(dicpes2011, 15) 

# carregar as funções fwf2csv() e txt2df() desenvolvidas por Marcos 
source("fwf2csv.R") 
source("txt2df.R") 

# ler os dados da PNAD 2011 domicílios 
fwf2csv(input="DOM2011.TXT", output="2011_DOM.csv", dic=dicdom2011, colunas=dicdom2011$cod) 

# transformando um arquivo .txt para .csv 
# ler os dados da PNAD 211 pessoas 
fwf2csv(input="PES2011.txt", output="2011_PES.csv", dic=dicpes2011, colunas=dicpes2011$cod) 

# OBS: A função fwf2csv() do pacote 'descr' desenvolvido por Jakson Aquino é mais rápida pois foi escrita em C ou C++
#library(descr)

# ---------------------------------------------- 
# Acessar dados do arquivo csv via sqldf 
# instalar o pacote sqldf() 
#install.packages(sqldf) 

# carregar o pacote 
library(sqldf) 
library(tcltk)
# listar itens no diretório de trabalho 
dir() 

#acessar dados selecionados em um arquivo csv 
a <- read.csv2.sql('2011_PES.csv', sql=" select V0101, UF, V0302, V8005, V0404, V4011, V06111, V06112, V9906, V9008, V9532, V4729 from file where V9906=2141 ", header=TRUE, sep=";") 
#ls() 
dim(a) 
names(a) 
head(a,15) 

# ---------------------------------------------- 
#{{código do R - Fim}}

##---------------------------------------------------------------------##
#Os comandos apresentados até o momento facilitam a extração dos dados considerando que os arquivos a 
#serem abertos são grandes. Contudo, quando necessitamos tirar a média de um vetor muito grande, que 
#deve ser carregado na memória do R, os comandos que utilizamos até então não ajudam. Ou seja, 
#aprendemos a tirar informações "pequenas" de arquivos grandes. Assim, os dois próximos comandos que 
#serão apresentados tem como diferencial permitir realizar cálculos sem que os dados sejam carregados 
#na memória do R
##---------------------------------------------------------------------##


# ---------------------------------------------- 
# Primeiros passos com o SQLite 
# carregar pacote RSQLite 
library('RSQLite') 

# criar uma base de dados 
sqldf("attach pnad as new") 

# estabelecer uma conexão. Isso faz com que os dados não fiquem na memória do R
drv<-dbDriver("SQLite") 
con<-dbConnect(drv, dbname='pnad') 

# transformar um csv em uma base SQLite 
# PNAD 2011 domicílios 
a <- read.csv2.sql(file="2011_PES.csv", sql="create table pes2011 as select * from file", header=TRUE, row.names=FALSE, sep=';' , dbname="pnad") 
dbGetQuery(con, "select * from pes2011 limit 20") 

# demais funções para serem estudadas para obter bom desempenho com base de dados SQL 
# dbListTables() 
# dbRemoveTable() 
# dbWriteTable() 
# dbGetQuery() 
# ----------------------------------------------


# http://www.rmining.net/2014/09/24/leitura-da-pnad-2013-com-o-r/index.html


## 1 - limpo a memória do R 
rm(list=ls(all=TRUE))

# 2 - Indique o diretório de trabalho 
setwd('C://Users//earibeiro//Desktop//microdados')


# 3 - Mostra até 15 casas decimais 
options("scipen" = 15) 

# 4 - Listar todos os arquivos do diretório de trabalho 
dir() 

## ----------------------------------------------------- ##
library(bit64)
library(data.table)
library(descr)
library(reshape)
library(survey)
library(rJava)
library(xlsx)

## Criando o dicionário a partir das três primeiras colunas da planilha
load('dicPNAD2014.RData')
#dicdom <- read.csv(file = 'dicdom.csv', header=F)
dicdom<-dicdom2014
dicdom <- dicdom[complete.cases(dicdom),]
colnames(dicdom) <- c('inicio','cod', 'tamanho', 'variavel')

#dicpes <- read.csv(file = 'dicpes.csv', header=F)
dicpes<-dicpes2014
dicpes <- dicpes[complete.cases(dicpes),]
colnames(dicpes) <- c('inicio', 'cod','tamanho', 'variavel')

#write.table(dicpes, "K://Elisalvo//Microdados//PNAD-2014//Dados//variavel.txt", sep="\t")

## Parâmetro com o final de cada campo
end_dom = dicdom$inicio + dicdom$tamanho - 1
end_pes = dicpes$inicio + dicpes$tamanho - 1

## Converte o microdado para um arquivo csv
fwf2csv(fwffile='DOM2014.TXT', csvfile='dadosdom.csv', names=dicdom$variavel, begin=dicdom$inicio, end=end_dom)
fwf2csv(fwffile='PES2014.txt', csvfile='dadospes.csv', names=dicpes$variavel, begin=dicpes$inicio, end=end_pes)

## Efetua a leitura do conjunto de dados com o fread do data.table
dadosdom <- fread(input='dadosdom.csv', sep='auto', sep2='auto', integer64='double')
#dadospes <- fread(input='dadospes.csv', sep='auto', sep2='auto', integer64='double')

dadospes <- fread(input='dadospes.csv', sep='\t', sep2='\t', integer64='double')

#####==================================================================================#####
#####                                                                                  #####
#####                           Para Educação                                          #####
#####                                                                                  #####
#####==================================================================================#####
#####                      Dados Demográficos e Sociais                                ##### 
#####                                                                                  ##### 
#####     3.1.2.2.2 - Taxa de analfabetismo da população de 10 anos ou mais            ##### 

###### Região Norte    ###############
TAP <- subset(dadospes, select=c("V2","V10","V46")) 
TAP11 <- subset(TAP, V2==11 & V10>=10)
TAP12 <- subset(TAP, V2==12 & V10>=10)
TAP13 <- subset(TAP, V2==13 & V10>=10)
TAP14 <- subset(TAP, V2==14 & V10>=10)
TAP15 <- subset(TAP, V2==15 & V10>=10)
TAP16 <- subset(TAP, V2==16 & V10>=10)
TAP17 <- subset(TAP, V2==17 & V10>=10)
RegiNorte<-rbind(TAP11,TAP12,TAP13,TAP14,TAP15,TAP16,TAP17)

RO<-round(table(TAP11$V46)[2]/sum(table(TAP11$V46))*100,2)
AC<-round(table(TAP12$V46)[2]/sum(table(TAP12$V46))*100,2)
AM<-round(table(TAP13$V46)[2]/sum(table(TAP13$V46))*100,2)
RR<-round(table(TAP14$V46)[2]/sum(table(TAP14$V46))*100,2)
PA<-round(table(TAP15$V46)[2]/sum(table(TAP15$V46))*100,2)
AP<-round(table(TAP16$V46)[2]/sum(table(TAP16$V46))*100,2)
TO<-round(table(TAP17$V46)[2]/sum(table(TAP17$V46))*100,2)
Norte<-round(table(RegiNorte$V46)[2]/sum(table(RegiNorte$V46))*100,2)


###### Região Nordeste    ###############
TAP21 <- subset(TAP, V2==21 & V10>=10)
TAP22 <- subset(TAP, V2==22 & V10>=10)
TAP23 <- subset(TAP, V2==23 & V10>=10)
TAP24 <- subset(TAP, V2==24 & V10>=10)
TAP25 <- subset(TAP, V2==25 & V10>=10)
TAP26 <- subset(TAP, V2==26 & V10>=10)
TAP27 <- subset(TAP, V2==27 & V10>=10)
TAP28 <- subset(TAP, V2==28 & V10>=10)
TAP29 <- subset(TAP, V2==29 & V10>=10)
RegiNordeste<-rbind(TAP21,TAP22,TAP23,TAP24,TAP25,TAP26,TAP27,TAP28,TAP29)

MA<-round(table(TAP21$V46)[2]/sum(table(TAP21$V46))*100,2)
PI<-round(table(TAP22$V46)[2]/sum(table(TAP22$V46))*100,2)
CE<-round(table(TAP23$V46)[2]/sum(table(TAP23$V46))*100,2)
RN<-round(table(TAP24$V46)[2]/sum(table(TAP24$V46))*100,2)
PB<-round(table(TAP25$V46)[2]/sum(table(TAP25$V46))*100,2)
PE<-round(table(TAP26$V46)[2]/sum(table(TAP26$V46))*100,2)
AL<-round(table(TAP27$V46)[2]/sum(table(TAP27$V46))*100,2)
SE<-round(table(TAP28$V46)[2]/sum(table(TAP28$V46))*100,2)
BA<-round(table(TAP29$V46)[2]/sum(table(TAP29$V46))*100,2)
Nordeste<-round(table(RegiNordeste$V46)[2]/sum(table(RegiNordeste$V46))*100,2)


###### Região Sudeste   ###############
TAP31 <- subset(TAP, V2==31 & V10>=10)
TAP32 <- subset(TAP, V2==32 & V10>=10)
TAP33 <- subset(TAP, V2==33 & V10>=10)
TAP35 <- subset(TAP, V2==35 & V10>=10)
RegiSudeste<-rbind(TAP31,TAP32,TAP33,TAP35)

MG<-round(table(TAP31$V46)[2]/sum(table(TAP31$V46))*100,2)
ES<-round(table(TAP32$V46)[2]/sum(table(TAP32$V46))*100,2)
RJ<-round(table(TAP33$V46)[2]/sum(table(TAP33$V46))*100,2)
SP<-round(table(TAP35$V46)[2]/sum(table(TAP35$V46))*100,2)
Sudeste<-round(table(RegiSudeste$V46)[2]/sum(table(RegiSudeste$V46))*100,2)


######        Região SuL         ###############
TAP41 <- subset(TAP, V2==41 & V10>=10)
TAP42 <- subset(TAP, V2==42 & V10>=10)
TAP43 <- subset(TAP, V2==43 & V10>=10)
RegiSul<-rbind(TAP41,TAP42,TAP43)

PR<-round(table(TAP41$V46)[2]/sum(table(TAP41$V46))*100,2)
SC<-round(table(TAP42$V46)[2]/sum(table(TAP42$V46))*100,2)
RS<-round(table(TAP43$V46)[2]/sum(table(TAP43$V46))*100,2)
Sul<-round(table(RegiSul$V46)[2]/sum(table(RegiSul$V46))*100,2)


######        Região Centro-oeste    ###############
TAP50 <- subset(TAP, V2==50 & V10>=10)
TAP51 <- subset(TAP, V2==51 & V10>=10)
TAP52 <- subset(TAP, V2==52 & V10>=10)
TAP53 <- subset(TAP, V2==53 & V10>=10)
RegiCentro<-rbind(TAP50,TAP51,TAP52,TAP53)

MS<-round(table(TAP50$V46)[2]/sum(table(TAP50$V46))*100,2)
MT<-round(table(TAP51$V46)[2]/sum(table(TAP51$V46))*100,2)
GO<-round(table(TAP52$V46)[2]/sum(table(TAP52$V46))*100,2)
DF<-round(table(TAP53$V46)[2]/sum(table(TAP53$V46))*100,2)
CentrOeste<-round(table(RegiCentro$V46)[2]/sum(table(RegiCentro$V46))*100,2)

############ BRASIL   ###################
pais<-rbind(RegiNorte,RegiNordeste,RegiSudeste,RegiSul,RegiCentro)
brasil<-round(table(pais$V46)[2]/sum(table(pais$V46))*100,2)

message("Brasil \n",brasil)

message("Norte \n",Norte,"\n RO \n",RO,"\n AC \n",AC,
        "\n AM \n",AM,"\n RR \n",RR,"\n PA \n",PA,
        "\n AP \n",AP,"\n TO \n",TO);


message("Nordeste \n",Nordeste,"\n MA \n",MA,"\n PI \n",PI,
        "\n CE \n",CE,"\n RN \n",RN,"\n PB \n",PB,
        "\n PE \n",PE,"\n AL \n",AL,"\n SE \n",SE,"\n BA \n",BA);


message("Sudeste \n",Sudeste,"\n MG \n",MG,"\n ES \n",ES,
        "\n RJ \n",RJ,"\n SP \n",SP);

message("Sul \n",Sul,"\n PR \n",PR,"\n SC \n",SC,
        "\n RS \n",RS);


message("Centro-Oeste \n",CentrOeste,"\n MS \n",MS,"\n MT \n",MT,
        "\n GO \n",GO,"\n DF \n",DF);




#####==================================================================================#####
#####                                                                                  #####
#####                           Para Educação                                          #####
#####                                                                                  #####
#####==================================================================================#####
#####                      Dados Demográficos e Sociais                                ##### 
#####                                                                                  ##### 
#####     3.1.2.2.3 - Taxa de analfabetismo da população de 10 anos ou mais de idade   #####
#####     dentre os 20% mais pobre                                                     #####
#####                         ====                  ====                               #####
#####                         ====  Região Norte    ====                               ##### 

TAP2 <- subset(dadospes, select=c("V2","V10","V46","V333")) 
TAP2$V333[TAP2$V333==999999999999]<-NA

TAP11 <- subset(TAP2, V2==11 & V10>=10);quantile(TAP11$V333,na.rm=T,c(.20,.25,.50,.80,.95))
TAP12 <- subset(TAP2, V2==12 & V10>=10);quantile(TAP12$V333,na.rm=T,c(.20,.25,.50,.80,.95))
TAP13 <- subset(TAP2, V2==13 & V10>=10);quantile(TAP13$V333,na.rm=T,c(.20,.25,.50,.80,.95))
TAP14 <- subset(TAP2, V2==14 & V10>=10);quantile(TAP14$V333,na.rm=T,c(.20,.25,.50,.80,.95))
TAP15 <- subset(TAP2, V2==15 & V10>=10);quantile(TAP15$V333,na.rm=T,c(.20,.25,.50,.80,.95))
TAP16 <- subset(TAP2, V2==16 & V10>=10);quantile(TAP16$V333,na.rm=T,c(.20,.25,.50,.80,.95))
TAP17 <- subset(TAP2, V2==17 & V10>=10);quantile(TAP17$V333,na.rm=T,c(.20,.25,.50,.80,.95))
TAP11 <- subset(TAP2, V2==11 & V10>=10 & V333<=323)
TAP12 <- subset(TAP2, V2==12 & V10>=10 & V333<=214)
TAP13 <- subset(TAP2, V2==13 & V10>=10 & V333<=233)
TAP14 <- subset(TAP2, V2==14 & V10>=10 & V333<=300)
TAP15 <- subset(TAP2, V2==15 & V10>=10 & V333<=236)
TAP16 <- subset(TAP2, V2==16 & V10>=10 & V333<=300)
TAP17 <- subset(TAP2, V2==17 & V10>=10 & V333<=268)
RegiNorte<-rbind(TAP11,TAP12,TAP13,TAP14,TAP15,TAP16,TAP17)

RO<-round(table(TAP11$V46)[2]/sum(table(TAP11$V46))*100,2)
AC<-round(table(TAP12$V46)[2]/sum(table(TAP12$V46))*100,2)
AM<-round(table(TAP13$V46)[2]/sum(table(TAP13$V46))*100,2)
RR<-round(table(TAP14$V46)[2]/sum(table(TAP14$V46))*100,2)
PA<-round(table(TAP15$V46)[2]/sum(table(TAP15$V46))*100,2)
AP<-round(table(TAP16$V46)[2]/sum(table(TAP16$V46))*100,2)
TO<-round(table(TAP17$V46)[2]/sum(table(TAP17$V46))*100,2)
Norte<-round(table(RegiNorte$V46)[2]/sum(table(RegiNorte$V46))*100,2)

######     =====        =====      ######
######      Região Nordeste        ######
######     =====        =====      ###### 
TAP21 <- subset(TAP2, V2==21 & V10>=10);quantile(TAP21$V333,na.rm=T,c(.20,.25,.50,.80,.95))
TAP22 <- subset(TAP2, V2==22 & V10>=10);quantile(TAP22$V333,na.rm=T,c(.20,.25,.50,.80,.95))
TAP23 <- subset(TAP2, V2==23 & V10>=10);quantile(TAP23$V333,na.rm=T,c(.20,.25,.50,.80,.95))
TAP24 <- subset(TAP2, V2==24 & V10>=10);quantile(TAP24$V333,na.rm=T,c(.20,.25,.50,.80,.95))
TAP25 <- subset(TAP2, V2==25 & V10>=10);quantile(TAP25$V333,na.rm=T,c(.20,.25,.50,.80,.95))
TAP26 <- subset(TAP2, V2==26 & V10>=10);quantile(TAP26$V333,na.rm=T,c(.20,.25,.50,.80,.95))
TAP27 <- subset(TAP2, V2==27 & V10>=10);quantile(TAP27$V333,na.rm=T,c(.20,.25,.50,.80,.95))
TAP28 <- subset(TAP2, V2==28 & V10>=10);quantile(TAP28$V333,na.rm=T,c(.20,.25,.50,.80,.95))
TAP29 <- subset(TAP2, V2==29 & V10>=10);quantile(TAP29$V333,na.rm=T,c(.20,.25,.50,.80,.95))
TAP21 <- subset(TAP2, V2==21 & V10>=10 & V333<=170)
TAP22 <- subset(TAP2, V2==22 & V10>=10 & V333<=214)
TAP23 <- subset(TAP2, V2==23 & V10>=10 & V333<=236)
TAP24 <- subset(TAP2, V2==24 & V10>=10 & V333<=244)
TAP25 <- subset(TAP2, V2==25 & V10>=10 & V333<=238)
TAP26 <- subset(TAP2, V2==26 & V10>=10 & V333<=243)
TAP27 <- subset(TAP2, V2==27 & V10>=10 & V333<=186)
TAP28 <- subset(TAP2, V2==28 & V10>=10 & V333<=237)
TAP29 <- subset(TAP2, V2==29 & V10>=10 & V333<=245)
RegiNordeste<-rbind(TAP21,TAP22,TAP23,TAP24,TAP25,TAP26,TAP27,TAP28,TAP29)

MA<-round(table(TAP21$V46)[2]/sum(table(TAP21$V46))*100,2)
PI<-round(table(TAP22$V46)[2]/sum(table(TAP22$V46))*100,2)
CE<-round(table(TAP23$V46)[2]/sum(table(TAP23$V46))*100,2)
RN<-round(table(TAP24$V46)[2]/sum(table(TAP24$V46))*100,2)
PB<-round(table(TAP25$V46)[2]/sum(table(TAP25$V46))*100,2)
PE<-round(table(TAP26$V46)[2]/sum(table(TAP26$V46))*100,2)
AL<-round(table(TAP27$V46)[2]/sum(table(TAP27$V46))*100,2)
SE<-round(table(TAP28$V46)[2]/sum(table(TAP28$V46))*100,2)
BA<-round(table(TAP29$V46)[2]/sum(table(TAP29$V46))*100,2)
Nordeste<-round(table(RegiNordeste$V46)[2]/sum(table(RegiNordeste$V46))*100,2)



message("Norte \n",Norte,"\n RO \n",RO,"\n AC \n",AC,
        "\n AM \n",AM,"\n RR \n",RR,"\n PA \n",PA,
        "\n AP \n",AP,"\n TO \n",TO);


message("Nordeste \n",Nordeste,"\n MA \n",MA,"\n PI \n",PI,
        "\n CE \n",CE,"\n RN \n",RN,"\n PB \n",PB,
        "\n PE \n",PE,"\n AL \n",AL,"\n SE \n",SE,"\n BA \n",BA);




#####    Para Renda, Pobreza e Desigualdade  #####
#install.packages("ineq")
library(ineq)
x<-c(1,1,1,2,4,8,13,20)

ineq(x,type="Gini")

#Lorenz curve
plot(Lc(x))
plot(Lc(x),col="darkred",lwd=2)
