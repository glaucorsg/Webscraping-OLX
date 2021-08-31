# Limpando a memória e solicitando as bibliotecas necessárias

rm(list=ls())

library(dplyr)
library(rvest)
library(funModeling)
library(lubridate)
library(rstatix)
library(PMCMR)
library(mice)
library(VIM)
library(stringr)
library(readr)
library(magrittr)

# Scraping ES

result_ES = data.frame()
url_base_ES = "https://es.olx.com.br/instrumentos-musicais/guitarras?o="
url_num_ES = read_html(url_base_ES)

num_pag_ES = url_num_ES %>%
  html_nodes(".fhJlIo") %>%
  html_text()

num_pag_ES = ceiling((as.numeric(substr(num_pag_ES,11,13))/50))

for (j in 1:(num_pag_ES)){
  print(paste("Página: ", j, "em ES"))
  
  olx = paste(url_base_ES, j, sep='')
  olx = read_html(olx)
  
  titulo = olx %>%
    html_nodes(".fnmrjs-2 .fnmrjs-6 h2")%>%
    html_text()
  
  preco = olx %>%
    html_nodes(".aoie8y-0 span:first-child")%>%
    html_text()
  
  data = olx %>%
    html_nodes(".wlwg1t-0 span:first-child")%>%
    html_text()
  
  uf = olx %>%
    html_nodes(".yXlon")%>%
    html_text()
  
  result_ES = rbind(result_ES, data.frame(titulo,preco,data,uf))
}

# Scraping MG

result_MG = data.frame()
url_base_MG = "https://mg.olx.com.br/instrumentos-musicais/guitarras?o="
url_num_MG = read_html(url_base_MG)
num_pag_MG = url_num_MG %>%
  html_nodes(".fhJlIo") %>%
  html_text()

num_pag_MG = ceiling((as.numeric(substr(num_pag_MG,11,15))/50)*1000)

for (j in 1:(num_pag_MG)){
  print(paste("Página: ", j, "em MG"))
  
  olx = paste(url_base_MG, j, sep='')
  olx = read_html(olx)
  
  titulo = olx %>%
    html_nodes(".fnmrjs-2 .fnmrjs-6 h2")%>%
    html_text()
  
  preco = olx %>%
    html_nodes(".aoie8y-0 span:first-child")%>%
    html_text()
  
  data = olx %>%
    html_nodes(".wlwg1t-0 span:first-child")%>%
    html_text()
  
  uf = olx %>%
    html_nodes(".yXlon")%>%
    html_text()
  
  result_MG = rbind(result_MG, data.frame(titulo,preco,data,uf))
}


# Scraping RJ

result_RJ = data.frame()
url_base_RJ = "https://rj.olx.com.br/instrumentos-musicais/guitarras?o="
url_num_RJ = read_html(url_base_RJ)
num_pag_RJ = url_num_RJ %>%
  html_nodes(".fhJlIo") %>%
  html_text()

num_pag_RJ = ceiling((as.numeric(substr(num_pag_RJ,11,15))/50)*1000)

for (j in 1:(num_pag_RJ)){
  print(paste("Página: ", j, "em RJ"))
  
  olx = paste(url_base_RJ, j, sep='')
  olx = read_html(olx)
  
  titulo = olx %>%
    html_nodes(".fnmrjs-2 .fnmrjs-6 h2")%>%
    html_text()
  
  preco = olx %>%
    html_nodes(".aoie8y-0 span:first-child")%>%
    html_text()
  
  data = olx %>%
    html_nodes(".wlwg1t-0 span:first-child")%>%
    html_text()
  
  uf = olx %>%
    html_nodes(".yXlon")%>%
    html_text()
  
  result_RJ = rbind(result_RJ, data.frame(titulo,preco,data,uf))
}


# Scraping SP

result_SP = data.frame()
url_base_SP = "https://sp.olx.com.br/instrumentos-musicais/guitarras?o="
url_num_SP = read_html(url_base_SP)

# SP atinge o limite máximo de páginas previsto pelo Olx (100 pg.)

for (j in 1:100){
  print(paste("Página: ", j, "em SP"))
  
  olx = paste(url_base_SP, j, sep='')
  olx = read_html(olx)
  
  titulo = olx %>%
    html_nodes(".fnmrjs-2 .fnmrjs-6 h2")%>%
    html_text()
  
  preco = olx %>%
    html_nodes(".aoie8y-0 span:first-child")%>%
    html_text()
  
  data = olx %>%
    html_nodes(".wlwg1t-0 span:first-child")%>%
    html_text()
  
  uf = olx %>%
    html_nodes(".yXlon")%>%
    html_text()
  
  result_SP = rbind(result_SP, data.frame(titulo,preco,data,uf))
}


# Criando data frame com todos os resultados parciais

result = rbind(result_ES,result_MG,result_RJ,result_SP)


# Ajustando o formato da data (de character para numeric) com a 
# criação de data_num e o t_anunc

for (i in 1:length(result$data)){
  if (result$data[i] == "Hoje"){
    result$data_num[i] = today()
  }
  else if (result$data[i] == "Ontem"){
    result$data_num[i] = (today()-1)
  }
  else if (result$data[i] != "Hoje" | result$data[i] != "Ontem"){
    result$data_num[i] = as.Date.character (x = result$data[i], format = '%d %b', tz = 'GMT' , tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d-%b-%Y"))
  }
}

for (i in 1:length(result$data)){
  result$t_anunc[i] = (as.numeric(today()+1)) - result$data_num[i]
}


# Ajustando o formato do preço (de character para numeric)
# com a criação de preco_num'

for (i in 1:length(result$data)){
  result$preco_num[i] = result$preco[i]
  
  for (i in 1:length(result$data)){
    if (result$preco_num[i] == "R$ 0" | result$preco_num[i] == "R$ 1"){
      result$preco_num[i] = ""
    }
  }
}

for (i in 1:length(result$preco)){
  result$preco_num[i] %<>% str_remove_all("[R$ ]")
  result$preco_num[i] %<>% str_remove_all("[.]")
  
}

result$preco_num = as.numeric(result$preco_num, sep="")


# Transformando UF para variável categórica

for (i in 1:length(result$titulo)){
  if ("ES" %in% result$uf[i]){
    (result$uf[i] = "ES")
  } else if ("MG" %in% result$uf[i]){
    (result$uf[i] = "MG")
  } else if ("RJ" %in% result$uf[i]){
    (result$uf[i] = "RJ")
  } else if ("SP" %in% result$uf[i]){
    (result$uf[i] = "SP")
  }
}

result$uf = as.factor(result$uf)


# Análise exploratória do data frame e tratamento de dados ausentes

dim(result)
View(result)


summary(result)

bkp = result

result = bkp

colSums(is.na(result))

md.pattern(result)
md.pairs(result)
mice_plot <- aggr(result, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(result), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern")) 

mean(result$preco_num, na.rm = TRUE)
mean(result$t_anunc, na.rm = TRUE)

ausentes = result[which(is.na(result$preco_num)),]

result$preco_num = impute(result$preco_num, mean)
mean(result$preco_num)
colSums(is.na(result))

boxplot(result[,6], main = "Tempo de anúncio", ylab = "Dias")
boxplot(result[,7], main = "Preço do produto", ylab = "R$")



# Identificando e tratando outliers

length(boxplot(result[,6:7], plot=FALSE)$out)
outliers <- boxplot(result[,6:7], plot=FALSE)$out

IQR(result$t_anunc, na.rm = TRUE) #identifica interquartil
IQR(result$preco_num, na.rm = TRUE)

temp_tempo = summary(result$t_anunc)
li_tempo = temp_tempo[2] - 1.5*IQR(result$t_anunc, na.rm = TRUE) #Calcula limite inferior
ls_tempo = temp_tempo[5] + 1.5*IQR(result$t_anunc, na.rm = TRUE)  #Calcula limite superior

temp_preco = summary(result$preco_num)
li_preco = temp_preco[2] - 1.5*IQR(result$preco_num, na.rm = TRUE) #Calcula limite inferior
ls_preco = temp_preco[5] + 1.5*IQR(result$preco_num, na.rm = TRUE)  #Calcula limite superior

boxplot(result[,6], main = "Tempo de anúncio", ylab = "Dias")
boxplot(result[,7], main = "Preço do produto", ylab = "R$")

result = result[-which(result$preco_num > ls_preco),]
result = result[-which(result$t_anunc > ls_tempo),]


boxplot(result[,6], main = "Tempo de anúncio", ylab = "Dias")
boxplot(result[,7], main = "Preço do produto", ylab = "R$")

par(mfrow = c(1,2))
boxplot(t_anunc~uf, data=result, main = 'Tempo de anúncio por estado', xlab = 'UF',ylab='Tempo de anúncio')
boxplot(preco_num~uf, data=result, main = 'Preço por estado', xlab = 'UF',ylab='Preço (R$)')

par(mfrow = c(1,4))
hist(result$t_anunc[result$uf == "ES"],
     ylab = "Frequência", xlab = "Tempo de anúncio (dias)", main="Tempo de anúncio ES")
hist(result$t_anunc[result$uf == "MG"],
     ylab = "Frequência", xlab = "Tempo de anúncio (dias)", main="Tempo de anúncio MG")
hist(result$t_anunc[result$uf == "RJ"],
     ylab = "Frequência", xlab = "Tempo de anúncio (dias)", main="Tempo de anúncio RJ")
hist(result$t_anunc[result$uf == "SP"],
     ylab = "Frequência", xlab = "Tempo de anúncio (dias)", main="Tempo de anúncio SP")


par(mfrow = c(1,4))
hist(result$preco_num[result$uf == "ES"],
     ylab = "Frequência", xlab = "Preço (R$)", main="Preço ES")
hist(result$preco_num[result$uf == "MG"],
     ylab = "Frequência", xlab = "Preço (R$)", main="Preço MG")
hist(result$preco_num[result$uf == "RJ"],
     ylab = "Frequência", xlab = "Preço (R$)", main="Preço RJ")
hist(result$preco_num[result$uf == "SP"],
     ylab = "Frequência", xlab = "Preço (R$)", main="Preço SP")


# Análise da normalidade das variáveis t_anunc e preco_num
# via histograma e via teste de Shapiro-Wilk

plot_num(result[,6:7])

shapiro.test(result$t_anunc[0:5000])
shapiro.test(result$preco_num[0:5000])

# Teste de Kruskal-Wallis

kruskal.test(t_anunc ~ uf, data = result)
kruskal.test(preco_num ~ uf, data = result)



# Realizando testes post-hoc de Nemenyi e de Dunn (com ajuste de p-valor)
posthoc.kruskal.nemenyi.test(t_anunc ~ uf, data = result)
posthoc.kruskal.nemenyi.test(preco_num ~ uf, data = result)

dunn_test(t_anunc ~ uf, data = result, p.adjust.method = "bonferroni")
dunn_test(preco_num ~ uf, data = result, p.adjust.method = "bonferroni")

# Normalizando dados e implementando o modelo de regressão

dataNorm = result[,6:7]
dataNorm$t_anunc = scale(dataNorm$t_anunc, center = TRUE, scale = TRUE)
dataNorm$preco_num = scale(dataNorm$preco_num, center = TRUE, scale = TRUE)

modelo = lm(t_anunc ~ (preco_num), data = result, qr = TRUE)
summary(modelo)
