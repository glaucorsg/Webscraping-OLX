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

# Scraping PR

result_PR = data.frame()
url_base_PR = "https://pr.olx.com.br/instrumentos-musicais/guitarras?o="
url_num_PR = read_html(url_base_PR)

num_pag_PR = url_num_PR %>%
  html_nodes(".fhJlIo") %>%
  html_text()

num_pag_PR = ceiling((as.numeric(substr(num_pag_PR,11,15))/50)*1000)

for (j in 1:(num_pag_PR)){
  print(paste("Página: ", j, "em PR"))
  
  olx = paste(url_base_PR, j, sep='')
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
  
  result_PR = rbind(result_PR, data.frame(titulo,preco,data,uf))
}

# Scraping RS

result_RS = data.frame()
url_base_RS = "https://rs.olx.com.br/instrumentos-musicais/guitarras?o="
url_num_RS = read_html(url_base_RS)
num_pag_RS = url_num_RS %>%
  html_nodes(".fhJlIo") %>%
  html_text()

num_pag_RS = ceiling((as.numeric(substr(num_pag_RS,11,13))/50))

for (j in 1:(num_pag_RS)){
  print(paste("Página: ", j, "em RS"))
  
  olx = paste(url_base_RS, j, sep='')
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
  
  result_RS = rbind(result_RS, data.frame(titulo,preco,data,uf))
}


# Scraping SC

result_SC = data.frame()
url_base_SC = "https://sc.olx.com.br/instrumentos-musicais/guitarras?o="
url_num_SC = read_html(url_base_SC)
num_pag_SC = url_num_SC %>%
  html_nodes(".fhJlIo") %>%
  html_text()

num_pag_SC = ceiling((as.numeric(substr(num_pag_SC,11,13))/50))

for (j in 1:(num_pag_SC)){
  print(paste("Página: ", j, "em SC"))
  
  olx = paste(url_base_SC, j, sep='')
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
  
  result_SC = rbind(result_SC, data.frame(titulo,preco,data,uf))
}


# Criando data frame com todos os resultados parciais

result = rbind(result_PR,result_RS,result_SC)

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
  if ("PR" %in% result$uf[i]){
    (result$uf[i] = "PR")
  } else if ("RS" %in% result$uf[i]){
    (result$uf[i] = "RS")
  } else if ("SC" %in% result$uf[i]){
    (result$uf[i] = "SC")
  } 
}

result$uf = as.factor(result$uf)


# Análise exploratória do data frame e tratamento de dados ausentes

dim(result)
View(result)


summary(result)

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

result[which(result$t_anunc < ls_tempo),]
result[which(result$preco_num > ls_preco),]

result = result[-which(result$preco_num > ls_preco),]

result = result[-which(result$t_anunc < li_tempo),]
result = result[-which(result$t_anunc > ls_tempo),]

par(mfrow = c(1,2))
boxplot(t_anunc~uf, data=result, main = 'Tempo de anúncio por estado', xlab = 'UF',ylab='Tempo de anúncio')
boxplot(preco_num~uf, data=result, main = 'Preço por estado', xlab = 'UF',ylab='Preço (R$)')

par(mfrow = c(1,3))
hist(result$t_anunc[result$uf == "PR"],
     ylab = "Frequência", xlab = "Tempo de anúncio (dias)", main="Tempo de anúncio PR")
hist(result$t_anunc[result$uf == "RS"],
     ylab = "Frequência", xlab = "Tempo de anúncio (dias)", main="Tempo de anúncio RS")
hist(result$t_anunc[result$uf == "SC"],
     ylab = "Frequência", xlab = "Tempo de anúncio (dias)", main="Tempo de anúncio SC")

par(mfrow = c(1,3))
hist(result$preco_num[result$uf == "PR"],
     ylab = "Frequência", xlab = "Preço (R$)", main="Preço PR")
hist(result$preco_num[result$uf == "RS"],
     ylab = "Frequência", xlab = "Preço (R$)", main="Preço RS")
hist(result$preco_num[result$uf == "SC"],
     ylab = "Frequência", xlab = "Preço (R$)", main="Preço SC")

# Análise da normalidade das variáveis t_anunc e preco_num
# via histograma e via teste de Shapiro-Wilk

plot_num(result[,6:7])

shapiro.test(result$t_anunc)
shapiro.test(result$preco_num)

# Teste de Kruskal-Wallis

kruskal.test(t_anunc ~ uf, data = result)
kruskal.test(preco_num ~ uf, data = result)


# Realizando testes post-hoc de Nemenyi e de Dunn (com ajuste de p-valor) 
# para o tempo de anúncio

posthoc.kruskal.nemenyi.test(t_anunc ~ uf, data = result)

View(dunn_test(t_anunc ~ uf, data = result, p.adjust.method = "bonferroni"))

View(dunn_test(t_anunc ~ uf, data = result, p.adjust.method = "hommel"))


# Realizando testes post-hoc de Nemenyi e de Dunn (com ajuste de p-valor) 
# para o preço dos produtos

posthoc.kruskal.nemenyi.test(preco_num ~ uf, data = result)

View(dunn_test(preco_num ~ uf, data = result, p.adjust.method = "bonferroni"))

View(dunn_test(preco_num ~ uf, data = result, p.adjust.method = "hommel"))


# Normalizando dados e implementando o modelo de regressão

dataNorm = result[,6:7]
dataNorm$t_anunc = scale(dataNorm$t_anunc, center = TRUE, scale = TRUE)
dataNorm$preco_num = scale(dataNorm$preco_num, center = TRUE, scale = TRUE)

modelo = lm(t_anunc ~ (preco_num), data = dataNorm, qr = TRUE)
summary(modelo)
