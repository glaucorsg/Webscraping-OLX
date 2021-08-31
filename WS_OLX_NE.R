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

# Scraping AL

result_AL = data.frame()
url_base_AL = "https://al.olx.com.br/instrumentos-musicais/guitarras?o="
url_num_AL = read_html(url_base_AL)

num_pag_AL = url_num_AL %>%
  html_nodes(".fhJlIo") %>%
  html_text()

num_pag_AL = ceiling((as.numeric(substr(num_pag_AL,11,13))/50))

for (j in 1:(num_pag_AL)){
  print(paste("Página: ", j, "em AL"))
  
  olx = paste(url_base_AL, j, sep='')
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
  
  result_AL = rbind(result_AL, data.frame(titulo,preco,data,uf))
}

# Scraping BA

result_BA = data.frame()
url_base_BA = "https://ba.olx.com.br/instrumentos-musicais/guitarras?o="
url_num_BA = read_html(url_base_BA)
num_pag_BA = url_num_BA %>%
  html_nodes(".fhJlIo") %>%
  html_text()

num_pag_BA = ceiling((as.numeric(substr(num_pag_BA,11,13))/50))

for (j in 1:(num_pag_BA)){
  print(paste("Página: ", j, "em BA"))
  
  olx = paste(url_base_BA, j, sep='')
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
  
  result_BA = rbind(result_BA, data.frame(titulo,preco,data,uf))
}


# Scraping CE

result_CE = data.frame()
url_base_CE = "https://ce.olx.com.br/instrumentos-musicais/guitarras?o="
url_num_CE = read_html(url_base_CE)
num_pag_CE = url_num_CE %>%
  html_nodes(".fhJlIo") %>%
  html_text()

num_pag_CE = ceiling((as.numeric(substr(num_pag_CE,11,13))/50))

for (j in 1:(num_pag_CE)){
  print(paste("Página: ", j, "em CE"))
  
  olx = paste(url_base_CE, j, sep='')
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
  
  result_CE = rbind(result_CE, data.frame(titulo,preco,data,uf))
}


# Scraping MA

result_MA = data.frame()
url_base_MA = "https://ma.olx.com.br/instrumentos-musicais/guitarras?o="
url_num_MA = read_html(url_base_MA)
num_pag_MA = url_num_MA %>%
  html_nodes(".fhJlIo") %>%
  html_text()

num_pag_MA = ceiling((as.numeric(substr(num_pag_MA,11,13))/50))

for (j in 1:(num_pag_MA)){
  print(paste("Página: ", j, "em MA"))
  
  olx = paste(url_base_MA, j, sep='')
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
  
  result_MA = rbind(result_MA, data.frame(titulo,preco,data,uf))
}


# Scraping PB

result_PB = data.frame()
url_base_PB = "https://pb.olx.com.br/instrumentos-musicais/guitarras?o="
url_num_PB = read_html(url_base_PB)
num_pag_PB = url_num_PB %>%
  html_nodes(".fhJlIo") %>%
  html_text()

num_pag_PB = ceiling((as.numeric(substr(num_pag_PB,11,13))/50))

for (j in 1:(num_pag_PB)){
  print(paste("Página: ", j, "em PB"))
  
  olx = paste(url_base_PB, j, sep='')
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
  
  result_PB = rbind(result_PB, data.frame(titulo,preco,data,uf))
}


# Scraping PE

result_PE = data.frame()
url_base_PE = "https://pe.olx.com.br/instrumentos-musicais/guitarras?o="
url_num_PE = read_html(url_base_PE)
num_pag_PE = url_num_PE %>%
  html_nodes(".fhJlIo") %>%
  html_text()

num_pag_PE = ceiling((as.numeric(substr(num_pag_PE,11,13))/50))

for (j in 1:(num_pag_PE)){
  print(paste("Página: ", j, "em PE"))
  
  olx = paste(url_base_PE, j, sep='')
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
  
  result_PE = rbind(result_PE, data.frame(titulo,preco,data,uf))
}


# Scraping PI

result_PI = data.frame()
url_base_PI = "https://pi.olx.com.br/instrumentos-musicais/guitarras?o="
url_num_PI = read_html(url_base_PI)
num_pag_PI = url_num_PI %>%
  html_nodes(".fhJlIo") %>%
  html_text()

num_pag_PI = ceiling((as.numeric(substr(num_pag_PI,11,13))/50))

for (j in 1:(num_pag_PI)){
  print(paste("Página: ", j, "em PI"))
  
  olx = paste(url_base_PI, j, sep='')
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
  
  result_PI = rbind(result_PI, data.frame(titulo,preco,data,uf))
}

# Scraping RN

result_RN = data.frame()
url_base_RN = "https://rn.olx.com.br/instrumentos-musicais/guitarras?o="
url_num_RN = read_html(url_base_RN)
num_pag_RN = url_num_RN %>%
  html_nodes(".fhJlIo") %>%
  html_text()

num_pag_RN = ceiling((as.numeric(substr(num_pag_RN,11,13))/50))

for (j in 1:(num_pag_RN)){
  print(paste("Página: ", j, "em RN"))
  
  olx = paste(url_base_RN, j, sep='')
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
  
  result_RN = rbind(result_RN, data.frame(titulo,preco,data,uf))
}


# Scraping SE

result_SE = data.frame()
url_base_SE = "https://se.olx.com.br/instrumentos-musicais/guitarras?o="
url_num_SE = read_html(url_base_SE)
num_pag_SE = url_num_SE %>%
  html_nodes(".fhJlIo") %>%
  html_text()

num_pag_SE = ceiling((as.numeric(substr(num_pag_SE,11,13))/50))

for (j in 1:(num_pag_SE)){
  print(paste("Página: ", j, "em SE"))
  
  olx = paste(url_base_SE, j, sep='')
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
  
  result_SE = rbind(result_SE, data.frame(titulo,preco,data,uf))
}

# Criando data frame com todos os resultados parciais

result = rbind(result_AL,result_BA,result_CE,result_MA,result_PB,result_PE,result_PI,result_RN,result_SE)

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
  if ("DDD 82 - Alagoas" %in% result$uf[i]){
    (result$uf[i] = "AL")
  } else if ("BA" %in% result$uf[i]){
    (result$uf[i] = "BA")
  } else if ("CE" %in% result$uf[i]){
    (result$uf[i] = "CE")
  } else if ("MA" %in% result$uf[i]){
    (result$uf[i] = "MA")
  } else if ("DDD 83 - Paraíba" %in% result$uf[i]){
    (result$uf[i] = "PB")
  } else if ("PE" %in% result$uf[i]){
    (result$uf[i] = "PE")
  } else if ("PI" %in% result$uf[i]){
    (result$uf[i] = "PI")
  } else if ("DDD 84 - Rio Grande do Norte" %in% result$uf[i]){
    (result$uf[i] = "RN")
  } else if ("DDD 79 - Sergipe" %in% result$uf[i]){
    (result$uf[i] = "SE")
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

boxplot(result[,6:7])
result[which(result$t_anunc < ls_tempo),]
result[which(result$preco_num > ls_preco),]

result = result[-which(result$preco_num > ls_preco),]
result = result[-which(result$t_anunc < li_tempo),]

par(mfrow = c(1,2))
boxplot(t_anunc~uf, data=result, main = 'Tempo de anúncio por estado', xlab = 'UF',ylab='Tempo de anúncio')
boxplot(preco_num~uf, data=result, main = 'Preço por estado', xlab = 'UF',ylab='Preço (R$)')

par(mfrow = c(2,5))
hist(result$t_anunc[result$uf == "AL"],
     ylab = "Frequência", xlab = "Tempo de anúncio (dias)", main="Tempo de anúncio AL")
hist(result$t_anunc[result$uf == "BA"],
     ylab = "Frequência", xlab = "Tempo de anúncio (dias)", main="Tempo de anúncio BA")
hist(result$t_anunc[result$uf == "CE"],
     ylab = "Frequência", xlab = "Tempo de anúncio (dias)", main="Tempo de anúncio CE")
hist(result$t_anunc[result$uf == "MA"],
     ylab = "Frequência", xlab = "Tempo de anúncio (dias)", main="Tempo de anúncio MA")
hist(result$t_anunc[result$uf == "PB"],
     ylab = "Frequência", xlab = "Tempo de anúncio (dias)", main="Tempo de anúncio PB")
hist(result$t_anunc[result$uf == "PE"],
     ylab = "Frequência", xlab = "Tempo de anúncio (dias)", main="Tempo de anúncio PE")
hist(result$t_anunc[result$uf == "PI"],
     ylab = "Frequência", xlab = "Tempo de anúncio (dias)", main="Tempo de anúncio PI")
hist(result$t_anunc[result$uf == "RN"],
     ylab = "Frequência", xlab = "Tempo de anúncio (dias)", main="Tempo de anúncio RN")
hist(result$t_anunc[result$uf == "SE"],
     ylab = "Frequência", xlab = "Tempo de anúncio (dias)", main="Tempo de anúncio SE")

par(mfrow = c(2,5))
hist(result$preco_num[result$uf == "AL"],
     ylab = "Frequência", xlab = "Preço (R$)", main="Preço AL")
hist(result$preco_num[result$uf == "BA"],
     ylab = "Frequência", xlab = "Preço (R$)", main="Preço BA")
hist(result$preco_num[result$uf == "CE"],
     ylab = "Frequência", xlab = "Preço (R$)", main="Preço CE")
hist(result$preco_num[result$uf == "MA"],
     ylab = "Frequência", xlab = "Preço (R$)", main="Preço MA")
hist(result$preco_num[result$uf == "PB"],
     ylab = "Frequência", xlab = "Preço (R$)", main="Preço PB")
hist(result$preco_num[result$uf == "PE"],
     ylab = "Frequência", xlab = "Preço (R$)", main="Preço PE")
hist(result$preco_num[result$uf == "PI"],
     ylab = "Frequência", xlab = "Preço (R$)", main="Preço PI")
hist(result$preco_num[result$uf == "RN"],
     ylab = "Frequência", xlab = "Preço (R$)", main="Preço RN")
hist(result$preco_num[result$uf == "SE"],
     ylab = "Frequência", xlab = "Preço (R$)", main="Preço SE")


# Análise da normalidade das variáveis t_anunc e preco_num
# via histograma e via teste de Shapiro-Wilk

plot_num(result[,6:7])

shapiro.test(result$t_anunc)
shapiro.test(result$preco_num)

# Teste de Kruskal-Wallis

kruskal.test(t_anunc ~ uf, data = result)
kruskal.test(preco_num ~ uf, data = result)


# Realizando testes post-hoc de Nemenyi e de Dunn (com ajuste de p-valor)

posthoc.kruskal.nemenyi.test(preco_num ~ uf, data = result)

View(dunn_test(preco_num ~ uf, data = result, p.adjust.method = "bonferroni"))

View(dunn_test(preco_num ~ uf, data = result, p.adjust.method = "hommel"))


# Normalizando dados e implementando o modelo de regressão

dataNorm = result[,6:7]
dataNorm$t_anunc = scale(dataNorm$t_anunc, center = TRUE, scale = TRUE)
dataNorm$preco_num = scale(dataNorm$preco_num, center = TRUE, scale = TRUE)

modelo = lm(t_anunc ~ (preco_num), data = result, qr = TRUE)
summary(modelo)
