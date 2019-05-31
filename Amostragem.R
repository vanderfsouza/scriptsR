#### Distribuição Normal ####

N = 210000000 # população brasileira
Media = 173.1 # média da altura da população brasileira
DP = 7.3 # desvio padão da altura da população brasileira

dados = rnorm(N,Media,DP)
plot(density(dados))

(z_0.025_sup = qnorm(0.975, mean = 173.1, sd = 7.3))
(z_0.025_inf = qnorm(0.025, mean = 173.1, sd = 7.3))

abline(v=Media,lty=1,col="blue") 
abline(v=z_0.025_sup,lty=2,col="red") 
abline(v=z_0.025_inf,lty=2,col="red") 

#### Amostragem, intervalo de confiança e teste t ####

n = 30

amostra = sample(dados, n, replace = FALSE) 
(media = mean(amostra))
(dp = sd(amostra))

(t_tabelado_inf=qt(0.025,(n-1)))
(t_tabelado_sup=qt(0.975,(n-1)))

(vcritico_inf=media + (t_tabelado_inf*dp/sqrt(n)))
(vcritico_sup=media + (t_tabelado_sup*dp/sqrt(n)))

plot(density(amostra))
abline(v=mean(amostra),lty=1,col="blue") 
abline(v=median(amostra),lty=3,col="green") 
abline(v=vcritico_inf,lty=2,col="red") 
abline(v=vcritico_sup,lty=2,col="red") 

#
amostragem = function(dados,n){
  set.seed(1) # desabilitar para ficar randômico
  result = list()
  for (i in 1:1000){
    result[i]=mean(sample(dados, n, replace = FALSE))
  }
  print(unlist(result))
}

teste = amostragem(dados,n)
plot(density(teste))
mean(teste)

amostragem2 = function(dados,n){
  media = list()
  dp = list()
  IC_inf = list()
  IC_sup = list()

  for (i in 1:1000){
    coleta = sample(dados, n, replace = FALSE)
    media[i] = mean(coleta)
    dp[i] = sd(coleta)
    IC_inf[i] = mean(coleta) + (qt(0.025,(n-1))*sd(coleta)/sqrt(n))
    IC_sup[i] = mean(coleta) + (qt(0.975,(n-1))*sd(coleta)/sqrt(n))
  }
  print(data.frame(unlist(IC_inf),unlist(media),unlist(IC_sup)))
}

teste2 = amostragem2(dados,n)
sum(teste2$unlist.IC_inf.>Media)
sum(teste2$unlist.IC_sup.<Media)

#
t.test(amostra, mu = Media) # média da população
t.test(amostra, mu = vcritico_inf)
t.test(amostra, mu = vcritico_sup) 

###################################
#### Teorema Central do Limite ####

set.seed(1)
numeros = c(sample(1:35, 100, replace = T),sample(55:100, 100, replace = T)) 
numeros = runif(100,0,100)
hist(numeros)
plot(density(numeros))
mean(numeros)

teste3_n3 = amostragem(numeros,3)
hist(teste3_n3)
plot(density(teste3_n3))
mean(teste3_n3)

teste3_n30 = amostragem(numeros,30)
hist(teste3_n30)
plot(density(teste3_n30))
mean(teste3_n30)
