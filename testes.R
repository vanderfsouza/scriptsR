#### Distribuição Normal ####

N = 210000000 # população brasileira
Media = 173.1 # média da altura da população brasileira
SD = 7.3 # desvio padão da altura da população brasileira

dados = rnorm(N,Media,SD)
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
(sd = sd(amostra))

(t_tabelado_inf=qt(0.025,(n-1)))
(t_tabelado_sup=qt(0.975,(n-1)))

(vcritico_inf=media + (t_tabelado_inf*sd/sqrt(n)))
(vcritico_sup=media + (t_tabelado_sup*sd/sqrt(n)))

plot(density(amostra))
abline(v=mean(amostra),lty=1,col="blue") 
abline(v=median(amostra),lty=3,col="green") 
abline(v=vcritico_inf,lty=2,col="red") 
abline(v=vcritico_sup,lty=2,col="red") 

#
amostragem = function(dados,n){
  result = list()
  for (i in 1:100){
    result[i]=mean(sample(dados, n, replace = FALSE))
  }
  print(unlist(result))
}
teste = amostragem(dados,n)
plot(density(teste))
mean(teste)
sum(teste<vcritico_inf)
sum(teste>vcritico_sup)

#
t.test(amostra, mu = Media) # média da população
t.test(amostra, mu = vcritico_inf)
t.test(amostra, mu = vcritico_sup) 

#### Regressão Linear ####

X_data <- seq(1, 100, 1)

Y_raw <- 3.5 + 2.1 * X_data

Y_noise <- rnorm(n = 100, mean = 0, sd = 5)

Y <- data.frame(X = X_data, Y = Y_raw + Y_noise)

Model  <- lm(Y ~ X, data = Y)

summary(Model)

plot(Y$X,Y$Y)
