#### Adaline ####
X_data <- seq(1, 100, 1)

Y_raw <- 4.5 + -7.1 * X_data
Y_noise <- rnorm(n = 100, mean = 0, sd = 10)

dados <- data.frame(X = X_data, Y = Y_raw + Y_noise)
Xo = t(matrix(dados$X))
ydo = dados$Y
plot(Xo,ydo)

dados_sample <- dados[sample(nrow(dados), nrow(dados)), ]
X = t(matrix(dados_sample$X))
yd = dados_sample$Y
plot(X,yd)

#### Declaração de parâmetros ####
alfa = 0.0001                              # Taxa de correção dos pesos
maxep = 10000                           # valor máximo de épocas de treinamento
tol = 0.000000001                             # erro máximo tolerável

beta = 0.01 #incluir 

# Pesos (W) e Bias (b) aleatórios 
(W = sample(-100:100, 1, replace=TRUE)/100)  ## Pesos
(b = sample(-100:100, 1, replace=TRUE)/100)  ## bias

#### Função adaline ####
yadaline = function(W,b,X) {
  U=W%*%X+b                             # Calcula a saída do perceptron
  return(U)
}

#### Algoritmo de treinamento do Adaline ####
treina_adaline = function(W,b,X,yd,alfa,beta,maxep,tol){
  N = ncol(X)                          # N = número de amostras de X
  SEQ = tol                            # SEQ(somatorio dos erros quadraticos) = tolerância estabelecida  
  Epoca = 1                            # critério de início 
  erro=0 ;VetorSEQ=c()                 # cria vetores para armazenamento
  Watual = W; Wpassado = W
  batual = b; bpassado = b
  beta = beta
  while (Epoca <= maxep & SEQ >= tol) {
    SEQ =  0                                 # inicia SEQ da Epoca em 0
    for(i in 1:N){                           # loop para cada padrão i de entrada
      y = yadaline(Watual,batual,X[,i])      # aplica o par Xi ao perceptron
      erro = yd[i] - y                       # determina o erro pela diferença entre esperado e obtido
      Wfuturo = Watual + alfa * erro * X[,i] + beta * (Watual - Wpassado)
      bfuturo = batual + alfa * erro + beta * (Watual - Wpassado)
      SEQ = SEQ + erro^2                     # acumula SEQ
      Wpassado = Watual; bpassado = batual
      Watual = Wfuturo; batual = bfuturo
      }
    VetorSEQ = c(VetorSEQ, SEQ)        # salva sequencia SQE 
    Epoca = Epoca + 1                  # Incrementa Epoca
    }
  plot(VetorSEQ,type = "l",ylab = "SEQ", xlab = "Épocas")  # plota gráfico dos erros
  return(list(W=Watual,b=batual,SEQ=SEQ))                  # retorna resultados
}

(treino=treina_adaline(W,b,X,yd,alfa,beta,maxep,tol)) # Mostra os valores de W, b e VetorSEQ

(summary=lm(yd ~ as.vector(X)))

#### Plotar gráfico ####
plot(Xo,ydo)
lines(X, as.numeric(treino$b) + as.numeric(treino$W) * X, col = "blue",lwd=4)
lines(X, summary$coefficients[1] + summary$coefficients[2] * X, col = "red")
