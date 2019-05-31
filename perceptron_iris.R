#### Preceptron ####
str(iris)
plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species)
# versicolor e virginica não são linearmente separáveis 
# setosa
dados = iris[iris$Species == "setosa" | iris$Species == "versicolor",]
dados$novo[dados$Species == "setosa"]=0
dados$novo[dados$Species == "versicolor"]=1 

#### separação dos dados em conjunto de treinamento e conjunto de teste ####
smp_size <- floor(0.75 * nrow(dados)) ## 75% of the sample size
#set.seed(123) ## set the seed to make your partition reproducible
train_ind <- sample(seq_len(nrow(dados)), size = smp_size)
train <- dados[train_ind, ] # Treinamento
test  <- dados[-train_ind, ] # Test

X = t(as.matrix(train[1:4])) # X treino
yd = t(train[6])             # y treino

Xt = t(as.matrix(test[1:4])) # X teste
ydt = t(test[6])             # y teste

#### Declaração de parâmetros ####
alfa = 1.2                              # Taxa de correção dos pesos
maxep = 10                              # valor máximo de épocas de treinamento
tol = 0.001                             # erro máximo tolerável

# Pesos (W) e Bias (b) aleatórios 
(W = sample(-100:100, 4, replace=TRUE)/100)  ## Pesos
(b = sample(-100:100, 1, replace=TRUE)/100)  ## bias

#### Função perceptron ####
yperceptron = function(W,b,X) {
  U=W%*%X+b                             # Calcula a saída do perceptron
  y=matrix()
  for (i in 1:ncol(U)){                 # Função de ativação
    if (U[i] > 0){
      y[i] = 1
    } else {
      y[i] = 0
    }
  }
  return(y)
}

## Teste da percetron sem os pesos e o bias ajustados
(y = yperceptron(W,b,X))               # o resultado correto seria: 0 0 0 1


#### Algoritmo de treinamento do Perceptron ####
treina_perceptron = function(W,b,X,yd,alfa,maxep,tol){
  N = ncol(X)                          # N = número de amostras de X
  SEQ = tol                            # SEQ(somatorio dos erros quadraticos) = tolerância estabelecida  
  Epoca = 1                            # critério de início 
  erro=c();VetorSEQ=c()                # cria vetores para armazenamento
  while (Epoca <= maxep & SEQ >= tol) {
    SEQ =  0                           # inicia SEQ da Epoca em 0
    for(i in 1:N){                     # loop para cada padrão i de entrada
      y[i] = yperceptron(W,b,X[,i])    # aplica o par Xi ao perceptron
      erro[i] = yd[i] - y[i]           # determina o erro pela diferença entre esperado e obtido
      W = W + alfa*erro[i]*X[,i]       # atualiza o vetor peso
      b = b + alfa*erro[i]             # atualiza o bias
      SEQ = SEQ + erro[i]^2            # acumula SEQ
    }
    VetorSEQ = c(VetorSEQ, SEQ)        # salva sequencia SQE 
    Epoca = Epoca + 1                  # Incrementa Epoca
  }
  plot(VetorSEQ,type = "l",ylab = "SEQ", xlab = "Épocas")  # plota gráfico dos erros
  return(list(W=W,b=b,VetorSEQ=VetorSEQ))                  # retorna resultados
}

(treino=treina_perceptron(W,b,X,yd,alfa,maxep,tol)) # Mostra os valores de W, b e VetorSEQ

(y=yperceptron(treino$W,treino$b,X))
y==yd

(yt=yperceptron(treino$W,treino$b,Xt))
yt==ydt

#### Plotar gráfico ####
# w1*x + w2*y + b = 0
# se x = 0 então y = -b/w2
# se y = 0 então x = -b/w1
# equação da reta: y = slope*x + intercept

slope = -(treino$b/treino$W[2])/(treino$b/treino$W[1])  
intercept = -treino$b/treino$W[2]

x1 = seq(min(X),max(X),0.1)
y1 = slope*x1 + intercept

plot(X[1,],X[2,],pch=y,col=as.factor(yd))
lines(x1,y1) #ERRO
