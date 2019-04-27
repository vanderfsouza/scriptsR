#### Preceptron ####

## Declaração das variáveis
X = matrix(c(0, 1, 0, 1, 0, 0, 1, 1),   # Matriz de dados
           nrow=2,ncol=4,byrow = TRUE)   
yd = c(0, 0, 0, 1)                      # Saída desejada (AND) para cada coluna de X
alfa = 1.2                              # Taxa de correção dos pesos
maxep = 10                              # valor máximo de épocas de treinamento
tol = 0.001                             # erro máximo tolerável

# Pesos (W) e Bias (b) aleatórios
(W = sample(-100:100, 2, replace=TRUE)/100)  ## Pesos
(b = sample(-100:100, 1, replace=TRUE)/100)  ## bias

## Função perceptron
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

## Teste da percetron sem pesos e bias ajustados
(y = yperceptron(W,b,X))               # o resultado correto seria: 0 0 0 1


## Algoritmo de treinamento do Perceptron
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

yperceptron(treino$W,treino$b,X)  # o resultado esperado é: 0 0 0 1
