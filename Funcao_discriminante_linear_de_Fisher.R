## Biblioteca com a fun��o discriminante linear de Fisher
library(MASS)

## Dados (?iris apresenta informa��es sobre o conjunto de dados)
dados <- iris
names(dados)

cat("\n Tamanho da amostra:", n <- length(dados$Species))

summary(dados)

## Gr�ficos de dispers�o
cores <- rainbow(length(levels(dados[, "Species"])))
pairs(dados[, -5], pch = 21, bg = cores[dados$Species], lower.panel = NULL)

## An�lise discriminante
# Primeiro modelo (valida��o cruzada)
m1 <- lda(dados[, -5], dados$Species, CV = TRUE)

# Componentes de m1
names(m1)

# Segundo modelo (ressubstitui��o)
m2 <- lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
          data = dados)
# Componentes de m2
names(m2)

# Matriz de confus�o (valida��o cruzada)
tabela1 <- xtabs(~ m1$class + Species, data = dados)
cat("\n Matriz de confus�o (com valida��o cruzada):")
tabela1

cat("\n Acerto (%) = \n", levels(dados[, "Species"]), "\n",
    diag(tabela1) / colSums(tabela1) * 100)

cat("\n Acerto global (%) =", sum(diag(tabela1)) / n * 100)

# Matriz de confus�o (ressubstitui��o)
m2class <- predict(m2, dados)$class
tabela2 <- xtabs(~ m2class + Species, data = dados)
cat("\n Matriz de confus�o (com ressubstitui��o):")
tabela2

cat("\n Acerto (%) = \n", levels(dados[, "Species"]), "\n",
    diag(tabela2) / colSums(tabela2) * 100)

cat("\n Acerto global (%) =", sum(diag(tabela2)) / n * 100)

cat("\n Fun��es discriminantes: \n")
coef(m2)

cat("\n Raz�o dos desvios padr�o entre e intragrupos para cada FD =")
m2$svd

# Escores das observa��es
FD <- as.matrix(dados[, -5]) %*% coef(m2)
dim(FD)

#FD � uma matriz com 150 linhas (pois n = 150) e duas colunas. Cada coluna cont�m o escore de sua
#respectiva fun��o discriminante calculado para cada observa��o (nas linhas de FD).
# Centr�ides dos grupos e escores dos centr�ides
m2$means
(FDb <- m2$means %*% coef(m2))

# Gr�fic de pontos de FD1
stripchart(FD[, 1] ~ Species, pch = 20, xlab = "Fun��o discriminante 1",
           ylab = "Esp�cie", col = cores, method = "stack", data = dados)
points(FDb[, 1], (1:length(m2$lev)) + 0.05, pch = 13, col = cores, cex = 1.5)

# Histograma de FD1
ldahist(FD[, 1], dados$Species)

# Histograma de FD2
ldahist(FD[, 2], dados$Species, col = "red")

# Gr�fico de dispers�o de FD1 e FD2
plot(FD[, 1], FD[, 2], pch = 20, col = cores[dados$Species],
     xlab = "Fun��o discriminante 1", ylab = "Fun��o discriminante 2")
points(FDb[, 1], FDb[, 2], pch = 13, col = cores, cex = 1.5)
text(FDb[, 1], FDb[, 2], m2$lev)