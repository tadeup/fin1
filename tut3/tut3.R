install.packages("quantmod")
library(quantmod)
library(dplyr)

selic.janeiro <- 1.06 / 100
selic.day <- (1 + selic.janeiro)^(1/21) - 1
selic.week <- (1 + selic.janeiro)^(1/4) - 1

symbols <- c("CCRO3", "HGTX3", "LAME4","POMO4","RENT3","GUAR3", "IBXX")
#getSymbols(symbols, src = "google", from = "2016-01-01", to = "2016-12-31")[,4]
#symbols <- c("CCRO3","HGTX3","LAME4","POMO4","RENT3","GUAR3","IBXX")

GetValues <- function(x) getSymbols.google(x,from = "2016-01-01", to = "2016-12-31", auto.assign = FALSE)[,4]
percent.diff <- function(x) diff(x)/x[-nrow(x),]
#percent.diff <- function(x) diff(x)/x[-1,]
retorno.real <- function(x) (x - selic.day)*100

prices <- lapply(symbols, GetValues) %>%
  lapply(percent.diff) %>%
  lapply(retorno.real) %>%
  as.data.frame()

prices <- prices[-1,]
colnames(prices) <- symbols

equations <- list(
  ccro = CCRO3 ~ IBXX,
  hgtx3 = HGTX3 ~ IBXX,
  lame4 = LAME4 ~ IBXX,
  pomo4 = POMO4 ~ IBXX,
  rent3 = RENT3 ~ IBXX,
  guar3 = GUAR3 ~ IBXX
)


regressoes <- lapply(equations, lm, data=prices)
lapply(regressoes, summary)

#### fazendo do jeito do livro ####
coeficientes <- lapply(regressoes, coef) %>%
  as.data.frame()
alpha <- coeficientes[1,]
beta <- coeficientes[2,]

variancia <- lapply(regressoes, resid) %>% lapply(var)

w0 <- alpha/variancia

w <- w0 / sum(w0)

alpha.A <- sum(alpha*w)

variancia.A <- sum((w^2)*(unlist(variancia)))

R_m <- prices$IBXX
w0.A <- (alpha.A/variancia.A)/(mean(R_m)/var(R_m))

beta.A <- sum(w*beta)

w.A <- w0.A/(1+(1-beta.A)*w0.A)

w.M <- 1 - w.A
w.star <- w.A*w

esperanca.R_p <- (w.M+w.A*beta.A)*mean(R_m)+w.A*alpha.A

variancia.R_p <- ((w.M+w.A*beta.A)^2)*var(R_m) + (w.A*variancia.A^(1/2))^2






#symbolsDividends <- c("CMIG4.SA","ENBR3","CPLE6","BRKM5","EGIE3","VIVT4")
#getSymbols(symbolsDividends,src="google")