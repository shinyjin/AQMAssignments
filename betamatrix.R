library(AER)
library(ggplot2)
library(car)
adv <- read.csv("Advertising.csv")
Sales <- adv$Sales
adv <- adv[-1]
adv <- adv[-4] 
adv2 <- cbind(1,adv)
X <- data.matrix(adv2, rownames.force = NA)
Y <- Sales
beta <- (solve(t(X)%*%X))%*%t(X)%*%Y
#residuals(Y-Xbeta)^t(Y-Xbeta)
SS <- t(Y)%*%Y+(t(X%*%beta))%*%(X%*%beta)-t(Y)%*%X%*%beta-t(beta)%*%t(X)%*%Y
R <- Y-X%*%(beta)

residualsTV <- ggplot(adv,aes(TV,R))

residualsTV+geom_point()+geom_hline(yintercept = 0, col = "blue", lwd = 1)+
  labs(title = "TV residuals")
labs(y = "Residuals")

residualsTV <- ggplot(adv,aes(TV,R))
residualsTV+geom_point()+geom_hline(yintercept = 0, col = "blue", lwd = 1)+
  labs(title = "TV residuals") + labs(y = "Residuals")

residualsRadio <- ggplot(adv,aes(Radio,R))
residualsRadio+geom_point()+geom_hline(yintercept = 0, col = "blue", lwd = 1)+
  labs(title = "Radio residuals") + labs(y = "Residuals")

residualsNewspaper <- ggplot(adv,aes(Newspaper,R))
residualsNewspaper+geom_point()+geom_hline(yintercept = 0, col = "blue", lwd = 1)+
  labs(title = "Newspaper residuals") + labs(y = "Residuals")