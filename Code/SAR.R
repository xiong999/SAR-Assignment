require(ggplot2)
require(ggthemes)

# square function

f.square <- function(x){
  x^2
}
ggplot(data = data.frame(x = seq(-pi,pi,length.out = 500)),aes(x=x)) +
  stat_function(fun = f.square, geom = "line", size=2, col="black") +
  theme_classic() + 
  theme(text = element_text(size = 20)) +
  xlab("x") + ylab("Square")
ggsave(file = "F:/assignment/Figures/square.pdf")

# power function

f.power <- function(x,a){
  x^a
}
ggplot(data = data.frame(x = seq(-pi,pi,length.out = 500)),aes(x=x)) +
  stat_function(fun = f.power, geom = "line", size=2, col="blue",args = (a=1)) +
  stat_function(fun = f.power, geom = "line", size=2, col="red",args = (a=2)) +
  stat_function(fun = f.power, geom = "line", size=2, col="green",args = (a=3)) +
  theme_classic() + 
  theme(text = element_text(size = 20)) +
  xlab("x") + ylab("Power")
ggsave(file = "F:/assignment/Figures/power.pdf")

# sin(1/x)*x function

f.sinx <- function(x){
  x*sin(1/x)
}
ggplot(data = data.frame(x = seq(-pi,pi,length.out = 500)),aes(x=x)) +
  stat_function(fun = f.sinx, geom = "line", size=2, col="black") +
  theme_classic() + 
  theme(text = element_text(size = 20)) +
  xlab("x") + ylab("sinx")
ggsave(file = "F:/assignment/Figures/sinx.pdf")

# Intensity K distributions

f.K <- function(x,p_alpha,p_lambda,p_L){
  (2*p_lambda*p_L/(gamma(p_alpha)*gamma(p_L)))*((p_lambda*p_L*x)^((p_alpha+p_L)/2-1))*besselK(2*sqrt(p_lambda*p_L*x),p_alpha-p_L)
}
ggplot(data=data.frame(x=seq(0.01, 7, length.out = 500)), aes(x=x)) +
  stat_function(fun=dexp, geom = "line", size=2, col="green", args = (mean=1)) +
  stat_function(fun=f.K, geom = "line", size=2, col="red", args = list(p_alpha=1, p_lambda=1, p_L=1)) +
  stat_function(fun=f.K, geom = "line", size=2, col="blue", args = list(p_alpha=3, p_lambda=3, p_L=1)) +
  stat_function(fun=f.K, geom = "line", size=2, col="black", args = list(p_alpha=8, p_lambda=8, p_L=1)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Intensity K and Exponential Densities")
ggsave(file = "F:/assignment/Figures/KIDensities.pdf")

ggplot(data=data.frame(x=seq(0.01, 7, length.out = 500)), aes(x=x)) +
  stat_function(fun=dexp, geom = "line", size=2, col="green", args = (mean=1)) +
  stat_function(fun=f.K, geom = "line", size=2, col="red", args = list(p_alpha=1, p_lambda=1, p_L=1)) +
  stat_function(fun=f.K, geom = "line", size=2, col="blue", args = list(p_alpha=3, p_lambda=3, p_L=1)) +
  stat_function(fun=f.K, geom = "line", size=2, col="black", args = list(p_alpha=8, p_lambda=8, p_L=1)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  coord_trans(y="log10") +
  xlab("x") + ylab("Intensity K and Exponential Densities")
ggsave(file="F:/assignment/Figures/KIDensitiesSemilog.pdf")  

ggplot(data=data.frame(x=seq(0.01, 7, length.out = 500)), aes(x=x)) +
  stat_function(fun=f.K, geom = "line", size=2, col="red", args = list(p_alpha=2, p_lambda=2, p_L=1)) +
  stat_function(fun=f.K, geom = "line", size=2, col="blue", args = list(p_alpha=2, p_lambda=2, p_L=3)) +
  stat_function(fun=f.K, geom = "line", size=2, col="black", args = list(p_alpha=2, p_lambda=2, p_L=8)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Intensity K densities with varying Looks ")
ggsave(file="F:/assignment/Figures/KIDensitiesLooks.pdf")  

ggplot(data=data.frame(x=seq(0.01, 7, length.out = 500)), aes(x=x)) +
  stat_function(fun=f.K, geom = "line", size=2, col="red", args = list(p_alpha=2, p_lambda=2, p_L=1)) +
  stat_function(fun=f.K, geom = "line", size=2, col="blue", args = list(p_alpha=2, p_lambda=2, p_L=3)) +
  stat_function(fun=f.K, geom = "line", size=2, col="black", args = list(p_alpha=2, p_lambda=2, p_L=8)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  coord_trans(y="log10") +
  xlab("x") + ylab("Intensity K Densities with varying Looks")
ggsave(file="F:/assignment/Figures/KIDensitiesSemilogLooks.pdf")  

# Intensity GI0 distributions

f.GI0 <- function(x,p_alpha,p_gamma,p_L){
  ((p_L^p_L*gamma(p_L-p_alpha))/(p_gamma^p_alpha*gamma(p_L)*gamma(-p_alpha)))*(x^(p_L-1)/(p_gamma+p_L*x)^(p_L-p_alpha))
}

ggplot(data=data.frame(x=seq(0.01, 10, length.out = 500)), aes(x=x)) +
  stat_function(fun=dexp, geom = "line", size=2, col="green", args = (mean=1)) +
  stat_function(fun=f.GI0, geom = "line", size=2, col="red", args = list(p_alpha=-1.5, p_gamma=.5, p_L=1)) +
  stat_function(fun=f.GI0, geom = "line", size=2, col="blue", args = list(p_alpha=-3, p_gamma=2, p_L=1)) +
  stat_function(fun=f.GI0, geom = "line", size=2, col="black", args = list(p_alpha=-8, p_gamma=7, p_L=1)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Intensity G0 and Exponential Densities")
ggsave(file="F:/assignment/Figures/GI0Densities.pdf")  

ggplot(data=data.frame(x=seq(0.01, 10, length.out = 500)), aes(x=x)) +
  stat_function(fun=dexp, geom = "line", size=2, col="green", args = (mean=1)) +
  stat_function(fun=f.GI0, geom = "line", size=2, col="red", args = list(p_alpha=-1.5, p_gamma=.5, p_L=1)) +
  stat_function(fun=f.GI0, geom = "line", size=2, col="blue", args = list(p_alpha=-3, p_gamma=2, p_L=1)) +
  stat_function(fun=f.GI0, geom = "line", size=2, col="black", args = list(p_alpha=-8, p_gamma=7, p_L=1)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  coord_trans(y="log10") +
  xlab("x") + ylab("Intensity G0 and Exponential Densities")
ggsave(file="F:/assignment/Figures/GI0DensitiesSemilog.pdf")  

ggplot(data=data.frame(x=seq(0.01, 10, length.out = 500)), aes(x=x)) +
  stat_function(fun=f.GI0, geom = "line", size=2, col="red", args = list(p_alpha=-5, p_gamma=4, p_L=1)) +
  stat_function(fun=f.GI0, geom = "line", size=2, col="blue", args = list(p_alpha=-5, p_gamma=4, p_L=3)) +
  stat_function(fun=f.GI0, geom = "line", size=2, col="black", args = list(p_alpha=-5, p_gamma=4, p_L=8)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Intensity G0 densities with varying Looks ")
ggsave(file="F:/assignment/Figures/GI0DensitiesLooks.pdf")  

ggplot(data=data.frame(x=seq(0.01, 10, length.out = 500)), aes(x=x)) +
  stat_function(fun=f.GI0, geom = "line", size=2, col="red", args = list(p_alpha=-5, p_gamma=4, p_L=1)) +
  stat_function(fun=f.GI0, geom = "line", size=2, col="blue", args = list(p_alpha=-5, p_gamma=4, p_L=3)) +
  stat_function(fun=f.GI0, geom = "line", size=2, col="black", args = list(p_alpha=-5, p_gamma=4, p_L=8)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  coord_trans(y="log10") +
  xlab("x") + ylab("Intensity G0 Densities with varying Looks")
ggsave(file="F:/assignment/Figures/GI0DensitiesSemilogLooks.pdf")  

# estimator
GI0.Estimator.m1m2 <- function(z,L){
  m1 <- mean(z)
  m2 <- mean(z^2)
  m212 <- m2/m1^2
  
  a <- -2 - (L+1) / (L * m212)
  g <- m1 * (2 + (L+1) / (L *m212))
  
  return(list("alpha"=a,"gamma"=g))
}


# LogLikelihoodLknown
LogLikelihoodLknown <- function(params){
  p_alpha <- -abs(params[1])
  p_gamma <- abs(params[2])
  p_L <- abs(params[3])
  
  n <- length(z)
  
  return(
    n*(lgamma(p_L-p_alpha) - p_alpha*log(p_gamma) - lgamma(-p_alpha)) + (p_alpha-p_L)*sum(log(p_gamma + z*p_L))
  )
}

Intensity_restricted <- subset(vUrbanHV$UHV, subset = vUrbanHV$UHV <= 100000)
binwidth_restricted <- 2*IQR(Intensity_restricted)*length(Intensity_restricted)^(-1/3)
vUrbanHV <- data.frame(UHV=as.vector(UrbanHV[90:200,50:100]))
meanUHV <- mean(vUrbanHV$UHV)
secondUHV <- mean(vUrbanHV$UHV^2)
a <- 2 * (1-CoeffVariation2) / (CoeffVariation2-2)
g <- meanUHV * (-a-1)
z <- vUrbanHV$UHV

estim.Urban <- GI0.Estimator.m1m2(UrbanHV,1)
estim.UrbanML <- maxNR(LogLikelihoodLknown,
                       start = c(estim.Urban$alpha,estim.Urban$gamma,1),
                       activePar = c(TRUE,TRUE,FALSE))$estimate[1:2]

ggplot(data=vUrbanHV, aes(x=UHV)) + 
  geom_histogram(aes(y=..density..), 
                 binwidth = binwidth_restricted) + 
  xlim(0,200000) +
  stat_function(fun=dexp, args=list(rate=1/meanUHV), 
                col="red", lwd=2, alpha=.7) +
  stat_function(fun=f.GI0, args = list(p_alpha=estim.Urban$alpha, p_gamma=estim.Urban$gamma, p_L=1),
                col="blue", lwd=2, alpha=.7) +
  stat_function(fun=f.GI0, args = list(p_alpha=estim.UrbanML[1], p_gamma=estim.UrbanML[2], p_L=1),
                col="green", lwd=2, alpha=.7) +
  xlab("Intensities from the Urban Area") +
  ylab("Histogram, and fitted Exponential and G0 Laws") +
  ggtitle("Restricted Histogram") +
  theme_few()
ggsave(file="F:/assignment/Figures/HistogramRestrictedUrbanWFitted.pdf")











