library(readxl)
library(MASS)
library(ggplot2)
gdp <- read_excel("Factors_Affecting_GDP_per_Capita.xlsx", 
                  sheet = "Sheet1")
attach(gdp)

cor(FDL,GDPPC)
cor(FE,GDPPC)
cor(UR,GDPPC)
cor(OL,GDPPC)
cor(TIL,GDPPC)
cor(POEP,GDPPC)
cor(AVOPI,GDPPC)
cor(AVOSI,GDPPC)
cor(AVOTI,GDPPC)
cor(ILOFA,GDPPC)

par(mfrow=c(2,5))
plot(FDL,GDPPC)
plot(FE,GDPPC)
plot(UR,GDPPC)
plot(OL,GDPPC)
plot(TIL,GDPPC)
plot(POEP,GDPPC)
plot(AVOPI,GDPPC)
plot(AVOSI,GDPPC)
plot(AVOTI,GDPPC)
plot(ILOFA,GDPPC)

lm_f <- lm(GDPPC~FDL+FE+UR+OL+TIL
           +POEP+AVOPI+AVOSI+AVOTI
           +ILOFA)
summary(lm_f)

par(mfrow=c(1,3))
evals <- stdres(lm_f)
hist(evals)
qqnorm(evals)
abline(0,1)
plot(fitted(lm_f),evals)
ks.test(evals,pnorm,0,1)

lm_r <- lm(GDPPC~FE+TIL+POEP)
summary(lm_r)

gdp_fitted <- data.frame(YEAR,GDPPC,
                         predict.lm(lm_r,interval="confidence",level=0.95))
ggplot(data=gdp_fitted, mapping=aes(x=YEAR))+
  geom_ribbon(aes(ymin=lwr,ymax=upr,fill="95% Confidence \nInterval of the \nfitted values"))+
  scale_fill_manual(values=c("grey"))+
  geom_line(aes(y=fit, color = "Fitted Values"))+
  geom_line(aes(y=GDPPC, color="Actual Values"),size=1)+
  theme_bw()+
  labs(x="Year", y="GDP Per Capita")+
  theme(legend.title = element_blank())