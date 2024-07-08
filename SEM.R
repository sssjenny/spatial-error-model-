library(spData)
library(sf)
library(spdep) 
library(sfdep) 
library(spatialreg)
library(lmtest) #for LM test
GA_sf = st_read(system.file("shapes/georgia.shp", package="spgwr") )
table(st_is_valid(GA_sf))
nrow(GA_sf)
head(GA_sf)

#Standard linear regression
#Model fitting
m1 = lm(PctPov ~ PctRural + PctBach + PctEld + PctFB + PctBlack, data=GA_sf)
summary(m1)

m2 = step(m1)
summary(m2)

n <- nrow(GA_sf) #n is the sample size
m3 <- step(m1, k=log(n))

summary(m3)

#Neighborhood structure and spatial weights matrix
GA_sf = st_read(system.file("shapes/georgia.shp", package="spgwr") )
table(st_is_valid(GA_sf))
nrow(GA_sf)
head(GA_sf)

sf_use_s2(FALSE)
sf::sf_use_s2(FALSE)

pov_nb <- poly2nb(GA_sf, queen=TRUE)
pov_nb

listw_povW <- nb2listw(pov_nb, style = "W", zero.policy = TRUE)
listw_povB <- nb2listw(pov_nb, style = "B", zero.policy = TRUE)


#4.	Spatial Regression
#a.Diagnostics for spatial dependence
lm.morantest(m3, listw_povB, zero.policy = TRUE, alternative = "two.sided")

#b.Spatial Lag Models
m3_lag <- lagsarlm(PctPov ~ PctBach + PctEld + PctBlack, data = GA_sf, listw = listw_povW, type = "lag", zero.policy = TRUE)
summary(m3_lag, correlation=FALSE)

cbind(coefest= coef(m3_lag), confint(m3_lag))

plot(m3_lag$fitted.values, m3_lag$residuals, xlab="fitted values", ylab="Residuals", main ="Residuals vs Fitted", cex=0.1)
abline(h=0, lty=2)

class(m3_lag)
bptest.Sarlm(m3_lag)

#c.Spatial Error Models
m3_err = errorsarlm(PctPov ~ PctBach + PctEld + PctBlack, data = GA_sf, listw=listw_povW, zero.policy = TRUE)
summary(m3_err)

moran.mc(residuals(m3_err), listw_povW, zero.policy = TRUE, nsim=999)
