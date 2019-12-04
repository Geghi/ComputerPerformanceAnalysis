#Caricamento e pulizia del dataset.
data = read.csv("tabella.csv", sep =";")
head(data)
data$Vendor.Name<- NULL
data$Model.Name<-NULL
data$ERP <- NULL


#Standardizzazione tabella
data.st = data.frame(scale(data))

#Visualizzazione grafica
plot(data, pch=19)

#La standardizzazione non varia il grafico di dispersione
#non è stata quindi considerata nel seguito dell'esperimento
plot(data.st, pch=19)

library(corrplot)
corrplot(cor(data))


#Regressione Lineare
data.lm1 = lm(PRP~.,data = data)
r = matrix(ncol = 2, nrow = 6)

summary(data.lm1)
r[1,]= c(summary(data.lm1)$r.squared, summary(data.lm1)$adj.r.squared)
r

data.lm2 = lm(PRP~.-CHMIN,data = data)
summary(data.lm2)
r[2,]= c(summary(data.lm2)$r.squared, summary(data.lm2)$adj.r.squared)
r

data.lm3 = lm(PRP~.-CHMIN-CHMAX,data = data)
summary(data.lm3)
r[3,]= c(summary(data.lm3)$r.squared, summary(data.lm3)$adj.r.squared)
r

data.lm4 = lm(PRP~.-CHMIN-CHMAX-MYCT,data = data)
summary(data.lm4)
r[4,]= c(summary(data.lm4)$r.squared, summary(data.lm4)$adj.r.squared)
r

data.lm5 = lm(PRP~.-CHMIN-CHMAX-MYCT-MMIN,data = data)
summary(data.lm5)
r[5,]= c(summary(data.lm5)$r.squared, summary(data.lm5)$adj.r.squared)
r

data.lm6 = lm(PRP~.-CHMIN-CHMAX-MYCT-MMIN-CACH,data = data)
summary(data.lm6)
r[6,]= c(summary(data.lm6)$r.squared, summary(data.lm6)$adj.r.squared)
r

ymin = min(r)
ymax = max(r)
plot(r[,1],pch=19, type = "b", col = "red", ylim=c(ymin,ymax))
lines(r[,2], pch=19, type = "b", col = "blue")

#Regressione Non Lineare
#Aggiungo 1 agli attributi CACH, CHMIN e CHMAZ in quanto contenenti zeri. 
#in questo modo posso applicare il logaritmo senza ottenere 
#valori pari a -Inf e senza comportare visibili variazioni 
#nei risultati della regressione.
data$CACH <- data$CACH + 1
data$CHMAX<- data$CHMAX + 1
data$CHMIN<- data$CHMIN + 1
ldata = log(data)
r = matrix(ncol = 2, nrow = 6)

data.lm1 = lm(PRP~.,data = ldata)
summary(data.lm1)
r[1,]= c(summary(data.lm1)$r.squared, summary(data.lm1)$adj.r.squared)
r
data.lm2 = lm(PRP~.-CHMIN,data = ldata)
summary(data.lm2)
r[2,]= c(summary(data.lm2)$r.squared, summary(data.lm2)$adj.r.squared)
r
data.lm3 = lm(PRP~.-CHMIN-CHMAX,data = ldata)
summary(data.lm3)
r[3,]= c(summary(data.lm3)$r.squared, summary(data.lm3)$adj.r.squared)
r
data.lm4 = lm(PRP~.-CHMIN-CHMAX-MYCT,data = ldata)
summary(data.lm4)
r[4,]= c(summary(data.lm4)$r.squared, summary(data.lm4)$adj.r.squared)
r
data.lm5 = lm(PRP~.-CHMIN-CHMAX-MYCT-MMIN,data = ldata)
summary(data.lm5)
r[5,]= c(summary(data.lm5)$r.squared, summary(data.lm5)$adj.r.squared)
r
data.lm6 = lm(PRP~.-CHMIN-CHMAX-MYCT-MMIN-CACH,data = ldata)
summary(data.lm6)
r[6,]= c(summary(data.lm6)$r.squared, summary(data.lm6)$adj.r.squared)
r

ymin = min(r)
ymax = max(r)
plot(r[,1],pch=19, type = "b", col = "red", ylim=c(ymin,ymax))
lines(r[,2], pch=19, type = "b", col = "blue")

#Controllo modello regressione lineare.
data.linear.lm = lm(PRP~.-CHMIN-CHMAX-MYCT-MMIN,data = data)
data.r = residuals(data.linear.lm)
plot(data.r, pch=19)
1-var(data.r)/var(data$PRP)
plot(fitted(data.lm5),data.r)

hist(data.r, 30, freq=F)
lines(density(data.r),col="red")
lines(sort(data.r),dnorm(sort(data.r),mean(data.r),sd(data.r)))

plot(ecdf(data.r), pch=19)
m = mean(data.r)
s = sd(data.r)
y = seq(m-3*s,m+3*s,6*s/100)
lines(y,pnorm(y,m,s),col="red")

qqnorm(data.r)
qqline(data.r)

shapiro.test(data.r)

#Controllo modello regressione non lineare.
ldata.r = residuals(data.lm5)
plot(ldata.r, pch=19)
1-var(ldata.r)/var(ldata$PRP)
plot(fitted(data.lm5),data.r)

hist(ldata.r, 30, freq=F)
lines(density(ldata.r),col="red")
lines(sort(ldata.r),dnorm(sort(ldata.r),mean(ldata.r),sd(ldata.r)))

plot(ecdf(ldata.r), pch=19)
m = mean(ldata.r)
s = sd(ldata.r)
y = seq(m-3*s,m+3*s,6*s/100)
lines(y,pnorm(y,m,s),col="red")

qqnorm(ldata.r)
qqline(ldata.r)

shapiro.test(ldata.r)

#Predizione
set.seed(4367)
testset = sort(sample(209, 20))

trainingSet <- data[-testset,]
testS <- data[testset,]

trainingSet.log <- ldata[-testset,]
testS.log <- ldata[testset,]

trainingSet.lm = lm(PRP~.-CHMIN-CHMAX-MYCT-MMIN,data = trainingSet)
prediction <- predict (trainingSet.lm, testS)
trainingSet.log.lm = lm(PRP~.-CHMIN-CHMAX-MYCT-MMIN,data = trainingSet.log)
prediction.log <- predict(trainingSet.log.lm, testS.log)
sqrt(mean((prediction - testS$PRP)^2)/mean(testS$PRP)^2)
sqrt(mean((prediction.log - testS.log$PRP)^2)/mean(testS.log$PRP)^2)


sqrt(mean((prediction - testS$PRP)^2))
sqrt(mean((exp(prediction.log) - testS$PRP)^2))

gmin=min(prediction ,exp(prediction.log),testS$PRP)
gmax=max(prediction ,exp(prediction.log),testS$PRP)
plot(testS$PRP,pch=20,ylim=c(gmin,gmax))
points(prediction ,col="blue",pch=20)
points(exp(prediction.log),col="red",pch=20)
legend("topright",inset=0.02,c("dati","modello lineare",
"modello logaritmico"),col=c("black","blue","red"),
pch=c(19,19),bg="gray",cex=.8)


n=30
 err_lin = rep(0,n)
 err_log = rep(0,n)
 for(i in 1:n){
 testset = sort(sample(209, 20))
 
 trainingSet <- data[-testset,]
 testS <- data[testset,]
 
 trainingSet.log <- ldata[-testset,]
 testS.log <- ldata[testset,]
 
 trainingSet.lm = lm(PRP~.-CHMIN-CHMAX-MYCT-MMIN,data = trainingSet)
 prediction <- predict (trainingSet.lm, testS)
 trainingSet.log.lm = lm(PRP~.-CHMIN-CHMAX-MYCT-MMIN,data = trainingSet.log)
 prediction.log <- predict(trainingSet.log.lm, testS.log)
 err_lin[i] = sqrt(mean((prediction - testS$PRP)^2)/mean(testS$PRP)^2)
 err_log[i] = sqrt(mean((prediction.log - testS.log$PRP)^2)/mean(testS.log$PRP)^2)
}

mean(err_lin)
mean(err_log)
sd(err_lin)
sd(err_log)
gmin=min(err_lin,err_log)
gmax=max(err_lin,err_log)
plot(err_lin,type="b",pch=20,col="blue",ylim=c(gmin,gmax))
points(err_log,type="b",pch=20,col="red")
legend("topleft",inset=0.02,c("modello lineare",
"modello logaritmico"),col=c("blue","red"),
pch=c(19,19),bg="gray",cex=.8)
