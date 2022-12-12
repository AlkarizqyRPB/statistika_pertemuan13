regresi<-read.table("C:/Intel/regresi.txt",header=T)
kualitaslayanan<-regresi$X
tingkatpenjualan<-regresi$Y
plot(kualitaslayanan,tingkatpenjualan)
hasil<-lm(tingkatpenjualan~kualitaslayanan)

#normalitas
residual<-resid(hasil)
residual
qqnorm(residual)
shapiro.test(residual)

#nonautokorelasi
plot(residual)
library(lmtest)
dwtest(tingkatpenjualan~kualitaslayanan)

#homoskedastisitas
taksiran<-fitted(hasil)
taksiran
kresid<-residual*residual
kresid
plot(taksiran,kresid)
cor.test(abs(residual),kualitaslayanan,method="spearman")

#model
summary(hasil)

#analisis korelasi
cor.test(kualitaslayanan, tingkatpenjualan)
