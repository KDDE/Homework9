data<-read.table(url("http://www.datagarage.io/api/5488687d9cbc60e12d300ba5"))

o<-seq(2, 4000, by=2)
d_c<-as.character(data[o, ])
p_m<-sapply(strsplit(d_c[], ""), function(d_c) which(d_c == ":"))
data<-data.frame(X=as.double(substr(d_c,p_m[2,]+1, 38)), Y=as.double(substr(d_c,p_m[1,]+1, 17)))

x <- data[,1]
y <- data[,2]

fit <- lm(y ~ poly(x, 25, raw=TRUE))

xx <- seq(min(x),max(x),len=500)
yy <- data.frame(predict(fit, data.frame(x)))

plot(data)
lines(xx,predict(fit, data.frame(x=xx)), col="red")

print(yy)
