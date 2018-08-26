getwd()
data=read.csv("data2803.csv",header = TRUE)
str(data)

#min-max normalization
data$loud=(data$loud-min(data$loud))/(max(data$loud)-min(data$loud))
data$tempo=(data$tempo-min(data$tempo))/(max(data$tempo)~min(data$tempo))

#data partition
set.seed(222)
ind=sample(2,nrow(data),replace = TRUE, prob = c(0.7,0.3))
training=data[ind==1,]
testing=data[ind==2,]

#export to excell
write.csv(training, "F:/Tugas Akhir/baru/traininkFIX.csv")
write.csv(testing, "F:/Tugas Akhir/baru/testinkFIX.csv")

#neural network
library(neuralnet)
n=neuralnet(playlist~acou+dance+energy+loud+speech+tempo+val,
            data = training,
            hidden = c(4),
            err.fct="ce",
            linear.output = FALSE,
            algoritm = "backprop",
            learningrate = 0.01)
plot(n)

#prediction
output=compute(n,training[,-1])
output
head(output$net,result)
head(training[1,])


#confuisions matrix & missclasification error - training
p1=output$net.result
pred1=ifelse(p1>0.5,1,0)
tab1=table(pred1,training$playlist)
tab1

1-sum(diag(tab1))/sum(tab1)


#confuisions matrix & missclasification error - testing
output=compute(n,testing[,-1])
p2=output$net.result
pred2=iflse(p2>0.5,1,0)
tab2=table(pred2,testing$playlist)
tab2

1-sum(diag(tab2))/sum(tab2)
