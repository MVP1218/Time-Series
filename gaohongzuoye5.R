fai<-50
beta<-0.99
rho<-0.95
theta<-0.4
zbar<-1
delta<-0.025
sk<-(1/beta-1+delta)/theta
kbar<-(1/sk)^(1/(1-theta))
ybar<-sk*kbar
cbar<-(sk-delta)*kbar
lambda<-1/cbar
a1<-1/beta
a2<-sk-delta
a3<-sk
a4<-beta*fai*kbar
a5<-beta*theta*(theta-1)*sk-fai*kbar-beta*fai*kbar
a6<-fai*kbar
a7<-beta*theta*sk



a<-a4+1/a2
b<-a5-(a1/a2)-(1/a2)
c<-a6+(a1/a2)
nkk<- -0.99932
nlk<-(nkk-a1)/a2
coe<-a4*nkk+a5+nlk+a4*rho+(rho-1)/a2
inter<-a3*(rho-1)/a2-a7*rho
nkz<-inter/coe
nlz<-nkz/a2-a3/a2
zt<-vector(length = 8)
zt[1]<-1
for(i in 2:8){
  zt[i]<-rho*zt[i-1]
}
kt<-vector(length = 8)
kt[1]<-nkz
for(i in 2:8){
  kt[i]<-nkk*kt[i-1]+nkz*zt[i]
}