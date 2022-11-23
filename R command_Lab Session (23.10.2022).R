##Pearson Correlation Manually##
x=c(13,13.5,12,12.8,11,7.8,7.9,8.3,8.5,8.9)
y=c(339.3,361,317,333.9,278.5,175,180.9,191.6,204.5,202.7)
df<-data.frame(x,y)
df
plot(x,y)
num_first=sum(x*y)
num_first
n=10
num_second=n*mean(x)*mean(y)
num_second
num_third=sqrt(sum(x^2)-n*(mean(x)^2))
num_third
num_fourth=sqrt(sum(y^2)-n*(mean(y)^2))
num_fourth
pearson_Correlation=(num_first-num_second)/(num_third*num_fourth)
pearson_Correlation

##Pearson Correlation with cor function##
cor(df,method="pearson")

##Test of Significance for Correlation Coefficient Manually##
t=abs(pearson_Correlation)/sqrt((1-pearson_Correlation^2)/(n-2))
t
p_value=pt(t,8,lower.tail=FALSE)
p_value

##Test of Significance for Correlation Coefficient with cor.test function##
cor.test(df$x,df$y,method="pearson")

##Spearman Rank Correlation Manually##
score_x=c(7.1,7.4,7.9,6.3,8.3,9.6,7.6,8.8,5.9,6.6)
score_y=c(61,53,76,47,73,77,69,81,43,36)
rank_score_x=rank(-score_x)
rank_score_x
rank_score_y=rank(-score_y)
rank_score_y
df1<-data.frame(score_x,score_y,rank_score_x,rank_score_y)
df1
di_square=(rank_score_x-rank_score_y)^2
di_square
n=10
spearman_correlation=1-(6*sum(di_square)/(n*(n^2-1)))
spearman_correlation

##Spearman Correlation with cor function##
cor(df1$score_x,df1$score_y,method="spearman")

##Test of Significance for Correlation Coefficient with cor.test function##
cor.test(score_x,score_y,method="spearman")

##Point Bi-serial Rank Correlation Manually##
love_animal=c(0,0,1,1,1,0,0,1,1,1)
EI=c(67,77,98,95,85,68,71,89,82,79)
df2=data.frame(love_animal,EI)
df2
Mp=mean(subset(EI,df2$love_animal=="1"))
Mp
Mq=mean(subset(EI,df2$love_animal=="0"))
Mq
SD=sd(EI)
SD
count=table(df2$love_animal)
count
p=count[2]/n
p
q=count[1]/n
q
point_bi_serl_corr=((Mp-Mq)/SD)*sqrt(p*q)
point_bi_serl_corr

##Phi Correlation Manually##
vc=c(1,1,1,1,1,0,0,0,0,0)
ef=c(1,1,1,0,0,0,0,0,1,1)
df3=data.frame(vc,ef)
df3
A=table(df3$vc==0,df3$ef=="1")
A                                   ## 1st row (FALSE->vc=1, TRUE->vc=0)##
				    ## 1st column (FALSE->ef=0, TRUE->ef=1
Phi_correl=(A[1]*A[4]-A[2]*A[3])/sqrt((A[1]+A[2])*(A[3]+A[4])*(A[1]+A[3])*(A[2]*A[4]))
Phi_correl

##Simple Linear Regression##
BW=c(176,176,190,176,200,167,188,195,176,165,158,148,149,163,170,186,146,181,149)
LW=c(6.5,9.5,9.0,8.9,7.2,8.9,8,10,8,7.9,6.9,7.3,5.2,8.4,7.2,6.3,7.3,9.1,6.4)
df4=data.frame(BW,LW)
SLR<-lm(LW~BW,data=df4)
summary(SLR)
predict_LW=1.64975+0.03579*150
predict_LW
par(mfrow=c(2,2))
plot(SLR)

##Binary Logistic Regression##
hours_study=c(2,2.5,4,5,8,4,3,2.2,7,3,3,5,7.5,3.2,5.5,7,7.9,5,4,1.5,5.5,5.9,6.9,5.6,1.6,5.8,2.8,2.1,6.8,7.5)
result=c(0,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,0,1,0,1,1,1,1,0,1,0,0,1,1)
df5=data.frame(hours_study,result)
df5
binary_reg_model=glm(result~hours_study,family=binomial(link="logit"))
summary(binary_reg_model)
exp(coef(binary_reg_model))
c1=seq(min(hours_study),max(hours_study),0.01)
c2=predict(binary_reg_model,list(hours_study=c1),type="response")
par(mfrow=c(1,1))
plot(hours_study,result)
lines(c1,c2)