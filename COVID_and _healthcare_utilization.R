library(ggplot2)
library(ggthemes)
library(MASS)

load("df_byregion.Rdata")
load("df_by_facility_adjusted.Rdata")

df2<-subset(df.r, year>2015)

df2$jan20<-ifelse(df2$month.index==49,1,0) ## Jan 2020
df2$feb20<-ifelse(df2$month.index==50,1,0)  ## Feb 2020
df2$mar20<-ifelse(df2$month.index>50,1,0)  ## Mar 2020

df2$t_mar20<-ifelse(df2$month.index<51,0,df2$month.index-51)
df2$t_mar20<-ifelse(df2$month.index>53,3,df2$t_mar20)
df2$t_jun20<-ifelse(df2$month.index<54,0,df2$month.index-54)

df2$feb22<-ifelse(df2$month.index==74,1,0) ## feb 22
df2$mar22<-ifelse(df2$month.index==75,1,0) ## march 22
df2$apr22<-ifelse(df2$month.index==76,1,0)  ## april 2022
df2$may22<-ifelse(df2$month.index==77,1,0) 
#df2$jan22<-ifelse(df2$month.index==73&(df2$region=="Shaanxi"|df2$region=="Tianjin"),1,0) 

df2$jan22<-ifelse(df2$month.index==73,1,0) 
######
library(MASS)




##### feb_opt,ipt
op_result_feb22<-matrix(,nrow=4,ncol=33)
ip_result_feb22<-matrix(,nrow=4,ncol=33)

for (i in 1:length(unique(df2$region))){
  model <- glm.nb(hospital.visits*10000 ~ month.index+as.factor(month)+jan20+feb20+mar20+t_mar20+t_jun20+feb22+mar22+apr22
                  +sf.day+offset(log(population*10000)) +jan22+may22,
                  data=subset(df2, region==unique(df2$region)[i]))
  imme<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[18])
  imme_low<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[18]-1.96*
                  coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[18,2])
  imme_upper<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[18]+1.96*
                    coeftest(model, vcov=NeweyWest(model, lag=1,prewhite = F))[18,2])
  p1<-coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[18,4]
  
  op_result_feb22[,i]<-c(imme,imme_low,imme_upper, p1)
}

View(op_result_feb22)

for (i in 1:length(unique(df2$region))){
  model <- glm.nb(hospital.discharged*10000 ~ month.index+as.factor(month)+jan20+feb20+mar20+t_mar20+t_jun20+feb22+mar22+apr22
                  +sf.day+offset(log(population*10000))+jan22+may22 ,
                  data=subset(df2, region==unique(df2$region)[i]))
  imme<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[18])
  imme_low<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[18]-1.96*
                  coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[18,2])
  imme_upper<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[18]+1.96*
                    coeftest(model, vcov=NeweyWest(model, lag=1,prewhite = F))[18,2])
  p1<-coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[18,4]
  
  ip_result_feb22[,i]<-c(imme,imme_low,imme_upper, p1)
}

View(ip_result_feb22)

##### may_opt,ipt
op_result_may22<-matrix(,nrow=4,ncol=33)
ip_result_may22<-matrix(,nrow=4,ncol=33)

for (i in 1:length(unique(df2$region))){
  model <- glm.nb(hospital.visits*10000 ~ month.index+as.factor(month)+jan20+feb20+mar20+t_mar20+t_jun20+feb22+mar22+apr22
                  +sf.day+offset(log(population*10000)) +may22+jan22,
                  data=subset(df2, region==unique(df2$region)[i]))
  imme<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[22])
  imme_low<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[22]-1.96*
                  coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[22,2])
  imme_upper<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[22]+1.96*
                    coeftest(model, vcov=NeweyWest(model, lag=1,prewhite = F))[22,2])
  p1<-coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[22,4]
  
  op_result_may22[,i]<-c(imme,imme_low,imme_upper, p1)
}

View(op_result_may22)

for (i in 1:length(unique(df2$region))){
  model <- glm.nb(hospital.discharged*10000 ~ month.index+as.factor(month)+jan20+feb20+mar20+t_mar20+t_jun20+feb22+mar22+apr22
                  +sf.day+offset(log(population*10000))+may22+jan22 ,
                  data=subset(df2, region==unique(df2$region)[i]))
  imme<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[22])
  imme_low<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[22]-1.96*
                  coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[22,2])
  imme_upper<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[22]+1.96*
                    coeftest(model, vcov=NeweyWest(model, lag=1,prewhite = F))[22,2])
  p1<-coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[22,4]
  
  ip_result_may22[,i]<-c(imme,imme_low,imme_upper, p1)
}

View(ip_result_may22)



############
### march_april _opt
op_result_mar22<-matrix(,nrow=4,ncol=33)
op_result_apr22<-matrix(,nrow=4,ncol=33)


for (i in 1:length(unique(df2$region))){
  model <- glm.nb(hospital.visits*10000 ~ month.index+as.factor(month)+jan20+feb20+mar20+t_mar20+t_jun20+mar22+apr22
                  +sf.day+offset(log(population*10000))+may22+jan22+feb22 ,
                  data=subset(df2, region==unique(df2$region)[i]))
  imme<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[18])
  imme_low<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[18]-1.96*
                  coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[18,2])
  imme_upper<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[18]+1.96*
                    coeftest(model, vcov=NeweyWest(model, lag=1,prewhite = F))[18,2])
  p1<-coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[18,4]
  
  op_result_mar22[,i]<-c(imme,imme_low,imme_upper, p1)
}

View(op_result_mar22)


for (i in 1:length(unique(df2$region))){
  model <- glm.nb(hospital.visits*10000 ~ month.index+as.factor(month)+jan20+feb20+mar20+t_mar20+t_jun20+mar22+apr22
                  +sf.day+offset(log(population*10000))+may22+jan22+feb22 ,
                  data=subset(df2, region==unique(df2$region)[i]))
  imme<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[19])
  imme_low<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[19]-1.96*
                  coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[19,2])
  imme_upper<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[19]+1.96*
                    coeftest(model, vcov=NeweyWest(model, lag=1,prewhite = F))[19,2])
  p1<-coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[19,4]
  
  op_result_apr22[,i]<-c(imme,imme_low,imme_upper, p1)
}
View(op_result_apr22)


### march_april _ipt
ip_result_mar22<-matrix(,nrow=4,ncol=33)
ip_result_apr22<-matrix(,nrow=4,ncol=33)

for (i in 1:length(unique(df2$region))){
  model <- glm.nb(hospital.discharged*10000 ~ month.index+as.factor(month)+jan20+feb20+mar20+t_mar20+t_jun20+mar22+apr22
                  +sf.day+offset(log(population*10000))+may22+jan22+feb22 ,control=glm.control(maxit=500),
                  data=subset(df2, region==unique(df2$region)[i]))
  imme<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[18])
  imme_low<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[18]-1.96*
                  coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[18,2])
  imme_upper<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[18]+1.96*
                    coeftest(model, vcov=NeweyWest(model, lag=1,prewhite = F))[18,2])
  p1<-coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[18,4]
  
  ip_result_mar22[,i]<-c(imme,imme_low,imme_upper, p1)
}

View(ip_result_mar22)


for (i in 1:length(unique(df2$region))){
  model <- glm.nb(hospital.discharged*10000 ~ month.index+as.factor(month)+jan20+feb20+mar20+t_mar20+t_jun20+mar22+apr22
                  +sf.day+offset(log(population*10000)) +may22+jan22+feb22+may22,control=glm.control(maxit=500),
                  data=subset(df2, region==unique(df2$region)[i]))
  imme<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[19])
  imme_low<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[19]-1.96*
                  coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[19,2])
  imme_upper<-exp(coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[19]+1.96*
                    coeftest(model, vcov=NeweyWest(model, lag=1,prewhite = F))[19,2])
  p1<-coeftest(model, vcov=NeweyWest(model,lag=1, prewhite = F))[19,4]
  
  ip_result_apr22[,i]<-c(imme,imme_low,imme_upper, p1)
}
View(ip_result_apr22)

##### Overall impact

D<-Y_sum<-Y0_sum<-D_P<-NA
mD<-low_D<-up_D<-mY_sum<-mY0_sum<-mD_P<-low_D_P<-up_D_P<-P<-NA

for (r in 1:33) {
  for(i in 1:10000){
    glm_m <- glm.nb(hospital.discharged*10000 ~ month.index+as.factor(month)+jan20+feb20+mar20+t_mar20+t_jun20+jan22+feb22+mar22+apr22+may22
                    +sf.day+offset(log(population*10000)),
                    data=subset(df2, region==unique(df2$region)[r]))
    
    model<-coeftest(glm_m)
    X<-model.matrix(glm_m)
    X0<-X
    X0[,20:22]<-0
    Sigma<-vcov(glm_m)
    set.seed(123)
    beta<-mvrnorm(n=10000,model[,1],Sigma)
    
    Y_sum[i]<-sum(as.vector(exp(X %*% beta[i,])* subset(df2,year==2022&region==unique(df2$region)[r])$population[1])[69:71])  ## * population
    Y0_sum[i]<-sum(as.vector(exp(X0 %*% beta[i,])* subset(df2,year==2022&region==unique(df2$region)[r])$population[1])[69:71])
    D[i]<-Y0_sum[i]-Y_sum[i]
    D_P[i]<-(Y0_sum[i]-Y_sum[i])/Y0_sum[i]
  }
  mY_sum[r]<-mean(Y_sum)
  mY0_sum[r]<-mean(Y0_sum)
  mD[r]<-mean(D)
  low_D[r]<-quantile(D, 0.025, na.rm = T)
  up_D[r]<-quantile(D, 0.975, na.rm = T)
  
  mD_P[r]<-mean(D_P)
  low_D_P[r]<-quantile(D_P, 0.025, na.rm = T)
  up_D_P[r]<-quantile(D_P, 0.975, na.rm = T)
  
  P[r]<-2*length(which(D>0))/1000
  P[r]<-ifelse(P[r]>1,2-P[r],P[r])
}

overall<-cbind(mY_sum,mY0_sum,mD,low_D,up_D,mD_P,low_D_P,up_D_P,P)

####
df_f1<-read.csv("result_fig1_long.csv")
df_f1$opt_ipt<-factor(df_f1$opt_ipt,levels=c("opt","ipt"),labels=c("Outpatient","Inpatient"))
df_f1$month<-factor(df_f1$month,levels=c("feb","mar","apr","may"),labels=c("February","March","April","May"))
df_f1$quadrant<-"I"
df_f1$quadrant<-ifelse(df_f1$volumn_change<0&df_f1$moveout_change>1,"II",df_f1$quadrant)
df_f1$quadrant<-ifelse(df_f1$volumn_change<0&df_f1$moveout_change<1,"III",df_f1$quadrant)
df_f1$quadrant<-ifelse(df_f1$volumn_change>0&df_f1$moveout_change<1,"IV",df_f1$quadrant)

pop<-read.csv("pop_byregion.csv")
df_f1$population<-NA
df_f1$region<-as.character(df_f1$region)
pop$region<-as.character(pop$region)

for (i in 1: dim(df_f1)[1]){
  df_f1$population[i]<-subset(pop, year==2022&region==df_f1$region[i] )$population
}

with(subset(df_f1,opt_ipt=="Outpatient"&quadrant=="III"),tapply(population,month,sum))/139653 
with(subset(df_f1,opt_ipt=="Inpatient"&quadrant=="III"),tapply(population,month,sum))/139653 

#### Figure 1
#######
####

df_f2<-df_f1[,c("month","case","opt_ipt","quadrant")]
df_f2<-aggregate(.~month+opt_ipt+quadrant,data=df_f2,sum,drop=FALSE)
df_f2$case<-ifelse(is.na(df_f2$case),0,df_f2$case)

df_f2$title<-"COVID-19 cases"

df_f3<-read.csv("pop_case_quadrant.csv")
#df_f3<-subset(df_f3,pop==1)
df_f3$opt_ipt<-droplevels(df_f3$opt_ipt)
df_f3$opt_ipt<-factor(df_f3$opt_ipt,levels=c("Outpatient","Inpatient"))
df_f3$month<-factor(df_f3$month,levels=c("May","April","March","February"))

###
f1a_0<-ggplot(subset(df_f1,region!="Shanghai"&region!="Jilin"),
              aes(y=volumn_change, x=moveout_change, color=quadrant)) +
  geom_rect(aes(xmin=-Inf,xmax=0,ymin=-Inf,ymax=0),fill="grey95",color=NA)+
  geom_hline(yintercept = 0,linetype="dashed",color="grey40",size=0.2)+
  geom_vline(xintercept = 0,linetype="dashed",color="grey40",size=0.2)+
  geom_point(aes(size=case,shape=factor(non_zero_case)),alpha=0.60)+
  geom_text(aes(label=region_short),size=2,alpha=0.8,hjust=-0.1,col="grey60")+
  scale_color_manual(values=c("palegreen3","orange2","tomato2","skyblue2"),
                     labels = c("p>=0.05", "p<0.05"),name="Significance")+
  #theme_stata(scheme="s1mono")+
  theme_few()+
  theme(legend.position ="none",
        axis.title=element_text(face="bold"),
        strip.text.x=element_text(face="bold"),
        strip.text.y=element_text(face="bold"))+
  xlim(-100,55)+
  ylim(-100,20)+
  #scale_y_continuous(lim=c(-100,25))+
  facet_grid(month~opt_ipt)+
  #ggtitle(paste("Change in Outpatient, April 2022"))+
  xlab("Change in Move-out Movement Index, %")+
  ylab("Change in volumn, %")+
  scale_shape_manual(values = c(1, 19)) + 
  ggtitle("(A)")


f1a2<-ggplot(subset(df_f1,region=="Shanghai"|region=="Jilin"),
             aes(y=volumn_change, x=moveout_change, color=quadrant)) +
  geom_rect(aes(xmin=-Inf,xmax=0,ymin=-Inf,ymax=0),fill="grey95",color=NA)+
  geom_hline(yintercept = 0,linetype="dashed",color="grey40",size=0.2)+
  geom_vline(xintercept = 0,linetype="dashed",color="grey40",size=0.2)+
  geom_point(aes(size=factor(case)),alpha=0.6)+
  geom_text(aes(label=region_short),size=2,alpha=0.8,hjust=-0.1,color="grey60")+
  scale_color_manual(values=c("palegreen3","orange2","tomato2","skyblue2"),
                     labels = c("p>=0.05", "p<0.05"),name="Significance")+
  #theme_stata(scheme="s1mono")+
  theme_few()+
  theme(legend.position ="none",
        axis.title=element_text(face="bold"),
        strip.text.x=element_text(face="bold"),
        strip.text.y=element_text(face="bold"))+
  xlim(-100,55)+
  ylim(-100,20)+
  facet_grid(month~opt_ipt)+
  #ggtitle(paste("Change in Outpatient, April 2022"))+
  xlab("Change in Move-out index, %")+
  ylab("Change in volumn, %")+
  scale_size_manual(values=c(1,2,4,6,9,11,14,19)) +
  ggtitle("(A)")#

#### Figure 1
#######
####
df_f3$month<-factor(df_f3$month,levels=c("February","March","April","May"))

f2a<-ggplot(df_f3,aes(x=month,y=Proportion,group=metric,shape=metric))+
  geom_hline(yintercept=c(25,50, 75,
                          100),linetype="dotted",color="grey80")+
  geom_point(size=3,color="tomato2",alpha=0.8)+
  geom_line(color="tomato2",alpha=0.8)+
  #coord_flip()+
  theme_few()+
  facet_grid(strip_title2~opt_ipt)+
  theme(legend.position =c(0.9,0.20),
        legend.title = element_blank(),
        #plot.title=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face="bold"),
        axis.text.x =element_blank(),
        strip.text.x=element_text(face="bold"),
        strip.text.y=element_blank(),
        panel.grid.major.y  = element_blank()) +
  # ggtitle("(A)")+
  ylab("Quadrant III, %")+
  scale_y_continuous(
    breaks=c(25,50,75,100),
    labels=c("25","50","75","     100")
  )

+
  scale_y_continuous(#position="right",
    limit=c(4,105),
    breaks=c(12.8641756873934,42.979385, 79.704697,
             100),
    labels=c("5","43","80","100"),
    minor_breaks = c(12.8641756873934,42.979385, 79.704697,
                     92.122618, 100))+
  geom_hline(yintercept=c(12.8641756873934,42.979385, 79.704697,
                          92.122618, 100),linetype="dotted",color="grey80")

table(df_f2$month)

f2b<-ggplot(df_f2,aes(x=month,y=(case+1)^0.5,fill=quadrant))+
  geom_hline(yintercept=c(1000^0.5, 10000^0.5,30000^0.5, 50000^0.5),linetype="dotted",color="grey80")+
  geom_bar(position="dodge",stat="identity",alpha=0.8)+
  theme_few()+
  theme(legend.position =c(0.92,0.72),
        legend.title = element_text(face="bold",size=10),
        #plot.title=element_blank(),
        axis.title=element_text(face="bold"),
        #axis.text.y=element_blank(),
        strip.text.x=element_blank(),
        strip.text.y=element_blank())+
  scale_y_continuous(position="left",limit=c(0,280),breaks=c(0,1000^0.5, 10000^0.5,30000^0.5, 50000^0.5),
                     labels=c("0","1 000","10 000","30 000","50 000"))+
  scale_color_manual(values=c("palegreen3","orange2","tomato2","skyblue2"),
                     labels = c("I","II","III","IV"),name="Qurdrant")+
  scale_fill_manual(values=c("palegreen3","orange2","tomato2","skyblue2"))+
  
  facet_grid(title~opt_ipt)+
  xlab("")+
  ylab("Number of COVID-19 cases")+
  # ggtitle("(B)")+
  guides(fill=guide_legend(title="Quadrant"))

grid.arrange(f2a,f2b,heights=c(1,1.1))
