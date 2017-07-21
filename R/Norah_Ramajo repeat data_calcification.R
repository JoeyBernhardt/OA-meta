library(metafor)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)



calcification <- read_csv("data-raw/calcification.csv")
# meta.factorial.repeat.calcification<- read.csv(file.choose())
#readin June long dataset
meta.factorial.repeat.calcification<- calcification

head(meta.factorial.repeat.calcification)
length(meta.factorial.repeat.calcification$study)
View(meta.factorial.repeat.calcification)


#### 
#overall effect of co2
#Lnrr Food, log (1/2(mean high food + mean high food, CO2)/)
head(meta.factorial.repeat.calcification.highfood)

meta.factorial.repeat.calcification.lowstarve<-meta.factorial.repeat.calcification[meta.factorial.repeat.calcification$Food.supply=="High", ]
length(meta.factorial.repeat.calcification.lowstarve$Mean)
meta.factorial.repeat.calcification.highstarve<-meta.factorial.repeat.calcification[meta.factorial.repeat.calcification$Food.supply=="Low", ]
length(meta.factorial.repeat.calcification.highstarve$Mean)
meta.factorial.repeat.calcification.highco2<-meta.factorial.repeat.calcification[meta.factorial.repeat.calcification$CO2=="Elevated", ]
length(meta.factorial.repeat.calcification.highco2$Mean)
meta.factorial.repeat.calcification.lowco2<-meta.factorial.repeat.calcification[meta.factorial.repeat.calcification$CO2=="Ambient", ]
length(meta.factorial.repeat.calcification.lowco2$Mean)

meta.factorial.repeat.calcification.ambienthigh<-meta.factorial.repeat.calcification[meta.factorial.repeat.calcification$Treatment=="AmbientHigh", ]
length(meta.factorial.repeat.calcification.ambienthigh$Mean)
meta.factorial.repeat.calcification.ambientstarve<-meta.factorial.repeat.calcification[meta.factorial.repeat.calcification$Treatment=="AmbientLow", ]
length(meta.factorial.repeat.calcification.ambientstarve$Mean)

meta.factorial.repeat.calcification.elevatedhigh<-meta.factorial.repeat.calcification[meta.factorial.repeat.calcification$Treatment=="ElevatedHigh", ]
length(meta.factorial.repeat.calcification.elevatedhigh$Mean)

meta.factorial.repeat.calcification.elevatedstarve<-meta.factorial.repeat.calcification[meta.factorial.repeat.calcification$Treatment=="ElevatedLow", ]
length(meta.factorial.repeat.calcification.elevatedstarve$Mean)

meta.factorial.repeat.calcification2<-meta.factorial.repeat.calcification.highstarve[,1:17]
head(meta.factorial.repeat.calcification2)




###LnRR calculations, from Morris appendix B, http://onlinelibrary.wiley.com/doi/10.1890/06-0442/full
#https://figshare.com/collections/DIRECT_AND_INTERACTIVE_EFFECTS_OF_ENEMIES_AND_MUTUALISTS_ON_PLANT_PERFORMANCE_A_META-ANALYSIS/3299699

#### Individual effects ... 

##make a vector placeholder
meta.factorial.repeat.calcification2.ind<-meta.factorial.repeat.calcification.highco2[,1:17]
head(meta.factorial.repeat.calcification2.ind)


meta.factorial.repeat.calcification2.ind$LnRR.starve.ind<-log(meta.factorial.repeat.calcification.highstarve$Mean)-log(meta.factorial.repeat.calcification.lowstarve$Mean) 
length(meta.factorial.repeat.calcification.highstarve$Mean)
length(meta.factorial.repeat.calcification.lowstarve$Mean)


meta.factorial.repeat.calcification2.ind$LnRR.co2.ind<-log(meta.factorial.repeat.calcification.highco2$Mean)-log(meta.factorial.repeat.calcification.lowco2$Mean)
length(meta.factorial.repeat.calcification.highco2$Mean)
length(meta.factorial.repeat.calcification.lowco2$Mean)

meta.factorial.repeat.calcification2.ind$LnRR.s.starve.ind<-((meta.factorial.repeat.calcification.highstarve$SD^2)/((meta.factorial.repeat.calcification.highstarve$Mean^2)*(meta.factorial.repeat.calcification.highstarve$N)))+((meta.factorial.repeat.calcification.lowstarve$SD^2)/((meta.factorial.repeat.calcification.lowstarve$Mean^2)*(meta.factorial.repeat.calcification.lowstarve$N)))
meta.factorial.repeat.calcification2.ind$LnRR.s.co2.ind<-((meta.factorial.repeat.calcification.highco2$SD^2)/((meta.factorial.repeat.calcification.highco2$Mean^2)*(meta.factorial.repeat.calcification.highco2$N)))+((meta.factorial.repeat.calcification.lowco2$SD^2)/((meta.factorial.repeat.calcification.lowco2$Mean^2)*(meta.factorial.repeat.calcification.lowco2$N)))

head(meta.factorial.repeat.calcification2.ind)
######

#### problem: Morris calculates as ln (ambientstarve + elevated starve) but Hawkes and Sullivan and Gruner calculate as ln (ambient stave + ln)eleveated starev 
# which is the equivalent to ln(ambient starev *elevated starve)....
#VERY DIFFERENT
#use Morris - see Lajeuness.... 

meta.factorial.repeat.calcification2$LnRR.starve<-log(meta.factorial.repeat.calcification.ambientstarve$Mean+meta.factorial.repeat.calcification.elevatedstarve$Mean)-log(meta.factorial.repeat.calcification.ambienthigh$Mean+meta.factorial.repeat.calcification.elevatedhigh$Mean)
meta.factorial.repeat.calcification2$LnRR.co2<-log(meta.factorial.repeat.calcification.elevatedstarve$Mean+meta.factorial.repeat.calcification.elevatedhigh$Mean)-log(meta.factorial.repeat.calcification.ambienthigh$Mean+meta.factorial.repeat.calcification.ambientstarve$Mean)
meta.factorial.repeat.calcification2$LnRR.interaction<-log(meta.factorial.repeat.calcification.elevatedstarve$Mean)-log(meta.factorial.repeat.calcification.ambientstarve$Mean)-log(meta.factorial.repeat.calcification.elevatedhigh$Mean)+log(meta.factorial.repeat.calcification.ambienthigh$Mean)

#### sampling variance, from Morris appendix B

meta.factorial.repeat.calcification2$s.starve<-(((1/(meta.factorial.repeat.calcification.ambientstarve$Mean+meta.factorial.repeat.calcification.elevatedstarve$Mean))^2)*(((meta.factorial.repeat.calcification.ambientstarve$SD^2)/(meta.factorial.repeat.calcification.ambientstarve$N))+((meta.factorial.repeat.calcification.elevatedstarve$SD^2)/(meta.factorial.repeat.calcification.elevatedstarve$N))))+ (((1/(meta.factorial.repeat.calcification.ambienthigh$Mean+meta.factorial.repeat.calcification.elevatedhigh$Mean))^2)*(((meta.factorial.repeat.calcification.ambienthigh$SD^2)/(meta.factorial.repeat.calcification.ambienthigh$N))+((meta.factorial.repeat.calcification.elevatedhigh$SD^2)/(meta.factorial.repeat.calcification.elevatedhigh$N))))                                                                                                                                                                                                      


meta.factorial.repeat.calcification2$s.co2<-(((1/(meta.factorial.repeat.calcification.elevatedstarve$Mean+meta.factorial.repeat.calcification.elevatedhigh$Mean))^2)*(((meta.factorial.repeat.calcification.elevatedhigh$SD^2)/(meta.factorial.repeat.calcification.elevatedhigh$N))+((meta.factorial.repeat.calcification.elevatedstarve$SD^2)/(meta.factorial.repeat.calcification.elevatedstarve$N))))+ (((1/(meta.factorial.repeat.calcification.ambienthigh$Mean+meta.factorial.repeat.calcification.ambientstarve$Mean))^2)*(((meta.factorial.repeat.calcification.ambienthigh$SD^2)/(meta.factorial.repeat.calcification.ambienthigh$N))+((meta.factorial.repeat.calcification.ambientstarve$SD^2)/(meta.factorial.repeat.calcification.ambientstarve$N))))                                                                                                                                                                                                      


meta.factorial.repeat.calcification2$s.interaction<-((meta.factorial.repeat.calcification.elevatedhigh$SD^2)/((meta.factorial.repeat.calcification.elevatedhigh$Mean^2)*(meta.factorial.repeat.calcification.elevatedhigh$N)))+ ((meta.factorial.repeat.calcification.elevatedstarve$SD^2)/((meta.factorial.repeat.calcification.elevatedstarve$Mean^2)*(meta.factorial.repeat.calcification.elevatedstarve$N)))+ ((meta.factorial.repeat.calcification.ambienthigh$SD^2)/((meta.factorial.repeat.calcification.ambienthigh$Mean^2)*(meta.factorial.repeat.calcification.ambienthigh$N)))+ ((meta.factorial.repeat.calcification.ambientstarve$SD^2)/((meta.factorial.repeat.calcification.ambientstarve$Mean^2)*(meta.factorial.repeat.calcification.ambientstarve$N)))



head(meta.factorial.repeat.calcification2)



meta.factorial.repeat.calcification2_long <- gather(meta.factorial.repeat.calcification2, LnRR.type, LnRR, c(18:20), factor_key=TRUE)
head(meta.factorial.repeat.calcification2_long)
meta.factorial.repeat.calcification2_long_s <- gather(meta.factorial.repeat.calcification2, s.type, s, c(21:23), factor_key=TRUE)
head(meta.factorial.repeat.calcification2_long_s)

meta.factorial.repeat.calcification2_long_bind<-as.data.frame(cbind(meta.factorial.repeat.calcification2_long,meta.factorial.repeat.calcification2_long_s[, 21:22] ))
head(meta.factorial.repeat.calcification2_long_bind)

####individual
head(meta.factorial.repeat.calcification2.ind)

meta.factorial.repeat.calcification2_long_ind <- gather(meta.factorial.repeat.calcification2.ind, LnRR.type, LnRR, c(18:19), factor_key=TRUE)
head(meta.factorial.repeat.calcification2_long_ind)
meta.factorial.repeat.calcification2_long_s_ind <- gather(meta.factorial.repeat.calcification2.ind, s.type, s, c(20:21), factor_key=TRUE)
head(meta.factorial.repeat.calcification2_long_s_ind)

meta.factorial.repeat.calcification2_long_bind_ind<-as.data.frame(cbind(meta.factorial.repeat.calcification2_long_ind,meta.factorial.repeat.calcification2_long_s_ind[, 20:21] ))
View(meta.factorial.repeat.calcification2_long_bind_ind)

cbbPalette.black<-c("#000000", "#000000", "#000000", "#000000", "#000000")



cdata.LRR.factorial.repeat.calc<-ddply(na.omit(meta.factorial.repeat.calcification2_long_bind),~LnRR.type,summarise,mean=mean(LnRR), sd=mean(s))
head(cdata.LRR.factorial.repeat.calc)


cdata.LRR.factorial.repeat.ind.calc<-ddply(na.omit(meta.factorial.repeat.calcification2_long_bind_ind),~LnRR.type,summarise,mean=mean(LnRR), sd=mean(s))
head(cdata.LRR.factorial.repeat.ind.calc)

cdata.LRR.factorial.repeat.bind.calc<-rbind(cdata.LRR.factorial.repeat.ind.calc, cdata.LRR.factorial.repeat.calc)


plot.factorial.repeat.calc <-ggplot(cdata.LRR.factorial.repeat.bind.calc,aes(x=LnRR.type,y=mean,ymax=(mean+(1.96*sd)),ymin=(mean-(1.96*sd)),size=2, col=LnRR.type, shape=LnRR.type))+ scale_colour_manual(values=cbbPalette.black)+ scale_shape_manual(values=c(19,19, 19, 19, 19))
plot.factorial.repeat.calc <-plot.factorial.repeat.calc +geom_pointrange(size=1)
plot.factorial.repeat.calc <-plot.factorial.repeat.calc  #+coord_flip()
plot.factorial.repeat.calc <-plot.factorial.repeat.calc +geom_hline(aes(x=0, yintercept=0), lty=2,size=1)+ylim(-2,1)
plot.factorial.repeat.calc <-plot.factorial.repeat.calc +theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+theme(axis.ticks = element_blank(), axis.text.x = element_blank())+ theme_bw()+theme(legend.position = "none")
plot.factorial.repeat.calc <-plot.factorial.repeat.calc + xlab('') +ylab (expression("LnRR calcification"))
plot.factorial.repeat.calc <-plot.factorial.repeat.calc +theme(axis.text.x = element_text(size = 16, colour = 'black')) +theme(axis.text.y = element_text(size = 16, colour = 'black'))
plot.factorial.repeat.calc <-plot.factorial.repeat.calc +theme(axis.title.x = element_text(size = 20, colour = 'black'))+theme(axis.title.y = element_text(size = 20, colour = 'black'))
plot.factorial.repeat.calc 


plot_grid(plot.mod.factorial.repeat.repeat.exact.calc, plot.mod.factorial.repeat.repeat.calc, plot.mod.factorial.repeat.calc  , ncol=3, align='h')


### write out as csv

write_csv(meta.factorial.repeat.calcification2_long_bind, "data-processed/calcification_ES.csv")



### model
head(meta.factorial.repeat.calcification2_long_bind_ind)
mod.overall.factorial.repeat.calcification.ind<- rma.mv(LnRR, s, mods = ~factor(LnRR.type) - 1,random= ~LnRR.type|Paper_no, data=meta.factorial.repeat.calcification2_long_bind_ind)
mod.overall.factorial.repeat.calcification<- rma.mv(LnRR, s, mods = ~factor(LnRR.type) - 1,random= ~LnRR.type|Paper_no, data=meta.factorial.repeat.calcification2_long_bind)


summary(mod.overall.factorial.repeat.calcification)
cdata.LRR.factorial.repeat.bind.calc
cdata.LRR.factorial.repeat.bind.calc$estimate<-rbind(mod.overall.factorial.repeat.calcification.ind$b, mod.overall.factorial.repeat.calcification$b)
cdata.LRR.factorial.repeat.bind.calc$ci.lb<-rbind(mod.overall.factorial.repeat.calcification.ind$ci.lb[[1]][1], mod.overall.factorial.repeat.calcification.ind$ci.lb[[2]][1], mod.overall.factorial.repeat.calcification$ci.lb[[1]][1], mod.overall.factorial.repeat.calcification$ci.lb[[2]][1], mod.overall.factorial.repeat.calcification$ci.lb[[3]][1])
cdata.LRR.factorial.repeat.bind.calc$ci.ub<-rbind(mod.overall.factorial.repeat.calcification.ind$ci.ub[[1]][1], mod.overall.factorial.repeat.calcification.ind$ci.ub[[2]][1], mod.overall.factorial.repeat.calcification$ci.ub[[1]][1], mod.overall.factorial.repeat.calcification$ci.ub[[2]][1], mod.overall.factorial.repeat.calcification$ci.ub[[3]][1])

mod.overall.factorial.repeat.calcification.ind$b
mod.overall.factorial.repeat.calcification.ind$ci.ub[[2]][1]


######## Model can estimate

plot.mod.factorial.repeat.calcification <-ggplot(cdata.LRR.factorial.repeat.bind.calc,aes(x=LnRR.type,y=estimate,ymax=(ci.ub),ymin=(ci.lb),size=2, col=LnRR.type, shape=LnRR.type))+ scale_colour_manual(values=cbbPalette.black)+ scale_shape_manual(values=c(19,19, 19, 19, 19))
plot.mod.factorial.repeat.calcification <-plot.mod.factorial.repeat.calcification +geom_pointrange(size=1)
plot.mod.factorial.repeat.calcification <-plot.mod.factorial.repeat.calcification  #+coord_flip()
plot.mod.factorial.repeat.calcification <-plot.mod.factorial.repeat.calcification +geom_hline(aes(x=0, yintercept=0), lty=2,size=1)+ ylim(-1.5, 0.5)
plot.mod.factorial.repeat.calcification <-plot.mod.factorial.repeat.calcification +theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+theme(axis.ticks = element_blank(), axis.text.x = element_blank())+ theme_bw()+theme(legend.position = "none")
plot.mod.factorial.repeat.calcification <-plot.mod.factorial.repeat.calcification + xlab('') +ylab (expression("LnRR calcification"))
plot.mod.factorial.repeat.calcification <-plot.mod.factorial.repeat.calcification +theme(axis.text.x = element_text(size = 16, colour = 'black')) +theme(axis.text.y = element_text(size = 16, colour = 'black'))
plot.mod.factorial.repeat.calcification <-plot.mod.factorial.repeat.calcification +theme(axis.title.x = element_text(size = 20, colour = 'black'))+theme(axis.title.y = element_text(size = 20, colour = 'black'))
plot.mod.factorial.repeat.calcification 



### plot all together.... 
require(cowplot)
theme_set(theme_classic())

plot_grid(plot.mod.factorial.repeat.calcification, plot.mod.factorial.repeat.calcification, plot.mod.factorial.repeat,plot.mod.factorial.repeat  , ncol=2, align='h')


plot_grid(plot.mod.factorial.repeat.exact.boot.calc , plot.mod.factorial.repeat.boot.calc, plot.mod.factorial.repeat.exact.boot, plot.mod.factorial.repeat.boot  , ncol=2, align='h', labels=c('(a)', '(b)', '(c)', '(d)'))



#### bootstraping

boot.func.starve.ind <- function(dat, indices) {
  
  res <- try(rma(LnRR.starve.ind, LnRR.s.starve.ind, data=dat, subset=indices), silent=TRUE)
  
  if (is.element("try-error", class(res))) {
    NA
  } else {
    c(coef(res), vcov(res), res$tau2, res$se.tau2^2)
  }
  
}


boot.func.co2.ind <- function(dat, indices) {
  
  res <- try(rma(LnRR.co2.ind, LnRR.s.co2.ind, data=dat, subset=indices), silent=TRUE)
  
  if (is.element("try-error", class(res))) {
    NA
  } else {
    c(coef(res), vcov(res), res$tau2, res$se.tau2^2)
  }
  
}

boot.func.starve <- function(dat, indices) {
  
  res <- try(rma(LnRR.starve, s.starve, data=dat, subset=indices), silent=TRUE)
  
  if (is.element("try-error", class(res))) {
    NA
  } else {
    c(coef(res), vcov(res), res$tau2, res$se.tau2^2)
  }
  
}


boot.func.co2 <- function(dat, indices) {
  
  res <- try(rma(LnRR.co2, s.co2, data=dat, subset=indices), silent=TRUE)
  
  if (is.element("try-error", class(res))) {
    NA
  } else {
    c(coef(res), vcov(res), res$tau2, res$se.tau2^2)
  }
  
}
boot.func.interaction <- function(dat, indices) {
  
  res <- try(rma(LnRR.interaction, s.interaction, data=dat, subset=indices), silent=TRUE)
  
  if (is.element("try-error", class(res))) {
    NA
  } else {
    c(coef(res), vcov(res), res$tau2, res$se.tau2^2)
  }
  
}
head(meta.factorial.repeat.calcification2)
head(meta.factorial.repeat.calcification2.ind)


head(meta.factorial.repeat.calcification.lowstarve)
library(boot)
res.boot.meta.factorial.repeat.calcification.starve<- boot(meta.factorial.repeat.calcification2, boot.func.starve, R=10000)
res.boot.meta.factorial.repeat.calcification.starve.cis<-boot.ci(res.boot.meta.factorial.repeat.calcification.starve,index=1:2)
res.boot.meta.factorial.repeat.calcification.co2<- boot(meta.factorial.repeat.calcification2, boot.func.co2, R=10000)
res.boot.meta.factorial.repeat.calcification.co2.cis<-boot.ci(res.boot.meta.factorial.repeat.calcification.co2,index=1:2)
res.boot.meta.factorial.repeat.calcification.interaction<- boot(meta.factorial.repeat.calcification2, boot.func.interaction, R=10000)
res.boot.meta.factorial.repeat.calcification.interaction.cis<-boot.ci(res.boot.meta.factorial.repeat.calcification.interaction,index=1:2)

res.boot.meta.factorial.repeat.calcification.starve.ind<- boot(meta.factorial.repeat.calcification2.ind, boot.func.starve.ind, R=10000)
res.boot.meta.factorial.repeat.calcification.starve.ind.cis<-boot.ci(res.boot.meta.factorial.repeat.calcification.starve.ind,index=1:2)
res.boot.meta.factorial.repeat.calcification.co2.ind<- boot(meta.factorial.repeat.calcification2.ind, boot.func.co2.ind, R=10000)
res.boot.meta.factorial.repeat.calcification.co2.ind.cis<-boot.ci(res.boot.meta.factorial.repeat.calcification.co2.ind,index=1:2)


cdata.LRR.factorial.repeat.bind.calc
cdata.LRR.factorial.repeat.bind.calc$lower.calc.factorial <- rbind((res.boot.meta.factorial.repeat.calcification.starve.ind.cis[[8]][4]), (res.boot.meta.factorial.repeat.calcification.co2.ind.cis[[8]][4]),  (res.boot.meta.factorial.repeat.calcification.starve.cis[[8]][4]),  (res.boot.meta.factorial.repeat.calcification.co2.cis[[8]][4]),  (res.boot.meta.factorial.repeat.calcification.interaction.cis[[8]][4]))
cdata.LRR.factorial.repeat.bind.calc$upper.calc.factorial <-rbind((res.boot.meta.factorial.repeat.calcification.starve.ind.cis[[8]][5]), (res.boot.meta.factorial.repeat.calcification.co2.ind.cis[[8]][5]),  (res.boot.meta.factorial.repeat.calcification.starve.cis[[8]][5]),  (res.boot.meta.factorial.repeat.calcification.co2.cis[[8]][5]),  (res.boot.meta.factorial.repeat.calcification.interaction.cis[[8]][5]))
cdata.LRR.factorial.repeat.bind.calc$mean.scaled.calcification<-rbind((res.boot.meta.factorial.repeat.calcification.starve.ind.cis$t0), (res.boot.meta.factorial.repeat.calcification.co2.ind.cis$t0),  (res.boot.meta.factorial.repeat.calcification.starve.cis$t0),  (res.boot.meta.factorial.repeat.calcification.co2.cis$t0),  (res.boot.meta.factorial.repeat.calcification.interaction.cis$t0))



###### 
plot.mod.factorial.repeat.boot.calc <-ggplot(cdata.LRR.factorial.repeat.bind.calc ,aes(x=LnRR.type,y=mean.scaled.calcification,ymax=(upper.calc.factorial),ymin=(lower.calc.factorial)),size=2, col=LnRR.type, shape=LnRR.type)+ scale_colour_manual(values=cbbPalette.black)+ scale_shape_manual(values=c(19,19, 19, 19, 19))
plot.mod.factorial.repeat.boot.calc  <-plot.mod.factorial.repeat.boot.calc  +geom_pointrange(size=1)
plot.mod.factorial.repeat.boot.calc  <-plot.mod.factorial.repeat.boot.calc   #+coord_flip()
plot.mod.factorial.repeat.boot.calc  <-plot.mod.factorial.repeat.boot.calc  +geom_hline(aes(x=0, yintercept=0), lty=2,size=1) + ylim(-1.5, 0.5)
plot.mod.factorial.repeat.boot.calc  <-plot.mod.factorial.repeat.boot.calc  +theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+theme(axis.ticks = element_blank(), axis.text.x = element_blank())+ theme_bw()+theme(legend.position = "none")
plot.mod.factorial.repeat.boot.calc  <-plot.mod.factorial.repeat.boot.calc  + xlab('') +ylab (expression("LnRR calcification"))
plot.mod.factorial.repeat.boot.calc  <-plot.mod.factorial.repeat.boot.calc  +theme(axis.text.x = element_text(size = 16, colour = 'black')) +theme(axis.text.y = element_text(size = 16, colour = 'black'))
plot.mod.factorial.repeat.boot.calc  <-plot.mod.factorial.repeat.boot.calc  +theme(axis.title.x = element_text(size = 20, colour = 'black'))+theme(axis.title.y = element_text(size = 20, colour = 'black'))
plot.mod.factorial.repeat.boot.calc  

plot.mod.factorial.repeat















### small sample size correction (using weights in Hedges 1999)
### might need to use a for loop?? 
#Use the calculation in Lajeunesse 2011:
#https://climatechangeresponses.biomedcentral.com/articles/10.1186/s40665-014-0008-y
#### have some code but don't have the function Q(w, r).... can't find that in r?? 
### ias Q just a linear model??? 

meta.factorial.repeat.calcification2$s.starve<-(((1/(meta.factorial.repeat.calcification.ambientstarve$Mean+meta.factorial.repeat.calcification.elevatedstarve$Mean))^2)*(((meta.factorial.repeat.calcification.ambientstarve$SD^2)/(meta.factorial.repeat.calcification.ambientstarve$N))+((meta.factorial.repeat.calcification.elevatedstarve$SD^2)/(meta.factorial.repeat.calcification.elevatedstarve$N))))+ (((1/(meta.factorial.repeat.calcification.ambienthigh$Mean+meta.factorial.repeat.calcification.elevatedhigh$Mean))^2)*(((meta.factorial.repeat.calcification.ambienthigh$SD^2)/(meta.factorial.repeat.calcification.ambienthigh$N))+((meta.factorial.repeat.calcification.elevatedhigh$SD^2)/(meta.factorial.repeat.calcification.elevatedhigh$N))))                                                                                                                                                                                                      
dfi = 36+36 - 2
dfi


W=1/meta.factorial.repeat.calcification2$s.starve;                # weights are inverse of sampling variance
SW = sum(W);            # sum of weights
k = length(meta.factorial.repeat.calcification2$LnRR.starve)-1;

Qw = lm(meta.factorial.repeat.calcification2$LnRR.starve~W)
Qw
Wstar = ( W^-1 + S2b )^-1
S2b = ( -9.692e-01 - k )/(SW - 2.237e-05 *W/SW); 
Wstar = ( W^-1 + S2b )^-1;
SWstar = sum(Wstar)


meta.factorial.repeat.calcification2$s.starve.SE = ( (1/SWstar) * ( 1 + 4*sum( (1/dfi) * ((Wstar/W)^2) * (Wstar*(SWstar-Wstar)/(SWstar^2) ) ) ) )^.5; # H et al eq. 7

##### not sure why it doesn't work... 