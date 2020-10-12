
# Data visualization ------------------------------------------------------

rm(list=ls())
library(readr)
dataset_2 <- read_csv("C:/Users/J/Desktop/Course_DataAnalysis/dataset_2.csv")
Topology <-dataset_2$G

# Histogram of ultimate drift
loadfonts(device = "win")

library(ggplot2)
library(extrafont)
ggplot(data=dataset_2, aes(x=dataset_2$T)) + 
  geom_histogram(aes(fill = Topology),breaks=seq(0, 3.5, by=0.5),col="black") + 
  labs(x="Ultimate drift [%]", y="Frequency") + 
 
  
# Example----------------------------------------------------------  
#  theme(axis.text.x = element_text(family="",face="bold", color="#993333", 
#                                   size=14, angle=45),
#        axis.text.y = element_text(face="bold", color="#993333", 
#                                   size=14, angle=45))
#-----------------------------------------------------------------
  theme_linedraw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+  
  theme(axis.text.x = element_text(family="Times New Roman",
                                   size=9, angle=0),
        axis.text.y = element_text(family="Times New Roman", 
                                   size=9, angle=0),
        axis.title.x = element_text(family="Times New Roman", colour="black", size=9),
        axis.title.y = element_text(family="Times New Roman", colour="black", size=9),
        legend.text = element_text(family="Times New Roman",colour="black", size=9),legend.background = element_blank(),
        legend.direction = "vertical",legend.box = "vertical",legend.title = element_text(colour="black", size=9,family="Times New Roman"),
        legend.key.size = unit(0.4,"cm"))+

  scale_x_continuous(breaks=seq(0,3.5,0.5),limits=c(0, 3.5),expand = c(0, 0))+
  scale_y_continuous(breaks=seq(0,35,5),limits=c(0, 35),expand = c(0, 0.05))


ggsave("hist_ultimate_drift.png", plot = last_plot(), device = "png", 
       path = "C:\\Users\\J\\Desktop\\Course_DataAnalysis\\Proposal\\Pictures",
       scale = 1, width = 8, height = 6, units ="cm",
       dpi = 300)

# Histogram of ultimate force
rm(list=ls())
library(readr)
dataset_2 <- read_csv("C:/Users/J/Desktop/Course_DataAnalysis/dataset_2.csv")
Topology <-dataset_2$G

#hist(dataset_2$U)
loadfonts(device = "win")

library(ggplot2)
library(extrafont)
ggplot(data=dataset_2, aes(x=dataset_2$U)) + 
  geom_histogram(aes(fill = Topology),breaks=seq(0, 250, by=50),col="black") + 
  labs(x="Ultimate force [kN]", y="Frequency") +

  
  # Example----------------------------------------------------------  
#  theme(axis.text.x = element_text(family="",face="bold", color="#993333", 
#                                   size=14, angle=45),
#        axis.text.y = element_text(face="bold", color="#993333", 
#                                   size=14, angle=45))
#-----------------------------------------------------------------
theme_linedraw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+  
  theme(axis.text.x = element_text(family="Times New Roman",
                                   size=9, angle=0),
        axis.text.y = element_text(family="Times New Roman", 
                                   size=9, angle=0),
        axis.title.x = element_text(family="Times New Roman", colour="black", size=9),
        axis.title.y = element_text(family="Times New Roman", colour="black", size=9),
        legend.text = element_text(family="Times New Roman",colour="black", size=9),legend.background = element_blank(),
        legend.direction = "vertical",legend.box = "vertical",legend.title = element_text(colour="black", size=9,family="Times New Roman"),
        legend.key.size = unit(0.4,"cm"))+
  
  scale_x_continuous(breaks=seq(0,250,50),limits=c(0, 250),expand = c(0, 0))+
  scale_y_continuous(breaks=seq(0,50,10),limits=c(0, 50),expand = c(0, 0.05))


ggsave("hist_ultimate_force.png", plot = last_plot(), device = "png", 
       path = "C:\\Users\\J\\Desktop\\Course_DataAnalysis\\Proposal\\Pictures",
       scale = 1, width = 8, height = 6, units ="cm",
       dpi = 300)

                 

# model_ultdrift_reduced -----------------------------------------------------------------

rm(list=ls())
library(readr)
dataset_2 <- read_csv("C:/Users/J/Desktop/Course_DataAnalysis/dataset_2.csv")


model_ultdrift_reduced<- lm(T~I+J+K+M+Q,data=dataset_2)
summary(model_ultdrift_reduced)

sink("C:/Users/J/Desktop/Course_DataAnalysis/results/result_model_ultdrift_reduced.txt")
print(summary(model_ultdrift_reduced))
sink()

confint(model_ultdrift_reduced,level=0.95)
plot(model_ultdrift_reduced)

## Plotting the Residuals_model_ultdrift_reduced
res=c(resid(model_ultdrift_reduced))
fitted_values=c(model_ultdrift_reduced[["fitted.values"]])
modeldata_ultdrift_reduced=data.frame(res,fitted_values)

loadfonts(device = "win")

library(ggplot2)
library(extrafont)
font_import()

ggplot(data=modeldata_ultdrift_reduced, aes(x=modeldata_ultdrift_reduced$fitted_values,y=modeldata_ultdrift_reduced$res)) + 
  geom_point(colour = "Black", fill="white", size = 1.5,
                 alpha = 1) + 
  labs(x="Fitted values [%]", y="Residuals") +
  #geom_smooth(se=FALSE)+
  geom_hline(aes(yintercept=0),linetype="dashed", colour='red')+
  
  # Example----------------------------------------------------------  
#  theme(axis.text.x = element_text(family="",face="bold", color="#993333", 
#                                   size=14, angle=45),
#        axis.text.y = element_text(face="bold", color="#993333", 
#                                   size=14, angle=45))
#-----------------------------------------------------------------
theme_linedraw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+  
  theme(axis.text.x = element_text(family="Times New Roman",
                                   size=9, angle=0),
        axis.text.y = element_text(family="Times New Roman", 
                                   size=9, angle=0),
        axis.title.x = element_text(family="Times New Roman", colour="black", size=9),
        axis.title.y = element_text(family="Times New Roman", colour="black", size=9),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))+
  scale_x_continuous(breaks=seq(0,2.5,0.5),limits=c(0, 2.5),expand = c(0, 0.2))+
  scale_y_continuous(breaks=seq(-2,2,0.5),limits=c(-2, 2),expand = c(0, 0.2))

ggsave("reg_model_ultdrift_reduced.png", plot = last_plot(), device = "png", 
       path = "C:\\Users\\J\\Desktop\\Course_DataAnalysis\\Proposal\\Pictures",
       scale = 1, width = 6, height = 6, units ="cm",
       dpi = 300)


## Plotting the QQ plot_model_ultdrift_reduced
res=c(rstandard(model_ultdrift_reduced))
fitted_values=c(model_ultdrift_reduced[["fitted.values"]])
modeldata_ultdrift_reduced=data.frame(res,fitted_values)

loadfonts(device = "win")

library(ggplot2)
library(extrafont)
font_import()

ggplot(data=modeldata_ultdrift_reduced, aes(sample =modeldata_ultdrift_reduced$res)) +
  stat_qq(colour = "Black", fill="white", size = 1.5,
          alpha = 1) + 
 
  labs(x="Theoretical Quantiles", y="Standarized residuals") +
  #geom_smooth(se=FALSE)+

  # Example----------------------------------------------------------  
#  theme(axis.text.x = element_text(family="",face="bold", color="#993333", 
#                                   size=14, angle=45),
#        axis.text.y = element_text(face="bold", color="#993333", 
#                                   size=14, angle=45))
#-----------------------------------------------------------------
theme_linedraw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+  
  theme(axis.text.x = element_text(family="Times New Roman",
                                   size=9, angle=0),
        axis.text.y = element_text(family="Times New Roman", 
                                   size=9, angle=0),
        axis.title.x = element_text(family="Times New Roman", colour="black", size=9),
        axis.title.y = element_text(family="Times New Roman", colour="black", size=9),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))+
  scale_x_continuous(breaks=seq(-3,3,1),limits=c(-3, 3),expand = c(0, 0.2))+
  scale_y_continuous(breaks=seq(-3,3,1),limits=c(-3, 3),expand = c(0, 0.2))


ggsave("qq_model_ultdrift_reduced.png", plot = last_plot(), device = "png", 
       path = "C:\\Users\\J\\Desktop\\Course_DataAnalysis\\Proposal\\Pictures",
       scale = 1, width = 6, height = 6, units ="cm",
       dpi = 300)


# model_ultdrift_reduced [after selection]

rm(list=ls())
library(readr)
dataset_2 <- read_csv("C:/Users/J/Desktop/Course_DataAnalysis/dataset_2.csv")


model_ultdrift_reduced<- lm(T~Q,data=dataset_2)
summary(model_ultdrift_reduced)

sink("C:/Users/J/Desktop/Course_DataAnalysis/Results/result_model_ultdrift_reduced.txt")
print(summary(model_ultdrift_reduced))
sink()

confint(model_ultdrift_reduced,level=0.95)
plot(model_ultdrift_reduced)



# Model_2 -----------------------------------------------------------------

rm(list=ls())
library(readr)
dataset_2 <- read_csv("C:/Users/J/Desktop/Course_DataAnalysis/dataset_2.csv")
model2<- lm(U~I+J+K+M+Q,data=dataset_2)
summary(model2)

sink("C:/Users/J/Desktop/Course_DataAnalysis/Results/result_model2.txt")
print(summary(model2))
sink()

confint(model2,level=0.95)
plot(model2)


# sapply(dataset_2, FUN=typeof)
# sapply(dataset_2, FUN=class)
# Dfactor<- as.factor(dataset_2$D)
# sapply(Dfactor, FUN=class)
# dataset_2$D<-Dfactor
# 
# 
# catE<-cut(E,breaks = c(-1,0.5,1,2,3), labeles=c("0","1","2","3"))
# Efactor<- as.factor(dataset_2$E)





# model_ultdrift_full -----------------------------------------------------------------

rm(list=ls())
library(readr)
dataset_2 <- read_csv("C:/Users/J/Desktop/Course_DataAnalysis/dataset_2.csv")
dataset_2$D<-as.factor(dataset_2$D)
dataset_2$E<-as.factor(dataset_2$E)
dataset_2$G<-as.factor(dataset_2$G)
dataset_2$H<-as.factor(dataset_2$H)

model_ultdrift_full<- lm(T~ E+G+H+I+J+K+M+Q,data=dataset_2)
summary(model_ultdrift_full)

sink("C:/Users/J/Desktop/Course_DataAnalysis/results/result_model_ultdrift_full.txt")
print(summary(model_ultdrift_full))
sink()

confint(model_ultdrift_full,level=0.95)
plot(model_ultdrift_full)

## Plotting the Residuals_model_ultdrift_full
res=c(resid(model_ultdrift_full))
fitted_values=c(model_ultdrift_full[["fitted.values"]])
modeldata_ultdrift_full=data.frame(res,fitted_values)

loadfonts(device = "win")

library(ggplot2)
library(extrafont)
font_import()

ggplot(data=modeldata_ultdrift_full, aes(x=modeldata_ultdrift_full$fitted_values,y=modeldata_ultdrift_full$res)) + 
  
  geom_point(aes(colour = dataset_2$G), size = 1.5)+

  labs(x="Fitted values [%]", y="Residuals") +
  #geom_smooth(se=FALSE)+
  geom_hline(aes(yintercept=0),linetype="dashed", colour='black')+
  
  # Example----------------------------------------------------------  
#  theme(axis.text.x = element_text(family="",face="bold", color="#993333", 
#                                   size=14, angle=45),
#        axis.text.y = element_text(face="bold", color="#993333", 
#                                   size=14, angle=45))
#-----------------------------------------------------------------

theme_linedraw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+  
  theme(axis.text.x = element_text(family="Times New Roman",
                                   size=9, angle=0),
        axis.text.y = element_text(family="Times New Roman", 
                                   size=9, angle=0),
        axis.title.x = element_text(family="Times New Roman", colour="black", size=9),
        axis.title.y = element_text(family="Times New Roman", colour="black", size=9),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),legend.position=c(.3, .88),legend.title=element_blank(),
        legend.text = element_text(family="Times New Roman",colour="black", size=7),legend.background = element_blank(),
        legend.direction = "horizontal",legend.box = "horizontal",legend.key.size = unit(0.4,"cm"))+
      annotate("text", x = 2.4, y = 1.35, label = "Topology",family="Times New Roman", size=3)+
  scale_x_continuous(breaks=seq(-0.5,3,0.5),limits=c(-0.5, 3.0),expand = c(0, 0.2))+
  scale_y_continuous(breaks=seq(-1.5,1.5,0.5),limits=c(-1.5, 1.5),expand = c(0, 0.2))
  

ggsave("reg_model_ultdrift_full.png", plot = last_plot(), device = "png", 
       path = "C:\\Users\\J\\Desktop\\Course_DataAnalysis\\Proposal\\Pictures",
       scale = 1, width = 6, height = 6, units ="cm",
       dpi = 300)


## Plotting the QQ plot_model_ultdrift_full
res=c(rstandard(model_ultdrift_full))
fitted_values=c(model_ultdrift_full[["fitted.values"]])
modeldata_ultdrift_full=data.frame(res,fitted_values)

loadfonts(device = "win")

library(ggplot2)
library(extrafont)
font_import()

ggplot(data=modeldata_ultdrift_full, aes(sample =modeldata_ultdrift_full$res)) +
  stat_qq(colour = "Black", fill="white", size = 1.5,
          alpha = 1) + 
  
  labs(x="Theoretical Quantiles", y="Standarized residuals") +
  #geom_smooth(se=FALSE)+
  
  # Example----------------------------------------------------------  
#  theme(axis.text.x = element_text(family="",face="bold", color="#993333", 
#                                   size=14, angle=45),
#        axis.text.y = element_text(face="bold", color="#993333", 
#                                   size=14, angle=45))
#-----------------------------------------------------------------
theme_linedraw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+  
  theme(axis.text.x = element_text(family="Times New Roman",
                                   size=9, angle=0),
        axis.text.y = element_text(family="Times New Roman", 
                                   size=9, angle=0),
        axis.title.x = element_text(family="Times New Roman", colour="black", size=9),
        axis.title.y = element_text(family="Times New Roman", colour="black", size=9),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))+
  scale_x_continuous(breaks=seq(-3,3,1),limits=c(-3, 3),expand = c(0, 0.2))+
  scale_y_continuous(breaks=seq(-3,3,1),limits=c(-3, 3),expand = c(0, 0.2))


ggsave("qq_model_ultdrift_full.png", plot = last_plot(), device = "png", 
       path = "C:\\Users\\J\\Desktop\\Course_DataAnalysis\\Proposal\\Pictures",
       scale = 1, width = 6, height = 6, units ="cm",
       dpi = 300)



# Model selection for ultimate drift_F-test -------------------------------
rm(list=ls())
library(readr)
dataset_2 <- read_csv("C:/Users/J/Desktop/Course_DataAnalysis/dataset_2.csv")
dataset_2$D<-as.factor(dataset_2$D)
dataset_2$E<-as.factor(dataset_2$E)
dataset_2$G<-as.factor(dataset_2$G)
dataset_2$H<-as.factor(dataset_2$H)

model_ultdrift_reduced<- lm(T~I+J+K+M+Q,data=dataset_2)
model_ultdrift_full<- lm(T~ E+G+H+I+J+K+M+Q,data=dataset_2)


anova(model_ultdrift_reduced,model_ultdrift_full)

# Model-selection for ultimate drift (backward+forward+stepwise)
rm(list=ls())
library(readr)
dataset_2 <- read_csv("C:/Users/J/Desktop/Course_DataAnalysis/dataset_2.csv")
E=dataset_2$E
G=dataset_2$G
H=dataset_2$H
I=dataset_2$I
J=dataset_2$J
K=dataset_2$K
M=dataset_2$M
Q=dataset_2$Q
T=dataset_2$T
dataset_2=data.frame(E,G,H,I,J,K,M,Q,T)

dataset_2$E<-as.factor(dataset_2$E)
dataset_2$G<-as.factor(dataset_2$G)
dataset_2$H<-as.factor(dataset_2$H)

fitstart_ultdrift_full<- lm(T~ 1,data=dataset_2)
fitall_ultdrift_full<- lm(T~ E+G+H+I+J+K+M+Q,data=dataset_2)
step(model_ultdrift_full, direction="backward")
step(fitstart_ultdrift_full, direction="forward", scope=formula(fitall_ultdrift_full))
step(fitstart_ultdrift_full, direction="both", scope=formula(fitall_ultdrift_full))


model_ultdrift_full_stepwise<- lm(formula = T ~ E + Q + J, data = dataset_2)
summary(model_ultdrift_full_stepwise)


plot(model_ultdrift_full_stepwise)

#Ploting final proposed model

res=c(rstandard(model_ultdrift_full_stepwise))
fitted_values=c(model_ultdrift_full_stepwise[["fitted.values"]])
modeldata_ultdrift_full_final=data.frame(res,fitted_values)

loadfonts(device = "win")

library(ggplot2)
library(extrafont)
font_import()

ggplot(data=modeldata_ultdrift_full_final, aes(x=modeldata_ultdrift_full_final$fitted_values,y=modeldata_ultdrift_full_final$res)) + 
  
  geom_point(aes(colour = dataset_2$G), size = 1.5)+
  
  labs(x="Fitted values [%]", y="Residuals") +
  #geom_smooth(se=FALSE)+
  geom_hline(aes(yintercept=0),linetype="dashed", colour='black')+
  
  # Example----------------------------------------------------------  
#  theme(axis.text.x = element_text(family="",face="bold", color="#993333", 
#                                   size=14, angle=45),
#        axis.text.y = element_text(face="bold", color="#993333", 
#                                   size=14, angle=45))
#-----------------------------------------------------------------

theme_linedraw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+  
  theme(axis.text.x = element_text(family="Times New Roman",
                                   size=9, angle=0),
        axis.text.y = element_text(family="Times New Roman", 
                                   size=9, angle=0),
        axis.title.x = element_text(family="Times New Roman", colour="black", size=9),
        axis.title.y = element_text(family="Times New Roman", colour="black", size=9),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),legend.position=c(.3, .88),legend.title=element_blank(),
        legend.text = element_text(family="Times New Roman",colour="black", size=7),legend.background = element_blank(),
        legend.direction = "horizontal",legend.box = "horizontal",legend.key.size = unit(0.4,"cm"))+
  annotate("text", x = 2.4, y = 1.35, label = "Topology",family="Times New Roman", size=3)+
  scale_x_continuous(breaks=seq(0,3,0.5),limits=c(-0.5, 3.0),expand = c(0, 0.2))+
  scale_y_continuous(breaks=seq(-1.5,1.5,0.5),limits=c(-1.5, 1.5),expand = c(0, 0.2))


ggsave("reg_model_ultdrift_full_final.png", plot = last_plot(), device = "png", 
       path = "C:\\Users\\J\\Desktop\\Course_DataAnalysis\\Proposal\\Pictures",
       scale = 1, width = 6, height = 6, units ="cm",
       dpi = 300)


# Model_4 -----------------------------------------------------------------

rm(list=ls())
library(readr)
dataset_2 <- read_csv("C:/Users/J/Desktop/Course_DataAnalysis/dataset_2.csv")
dataset_2$D<-as.factor(dataset_2$D)
dataset_2$E<-as.factor(dataset_2$E)
dataset_2$G<-as.factor(dataset_2$G)
dataset_2$H<-as.factor(dataset_2$H)

model4<- lm(U~ E+G+H+I+J+K+M+Q,data=dataset_2)
summary(model4)

sink("C:/Users/J/Desktop/Course_DataAnalysis/Results/result_model4.txt")
print(summary(model4))
sink()

confint(model4,level=0.95)
plot(model4)

