
# Data visualization ------------------------------------------------------

rm(list=ls())
library(readr)
dataset_2 <- read_csv("C:/Users/J/Desktop/Course_DataAnalysis/dataset_2.csv")
Topology <-dataset_2$G



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



# model_ultdforce_reduced -----------------------------------------------------------------

rm(list=ls())
library(readr)
dataset_2 <- read_csv("C:/Users/J/Desktop/Course_DataAnalysis/dataset_2.csv")


model_ultforce_reduced<- lm(U~I+J+K+M+Q,data=dataset_2)
summary(model_ultforce_reduced)

sink("C:/Users/J/Desktop/Course_DataAnalysis/results/result_model_ultforce_reduced.txt")
print(summary(model_ultforce_reduced))
sink()

confint(model_ultforce_reduced,level=0.95)
plot(model_ultforce_reduced)


# Plotting the Residuals_model_ultforce_reduced ---------------------------

res=c(resid(model_ultforce_reduced))
fitted_values=c(model_ultforce_reduced[["fitted.values"]])
modeldata_ultforce_reduced=data.frame(res,fitted_values)

loadfonts(device = "win")

library(ggplot2)
library(extrafont)
font_import()

ggplot(data=modeldata_ultforce_reduced, aes(x=modeldata_ultforce_reduced$fitted_values,y=modeldata_ultforce_reduced$res)) + 
  geom_point(colour = "black", fill="white", size = 1.5,
             alpha = 1) + 
  labs(x="Fitted values [kN]", y="Residuals") +
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
  scale_x_continuous(breaks=c(50,70,90,110,130,150),limits=c(50, 160),expand = c(0, 0.2))+
  scale_y_continuous(breaks=seq(-160,160,40),limits=c(-160, 160),expand = c(0, 0.2))

ggsave("reg_model_ultforce_reduced.png", plot = last_plot(), device = "png", 
       path = "C:\\Users\\J\\Desktop\\Course_DataAnalysis\\Proposal\\Pictures",
       scale = 1, width = 6, height = 6, units ="cm",
       dpi = 300)



# Plotting the QQ plot_model_ultforce_reduced -----------------------------

res <-c(rstandard(model_ultforce_reduced))
fitted_values <-c(model_ultforce_reduced[["fitted.values"]])
modeldata_ultforce_reduced <-data.frame(res,fitted_values)
stdresiduals <- rstandard(model_ultforce_reduced)



loadfonts(device = "win")

library(ggplot2)
library(extrafont)
font_import()

ggplot(data=modeldata_ultforce_reduced, aes(sample= res)) +
  stat_qq(colour ="black" , fill="white", size = 1.5,
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
  scale_y_continuous(breaks=seq(-4,4,1),limits=c(-4, 4),expand = c(0, 0.2))


ggsave("qq_model_ultforce_reduced.png", plot = last_plot(), device = "png", 
       path = "C:\\Users\\J\\Desktop\\Course_DataAnalysis\\Proposal\\Pictures",
       scale = 1, width = 6, height = 6, units ="cm",
       dpi = 300)






# model_ultforce_full -----------------------------------------------------------------

rm(list=ls())
library(readr)
dataset_2 <- read_csv("C:/Users/J/Desktop/Course_DataAnalysis/dataset_2.csv")
dataset_2$D<-as.factor(dataset_2$D)
dataset_2$E<-as.factor(dataset_2$E)
dataset_2$G<-as.factor(dataset_2$G)
dataset_2$H<-as.factor(dataset_2$H)

model_ultforce_full<- lm(U~ E+G+H+I+J+K+M+Q,data=dataset_2)
summary(model_ultforce_full)

sink("C:/Users/J/Desktop/Course_DataAnalysis/results/result_model_ultforce_full.txt")
print(summary(model_ultforce_full))
sink()

confint(model_ultforce_full,level=0.95)
plot(model_ultforce_full)

## Plotting the Residuals_model_ultforce_full
res=c(resid(model_ultforce_full))
fitted_values=c(model_ultforce_full[["fitted.values"]])
modeldata_ultforce_full=data.frame(res,fitted_values)

loadfonts(device = "win")

library(ggplot2)
library(extrafont)
font_import()

ggplot(data=modeldata_ultforce_full, aes(x=modeldata_ultforce_full$fitted_values,y=modeldata_ultforce_full$res)) + 
  
  geom_point(aes(colour = dataset_2$G), size = 1.5)+
  
  labs(x="Fitted values [kN]", y="Residuals") +
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
        annotate("text", x = 160, y = 120, label = "Topology",family="Times New Roman", size=3)+
  
  scale_x_continuous(breaks=seq(0,200,50),limits=c(0, 200),expand = c(0, 0.2))+
  scale_y_continuous(breaks=seq(-150,150,50),limits=c(-150, 150),expand = c(0, 0.2))


ggsave("reg_model_ultforce_full.png", plot = last_plot(), device = "png", 
       path = "C:\\Users\\J\\Desktop\\Course_DataAnalysis\\Proposal\\Pictures",
       scale = 1, width = 6, height = 6, units ="cm",
       dpi = 300)


## Plotting the QQ plot_model_ultforce_full
res=c(rstandard(model_ultforce_full))
fitted_values=c(model_ultforce_full[["fitted.values"]])
modeldata_ultforce_full <-data.frame(res,fitted_values)
stdresiduals <- rstandard(model_ultforce_full)

loadfonts(device = "win")

library(ggplot2)
library(extrafont)
font_import()

ggplot(data=modeldata_ultforce_full, aes(sample =res)) +
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
  scale_y_continuous(breaks=seq(-4,4,1),limits=c(-4, 4),expand = c(0, 0.2))


ggsave("qq_model_ultforce_full.png", plot = last_plot(), device = "png", 
       path = "C:\\Users\\J\\Desktop\\Course_DataAnalysis\\Proposal\\Pictures",
       scale = 1, width = 6, height = 6, units ="cm",
       dpi = 300)



# Model selection for ultimate force_F-test -------------------------------
rm(list=ls())
library(readr)
dataset_2 <- read_csv("C:/Users/J/Desktop/Course_DataAnalysis/dataset_2.csv")
dataset_2$D<-as.factor(dataset_2$D)
dataset_2$E<-as.factor(dataset_2$E)
dataset_2$G<-as.factor(dataset_2$G)
dataset_2$H<-as.factor(dataset_2$H)

model_ultforce_reduced<- lm(U~I+J+K+M+Q,data=dataset_2)
model_ultforce_full<- lm(U~ E+G+H+I+J+K+M+Q,data=dataset_2)


anova(model_ultforce_reduced,model_ultforce_full)

# Model-selection for ultimate force (backward+forward+stepwise)
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
U=dataset_2$U
dataset_2=data.frame(E,G,H,I,J,K,M,Q,U)

dataset_2$E<-as.factor(dataset_2$E)
dataset_2$G<-as.factor(dataset_2$G)
dataset_2$H<-as.factor(dataset_2$H)

fitstart_ultforce_full<- lm(U~ 1,data=dataset_2)
fitall_ultforce_full<- lm(U~ E+G+H+I+J+K+M+Q,data=dataset_2)
step(model_ultforce_full, direction="backward")
step(fitstart_ultforce_full, direction="forward", scope=formula(fitall_ultforce_full))
step(fitstart_ultforce_full, direction="both", scope=formula(fitall_ultforce_full))


model_ultforce_full_stepwise<- lm(formula = U ~ G + J + H + Q, data = dataset_2)
summary(model_ultforce_full_stepwise)


plot(model_ultforce_full_stepwise)



