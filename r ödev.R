library(readxl)
heart <- read_excel("C:/Users/dell/Downloads/heart.xlsx")
View(heart)
library(alluvial)
alluvial(heart[c(1,2)], freq = heart$Age,
         col = ifelse(heart$Sex == "M","red", "blue"),
         border = ifelse(heart$Sex == "M", "red", "blue"),
         hide = heart$Age == 0,
         cex = 0.7)
library(ggplot2)
library(dplyr)
ggplot(heart, aes( x = Cholesterol))+
  geom_freqpoly(color="red", size = 3)+
  theme_dark()

ggplot(heart, aes( x = Oldpeak))+
  geom_freqpoly(color="red", size = 3)+
  theme_dark()

ggplot(heart, aes( x = Age))+
  geom_freqpoly(color="red", size = 3)+
  theme_dark()
#çizgi grafiği
ggplot(heart, aes( x = Cholesterol))+
  geom_area(stat = "bin", fill = "blue", color = "red", size = 3)

ggplot(heart, aes( x = Oldpeak))+
  geom_area(stat = "bin", fill = "blue", color = "red", size = 3)

ggplot(heart, aes( x = Age))+
  geom_area(stat = "bin", fill = "blue", color = "red", size = 3)

#alan grafiği
ggplot(heart, aes(x = Oldpeak))+
  geom_histogram(color = "purple", fill = "green")+
  geom_freqpoly()

ggplot(heart, aes(x = Age))+
  geom_histogram(color = "purple", fill = "green")+
  geom_freqpoly()

ggplot(heart, aes(x = Cholesterol))+
  geom_histogram(color = "purple", fill = "green")+
  geom_freqpoly()
#histogram
ggplot(data = heart, aes(x = Age, y = Cholesterol)) +
  geom_point() +
  geom_line(data = head(heart), color = "red")
par(mfrow=c(2,2))
a <- ggplot(heart, aes(x = Age))
a+geom_bar()

par(mfrow=c(2,2))
b <- ggplot(heart, aes(x = Cholesterol))
b+geom_bar()

par(mfrow=c(2,2))
c<- ggplot(heart, aes(x = Oldpeak))
c+geom_bar()
a+geom_dotplot()
b+geom_dotplot()
c+geom_dotplot()
a+geom_histogram()
b+geom_histogram()
c+geom_histogram()
a + geom_density(aes(fill = Sex), alpha=0.4)
b + geom_density(aes(fill = Sex), alpha=0.4)
c+ geom_density(aes(fill = Sex), alpha=0.4)
a + geom_histogram(aes(y=..density.., color = Sex, fill = Sex),
                   alpha=0.5, position="identity")+
  geom_density(aes(color = Sex), size = 1)

b + geom_histogram(aes(y=..density.., color = Sex, fill = Sex),
                   alpha=0.5, position="identity")+
  geom_density(aes(color = Sex), size = 1)

c + geom_histogram(aes(y=..density.., color = Sex, fill = Sex),
                   alpha=0.5, position="identity")+
  geom_density(aes(color = Sex), size = 1)
ggplot(heart, aes(x =Age , y =Cholesterol )) +
  stat_smooth(method = "lm", se = FALSE, color = "orange", formula = y ~ x) +
  stat_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x + I(x ^ 2)) +
  stat_smooth(method = "lm", se = FALSE, color = "green", formula = y ~ x + I(x ^ 2)+ I(x ^ 3)) +
  geom_point(colour = "black", size = 1)
Heart_<-data.frame(x=rnorm(40), y=rnorm(40))
plot(Heart_, main = "Heart Scatterplot")
plot(heart[c(1,5,10)])
car::scatterplotMatrix(heart[c(1,5,10)])
GGally::ggpairs(heart[c(1,5,10)])
ggplot(heart, aes(x=Age, y=Cholesterol) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")+theme_classic()
p <- ggplot(heart, aes(sample=Age))
p + stat_qq(aes(shape = Sex , color = Sex))+
  scale_color_manual(values=c("blue","green","red","yellow"))

q <- ggplot(heart, aes(sample=Cholesterol))
q + stat_qq(aes(shape = Sex , color = Sex))+
  scale_color_manual(values=c("blue", "green","red","yellow"))

z <- ggplot(heart, aes(sample=Oldpeak))
z + stat_qq(aes(shape = Sex , color = Sex))+
  scale_color_manual(values=c("blue", "green","red","yellow"))
e<- ggplot(heart, aes(x = Age, y = Cholesterol))
e + geom_hex(bins = 20)
g <- ggplot(data=heart, aes(x=ExerciseAngina, y=Cholesterol, fill=Sex))
g + geom_bar(stat = "identity")
palette(rainbow(12, s = 0.6, v = 0.75))
stars(heart[,5:9], len = 0.8, key.loc = c(12, 1.5),
      main = "Star Plot", draw.segments = TRUE)
