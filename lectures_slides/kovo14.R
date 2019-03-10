#### Jurgita Markeviciute
#### 2019 spring


## Data grouping

x <- c(212, 415, 356, 147, 95, 73, 236)
x
xx <- sort(x)
xx
w <- order(x)
w
q <- x[w]
q
y <- c(21, 18, 76, 0, -1, 12, 57)
sort(y)
order(y)
z <- x[order(y)]
z


min(x)
max(x)

a <- c(2,7,-6, NA, 8, 0)
a
min(a)
max(a)

min(a, na.rm=TRUE)
max(a, na.rm=TRUE)


apklausa <- sample(c("visiðkai pritariu", "pritariu", "neturiu nuomonës",
                       "nepritariu", "visiðkai nepritariu"), size=200, replace=TRUE)
head(apklausa)

table(apklausa)
prop.table(table(apklausa))
prop.table(table(apklausa))*100

cbind(Daþnis=table(apklausa), Sukauptas_daþnis=cumsum(table(apklausa)),
        Santykinis_daþnis =prop.table(table(apklausa)),
        Sukauptas_santykinis_daþnis=cumsum(prop.table(table(apklausa))))

freq.table <- function(x)
  {
    frtb <- table(x)
    sfrtb <- cumsum(table(x))
    rfrtb <- prop.table(table(x))
    srfrtb <- cumsum(prop.table(table(x)))
    my.table <- cbind(Daþnis=frtb, Sukauptas_daþnis=sfrtb,
                       Santykinis_daþnis =rfrtb, Sukauptas_santykinis_daþnis=srfrtb)
    return(my.table)
}


dat <- sample(1:4, size=1000, replace=TRUE)
head(dat)
freq.table(apklausa)
freq.table(dat)


ecdf(table(apklausa))
plot(ecdf(table(apklausa)), main = "Daþniø pasiskirstymo funkcija")

barplot(table(apklausa),las=1,cex.names=0.7,
        main="Apklausa dël vienalyèiø santuokø")


Titanic
require(graphics)
mosaicplot(Titanic, main = "Survival on the Titanic")


ftable(Titanic)


head(mtcars)
attach(mtcars)
table(cyl, gear)

table(cyl, gear,am)
ftable(table(cyl, gear,am))

sp500 <- read.table("data/SP500_daily.txt", h=T, skip=35)
head(x)
plot(as.numeric(sp500[,2]),type='l', xlab="Laikas",
     ylab="S&P 500", main = "S&P 500 indeksas")


sp <- sp500[,2]
my.cut <- cut(sp, breaks = seq(min(sp, na.rm=TRUE), max(sp, na.rm=TRUE), by=150))
head(my.cut)
table(my.cut)
hist(sp, breaks = 10, xlim = c(min(sp, na.rm=TRUE)-10, max(sp, na.rm=TRUE)+10),
    xaxt = 'n')
axis(1, at=c(0,max(sp, na.rm=TRUE)), labels=c("",""), lwd.ticks=0)
axis(1, at=seq(0 , max(sp, na.rm=TRUE), by=200), lwd=0, lwd.ticks=1)


## Descriptive statistics

mean(sp) # vidurkis
mean(sp, na.rm=TRUE)

my.mean <- function(x)
  {
    obs <- na.omit(sp)
    n <- length(obs)
    mn <- sum(obs)/n
    return(mn)
    }
my.mean(sp)


a <- c(-5,-4,3,2,8)
b <- c(-5,-4,3,2,80)
mean(a)
mean(b)

mean(a, trim=0.20)
mean(b, trim=0.20)


median(sp)
median(sp, na.rm=TRUE)
mean(sp, na.rm=TRUE, trim=0.5)

x <- c(-10,8,5,6,12,-15,16,18,20,-3,0,4,7)
quantile(x, 0.10)
quantile(x, 0.90)
quantile(x, 0.50)
median(x)

quantile(x, 0.25)
quantile(x, 0.75)
summary(x)


x <- c(-10,8,5,6,12,-15,16,18,20,-3,0,4,7)
var(x)

my.var1 <- function(x)
  {
    n <- length(x)
    vid <- mean(x)
    disp <- (1/(n-1))*sum(x^2) - (n/(n-1))*vid^2
    return(disp)
  }
my.var1(x)

sd(x)
sqrt(var(x))


x <- c(-10,8,5,6,12,-15,16,18,20,-3,0,4,7)
sample.dist <- function(x)
  {
    iqr <- quantile(x, 0.75, na.rm=TRUE) - quantile(x, 0.25, na.rm=TRUE)
    amp <- max(x, na.rm=TRUE) - min(x, na.rm=TRUE)
    return(list(Tarpkvantilinis_skirtumas = iqr, Aibës_plotis = amp))
  }
sample.dist(x)

range(x)
diff(range(x))

IQR(x)


mad(x)
sd(x)


iqv <- function(x)
  {
    frq <- table(x)
    k <- dim(frq)
    n <- sum(frq)
    sqfrq <- frq^2
    iqv <- (k*(n^2-sum(sqfrq)))/(n^2*(k-1))
    return(iqv)
  }
iqv(apklausa)


sp <- read.table("data/SP500_daily.txt", h=TRUE, skip=35)[,2]
asi <- function(x)
  {
    y <- na.omit(x)
    n <- length(y)
    m3 <- (sum((y-mean(y, na.rm=T))^3))/(n-1)
    m2 <- sd(y)
    g1 <- m3/m2^3
    return(g1)
  }

eks <- function(x)
    {
      y <- na.omit(x)
      n <- length(y)
      m4 <- (sum((y-mean(y, na.rm=T))^4))/(n-1)
      m2 <- sd(y)
      g2 <- m4/m2^4 - 3
      return(g2)
    }

asi(sp)
eks(sp)


head(sp)
summary(sp)
var(sp, na.rm=TRUE)
sd(sp, na.rm=TRUE)
zsp <- scale(sp)
head(zsp)
summary(zsp)
var(zsp, na.rm=TRUE)
sd(zsp, na.rm=TRUE)


## Outliers

boxplot(sp)
boxplot(sp)$out

## Covariance and correlation

x <- read.csv2("data/Stock_prices_1.csv", h=T)
var(x[,2:4])
cor(x[,2:4])


## Graphs

library(rpart)
data(stagec)
attach(stagec)
qpar <- par(mfrow=c(2,2))
hist(age, freq=TRUE)
hist(age, freq=FALSE)
hist(age, breaks=15)
hist(age, breaks=c(45,48,51,54,57,60,63,66,69,72,75))
par(qpar)


library(lattice)
?histogram
histogram(~age|ploidy, data=stagec)

qpar <- par(mfrow=c(2,2))
barplot(grade)
barplot(table(grade))
box()
barplot(table(grade),horiz=TRUE,col=4)
barplot(table(grade), legend.text="grade")
par(qpar)


qpar <- par(mfrow=c(2,2))
barplot(table(grade, pgstat), legend.text=c(1,2,3,4))
barplot(table(grade, pgstat),beside=TRUE)
barplot(table(pgstat, grade), col=c("aquamarine3","coral"))
barplot(table(pgstat, grade),beside=TRUE, col=c("aquamarine3","coral"),
        legend.text=c(1,2))
par(qpar)


pareto.diagrama <- function(x, ylab = "Daþnis", ylab1 = "Sukauptas daþnis",
                            barcol = rainbow(20), linecol = "blue") {
    par(mar=c(5,5,4,5))
    if(!is.table(x)) y <- table(x) else y <- x
    u <- sort(y, decreasing = TRUE)
    prts <- cumsum(u)
    proc <- cumsum(u)/sum(u)*100
    bp <- barplot(u, ann=FALSE, width=1, las = 1,
                  space=0.2, col=barcol, ylab = ylab, ylim=c(0,1.05*max(cumsum(u))),
                  main = "Pareto diagrama", cex.axis=0.7)
    lines(bp, prts, type="b",col=linecol,lwd=3, ylab=ylab1)
    axis(4,at=c(0, cumsum(u)), labels = paste(c(0, round(proc)) ,"%",sep=""),
        las=1, cex.axis=0.7)
    mtext(ylab1, side = 4, line = 3)
    box()
    }
pareto.diagrama(grade)


pie(table(grade), col=2:5, main="Skritulinë diagrama")

attach(mtcars)
plot(wt, mpg, main="Sklaidos diagrama",
    xlab="Maðinos svoris ", ylab="Galonai vienai myliai",
    pch=24, bg = 4, col = 2)

## ggplot

library(ggplot2)
library(dplyr)


dat<-read.csv("data/nations.csv", header = TRUE)
head(dat)
dt<-select(dat,country,year,gdp_percap,birth_rate, population, region)
dt1<-filter(dt,year=='2013')

p <- ggplot(data = dt1, mapping = aes(x = gdp_percap, y = birth_rate))+
      geom_point()
p

p <- ggplot(data = dt1, mapping = aes(x =gdp_percap, y = birth_rate,
                                      size=population,color=region))
p+geom_point()


options(scipen=8)
p <- ggplot(data = dt1, mapping = aes(x = gdp_percap, y = birth_rate)) 

p+ geom_point(colour="green", size=2, shape=21, fill="white")+
  geom_smooth(method='loess') +  scale_x_log10()+
  theme(axis.text.x = element_text(angle=45, hjust=1,size="12", color="brown"))+
  xlab("LOG_BVP") + ylab("Gimstamumas") +
  ggtitle("BVP vs Gimstamumas")

