#### Jurgita Markeviciute
#### 2019 spring


## Introduction

install.packages("ctv")
library(ctv)

install.views(Econometrcs)

library(help = stats)
demo(graphics)

getwd()
setwd()

#Session --> Set working directory --> Choose directory

source(kodas.R)

ls()
objects()


rm(y)
ls()

rm(list=ls())

###  R hep

help(integrate)
?integrate
help("for")
help("%*%")


help.start()
help.search("integrate")


RSiteSearch("integrate")


example("integrate")

## R objects: vectors

x <- c(10, 5, 3.8, 4.4, 12.7)
x

assign("y", c(0, -2, 4, 8))
y

c(10, 5, 3.8, 4.4, 12.7) -> x
x

1/y

length(x)
length(y)

z <- 2*x-3*y
z

length(z)

str(z)

1:10
seq(1,10)
seq(-10, 10)

seq(1,10, by=0.5)
seq(-10, 10, by=2)


y
(s1 <- rep(y, times=3))
(s2 <- rep(y, each=3))


sqrt(3)
sqrt(-3)
sqrt(-3+0i)
polyroot(c(2,6,1))


x
x >7
tmp <- x > 7
tmp

x
y
x > y

(x > 3) & (x < 10)
(x > 3) | (x < 10)
sum((x > 3) & (x < 10))
as.integer((x > 3) & (x < 10))


u <- c(1,2,NA,3,5)
is.na(u)
na.omit(u)

u == NA
u == "NA"

0/0
Inf-Inf
is.na(0/0)
is.nan(0/0)
is.nan(u)

p <- c("Juozas", "Petras", "Povilas", "Marija")
p

paste("Mano", 1:12, sep="")
paste("Tavo", 1:10, sep="_")
paste(paste("Vasara", 1:5, sep=""), "Ruduo", sep="_")


vardai <- paste("x", 1:3, sep="")
for(i in 1:3) {
  assign(vardai[i], i:(2*i))
}
x1
x2
x3

u 
v <- u[!is.na(u)]
v
(u+1)[!is.na(u) & u > 2] -> w
w

w[1]
v[1:3]
u[seq(1, length(u), by=2)]

w[-2]
v[-(3:4)]
u[-c(2,4)]

kaina <- c(25, 15, 3, 0.5, 47, 86, 12, 4, 6, 34)
names(kaina) <- c("kremas", "suris", "druska", "batai",
                  "rankine", "tortas", "mesa", "darzoves",
                  "auskarai", "vitaminai")
kaina
maistas <- kaina[c("suris", "druska", "tortas", "mesa", "darzoves")]
maistas
sum(maistas)


## Object types and attributes

vardai <- c("suris", "druska", "tortas", "mesa", "darzoves")
u <- c(1,2,NA,3,5)
cx <- sqrt(-3+0i)
mode(vardai)
mode(u)
mode(cx)

str(vardai); str(u); str(cx)
length(vardai); length(u); length(cx)

x <- -3:3
x
w <- as.character(x)
w
m <- as.matrix(x)
m
d <- as.numeric(w)
d
x == d
any(x != d)
all(x == d)

attributes(kaina)

class(kaina)
class(x)


## Nominal and rank variables

miestai <- c("Vilnius", "Kaunas", "Trakai", "Ukmerge")
miestai
mano.miestai <- factor(miestai)
mano.miestai
attributes(mano.miestai)
levels(mano.miestai)


prizai <- c(3:2, "nelaimejo", 1,2,1, "nelaimejo", 3,3,1, "nelaimejo")
prizai
mano.prizai <- ordered(prizai, levels=c("nelaimejo", 3:1))
mano.prizai


## R objects: array, matrix, list, data frame

m1 <- matrix(1:6, nrow=2, ncol=3, byrow = TRUE)
m1
m2 <- matrix(1:6, nrow=2, ncol=3, byrow = FALSE)
m2
m3 <- array(1:6, c(2,3))
m3


m1[1,2]
m2[2,3]
m1[,2]
m2[2,]
m3[1,]
m3[,]

m4 <- array(1:24, c(4,3,2))
m4
m4[,,2]
m4[,2,]
m4[2,,]

dim(m1); dim(m2); dim(m3); dim(m4)

m1 + m2 # matricu sudetis
m1 - m2 # matricu atimtis
m1 * m2 # matricu elementu daugyba
t(m2) # transponuota matrica
m1 %*% t(m2) # matricu daugyba
solve(m1 %*% t(m2)) # matricos atvirkstine
det(m1 %*% t(m2)) # matricos determinantas


m5 <- rbind(m1,m2)
m5
m6 <- cbind(m1,m2)
m6

anketa <- list(vardas=c("Kazys Jonaitis", "Povilas Petraitis"),
                 pareigos = c("mokytojas", "vairuotojas"),
                 seima=c("nevedes", "vedes"), vaiku_skaicius=c(2,3),
                 vaiku_amzius = matrix(c(5,6, NA, 1, 4, 7), ncol=3,byrow=TRUE))
anketa

mode(anketa)
class(anketa)
attributes(anketa)
dim(anketa)
anketa[[2]]
anketa$pareigos

salis <- c("Prancuzija", "Olandija", "Vokietija", "Anglija", "Ispanija")
salis
oro_bendrove <- c("Wizzair", "Lufthansa", "Lufthansa", "Ryanair", "Ryanair")
oro_bendrove
kaina <- c(60, 120, 80, 150, 130)
kaina
viesbutis <- c(250, 400, 200, 380, 300)
viesbutis
naktys <- c(3, 7, 5, 4, 7)
naktys

matr <- cbind(salis, oro_bendrove, kaina, viesbutis, naktys)
matr

sistema <- data.frame(salis, oro_bendrove, kaina, viesbutis, naktys)
sistema
mode(sistema)
class(sistema)
sistema[,2]
class(sistema[,2])
sistema[,4]
class(sistema[,4])

sistema[,3]
sistema[,"kaina"]
sistema$kaina

attach(sistema)
kaina
oro_bendrove
viesbutis/naktys
kaina+viesbutis
detach(sistema)


## Library dplyr

install.packages("dplyr")
library(dplyr)
mtcars

select(mtcars, am:wt)
select(mtcars, am:mpg)
select(mtcars, ends_with("t"))
select(mtcars, contains("m"))

filter(mtcars, cyl %in% c(4, 6))
filter(mtcars, cyl == 4 | cyl == 6)
filter(mtcars, mpg >= 15, mpg <= 22) 
filter(mtcars, mpg >= 15 & mpg <= 22) #tas pats rezultatas
filter(mtcars, hp > 100)


arrange(mtcars, hp, mpg)
arrange(mtcars, desc(hp))

mtcars <- mutate(mtcars, kml = mpg/3.7854*1.6093)
arrange(mtcars, desc(kml))


grpby<-group_by(mtcars,am)
summarise(grpby,vid = mean(mpg), med = median(wt))


## If else, cycle, function

y <- 5
if(y > 7) x <- 1 else x <- 0
x
y <- 9
if(y > 7) x <- 1 else x <- 0
x

y <- 9
ifelse(y > 7, 1, 0)


y <- c(4,3,7,8,6,1,0,2)
(y > 3) & (y <= 7)
(y > 3) && (y <= 7)

x <- numeric(10)
for(i in 1:10) x[i] <- i^2
x

z <- numeric(10)
ind <- seq(1,10,by=2)
for(k in ind) z[k] <- k^2+k
z

u <- numeric(10)
for(l in 1:10)
  {
    if(z[l] == 0) u[l] <- 8
     else
      {
        if(z[l] > 2) u[l] <- 9
        else u[l] <- 10
      }
  }
u


y <- 0
repeat
  {
    y <- y + 2
    print(y)
    if(y > 10) break
  }

y <- -3
while(y < 10)
  {
    y <- y+2
    if (y == 3) break
    print(y)
  }

y <- -3
while(y < 10)
  {
    y <- y+2
    if (y == 3) next
    print(y)
  }


kvadratinis.trinaris <- function(x, koef)
  {
    if(length(koef) != 3) stop("Privalote pateikti 3 trinario koeficientus!")
    if((mode(koef) != "numeric") || (mode(x) != "numeric"))
      stop("Privalote pateikti skaitines reikðmes!")
    rez <- koef[1]*x^2 + koef[2]*x + koef[3]
    sakn <- polyroot(koef)
    return(list(rezultatas = rez, ðaknys = sakn))
}

sk <- c(4,3,7,8,6,1,0,2)
kvadratinis.trinaris(sk, c(1,5,-8))

spalvos <- c("balta", "juoda")
kvadratinis.trinaris(spalvos, c(1,5,-8))
kvadratinis.trinaris(sk, c("u", "v", "m"))
kvadratinis.trinaris(sk, c(8,5,3,4))

"%ms%" <- function(x,y)
  {
    if(is.character(x) || (mode(y) == "character"))
      {
        return(paste(x , y, sep="_"))
        } else {
            .Primitive("*")(x,y)
            }
  }

x <- 5; y <- 8
x %ms% y
x <- "Sausis"; y <- "Vasaris"
x %ms% y
x <- 5; y <- 'Sausis'
x %ms% y

integralas <- function(f, a=0, b=1, graph=TRUE)
  {
    y <- integrate(f, lower=a, upper=b)
    print(y)
    if(graph)
      {
        x <- seq(a, b, by = 0.01)
        plot(x, f(x), type='l', main="Funkcijos grafikas")
        }
    }
integralas(function(x) sin(x))
integralas(function(x) sin(x), a=-pi, b=pi, graph=FALSE)
integralas(function(x) sin(x), a=-pi, b=pi, graph=TRUE)


## Loading data from file

x <- read.table("data/SP500_daily.txt", h=T, skip=35)
head(x)
tail(x)

y <- read.csv2("data/Stock_prices_1.csv", h=T)
head(y)
tail(y)



