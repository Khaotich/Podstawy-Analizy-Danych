multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

library(readxl)
library(ggplot2)

dane = read_excel("Zadanie_domowe_nr_3_2022_2023_KP.xlsx")
odmiana = dane$odmiana
cecha1 = dane$cecha_1
cecha2 = dane$cecha_2
cecha3 = dane$cecha_3
cecha4 = dane$cecha_4

odmiana_1_cecha1 = cecha1[1:30]
odmiana_2_cecha1 = cecha1[31:60]
odmiana_3_cecha1 = cecha1[61:90]
odmiana_4_cecha1 = cecha1[91:120]
odmiana_1_cecha2 = cecha2[1:30]
odmiana_2_cecha2 = cecha2[31:60]
odmiana_3_cecha2 = cecha2[61:90]
odmiana_4_cecha2 = cecha2[91:120]
odmiana_1_cecha3 = cecha3[1:30]
odmiana_2_cecha3 = cecha3[31:60]
odmiana_3_cecha3 = cecha3[61:90]
odmiana_4_cecha3 = cecha3[91:120]
odmiana_1_cecha4 = cecha4[1:30]
odmiana_2_cecha4 = cecha4[31:60]
odmiana_3_cecha4 = cecha4[61:90]
odmiana_4_cecha4 = cecha4[91:120]

x1 = ggplot(dane, aes(x=odmiana, y=cecha1)) + geom_point() +
  geom_point(aes(x="pierwsza", y=mean(odmiana_1_cecha1)), col='red', size=4) +
  geom_point(aes(x="druga", y=mean(odmiana_2_cecha1)), col='red', size=4) +
  geom_point(aes(x="trzecia", y=mean(odmiana_3_cecha1)), col='red', size=4) +
  geom_point(aes(x="czwarta", y=mean(odmiana_4_cecha1)), col='red', size=4) 

x2 = ggplot(dane, aes(x=odmiana, y=cecha2)) + geom_point() +
  geom_point(aes(x="pierwsza", y=mean(odmiana_1_cecha2)), col='red', size=4) +
  geom_point(aes(x="druga", y=mean(odmiana_2_cecha2)), col='red', size=4) +
  geom_point(aes(x="trzecia", y=mean(odmiana_3_cecha2)), col='red', size=4) +
  geom_point(aes(x="czwarta", y=mean(odmiana_4_cecha2)), col='red', size=4)

x3 = ggplot(dane, aes(x=odmiana, y=cecha3)) + geom_point() +
  geom_point(aes(x="pierwsza", y=mean(odmiana_1_cecha3)), col='red', size=4) +
  geom_point(aes(x="druga", y=mean(odmiana_2_cecha3)), col='red', size=4) +
  geom_point(aes(x="trzecia", y=mean(odmiana_3_cecha3)), col='red', size=4) +
  geom_point(aes(x="czwarta", y=mean(odmiana_4_cecha3)), col='red', size=4)

x4 = ggplot(dane, aes(x=odmiana, y=cecha4)) + geom_point() +
  geom_point(aes(x="pierwsza", y=mean(odmiana_1_cecha4)), col='red', size=4) +
  geom_point(aes(x="druga", y=mean(odmiana_2_cecha4)), col='red', size=4) +
  geom_point(aes(x="trzecia", y=mean(odmiana_3_cecha4)), col='red', size=4) +
  geom_point(aes(x="czwarta", y=mean(odmiana_4_cecha4)), col='red', size=4)

x = multiplot(x1, x2, x3, x4, cols=2)
print(x)

y1 = ggplot(dane, aes(x=odmiana, y=cecha1)) + geom_boxplot()
y2 = ggplot(dane, aes(x=odmiana, y=cecha2)) + geom_boxplot()
y3 = ggplot(dane, aes(x=odmiana, y=cecha3)) + geom_boxplot()
y4 = ggplot(dane, aes(x=odmiana, y=cecha4)) + geom_boxplot()

y = multiplot(y1, y2, y3, y4, cols=2)
print(y)

z1 = ggplot(dane, aes(x=cecha1)) + geom_histogram(aes(y=..density..)) + 
  stat_function(fun=dnorm, args=list(mean=mean(cecha1), sd=sd(cecha1)), col="red")

z2 = ggplot(dane, aes(x=cecha2)) + geom_histogram(aes(y=..density..)) + 
  stat_function(fun=dnorm, args=list(mean=mean(cecha2), sd=sd(cecha2)), col="red")

z3 = ggplot(dane, aes(x=cecha3)) + geom_histogram(aes(y=..density..)) + 
  stat_function(fun=dnorm, args=list(mean=mean(cecha3), sd=sd(cecha3)), col="red")

z4 = ggplot(dane, aes(x=cecha4)) + geom_histogram(aes(y=..density..)) + 
  stat_function(fun=dnorm, args=list(mean=mean(cecha4), sd=sd(cecha4)), col="red")

z = multiplot(z1, z2, z3, z4, cols=2)
print(z)

h1 = ggplot(dane, aes(x=cecha1, y=length(cecha1))) + geom_violin()
h2 = ggplot(dane, aes(x=cecha2, y=length(cecha2))) + geom_violin()
h3 = ggplot(dane, aes(x=cecha3, y=length(cecha3))) + geom_violin()
h4 = ggplot(dane, aes(x=cecha4, y=length(cecha4))) + geom_violin()

h = multiplot(h1, h2, h3, h4, cols=2)
print(h)