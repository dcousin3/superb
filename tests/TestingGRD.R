#######################################################
#######################################################
#######################################################
#######################################################
#####                                             #####
#####     (1) how to use GRD by examples          #####
#####                                             #####
#######################################################
#######################################################
#######################################################
#######################################################

setwd("C:\\Users\\DenisCousineau\\Documents\\1_Publications\\Mes Articles\\_4_Publié (107x)\\Étudiants (30x)\\_BradleyHarding(6x)\\MaC01-GRD4R\\Source\\Version5.0-2018.12.10")
source("GRD_20.R")
library(lattice)
library(lsr)


#######################################################
# this is the minimum specification
#######################################################
dta <- GRD()
head(dta)
tail(dta)
hist(dta$DV)


# renaming the dependant variable and setting the group size
dta <- GRD( RenameDV = "score", SubjectsPerGroup = 1000 )
hist(dta$score )


# placing an experimental design, first between groups...
# the divider between factors is the colon ":"
dta <- GRD( BSFactors = '3')
dta <- GRD( BSFactors = "3 : 2")
dta <- GRD( BSFactors = "(yes,no) : (CBT, Control, Exercice)")
dta <- GRD( BSFactors = "Stress(3)")
dta <- GRD( WSFactors = "Moment (2)")
dta <- GRD( BSFactors = "Group(3)", WSFactors = "2 : 3")


# obtaining debug information (not required) and a summary
dta <- GRD( BSFactors = "Group(3)", Debug=TRUE, Summary=TRUE)


#######################################################
# A whole test with plots and anova
#######################################################
dta <- GRD(BSFactors = "difficulty(3) : gender (2)", 
  WSFactors="day(2)",
  SubjectsPerGroup=c(2,2,3,3,2,2)
)
  
Day = factor(c("DV.1", "DV.2")) # good for anova but not for plot
dta$gender = factor(dta$gender, labels=c(1,2))
dta$difficulty = factor(dta$difficulty, labels=c(1,2,3))
anv1 = lm(cbind(DV.1, DV.2)~difficulty*gender, data=dta)
anv2 = Anova(anv1, idata=data.frame(Day), idesign=~Day, type=3)
summary(anv2, multivariate=F)

histogram(~ dta$DV.1 | difficulty, data = dta)
histogram(~ dta$DV.2 | difficulty, data = dta)
library("reshape2")
dta2 <- melt(dta, id.vars=c("difficulty","gender"))
dta3 <- dta2[dta2$variable != "id",]
dta3$gender = factor(dta3$gender, labels=c(1, 2))
histogram(~ value | difficulty, data = dta3)
histogram(~ value | difficulty + gender + variable, data = dta3)
# lattice::histogram is limited to 2 variables for column and rows...


#######################################################
# defining the population characteristics
#######################################################
dta <- GRD( 
  RenameDV = "IQ",
  Population=list(
    mean=100,  # will set GM to 100
    stddev=15  # will set STDDEV to 15
  ) 
)
hist(dta$IQ)

dta <- GRD(BSFactors="difficulty(2)", SubjectsPerGroup = 1000,
  Population=list(mean=100,stddev=15)
)
histogram(~ DV | difficulty, data = dta)


#######################################################
# adding effects
#######################################################
dta <- GRD(BSFactors="difficulty(2)", SubjectsPerGroup = 1000,
  Population=list(mean=100,stddev=15),
  Effects = list("difficulty" = range(50) )
)
histogram(~ DV | difficulty, data = dta)


dta <- GRD(BSFactors="difficulty(5)", SubjectsPerGroup = 1000,
  Population=list(mean=0,stddev=5), Debug=TRUE,
  Effects = list("difficulty" = slope(50) )
)
histogram(~ DV | difficulty, data = dta)
hist(dta$DV, breaks=seq(-150,150,by=5) )


dta <- GRD(BSFactors="difficulty(3):gender(2)", 
  Population=list(mean=100,stddev=15), SubjectsPerGroup = 5000,
  Effects = list(
    "difficulty" = range(10),
    "gender"=slope(10),
    "difficulty*gender"=custom(-300,+200,-100,0,0,0) 
  ) 
)
dta$gender = factor(dta$gender, labels=c("Male","Femelle"))
dta$difficulty = factor(dta$difficulty, labels=c("easy","medium","hard"))
histogram(~ DV | difficulty + gender, data = dta,
  type="density",breaks=seq(-300,400,by=10)
)


dta <- GRD(
  BSFactors = 'Reply(yes, no) : Therapy(CBT, Exercise, Control)',
  WSFactors = 'Contrast(C1, C2, C3) : Size(small, big)',
  SubjectsPerGroup = 1000,
  Effects = list(
    "Therapy*Contrast"=slope(10) 
  ) 
)
dta$gender = factor(dta$gender, labels=c("Male","Femelle"))
dta$difficulty = factor(dta$difficulty, labels=c("easy","medium","hard"))
histogram(~ DV | difficulty + gender, data = dta,
  type="density",breaks=seq(-300,400,by=10)
)




# The Rexpression effects are given arbitrary names 
# instead of factors on which to operate
dta <- GRD(BSFactors="difficulty(5)", SubjectsPerGroup = 1000,
  Population=list(mean=0,stddev=5),
  Effects = list(
    "code1" = Rexpression("if (difficulty ==1) {-50} else {0}"), 
    "code2" = Rexpression("if (difficulty ==3) {+50} else {0}") 
  )
)
dta$difficulty = factor(dta$difficulty, labels=c("easy","e-m","medium","m-h","hard"))
histogram(~ DV | difficulty, data = dta,
  breaks=seq(min(dta$DV)-5,max(dta$DV)+5,by=2.5)
)
# Rexpression can be any expression which can be applied to the 
# subject "id", the factor(s) values, and the DV itself


#######################################################
# specifying underlying distributions
#######################################################

dta <- GRD(SubjectsPerGroup = 5000, 
  Population=list(mean=100,stddev=15)
)
hist(dta$DV,breaks=seq(35,165,by=2.5))


# heterogeneous variances across groups
dta <- GRD(SubjectsPerGroup = 5000, 
  BSFactors = "Group(2)",
  Population=list(
    mean = 100, 
    stddev = 15, 
    scores = "rnorm(1, mean = GM, sd = 5 * Group)"
  ) 
)
library(lattice)
dta$Group = factor(dta$Group, labels=c("compact group","Spread out group"))
histogram(~ DV | Group, data = dta,
  type="density",breaks=seq(25,175,by=2.5) )


dta <- GRD(SubjectsPerGroup = 5000, 
  Population=list(
    scores = "rweibull(1, shape=2, scale=40)"
  )
)
hist(dta$DV,breaks=seq(0,165,by=2.5))
# When using random number generator, always generate the numbers
# one by one (so that the first argument is 1) unless rho is set


#######################################################
# introducing contaminants
#######################################################
dta <- GRD(SubjectsPerGroup = 5000, 
  Population=list(
    mean=100, stddev = 15  
  ), 
  Contaminant=list(
    mean=200, stddev = 15, proportion = 0.10
  )
)
hist(dta$DV,breaks=seq(0,265,by=2.5))

dta <- GRD(SubjectsPerGroup = 10000, 
  Population=list(
    mean=100, stddev = 15  
  ), 
  Contaminant=list(
    scores="rweibull(1,shape=2, scale=30)+1.5*GM", proportion = 0.10
  )
)
hist(dta$DV,breaks=seq(0,265,by=2.5))


#######################################################
# generating multivariate normal data
#######################################################

dta <- GRD( BSFactors="grp(2)",WSFactors = "Moment (2)", 
   SubjectsPerGroup = 1000,
   Population=list(mean=0,stddev=20,rho=-0.85), 
   Contaminant=list(mean=100,stddev=4,rho=-0.99,proportion=0.25),
   Summary=TRUE 
)
dta$grp = factor(dta$grp, labels=c("grp 1","grp 2"))
histogram(~ DV.1 | grp, data = dta,
  breaks=seq(min(dta$DV.1)-5,max(dta$DV.1)+5,by=2.5) )
plot(dta$DV.1, dta$DV.2)
 

dta <- GRD( BSFactors="grp(2)",WSFactors = "Moment (2)", 
   SubjectsPerGroup = 1000,
   Effects = list("grp" = slope(100) ),
   Population=list(mean=0,stddev=20,rho=-0.85), 
   Contaminant=list(mean=100,stddev=4,rho=-0.99,proportion=0.25),
   Summary=TRUE 
)
par(mfrow=c(1,2))
plot(dta[dta$grp == 1,]$DV.1,dta[dta$grp==1,]$DV.2, ylim=c(-150,150), xlim=c(-150,150))
plot(dta[dta$grp == 2,]$DV.1,dta[dta$grp==2,]$DV.2, ylim=c(-150,150), xlim=c(-150,150)) 



#######################################################
#######################################################
#######################################################
#######################################################
#####                                             #####
#####  (2) Testing the example from the article   #####
#####                                             #####
#######################################################
#######################################################
#######################################################
#######################################################

# page 4
setwd("C:\\Users\\DenisCousineau\\Documents\\1_Publications\\Mes Articles\\MaC01-GRD4R\\Source\\Version5.0-2018.12.10")
source("GRD_20.R")
dta <- GRD()

# page 5
head(dta,2)
tail (dta,2)
hist(dta$DV)

# page 6
dta <- GRD( RenameDV = "score")

# page 7
dta <- GRD( BSFactors = "3")
head(dta,2)
dta <- GRD( BSFactors = "2 : 3")
dta <- GRD( BSFactors = "(yes,no) : (CBT, Control, Exercice)")

# page 8
dta <- GRD( BSFactors = "Surgery(2) : Therapy(3)")
dta <- GRD( BSFactors = "Surgery(yes,no) : Therapy(CBT, Control, Exercice)")
dta <- GRD( BSFactors = "(yes,no) : Therapy(3)")
dta <- GRD( WSFactors = "3")
dta <- GRD( WSFactors = "Contrast(Low,Medium,High)")
head(dta,2)

# page 9
dta <- GRD(
  BSFactors = "Surgery(yes,no) : Therapy(CBT, Control, Exercice)",
  WSFactors = "Contrast(Low,Medium,High)"  
)
dta <- GRD( SubjectsPerGroup = 1000 )
dim(dta)
dta <- GRD( BSFactors = "3", SubjectsPerGroup = c(20,25,50) )

# page 10
dta <- GRD(
  RenameDV = "IQ", 
  Population = list(mean = 100, stddev = 15)
)
hist(dta$IQ)
dta <- GRD(
  BSFactors = "Group(2)",
  Population = list(scores = "1")
)

# page 11
dta <- GRD(
  BSFactors = "Group(2)",
  Population = list(scores = "Group")
)
dta <- GRD(
  BSFactors = "Group(2)",
  Population = list(
    mean = 100,
    stddev = 15,
    scores = "rnorm(1, mean=GM, sd=STDDEV*Group)"
  )
)
dta <- GRD(
  BSFactors = "Group(2)",
  Population = list(
    scores = "rnorm(1, mean=100, sd=15*Group)"
  )
)
library(lawstat)
levene.test(dta$DV, dta$Group, location="mean")

# page 12
dta <- GRD( SubjectsPerGroup = 5000,
  RenameDV = "RT", 
  Population = list(
    scores = "rweibull(1, shape=2, scale=40)+250"
  )
)
hist(dta$RT, breaks = seq(250, 425, by = 5) ) 

# page 14
dta <- GRD(
  BSFactors = "Therapy(CBT, Control, Exercice)",
  WSFactors = "Contrast(3)",
  SubjectsPerGroup = 1000,
  Effects = list("Therapy" = slope(2) )
)
library(lattice)
histogram(~ DV.1 | Therapy, data = dta,
  breaks = seq(-6,6,by=1), layout=c(3,1) )

dta <- GRD(
  BSFactors = "Therapy(CBT, Control, Exercice)",
  WSFactors = "Contrast(3)",
  SubjectsPerGroup = 1000,
  Effects = list("Contrast" = range(4) )
)
library(lsr)
dta2 <- wideToLong(dta, within = c("Contrast"), sep = ".")
histogram( ~DV | Contrast, data = dta2, 
  breaks = seq(-7, 7, by=1), layout = c(3,1), aspect = 1,
  ylab="Percent total"
)

# page 15
dta <- GRD(
  BSFactors = "Therapy(CBT, Control, Exercice)",
  WSFactors = "Contrast(3)",
  SubjectsPerGroup = 1000,
  Effects = list("Therapy" = custom(0,0,2) )
)
dta <- GRD(
  BSFactors = "Therapy(CBT, Control, Exercice)",
  WSFactors = "Contrast(3)",
  SubjectsPerGroup = 1000,
  Effects = list("Therapy*Contrast" = slope(10) )
)
dta <- GRD(
  BSFactors = "Therapy(CBT, Control, Exercice)",
  WSFactors = "Contrast(3)",
  SubjectsPerGroup = 10,
  Effects = list(
    "code1" = Rexpression("if(Therapy  == 'CBT') {-50} else {0}"),
    "code2" = Rexpression("if(Contrast == 3)     {+50} else {0}")
    )
)
# page 16
library(lsr)
dta2 <- wideToLong(dta, within = c("Contrast"), sep = ".")
histogram(~ DV | Contrast + Therapy, data=dta2,
  breaks = seq(min(dta2$DV)-5,max(dta2$DV)+5,by=2.5)
)

dta <- GRD(
  BSFactors = "Therapy(CBT, Control, Exercice)",
  WSFactors = "Contrast(3)",
  SubjectsPerGroup = 10,
  Effects = list("code1" = Rexpression("print(id);0") )
)

dta <- GRD(
  WSFactors = "Difficulty(2)",
  SubjectsPerGroup = 1000,
  Population = list(Mean = 0, stddev = 20, rho = 0.5)
)
plot(dta$DV.1, dta$DV.2)

# page 17
dta <- GRD(
  WSFactors = "Difficulty(2)",
  SubjectsPerGroup = 1000,
  Population = list(Mean = c(10,2), stddev = c(1,0.2), rho = -0.85)
)
plot(dta$DV.1, dta$DV.2)


# page 18
library(fMultivar)
xi =c(0,0)
omega = diag(2)
omega[1,2] = omega[2,1] = 0.5
alpha = c(2,-6)
dta <- GRD(
  WSFactors = "Difficulty(2)",
  SubjectsPerGroup = 1000,
  Population = list(rho = 99,
    scores = 'sn::rmsn(1,xi,omega,alpha)'
  )
)
plot(dta$DV.1, dta$DV.2)


# page 19
dta <- GRD(
  SubjectsPerGroup = 5000,
  Population = list(mean = 100, stddev = 15 ),
  Contaminant = list(mean = 200, stddev = 15, proportion = 0.1) 
)
hist(dta$DV, breaks = seq(0,265,by=2.5))

dta <- GRD(
  SubjectsPerGroup = 5000,
  Population = list(mean = 100, stddev = 15 ),
  Contaminant = list(scores = 'rweibull(1,shape=1.5,scale=30)+1.5*GM', proportion = 0.1) 
)
hist(dta$DV, breaks = seq(0,265,by=2.5))

dta <- GRD( BSFactors="grp(2)",WSFactors="M(2)",
  SubjectsPerGroup = 1000,
  Effects = list("grp"=slope(100)),
  Population = list(mean = 0, stddev = 15, rho=-0.85 ),
  Contaminant = list(mean =100, stddev = 4, rho = -0.99, proportion = 0.1) 
)
par(mfrow=c(1,2))
plot(dta[dta$grp ==1,]$DV.1,dta[dta$grp==1,]$DV.2,
  ylim = c(-150,150), xlim = c(-150,150))
plot(dta[dta$grp ==2,]$DV.1,dta[dta$grp==2,]$DV.2,
  ylim = c(-150,150), xlim = c(-150,150))

# page 20
dta <- GRD(
  SubjectsPerGroup = 5000,
  Population = list(mean = 100, stddev = 15 ),
  Contaminant = list(scores = 'NA', proportion = 0.1) 
)













#######################################################
#######################################################
#######################################################
#######################################################
#####                                             #####
#####  (3) Making the figures for the article     #####
#####                                             #####
#######################################################
#######################################################
#######################################################
#######################################################

setwd("C:\\Users\\DenisCousineau\\Documents\\1_Publications\\Mes Articles\\_1_Soumis\\MaC01-GRD4R\\Source\\Version5.0-2018.12.10")
source("GRD_20.R")
chemin="C:\\Users\\DenisCousineau\\Documents\\1_Publications\\Mes Articles\\_1_Soumis\\MaC01-GRD4R\\Texte\\Version8\\images"


## figure 1:
png(paste(chemin,"\\",'figure1.png',sep=""),width=800,height=300,res=96)
par(mfrow=c(1,3))
# panel 1: unaffected (p. 4)
dta <- GRD( SubjectsPerGroup = 1000 )
hist(dta$DV )
# panel 2: IQ example (p. 10)
dta <- GRD( 
  RenameDV = "IQ",
  Population=list(mean=100,stddev=15) 
)
hist(dta$IQ)
# panel 3: weibull (p. 12)
dta <- GRD(SubjectsPerGroup = 5000, 
  RenameDV = "RT",
  Population=list(
    scores = "rweibull(1, shape=2, scale=40)+250"
  )
)
hist(dta$RT,breaks=seq(240,415,by=5))
dev.off()



# figure 2:

# panel 1: a slope of 2 on Therapy (p. 14)
dta <- GRD(
  BSFactors = 'Surgery(yes, no) : Therapy(CBT, Control, Exercise)',
  WSFactors = 'Contrast(3)',
  SubjectsPerGroup = 100,
  Effects = list('Therapy' = slope(2)) 
)
library(lattice)
p1 <- histogram(~ DV.1 | Therapy, data = dta, breaks=seq(-7,+7,by=0.5),layout = c(3,1), aspect =1, ylab="Percent total" )
# panel 2: a range of 20
dta <- GRD(
  BSFactors = 'Surgery(no, yes) : Therapy(CBT, Control, Exercise)',
  WSFactors = 'Contrast(3)',
  SubjectsPerGroup = 100,
  Effects = list('Contrast' = range(4)) 
)
library(lsr)
dta2 <- wideToLong(dta, within = c("Contrast"),sep=".")
p2 <- histogram(~ DV | Contrast, data = dta2, breaks=seq(-7,+7,by=0.5),layout = c(3,1), aspect =1, ylab="Percent total")

# panel 3: a custom setting
dta <- GRD(
  BSFactors = 'Surgery(yes, no) : Therapy(CBT, Control, Exercise)',
  WSFactors = 'Contrast(3)',
  SubjectsPerGroup = 100,
  Effects = list(
    "Therapy"=custom(0,0,2) 
  ) 
)
dta2 <- wideToLong(dta, within = c("Contrast"),sep=".")
p3 <- histogram(~ DV | Therapy, data = dta2, breaks=seq(-7,+7,by=0.5),layout = c(3,1), aspect =1, ylab="Percent total")

png(paste(chemin,"\\","figure2.png",sep=""), 
    units="in", width=14, height=8, 
    pointsize=24, res=96)
print(p1, position=c(0.00, 0.50, 0.50, 1.00), more=TRUE)
print(p2, position=c(0.50, 0.50, 1.00, 1.00), more=TRUE)
print(p3, position=c(0.25, 0.00, 0.75, 0.50))
dev.off()



# figure 3:
png(paste(chemin,"\\",'figure3.png',sep=""),width=800,height=300,res=96)
par(mfrow=c(1,3))
# panel 1: multivariate normal
dta <- GRD( 
  WSFactors = 'Difficulty(1,2)',
  SubjectsPerGroup = 1000,
  Population=list(mean=0,stddev=20,rho=0.5)
)
plot(dta$DV.1, dta$DV.2)
# panel 2: multivariate normal
dta <- GRD( 
  WSFactors = 'Difficulty(1,2)',
  SubjectsPerGroup = 1000,
  Population=list(mean=c(10,2),stddev=c(1,0.2),rho=-0.85)
)
plot(dta$DV.1, dta$DV.2)
# panel 3: multivariate skew normal 
library(fMultivar)
xi    <- c(0,0)
Omega <- diag(2)
Omega[1,2] <- Omega[2,1] <- 0.5
alpha <- c(2,-6)

dta <- GRD( 
  WSFactors = 'Difficulty(1, 2)',
  SubjectsPerGroup = 1000,
  Population=list(rho=99,scores="sn::rmsn(1,xi,Omega,alpha)")
)
plot(dta$DV.1, dta$DV.2)
dev.off()




#Figure 4
png(paste(chemin,"\\",'figure4.png',sep=""),width=800,height=300,res=96)
par(mfrow=c(1,4))
# panel 1
dta <- GRD(SubjectsPerGroup = 5000, 
  Population=list( mean=100, stddev = 15 ), 
  Contaminant=list( mean=200, stddev = 15, proportion = 0.10 )
)
hist(dta$DV,breaks=seq(0,265,by=2.5))
#panel 2
dta <- GRD(SubjectsPerGroup = 10000, 
  Population=list( mean=100, stddev = 15 ), 
  Contaminant=list(
    scores="rweibull(1,shape=1.5, scale=30)+2*GM", proportion = 0.10
  )
)
hist(dta$DV,breaks=seq(0,365,by=2.5))
# panel 3
dta <- GRD( BSFactors="grp(2)",WSFactors = "Moment (2)", 
   SubjectsPerGroup = 1000,
   Effects = list("grp" = slope(100) ),
   Population=list(mean=0,stddev=20,rho=-0.85), 
   Contaminant=list(mean=100,stddev=4,rho=-0.99,proportion=0.2),
)
plot(dta[dta$grp == 1,]$DV.1,dta[dta$grp==1,]$DV.2, ylim=c(-150,150), xlim=c(-150,150))
plot(dta[dta$grp == 2,]$DV.1,dta[dta$grp==2,]$DV.2, ylim=c(-150,150), xlim=c(-150,150)) 
#done
dev.off()
