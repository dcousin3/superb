## ---- echo = FALSE, warning=FALSE, message = FALSE, results = 'hide', warning = FALSE----
cat("this will be hidden; use for general initializations.\n")
library(superb)
library(ggplot2)
options(superb.feedback = 'none')

## ---- eval=TRUE---------------------------------------------------------------
library(superb)
library(ggplot2)
t <- superbPlot(ToothGrowth, 
    BSFactors = c("dose","supp"), 
    variables = "len" )

## ---- fig.width = 4, eval=TRUE, fig.cap="**Figure 1: A basic plot**"----------
t + geom_text(aes(x=dose, y=center, label=center) )

## ---- fig.width = 4, eval=TRUE, fig.cap="**Figure 2: A better-looking plot**"----
t + geom_text(aes(x=dose, y=center, label=center), 
      position = position_dodge(0.9),
      vjust = 1.5,color = "black" )

## ---- eval=FALSE--------------------------------------------------------------
#  t + geom_text(aes(x=dose, y=center, label=round(center)),
#        position=position_dodge(0.9),vjust=1.5,color="black")
#  t + geom_text(aes(x=dose, y=center, label=sprintf('%.1f', center)),
#        position=position_dodge(0.9),vjust=1.5,color="black")
#  t + geom_text(aes(x=dose, y=center, label=sprintf('%.1f', center), color=supp),
#        position=position_dodge(0.9), vjust=-1.5)

## ---- fig.width = 4, eval=TRUE, fig.cap="**Figure 3: A plot with conditions on the bars**"----
t + geom_text(aes(x=dose, y=center, label=supp), # changed "label"
      position=position_dodge(0.9),
      vjust=1.5,color="black")

## ---- fig.width = 4, eval=TRUE, fig.cap="**Figure 4: A plethora of labels**"----
t + geom_text(aes(x=dose, y=center, label=sprintf('%.1f', center)), color="black", position=position_dodge(0.9), vjust=-1) + 
 geom_text(aes(x=dose, y=center,label=supp), position=position_dodge(0.9), vjust=1.5, color="black") +
 geom_text(aes(x=dose, y=center+upperwidth, label=round(center+upperwidth)), position=position_dodge(0.9), vjust=-1, color="gray43") 

## ---- eval=TRUE---------------------------------------------------------------
d <- superbData(ToothGrowth, 
    BSFactors = c("dose","supp"), 
    variables = "len" )
d$summaryStatistics

## ---- eval=TRUE---------------------------------------------------------------
# taken from library(babynames)
# head(unique(babynames[order(-babynames$prop),]$name),60)
names=c(
 "John",        "William",     "Mary",        "Robert",      "James",      
 "Linda",       "Michael",     "Charles",     "George",      "David",     
 "Jennifer",    "Shirley",     "Richard",     "Barbara",     "Jason",      
 "Lisa",        "Betty",       "Christopher", "Dorothy",     "Patricia",   
 "Helen",       "Jessica",     "Ashley",      "Donald",      "Anna",       
 "Joseph",      "Deborah",     "Frank",       "Mark",        "Matthew",    
 "Thomas",      "Debra",       "Susan",       "Margaret",    "Carol",      
 "Amanda",      "Brian",       "Joshua",      "Henry",       "Harry",      
 "Ruth",        "Amy",         "Emma",        "Edward",      "Ronald",     
 "Daniel",      "Gary",        "Elizabeth",   "Melissa",     "Sandra",     
 "Michelle",    "Karen",       "Kimberly",    "Joan",        "Brittany",   
 "Judith",      "Larry",       "Cynthia",     "Andrew",      "Steven")
# append the names as the last columns of ToothGrowth
ToothGrowth$names <- names

head(ToothGrowth)

## ---- fig.width = 4, eval=TRUE, fig.cap="**Figure 5: Individual cases' labels**"----
t + geom_text(aes(x=factor(dose), y=len, label=names),
      data=ToothGrowth, # new: I add the original data frame
      color="black", position=position_dodge(0.9), vjust=-1.) 

