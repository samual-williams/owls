# Plots for EcoRodMan report
library(ggplot2)
ggplot(questionTreatmentData, aes(fill=home, x = stage)) + geom_bar() # Could be a relationship


library(gcookbook) # For the data set
detach("package:dplyr", unload=TRUE)
library(plyr)

# Do a group-wise transform(), splitting on "Date"
ce <- ddply(cabbage_exp, "Date", transform,
            percent_weight = Weight / sum(Weight) * 100)
ggplot(ce, aes(x=Date, y=percent_weight, fill=Cultivar)) +
  geom_bar(stat="identity")

# my data
ce <- ddply(questionTreatmentData, "stage", transform,
            percent_weight = home / count(home) * 100)
ggplot(ce, aes(fill=home, x = stage)) + geom_bar() # Could be a relationship

###

library(plyr)
count(questionTreatmentData, 'home', 'stage')

detach('package:plyr', unload = TRUE)
library(dplyr)
# tally() is short-hand for summarise()
mtcars %>% tally()

mtcars %>% group_by(cyl) %>% tally()
#> # A tibble: 3 x 2
#>     cyl     n
#>   <dbl> <int>
#> 1     4    11
#> 2     6     7
#> 3     8    14
# count() is a short-hand for group_by() + tally()
mtcars %>% count(cyl)


questionTreatmentData %>% tally()
??tally
mtcars %>% count(cyl)


################################################################
################################################################


# Plots for technical report

# legend
p1Legend <- ggplot(questionTreatmentData, aes(fill=home, x = stage)) + geom_bar() + labs(fill="Response")
legend <- cowplot::get_legend(p1Legend)
legend <- plot_grid(legend, NULL, nrow = 1)
legend


# p1
p1Treatment <- ggplot(questionTreatmentData, aes(fill=home, x = stage)) + geom_bar() + 
  theme(legend.position="none")
p1Treatment

p1Control <- ggplot(questionControl, aes(fill=home, x = stage)) + geom_bar() + 
  theme(legend.position="none")
p1Control

library(cowplot)
(p1Combined <- plot_grid(p1Treatment, p1Control, nrow = 1, labels = c("1", NULL)))
p1Combined

### p2
(p2Treatment <- ggplot(questionTreatmentData, aes(fill=like.nesting.in.roof, x = stage)) + geom_bar() + theme(legend.position="none"))

(p2Control <- ggplot(questionControl, aes(fill=like.nesting.in.roof, x = stage)) + geom_bar() + theme(legend.position="none"))

(p2Combined <- plot_grid(p2Treatment, p2Control, nrow = 1, labels = c("2", NULL)))
p2Combined

### p3
(p3Treatment <- ggplot(questionTreatmentData, aes(fill=would.put.nest.box.in.yard, x = stage)) + geom_bar() + theme(legend.position="none"))

(p3Control <- ggplot(questionControl, aes(fill=would.put.nest.box.in.yard, x = stage)) + geom_bar() + theme(legend.position="none"))

(p3Combined <- plot_grid(p3Treatment, p3Control, nrow = 1, labels = c("3", NULL)))
p3Combined



###
# Binary plots combined
p1to3 <- plot_grid(p1Combined, p2Combined, p3Combined, ncol = 1)
p1to3 <- plot_grid(NULL, p1to3, nrow = 2, rel_heights = c(1,10))
p1to3 <- p1to3 + draw_label("Treatment", x = 0.3, y = 0.95)
p1to3 <- p1to3 + draw_label("Control", x = 0.8, y = 0.95)
p1to3
p1to3 <- plot_grid(p1to3, legend, nrow = 2, rel_heights = c(10,2))
p1to3


#
# p4

p4Treatment <- ggplot(questionTreatmentData, aes(fill=attitude.to.owls, x = stage)) + geom_bar()
p4Treatment

p4Control <- ggplot(questionControlData, aes(fill=attitude.to.owls, x = stage)) + geom_bar() 
p4Control
 
# Take 2
p1 <- ggplot(questionTreatmentData, aes(fill=home, x = stage)) + geom_bar() 
p1

p2 <- ggplot(questionTreatmentData, aes(fill=like.nesting.in.roof, x = stage)) + geom_bar() # Could be a relationship
p2

p3 <- ggplot(questionTreatmentData, aes(fill=would.put.nest.box.in.yard, x = stage)) + geom_bar() # Could be a relationship
p3

fig1 <- plot_grid(p1, p2, p3, ncol = 1, labels = c("1", "2", "3"))
fig1

p4 <- ggplot(questionTreatmentData, aes(fill=attitude.to.owls, x = stage)) + geom_bar() # Looks like a relationship
p4

p5 <- # Check plot of resonse variable
  ggplot(questionTreatmentData, aes(fill=afraid.of.owls, x = stage)) + geom_bar() # Looks like a relationship
p5

p6 <- ggplot(questionTreatmentData, aes(fill=what.do.if.owl.lands.on.the.roof, x = stage)) + geom_bar() # Looks like a relationship
p6

p7 <- ggplot(questionTreatmentData, aes(fill=what.did.when.last.saw.owl, x = stage)) + geom_bar() # Looks like a relationship
p7

fig2 <- plot_grid(p4, p5, p6, p7, ncol = 1, labels = c("4", "5", "6", "7"))
fig2
