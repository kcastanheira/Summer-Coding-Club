library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
Quebec = read.csv("~/coding-club/Quebec/Quebec.csv")

#### dplyr####

head(Quebec, 2)
summary(Quebec)

Quebec %>% group_by(postal_code) %>% summarise(Mean = mean(stars))

Quebec %>% group_by(postal_code) %>% summarise(Mean = mean(stars), SD= sd(stars), N = n())

Quebec %>% group_by(postal_code, PriceRange) %>% summarise(Mean = mean(stars), SD= sd(stars), N = n())

## ggplot ####
Quebec %>% group_by(PriceRange) %>% summarise(sem= sd(stars)/sqrt(length(stars)), stars=mean(stars)) %>% 
  ggplot(aes(PriceRange, stars)) + geom_bar(stat = "summary", position="dodge") + theme_minimal()

Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=city, size=city)) + geom_point(alpha=0.5) + theme_minimal()

Quebec %>% 
  ggplot(aes(stars)) + geom_histogram() + theme_minimal()

Quebec %>% 
  ggplot(aes(Portuguese, stars)) + geom_violin() + theme_minimal()

Quebec %>% group_by(PriceRange) %>% summarise(sem= sd(stars)/sqrt(length(stars)), stars=mean(stars)) %>% 
  ggplot(aes(PriceRange, stars)) + geom_bar(stat = "summary", position="dodge") + geom_errorbar(aes(ymin=stars-sem, ymax=stars+sem), width=0, position="dodge") + theme_minimal()

#### Best Fit lines #######
Quebec %>% group_by(PriceRange) %>% summarise(sem= sd(stars)/sqrt(length(stars)), stars=mean(stars)) %>% 
  ggplot(aes(PriceRange, stars)) + geom_point() + geom_errorbar(aes(ymin=stars-sem, ymax=stars+sem), width=0) + geom_smooth(method="lm") + theme_minimal()

Quebec %>% group_by(PriceRange) %>% summarise(sem= sd(stars)/sqrt(length(stars)), stars=mean(stars)) %>% 
  ggplot(aes(PriceRange, stars)) + geom_point() + geom_errorbar(aes(ymin=stars-sem, ymax=stars+sem), width=0) + geom_smooth(method="lm", se=FALSE) + theme_minimal()

Quebec %>% group_by(PriceRange, Italian) %>% summarise(sem= sd(stars)/sqrt(length(stars)), stars=mean(stars)) %>% 
  ggplot(aes(PriceRange, stars, color=Italian)) + geom_point() + geom_errorbar(aes(ymin=stars-sem, ymax=stars+sem), width=0) + geom_smooth(method="lm", se=FALSE) + theme_minimal()

### Finishing touches #####
#### Color, Size, alpha ########
Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=PriceRange)) + geom_point() + theme_minimal()

Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=PriceRange)) + geom_point() + scale_color_gradientn(colours=c("deepskyblue", "hotpink1")) + theme_minimal()
```
```{r, message=FALSE, warning=FALSE}
Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=PriceRange)) + geom_point() + scale_color_gradientn(colours=c("deepskyblue", "white", "hotpink1")) + theme_minimal()
```

Or you can map color using discrete variables and not make a gradient:
  ```{r, message=FALSE, warning=FALSE}
Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=city)) + geom_point() + theme_minimal()

Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=city)) + geom_point() + scale_color_manual(values=c("deepskyblue", "hotpink1", "orchid")) + theme_minimal()

Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=city, size=city)) + geom_point(alpha=0.5) + theme_minimal()

Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=city, alpha=city)) + geom_point(size=2) + theme_minimal()

Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=city, size=French)) + geom_point(alpha=0.5) + theme_minimal()

#### Title & Labels ####

Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=PriceRange)) + geom_point() +ggtitle("TITLE") + xlab("X") + ylab("Y") 

Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=city, size=city)) + geom_point() +ggtitle("TITLE") + xlab("X") + ylab("Y") + labs(color="TEST 1", size="TEST 2")

#### Theme #####
Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=PriceRange)) + geom_point() 

Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=PriceRange)) + geom_point() + theme_classic()

Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=PriceRange)) + geom_point() + theme_void()

Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=PriceRange)) + geom_point() +
  ggtitle("TITLE") + theme(plot.title = element_text(hjust = 0.5))

Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=PriceRange)) + geom_point() +
  ggtitle("TITLE") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=PriceRange)) + geom_point() +
  ggtitle("TITLE") + theme(panel.background = element_blank())

Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=PriceRange)) + geom_point() +
  ggtitle("TITLE") + theme( panel.border = element_rect(colour = "black", fill=NA, size=0.5))

Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=PriceRange)) + geom_point() +
  ggtitle("TITLE") + theme(text = element_text(size=20))

Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=PriceRange)) + geom_point() +
  ggtitle("TITLE") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(),  panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                           legend.key.size = unit(1,"cm"), text = element_text(size=20), 
                           plot.title = element_text(hjust = 0.5))

#### Horizontal and Verticle Lines #######

Quebec %>% group_by(PriceRange) %>% summarise(sem= sd(stars)/sqrt(length(stars)), stars=mean(stars)) %>% 
  ggplot(aes(PriceRange, stars)) + geom_bar(stat = "summary", position="dodge") + geom_errorbar(aes(ymin=stars-sem, ymax=stars+sem), width=0, position="dodge") + theme_minimal() + geom_hline(yintercept=3)

Quebec %>% group_by(PriceRange) %>% summarise(sem= sd(stars)/sqrt(length(stars)), stars=mean(stars)) %>% 
  ggplot(aes(PriceRange, stars)) + geom_bar(stat = "summary", position="dodge") + geom_errorbar(aes(ymin=stars-sem, ymax=stars+sem), width=0, position="dodge") + theme_minimal() + geom_hline(yintercept=3, linetype='dashed')

Quebec %>% group_by(PriceRange) %>% summarise(sem= sd(stars)/sqrt(length(stars)), stars=mean(stars)) %>% 
  ggplot(aes(PriceRange, stars)) + geom_bar(stat = "summary", position="dodge") + geom_errorbar(aes(ymin=stars-sem, ymax=stars+sem), width=0, position="dodge") + theme_minimal() + geom_hline(yintercept=seq(0.5,4.5, 0.5), linetype=4)
  
Quebec %>% 
  ggplot(aes(review_count, stars)) + geom_point() + theme_minimal() + geom_vline(xintercept=0, linetype=3)

Quebec %>% 
  ggplot(aes(review_count, stars)) + geom_point() + theme_minimal() + geom_vline(xintercept=seq(0, 15, 2.5), linetype=5)

###### Facet Wrapping ########

Quebec %>%
  ggplot(aes(review_count, stars)) + geom_point(stat = "summary", position="dodge")  + theme_minimal() + facet_wrap(~PriceRange)

Quebec %>%
  ggplot(aes(review_count, stars)) + geom_point(stat = "summary", position="dodge")  + theme_minimal() + facet_wrap(~PriceRange, nrow=1)

Quebec %>% 
  ggplot(aes(review_count, stars)) + geom_point(stat = "summary", position="dodge")  + theme_minimal() + facet_wrap(~PriceRange, ncol=1)

Quebec %>% 
  ggplot(aes(review_count, stars)) + geom_point(stat = "summary", position="dodge")  + theme_minimal() + facet_wrap(~PriceRange+French, nrow=2, ncol=4)

Quebec %>% 
  ggplot(aes(review_count, stars)) + geom_point(stat = "summary", position="dodge")  + theme_minimal() + facet_wrap(~French+PriceRange, nrow=2, ncol=4)

Quebec$French = plyr::revalue(as.factor(Quebec$French), c("TRUE"="French", "FALSE"="Other"))
Quebec %>%
  ggplot(aes(review_count, stars)) + geom_point(stat = "summary", position="dodge")  + theme_minimal() + facet_wrap(~French+PriceRange, nrow=2, ncol=4)

######## Arranging Plots #########
plot1 <- Quebec %>% filter(city %in% c("MONTREAL", "LAVAL", "WESTMOUNT")) %>%
  ggplot(aes(PriceRange, stars, color=city, size=city)) + geom_point(alpha=0.5) + theme_minimal()

plot2 <- Quebec %>% group_by(PriceRange) %>% summarise(sem= sd(stars)/sqrt(length(stars)), stars=mean(stars)) %>% 
  ggplot(aes(PriceRange, stars)) + geom_bar(stat = "summary", position="dodge") + geom_errorbar(aes(ymin=stars-sem, ymax=stars+sem), width=0, position="dodge") + theme_minimal() + geom_hline(yintercept=3)

plot3 <- Quebec %>% 
  ggplot(aes(stars)) + geom_histogram() + theme_minimal()

plot4 <- Quebec %>% 
  ggplot(aes(French, stars)) + geom_violin() + theme_minimal()

grid.arrange(plot1, plot2, plot3, plot4)

grid.arrange(plot2, plot3, plot4, nrow=1)
grid.arrange(plot2, plot3, ncol=1, nrow=2)

grid.arrange(plot1, plot2, plot3, plot4,
             widths = c(2, 1),
             layout_matrix = rbind(c(1, 2),
                                   c(3, 4)))

grid.arrange(plot1, plot2, plot3, plot4,
widths = c(1, 1, 1),
layout_matrix = rbind(c(1, 1, 2),
c(NA, 3, 4)))


grid.arrange(plot1, plot2, plot3, plot4,
widths = c(1, 1, 1, 1, 1, 1),
layout_matrix = rbind(c(1, 1, 1, 1, 2, 2),
c(3, 3, 3, 4, 4, 4)))

grid.arrange(plot1 + ggtitle("A"), plot2 + ggtitle("B"), plot3 + ggtitle("C"), plot4 + ggtitle("D"),
             widths = c(1, 1, 1, 1, 1, 1),
             layout_matrix = rbind(c(1, 1, 1, 1, 2, 2),
                                   c(3, 3, 3, 4, 4, 4)))

#### Titles ########
grid.arrange(plot1 + ggtitle("A"), plot2 + ggtitle("B"), plot3 + ggtitle("C"), plot4 + ggtitle("D"),
             top=textGrob("Figure 1"),
             widths = c(1, 1, 1, 1, 1, 1),
             layout_matrix = rbind(c(1, 1, 1, 1, 2, 2),
                                   c(3, 3, 3, 4, 4, 4)))

#### Graph Inserts #######
plot2 <- Quebec %>% group_by(PriceRange) %>% summarise(sem= sd(stars)/sqrt(length(stars)), stars=mean(stars)) %>% 
  ggplot(aes(PriceRange, stars)) + geom_bar(stat = "summary", position="dodge") + geom_errorbar(aes(ymin=stars-sem, ymax=stars+sem), width=0, position="dodge") + theme_minimal() + geom_hline(yintercept=3)

plot2Grob = ggplotGrob(plot2 + theme(plot.background = element_rect(colour = "black")))

Quebec %>% 
  ggplot(aes(stars)) + geom_histogram() + theme_minimal() +
  annotation_custom(
    grob = plot2Grob,
    xmin = 1,
    xmax = 3,
    ymin = 150,
    ymax = 300)

plot2 <- Quebec %>% group_by(PriceRange) %>% summarise(sem= sd(stars)/sqrt(length(stars)), stars=mean(stars)) %>% 
  ggplot(aes(PriceRange, stars)) + geom_bar(stat = "summary", position="dodge") + geom_errorbar(aes(ymin=stars-sem, ymax=stars+sem), width=0, position="dodge") + theme_minimal() + geom_hline(yintercept=3)

Grob_insert = textGrob("Kevin Made This!!")

Quebec %>% 
  ggplot(aes(stars)) + geom_histogram() + theme_minimal() +
  annotation_custom(
    grob = Grob_insert,
    xmin = 1,
    xmax = 3,
    ymin = 150,
    ymax = 300)

plot2 <- Quebec %>% group_by(PriceRange) %>% summarise(sem= sd(stars)/sqrt(length(stars)), stars=mean(stars)) %>% 
  ggplot(aes(PriceRange, stars)) + geom_bar(stat = "summary", position="dodge") + geom_errorbar(aes(ymin=stars-sem, ymax=stars+sem), width=0, position="dodge") + theme_minimal() + geom_hline(yintercept=3)

Grob_insert = tableGrob(Quebec %>% group_by(Portuguese) %>% summarise(mean=mean(stars)), rows = c())

Quebec %>% 
  ggplot(aes(stars)) + geom_histogram() + theme_minimal() +
  annotation_custom(
    grob = Grob_insert,
    xmin = 1,
    xmax = 3,
    ymin = 150,
    ymax = 300)