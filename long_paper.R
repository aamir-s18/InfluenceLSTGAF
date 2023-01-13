library(car)
library(DescTools)
library(ggplot2)
library(ggpubr)
library(rstatix)

#import data
data <- read.csv("data/final/finalClean.csv")
data$gaf_lisaCl <- factor(data$gaf_lisaCl)
data$adjgaf_lisaCl <- factor(data$adjgaf_lisaCl)

#delete datapoint which have not been included in the Moran's I
data <- subset(data, !data$adjgaf_lisaCl=="6")
data$adjgaf_lisaCl <- as.factor(data$adjgaf_lisaCl)
summary(data$adjgaf_lisaCl)


# DELTA

###########################
# adjusted gaf
###########################

# check for equal variances 
leveneTest(data$DelaLstMedian9818 ~ data$adjgaf_lisaCl,
           data = data
)
res_aov1 <- aov(data$DelaLstMedian9818 ~ data$adjgaf_lisaCl,
                data = data
)

#test of normality -> not normal
shapiro.test(res_aov1$residuals)

# equal variances
oneway.test(data$DelaLstMedian9818 ~ data$adjgaf_lisaCl,
            data = data,
            var.equal = TRUE # assuming unequal variances
)
kruskal.test(data$DelaLstMedian9818 ~ data$adjgaf_lisaCl,
             data = data
             )

#check which are different
dunn.delta.adj <- dunn_test(DelaLstMedian9818 ~ adjgaf_lisaCl,
                           data = data          
)
dunn.delta.adj <- dunn.delta.adj %>% add_xy_position(x = "adjgaf_lisaCl")
dunn.delta.adj

#################################################
#  unadjusted gaf
#################################################

# check for equal variances 
leveneTest(data$DelaLstMedian9818 ~ data$gaf_lisaCl,
           data = data
)
res_aov2 <- aov(data$DelaLstMedian9818 ~ data$gaf_lisaCl,
                data = data
)

#test of normality -> not normal
shapiro.test(res_aov2$residuals)

# equal variances
oneway.test(data$DelaLstMedian9818 ~ data$gaf_lisaCl,
            data = data,
            var.equal = FALSE # assuming unequal variances
)

kruskal.test(data$DelaLstMedian9818 ~ data$gaf_lisaCl,
             data = data)

#check which are different
dunn.delta.gaf <- dunn_test(DelaLstMedian9818 ~ gaf_lisaCl,
         data = data          
)
dunn.delta.gaf <- dunn.delta.gaf %>% add_xy_position(x = "gaf_lisaCl")
dunn.delta.gaf



# LST 2018

###########################
# adjusted gaf
###########################

# check for equal variances 
leveneTest(data$lstMedian2018 ~ data$adjgaf_lisaCl,
           data = data
)
res_aov3 <- aov(data$lstMedian2018 ~ data$adjgaf_lisaCl,
                data = data
)

#test of normality -> not normal
shapiro.test(res_aov3$residuals)

# equal variances
oneway.test(data$lstMedian2018 ~ data$adjgaf_lisaCl,
            data = data,
            var.equal = FALSE # assuming unequal variances
)

kruskal.test(data$lstMedian2018 ~ data$adjgaf_lisaCl,
             data = data)

#check which are different
dunn.18med.adj <- dunn_test(lstMedian2018 ~ adjgaf_lisaCl,
         data = data          
)
dunn.18med.adj <- dunn.18med.adj %>% add_xy_position(x = "adjgaf_lisaCl")
dunn.18med.adj

#################################################
#  unadjusted
#################################################

# check for equal variances 
leveneTest(data$lstMedian2018 ~ data$gaf_lisaCl,
           data = data
)
res_aov4 <- aov(data$lstMedian2018 ~ data$gaf_lisaCl,
                data = data
)

#test of normality -> not normal
shapiro.test(res_aov4$residuals)

# equal variances
oneway.test(data$lstMedian2018 ~ data$gaf_lisaCl,
            data = data,
            var.equal = FALSE # assuming unequal variances
)

kruskal.test(data$lstMedian2018 ~ data$gaf_lisaCl,
             data = data)

#check which are different
dunn.18med.gaf <- dunn_test(lstMedian2018 ~ gaf_lisaCl,
         data = data          
)
dunn.18med.gaf <- dunn.18med.gaf %>% add_xy_position(x = "adjgaf_lisaCl")
dunn.18med.gaf




########################################################
# plotting
########################################################

#median delta
plot.delta.adj <- ggplot(data) +
  geom_boxplot(aes(x = adjgaf_lisaCl, y = DelaLstMedian9818, fill=adjgaf_lisaCl), 
               notch=TRUE) +
  scale_fill_manual(values=c("white",
                             "red",
                             "blue",
                             "#9696ff",
                             "#ff9696"),
                    name = "Moran's I clusters of adjusted GAF scores", 
                    labels = c("0: no", "1: High-High", "2: Low-Low",
                          "3: Low-High", "4:High-Low")) +
  scale_x_discrete(labels=c("no significance", "High-High", "Low-Low",
                            "Low-High", "High-Low")) +
  labs(#title="Plot of median LST difference\nbetween 1998 and 2018",
       x="Moran's I clusters of adjusted GAF scores", 
       y = "LST delta (°C)") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

plot.delta.gaf <- ggplot(data) +
  geom_boxplot(aes(x = gaf_lisaCl, y = DelaLstMedian9818, fill=gaf_lisaCl),
               notch=TRUE) +
  scale_fill_manual(values=c("white",
                             "red",
                             "blue",
                             "#9696ff",
                             "#ff9696"),
                    name = "Moran's I clusters of GAF scores", 
                    labels = c("0: no", "1: High-High", "2: Low-Low",
                               "3: Low-High", "4:High-Low")) +
  scale_x_discrete(labels=c("no significance", "High-High", "Low-Low",
                            "Low-High", "High-Low")) +
  labs(#title="Plot of median LST difference\nbetween 1998 and 2018",
       x="Moran's I clusters of GAF scores", 
       y = "LST delta (°C)") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

plot.18.adj <- ggplot(data) +
  geom_boxplot(aes(x = adjgaf_lisaCl, y = lstMedian2018, fill=adjgaf_lisaCl),
               notch=TRUE) +
  scale_fill_manual(values=c("white",
                             "red",
                             "blue",
                             "#9696ff",
                             "#ff9696"),
                    name = "Moran's I clusters of adjusted GAF scores", 
                    labels = c("0: no", "1: High-High", "2: Low-Low",
                               "3: Low-High", "4:High-Low")) +
  scale_x_discrete(labels=c("no significance", "High-High", "Low-Low",
                            "Low-High", "High-Low")) +
  labs(#title="Plot of median LST in 2018",
       x="Moran's I clusters of adjusted GAF scores", 
       y = "LST (°C)") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

plot.18.gaf <- ggplot(data) +
  geom_boxplot(aes(x = gaf_lisaCl, y = lstMedian2018, fill=gaf_lisaCl),
               notch=TRUE) +
  scale_fill_manual(values=c("white",
                             "red",
                             "blue",
                             "#9696ff",
                             "#ff9696"),
                    name = "Moran's I clusters of GAF scores", 
                    labels = c("0: no", "1: High-High", "2: Low-Low",
                               "3: Low-High", "4:High-Low")) +
  scale_x_discrete(labels=c("no significance", "High-High", "Low-Low",
                            "Low-High", "High-Low")) +
  labs(#title="Plot of median LST in 2018",
       x="Moran's I clusters of GAF scores", 
       y = "LST (°C)") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))


plot.delta.adj +
  stat_pvalue_manual(dunn.delta.adj, hide.ns = TRUE, 
                     tip.length = 0.01, bracket.shorten = 0.15)
plot.delta.gaf +
  stat_pvalue_manual(dunn.delta.gaf, hide.ns = TRUE, 
                     tip.length = 0.01, bracket.shorten = 0.15)
plot.18.adj +
  stat_pvalue_manual(dunn.18med.adj, hide.ns = TRUE, 
                     tip.length = 0.01, bracket.shorten = 0.15)
plot.18.gaf +
  stat_pvalue_manual(dunn.18med.gaf, hide.ns = TRUE, 
                     tip.length = 0.01, bracket.shorten = 0.15)
