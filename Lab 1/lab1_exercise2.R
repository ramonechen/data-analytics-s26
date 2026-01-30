library(readr)
library(nortest)

setwd("G:/My Drive/RPI/Data Analytics/Lab 1")

epi_data <- read_csv("epi_results_2024_pop_gdp.csv")

View(epi_data)

# Choose two other variables in the dataset (aside from EPI & MHP) and
# print/plot the following for both variables:

# ECO: The Ecosystem Vitality policy objective measures how well countries are
# preserving

# BDH: The Biodiversity and Habitat issue category assesses countries’ actions
# toward retaining natural ecosystems and protecting the full range of
# biodiversity within their borders. It consists of 12 indicators: protection of
# marine key biodiversity areas

ECO <- epi_data$ECO.new
BDH <- epi_data$BDH.new

# Variable summaries
summary(ECO)
summary(BDH)

# Variable boxplots
boxplot(ECO, BDH, names=c("ECO","BDH"))

# Histograms with overlayed theoretical probability distributions
hist(ECO, probability=TRUE)
lines(density(ECO,bw="SJ"))
hist(BDH, probability=TRUE)
lines(density(BDH, bw="SJ"))

# ECDF plots
plot(ecdf(ECO), do.points=FALSE, verticals=TRUE)
plot(ecdf(BDH), do.points=FALSE, verticals=TRUE)

# QQ plots of each variable against the normal distribution
qqnorm(ECO)
qqline(ECO)
qqnorm(BDH)
qqline(BDH)

# QQ plots of the 2 variables against each other
qqplot(ECO, BDH, xlab="Q-Q plot for ECO & BDH")

# Normality statistical tests for each variable
shapiro.test(ECO)
shapiro.test(BDH)

ad.test(ECO)
ad.test(BDH)

ks.test(ECO, BDH)

wilcox.test(ECO, BDH)

# A statistical test for whether the variables having identical distributions
var.test(ECO, BDH)
t.test(ECO, BDH)
