# Zachary Fogelson HW1

######## Libraries #########

# install.packages("xts")
# install.packages("xtsExtra", repos="http://R-Forge.R-project.org")

library(xts)
# library(xtsExtra)


######## Problem 2) ########

# Assuming that conret is in current working directory
conret <- read.csv("conret.csv", header = TRUE)

# a
titles <- "Histogram of %s Monthly Returns"
xlabel <- "Monthly Return"

# USA
hist(conret$usa, 
     main   = sprintf(sprintf(titles, "USA")), 
     xlab   = xlabel, 
     border ="black", 
     col    = "blue",
     breaks = 10)

# Italy
hist(conret$italy, 
     main   = sprintf(titles, "Italy"), 
     xlab   = xlabel, 
     border = "black", 
     col    = "red",
     breaks = 10)

# b
# Based on these results, Italy's returns appear more spread out because its
# monthly returns range from -.2 to .2 while the USA's retunrs only range from
# -.1 to .1.

# c

# USA
print(mean(conret$usa))  # 0.01345794
print(sd(conret$usa))  # 0.03328275

# Italy
print(mean(conret$italy))  # 0.006074766
print(sd(conret$italy))  # 0.07126561

# Because the standard deviation of Italy's monthly returns is larger 
# than the standard deviation of the United States' monthly returns
# this supports the conclusion that Italy's monthly returns are more spread
# out than those of the United States.

# d
# According to the empirical rule 95% of all data should be within 2 standard
# deviations of the mean

getRange <- function(m, s){
  list(low <- m - 2 * s, high <- m + 2 * s)
}

percentageResultsInRange <- function(arr, r){
    acceptable <- arr[arr > r[1] & arr < r[2]]
    length(acceptable) / length(arr)
}

usaRange <- getRange(mean(conret$usa), sd(conret$usa))
italyRange <- getRange(mean(conret$italy), sd(conret$italy))

print(percentageResultsInRange(conret$usa, usaRange))  # 0.953271
print(percentageResultsInRange(conret$italy, italyRange))  # 0.9439252

# Yes each of the intervals are approximately correct, the USA has slightly 
# more data within two standard deviations while the Italian data has 
# slightly less

# e
plot(conret$usa, 
     conret$italy, 
     main = "US returns vs. Italian returns", 
     xlab = "US Returns",
     ylab = "Italian Returns")

# Based on the scatter plot, I expect the correlation between US and Italian 
# returns is approximately .6

# f
print(cov(conret$usa, conret$italy, use = "all.obs"))  # 0.0005693617
print(cor(conret$usa, conret$italy, use = "all.obs"))  # 0.240043

# My guess compared to the actual correlation was far too high


######## Problem 3) ########
# a
# Assuming that oil_gdp.csv is in current working directory
oil_gdp <- read.csv("oil_gdp.csv", header = TRUE)

plot(oil_gdp$date, oil_gdp$G_CPI, col="red", xlab = "CPI", ylab = "Time", main = "CPIs vs Time")
points(oil_gdp$date, oil_gdp$G_CPI.1, col="black")
legend('bottomleft', c("G_CPI","G_CPI.1"), pch=1, col=c('red', 'black'), bty='o', cex=.75)
# The G_CPI is more volatile then than the G_CPI.1 series

# b
print(mean(oil_gdp$G_CPI))  # 0.03394408
print(var(oil_gdp$G_CPI))  # 0.001356836

print(mean(oil_gdp$G_CPI.1))  # 0.03505742
print(var(oil_gdp$G_CPI.1))  # 0.0004995316

# c
# Comparing the plots, when G_CPI is high, G_CPI.1 is also high but less so. 
# This is most apparent in the early 1980s but there are also times when 
# G_CPI collapses and G_CPI.1 appears to track it such as circa 1986 as well as
# circa 2009.

# d 
print(cov(oil_gdp$G_CPI, oil_gdp$G_CPI.1, use = "all.obs"))  # 0.0004749797
print(cor(oil_gdp$G_CPI, oil_gdp$G_CPI.1, use = "all.obs"))  # 0.5769391

# e
# Because the correlation is positive, it means that as the G_CPI increases 
# it tends to be the case that G_CPI.1 also increases. The relationship
# also appears to be strong because the magnitude of correlation is neither
# very strong nor weak. 

# f
print(cor(oil_gdp$G_CPI.1[seq(length=length(oil_gdp$G_CPI)-1, from=2)],
    oil_gdp$G_CPI[seq(length=length(oil_gdp$G_CPI)-1, from=1)]))  # 0.8505302

######## Problem 4) ########
# a
conret$usaItaly <- .5 * conret$usa + .5 * conret$italy
print(mean(conret$usaItaly))  # 0.009766355
print(sd(conret$usaItaly))  # 0.04279384

# b
linComboMean <- function(w1, m1, w2, m2){
  w1*m1 + w2*m2
}
linComboVar <- function(w1, sd1, w2, sd2, cr){
  w1^2 * sd1^2 + w2^2 * sd2^2 + 2 * w1 * w2 * cr * sd1 * sd2
}

print(linComboMean(.5, mean(conret$usa), .5, mean(conret$italy)))  # 0.009766355
print(linComboVar(.5, sd(conret$usa),
                  .5, sd(conret$italy),
                  cor(conret$usa, conret$italy, use = "all.obs"))^(1/2))  # 0.04279384


# c
# .25 USA, .75 Italy
print(linComboMean(.25, mean(conret$usa), .75, mean(conret$italy)))  # 0.007920561
print(linComboVar(.25, sd(conret$usa),
                  .75, sd(conret$italy),
                  cor(conret$usa, conret$italy, use = "all.obs"))^(1/2))  # 0.05603179

# .75 USA, .25 Italy
print(linComboMean(.75, mean(conret$usa), .25, mean(conret$italy)))  # 0.01161215
print(linComboVar(.75, sd(conret$usa),
                  .25, sd(conret$italy),
                  cor(conret$usa, conret$italy, use = "all.obs"))^(1/2))  # 0.03397115

investRatio <- list(0, .25, .5, .75, 1)
investMean <- list()
investSD <- list()

for (el in investRatio){
  investMean <- c(investMean, linComboMean(el, mean(conret$usa), 1 - el, mean(conret$italy)))
  investSD <- c(investSD, linComboVar(el, sd(conret$usa),
                          1-el, sd(conret$italy),
                          cor(conret$usa, conret$italy, use = "all.obs"))^(1/2))
}


plot(investSD, 
     investMean, 
     main = "Mean Return vs SD of Portfollio", 
     xlab = "Standard Deviation",
     ylab = "Mean")

text(x = investSD, y = investMean, labels = c("0.0 USA", ".25 USA", ".5 USA", ".75 USA", "1.0 USA"))

