#Specify file path for Mac OS 
path <- file.path("~", "desktop", "lasvegas_tripadvisor.csv")
# Import the data and store in a vaviable 
mydata <- read.table(path, header = TRUE, sep = ",",
                     stringsAsFactors = FALSE) #
View(mydata)
# The 'Traveler.type' variable is the first categorical variable I chose to explore
# Create frequecy table of each traveler type
trav.type <- table(mydata$Traveler.type)
# Display proportion of each traveler type 
trav.type / length(mydata$Traveler.type)
# Create barplot for traveler type
plot_ly(mydata, x = ~Traveler.type, type = "histogram")

# Create pie chart for traveler type
plot_ly(mydata, labels = ~Traveler.type, type = "pie")
# The 'User.country' variable is the second categorical variable I chose to explore
# Create frequency table of each reviewer country
country <- table(mydata$User.country)
head(sort(country, decreasing = TRUE))
# Display proportion of each reviewer country
country / length(mydata$User.country)
head(sort(country / length(mydata$User.country), decreasing = TRUE))
# Create barplot from sorted table.
plot_ly(mydata, x = ~User.country, type = "histogram")
# The 'Score' variable is the first numerical varible I chose to explore
scores <- mydata$Score
mean(scores)
sd(scores)
median(scores)
table(scores)
range(scores)

plot_ly(mydata, x = ~Score, type = "histogram")
plot_ly(mydata, labels = ~Score, type = "pie")
# The 'Nr..hotel.reviews' variable is the second numerical variable I chose to analyze
mean(mydata$Nr..hotel.reviews)
median(mydata$Nr..hotel.reviews)
range(mydata$Nr..hotel.reviews)
var(mydata$Nr..hotel.reviews)
sd(mydata$Nr..hotel.reviews)
fivenum(mydata$Nr..hotel.reviews)
summary(mydata$Nr..hotel.reviews)
quantile(mydata$Nr..hotel.reviews, c(0, 0.25, 0.5, 0.75, 1))
IQR(mydata$Nr..hotel.reviews)
table(mydata$Nr..hotel.reviews)
plot_ly(mydata, y = ~Nr..hotel.reviews, type = "histogram")
plot_ly(mydata, x = ~Nr..hotel.reviews, type = "box", name = "Respondents")
# Bivariate analysis
attach(mydata)
ftable(table(Score, Pool, Free.internet),
       col.vars = c("Free.internet", "Pool"))
table(Score, Pool)
table(Score, Casino)
par(mfrow = c(1,2))
mosaicplot(table(Score,Pool), color = c("red","green"),
           main = "Pool vs. no pool")
mosaicplot(table(Score,Free.internet), color = c("red","green"),
           main = "Internet vs. no internet")
par(mfrow = c(1,1))
mosaicplot(table(Score,Casino), color = c("red","green"),
           main = "Scores for casino vs. no casino")
prop.table(table(Traveler.type))*100
table(Score, Traveler.type)
mosaicplot(prop.table(table(Score, Traveler.type)),
           color = rainbow(5), main = "Scores among demographics")

# Central Limit Thorem
library(sampling)
par(mfrow = c(2,2))
set.seed(223)
samples <- 1000
sample.size <- 10

xbar <- numeric(samples)

for (i in 1:samples) {
  xbar[i] <- mean(rnorm(sample.size,
                        mean = 4.111789, sd = 1.014007))
}

hist(xbar, prob = TRUE,xlim = c(3,5), 
     ylim = c(0,2), main = "Sample size of 10",
     xlab = "Scores")

set.seed(234)
samples <- 1000
sample.size <- 20

xbar <- numeric(samples)

for (i in 1:samples) {
  xbar[i] <- mean(rnorm(sample.size,
                        mean = 4, sd = 1))
}

hist(xbar, prob = TRUE,xlim = c(3,5), 
     ylim = c(0,2), main = "Sample size of 20",
     xlab = "Scores")

set.seed(456)
samples <- 1000
sample.size <- 30

xbar <- numeric(samples)

for (i in 1:samples) {
  xbar[i] <- mean(rnorm(sample.size,
                        mean = 4, sd = 1))
}

hist(xbar, prob = TRUE,xlim = c(3,5), 
     ylim = c(0,2), main = "Sample size of 30",
     xlab = "Scores")

set.seed(678)
samples <- 1000
sample.size <- 40

xbar <- numeric(samples)

for (i in 1:samples) {
  xbar[i] <- mean(rnorm(sample.size,
                        mean = 4, sd = 1))
}

hist(xbar, prob = TRUE,xlim = c(3,5), 
     ylim = c(0,2), main = "Sample size of 40",
     xlab = "Scores")
par(mfrow = c(1,1))

# Sampling
# SRSWR
set.seed(987)
n <- 10000
s <- srswr(n,nrow(mydata))
rows <- (1:nrow(mydata))[s!=0]
rows <- rep(rows, s[s!=0])
sample.1 <- mydata[rows, ]
barplot(table(sample.1$Score))
mean(sample.1$Score)
sd(sample.1$Score)
# SRSWOR
set.seed(321)
s1 <- srswor(20, nrow(mydata))
sample.2 <- mydata[s1 != 0, ]
sample.2$Score
table(sample.2$Score)
barplot(table(sample.2$Score))
mean(sample.2$Score)
sd(sample.2$Score)
# Systematic sampling
set.seed(765)
N <- nrow(mydata)
n <- 20
k <- ceiling(N / n)
k
r <- sample(k, 1)
r
  #select every kth item
s2 <- seq(r, by = k, length = n)
sample.3 <- mydata[s2, ]
sample.3$Score
table(sample.3$Score)
barplot(table(sample.3$Score))
mean(sample.3$Score)
sd(sample.3$Score)
# Stratified sampling
set.seed(212)
order.index <- order(mydata$Score)
ord.data <- mydata[order.index, ]
freq <- table(mydata$Score)
st.sizes <-ceiling(20 * freq / sum(freq))
st.1 <- strata(ord.data, stratanames = c("Score"),
               size = st.sizes, method = "srswor",
               description = TRUE)
sample.5 <- getdata(ord.data, st.1)
mean(sample.5$Score)
sd(sample.5$Score)

