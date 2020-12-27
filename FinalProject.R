# John Sebastian Guerrero -- Data Science Final Project

# Question 1, Part 2: Monte-Carlo Simulation

# (a) Generate random vectors X and Y of size 1000 from Poisson distribution with parameters 1 and 2.


# Creating both vectors
X <- rpois(1, 1)
Y <- rpois(1, 2)


# (b) Find the probability X + Y = 4 using the Monte-Carlo Method.

# Creating a counter
counter <- 0

# Setting number of trials
N <- 1000

# Using Monte Carlo method to find out the probability
for(i in 1:N){
  X <- rpois(1, 1)
  Y <- rpois(1, 2)
  
  if(X + Y == 4){
    counter <- counter + 1
  }
  
}
  
# Finding probability by dividing the counter by number of trials
probability <- counter / N
probability  

# (c) Compare the previous result with the real probability.

# Finding the real probability
realProbability <- (exp(-3)*3^4)/24
realProbability

# Finding difference of both our probability and the real probability
probability - realProbability  # Very close to 0!




# -----------------------------------------------------------------------------------------------------------------



# Question 2, Part 2: Monte-Carlo Simulation

# (a) Suppose that the airline sells 210 tickets. Write down code that computes 
# the expected revenue for the airline. Does the airline earn or lose money on
# average by selling 10 extra tickets?


# Setting number of tickets sold
numTicketsSold <- 210

# This is the best case expected revenue for 210 tickets
expectedRevenueBestCase210 <- 210*300
expectedRevenueBestCase210

# Creating sum
sum <- 0

# Using for loop to add up respective weighted probabilities
for(i in 0:10){
  sum <- sum + (dbinom(200+ i, 210, 0.9) * (i)* 1000)
} 
sum

# Subtracting the probabilities with how much we will have to pay for bumped passangers from the best case expected revenue
expectedRevenue210 <- 210*300 - sum
expectedRevenue210
             
# We get an expected revenue of 62,997.1 dollars, Which is slightly more than our expected 60,000 from selling 200 tickets. We expect to make
# 2,997.1 dollars by selling an extra 10 tickets.


# (b) Now use the previous code to compute the expected revenue for the airline if they
# sell n tickets where n ranges from 200 to 225. Make a chart that shows the expected
# revenue for each value of n.

# Creating empty vector so we can fill it later
Revenues <- c()

# Creating another sum variable
sum2 <- 0

# Using nested for loops to add up the sums to subtract for each case from 200-225 individually. First loop goes through n=200 to n=225 and second 
# for loop adds up all cases for the tickets.
for(i in 0:25){
  
    for(j in 0:i){
      sum2 <- sum2 + dbinom(200 + j, 200 + i, 0.9) * j * 1000 
    }
  
  Revenues[i] <- ((200 + i)*300) - sum2
  sum2 <- 0
  
}
# Looking at the vector and finding max
Revenues
max(Revenues) # Max is 65,077.02

# Plotting chart to see where curve shows the maximum or optimum tickets sold. We see that if we sell an extra 20 tickets, we will expect maximum revenue.
plot(Revenues, xlab = "Extra Tickets Sold", ylab = "Expected Revenue", main = "Expected Revnues From Overbooking", type = "o", col = "blue")





# ----------------------------------------------------------------------------------------------------------------




# Question 3 Part 1: Hypothesis Test Using R

# (a) Import the data file Tuna.csv into R

# Creating vairable to store csv information
TUNA <- read.csv(file.choose())

# (b) The CEO is particularly interested in the weight of the light tuna in
# water. Generate a vector whose values are the light tuna in water.


# Creating vector that stores light tuna in water information
LightTuna <- TUNA$Light.Tuna.in.Water 
LightTuna


# (c) You will see that there are missing values marked by NA. Clean the data 
# file and remove all NA values from the vector. With this cleaned data, find the sample mean 
# of the light tuna in water.


# Creating logical vector with true value if there is a number value and false if there is NA

LightTunaClean <- !is.na(LightTuna)
LightTunaClean

# Creating new clean vector without any NA in the vector
TUNA_Light <- TUNA[LightTunaClean , ]
TUNA_Light

# Filtering out the new clean vector to just include the Light Tuna in water information
TUNA_Clean <- TUNA_Light$Light.Tuna.in.Water
TUNA_Clean

# Checking the mean of the clean vector and storing that in a new variable
sampleMean <- mean(TUNA_Clean)
sampleMean

# (d) The weight of the light tuna in water is supposed to be equal to 1.1 once per
# can. The CEO suspects that it is less than 1.1 once. Perform a t-test with alpha = 5%. 
# What is the conclusion of the test?

# Using t test function to perform t test on the clean vector
t.test(mu = 1.1, alternative = c("less"),TUNA_Clean)

# The conclusion of the test is that we reject the null hypothesis that the weights are
# less than 1.1 once, since the p-value is less than alpha.


