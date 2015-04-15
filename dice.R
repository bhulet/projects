#This program will roll a 'fair' dice until the sum of the dice rolls is >=  m, where m = "  "
#It wil do this for a number of trials where, trials = "  "
#Then calculate the mean and standard deviation for the (sum of the dice rolls - m), over all trials
#Calculate the mean and standard deviation for the number of rolls taken, over all trials

dice <- function(m = 20, trials = 1000){
set.seed(7)
b <- numeric(m)
c <- numeric(trials)
d <- numeric(trials)
for (i in 1:length(c)){
  for (j in 1:m){
    if (sum(b) < m){
      b[j] <- sample(1:6, 1, T)
    }
    b[which(b == 0)] = NA
    b = b[!is.na(b)]
  }
  c[i] <- (sum(b) - m)
  d[i] <- length(b)
  b <- numeric(m)
}
ans <- mean(c)
ans_sd <- sd(c)
rolls <- mean(d)
rolls_sd <- sd(d)
message("When 'M' is equal to ", m, ", with ", trials, " trials")
display <- data.frame(ans, ans_sd)
names(display) <- c(" Mean Sum Minus M", "       Std. Dev.")
print(display)
display2 <- data.frame(rolls, rolls_sd)
names(display2) <- c(" Mean Number of Rolls", "   Std. Dev.")
print(display2)
}
