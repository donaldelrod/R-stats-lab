#1
rolls1=sample(x=c(1,2,3,4,5,6), size=4000, replace=TRUE)
table(rolls1)
#2
four.rolls = matrix(rolls1, nrow=1000, ncol=4)
#3
min.roll = apply(four.rolls, 1, min)
#4
sum(min.roll == 1)
#5
rolls2 = sample(x=c(1,2,3,4,5,6), size = 48000, replace=TRUE)
two.rolls = matrix(rolls2, nrow=24000, ncol=2)
sum.rolls = apply(two.rolls, 1, sum)
twodozen = matrix(sum.rolls, nrow = 24, ncol = 1000)
min.pair = apply(twodozen, 2, min)
sum(min.pair == 2)
#6
p1.est = sum(min.roll == 1)/1000
p2.est = sum(min.pair == 2)/1000
#7
results1 <- 0
results2 <- 0
for (i in 1:25) {

  rolls1.test=sample(x=c(1,2,3,4,5,6), size=4000, replace=TRUE)
  table(rolls1.test)
  four.rolls.test = matrix(rolls1.test, nrow=1000, ncol=4)
  min.roll.test = apply(four.rolls.test, 1, min)
  results1[i] <- sum(min.roll.test == 1)
  
  rolls2.test = sample(x=c(1,2,3,4,5,6), size = 48000, replace=TRUE)
  two.rolls.test = matrix(rolls2.test, nrow=24000, ncol=2)
  sum.rolls.test = apply(two.rolls.test, 1, sum)
  twodozen.test = matrix(sum.rolls.test, nrow = 24, ncol = 1000)
  min.pair.test = apply(twodozen.test, 2, min)
  results2[i] <- sum(min.pair.test == 2)
  
}
#8
prob.ests1 <- results1/1000
prob.ests2 <- results2/1000
se1 <- sd(prob.ests1)
se2 <- sd(prob.ests2)
#9
p1 = 1 - (5/6)^4
p2 = 1 - (35/36)^24
#10
abs(p1.est - p1)/se1
abs(p2.est - p2)/se2