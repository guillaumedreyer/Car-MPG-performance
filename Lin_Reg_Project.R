data(mtcars)

head(mtcars)
dim(mtcars)
str(mtcars)
help(mtcars)


# Remarks:

# 1. Unfortunatly, the data set does not contain data for the version
# of the same vehicule, which would lead
# to a more meaningful analysis. For this reason, we begin with
# aggregating the data set with respect to the (binary) variable 'am'
# (0: automatic, 1: manual). Also, 'mpg' clearly depends on the variable 
# 'cyl' (number of cylinders), let's aggregate using the latter as well.

# Let's ca is conformed  the correlation of each variable with respect 
# to 'mpg'. 

# 2. All the automatic cars are american, while 
# the manual ones are European cars. American cars are known to 
# be much heavier so this does not allow to draw a clear conclusion
# regarding which type of cars are better.

# 3. Fewer data regarding the manual cars (only 13 samples).

# 4 There is a clear correlation between 'mpg' and 'wt'. This is
# naturally visible in both groups. However, saying that 'mpg' for
# automatic is worse is misleading with our data, since all the 
# automatic cars are all American cars.





# Calculate the correlation of each variable with respect to 'mpg'.


# Some exploratory analysis... 
agreg_mtcars <- aggregate(mtcars, mtcars[c('am', 'cyl')], FUN = mean)[-c(1,2)]
agreg_mtcars <- agreg_mtcars[order(agreg_mtcars$am),]
agreg_mtcars

cor(mtcars$mpg, mtcars$cyl)




?mtcars

attach(mtcars)
f <-function(x){
        function(y){
                if (is.character(x)){cor(get(x), get(y))}
        else {cor(x, get(y))}
        }
}
mapply(f('mpg'), names(mtcars))
sort(abs(mapply(f('mpg'), names(mtcars))),decreasing = TRUE)
detach(mtcars)

cor(wt,cyl)
cor(wt,disp)
cor(cyl,disp)

sort(abs(mapply(f('mpg'), names(mtcars))),decreasing = TRUE)[-1]

summary(lm(wt ~ I(disp/100)))

#####

fit <- lm(mpg ~ cyl)



# Bunch of t tests...

attach(mtcars)
t.test(mtcars[mtcars$am==1,]$mpg, mtcars[mtcars$am==0,]$mpg)
t.test(mtcars[mtcars$am==1,]$wt, mtcars[mtcars$am==0,]$wt)
t.test(mtcars[mtcars$am==1,]$qsec, mtcars[mtcars$am==0,]$qsec)

t.test(mtcars[am==1 & cyl==4,]$mpg, mtcars[am==0 & cyl==4,]$mpg)
t.test(mtcars[am==1 & cyl==6,]$mpg, mtcars[am==0 & cyl==6,]$mpg)
t.test(mtcars[am==1 & cyl==8,]$mpg, mtcars[am==0 & cyl==8,]$mpg)
detach(mtcars)


attach(mtcars[order(mtcars$wt, decreasing = T),][-(1:3),])
t.test(mtcars[mtcars$am==1,]$mpg, mtcars[mtcars$am==0,]$mpg)
t.test(mtcars[mtcars$am==1,]$wt, mtcars[mtcars$am==0,]$wt)
t.test(mtcars[mtcars$am==1,]$qsec, mtcars[mtcars$am==0,]$qsec)

t.test(mtcars[am==1 & cyl==4,]$mpg, mtcars[am==0 & cyl==4,]$mpg)
t.test(mtcars[am==1 & cyl==6,]$mpg, mtcars[am==0 & cyl==6,]$mpg)
t.test(mtcars[am==1 & cyl==8,]$mpg, mtcars[am==0 & cyl==8,]$mpg)
detach(mtcars[order(mtcars$wt, decreasing = T),][-(1:3),])



t.test(mtcars[mtcars$am==1 & mtcars$cyl==4,]$wt,
       mtcars[mtcars$am==0 & mtcars$cyl==4,]$wt)

t.test(mtcars[mtcars$am==1 & mtcars$cyl==6,]$wt,
       mtcars[mtcars$am==0 & mtcars$cyl==6,]$wt)

t.test(mtcars[mtcars$am==1 & mtcars$cyl==8,]$wt,
       mtcars[mtcars$am==0 & mtcars$cyl==8,]$wt)



t.test(mtcars[mtcars$am==1 & mtcars$cyl==4,]$qsec,
       mtcars[mtcars$am==0 & mtcars$cyl==4,]$qsec)

t.test(mtcars[mtcars$am==1 & mtcars$cyl==6,]$qsec,
       mtcars[mtcars$am==0 & mtcars$cyl==6,]$qsec)

t.test(mtcars[mtcars$am==1 & mtcars$cyl==8,]$qsec,
       mtcars[mtcars$am==0 & mtcars$cyl==8,]$qsec)



# Plot some graphs...

help(plot)

attach(mtcars)
par(mfrow=c(1,1))
plot(wt, mpg, col='black', pch=19, xlim=c(1,5.5), ylim=c(10,35))
abline(lm(mpg ~ wt), col = 'black')
#plot(wt, mpg, pch=19)
detach(mtcars)


attach(mtcars)
par(mfrow=c(1,1))
plot(wt[am == 0], mpg[am == 0], col='red', pch=19, xlim=c(1,5.5), ylim=c(10,35))
points(wt[am == 1], mpg[am == 1], col='blue', pch=19, add=T)
abline(lm(mpg[am == 1] ~ wt[am == 1]), col = 'blue')
abline(lm(mpg[am == 0] ~ wt[am == 0]), col = 'red')

attach(mtcars[order(mtcars$wt, decreasing = T),][-(1:3),])
abline(lm(mpg[am == 0] ~ wt[am == 0]), col = 'green')
points(wt[am == 0], mpg[am == 0], col='green', pch=19)
detach(mtcars[order(mtcars$wt, decreasing = T),][-(1:3),])
#plot(wt, mpg, pch=19)
detach(mtcars)


mtcars[order(mtcars$wt, decreasing = T),][-(1:3),]


attach(mtcars[mtcars$cyl== 4,])
par(mfrow=c(1,1))
plot(wt[am == 0], mpg[am == 0], col='red', pch=19, xlim=c(1,5.5), ylim=c(10,35))
points(wt[am == 1], mpg[am == 1], col='blue', pch=19, add=T)
abline(lm(mpg[am == 1] ~ wt[am == 1]), col = 'blue')
abline(lm(mpg[am == 0] ~ wt[am == 0]), col = 'red')
detach(mtcars[mtcars$cyl== 4,])

attach(mtcars[mtcars$cyl== 6,])
par(mfrow=c(1,1))
plot(wt[am == 0], mpg[am == 0], col='red', pch=19, xlim=c(1,5.5), ylim=c(10,35))
points(wt[am == 1], mpg[am == 1], col='blue', pch=19, add=T)
abline(lm(mpg[am == 1] ~ wt[am == 1]), col = 'blue')
abline(lm(mpg[am == 0] ~ wt[am == 0]), col = 'red')
detach(mtcars[mtcars$cyl== 6,])


attach(mtcars[mtcars$cyl== 8,])
par(mfrow=c(1,1))
plot(wt[am == 0], mpg[am == 0], col='red', pch=19, xlim=c(1,5.5), ylim=c(10,35))
points(wt[am == 1], mpg[am == 1], col='blue', pch=19, add=T)
abline(lm(mpg[am == 1] ~ wt[am == 1]), col = 'blue')
abline(lm(mpg[am == 0] ~ wt[am == 0]), col = 'red')
detach(mtcars[mtcars$cyl== 8,])


# Bootstrapping

mtcars[order(mtcars$cyl), ]
mtcars[order(mtcars$wt, decreasing = T),][-(1:3),]

quantile(mtcars$wt)

hist(sort(mtcars$wt))

# Creating a new data set.

DF <- mtcars



par(mfrow = c(1,2))
hist(mtcars$wt,freq=F)
lines(density(mtcars$wt), add=T)

DF$wt <- cut(DF$wt,quantile(DF$wt),
             include.lowest = T)


as.integer(DF$wt)

hist(as.integer(DF$wt),freq=F)
abline(lm(DF$mpg~DF$wt))

plot(wt,mpg)
abline(lm(mpg~wt))

summary(lm(DF$mpg~DF$wt))
help(plot)







