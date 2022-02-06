# Smooth-Survival-Curve

library(asaur)
library(survival)
# # # # # # #
#  smooth hazard
# # # # # # #

# install.packages("muhaz")  # do this once
library(survival)


library(muhaz)

time <- c(6,6,6,6,7,9,10,10, 11,13)

cens <- c(0,1,1,1,1,0,0, 1,0,1)

result.sm <- muhaz(time, cens)



# Here is the hazard plot:

plot(result.sm$haz.est ~ result.sm$est.grid, type="l",

xlab="time", ylab="hazard")



result.km <- survfit(Surv(time, cens) ~ 1)





plot(result.km, main = "Smooth Survival Curve", xlab = "Time", ylab = "Survival Probablity")

haz <- result.sm$haz.est[-length(result.sm$haz.est)]    # drop last element

times <- result.sm$est.grid

surv <- exp(-cumsum(haz*diff(times)))

lines(surv ~ times[-length(times)], lwd=2, col="blue")   # drop last element of "times"






