
SumOfPostitives = c(0:8)
SumOfInfected = c(0:8)
SumOfVaccinated = c(0:8)

###Prob of Attitude change if agent is confident###

#if agent holds novelty bias
px = exp(-4*(1-(SumOfPostitives/8)))#prob_of_change
px3 = 0.002 + (0.99/(1 + exp(-13*((SumOfPostitives/8)-0.5))))#***


#if neutral bias
slopes <-0.5# still some prob of changing to majority, but not as much as conform
b <-0
py = slopes*(1-(SumOfPostitives/8)) + b
py3 = 0.5
#if conformity bias
pz = exp(-4*((SumOfPostitives/8)))
pz3 = 0.99 - 0.99/(1+ exp(-13*((SumOfPostitives/8)-0.5)))

###prob of attitude change. if Hesitant###

#if agent holds novelty bias
pp = exp(-4*(SumOfPostitives/8))#
pp3 = 0.99 - 0.99/(1+ exp(-13*((SumOfPostitives/8)-0.5))) #same as pz3
pp2 = (exp(-4*(SumOfPostitives/8)))/2# Used for prob of Vacc


#if neutral bias
slopes <-0.5
b <-0
pq = slopes*(SumOfPostitives/8) + b
pq3 = 0.5
pq2 = (slopes*(SumOfPostitives/8) + b)/2# Vacc


#if conformity bias
pr = exp(-4*(1-(SumOfPostitives/8)))

pr3 = 0.002 + 0.99/(1+ exp(-13*((SumOfPostitives/8)-0.5)))

pr2 = (exp(-4*(1-(SumOfPostitives/8))))/2# Vacc



###Prob of vaccination based on D+###

#confident
 pm = 0.4*(SumOfInfected/8)+ 0.5#prob_of_vacc
 #pm = 0.7*(SumOfInfected/8)+ 0.3#prob_of_vacc
 #prob_of_vacc = slopes_vacc*(SumOfInfected/8)+ b[y]
 #5/22/23 New
 pm3 =  (1.01 - exp(-0.35* SumOfInfected))/2 + 0.5 #*** (The one used in model. Checked 12/4/24)

 #hesitant
 pn = 0.4*(SumOfInfected/8)+ 0.1
 pn3 = (1.2 - exp(-0.35* SumOfInfected))/2 + 0.1 #***
 
 
# prob_of_vacc based on V+
# Original: pp; pq; pr
# 5/22/23 New
topnov = 0.99 - 0.5/(1+ exp(-13*((SumOfVaccinated/8)-0.5))) #A+ Prob of vacc Nov
topconfm = 0.5+ 0.49/(1+ exp(-13*((SumOfVaccinated/8)-0.5))) #A+ Prob of vacc Conform
botnov = 0.5 - 0.5/(1+ exp(-13*((SumOfVaccinated/8)-0.5))) #A- Prob of vacc Nov
botconfm = 0.002+ 0.49/(1+ exp(-13*((SumOfVaccinated/8)-0.5)))#A- Prob of vacc conform

#8/14/23
neut_conf_trans = -0.0003125*(SumOfVaccinated)^2 - 0.12125*SumOfVaccinated + 0.99
neut_conf_trans2 = 0.003125*(SumOfVaccinated)^2 - 0.0875*SumOfVaccinated + 0.8#*** Probably supposed to be SumOfPoss
#neut_conf_trans2 = 0.1125*(SumOfVaccinated) + 0.05 #Change in main code 1/27/25

neut_hes_trans = -0.0003125*(SumOfVaccinated)^2 + 0.12625*SumOfVaccinated
neut_hes_trans2 = 0.003125*(SumOfVaccinated)^2 + 0.0375*SumOfVaccinated + 0.3

#neut_conf_vacc = -0.0003125*(SumOfVaccinated)^2 + 0.06375*SumOfVaccinated + 0.5 #***
#neut_conf_vacc = -0.003125*(SumOfVaccinated)^2 + 0.06375*SumOfVaccinated + 0.55
neut_conf_vacc = 0.05*(SumOfVaccinated) + 0.55 #(12/04/24)
#neut_hes_vacc = 0.003125*(SumOfVaccinated)^2 + 0.025*SumOfVaccinated + 0.1 #***
#neut_hes_vacc = 0.003125*(SumOfVaccinated)^2 + 0.03*SumOfVaccinated + 0.05
neut_hes_vacc = 0.05*(SumOfVaccinated) + 0.05 #(12/04/24)

### Testing Literature based functions
#p'(trait freq at next generation) = p (trait freq) + D(conformity level)p(1-p)(2p-1)
#



###Plots###
# pdf(file.path(dir_out_pdf, "PR1122_Functions.pdf"),height=12, width=9)#saves file as "noveltest.pdf"
# par(mfrow=c(2,1))
#par(mfrow = c(1,3)) 
#
##Switch if Confident 
# plot(SumOfPostitives, px, type= "o", col = "red",lwd=2, ylim = 0:1,xlab = "Number of (A+)", ylab = "Prob. of Change (A+)")#novelty
# lines(SumOfPostitives, py, type = "o", lwd=2)#neutral
# lines(SumOfPostitives, pz, type = "o", col = "blue", lwd=2)#conform
# legend("top", c("conform", "neutral", "novelty"), fill = c("blue","black", "red"))

# 5/22/23 New#
plot(SumOfPostitives, px3, type= "o", col = "red",lwd=2, ylim = 0:1,xlab = "Number of (A+)", ylab = "Prob. of Change (A+)")#novelty
#lines(SumOfPostitives, rep(py3,length(SumOfPostitives)), type = "o", lwd=2)#neutral
#lines(SumOfVaccinated, neut_conf_trans, type = "o", lwd=2)#neutral 8/14
lines(SumOfVaccinated, neut_conf_trans2, type = "o", lwd=2)#neutral 8/14 poly
lines(SumOfPostitives, pz3, type = "o", col = "blue", lwd=2)#conform
legend("top", c("conform", "neutral", "novelty"), fill = c("blue","black", "red"))
#dev.off()
#
#Switch if Hesitant
plot(SumOfPostitives, pp, type= "o", col = "red",lwd=2, ylim = 0:1, xlab = "Number of (A+)", ylab = "Prob. of Change (A-)")
#Vaccination freq. overtime
#lines(SumOfPostitives, pp2, type = "o", col = "red")
lines(SumOfPostitives, pq, type = "o", lwd=2)
lines(SumOfPostitives, pr, type = "o", col = "blue", lwd=2)
#lines(SumOfPostitives, pq2, type = "o")
#lines(SumOfPostitives, pr2, type = "o", col = "blue")
#
## 5/22/23 New# and 8/14
plot(SumOfPostitives, pp3, type= "o", col = "red",lwd=2, ylim = 0:1, xlab = "Number of (A+)", ylab = "Prob. of Change (A-)")
#lines(SumOfPostitives, rep(pq3,length(SumOfPostitives)), type = "o", lwd=2)#neutral
#lines(SumOfVaccinated, neut_hes_trans, type = "o", lwd=2)#neutral 8/14
lines(SumOfVaccinated, neut_hes_trans2, type = "o", lwd=2)#neutral 8/14 poly
lines(SumOfPostitives, pr3, type = "o", col = "blue", lwd=2)
legend("top", c("conform", "neutral", "novelty"), fill = c("blue","black", "red"))
#dev.off()
#
#Prob of Vaccinating Conf and Hesit-- based on D+
plot(SumOfInfected, pm, type = "o", col = "black", lwd=2, ylim = 0:1, xlab="Number of (D+)", ylab = "Prob of Vaccination(2)")#A+
lines(SumOfInfected, pn, type = "o", col = "green", lwd=2)#A-

#5/23/22 New: #Prob of Vaccinating Conf and Hesit-- based on D+ #***
plot(SumOfInfected, pm3, type = "o", col = "black", lwd=2, ylim = 0:1, xlab="Number of (D+)", ylab = "Prob of Vaccination(2)")#A+
lines(SumOfInfected, pn3, type = "o", col = "green", lwd=2)#A-
legend("topleft", c("A+", "A-"), fill = c("black", "green"))
#dev.off()
#

#Prob of Vaccinating Conf and Hesit Bias ["2" --> Hesitant]
plot(SumOfVaccinated, pp, type = "o", col = "red",lwd=2, ylim = 0:1, xlab="Number of (V+)", ylab = "Prob of Vaccination(1)")#A+
lines(SumOfVaccinated, pp2, type = "o", col = "red", lty=2, lwd=2)
lines(SumOfVaccinated, pq, type = "o", lwd=2)
lines(SumOfVaccinated, pq2, type = "o", lty=2, lwd=2)
lines(SumOfVaccinated, pr, type = "o", col = "blue", lwd=2)
lines(SumOfVaccinated, pr2, type = "o", col = "blue", lty=2, lwd=2)
legend("top", c("conform", "neutral", "novelty"), fill = c("blue","black", "red"))
legend("topright", lty = c(1,2),lwd = 2, c("A+", "A-"))
dev.off()

#5/23/22 New Prob. of Vaccinaing based on surrounding V+ #***
plot(SumOfVaccinated, topnov, type = "o", col = "red",lwd=2, ylim = 0:1, xlab="Number of (V+)", ylab = "Prob of Vaccination(1)")#A+
lines(SumOfVaccinated, botnov, type = "o", col = "red", lty=2, lwd=2)
#lines(SumOfVaccinated, rep(0.75, length(SumOfVaccinated)), type = "o", lwd=2)
lines(SumOfVaccinated, neut_conf_vacc, type = "o", lwd=2)

lines(SumOfVaccinated,topconfm , type = "o", col = "blue", lwd=2)
lines(SumOfVaccinated, botconfm, type = "o", col = "blue", lty=2, lwd=2)
#lines(SumOfVaccinated, rep(0.25, length(SumOfVaccinated)), type = "o", lty=2, lwd=2)
lines(SumOfVaccinated, neut_hes_vacc, type = "o", lty=2, lwd=2)

#legend("top", c("conform", "neutral", "novelty"), fill = c("blue","black", "red"))
#legend("topright", lty = c(1,2),lwd = 2, c("A+", "A-"))


dev.off()