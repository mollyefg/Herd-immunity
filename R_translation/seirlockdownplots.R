###
require(deSolve)

#This is a script file to produce Figures 1 and 2 of the paper.  

#define parameters
gamma=1/4
sigma=1/3
Rzero=2.5

tlockdown1=30
alphalockdown=.8
tlockdown2=135 
alphalockdown2=1
epsilon=10^(-4)
tend=255

# specify the number of classes here:
k = 18

# and stepsize of sims:
dt = 1

pactivity = c(.5, 1, 2)
activitypi= c(.25, .5, .25)

# call external functions
source('multiseirjackoactivitydet.R')
out0 = multiseirjackoactivitydet(gamma, sigma, pactivity, activitypi, Rzero, epsilon, tend, k, dt)	# returns tdet, then ydet

alphalockdown1=0.8
source('multiseirjackoactivitydetlockdown2.R')
out1 = multiseirjackoactivitydetlockdown2(gamma, sigma, pactivity, activitypi, Rzero, tlockdown1, alphalockdown1, tlockdown2, alphalockdown2, epsilon, tend, k, dt)

alphalockdown1=0.6
out2 = multiseirjackoactivitydetlockdown2(gamma, sigma, pactivity, activitypi, Rzero, tlockdown1, alphalockdown1, tlockdown2, alphalockdown2, epsilon, tend, k, dt)

alphalockdown1=8/15;
out3 = multiseirjackoactivitydetlockdown2(gamma, sigma, pactivity, activitypi, Rzero, tlockdown1, alphalockdown1, tlockdown2, alphalockdown2, epsilon, tend, k, dt)

### plot the results: first number infected (daily), then cumulative infections, for the different lockdown options
pdf('plots1and2.pdf')
par(mfrow = c(2, 1))
par(mai = c(1, 1, .5, .3))

cols = c("blue", "orange", "yellow", "purple")
weight = 2
labsize = 1.5
axsize = 1.5
legendsize = 1.2
mainsize = 1.5

sums = array(dim = c(4,length(out0[,1])))	# sum up all individuals in the the 'infected' classes over time
for(i in 1:length(out0[,1])){
	sums[1,i] = sum(out0[i,38:55])
	sums[2,i] = sum(out1[i,38:55])
	sums[3,i] = sum(out2[i,38:55])
	sums[4,i] = sum(out3[i,38:55])
	}


plot(out0[,1],sums[1,], xlim = c(0, 255), ylim = c(0, 0.1), xlab = 'time (days)', ylab = 'infectives', col = cols[1], type = 'l', lwd = weight, cex.axis = axsize, cex.lab = labsize, cex.main = mainsize)
lines(out1[,1],sums[2,], col = cols[2], lwd = weight)
lines(out2[,1],sums[3,], col = cols[3], lwd = weight)
lines(out3[,1],sums[4,], col = cols[4], lwd = weight)

legendLabs = c(expression(alpha), "=1")
legend("topright", legend = c(expression(paste(alpha,"=1"), paste(alpha,"=0.8"), paste(alpha,"=0.6"), paste(alpha,"=8/15"))), col = cols, bty = "n", lwd = weight, cex = legendsize)

othersums = array(dim = c(4,length(out0[,1])))	# sum up all individuals in the the 'infected' classes over time
for(i in 1:length(out0[,1])){
	othersums[1,i] = sum(out0[i,2:19])
	othersums[2,i] = sum(out1[i,2:19])
	othersums[3,i] = sum(out2[i,2:19])
	othersums[4,i] = sum(out3[i,2:19])
	}

plot(out0[,1],1-othersums[1,], xlim = c(0, 255), ylim = c(0, 0.8), xlab = 'time (days)', ylab = 'cumulative infectives', col = cols[1], type = 'l', lwd = weight, cex.axis = axsize, cex.lab = labsize, cex.main = mainsize)
lines(out1[,1], 1-othersums[2,], col = cols[2], lwd = weight)
lines(out2[,1], 1-othersums[3,], col = cols[3], lwd = weight)
lines(out3[,1], 1-othersums[4,], col = cols[4], lwd = weight)

legend("topleft", legend = c(expression(paste(alpha,"=1"), paste(alpha,"=0.8"), paste(alpha,"=0.6"), paste(alpha,"=8/15"))), col = cols, bty = "n", lwd = weight, cex = legendsize)

dev.off()
### end
