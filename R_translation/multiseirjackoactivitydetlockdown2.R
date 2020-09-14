multiseirjackoactivitydetlockdown2 <- function(gamma, sigma, pactivity, activitypi, Rzero, tlockdown1, alphalockdown1, tlockdown2, alphalockdown2, epsilon, tend, k, dt){
#This is the equivalent code to multiseirjackoactivitydet.m but
#incorporating lockdown, details of which are given in multiseirdydtB.

	# another nested function:
	source('jaccomultiactivity.R')
	outJacko = jaccomultiactivity(pactivity, activitypi)

	lambda = matrix(outJacko[1:k^2], nrow = k, ncol = k, byrow=T)
	vecpi = c(outJacko[(k^2+1):length(outJacko)])

	source('R0.R')
	r=R0(vecpi, lambda/gamma);
	lambda=Rzero/r*lambda;
	tspan = seq(0, tend, dt)
	y0 = c((1-epsilon)*vecpi, epsilon*vecpi, rep(0, k))

	### now solve the ODEs:

	p = list(lambda=lambda, gamma, sigma, k, tlockdown1, alphalockdown1, tlockdown2, alphalockdown2)
	#
	source('multiseirdydtB.R')
	out1 = ode(y=y0, times=tspan, func=multiseirdydtB, parms=p, method='ode45')

	}






    




