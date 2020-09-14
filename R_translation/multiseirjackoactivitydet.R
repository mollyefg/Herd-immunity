

multiseirjackoactivitydet<-function(gamma, sigma, pactivity, activitypi, Rzero, epsilon, tend, k, dt){
	
	# another nested function:
	source('jaccomultiactivity.R')
	outJacko = jaccomultiactivity(pactivity, activitypi)

	# this is NOT ideal: pull the matrix and vector out from the jaccomultiactivity function and separate them
	lambda = matrix(outJacko[1:k^2], nrow = k, ncol = k, byrow=T)
	vecpi = c(outJacko[(k^2+1):length(outJacko)])
	
	# R0 function:
	source('R0.R')
	r = R0(vecpi, lambda/gamma)
	lambda = (Rzero/r)*lambda
	tspan = seq(0, tend, dt)
	y0 = c((1-epsilon)*vecpi, epsilon*vecpi, rep(0, k))
	
	### now solve the ODEs:
	p = list(lambda=lambda, gamma, sigma, k)
	
	source('multiseirdydt.R')
	out = ode(y=y0, times=tspan, func=multiseirdydt, parms=p, method='ode45')
	}
