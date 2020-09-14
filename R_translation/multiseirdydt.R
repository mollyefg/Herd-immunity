multiseirdydt<-function(t,y,p){

	# specify the initial conditions, for each class/type, k

	S = y[1:k]
	E = y[(k+1):(2*k)]
	I = y[(2*k+1):(3*k)]

	dX = c()	# vector to store the outputs

	with(as.list(p),{

	for(i in 1:k){

		x = 0
		for(j in 1:k){
			x = x + lambda[j,i]*I[j]
			}

		# equation for dS.dt:
		dX[i] = -x*S[i]; 	

		#equation for dE.dt: 	
		dX[i+k] = x*S[i] - sigma*E[i]

		#equation for dI.dt:
		dX[i + 2*k] = sigma*E[i] - gamma*I[i]

		} # closes the loop over classes	
	return(list(dX)) 	
		}) # stops the parameter list		
	}	# closes the function

###

