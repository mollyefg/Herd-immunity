R0<-function(vecpi,lambda){
#Calculates R_0 for a multitype SIR epidemic having infection rate matrix lambda
#assuming all infectives have mean infectious period 1.  vecpi contains the
#fractions of the population of the different types.
	a=lambda%*%(diag(vecpi))
	eigenmat = eigen(print(a))
	#eigenmat = eigen(print(a))

	x=max(Mod(eigenmat$values))
	return(x)
	}
