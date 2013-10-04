

for(i in 1:100){
	if(i%%3 == 0) {
		cat("FIZZ")
	}
	if(i%%5 == 0) cat("BUZZ")
	if(i%%3 !=0 & i%%5 != 0) cat(i)
	cat("\n")
			
}