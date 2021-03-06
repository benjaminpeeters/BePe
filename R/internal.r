




clean <- function(name)
{
	
		name = stringr::str_to_lower(name)
		name=gsub("\\.\\.\\.\\."," ", name)
		name=gsub("\\.\\.\\."," ", name)
		name=gsub("\\.\\."," ", name)
		name=gsub("\\."," ", name)
		name=gsub("\\'","", name)
		name=gsub('\\"',"", name)
		name=gsub("\\,","", name)
		name=gsub("\\ ","", name)
		name=gsub("\\:","", name)
		name=gsub("\\-","", name)
		name=gsub("\\*","", name)
		name=gsub("\\(","", name)
		name=gsub("\\)","", name)
		name=gsub("the","", name) 
		name=gsub("of","", name) 
		name=gsub("and","", name)
		name=gsub("&","", name)
		name=gsub("’","", name)
		
		return(name) 
	
}


cleanCountry <- function(name)
{
	
		name = clean(name)
		name=gsub("tfyr","", name)
		name=gsub("fyr","", name) # Former Yugoslav Republic
		name=gsub("sar","", name)
		name=gsub("ublic","", name) #republic -> rep
		name=gsub("ocratic","", name) #democratic -> dem
		name=gsub("arab","", name)
		name=gsub("islamic","", name)
		name=gsub("union","", name)
		name=gsub("federal","", name)
		name=gsub("ovince","", name) # province -> pr
		
		united = grepl("united",name)
		noUni = abs(united -1)*(1:length(name))
		name[noUni]=gsub("states","", name[noUni])
		name[noUni]=gsub("state","", name[noUni])
		name[noUni]=gsub("kingdom","", name[noUni])
		name[noUni]=gsub("kingdoms","", name[noUni])
		
		korea = grepl("korea",name)
		congo = grepl("congo",name)
		nokc = abs(korea + congo -1)*(1:length(name))
		name[nokc] = gsub("rep","", name[nokc])
		name[nokc] = gsub("dem","", name[nokc])
		
		return(name) 
	
}



inclStr = function(strings, ..., clean=TRUE)
{
	conditions = c(...)
	if(clean){
		conditions = clean(conditions)
		strings = clean(strings)
	}
	S = rep(NA,length(conditions))
	which = rep(NA,length(conditions))
	for(i in 1:length(conditions)){
		S[i] = Sum(strings==conditions[i])
		if(S[i]==1){which[i] = which(strings==conditions[i])}
	}
	output = list(boolean=(S>0), which=which)
	return(output)
}

boolInclStr = function(strings, ..., clean=TRUE)
{
	return(Sum(inclStr(strings, ..., clean=TRUE)$boolean))
}



