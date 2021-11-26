# devtools::document()





#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
name2iso <- function(name){
	if(!is.character(name)){name=as.character(name)}
	
	for(i in 1:length(name)){
		
		name[i]=gsub("\\.\\.\\.\\."," ", name[i])
		name[i]=gsub("\\.\\.\\."," ", name[i])
		name[i]=gsub("\\.\\."," ", name[i])
		name[i]=gsub("\\."," ", name[i])
		
		if(name[i]=='Hong Kong'){name[i] = countryCode[47,1]
		}else if(name[i]=='United States' | name[i]=='USA' | name[i]=='US'){name[i] = countryCode[235,1]
		}else if(name[i]=='Cent Afr Rep'){name[i] = countryCode[43,1]
		}else if(name[i]=='Congo'){name[i] = countryCode[53,1] # /!\
		}else if(name[i]=='Dem Rp Congo'){name[i] = countryCode[54,1]
		}else if(name[i]=='Congo Democratic Republic '){name[i] = countryCode[54,1]
		}else if(name[i]=='Cote Divoire'){name[i] = countryCode[57,1]
		}else if(name[i]=='Ivory Coast'){name[i] = countryCode[57,1]
		}else if(name[i]=='Dominican Rp'){name[i] = countryCode[65,1]
		}else if(name[i]=='Neth Ant Aru'){name[i] = countryCode[157,1]
		}else if(name[i]=='GuineaBissau'){name[i] = countryCode[95,1]
		}else if(name[i]=='Guinea Bissau'){name[i] = countryCode[95,1]
		}else if(name[i]=='St Helena'){name[i] = countryCode[186,1]
		}else if(name[i]=='Tanzania'){name[i] = countryCode[219,1]
		}else if(name[i]=='St Kt-Nev-An'){name[i] = countryCode[187,1]
		}else if(name[i]=='Saint Kitts Nevis'){name[i] = countryCode[187,1]
		}else if(name[i]=='Trinidad Tbg'){name[i] = countryCode[225,1]
		}else if(name[i]=='Trinidad Tobago'){name[i] = countryCode[225,1]
		}else if(name[i]=='Falkland Is'){name[i] = countryCode[73,1]
		}else if(name[i]=='Suriname'){name[i] = countryCode[211,1]
		}else if(name[i]=='Iran'){name[i] = countryCode[105,1]
		}else if(name[i]=='Syria'){name[i] = countryCode[216,1]
		}else if(name[i]=='Untd Arab Em'){name[i] = countryCode[233,1]
		}else if(name[i]=='China HK SAR'){name[i] = countryCode[47,1]
		}else if(name[i]=='China,P R :Hong Kong'){name[i] = countryCode[47,1]
		}else if(name[i]=='Korea Rep '){name[i] = countryCode[119,1]
		}else if(name[i]=='South Korea'){name[i] = countryCode[119,1]
		}else if(name[i]=='Lao P Dem R'){name[i] = countryCode[122,1]
		}else if(name[i]=='Laos'){name[i] = countryCode[122,1]
		}else if(name[i]=='China MC SAR'){name[i] = countryCode[48,1]
		}else if(name[i]=='Macao'){name[i] = countryCode[48,1]
		}else if(name[i]=='Taiwan'){name[i] = countryCode[217,1]
		}else if(name[i]=='Korea D P Rp'){name[i] = countryCode[118,1]
		}else if(name[i]=='Belgium-Lux'){name[i] = countryCode[22,1]
		}else if(name[i]=='France,Monac'){name[i] = countryCode[77,1]
		}else if(name[i]=='UK'){name[i] = countryCode[234,1]
		}else if(name[i]=='Switz Liecht'){name[i] = countryCode[215,1]
		}else if(name[i]=='Czech Rep'){name[i] = countryCode[61,1]
		}else if(name[i]=='Russian Fed'){name[i] = countryCode[183,1]
		}else if(name[i]=='Russia'){name[i] = countryCode[183,1]
		}else if(name[i]=='Bosnia Herzg'){name[i] = countryCode[28,1]
		}else if(name[i]=='Bosnia Herzegovina'){name[i] = countryCode[28,1]
		}else if(name[i]=='New Calednia'){name[i] = countryCode[158,1]
		}else if(name[i]=='Papua N Guin'){name[i] = countryCode[172,1]
		}else if(name[i]=='Eq Guinea'){name[i] = countryCode[69,1]
		}else if(name[i]=='Venezuela'){name[i] = countryCode[240,1]
		}else if(name[i]=='Vietnam'){name[i] = countryCode[241,1]
		}else if(name[i]=='St Pierre Mq'){name[i] = countryCode[190,1]
		}else if(name[i]=='Sao Tome Principe'){name[i] = countryCode[194,1]
		}else if(name[i]=='Fm Yemen Dm'){name[i] = countryCode[245,1]
		}else if(name[i]=='Fr Guiana'){name[i] = countryCode[78,1]
		}else if(name[i]=='Rep Moldova'){name[i] = countryCode[145,1]
		}else if(name[i]=='Antigua Barbuda'){name[i] = countryCode[10,1]
		}else if(name[i]=='Brunei'){name[i] = countryCode[34,1]
		}else if(name[i]=='Burma Myanmar '){name[i] = countryCode[152,1]
		}else if(name[i]=='CIS Mongolia'){name[i] = countryCode[147,1]
		}else if(name[i]=='Macedonia'){name[i] = countryCode[131,1]
		}else if(name[i]=='Sint Maarten'){name[i] = countryCode[189,1]
		}else if(name[i]=='Saint Vincent The Grenadines'){name[i] = countryCode[191,1]
		}else if(name[i]=='Timor Leste'){name[i] = countryCode[121,1]
		}else if(name[i]=='Korea'){name[i] = countryCode[119,1]}
		
		name[i] = str_to_lower(name[i])
		
		name[i] = countryCode[name[i]==str_to_lower(countryCode[,1]),2]
	}
	return(name)
} 

#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
iso2name <- function(iso){
	if(!is.character(iso)){iso=as.character(iso)}
	
	if(nchar(iso[1])==2){
		for(i in 1:length(iso)){
			iso[i] = countryCode[iso[i]==countryCode[,3],1]
		}
	}else if(nchar(iso[1])==3){
		for(i in 1:length(iso)){
			iso[i] = countryCode[iso[i]==countryCode[,2],1]
		}
	}
	return(iso)
} 

#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
countryNameShift <- function(db){
	mismatching = NULL
	countrySize = dim(db)[1]
	for(i in 1:countrySize){
		if(sum(rownames(db)[i]==countryCode[,1])==1){
			rownames(db)[i] = countryCode[rownames(db)[i]==countryCode[,1],2]
		} else {
			mismatching = c(mismatching, i)
		}
	}
	
	return(list(db,mismatching))
} 



#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
Sum <- function(X){
	return(sum(X, na.rm=T))
	}

