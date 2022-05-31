# devtools::document()





#' Convert a country name (resp. a vector of country names) to a ISO3 code (resp. a vector of ISO3 codes).
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile A (vector of) character(s) containing country names.
#' @return A (vector of) character(s) containing the ISO3 codes.
#' @family iso3
#' @export
name2iso <- function(name){
	if(!is.character(name)){name=as.character(name)}
	
	for(i in 1:length(name)){
		
		name[i] = cleanCountry(name[i])
		
		# china
		if(name[i]=='hongkong'){name[i] = countryCode[47,1] 
		}else if(name[i]=='chinahk'){name[i] = countryCode[47,1]
		}else if(name[i]=='chinahongkong'){name[i] = countryCode[47,1]
		}else if(name[i]=='chinaprhongkong'){name[i] = countryCode[47,1]
		}else if(name[i]=='hongkongchina'){name[i] = countryCode[47,1]
		}else if(name[i]=='chinamc'){name[i] = countryCode[48,1]
		}else if(name[i]=='macao'){name[i] = countryCode[48,1]
		}else if(name[i]=='chinamacao'){name[i] = countryCode[48,1]
		}else if(name[i]=='chinaprmacao'){name[i] = countryCode[48,1]
		}else if(name[i]=='taiwan'){name[i] = countryCode[217,1]
		}else if(name[i]=='chinataiwanpro'){name[i] = countryCode[217,1]
		}else if(name[i]=='taiwanprchina'){name[i] = countryCode[217,1]
		}else if(name[i]=='chinaprmainl'){name[i] = countryCode[46,1]
		# north america
		}else if(name[i]=='unitedstates' | name[i]=='usa' | name[i]=='us'){name[i] = countryCode[235,1]
		# europe
		}else if(name[i]=='belgiumlux'){name[i] = countryCode[22,1]
		}else if(name[i]=='belgiumluxembourg'){name[i] = countryCode[22,1]
		}else if(name[i]=='francemonac'){name[i] = countryCode[77,1]
		}else if(name[i]=='orraprincipality'){name[i] = countryCode[6,1]
		}else if(name[i]=='uk'){name[i] = countryCode[234,1]
		}else if(name[i]=='switzliecht'){name[i] = countryCode[215,1]
		}else if(name[i]=='switzerlandliechtenstein'){name[i] = countryCode[215,1]
		}else if(name[i]=='switzerlliechtenstein'){name[i] = countryCode[215,1]
		}else if(name[i]=='czechrep'){name[i] = countryCode[61,1]
		}else if(name[i]=='czechia'){name[i] = countryCode[61,1]
		}else if(name[i]=='czechoslovakia/czechia'){name[i] = countryCode[61,1]; print("czechoslovakia -> czechia")
		}else if(name[i]=='czechoslovakia'){name[i] = countryCode[61,1]; print("czechoslovakia -> czechia")
		}else if(name[i]=='bosniaherzg'){name[i] = countryCode[28,1]
		}else if(name[i]=='bosniaherzegovina'){name[i] = countryCode[28,1]
		}else if(name[i]=='repmoldova'){name[i] = countryCode[145,1]
		}else if(name[i]=='republicmoldova'){name[i] = countryCode[145,1]
		}else if(name[i]=='macedonia'){name[i] = countryCode[131,1] 
		}else if(name[i]=='northmacedonia'){name[i] = countryCode[131,1]
		}else if(name[i]=='holysee'){name[i] = countryCode[99,1]
		}else if(name[i]=='serbiamontenegro'){name[i] = countryCode[197,1]; print("serbia and montenegro -> serbia")
		}else if(name[i]=='serbia/yugoslavia'){name[i] = countryCode[197,1]; print("Yugoslavia -> serbia")
		}else if(name[i]=='yugoslavia'){name[i] = countryCode[197,1]; print("Yugoslavia -> serbia")
		}else if(name[i]=='slovak'){name[i] = countryCode[201,1]
		# russia
		}else if(name[i]=='russianfed'){name[i] = countryCode[183,1]
		}else if(name[i]=='russia'){name[i] = countryCode[183,1]
		}else if(name[i]=='russia/ussr'){name[i] = countryCode[183,1]
		}else if(name[i]=='ussr/russia'){name[i] = countryCode[183,1]
		}else if(name[i]=='southgeorgiaandsouthsandwichislands'){name[i] = countryCode[206,1]
		# africa
		}else if(name[i]=='congo'){name[i] = countryCode[53,1] # /!\
		}else if(name[i]=='congorep'){name[i] = countryCode[53,1]
		}else if(name[i]=='repcongobrazzaville'){name[i] = countryCode[53,1]
			#rdc
		}else if(name[i]=='demrepcongokinshasa'){name[i] = countryCode[54,1]
		}else if(name[i]=='congodemrep'){name[i] = countryCode[54,1]
		}else if(name[i]=='demrpcongo'){name[i] = countryCode[54,1]
		}else if(name[i]=='drcongo'){name[i] = countryCode[54,1]
		}else if(name[i]=='congodr'){name[i] = countryCode[54,1]
		}else if(name[i]=='congodemocraticrepublic'){name[i] = countryCode[54,1]
		}else if(name[i]=='congodemr'){name[i] = countryCode[54,1]
		}else if(name[i]=='demrepcongo'){name[i] = countryCode[54,1]
		}else if(name[i]=='centafrrep'){name[i] = countryCode[43,1]
		}else if(name[i]=='car'){name[i] = countryCode[43,1]
		}else if(name[i]=='cotedivoire'){name[i] = countryCode[57,1]
		}else if(name[i]=='côtedivoire'){name[i] = countryCode[57,1]
		}else if(name[i]=='côted’ivoire'){name[i] = countryCode[57,1]
		}else if(name[i]=='ivorycoast'){name[i] = countryCode[57,1]
		}else if(name[i]=='dominicanrp'){name[i] = countryCode[65,1]
		}else if(name[i]=='dominicanr'){name[i] = countryCode[65,1]
		}else if(name[i]=='guineabissau'){name[i] = countryCode[95,1]
		}else if(name[i]=='tanzania'){name[i] = countryCode[219,1]
		}else if(name[i]=='unitedrepublictanzania'){name[i] = countryCode[219,1]
		}else if(name[i]=='unitedtanzania'){name[i] = countryCode[219,1]
		}else if(name[i]=='eqguinea'){name[i] = countryCode[69,1]
		}else if(name[i]=='eswatini'){name[i] = countryCode[213,1]
		}else if(name[i]=='eswatiniswazil'){name[i] = countryCode[213,1]
		}else if(name[i]=='lybia'){name[i] = countryCode[127,1]
		}else if(name[i]=='sudanformer'){name[i] = countryCode[210,1]
		# saint and little islands
		}else if(name[i]=='slena'){name[i] = countryCode[186,1]
		}else if(name[i]=='stktnevan'){name[i] = countryCode[187,1]
		}else if(name[i]=='saintkittsnevis'){name[i] = countryCode[187,1]
		}else if(name[i]=='stkittsnevis'){name[i] = countryCode[187,1]
		}else if(name[i]=='stpierremq'){name[i] = countryCode[190,1]
		}else if(name[i]=='sintmaarten'){name[i] = countryCode[189,1]
		}else if(name[i]=='sintmaartendutchpart'){name[i] = countryCode[189,1]
		}else if(name[i]=='saintvincentgrenadines'){name[i] = countryCode[191,1]
		}else if(name[i]=='stvincentgrenadines'){name[i] = countryCode[191,1]
		}else if(name[i]=='timorleste'){name[i] = countryCode[221,1]
		}else if(name[i]=='saotomeprincipe'){name[i] = countryCode[194,1]
		}else if(name[i]=='sãotomépríncipe'){name[i] = countryCode[194,1]
		}else if(name[i]=='newcalednia'){name[i] = countryCode[158,1]
		}else if(name[i]=='antiguabarbuda'){name[i] = countryCode[10,1]
		}else if(name[i]=='nethantaru'){name[i] = countryCode[157,1]
		}else if(name[i]=='caboverde'){name[i] = countryCode[41,1]
		}else if(name[i]=="stlucia"){name[i] = countryCode[188,1]
		}else if(name[i]=="arubanerls"){name[i] = countryCode[13,1]
		}else if(name[i]=="virginislsbritish"){name[i] = countryCode[32,1]
		}else if(name[i]=="frenchterritoriesnewcaledonia"){name[i] = countryCode[158,1]
		}else if(name[i]=="frenchterritoriesfrenchpolynesia"){name[i] = countryCode[79,1]
		}else if(name[i]=="micronesiafed"){name[i] = countryCode[144,1]
		# latin america
		}else if(name[i]=='trinidadtbg'){name[i] = countryCode[225,1]
		}else if(name[i]=='trinidadtobago'){name[i] = countryCode[225,1]
		}else if(name[i]=='falklandis'){name[i] = countryCode[73,1]
		}else if(name[i]=='suriname'){name[i] = countryCode[211,1]
		}else if(name[i]=='venezuela'){name[i] = countryCode[240,1]
		}else if(name[i]=='venezuelabolivarian'){name[i] = countryCode[240,1]
		}else if(name[i]=='venezuelabolivarianade'){name[i] = countryCode[240,1]
		}else if(name[i]=='venezuelaúblicabolivarianade'){name[i] = countryCode[240,1]
		}else if(name[i]=='venezuelaabolivarianade'){name[i] = countryCode[240,1]
		}else if(name[i]=='frguiana'){name[i] = countryCode[78,1]
		}else if(name[i]=='boliviaplurinational'){name[i] = countryCode[27,1]
		# middle east
		}else if(name[i]=='iran'){name[i] = countryCode[105,1]
		}else if(name[i]=='iranislamicrepublic'){name[i] = countryCode[105,1]
		}else if(name[i]=='syria'){name[i] = countryCode[216,1]
		}else if(name[i]=='syrian'){name[i] = countryCode[216,1]
		}else if(name[i]=='syrianarabrepublic'){name[i] = countryCode[216,1]
		}else if(name[i]=='untdarabem'){name[i] = countryCode[233,1]
		}else if(name[i]=='unitedemirates'){name[i] = countryCode[233,1]
		}else if(name[i]=='fmyemendm'){name[i] = countryCode[245,1]
		}else if(name[i]=='yemenpeoples'){name[i] = countryCode[245,1]
		}else if(name[i]=='statepalestine'){name[i] = countryCode[170,1]
		}else if(name[i]=='palestine'){name[i] = countryCode[170,1]
		# south est asia
		}else if(name[i]=='laopdemr'){name[i] = countryCode[122,1]
		}else if(name[i]=='laos'){name[i] = countryCode[122,1]
		}else if(name[i]=='lao'){name[i] = countryCode[122,1]
		}else if(name[i]=='laopeoplesdemrep'){name[i] = countryCode[122,1]
		}else if(name[i]=='laopeoplesdr'){name[i] = countryCode[122,1]
		}else if(name[i]=='laopeoples'){name[i] = countryCode[122,1]
		}else if(name[i]=='papuanguin'){name[i] = countryCode[172,1]
		}else if(name[i]=='brunei'){name[i] = countryCode[34,1]
		}else if(name[i]=='vietnam'){name[i] = countryCode[241,1]
		}else if(name[i]=='burmamyanmar'){name[i] = countryCode[152,1]
		# others
		}else if(name[i]=='cismongolia'){name[i] = countryCode[147,1]
		}else if(name[i]=='kyrgyz'){name[i] = countryCode[121,1]
		# korea
		}else if(name[i]=='northkorea'){name[i] = countryCode[118,1]
		}else if(name[i]=='koreanorth'){name[i] = countryCode[118,1]
		}else if(name[i]=='koreadprp'){name[i] = countryCode[118,1]
		}else if(name[i]=='dprkorea'){name[i] = countryCode[118,1]
		}else if(name[i]=='koreadempeoplesrep'){name[i] = countryCode[118,1]
		}else if(name[i]=='southkorea'){name[i] = countryCode[119,1]
		}else if(name[i]=='koreasouth'){name[i] = countryCode[119,1]
		}else if(name[i]=='korearep'){name[i] = countryCode[119,1]
		}else if(name[i]=='republickorea'){name[i] = countryCode[119,1]
		}else if(name[i]=='repkorea'){name[i] = countryCode[119,1]
		}else if(name[i]=='korea'){name[i] = countryCode[119,1]}
		
		name[i] = cleanCountry(name[i])
		matching = cleanCountry(countryCode[,1])
		
		name[i] = countryCode[name[i]==matching,2]
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
#' @family iso3
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

#' Change the row names of a matrix/data.frame to ISO-3 country codes 
#'
#' This function changes the row names of a matrix/data.frame to ISO-3 country codes 
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @family iso3
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


#' Indicate which entries are not properly converted by the function 'name2iso'
#'
#'
#'
#'
#' @family iso3
#' @export
incorrect.name2iso <-function(db){
	
	errorIndex = NULL
	for(i in 1: length(db)){
		
		a = try(name2iso(db[i]), TRUE)
		if(nchar(a)!=3){errorIndex = c(errorIndex, i)}
		
	}
	return(errorIndex)
}
