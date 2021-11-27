# Encoding norm for countries: "ISO 3166-1 alpha-3"
# link: http://www.trucsweb.com/tutoriels/internet/iso_3166/

countryCode = read.csv('country-code-en.csv', header = TRUE)
countryCode = countryCode[,c(1,3,2)]
countryCode[,1] = as.character(countryCode[,1])
countryCode[,2] = as.character(countryCode[,2])
countryCode[,3] = as.character(countryCode[,3])
countryCode[153,3]="NA"

countryCode[nrow(countryCode) + 1,] = list("Euro area","EUR", "EU")

#devtools::use_data(countryCode)

save(countryCode, file='data/countryCode.rda')
