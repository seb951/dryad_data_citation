

setwd("/Users/jerry/Documents/MANUSCRIPTS/2016/34.Renaut_PeresNeto_ME/test_dryad")

set.seed(123)
###
#First get all the data for years 2012,2013,2014,2015, by hand
#(trick: change the rpp to a lot!)
#http://datadryad.org/discover?query=2012&submit=Go&filtertype=*&filter=&rpp=2342&sort_by=score&order=DESC
#http://datadryad.org/discover?query=2013&submit=Go&filtertype=*&filter=&rpp=3172&sort_by=score&order=DESC
#http://datadryad.org/discover?query=2013&submit=Go&filtertype=*&filter=&rpp=3172&sort_by=score&order=DESC
#http://datadryad.org/discover?query=2013&submit=Go&filtertype=*&filter=&rpp=3172&sort_by=score&order=DESC


#get only the good lines:
system("grep 'span class=\"author\"' 2013_all.html | awk '{sub(/<span class=\"author\">/,\"\")}; 1' | awk '{sub(/<\\/span>/,\"\")}; 1' | awk '{sub(/&nbsp;/,\"\")}; 1' >author") 
system("grep 'pub-date' 2013_all.html | awk '{sub(/<span class=\"pub-date\">/,\"\")}; 1' | awk '{sub(/<\\/span>/,\"\")}; 1' | awk '{sub(/&nbsp;/,\"\")}; 1' >year") 
system("grep 'artifact-title' 2013_all.html | awk '{sub(/<span class=\"artifact-title\">Data from: /,\"\")}; 1' | awk '{sub(/<\\/span>/,\"\")}; 1' | awk '{sub(/&nbsp;/,\"\")}; 1' | awk '{gsub(/\\047/,\"\")}; 1' >title")    
system("grep 'italics' 2013_all.html | awk '{sub(/<span class=\"italics\">/,\"\")}; 1' | awk '{sub(/<\\/span>/,\"\")}; 1' | awk '{sub(/&nbsp;/,\"\")}; 1' >pub")  

#or search for the top ten journals..
#http://datadryad.org/discover?query=2012&submit=Go&filtertype=*&filter=&rpp=1000&sort_by=score&order=DESC

##
#author = read.table("author", stringsAsFactors = F,sep = "\t")
year = read.table("year", stringsAsFactors = F,sep = "\t")
title = read.table("title", stringsAsFactors = F,sep = "\t")
pub = read.table("pub", stringsAsFactors = F,sep = "\t")
pub2 = rep(0, nrow(pub))
doi = rep(0, nrow(pub))
citation = rep(0,nrow(pub))
for(i in 1:length(pub2))
{
pub2[i] = strsplit(pub[i,1],split = " <")[[1]][1]
doi[i] = strsplit(pub[i,1],split = ">")[[1]][2]
doi[i] = strsplit(doi[i],split = "<")[[1]][1]
}



#then use this python script:
for(i in 1:100)
{
Sys.sleep(rchisq(1, 1, ncp = 2))	
scholar_cmd = paste("scholar.py -c 1 -P '",pub2[i], "' --phrase '",title[i,1], "' >scholar 2>log",sep = "")
system(scholar_cmd)
system("wc -l scholar >scholar_wc");if(read.table("scholar_wc")[1] >0) {
scholar = read.table("scholar",stringsAsFactors = F, header = F,sep = "\t")
citation[i] = scholar[4,1]}
if(i %% 25 == 0) print(paste(i," of ",length(pub2)," The time is:",Sys.time(),sep = ""))
}


set.seed(123)
