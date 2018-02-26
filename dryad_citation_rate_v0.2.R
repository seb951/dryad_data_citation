

setwd("/Users/jerry/Documents/MANUSCRIPTS/2016/34.Renaut_PeresNeto_ME/test_dryad")

set.seed(123)
###
#First get all the data for years 2012,2013,2014,2015, by hand
#(trick: change the rpp to a lot!)
#http://datadryad.org/discover?query=2012&submit=Go&filtertype=*&filter=&rpp=2342&sort_by=score&order=DESC
#http://datadryad.org/discover?query=2013&submit=Go&filtertype=*&filter=&rpp=3172&sort_by=score&order=DESC
#http://datadryad.org/discover?query=2013&submit=Go&filtertype=*&filter=&rpp=3172&sort_by=score&order=DESC
#http://datadryad.org/discover?query=2013&submit=Go&filtertype=*&filter=&rpp=3172&sort_by=score&order=DESC

full = c(2010,2011,2012,2013,2014,2015,2016)
full2 = full;full2[7]="2016 (partial)"

pub_year = list(1,1,1,1,1,1,1)

for(f in 1:7)
{
	
	####i'll be easier to grep on authors for 3 lines extra...

	system(paste("grep 'span class=\"author\"' -A 3 ",full[f], "_all.html >info_",full[f],sep = ""))
	
	info = read.table(paste("info_",full[f],sep = ""), stringsAsFactors = F,sep = "\t", quote = "")
	#then parse it. 
	#then parse to remove the wrong year and go from there for fig1....
	author = info[seq(1,nrow(info),by = 5),1]
	date = info[seq(2,nrow(info),by = 5),1]
	pub = info[seq(3,nrow(info),by = 5),1]
	title = info[seq(4,nrow(info),by = 5),1]
	
	author = gsub("<span class=\"author\">","",author, fixed = T)
	author = gsub("</span>","",author, fixed = T)
	
	pub = gsub("<span class=\"artifact-title\">Data from: ","", pub, fixed = T)
	pub = gsub(" </span>","", pub, fixed = T)
	
	date = gsub("<span class=\"pub-date\"> (","", date, fixed = T)
	date = gsub(") </span>","", date, fixed = T)
	
	title = gsub("<span class=\"italics\">","", title, fixed = T)
	for(t in 1:length(title)){title[t] = substring(title[t],1,regexpr("<",title[t])-1)}
	
	title_year = title[as.numeric(date) == full[f]]
	title_year = gsub("PLOS","PLoS", title_year)
	pub_year[[f]] = title_year
	}

###get 30 publications for year 2012 for a specific journal

pub2 = pub[date == "2012"]
title2 = title[date == "2012"]
pub2[title2 == "PLoS One"]


###graphs
number_pub = c(length(pub_year[[1]]),length(pub_year[[2]]),length(pub_year[[3]]),length(pub_year[[4]]),length(pub_year[[5]]),length(pub_year[[6]]),length(pub_year[[7]]))
barplot(number_pub, names.arg =full, ylab = "Number of dryad datasets (2016 = up to May 10th)", main = "Total number of papers with dryad dataset")
dev.print(device=pdf, "Figure1.pdf", onefile=FALSE)
dev.off()

pub_year_unique = unique(sort(c(pub_year[[1]], pub_year[[2]], pub_year[[3]], pub_year[[4]], pub_year[[5]], pub_year[[6]], pub_year[[7]])))
pub_year_unique2 = cbind(rep(0,length(pub_year_unique)),0,0,0,0,0,0,0)
rownames(pub_year_unique2) = pub_year_unique
for(p in 1:nrow(pub_year_unique2))
{
	for(f in 1:7)
		{
	pub_year_unique2[p,f] = length(pub_year[[f]][pub_year[[f]] == pub_year_unique[p]])
	
}
pub_year_unique2[p,8] = sum(as.numeric(pub_year_unique2[p,1:7]))
}
pub_year_unique2 = pub_year_unique2[order(as.numeric(pub_year_unique2[,8]),decreasing = T),]

par(mar = c(12,4,4,2))
barplot(t(pub_year_unique2[1:10,1:7]), beside = T,names.arg = rep("",10),col = c("grey10","gray20","gray30","gray40","gray50","gray60","gray70"),main = "Rise in Dryad package for top 10 journals")
mtext(rownames(pub_year_unique2)[1:10],side = 1,las = 3,at = (c(1:10)*8)-2.5,cex = 0.8)
legend(y = 500,x=45,legend = full2,fill = c("grey10","gray20","gray30","gray40","gray50","gray60","gray70"))
dev.print(device=pdf, "Figure2.pdf", onefile=FALSE)
dev.off()

###rise in citation rate?


ran = read.table("../random_set.txt", header = T, sep = "\t",stringsAsFactors = F)
main = c("PLoSOne 2012 papers with dryad dataset vs without (n = 60)","Wilcoxon test pvalue = 0.006")
par(mfrow = c(3,2))
for(i in 1:length(unique(ran[,3])))
{
	ran_sub = ran[ran[,3]==unique(ran[,3])[i], ]
	ran_sub = ran_sub[ran_sub[,5]!=211,]
	#boxplot(ran_sub[,5]~ ran_sub[,6],main = unique(ran[,3])[i],ylab = "Number of citations",names = c("random","dryad"))
	#points(y = ran_sub[,5],x = ifelse(ran_sub[,6]=="1",2,1) )
	print(wilcox.test(ran_sub[,5]~ran_sub[,6]))
	
	random = density(ran_sub[ran_sub[,6]==0,5],na.rm = T)
	dryad = density(ran_sub[ran_sub[,6]==1,5],na.rm = T)
	plot(random, ylim = c(0,max(dryad$y)*1.1)); polygon(random, col="red")
	polygon(dryad, col="#00999999")

}

dev.print(device=pdf, "Figure3.pdf", onefile=FALSE)
dev.off()


		
	
#get only the good lines:
system(paste("grep 'span class=\"author\"' ", full[y],"_all.html | awk '{sub(/<span class=\"author\">/,\"\")}; 1' | awk '{sub(/<\\/span>/,\"\")}; 1' | awk '{sub(/&nbsp;/,\"\")}; 1' >author",sep = ""))
system(paste("grep 'pub-date' ", full[y],"_all.html | awk '{sub(/<span class=\"pub-date\">/,\"\")}; 1' | awk '{sub(/<\\/span>/,\"\")}; 1' | awk '{sub(/&nbsp;/,\"\")}; 1' >year",sep = ""))
system(paste("grep 'artifact-title' ", full[y],"_all.html | awk '{sub(/<span class=\"artifact-title\">Data from: /,\"\")}; 1' | awk '{sub(/<\\/span>/,\"\")}; 1' | awk '{sub(/&nbsp;/,\"\")}; 1' | awk '{gsub(/\\047/,\"\")}; 1' >title",sep = ""))
system(paste("grep 'class=\"doi\"' ", full[y],"_all.html | awk '{sub(/<span class=\"italics\">/,\"\")}; 1' | awk '{sub(/<\\/span>/,\"\")}; 1' | awk '{sub(/&nbsp;/,\"\")}; 1' >pub",sep = ""))

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
pub2 = gsub("PLOS","PLoS",pub2)
pub_year[[y]] = pub2

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
