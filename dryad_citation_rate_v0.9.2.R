

setwd("../")
###Sébastien Renaut 2017
###This R script is part of the analysis presented in "Sébastien Renaut, Amber E. Budden, Dominique Gravel, Timothée Poisot, Pedro Peres-Neto.
###Data curation and sharing for ecologists and the role of libraries in the 21st century"
###
#First get all the data for years 2012,2013,2014,2015,2016 by hand
#(trick: change the rpp to a lot!)
#if there are more than 2000 entry, may not to download several webpages. You can cat then into a single file afterwards.

#http://datadryad.org/discover?query=2010&submit=Go&filtertype=*&filter=&rpp=896&sort_by=score&order=DESC
#http://datadryad.org/discover?query=2011&submit=Go&filtertype=*&filter=&rpp=1612&sort_by=score&order=DESC
#http://datadryad.org/discover?query=2012&submit=Go&filtertype=*&filter=&rpp=2404&sort_by=score&order=DESC
#http://datadryad.org/discover?query=2013&submit=Go&filtertype=*&filter=&rpp=3370&sort_by=score&order=DESC
#http://datadryad.org/discover?query=2014&submit=Go&filtertype=*&filter=&rpp=4055&sort_by=score&order=DESC
#http://datadryad.org/discover?query=2015&submit=Go&filtertype=*&filter=&rpp=5554&sort_by=score&order=DESC
#http://datadryad.org/discover?query=2016&submit=Go&filtertype=*&filter=&rpp=7587&sort_by=score&order=DESC


###ten journals with most hits in Dryad
journals =  c("Molecular Ecology","PLoS ONE","Evolution","Proceedings of the Royal Society B","Journal of Evolutionary Biology","The American Naturalist","Heredity","Molecular Ecology Resources","Biology Letters","Ecology and Evolution")

###Code exists in a directory called "Rcode"
###figures will be stores in a directory called "figures "
if(file.exists("figures") == F) print("Please make a directory called figures")
###dataretrieve from webofscience is stored in directory called "webofscience"
if(file.exists("webofscience") == F) print("Please make a directory called webofscience")
#the webofscience data is stored in TEN subdirectories within webofscience. each directory is labelled the name of one of the top ten journals that published data to dryad (gsub(" ","_",tolower(journals[j]))).
#Within web of science's Core collection, I searched for the name of the journal, the years 2010-2016 and then refining it only for document types "articles".
#Downloading citations "full record" in the "win, tab-delimited win UTF-8". You can only download 500 at a time due to internal limits by webofscience, which you can save as seperate files ("savedrecs.txt") within each subdirectory
#also remove the apostrophe's in files because R does not like them!!!!!
#Note that for PLOS One, I chose only papers with the topic ecology OR evolution OR genetics, sorted from A-Z by author and downloaded the first 2500 hits.
#Note that for PRSB, I chose only papers with the topic ecology OR evolution OR genetics

###dryad data (one file per year) is stored in the same directory as the Rcode.

###helper function from Piwowar and Vison 2013
calcCI.exp= function(res, param) {
  coefs = summary(res)$coeff
  coeff = coefs[param,]
  x = coeff[1]
  stderr = coeff[2]
  p = coeff[4]
  return(data.frame(param = param,
              est = round(exp(x), 2), 
              ciLow = round(exp(x - 1.96*stderr), 2),
              ciHigh = round(exp(x + 1.96*stderr), 2), 
              p = round(p, 3)))
}


###years covered by the data
full = c(2010,2011,2012,2013,2014,2015,2016)

###2016 is partial
full2 = full

###results stored in list
pub_year = list(1,1,1,1,1,1,1)
all_info_year = list(1,1,1,1,1,1,1)

for(f in 1:7)
{
	if(file.exists(paste("dryad_datasets_2010_2016_update_2017/",full[f],"_all.html",sep = "")) == F) print(paste("Please get the dryad info for year",full[f]))
	###get info of interest from large file using Unix grep.
  #i'll be easier to grep on authors for 3 lines extra...
	system(paste("grep 'span class=\"author\"' -A 3 dryad_datasets_2010_2016_update_2017/",full[f], "_all.html >dryad_datasets_2010_2016_update_2017/info_",full[f],sep = ""))
	
	###load data in R
	info = read.table(paste("dryad_datasets_2010_2016_update_2017/info_",full[f],sep = ""), stringsAsFactors = F,sep = "\t", quote = "")
	
	#then parse the info and clean it up
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
	title = gsub("PLOS","PLoS", title)
	
	title_year = title[as.numeric(date) == full[f]]
	
	pub_year[[f]] = title_year
	###stick all the info into large matrix which itself is in a list.
	all_info_year[[f]] = cbind(title, pub, date, author)[as.numeric(date) == full[f],]
	#all_info_year[[f]] = cbind(title, pub, date, author)
	}

####

###store results in list
journal_webofscience = list(1,1,1,1,1,1,1,1,1,1)

###impact factors for journals in 2016
impact_factor =  c(6.5,3.2,4.6,5.1,3.5,4.7,3.7,3.8,2.3,7.8)

###
for(j in 1:length(journals))
#for(j in c(1,3,5,6))
{
	#find out the names of the files as they appear in the directories containing the info from webofscience and named after the journals
	system(paste("ls -1 webofscience_update_2017/",gsub(" ","_",tolower(journals[j]))," > webofscience_update_2017/temp_list",sep =""))
	
	#load the names of all the files 
	temp_list = read.table("webofscience_update_2017/temp_list",stringsAsFactors = F,header = F)
	
	#result object that will contain the web of science info per journal
	temp_journal_webofscience = NULL
	for(t in 1:nrow(temp_list))
	{
		#get only the columns of interests from the web of science files
		system(paste("awk -F \"\\t\" 'BEGIN { OFS = \"\\t\" } {print $2,$9,$10, $33, $45}' webofscience_update_2017/",gsub(" ","_",tolower(journals[j])),"/",temp_list[t,1]," > webofscience_update_2017/temp",t,sep = ""))
	
		#load it
		temp_journal_webofscience_sub = read.table(paste("webofscience_update_2017/temp",t,sep = ""),header = T, sep = "\t",stringsAsFactors = F,quote = "")
		colnames(temp_journal_webofscience_sub)[5] = "YR"
		#Rbind it for all files
		temp_journal_webofscience = rbind(temp_journal_webofscience, temp_journal_webofscience_sub)
	}
  #result list that will contain the web of science info per journal
	journal_webofscience[[j]] = cbind(temp_journal_webofscience,0,0,temp_journal_webofscience[,2],log(temp_journal_webofscience[,4]+1),0)
	colnames(journal_webofscience[[j]])[6:10] = c("nb_authors","dataset","temp_name","log_citation","IF")	
	
	#add impact factor
	journal_webofscience[[j]][10] = impact_factor[j]
	
	#clean up the data to get rid as much as possible of inconsistencies between the dryad and web of science datasets
  #The problem here is that authors often do not enter EXACTLY the same title as for their dryad entry. 
	#There are many typos, differences with the ways accents are treated (England vs American spelling) and titles that get modified in final published version...
	#the gsub functions below fix MOST problems, although it is likely that a very small proportion of errors remain and could only be fixed by visual inspections.
	journal_webofscience[[j]][,8] = gsub(" ","", journal_webofscience[[j]][,8])
	journal_webofscience[[j]][,8] = gsub(".","", journal_webofscience[[j]][,8],fixed = T)
	journal_webofscience[[j]][,8] = toupper(journal_webofscience[[j]][,8])
	journal_webofscience[[j]][,8] = iconv(journal_webofscience[[j]][,8], to='ASCII//TRANSLIT')
	journal_webofscience[[j]][,8] = gsub("^","", journal_webofscience[[j]][,8],fixed = T)
	journal_webofscience[[j]][,8] = gsub("~","", journal_webofscience[[j]][,8],fixed = T)
	journal_webofscience[[j]][,8] = gsub("¨","", journal_webofscience[[j]][,8],fixed = T)
	journal_webofscience[[j]][,8] = gsub("`","", journal_webofscience[[j]][,8],fixed = T)
	journal_webofscience[[j]][,8] = gsub("'","", journal_webofscience[[j]][,8],fixed = T)
	journal_webofscience[[j]][,8] = gsub("-","", journal_webofscience[[j]][,8],fixed = T)
	journal_webofscience[[j]][,8] = gsub("(","", journal_webofscience[[j]][,8],fixed = T)
	journal_webofscience[[j]][,8] = gsub(")","", journal_webofscience[[j]][,8],fixed = T)
	journal_webofscience[[j]][,8] = gsub(",","", journal_webofscience[[j]][,8],fixed = T)
	journal_webofscience[[j]][,8] = gsub(":","", journal_webofscience[[j]][,8],fixed = T)
	journal_webofscience[[j]][,8] = gsub("?","", journal_webofscience[[j]][,8],fixed = T)
	journal_webofscience[[j]][,8] = gsub("!","", journal_webofscience[[j]][,8],fixed = T)
	journal_webofscience[[j]][,8] = gsub("OU","O", journal_webofscience[[j]][,8],fixed = T)
	journal_webofscience[[j]][,8] = gsub("Z","S", journal_webofscience[[j]][,8],fixed = T)
	journal_webofscience[[j]][,8] = gsub("Ñ","N", journal_webofscience[[j]][,8],fixed = T)
	
	###compare the webofscience with the dryad dataset	
	dryad = NULL
	for(f in 1:7)
	{
	  #this is do get the dryad info into a single matrix, instead of a list.
		dryad = rbind(dryad,all_info_year[[f]][all_info_year[[f]][,1] == journals[j],])
		if(f ==7) dryad = cbind(dryad, dryad[,2])
	}
	
	#clean up the data to get rid as much as possible of inconsistencies between the dryad and web of science datasets
	dryad[,5] = gsub(" ","", dryad[,5])
	dryad[,5] = gsub(".","", dryad[,5],fixed = T)
	dryad[,5] = toupper(dryad[,5])
	dryad[,5] = iconv(dryad[,5], to='ASCII//TRANSLIT')
	dryad[,5] = gsub("^","", dryad[,5],fixed = T)
	dryad[,5] = gsub("~","", dryad[,5],fixed = T)
	dryad[,5] = gsub("¨","", dryad[,5],fixed = T)
	dryad[,5] = gsub("'","", dryad[,5],fixed = T)
	dryad[,5] = gsub("-","", dryad[,5],fixed = T)
	dryad[,5] = gsub("(","", dryad[,5],fixed = T)
	dryad[,5] = gsub(")","", dryad[,5],fixed = T)
	dryad[,5] = gsub(",","", dryad[,5],fixed = T)
	dryad[,5] = gsub(":","", dryad[,5],fixed = T)
	dryad[,5] = gsub("?","", dryad[,5],fixed = T)
	dryad[,5] = gsub("!","", dryad[,5],fixed = T)
	dryad[,5] = gsub("OU","O", dryad[,5],fixed = T)
	dryad[,5] = gsub("&NBSP;</SPAN>","",dryad[,5],fixed = T)
	dryad[,5] = gsub("\"","", dryad[,5],fixed = T)
	dryad[,5] = gsub("–","", dryad[,5],fixed = T)
	dryad[,5] = gsub("Z","S", dryad[,5],fixed = T)
	dryad[,5] = gsub("Ñ","N", dryad[,5],fixed = T)
	

	###now ask for each webofscience entry, if there is an accompanying dataset on Dryad. Remember, we are still doing this PER JOURNAL
	for(i in 1: nrow(journal_webofscience[[j]]))
	{
		temp = dryad[,5] %in% journal_webofscience[[j]][i,8]
		#add a 1 on column 7 if there is a dataset
		if(length(temp[temp==T]) == 1) {journal_webofscience[[j]][i,7] = 1}
		#find out the nb of authors for each pub (this could have been done earlier in the code...)
		journal_webofscience[[j]][i,6] = length(gregexpr(";",journal_webofscience[[j]][i,1],fixed = T)[[1]])+1
	}

##now this must be corrected for the year factor.
	#par(mfrow= c(3,2))
	for(f in 1:6)
	{
	#	temp_yr = journal_webofscience[[j]][journal_webofscience[[j]][,5] == full[f],]
	#	t.test(temp_yr[temp_yr[,7]==0,4], temp_yr[temp_yr[,7]==1,4])
	#	random_density = density(temp_yr[temp_yr[,7]==0,4],na.rm = T)
	#	dryad_density = density(temp_yr[temp_yr[,7]==1,4],na.rm = T)
	#	plot(random_density, ylim = c(0,max(dryad_density $y)*1.1),main = full[f]); polygon(random_density, col="red")
	#	polygon(dryad_density, col="#00999999")
	#	legend(x = 4,y = 0.4,legend = c(journals[j],"random","dryad"), fill = c("white","red","#00999999"),cex = 0.7)
	}
	
###print stats just to see the effect of the dryad dataset for each journals
myfit = lm(log_citation ~ dataset + nb_authors + YR,data = journal_webofscience[[j]])
print(journal_webofscience[[j]][1,c(3,10)])
print(calcCI.exp(myfit, "dataset"))	
print(sum(journal_webofscience[[j]][,4])) #total nb of citation per journal	
}

###run a multiple linear regression that includes all the factors (journal is not included, but instead impact factor is).
journal_webofscience_all = rbind(journal_webofscience[[1]],journal_webofscience[[2]], journal_webofscience[[3]],journal_webofscience[[4]], journal_webofscience[[5]],
journal_webofscience[[6]],journal_webofscience[[7]],journal_webofscience[[8]],journal_webofscience[[9]],journal_webofscience[[10]])

myfit_all = lm(log_citation ~ dataset + IF + nb_authors + YR,data = journal_webofscience_all)
print(anova(myfit_all))

###what is the effect size of "dataset" (in %)
print(calcCI.exp(myfit_all, "dataset"))	

###remove temp files
system("rm webofscience_update_2017/temp*")


###the rest of the code is to plot the four figures...
###
###FIGURE 2
###
par(mar = c(7,5,4,2))
number_pub = c(length(pub_year[[1]]),length(pub_year[[2]]),length(pub_year[[3]]),length(pub_year[[4]]),length(pub_year[[5]]),length(pub_year[[6]]),length(pub_year[[7]]))

barplot(number_pub, names.arg =full, ylab = "Number of Dryad datasets",font.lab = 2,cex.lab =2 )# main = "Total number of papers with dryad dataset")
#points(8,4500, pch = 8, lwd = 2,xpd = T)
axis(1,at = 4.4, labels = "Year of publication", line = 2, lwd.ticks = 0,lty = 0,font=2,cex.axis = 2, lwd = 2)

dev.print(device=pdf, "figures/Figure2updated.pdf", onefile=FALSE)
dev.off()

###
###FIGURE 3
###
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

par(mar = c(10,4,4,2))
coco = c("darkblue","yellow3","chartreuse4","darkorange2","cyan4","darkmagenta","darkgoldenrod4","black","cadetblue4","darkred")
coco_palette = rep("a",70)
for(i in 1:10)
{
	coco_palette[((i*7)-6): (i*7)] = colorRampPalette(c("white", coco[i]))(7)	
}

barplot(t(pub_year_unique2[1:10,1:7]), beside = T,names.arg = rep("",10),col = coco_palette,yaxt = "n")
axis(side = 2, at = c(0,100,200,300,400,500,600),labels =c(0,100,200,300,400,500,600),lwd  = 4,col = "black")#left
text(x = seq(5,80,by = 8), y = -10, rownames(pub_year_unique2)[1:10],srt = 45, cex= 0.8, xpd = T, col = "black", adj = 1)
mtext("Number of Dryad datasets",font = 2, side = 2, line = 2.4, cex = 2)
mtext("Journals",font = 2, side =1,line = 7,cex = 2)

for(i in 1:10)
{
if(i<10) legend(y = 500,x=(45 + (i/2) -0.5),legend = rep("",7),fill = coco_palette[((i*7)-6):(i*7)], border = "white", bty = "n",cex = 1)
if(i == 10) legend(y = 500,x=(45 + (i/2) -0.5),legend = full2,fill = coco_palette[((i*7)-6):(i*7)], border = "white", bty = "n",cex = 1)
if(i == 10) legend(y = 500,x=(45 + (i/2)),legend = rep("",7),fill = "white", border = "white", bty = "n")
}
for(i in 1:7)
{
rect(47.1, 290+ (29*(i-1)),52.3, 306+(29*(i-1)), border = "black")
if(i ==7) rect(45,278,63,500, border = "black",lwd = 2)
}

dev.print(device=pdf, "figures/Figure3updated.pdf", onefile=FALSE)
dev.off()


###
###FIGURE 4
###
par(mar = c(7,5,2,2))
dryad_rise = matrix(,nrow = 7,ncol = 10)
colnames(dryad_rise) = journals
coco = c("darkblue","yellow3","chartreuse4","darkorange2","cyan4","darkmagenta","darkgoldenrod4","black","cadetblue4","darkred")
for(j in 1:10)
{
  t2010 = journal_webofscience[[j]][journal_webofscience[[j]][,5] == 2010,7]
  t2011 = journal_webofscience[[j]][journal_webofscience[[j]][,5] == 2011,7]
  t2012 = journal_webofscience[[j]][journal_webofscience[[j]][,5] == 2012,7]
  t2013 = journal_webofscience[[j]][journal_webofscience[[j]][,5] == 2013,7]
  t2014 = journal_webofscience[[j]][journal_webofscience[[j]][,5] == 2014,7]
  t2015 = journal_webofscience[[j]][journal_webofscience[[j]][,5] == 2015,7]
  t2016 = journal_webofscience[[j]][journal_webofscience[[j]][,5] == 2016,7]
  dryad_rise[,j] = c(sum(t2010)/length(t2010),sum(t2011)/length(t2011),sum(t2012)/length(t2012),sum(t2013)/length(t2013),sum(t2014)/length(t2014),sum(t2015)/length(t2015),sum(t2016)/length(t2016))
  dryad_rise[,j] = signif(dryad_rise[,j],3)
  
  if(j == 1) plot(x = full[1:7], y = dryad_rise[1:7,1],type = "b",col = coco[j], lwd = 4,ylim = c(0,1),cex.lab = 1.7,ylab = "Proportion of papers with Dryad dataset",xlab = "Year of publication", font.lab = 2,font.axis =2,pch = rep(c(1:2),5)[j])
  points(x = full[1:7], y = dryad_rise[1:7,j],type = "b",col = coco[j], lwd = 4,pch = c(1:10)[j])
  if(j == 10) legend(x = 2010, y = 1, legend = journals, cex = 0.75,pch = c(1:10), col = coco, lwd = 2.4)  
}

dev.print(device=pdf, "figures/Figure4updated.pdf", onefile=FALSE)
dev.off()



###
###FIGURE 5
###
pdf(file = "figures/Figure5.pdf", width = 6, height = 9) #
par(mfrow= c(7,1), mar = c(2,5,2,2))
	for(f in 1:6)
	{
		temp_yr = journal_webofscience_all[journal_webofscience_all[,5] == full[f],]
		random_density = density(temp_yr[temp_yr[,7]==0,9],na.rm = T)
		dryad_density = density(temp_yr[temp_yr[,7]==1,9],na.rm = T)
		if(f==6) ylim = c(0,max(random_density$y)*1.1) else ylim = c(0,max(dryad_density$y)*1.1)
		plot(random_density, ylim = ylim,main = full[f],xlab = "",xaxt = "n", xlim = c(0,8),ylab = "",yaxt = "n")
		if(f==6) at = c(0,0.5,1,1.5) else at = c(0,0.2,0.4,0.6,0.8)
		axis(side = 2, at = at, labels = at,las = 1)
		#if(f ==1) text(x = 10, y = -4.2,labels = "top 10 contributing journals to dryad analysed = ", xpd = NA,font = 2,cex = 0.6)
		#if(f ==1) text(x = 10, y = -4.27,labels = paste(journals[1:5],collapse = "/"), xpd = NA,font = 2,cex = 0.6)
		#if(f ==1) text(x = 10, y = -4.34,labels = paste(journals[6:10],collapse = "/"), xpd = NA,font = 2,cex = 0.6)
		#if(f ==1) text(x = 10, y = 1,labels = "dryad dataset increases citation rate by 12%", xpd = NA,font = 2,cex = 1.2)
		if(f ==6) {axis(side = 1, at = c(0,2,4,6,8) , labels = c(0,2,4,6,8), line = 1, lwd = 4);mtext(side = 1, text = "log(number of citations)", line = 4,cex = 1.3,font = 2)}
		if(f ==3) mtext(side = 2, text = "Density", line = 3,cex = 1,adj = -1.5, font = 2)
		text(x = 6 , y = ifelse(f ==6,1.3,0.5),labels =paste("n (no dataset) = ", length(temp_yr[temp_yr[,7]==0,9])), col = "red", font = 3)
		text(x =6,y =  ifelse(f ==6,1,0.4), labels = paste("n (dryad dataset) = ", length(temp_yr[temp_yr[,7]==1,9])),col = "#00999999", font = 2)
		polygon(random_density, col="red")
		polygon(dryad_density, col="#00999999")
		}
dev.off()



for(i in 1:10)
{
if(i<10) legend(y = 500,x=(45 + (i/2) -0.5),legend = rep("",7),fill = coco_palette[((i*7)-6):(i*7)], border = "white", bty = "n",cex = 1)
if(i == 10) legend(y = 500,x=(45 + (i/2) -0.5),legend = full2,fill = coco_palette[((i*7)-6):(i*7)], border = "white", bty = "n",cex = 1)
if(i == 10) legend(y = 500,x=(45 + (i/2)),legend = rep("",7),fill = "white", border = "white", bty = "n")
}






