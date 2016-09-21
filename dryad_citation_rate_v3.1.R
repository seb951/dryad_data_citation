


###figures will be stores in a directory called "figures "
if(file.exists("../figures") == F) print("Please make a directory called ../figures")
###dataretrieve from webofscience is stored in directory called "webofscience"
if(file.exists("../webofscience") == F) print("Please make a directory called ../webofscience")
###dryad data (one file per year) is stored in a dirrectory called ".."

###helper from Piwowar and Vison
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


full = c(2010,2011,2012,2013,2014,2015,2016)
full2 = full;full2[7]="2016 (partial)"

pub_year = list(1,1,1,1,1,1,1)
all_info_year = list(1,1,1,1,1,1,1)
for(f in 1:7)
{
	if(file.exists(paste("../info_",full[f],sep = "")) == F) print(paste("Please get the dryad info for year",full[f]))
	####i'll be easier to grep on authors for 3 lines extra...
	system(paste("grep 'span class=\"author\"' -A 3 ../test_dryad/",full[f], "_all.html >../info_",full[f],sep = ""))
	
	info = read.table(paste("../info_",full[f],sep = ""), stringsAsFactors = F,sep = "\t", quote = "")
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
	
	title = gsub("PLOS","PLoS", title)
	
	title_year = title[as.numeric(date) == full[f]]
	
	pub_year[[f]] = title_year
	all_info_year[[f]] = cbind(title, pub, date, author)[as.numeric(date) == full[f],]
	#all_info_year[[f]] = cbind(title, pub, date, author)
	}

####
#this table is created by searching for all Molecular Ecology papers published between 2010-2016 on web of science's Core collection. Then refining it only for document types "articles". Downloading citations "full record" in the "tab-delimited win UTF-8". Then concatenating everything. 
#also remove the fuking apostrophe's.!!!!!
#note that for PLos One, I chose only papers with the topic ecology OR evolution OR genetics, sorted from A-Z by author and downloaded the first 2500 hits.
#note that for PRSB, I chose only papers with the topic ecology OR evolution OR genetics


journals =  c("Molecular Ecology","PLoS ONE","Evolution","Proceedings of the Royal Society B","Journal of Evolutionary Biology","The American Naturalist","Molecular Ecology Resources","Heredity","Ecology and Evolution","Systematic Biology")
journal_webofscience = list(1,1,1,1,1,1,1,1,1,1)
impact_factor =  c(6.5,3.2,4.6,5.1,3.5,4.7,3.7,3.8,2.3,7.8)

for(j in 1:length(journals))
#for(j in c(1,3,5,6))
{
	system(paste("ls -1 ../webofscience/",gsub(" ","_",tolower(journals[j]))," >../webofscience/temp_list",sep =""))
	temp_list = read.table("../webofscience/temp_list",stringsAsFactors = F,header = F)
	temp_journal_webofscience = NULL
	for(t in 1:nrow(temp_list))
	{
		system(paste("awk -F \"\\t\" 'BEGIN { OFS = \"\\t\" } {print $2,$9,$10, $33, $45}' ../webofscience/",gsub(" ","_",tolower(journals[j])),"/",temp_list[t,1]," >../webofscience/temp",t,sep = ""))
		
		temp_journal_webofscience_sub = read.table(paste("../webofscience/temp",t,sep = ""),header = T, sep = "\t",stringsAsFactors = F,quote = "")
		colnames(temp_journal_webofscience_sub)[5] = "YR"
		temp_journal_webofscience = rbind(temp_journal_webofscience, temp_journal_webofscience_sub)
	}	
	journal_webofscience[[j]] = cbind(temp_journal_webofscience,0,0,temp_journal_webofscience[,2],log(temp_journal_webofscience[,4]+1),0)
	colnames(journal_webofscience[[j]])[6:10] = c("nb_authors","dataset","temp_name","log_citation","IF")	
	journal_webofscience[[j]][10] = impact_factor[j]
	
	#clean up the data to get rid as much as possible of inconsistencies between the dryad and web of science datasets
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
	for(f in 1:6)
	{
		dryad = rbind(dryad,all_info_year[[f]][all_info_year[[f]][,1] == journals[j],])
		if(f ==6) dryad = cbind(dryad, dryad[,2])
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
	
	
	
	###compare
	for(i in 1: nrow(journal_webofscience[[j]]))
	{
		temp = dryad[,5] %in% journal_webofscience[[j]][i,8]
		
		if(length(temp[temp==T]) == 1) {journal_webofscience[[j]][i,7] = 1}
		#nb authors
		journal_webofscience[[j]][i,6] = length(gregexpr(";",journal_webofscience[[j]][i,1],fixed = T)[[1]])+1	
	#	if(i %% 500 ==0) print(paste(i," of ",nrow(journal_webofscience[[j]]), " The time is:",Sys.time(),sep = ""))
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
	
###stats
myfit = lm(log_citation ~ dataset + nb_authors + YR,data = journal_webofscience[[j]])
#print(anova(myfit))
print(journal_webofscience[[j]][1,c(3,10)])
print(calcCI.exp(myfit, "dataset"))		
}

###MEGASTATS!!!!!
journal_webofscience_all = rbind(journal_webofscience[[1]],journal_webofscience[[2]], journal_webofscience[[3]],journal_webofscience[[4]], journal_webofscience[[5]],
journal_webofscience[[6]],journal_webofscience[[7]],journal_webofscience[[8]],journal_webofscience[[9]],journal_webofscience[[10]])

myfit_all = lm(log_citation ~ dataset + IF + nb_authors + YR,data = journal_webofscience_all)
print(anova(myfit_all))
print(calcCI.exp(myfit_all, "dataset"))	


###
###FIGURE 4
###
pdf(file = "../figures/figure4.pdf", width = 6, height = 9) #
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
		if(f ==6) {axis(side = 1, at = c(0,2,4,6,8) , labels = c(0,2,4,6,8), line = 1, lwd = 4);mtext(side = 1, text = "log(number of citations)", line = 4,cex = 1.3)}
		if(f ==3) mtext(side = 2, text = "density", line = 3,cex = 1,adj = -1.5)
		text(x = 6 , y = ifelse(f ==6,1.3,0.5),labels =paste("n (no dataset) = ", length(temp_yr[temp_yr[,7]==0,9])), col = "red", font = 3)
		text(x =6,y =  ifelse(f ==6,1,0.4), labels = paste("n (dryad dataset) = ", length(temp_yr[temp_yr[,7]==1,9])),col = "#00999999", font = 2)
		polygon(random_density, col="red")
		polygon(dryad_density, col="#00999999")
		}
dev.off()


###
###FIGURE 3
###
dryad_rise = matrix(,nrow = 7,ncol = 10)
colnames(dryad_rise) = journals
coco = palette(rainbow(10))
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

if(j == 1) plot(x = full[1:6], y = dryad_rise[1:6,1],type = "b",col = coco[j], lwd = 4,ylim = c(0,1), ylab = "% papers with dryad dataset",xlab = "Year of publication")#,main = "Rise in dryad datasets")
points(x = full[1:6], y = dryad_rise[1:6,j],type = "b",col = coco[j], lwd = 4)
if(j == 10) legend(x = 2010, y = 1, legend = journals, fill = coco, cex = 0.8)
}

dev.print(device=pdf, "../figures/Figure3.pdf", onefile=FALSE)
dev.off()

###
###FIGURE 1
###
number_pub = c(length(pub_year[[1]]),length(pub_year[[2]]),length(pub_year[[3]]),length(pub_year[[4]]),length(pub_year[[5]]),length(pub_year[[6]]),length(pub_year[[7]]))
barplot(number_pub, names.arg =full, ylab = "Number of dryad datasets (2016 = up to May 10th)")# main = "Total number of papers with dryad dataset")
axis(1,at = 4.4, labels = "Year of publication", line = 1, lwd.ticks = 0,lty = 0)

dev.print(device=pdf, "../figures/Figure1.pdf", onefile=FALSE)
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

###
###FIGURE 2
###
par(mar = c(12,4,4,2))
barplot(t(pub_year_unique2[1:10,1:7]), beside = T,names.arg = rep("",10),col = c("grey10","gray20","gray30","gray40","gray50","gray60","gray70"),yaxt = "n", ylab ="Number of submissions") #main = "Rise in Dryad package for top 10 journals",
axis(side = 2, at = c(0,100,200,300,400,500,600),labels =c(0,100,200,300,400,500,600),lwd  = 4,col = "black")#left
mtext(rownames(pub_year_unique2)[1:10],side = 1,las = 3,at = (c(1:10)*8)-2.5,cex = 0.8)
legend(y = 500,x=45,legend = full2,fill = c("grey10","gray20","gray30","gray40","gray50","gray60","gray70"))

dev.print(device=pdf, "figures/Figure2.pdf", onefile=FALSE)
dev.off()
