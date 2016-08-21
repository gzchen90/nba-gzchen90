library(XML)
library(ggplot2)

#scrape data from basketball-reference.com
df.bbr.raw <- data.frame()
v.years <- 1990:2014																				#years to include
for(year in v.years) {
	v.url.bbr <- paste("http://www.basketball-reference.com/leagues/NBA_",year,"_totals.html",sep="")
	df.bbr.yr <- readHTMLTable(v.url.bbr,stringsAsFactors = FALSE)[[1]]								#extract table
	#rename columns: may need to update the list if bbr adds/removes columns from table
	names(df.bbr.yr) <- c("rk","player","pos","age","tm","g","gs","mp","fgm","fga","fgp","fgm3","fga3","fgp3","fgm2","fga2","fgp2","efg","ftm","fta","ftp","orb","drb","trb","ast","stl","blk","tov","pf","pts")
	df.bbr.yr <- df.bbr.yr[df.bbr.yr$rk != "Rk",]													#remove headers
	#extract unique bbr id since names are not unique
	v.bbr.id <- xpathSApply(htmlParse(v.url.bbr), '//tbody/tr/td/a', xmlAttrs)						#extract "a href" nodes from the table in the html code
	v.bbr.id <- v.bbr.id[grepl("players",v.bbr.id)]													#include only player links (not team links)
	v.bbr.id <- sapply(v.bbr.id,FUN=function(x) {
		head(unlist(strsplit(tail(unlist(strsplit(x,"/")),1),".html")),1)							#extract player id from url
	})
	df.bbr.yr$id <- v.bbr.id
	df.bbr.yr$yr <- year
	df.bbr.yr <- df.bbr.yr[,c(-1)]
	df.bbr.yr <- df.bbr.yr[,c("yr","id","player",names(df.bbr.yr)[!names(df.bbr.yr) %in% c("yr","id","player")])]
	for(i in names(df.bbr.yr)[!names(df.bbr.yr) %in% c("id","player","pos","tm")]) {
		df.bbr.yr[,i] <- as.numeric(df.bbr.yr[,i])
	}
	if(nrow(df.bbr.raw)==0) df.bbr.raw <- df.bbr.yr else df.bbr.raw <- rbind(df.bbr.raw,df.bbr.yr)	#append to df.bbr.raw
}

df.bbr <- df.bbr.raw

ggplot(data=data.frame(fgp3=sapply(unique(df.bbr$yr),FUN=function(x) sum(df.bbr$fgm3[df.bbr$yr==x])/sum(df.bbr$fga3[df.bbr$yr==x])),yr=unique(df.bbr$yr)),aes(x=yr,y=fgp3)) + geom_line() + ylab("Average 3P%") + xlab("Year")
#shorter line in 1995/1996/1997 led to artificially high percentages
#=> start from 1998
df.bbr <- df.bbr[df.bbr$yr>=1998,]

#
#eliminate duplicates of players who played for multiple teams in one year (listed under both team "TOT" and each of the individual teams)
#
dupteams <- df.bbr[df.bbr$tm=="TOT",c("id","yr")]
#keep "TOT", delete individual teams
df.bbr.2 <- df.bbr[df.bbr$tm=="TOT" | !(paste(df.bbr$id,df.bbr$yr,sep="-") %in% paste(dupteams$id,dupteams$yr,sep="-")),]
#keep individual teams, delete "TOT"
df.bbr.2a <- df.bbr[df.bbr$tm!="TOT",]

################################################################################################
#Step 1:
#
#using past 2/3/4/5 years of data, determine proper weighting for each year
################################################################################################
df.3p.t0 <- df.bbr.2[,c("id","pos","yr","age","fgm3","fga3")]
names(df.3p.t0) <- c("id","pos","yr","age_0","fgm3_0","fga3_0")
df.3p.t1 <- df.bbr.2[,c("id","yr","age","fgm3","fga3")]
names(df.3p.t1) <- c("id","yr","age_1","fgm3_1","fga3_1")
df.3p.t1$yr <- df.3p.t1$yr + 1
df.3p.t2 <- df.bbr.2[,c("id","yr","age","fgm3","fga3")]
names(df.3p.t2) <- c("id","yr","age_2","fgm3_2","fga3_2")
df.3p.t2$yr <- df.3p.t2$yr + 2
df.3p.t3 <- df.bbr.2[,c("id","yr","age","fgm3","fga3")]
names(df.3p.t3) <- c("id","yr","age_3","fgm3_3","fga3_3")
df.3p.t3$yr <- df.3p.t3$yr + 3
df.3p.t4 <- df.bbr.2[,c("id","yr","age","fgm3","fga3")]
names(df.3p.t4) <- c("id","yr","age_4","fgm3_4","fga3_4")
df.3p.t4$yr <- df.3p.t4$yr + 4
df.3p.t5 <- df.bbr.2[,c("id","yr","age","fgm3","fga3")]
names(df.3p.t5) <- c("id","yr","age_5","fgm3_5","fga3_5")
df.3p.t5$yr <- df.3p.t5$yr + 5

df.3p.t012 <- merge(merge(df.3p.t0,df.3p.t1,by=c("id","yr")),df.3p.t2,by=c("id","yr"))
df.3p.t0123 <- merge(merge(merge(df.3p.t0,df.3p.t1,by=c("id","yr")),df.3p.t2,by=c("id","yr")),df.3p.t3,by=c("id","yr"))
df.3p.t01234 <- merge(merge(merge(merge(df.3p.t0,df.3p.t1,by=c("id","yr")),df.3p.t2,by=c("id","yr")),df.3p.t3,by=c("id","yr")),df.3p.t4,by=c("id","yr"))
df.3p.t012345 <- merge(merge(merge(merge(merge(df.3p.t0,df.3p.t1,by=c("id","yr")),df.3p.t2,by=c("id","yr")),df.3p.t3,by=c("id","yr")),df.3p.t4,by=c("id","yr")),df.3p.t5,by=c("id","yr"))
df.3p.t012 <- df.3p.t012[df.3p.t012$fga3_0>0&(df.3p.t012$fga3_1+df.3p.t012$fga3_2>0),]
df.3p.t0123 <- df.3p.t0123[df.3p.t0123$fga3_0>0&(df.3p.t0123$fga3_1+df.3p.t0123$fga3_2+df.3p.t0123$fga3_3>0),]
df.3p.t01234 <- df.3p.t01234[df.3p.t01234$fga3_0>0&(df.3p.t01234$fga3_1+df.3p.t01234$fga3_2+df.3p.t01234$fga3_3+df.3p.t01234$fga3_4>0),]
df.3p.t012345 <- df.3p.t012345[df.3p.t012345$fga3_0>0&(df.3p.t012345$fga3_1+df.3p.t012345$fga3_2+df.3p.t012345$fga3_3+df.3p.t012345$fga3_4+df.3p.t012345$fga3_5>0),]
df.3p.t012$fgp3_0 <- df.3p.t012$fgm3_0/df.3p.t012$fga3_0
df.3p.t0123$fgp3_0 <- df.3p.t0123$fgm3_0/df.3p.t0123$fga3_0
df.3p.t01234$fgp3_0 <- df.3p.t01234$fgm3_0/df.3p.t01234$fga3_0
df.3p.t012345$fgp3_0 <- df.3p.t012345$fgm3_0/df.3p.t012345$fga3_0

df.3p.t012.err <- data.frame(i=numeric(0),j=numeric(0),err=numeric(0))
df.3p.t0123.err <- data.frame(i=numeric(0),j=numeric(0),k=numeric(0),err=numeric(0))
df.3p.t01234.err <- data.frame(i=numeric(0),j=numeric(0),k=numeric(0),l=numeric(0),err=numeric(0))
df.3p.t012345.err <- data.frame(i=numeric(0),j=numeric(0),k=numeric(0),l=numeric(0),m=numeric(0),err=numeric(0))
for(i in c(1:8)) {
	for(j in c(1:8)) {
		for(k in c(1:8)) {
			for(l in c(1:8)) {
				for(m in c(1:8)) {
					pred <- (df.3p.t012345$fgm3_1*i + df.3p.t012345$fgm3_2*j + df.3p.t012345$fgm3_3*k + df.3p.t012345$fgm3_4*l + df.3p.t012345$fgm3_5*m)/(df.3p.t012345$fga3_1*i + df.3p.t012345$fga3_2*j + df.3p.t012345$fga3_3*k + df.3p.t012345$fga3_4*l + df.3p.t012345$fga3_5*m)
					weight <- df.3p.t012345$fga3_0
					err <- sum((pred-df.3p.t012345$fgp3_0)^2*weight)/sum(weight)
					df.3p.t012345.err[nrow(df.3p.t012345.err)+1,] <- data.frame(i=i,j=j,k=k,l=l,m=m,err=err)
				}
				pred <- (df.3p.t01234$fgm3_1*i + df.3p.t01234$fgm3_2*j + df.3p.t01234$fgm3_3*k + df.3p.t01234$fgm3_4*l)/(df.3p.t01234$fga3_1*i + df.3p.t01234$fga3_2*j + df.3p.t01234$fga3_3*k + df.3p.t01234$fga3_4*l)
				weight <- df.3p.t01234$fga3_0
				err <- sum((pred-df.3p.t01234$fgp3_0)^2*weight)/sum(weight)
				df.3p.t01234.err[nrow(df.3p.t01234.err)+1,] <- data.frame(i=i,j=j,k=k,l=l,err=err)
			}
			pred <- (df.3p.t0123$fgm3_1*i + df.3p.t0123$fgm3_2*j + df.3p.t0123$fgm3_3*k)/(df.3p.t0123$fga3_1*i + df.3p.t0123$fga3_2*j + df.3p.t0123$fga3_3*k)
			weight <- df.3p.t0123$fga3_0
			err <- sum((pred-df.3p.t0123$fgp3_0)^2*weight)/sum(weight)
			df.3p.t0123.err[nrow(df.3p.t0123.err)+1,] <- data.frame(i=i,j=j,k=k,err=err)
		}
		pred <- (df.3p.t012$fgm3_1*i + df.3p.t012$fgm3_2*j)/(df.3p.t012$fga3_1*i + df.3p.t012$fga3_2*j)
		weight <- df.3p.t012$fga3_0
		err <- sum((pred-df.3p.t012$fgp3_0)^2*weight)/sum(weight)
		df.3p.t012.err[nrow(df.3p.t012.err)+1,] <- data.frame(i=i,j=j,err=err)
	}
}
head(df.3p.t012.err[order(df.3p.t012.err$err),])			#(8,7) for (t-1,t-2) has the lowest error, and no significant improvement in using higher coefficients (more precise weightings) than 8
head(df.3p.t0123.err[order(df.3p.t0123.err$err),])			#(8,7,6) for (t-1,t-2,t-3) has the lowest error using (8,7) for (t-1,t-2)
head(df.3p.t01234.err[order(df.3p.t01234.err$err),])		#(8,7,6,5) for (t-1,t-2,t-3,t-4) has the lowest error using (8,7,6) for (t-1,t-2,t-3)
head(df.3p.t012345.err[order(df.3p.t012345.err$err),])		#(8,7,6,5,4) for (t-1,t-2,t-3,t-4,t-5) has the lowest error using (8,7,6,5) for (t-1,t-2,t-3,t-4)

w1 <- 8
w2 <- 7
w3 <- 6
w4 <- 5
w5 <- 4

################################################################################################
#Step 2:
#
#Regression to the mean
#
#reliability of 3pt% = r
#r = opps/(opps + constant) => constant = (1-r) * opps/r
#Regressed rate = (PlayerObsRate*r + PopMeanRate*(1 - r))/(r + 1 - r) = PopMeanRate + r *(PlayerObsRate - PopMeanRate)
################################################################################################

#From http://nyloncalculus.com/2014/08/29/long-take-three-point-shooting-stabilize/
r <- 0.7
opps <- 750
r_const <- (1-r) * opps/r

df.bbr.3a <- df.bbr.2a[df.bbr.2a$fga3>0&df.bbr.2a$pos %in% c("PG","SG","SF","PF","C"), c("yr","pos","id","fgm3","fga3","fgp3")]
df.3p.pos.mean <- aggregate(df.bbr.3a[c("fgm3","fga3")],df.bbr.3a[c("yr","pos")],FUN=sum)
df.3p.pos.mean$fgp3 <- df.3p.pos.mean$fgm3/df.3p.pos.mean$fga3

ggplot(data=df.3p.pos.mean, aes(x=yr,y=fgp3,color=pos)) + geom_line() + ylab("Average 3P% By Position") + xlab("Year") + scale_color_discrete(name="Position")

#small sample sizes (especially for centers) leads to lots of noise
#single year positional averages might not be good measure of true population average
#=> weighted average of multiple seasons
df.3p.pos.mean$fgp3_mult <- sapply(1:nrow(df.3p.pos.mean),function(x) {
	yr <- df.3p.pos.mean$yr[x]
	pos <- df.3p.pos.mean$pos[x]
	if(yr-min(df.3p.pos.mean$yr[df.3p.pos.mean$pos==pos])>=4) {
		fgm3_mult <- w1*df.3p.pos.mean$fgm3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr] + w2*df.3p.pos.mean$fgm3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr-1] + w3*df.3p.pos.mean$fgm3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr-2] + w4*df.3p.pos.mean$fgm3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr-3] + w5*df.3p.pos.mean$fgm3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr-4]
		fga3_mult <- w1*df.3p.pos.mean$fga3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr] + w2*df.3p.pos.mean$fga3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr-1] + w3*df.3p.pos.mean$fga3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr-2] + w4*df.3p.pos.mean$fga3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr-3] + w5*df.3p.pos.mean$fga3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr-4]
		fgp3_mult <- fgm3_mult/fga3_mult
	} else if(yr-min(df.3p.pos.mean$yr[df.3p.pos.mean$pos==pos])==3) {
		fgm3_mult <- w1*df.3p.pos.mean$fgm3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr] + w2*df.3p.pos.mean$fgm3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr-1] + w3*df.3p.pos.mean$fgm3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr-2] + w4*df.3p.pos.mean$fgm3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr-3]
		fga3_mult <- w1*df.3p.pos.mean$fga3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr] + w2*df.3p.pos.mean$fga3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr-1] + w3*df.3p.pos.mean$fga3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr-2] + w4*df.3p.pos.mean$fga3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr-3]
		fgp3_mult <- fgm3_mult/fga3_mult
	} else if(yr-min(df.3p.pos.mean$yr[df.3p.pos.mean$pos==pos])==2) {
		fgm3_mult <- w1*df.3p.pos.mean$fgm3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr] + w2*df.3p.pos.mean$fgm3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr-1] + w3*df.3p.pos.mean$fgm3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr-2]
		fga3_mult <- w1*df.3p.pos.mean$fga3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr] + w2*df.3p.pos.mean$fga3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr-1] + w3*df.3p.pos.mean$fga3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr-2]
		fgp3_mult <- fgm3_mult/fga3_mult
	} else if(yr-min(df.3p.pos.mean$yr[df.3p.pos.mean$pos==pos])==1) {
		fgm3_mult <- w1*df.3p.pos.mean$fgm3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr] + w2*df.3p.pos.mean$fgm3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr-1]
		fga3_mult <- w1*df.3p.pos.mean$fga3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr] + w2*df.3p.pos.mean$fga3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr-1]
		fgp3_mult <- fgm3_mult/fga3_mult
	} else {
		fgm3_mult <- w1*df.3p.pos.mean$fgm3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr]
		fga3_mult <- w1*df.3p.pos.mean$fga3[df.3p.pos.mean$pos==pos&df.3p.pos.mean$yr==yr]
		fgp3_mult <- fgm3_mult/fga3_mult
	}
})
ggplot(data=df.3p.pos.mean, aes(x=yr,y=fgp3_mult,color=pos)) + geom_line() + ylab("Average 5-year 3P% By Position") + xlab("Year") + scale_color_discrete(name="Position")
################################################################################################
#Step 1':
#
#rerun coefficients after incorporating regression to mean
################################################################################################
df.3p.t0 <- df.bbr.2[,c("id","pos","yr","age","fgm3","fga3")]
names(df.3p.t0) <- c("id","pos","yr","age_0","fgm3_0","fga3_0")
df.3p.t1 <- df.bbr.2[,c("id","yr","age","fgm3","fga3")]
names(df.3p.t1) <- c("id","yr","age_1","fgm3_1","fga3_1")
df.3p.t1$yr <- df.3p.t1$yr + 1
df.3p.t2 <- df.bbr.2[,c("id","yr","age","fgm3","fga3")]
names(df.3p.t2) <- c("id","yr","age_2","fgm3_2","fga3_2")
df.3p.t2$yr <- df.3p.t2$yr + 2
df.3p.t3 <- df.bbr.2[,c("id","yr","age","fgm3","fga3")]
names(df.3p.t3) <- c("id","yr","age_3","fgm3_3","fga3_3")
df.3p.t3$yr <- df.3p.t3$yr + 3
df.3p.t4 <- df.bbr.2[,c("id","yr","age","fgm3","fga3")]
names(df.3p.t4) <- c("id","yr","age_4","fgm3_4","fga3_4")
df.3p.t4$yr <- df.3p.t4$yr + 4
df.3p.t5 <- df.bbr.2[,c("id","yr","age","fgm3","fga3")]
names(df.3p.t5) <- c("id","yr","age_5","fgm3_5","fga3_5")
df.3p.t5$yr <- df.3p.t5$yr + 5

df.3p.t012 <- merge(merge(df.3p.t0,df.3p.t1,by=c("id","yr")),df.3p.t2,by=c("id","yr"))
df.3p.t0123 <- merge(merge(merge(df.3p.t0,df.3p.t1,by=c("id","yr")),df.3p.t2,by=c("id","yr")),df.3p.t3,by=c("id","yr"))
df.3p.t01234 <- merge(merge(merge(merge(df.3p.t0,df.3p.t1,by=c("id","yr")),df.3p.t2,by=c("id","yr")),df.3p.t3,by=c("id","yr")),df.3p.t4,by=c("id","yr"))
df.3p.t012345 <- merge(merge(merge(merge(merge(df.3p.t0,df.3p.t1,by=c("id","yr")),df.3p.t2,by=c("id","yr")),df.3p.t3,by=c("id","yr")),df.3p.t4,by=c("id","yr")),df.3p.t5,by=c("id","yr"))
df.3p.t012 <- df.3p.t012[df.3p.t012$fga3_0>0&(df.3p.t012$fga3_1+df.3p.t012$fga3_2>0),]
df.3p.t0123 <- df.3p.t0123[df.3p.t0123$fga3_0>0&(df.3p.t0123$fga3_1+df.3p.t0123$fga3_2+df.3p.t0123$fga3_3>0),]
df.3p.t01234 <- df.3p.t01234[df.3p.t01234$fga3_0>0&(df.3p.t01234$fga3_1+df.3p.t01234$fga3_2+df.3p.t01234$fga3_3+df.3p.t01234$fga3_4>0),]
df.3p.t012345 <- df.3p.t012345[df.3p.t012345$fga3_0>0&(df.3p.t012345$fga3_1+df.3p.t012345$fga3_2+df.3p.t012345$fga3_3+df.3p.t012345$fga3_4+df.3p.t012345$fga3_5>0),]
df.3p.t012$fgp3_0 <- df.3p.t012$fgm3_0/df.3p.t012$fga3_0
df.3p.t0123$fgp3_0 <- df.3p.t0123$fgm3_0/df.3p.t0123$fga3_0
df.3p.t01234$fgp3_0 <- df.3p.t01234$fgm3_0/df.3p.t01234$fga3_0
df.3p.t012345$fgp3_0 <- df.3p.t012345$fgm3_0/df.3p.t012345$fga3_0

df.3p.pos.mean.off <- df.3p.pos.mean[,c("yr","pos","fgp3_mult")]
df.3p.pos.mean.off$yr <- df.3p.pos.mean.off$yr + 1
df.3p.t012 <- merge(df.3p.t012,df.3p.pos.mean.off,by=c("yr","pos"))
df.3p.t0123 <- merge(df.3p.t0123,df.3p.pos.mean.off,by=c("yr","pos"))
df.3p.t01234 <- merge(df.3p.t01234,df.3p.pos.mean.off,by=c("yr","pos"))
df.3p.t012345 <- merge(df.3p.t012345,df.3p.pos.mean.off,by=c("yr","pos"))

df.3p.t012.err <- data.frame(i=numeric(0),j=numeric(0),err=numeric(0))
df.3p.t0123.err <- data.frame(i=numeric(0),j=numeric(0),k=numeric(0),err=numeric(0))
df.3p.t01234.err <- data.frame(i=numeric(0),j=numeric(0),k=numeric(0),l=numeric(0),err=numeric(0))
df.3p.t012345.err <- data.frame(i=numeric(0),j=numeric(0),k=numeric(0),l=numeric(0),m=numeric(0),err=numeric(0))
for(i in c(1:10)) {
	for(j in c(1:i)) {
		for(k in c(1:j)) {
			for(l in c(1:k)) {
				for(m in c(1:l)) {
					pred_raw <- (df.3p.t012345$fgm3_1*i + df.3p.t012345$fgm3_2*j + df.3p.t012345$fgm3_3*k + df.3p.t012345$fgm3_4*l + df.3p.t012345$fgm3_5*m)/(df.3p.t012345$fga3_1*i + df.3p.t012345$fga3_2*j + df.3p.t012345$fga3_3*k + df.3p.t012345$fga3_4*l + df.3p.t012345$fga3_5*m)
					fga3 <- (df.3p.t012345$fga3_1*i + df.3p.t012345$fga3_2*j + df.3p.t012345$fga3_3*k + df.3p.t012345$fga3_4*l + df.3p.t012345$fga3_5*m)*5/(i+j+k+l+m)
					reliability <- fga3/(fga3+r_const)
					pred <- pred_raw*reliability + df.3p.t012345$fgp3_mult*(1-reliability)
					weight <- df.3p.t012345$fga3_0
					err <- sum((pred-df.3p.t012345$fgp3_0)^2*weight)/sum(weight)
					df.3p.t012345.err[nrow(df.3p.t012345.err)+1,] <- data.frame(i=i,j=j,k=k,l=l,m=m,err=err)
				}
				pred_raw <- (df.3p.t01234$fgm3_1*i + df.3p.t01234$fgm3_2*j + df.3p.t01234$fgm3_3*k + df.3p.t01234$fgm3_4*l)/(df.3p.t01234$fga3_1*i + df.3p.t01234$fga3_2*j + df.3p.t01234$fga3_3*k + df.3p.t01234$fga3_4*l)
				fga3 <- (df.3p.t01234$fga3_1*i + df.3p.t01234$fga3_2*j + df.3p.t01234$fga3_3*k + df.3p.t01234$fga3_4*l)*4/(i+j+k+l)
				reliability <- fga3/(fga3+r_const)
				pred <- pred_raw*reliability + df.3p.t01234$fgp3_mult*(1-reliability)
				weight <- df.3p.t01234$fga3_0
				err <- sum((pred-df.3p.t01234$fgp3_0)^2*weight)/sum(weight)
				df.3p.t01234.err[nrow(df.3p.t01234.err)+1,] <- data.frame(i=i,j=j,k=k,l=l,err=err)
			}
			pred_raw <- (df.3p.t0123$fgm3_1*i + df.3p.t0123$fgm3_2*j + df.3p.t0123$fgm3_3*k)/(df.3p.t0123$fga3_1*i + df.3p.t0123$fga3_2*j + df.3p.t0123$fga3_3*k)
			fga3 <- (df.3p.t0123$fga3_1*i + df.3p.t0123$fga3_2*j + df.3p.t0123$fga3_3*k)*3/(i+j+k)
			reliability <- fga3/(fga3+r_const)
			pred <- pred_raw*reliability + df.3p.t0123$fgp3_mult*(1-reliability)
			weight <- df.3p.t0123$fga3_0
			err <- sum((pred-df.3p.t0123$fgp3_0)^2*weight)/sum(weight)
			df.3p.t0123.err[nrow(df.3p.t0123.err)+1,] <- data.frame(i=i,j=j,k=k,err=err)
		}
		pred_raw <- (df.3p.t012$fgm3_1*i + df.3p.t012$fgm3_2*j)/(df.3p.t012$fga3_1*i + df.3p.t012$fga3_2*j)
		fga3 <- (df.3p.t012$fga3_1*i + df.3p.t012$fga3_2*j)*2/(i+j)
		reliability <- fga3/(fga3+r_const)
		pred <- pred_raw*reliability + df.3p.t012$fgp3_mult*(1-reliability)
		weight <- df.3p.t012$fga3_0
		err <- sum((pred-df.3p.t012$fgp3_0)^2*weight)/sum(weight)
		df.3p.t012.err[nrow(df.3p.t012.err)+1,] <- data.frame(i=i,j=j,err=err)
	}
}
head(df.3p.t012.err[order(df.3p.t012.err$err),])			#(10,8) for (t-1,t-2) has the lowest error, and no significant improvement in using higher coefficients (more precise weightings) than 8
head(df.3p.t0123.err[order(df.3p.t0123.err$err),])			#(10,8,6) for (t-1,t-2,t-3) has the lowest error using (10,8) for (t-1,t-2)
head(df.3p.t01234.err[order(df.3p.t01234.err$err),])		#(10,8,6,5) for (t-1,t-2,t-3,t-4) has the lowest error using (10,8,6) for (t-1,t-2,t-3)
head(df.3p.t012345.err[order(df.3p.t012345.err$err),])		#(10,8,6,5,3) for (t-1,t-2,t-3,t-4,t-5) has the lowest error using (10,8,6,5) for (t-1,t-2,t-3,t-4)
#no significant improvement from using 3 years of data to 4/5
#for sake of simplicity, only 3 years of data will be used with coefficients (10,8,6), or simplified, (5,4,3)
w1 <- 5
w2 <- 4
w3 <- 3
w4 <- NULL
w5 <- NULL

################################################################################################
#Step 3: 
#Aging Curve:
################################################################################################
#delta of 3fg% by 3fga
df.3p <- df.bbr.2[,c("id","yr","age","fgm3","fga3")]
df.3p.next <- df.3p
names(df.3p.next) <- c("id","yr","nextage","nextfgm3","nextfga3")
df.3p.next$yr <- df.3p.next$yr - 1
df.bbr.delta <- merge(df.3p, df.3p.next, by=c("id","yr"))
#eliminate delta pairs that don't have at least one attempt in both years
df.bbr.delta <- df.bbr.delta[df.bbr.delta$fga3>0 & df.bbr.delta$nextfga3>0,]
#determine deltas for each player and weight by harmonic mean of the number of attempts in the two years
df.bbr.delta$weighting <- 1/rowMeans(1/df.bbr.delta[,c("fga3","nextfga3")])
df.bbr.delta$dfgp3 <- df.bbr.delta$nextfgm3/df.bbr.delta$nextfga3 - df.bbr.delta$fgm3/df.bbr.delta$fga3
df.bbr.delta$dfgp3_w <- df.bbr.delta$dfgp3 * df.bbr.delta$weighting
df.aging <- aggregate(df.bbr.delta[c("dfgp3_w","weighting")],df.bbr.delta["age"],FUN=sum)
df.aging$dfgp3_avg <- df.aging$dfgp3_w/df.aging$weighting

ggplot(data=df.aging,aes(x=age,y=dfgp3_avg)) + geom_point(shape=1) + ylab("Average Change in 3P%") + xlab("Age in Year 1")
#looks mostly linear
#while it doesn't make sense conceptually for the deltas to have relative extrema (i.e. they should be monotonic),
#we'll check a few different degrees for the polynomial of best fit
lm1 <- lm(dfgp3_avg~poly(age,1,raw=TRUE),data=df.aging,weights=weighting)
lm2 <- lm(dfgp3_avg~poly(age,2,raw=TRUE),data=df.aging,weights=weighting)
lm3 <- lm(dfgp3_avg~poly(age,3,raw=TRUE),data=df.aging,weights=weighting)
lm4 <- lm(dfgp3_avg~poly(age,4,raw=TRUE),data=df.aging,weights=weighting)
lm5 <- lm(dfgp3_avg~poly(age,5,raw=TRUE),data=df.aging,weights=weighting)
lm6 <- lm(dfgp3_avg~poly(age,6,raw=TRUE),data=df.aging,weights=weighting)
summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)
summary(lm5)
summary(lm6)
#=> polynomial of degree 1 has highest adjusted r^2

#use 10-fold cross-validation (100 times each) to compare testing errors of different degrees
n <- 100
k <- 10
maxdeg <- 6				#max polynomial degree to test
#randomly divide into k samples of same size
df.bbr.delta$random <- rnorm(nrow(df.bbr.delta))
df.bbr.delta$randomcut <- factor(cut(df.bbr.delta$random,quantile(df.bbr.delta$random,seq(0,1,by=1/k)),include.lowest=TRUE,labels=FALSE))
#average errors for each polynomial degree and each k-test
df.bbr.delta.err <- as.data.frame(matrix(0,nrow=n*k,ncol=maxdeg+1))
names(df.bbr.delta.err) <- paste("d",c(0:maxdeg),sep="")
for(i in 1:n) {
	for(j in 1:k) {
		df.bbr.training <- df.bbr.delta[df.bbr.delta$randomcut!=j,]
		df.bbr.testing <- df.bbr.delta[df.bbr.delta$randomcut==j,]
		for(d in 0:maxdeg)
		{
			if(d==0) {
				model <- lm(dfgp3~1,data=df.bbr.training,weights=weighting)
			} else {
				model <- lm(dfgp3~poly(age,d,raw=TRUE),data=df.bbr.training,weights=weighting)
			}
			testing.fits <- predict(model,data.frame(age=df.bbr.testing$age))
			df.bbr.delta.err[(i-1)*k+j,d+1] <- sum(df.bbr.testing$weighting*(df.bbr.testing$dfgp3-testing.fits)^2)/sum(df.bbr.testing$weighting)
		}
	}
}
df.bbr.delta.err[nrow(df.bbr.delta.err)+1,] <- colSums(df.bbr.delta.err)
rownames(df.bbr.delta.err)[nrow(df.bbr.delta.err)] <- "total"
tail(df.bbr.delta.err,1)
#=> no significant difference in average testing errors between different polynomial degrees
#while degree 1 doesn't always exhibit the lowest testing error, it usually does

df.aging <- merge(data.frame(age=c((min(df.aging$age)-1):max(df.aging$age))),df.aging,all.x=TRUE)
df.aging$dfgp3_fit <- predict(lm1,data.frame(age=df.aging$age))
df.aging$dfgp3_fit_cum <- cumsum(df.aging$dfgp3_fit)
df.aging$dfgp3_fit_cum <- c(0,df.aging$dfgp3_fit_cum[-length(df.aging$dfgp3_fit_cum)])

ggplot(data=df.aging) + geom_point(aes(x=age,y=dfgp3_avg),shape=1) + geom_line(aes(x=age,y=dfgp3_fit)) + ylab("Average Change in 3P%") + xlab("Age in Year 1")

ggplot(data=df.aging,aes(x=age,y=dfgp3_fit_cum+0.30)) + geom_line() + ylab("3P%") + xlab("Age")			#the projected progression of a 30% 17-year old shooter




################################################################################################
#Step 4: 
#Backtest using available info at the time:
#(2001-2014)
################################################################################################

df.backtest.err <- data.frame(yr=numeric(0),err=numeric(0))
for(year in (min(df.bbr.2$yr)+3):max(df.bbr.2$yr)) {
	df.actual <- df.bbr.2[df.bbr.2$yr==year,c("yr","id","player","pos","age","fgm3","fga3","fgp3")]
	df.actual <- df.actual[df.actual$fga3>0,]
	df.actual.a <- df.bbr.2a[df.bbr.2a$yr==year,c("yr","id","pos","mp")]
	#players that change teams in a year sometimes are listed under different positions; use the position with the most minutes
	df.actual$pos[!df.actual$pos %in% c("PG","SG","SF","PF","C")] <- sapply(df.actual$id[!df.actual$pos %in% c("PG","SG","SF","PF","C")],FUN=function(x) {
		df.actual.a.x <- df.actual.a[df.actual.a$id==x,]
		df.actual.a$pos[which.max(df.actual.a$mp)]
	})
	df.actual.1 <- df.bbr.2[df.bbr.2$yr==year-1,c("id","fgm3","fga3")]
	names(df.actual.1) <- c("id","fgm3_1","fga3_1")
	df.actual.2 <- df.bbr.2[df.bbr.2$yr==year-2,c("id","fgm3","fga3")]
	names(df.actual.2) <- c("id","fgm3_2","fga3_2")
	df.actual.3 <- df.bbr.2[df.bbr.2$yr==year-3,c("id","fgm3","fga3")]
	names(df.actual.3) <- c("id","fgm3_3","fga3_3")
	df.prediction <- merge(merge(merge(data.frame(df.actual[,c("id","pos","age")]),df.actual.1,by="id",all.x=TRUE),df.actual.2,by="id",all.x=TRUE),df.actual.3,by="id",all.x=TRUE)
	df.prediction[is.na(df.prediction)] <- 0
	df.prediction <- merge(df.prediction,df.3p.pos.mean[df.3p.pos.mean$yr==year-1,c("pos","fgp3_mult")],by=c("pos"))
	df.prediction$pred_raw <- (df.prediction$fgm3_1*w1 + df.prediction$fgm3_2*w2 + df.prediction$fgm3_3*w3)/(df.prediction$fga3_1*w1 + df.prediction$fga3_2*w2 + df.prediction$fga3_3*w3)
	df.prediction$fga3_w <- (df.prediction$fga3_1*w1 + df.prediction$fga3_2*w2 + df.prediction$fga3_3*w3)*3/(w1+w2+w3)
	df.prediction$r <- df.prediction$fga3_w/(df.prediction$fga3_w+r_const)
	df.prediction$pred_rtm <- df.prediction$pred_raw*df.prediction$r + df.prediction$fgp3_mult*(1-df.prediction$r)
	df.prediction$pred <- df.prediction$pred_rtm + sapply(df.prediction$age-1,FUN=function(x) df.aging$dfgp3_fit[df.aging$age==x])
	#use positional mean if no prior data for player (i.e. rookies)
	df.prediction$pred[is.na(df.prediction$pred)] <- df.prediction$fgp3_mult[is.na(df.prediction$pred)]
	df.prediction <- merge(df.prediction,df.actual[,c("id","fga3","fgp3")],by="id")
	df.prediction$err <- df.prediction$pred - df.prediction$fgp3
	err <- sum((df.prediction$err)^2*df.prediction$fga3)/sum(df.prediction$fga3)
	df.backtest.err[nrow(df.backtest.err)+1,] <- data.frame(yr=year,err=err)
}

#use bbr's Simple Projection System for comparison
df.sps.err <- data.frame(yr=numeric(0),err=numeric(0))
for(year in (min(df.bbr.2$yr)+3):max(df.bbr.2$yr)) {
	df.actual <- df.bbr.2[df.bbr.2$yr==year,c("yr","id","player","pos","age","fgm3","fga3","fgp3")]
	df.actual <- df.actual[df.actual$fga3>0,]
	df.actual.a <- df.bbr.2a[df.bbr.2a$yr==year,c("yr","id","pos","mp")]
	#players that change teams in a year sometimes are listed under different positions; use the position with the most minutes
	df.actual$pos[!df.actual$pos %in% c("PG","SG","SF","PF","C")] <- sapply(df.actual$id[!df.actual$pos %in% c("PG","SG","SF","PF","C")],FUN=function(x) {
		df.actual.a.x <- df.actual.a[df.actual.a$id==x,]
		df.actual.a$pos[which.max(df.actual.a$mp)]
	})
	
	#extract SPS projections from bbr
	url.sps <- paste("http://www.basketball-reference.com/friv/projections.cgi?year=",year,sep="")
	df.sps.yr <- readHTMLTable(url.sps,stringsAsFactors = FALSE)[[1]]		#extract table
	df.sps.yr <- df.sps.yr[!df.sps.yr$Rk %in% c("Rk",""),]			#remove headers
	#extract bbr id
	v.bbr.id <- xpathSApply(htmlParse(url.sps), '//tbody/tr/td/a', xmlAttrs)	#extract "a href" nodes from the table in the html code
	v.bbr.id <- sapply(v.bbr.id,FUN=function(x) {
		head(unlist(strsplit(tail(unlist(strsplit(x,"/")),1),".html")),1)		#extract player id from url
	})
	df.sps.yr$id <- v.bbr.id
	df.sps.yr <- df.sps.yr[df.sps.yr$Type=="Projected",]
	df.sps.yr[,"3P%"] <- as.numeric(df.sps.yr[,"3P%"])

	df.sps.prediction <- merge(df.actual[,c("id","pos","fga3","fgp3")],df.sps.yr,by="id",all.x=TRUE)
	df.sps.prediction <- merge(df.sps.prediction,df.3p.pos.mean[df.3p.pos.mean$yr==year-1,c("pos","fgp3_mult")],by=c("pos"))
	df.sps.prediction[is.na(df.sps.prediction[,"3P%"]),"3P%"] <- df.sps.prediction$fgp3_mult[is.na(df.sps.prediction[,"3P%"])]
	df.sps.prediction$err <- df.sps.prediction[,"3P%"] - df.sps.prediction$fgp3
	err <- sum((df.sps.prediction$err)^2*df.sps.prediction$fga3)/sum(df.sps.prediction$fga3)
	df.sps.err[nrow(df.sps.err)+1,] <- data.frame(yr=year,err=err)
}

df.backtest.err$err < df.sps.err$err
ggplot(data=data.frame(yr=rep((min(df.bbr.2$yr)+3):max(df.bbr.2$yr),2),err=100*c(df.backtest.err$err,df.sps.err$err),model=rep(c("Mine","SPS"),each=max(df.bbr.2$yr)-min(df.bbr.2$yr)-2)),aes(x=yr,y=err,color=model)) + geom_line() + ylab("Weighted Average Error (3P%)") + xlab("Year") + scale_color_discrete(name="Model")
#every year but 2002, my model produces lower errors than SPS, and in 2002 it's only worse by a small amount

################################################################################################
#Step 5: 
#Run for 2015:
################################################################################################

year.curr <- 2015

#get list of current teams
url.teams <- "http://www.basketball-reference.com/teams"
v.teams <- xpathSApply(htmlParse(url.teams), '//tbody/tr/td/a', xmlAttrs)
v.teams <- v.teams[1:30]
#manually update NJN=>BRK, CHA=>CHO, NOH=>NOP link change
v.teams[v.teams=="/teams/NJN/"] <-"/teams/BRK/"
v.teams[v.teams=="/teams/CHA/"] <-"/teams/CHO/"
v.teams[v.teams=="/teams/NOH/"] <-"/teams/NOP/"

#get list of current players (defined as anyone on a roster)
df.players <- data.frame(player=character(0),id=character(0),pos=character(0))
for(team in v.teams) {
	url.roster <- paste("http://www.basketball-reference.com",team,year.curr,".html",sep="")
	
	#extract the correct table with the rosters (not always first table) by searching for "Birth Date"
	df.roster <- readHTMLTable(url.roster,stringsAsFactors = FALSE)
	for(i in 1:length(df.roster)) {
		if("Birth Date" %in% names(df.roster[[i]])) {
			n <- i
			df.roster <- df.roster[[i]]
			break
		}
	}
	names(df.roster) <- tolower(names(df.roster))
	
	#add bbr id
	v.players.team <- xpathSApply(htmlParse(url.roster), paste("//tbody[",n,"]/tr/td/a",sep=""), xmlAttrs)		#extract "a href" nodes from the roster table in the html code
	v.players.team <- v.players.team[grepl("players",v.players.team)]											#include only player links (not team links)
	v.players.team <- sapply(v.players.team,FUN=function(x) {
		head(unlist(strsplit(tail(unlist(strsplit(x,"/")),1),".html")),1)										#extract player id from url
	})
	
	df.roster$id <- v.players.team
	df.roster <- df.roster[,c("player","id","pos")]
	df.players <- rbind(df.players,df.roster)
}

#players that changed teams last year sometimes are listed under different positions; use the position with the most minutes from last year
df.players.a <- df.bbr.2a[df.bbr.2a$yr==year.curr-1,c("yr","id","pos","mp")]
df.players.b <- df.bbr.2a[df.bbr.2a$yr==year.curr-2,c("yr","id","pos","mp")]
df.players.c <- df.bbr.2a[df.bbr.2a$yr==year.curr-3,c("yr","id","pos","mp")]
df.players$pos[!df.players$pos %in% c("PG","SG","SF","PF","C")] <- sapply(df.players$id[!df.players$pos %in% c("PG","SG","SF","PF","C")],FUN=function(x) {
	df.players.a.x <- df.players.a[df.players.a$id==x,]
	df.players.b.x <- df.players.b[df.players.b$id==x,]
	df.players.c.x <- df.players.c[df.players.c$id==x,]
	if(nrow(df.players.a.x)>0) df.players.x <- df.players.a.x else if(nrow(df.players.b.x)>0) df.players.x <- df.players.b.x else if(nrow(df.players.c.x)>0) df.players.x <- df.players.c.x else df.players.x <- df.players.a.x[0,]
	if(nrow(df.players.x)>0) df.players.x$pos[which.max(df.players.x$mp)] else NA
})

#the remaining players with mixed positions are set manually using the position assigned to them by ESPN
df.players[!df.players$pos %in% c("PG","SG","SF","PF","C"),]
df.players$pos[df.players$id=="paynead01"] <- "PF"df.players$pos[df.players$id=="poweldw01"] <- "PF"df.players$pos[df.players$id=="brownma02"] <- "SG"df.players$pos[df.players$id=="hairspj02"] <- "SG"df.players$pos[df.players$id=="vonleno01"] <- "PF"df.players$pos[df.players$id=="hairspj01"] <- "SG"df.players$pos[df.players$id=="cherrwi01"] <- "PG"df.players$pos[df.players$id=="harriga01"] <- "SG"df.players$pos[df.players$id=="greener01"] <- "PG"df.players$pos[df.players$id=="whittsh01"] <- "PF"df.players$pos[df.players$id=="wilcocj01"] <- "SG"df.players$pos[df.players$id=="stokeja01"] <- "PF"df.players$pos[df.players$id=="inglida01"] <- "SF"df.players$pos[df.players$id=="obryajo01"] <- "PF"df.players$pos[df.players$id=="robingl02"] <- "SG"df.players$pos[df.players$id=="smithru01"] <- "PG"df.players$pos[df.players$id=="youngpa01"] <- "PF"df.players$pos[df.players$id=="jerregr01"] <- "PF"df.players$pos[df.players$id=="mcgarmi01"] <- "PF"df.players$pos[df.players$id=="embiijo01"] <- "C"df.players$pos[df.players$id=="grantje01"] <- "SF"df.players$pos[df.players$id=="warretj01"] <- "SF"df.players$pos[df.players$id=="dragizo01"] <- "SG"df.players$pos[df.players$id=="anderky01"] <- "SF"df.players$pos[df.players$id=="cabocbr01"] <- "SF"

#eliminate the second PJ Hairston
df.players <- df.players[!df.players$id=="hairspj02",]

df.actual.1 <- df.bbr.2[df.bbr.2$yr==year.curr-1,c("id","fgm3","fga3","age")]
names(df.actual.1) <- c("id","fgm3_1","fga3_1","age_1")
df.actual.2 <- df.bbr.2[df.bbr.2$yr==year.curr-2,c("id","fgm3","fga3","age")]
names(df.actual.2) <- c("id","fgm3_2","fga3_2","age_2")
df.actual.3 <- df.bbr.2[df.bbr.2$yr==year.curr-3,c("id","fgm3","fga3","age")]
names(df.actual.3) <- c("id","fgm3_3","fga3_3","age_3")
df.prediction <- merge(merge(merge(data.frame(df.players),df.actual.1,by="id",all.x=TRUE),df.actual.2,by="id",all.x=TRUE),df.actual.3,by="id",all.x=TRUE)
df.prediction[is.na(df.prediction)] <- 0
df.prediction$age_1[df.prediction$age_1==0] <- NA
df.prediction$age_2[df.prediction$age_2==0] <- NA
df.prediction$age_3[df.prediction$age_3==0] <- NA
df.prediction$age <- sapply(1:nrow(df.prediction),FUN=function(x) if(!is.na(df.prediction$age_1[x])) df.prediction$age_1[x]+1 else if(!is.na(df.prediction$age_2[x])) df.prediction$age_2[x]+2 else if(!is.na(df.prediction$age_3[x])) df.prediction$age_3[x]+3 else NA)
df.prediction <- merge(df.prediction,df.3p.pos.mean[df.3p.pos.mean$yr==year.curr-1,c("pos","fgp3_mult")],by=c("pos"))
df.prediction$pred_raw <- (df.prediction$fgm3_1*w1 + df.prediction$fgm3_2*w2 + df.prediction$fgm3_3*w3)/(df.prediction$fga3_1*w1 + df.prediction$fga3_2*w2 + df.prediction$fga3_3*w3)
df.prediction$fga3_w <- (df.prediction$fga3_1*w1 + df.prediction$fga3_2*w2 + df.prediction$fga3_3*w3)*3/(w1+w2+w3)
df.prediction$r <- df.prediction$fga3_w/(df.prediction$fga3_w+r_const)
df.prediction$pred_rtm <- df.prediction$pred_raw*df.prediction$r + df.prediction$fgp3_mult*(1-df.prediction$r)
df.prediction$pred[!is.na(df.prediction$age)] <- df.prediction$pred_rtm[!is.na(df.prediction$age)] + sapply(df.prediction$age[!is.na(df.prediction$age)]-1,FUN=function(x) df.aging$dfgp3_fit[df.aging$age==x])
#use positional mean if no prior data for player (i.e. rookies)
df.prediction$pred[is.na(df.prediction$pred)] <- df.prediction$fgp3_mult[is.na(df.prediction$pred)]
df.output <- df.prediction[,c("player","id","pred")]
df.output <- df.output[order(df.output$id),]
names(df.output) <- c("player","bbr_id","predicted_3p%")
df.output
write.csv(df.output,"2015_3pt%_projections.csv",row.names=FALSE)