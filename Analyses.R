{# TO DO 


 - update column definition
-  # at the end created final csv file containing only data used in some analyses (exclude everything that will not show up in the analyses)
			d<-read.csv(file=paste(wd, "observations_all.csv", sep=''),header=T,sep=",", fill=T, stringsAsFactors=FALSE)
			d=d[!is.na(d$obs_ID),]
			d=d[!d$obs_ID%in%e$obs_ID[which(e$total_exclusion=='y')],]
			write.csv(d, file=paste(wd,'observations.csv', sep=''), row.names=FALSE)	
			
			b<-read.csv(file=paste(wd, "time_series.csv", sep=''),header=T,sep=",", fill=T, stringsAsFactors=FALSE)
			b=b[!is.na(b$obs_ID),]
			b=b[!b$obs_ID%in%e$obs_ID[which(e$total_exclusion=='y')],]
						
 # exclude or control for
	# capture else keep only capture in 'n' and >3
 
 ##### SEE WHETHER SHORTER OBS MATTER OR NOT
}
{# INFO
 # not used at all
	# s608 - video time in 2000 and impossible to calibrate with the RFID data
	# non-exchange observation that are 30 min before/after exchange observation

 # not used for bout length analyses
	# obs_ID 274 - male just returned after release from captivity
	# ?? -	obs_ID 187 nest s409 the only exchange observation with left_before_presence as NA. Can we do something about it so that we do not have to explain why one observation out of all is NA? The difference in dt_first_present and dt_left is only 5s. 
 
 }
{# INFO
	# each bird was associated with a signle nest and a single year
	# add to methods: For comparison of behaviour of incubating bird during ~30 min prior to return of its partner with the behaviour during regular incubation, we randomly sampled the regular incubation behaviour for ~30 min. Such observatins were at least 30 min before start and 30 min after the end of an actual exchange (i.e. 30min before the incubating parent left the nest and 30 min after the coming bird set down on the nest). 
}

{# SETTINGS & DATA
	{# do you want plots in R (PNG=FALSE) or as PNG (PNG = TRUE)?
		PNG=FALSE
	}
	{# define working directory
	     wd = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Exchanges_SESA/Database/"
		 wd2 = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Exchanges_SESA/Analyses/"		 
	     outdir = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Exchanges_SESA/Analyses/plots/" 
		#wd = "C:/Users/OWNER/Dropbox/exchanges_SESA/Database/"	
	    #wd = paste0(getwd(), '/DATA/')
	}
	{# load packages, constants and data
		source(paste(wd2, 'Constants_packages_PrepareData.R',sep=""))
	}
}

 ######### ADD LATER How long were birds present before partner left?
					stat.desc(dd$presence[dd$left_bin==1])/60
          
		  
{# METHODS
	# N observations per nest
		d$n = 1
		d_= ddply(d,.(nest, year type), summarise, n = sum(n))
		ggplot(d_, aes(x = n, col = type)) + geom_density()
		summary(d_$n[d$type == 'ex'])

}
{# RESULTS
	{# Behaviour prior to return of the partner
	 {# Distributions
		{# run first
		dd$call_i=bb_$call_i[match(dd$obs_ID, bb_$obs_ID)]
			dd$fly_i=bb_$fly_i[match(dd$obs_ID, bb_$obs_ID)]
			dd$fly=as.factor(ifelse(dd$fly_i==0,'n','y'))
		}
		{# number of observations
			summary(factor(dd$left_before_presence)) 
			length(dd$left_before_presence[dd$left_before_presence=='y'])/nrow(dd)
			
		}
		{# time left before presence
			densityplot(~dd$presence/60, groups = dd$left_before_presence, auto.key=TRUE)
			
			summary(dd$presence[dd$left_before_presence=='y'])
			
			length(dd$presence[dd$left_before_presence=='y' & dd$presence< -60]/60) # number of cases where parent left more then 1 min before presence of the partner
			summary(dd$presence[dd$left_before_presence=='y' & dd$presence< -60]/60)
			
			length(dd$presence[dd$left_before_presence=='y' & dd$presence< -60*10]/60) # number of cases where parent left more then 5 min before presence of the partner
			
			length(dd$presence[dd$left_before_presence=='y' & dd$presence> -60]/60) # number of cases where parent left less then 1 min before presence of the partner
		}	
			{# not used - later delete		
				   densityplot(~presence/60, groups = dd$left_before_presence, dd[dd$presence>-50 & dd$presence<5,],auto.key=TRUE)
				   densityplot(~presence/60, dd[dd$presence>-50 & dd$presence<5 & dd$left_before_presence=='n',],auto.key=TRUE)
				   densityplot(~presence/60, dd[dd$left_before_presence=='n' & dd$presence<=0,],auto.key=TRUE)
				   densityplot(~presence/60, dd[dd$left_before_presence=='y' & dd$presence>-25,],auto.key=TRUE)
					dd[dd$presence > 30 & dd$left_before_presence =='y',] 
					dd[dd$presence <= 0 & dd$left_before_presence =='n',] 
				  densityplot(~log(abs(presence/60)), dd[dd$left_before_presence=='y',],auto.key=TRUE)
               
           	{# it is irrelevant - given the distribution - is it sex specific? why did they leave? - do not used - 
			{# check distributions
					table(dd$sex,dd$left_before_presence)
					
					densityplot(dd$call_i)
					densityplot(log(dd$call_i+0.01))
  					densityplot(dd$fly_i)
  					densityplot(log(dd$fly_i+0.01))
  					densityplot(dd$current_bout)
  				
  					densityplot(~dd$call_i, groups=dd$left_bin, auto.key=TRUE)			
  					densityplot(~dd$fly_i, groups=dd$left_bin, auto.key=TRUE)			
  					densityplot(~dd$current_bout, groups=dd$left_bin, auto.key=TRUE)	
			}		
			{# model
				m=glmer(left_bin~sex+(1|nest_ID),dd, family='binomial')
						plot(allEffects(m))
						summary(glht(m))
				
				dd_ = dd[!is.na(dd$current_bout),]
				m = glmer(left_bin ~ sex + fly + scale(call_i)+scale(current_bout)+(1|nest_ID), dd_, family='binomial') #### CONTROL FOR REMOVAL exPER???
  					plot(allEffects(m))
  					summary(glht(m))
  					# stats test???
										
  					ggplot(dd,aes(y=current_bout, x=left_before_presence))+geom_boxplot()
  					ggplot(dd,aes(y=call_i, x=left_before_presence))+geom_boxplot()
  					
  					ggplot(dd,aes(x=presence, fill=sex))+geom_histogram(position="dodge" ,  alpha=0.4)
  					dd_<-subset(dd,dd$left_before_presence=="n")
  					ggplot(dd_,aes(x=presence, fill=sex))+geom_histogram(position="dodge" ,  alpha=0.4)+scale_x_log10(breaks = c(1,5,10,20,60,120,1000))
  					ggplot(dd_,aes(x=presence, fill=sex))+geom_histogram(position="dodge" ,  alpha=0.4)+scale_x_log10(breaks = c(1,5,10,20,60,120,1000))
  					ggplot(dd_,aes(x=day_j, y=presence, col=sex))+geom_point()+stat_smooth()+scale_y_log10(breaks = c(1,5,10,20,60,120,1000))
  					
  					
  					table(dd$fly, dd$left_before_presence)
  						
  					ggplot(dd,aes(x=current_bout, y=left_bin))+geom_point()+stat_smooth()
  					ggplot(dd,aes(x=current_bout, y=left_bin))+geom_point()+stat_smooth(method='lm')
  					ggplot(dd,aes(x=call_i, y=left_bin))+geom_point()+stat_smooth()
  						
  																				
  					


			}
				{# model assumptions
						
						dev.new(width=6,height=9)
						par(mfrow=c(5,3))
						
						scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									 
						scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
						
						plot(fitted(m), jitter(dd_$left_bin, amount=0.05), xlab="Fitted values", ylab="Probability of left", las=1, cex.lab=1.2, cex=0.8)
							abline(0,1, lty=3)
							t.breaks <- cut(fitted(m), seq(0,1, by=0.1))
							means <- tapply(dd_$left_bin, t.breaks, mean)
							semean <- function(x) sd(x)/sqrt(length(x))
							means.se <- tapply(dd_$left_bin, t.breaks, semean)
							points(seq(0.05, 0.95, by=0.1), means, pch=16, col="orange")
							segments(seq(0.05, 0.95, by=0.1), means-2*means.se, seq(0.05, 0.95,by=0.1), means+2*means.se,lwd=2, col="orange")
							
						plot(fitted(m), jitter(dd_$left_bin, amount=0.05), xlab="Fitted values", ylab="Probability of presence", las=1, cex.lab=1.2, cex=0.8) # what to do with the miss-fit
							abline(0,1, lty=3)
							t.breaks <- cut(fitted(m), seq(0,1, by=0.2))
							means <- tapply(dd_$left_bin, t.breaks, mean)
							semean <- function(x) sd(x)/sqrt(length(x))
							means.se <- tapply(dd_$left_bin, t.breaks, semean)
							points(seq(0.05, 0.95, by=0.2), means, pch=16, col="orange")
							segments(seq(0.05, 0.95, by=0.2), means-2*means.se, seq(0.05, 0.95,by=0.2), means+2*means.se,lwd=2, col="orange")	
			 
						qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')  # what to do with the miss-fit
							qqline(resid(m))
						boxplot(resid(m)~dd$left_before_presence, xlab = 'left before presence');abline(h=0, lty=2, col='red')
						
						qqnorm(unlist(ranef(m)$nest[1]), main = "nest",col='red')
						qqline(unlist(ranef(m)$nest[1]))
						
						scatter.smooth(resid(m)~dd_$call_i);abline(h=0, lty=2, col='red')
						scatter.smooth(resid(m)~dd_$current_bout);abline(h=0, lty=2, col='red')
						scatter.smooth(resid(m)~dd_$fly);abline(h=0, lty=2, col='red')
						boxplot(resid(m)~dd_$fly);abline(h=0, lty=2, col='red')	
						scatter.smooth(resid(m)~dd_$sex);abline(h=0, lty=2, col='red')
						boxplot(resid(m)~dd_$sex);abline(h=0, lty=2, col='red')			
						
						acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
						
						# spatial autocorrelations - nest location
							spdata=data.frame(resid=resid(m), x=dd_$lon, y=dd_$lat)
								spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
								#cex_=c(1,2,3,3.5,4)
								cex_=c(1,1.5,2,2.5,3)
								spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
								plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
								
								plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8),xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
								plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8), xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
								#mtext("glmer(success_bin~prop_ip+uni_last+(1|sp),data=g,family='binomial')", side = 3, line = -1.2, cex=0.8,outer = TRUE)	
				
				
				}
				{# model assumptions - only sex
						
						dev.new(width=6,height=9)
						par(mfrow=c(5,3))
						
						scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									 
						scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
						
						plot(fitted(m), jitter(dd$left_bin, amount=0.05), xlab="Fitted values", ylab="Probability of left", las=1, cex.lab=1.2, cex=0.8)
							abline(0,1, lty=3)
							t.breaks <- cut(fitted(m), seq(0,1, by=0.1))
							means <- tapply(dd$left_bin, t.breaks, mean)
							semean <- function(x) sd(x)/sqrt(length(x))
							means.se <- tapply(dd$left_bin, t.breaks, semean)
							points(seq(0.05, 0.95, by=0.1), means, pch=16, col="orange")
							segments(seq(0.05, 0.95, by=0.1), means-2*means.se, seq(0.05, 0.95,by=0.1), means+2*means.se,lwd=2, col="orange")
							
						plot(fitted(m), jitter(dd$left_bin, amount=0.05), xlab="Fitted values", ylab="Probability of presence", las=1, cex.lab=1.2, cex=0.8) # what to do with the miss-fit
							abline(0,1, lty=3)
							t.breaks <- cut(fitted(m), seq(0,1, by=0.2))
							means <- tapply(dd$left_bin, t.breaks, mean)
							semean <- function(x) sd(x)/sqrt(length(x))
							means.se <- tapply(dd$left_bin, t.breaks, semean)
							points(seq(0.05, 0.95, by=0.2), means, pch=16, col="orange")
							segments(seq(0.05, 0.95, by=0.2), means-2*means.se, seq(0.05, 0.95,by=0.2), means+2*means.se,lwd=2, col="orange")	
			 
						qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')  # what to do with the miss-fit where presence equals one
							qqline(resid(m))
						qqnorm(unlist(ranef(m)$nest[1]), main = "nest",col='red')
						qqline(unlist(ranef(m)$nest[1]))
						
						boxplot(resid(m)~dd$left_before_presence, xlab = 'left before presence');abline(h=0, lty=2, col='red')
						
						scatter.smooth(resid(m)~dd$sex);abline(h=0, lty=2, col='red')
						boxplot(resid(m)~dd$sex);abline(h=0, lty=2, col='red')			
						
						acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
						
						# spatial autocorrelations - nest location
							spdata=data.frame(resid=resid(m), x=dd$lon, y=dd$lat)
								spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
								#cex_=c(1,2,3,3.5,4)
								cex_=c(1,1.5,2,2.5,3)
								spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
								plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
								
								plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8),xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
								plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8), xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
								#mtext("glmer(success_bin~prop_ip+uni_last+(1|sp),data=g,family='binomial')", side = 3, line = -1.2, cex=0.8,outer = TRUE)	
				
				
				}
			
			{# Supplementary Table 1
			pred=c('Intercept (female)','Male')
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model='1',type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model='1', type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o1=rbind(oii,ri)
				# create xlsx		
						sname = tempfile(fileext='.xls')
						wb = loadWorkbook(sname,create = TRUE)	
						createSheet(wb, name = "output")
						writeWorksheet(wb, o1, sheet = "output")
						#createSheet(wb, name = "output_AIC")
						#writeWorksheet(wb, rbind(o), sheet = "output_AIC")
						saveWorkbook(wb)
						shell(sname)
		
		}
		}	
			{# old plots models
						
						dd$n=1
						x=ddply(dd,. (nest_ID,sex),summarise,left = sum(left_bin), p=sum(left_bin)/ sum(n), n=sum(n))
						ggplot(x, aes(x=sex, y=left, col = sex)) + geom_boxplot()
						densityplot(~x$p)
						
						ratio <- table(dd$sex,dd$left_before_presence) 
    					par(mfrow=c(1,1))
    					bplt<-barplot(ratio,col=c("white","black"),legend=rownames(ratio),space=c(1,3),beside=TRUE, ylab="number of exchanges", xlab="Incubating bird left before partner's presence ")#cex.names=3
    					text(y=ratio+5, x= bplt, labels=as.character(ratio), xpd=T)
						
    					#Do males leave the nest before partner is present more often than females?
    					# chisq is pseudoreplicated, same individuals again!
    					length(dd$sex[dd$sex=="m"]) #86 observations of males
    					length(dd$sex[dd$sex=="f"])#83 observations for females
    					length(dd$sex[dd$sex=="f"&dd$left_before_presence=="n"]) #67 females stayed
    					length(dd$sex[dd$sex=="m"&dd$left_before_presence=="n"]) #76 males stayed
    					length(dd$sex[dd$sex=="f"&dd$left_before_presence=="y"]) #17 females left
    					length(dd$sex[dd$sex=="m"&dd$left_before_presence=="y"]) #11 males left
    					left<-matrix(c(11,76,17,67),2,2);left
    					dimnames(left)<-list(left=c("yes","no"),sex=c("male","female"));left
    					chisq.test(left) #->independent
						
						#violin time of presence, sexes adjusted!Save with width=1500
                  ggplot(dd[dd$presence>-0&dd$presence<500&!is.na(dd$presence),],
                         aes(y=presence,x=sex,fill=sex))+
                    geom_violin(alpha=0.5,color="black",size=1.5)+ #alpha determines how intense the color is, low alpha is durchscheinender
                    geom_boxplot(width=0.2,fill="white",size=1.5)+ #size determines line thickness
                    theme_bw()+
                    theme(legend.position="none",axis.text = element_text(size=30),axis.title=element_text(size=30),panel.grid.major = element_line(size=1.5))+ #axis text is Achsenbeschriftung, also hier male und female,axis.title is Achsenname, also hier sex, size is font size, panel.grid.major is line thickness of grid
                    ylab("Time present [sec]")+
                    scale_x_discrete(breaks=c("f","m"),labels=c("male","female"))+
                    scale_fill_manual(values=c("blue","red"))+
                    scale_colour_manual(values=c("blue","red"))
                  
                  #violin plot of time left before presence, sexes ok, save with width=1500
                  ggplot(dd[dd$presence>-500&dd$presence<0&!is.na(dd$presence),],
                         aes(y=-presence/60,x=sex,fill=sex))+
                    geom_violin(alpha=0.5,color="black",size=1.5)+ #alpha determines how intense the color is, low alpha is durchscheinender
                    geom_boxplot(width=0.2,fill="white",color="black",size=1.5)+
                    theme_bw()+
                    theme(legend.position="none",axis.text = element_text(size=30),axis.title=element_text(size=30),panel.grid.major = element_line(size=1.5))+
                    ylab("Time left before presence [min]")+
                    scale_x_discrete(breaks=c("f","m"),labels=c("female","male"))+
                    scale_fill_manual(values=c("red","blue"))+
                    scale_colour_manual(values=c("red","blue"))
                  
                  #presence by nest_ID, sexes adjusted! more space needed between nests, still produces NAs
                  ggplot(dd[!is.na(dd$presence)&dd$left_before_presence=="y",],
                         aes(y=presence, x=nest_ID, fill=sex))+
                    geom_boxplot(alpha=0.4)+
                    scale_y_log10(breaks = c(1,5,10,20,60,120,1000))+
                    theme_bw()+
                    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                    ylab("Time present in seconds")+
                    scale_colour_manual(values=c("blue","red"),breaks=c("f","m"),labels=c("m","f")) +
                    scale_fill_manual(values=c("blue","red"),breaks=c("f","m"),labels=c("m","f"))
                
				#Do sexes differ in the time they leave before the partner is present? random effects:year,nest,bird_ID, focal bird is incubating: sex ok
                  m1=lmer(rank(presence)~as.factor(sex)+(cage)+(1|year)+(1|nest_ID)+(1|pair_ID),REML=T,data=dd[dd$left_before_presence=="n"])
                  plot(m1)
                  summary(m1)
                  require(multcomp)
                  summary(glht(m1))
                  m1=lmer(rank(presence)~as.factor(sex)+(1|year)+(1|nest_ID)+(1|bird_ID)+(1|cage),REML=F,data=dd[dd$left_before_presence=="n"])
                  m1.null=lmer(rank(presence)~1+(1|year)+(1|nest_ID)+(1|bird_ID)+(1|cage),REML=F,data=dd[dd$left_before_presence=="n"])
       
                  #Do sexes differ in the time they have been present before the exchange? random intercept model, random effects: year, nest, bird ID, focal bird is coming: switch sex!
                  m0=lmer(rank(presence)~as.factor(sex)+(cage)+(1|year)+(1|nest_ID)+(1|pair_ID),  data=dd[dd$left_before_presence=="y"])
                  plot(m0)
                  summary(glht(m0))
                  
                  
                  				
			}			
			}
	}
	 {# Calls and fly-offs prior to exchange and during regular incubation
		    {# run first	
				
      			bb_$dt_video=d$dt_video[match(bb_$obs_ID,d$obs_ID)]
      			bb_$hour= as.numeric(difftime(bb_$dt_video, trunc(bb_$dt_video,"day"), units = "hours"))
      			bb_$rad=as.numeric(bb_$hour)*pi/12
				bb_$n=1
				bb_$fly_bin=ifelse(bb_$fly_i==0,0,1)				
				#length(unique(bb_$nest_ID))
				#length(unique(bb_$bird_ID))
				
			}	
			{# distributions
					summary(factor(bb_$call_i))
					ggplot(bb_,aes(x=call_i, fill = type))+geom_density(alpha=0.5)
					ggplot(bb_,aes(x=call_i, fill = type))+geom_histogram(alpha=0.5, position = 'dodge')
					
					summary(factor(bb_$fly_i))
					summary(factor(bb_$fly_i[bb_$type=='ex']))
					ggplot(bb_,aes(x=fly_i, fill = type))+geom_density(alpha=0.5)
					ggplot(bb_,aes(x=fly_i, fill = type))+geom_histogram(alpha=0.5, position = 'dodge')
					
					ggplot(bb_,aes(x=log(obs_time), col = type))+geom_density(alpha=0.5)
					ggplot(bb_,aes(x=obs_time, col = type))+geom_density(alpha=0.5)
					
					ggplot(bb_,aes(x=log(obs_time), y=call_i, fill = type))+geom_point()
					ggplot(bb_,aes(x=log(obs_time), y=fly_i, fill = type))+geom_point()
				
				}

			{# Figure 1 distribution of calls and fly-offs RATE including predictions FOR PAPER
			   {# run first 
			    {# model predictions
				{# calls
					m= glmer(call_i ~ type + (1|nest_ID), offset = log(obs_time/10),family = poisson,  bb_)# rate per 10 minute
						pred=c('Intercept (exchange)','Type (non-exchange)')
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
				# values to predict for		
					newD=data.frame(type=c('ex','non'))
						
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ type,data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v) 
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pp=newD	
					pt_=pp[pp$type=='ex',]
					pc_=pp[pp$type=='non',]
				}		
			    {# fly-offs
				    bb_$ob_sl = scale(log(bb_$obs_time))
					m= glmer(fly_bin ~ ob_sl+type + (1|nest_ID), family = binomial,  bb_)
						pred=c('Intercept (exchange)','Type (non-exchange)')
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
				# values to predict for		
					newD=data.frame(ob_sl = mean(bb_$ob_sl),
									type=c('ex','non'))
						
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ ob_sl+type,data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-plogis(X%*%v) 
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- plogis(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pp=newD	
					pe=pp[pp$type=='ex',]
					pn=pp[pp$type=='non',]
				}		
				}
				{# raw data
					u=ddply(bb_,.(nest_ID, type), summarise,m=median(10*call_i/obs_time), q1=quantile(10*call_i/obs_time,0.25), q2= quantile(10*call_i/obs_time,0.75), n = sum(n))
					u$type_j=jitter(ifelse(u$type=='ex',2,6)) 
				
					x=ddply(bb_,.(nest_ID, type), summarise,m=median(fly_bin), q1=quantile(fly_bin,0.25), q2= quantile(fly_bin,0.75), n = sum(n))
					x$type_j=jitter(ifelse(x$type=='ex',2,6))
				}
				}
			   {# plot
				if(PNG == TRUE) {
					png(paste(outdir,"Figure_1.png", sep=""), width=1.85+0.6,height=1.5*2,units="in",res=600) 
					}else{
					dev.new(width=1.85+0.6,height=1.5*2)
					}	
				
				par(mfrow=c(2,1),mar=c(0.25,0,0,1.2),oma = c(2.1, 1.8, 0.2, 2.8),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
				{# calls
						plot(u$m~u$type_j, xlim=c(0,8), ylim=c(0,3.2),xaxt='n',  ylab = "Number of calls",xlab = NULL,type='n')
						
						mtext("Calling rate / 10min",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						for(i in 1:length(unique(u$nest_ID))){
									ui=u[u$nest_ID==unique(u$nest_ID)[i],]
									if(nrow(ui)==2){lines(ui$type_j, ui$m, col='grey90')}
									}
						arrows(x0=u$type_j, y0=u$q1,x1=u$type_j, y1=u$q2, code = 0, col=col_p, angle = 90, length = .025, lwd=0.5, lty=1)
						
						symbols(u$type_j, u$m, circles=sqrt(u$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) #
						
						
						
						# add predictions + 95%CI
							lines(c(2,6), c(pt_$pred,pc_$pred), col='red')
							
							arrows(x0=2, y0=pt_$lwr,x1=2, y1=pt_$upr, code = 0, col="red", angle = 90, length = .025, lwd=1.5, lty=1)
							arrows(x0=6, y0=pc_$lwr,x1=6, y1=pc_$upr, code = 0, col="red", angle = 90, length = .025, lwd=1.5, lty=1)
							
							points(y=pt_$pred,x=2, pch=19, cex=0.9,col="red")
							points(y=pc_$pred,x=6, pch=19, cex=0.9,col="red")
						
						# legend
							mtext(expression(italic('N')*' observations:'),side = 4,line=-0.3, padj=-7,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
							symbols(c(8.5,8.5,8.5),c(3.3,2.8,2.3)-0.5,circles=sqrt(c(1,10,20)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
							text(c(8.5,8.5,8.5)+1,c(3.3,2.8,2.3)-0.5,labels=c(1,10,20), xpd=TRUE, cex=0.5,col='grey30') 
							
							text(c(7.1),c(2.05),labels=c('Median & IQR'), xpd=TRUE, cex=0.5,col='grey30', srt=90, xpd=FALSE) 
							
							#arrows(x0=8.5,x1=8.5, y0=1-0.1, y1=1+0.1,  code = 0, col=col_p, angle = 90, length = .025, lwd=1.5, lty=1)
							#symbols(c(8.5),c(1),circles=sqrt(c(1)/pi),inches=0.03,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) 
							#text(c(8.5)+1,c(1),labels=c('Median & IQR'), xpd=TRUE, cex=0.5,col='grey30', adj = 0, pos=4, xpd=FALSE) 
							
							#points(8.5, 1.5, pch=19,cex=0.9, col='red',xpd=NA)
							#arrows(x0=8.5,x1=8.5, y0=1.5-0.3, y1=1.5+0.3,  code = 0, col="red", angle = 90, length = .025, lwd=1.5, lty=1)
							#arrows(x0=7.75,x1=6.3, y0=2.2, y1=1.8,  code = 2, col=col_p, angle = 30, length = .025, lwd=1, lty=1)
							mtext('Predictions &\n95%CrI',side = 4,line=-0.3,padj=2, cex=0.5,las=1,col='red', xpd=TRUE) 
									# use if plotting within RData
									#mtext('Weighted\nmedian',side = 4,line=3, cex=0.5,padj=-3.25,adj=0.5, las=1,col='grey30',xpd=TRUE)
									#points(12.5, 85, pch=19, col='black',xpd=NA)
					}				
				{# fly-offs
						plot(x$m~x$type_j, xlim=c(0,8), ylim=c(0,1),xaxt='n',  ylab = "Number of calls",xlab = NULL,type='n')
						
												
						axis(1, at=c(2,6), label=c('Before exchange', 'Regular'), mgp=c(0,-0.20,0))
						mtext("Incubation",side=1,line=0.4, cex=0.5, las=1, col='grey30')
											
						#axis(2, at=seq(0,100,by=20))
						mtext("Fly-off probability",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						for(i in 1:length(unique(x$nest_ID))){
									ui=x[x$nest_ID==unique(x$nest_ID)[i],]
									if(nrow(ui)==2){lines(ui$type_j, ui$m, col='grey90')}
									}
						
						arrows(x0=x$type_j, y0=x$q1,x1=x$type_j, y1=x$q2, code = 0, col=col_p, angle = 90, length = .025, lwd=0.5, lty=1)
						
						symbols(x$type_j, x$m, circles=sqrt(x$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) #
						
						
						# add predictions + 95%CI
							lines(c(2,6), c(pe$pred,pn$pred), col='red')
							arrows(x0=2, y0=pe$lwr,x1=2, y1=pe$upr, code = 0, col="red", angle = 90, length = .025, lwd=1.5, lty=1)
							arrows(x0=6, y0=pn$lwr,x1=6, y1=pn$upr, code = 0, col="red", angle = 90, length = .025, lwd=1.5, lty=1)
							
							points(y=pe$pred,x=2, pch=19, cex=0.9,col="red")
							points(y=pn$pred,x=6, pch=19, cex=0.9,col="red")
				}			
				if(PNG == TRUE) {dev.off()}
				}			
			}
				{# not used PLOT distribution of calls and fly-offs including predictions FOR PAPER
			   {# run first 
				{# model predictions
				{# calls
					m= glmer(call_i ~ type + (1|nest_ID), offset = log(obs_time/10),family = poisson,  bb_)
						pred=c('Intercept (exchange)','Type (non-exchange)')
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
				# values to predict for		
					newD=data.frame(type=c('ex','non'))
						
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ type,data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v) 
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pp=newD	
					pt_=pp[pp$type=='ex',]
					pc_=pp[pp$type=='non',]
				}		
			    {# fly-offs
					m= glmer(fly_bin ~ scale(log(obs_time))+type + (1|nest_ID), family = binomial,  bb_)
						pred=c('Intercept (exchange)','Type (non-exchange)')
						nsim <- 2000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
				# values to predict for		
					newD=data.frame(type=c('ex','non'))
						
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ type,data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v) 
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pp=newD	
					pe=pp[pp$type=='ex',]
					pn=pp[pp$type=='non',]
				}		
				}			
				{# raw data
					u=ddply(bb_,.(nest_ID, type), summarise,m=median(call_i), q1=quantile(call_i,0.25), q2= quantile(call_i,0.75), n = sum(n))
					u$type_j=jitter(ifelse(u$type=='ex',2,6)) 
				
					x=ddply(bb_,.(nest_ID, type), summarise,m=median(fly_i), q1=quantile(fly_i,0.25), q2= quantile(fly_i,0.75), n = sum(n))
					x$type_j=jitter(ifelse(x$type=='ex',2,6))
				
				}
				}
				if(PNG == TRUE) {
					png(paste(outdir,"distribution_call_fly_before.png", sep=""), width=1.85+0.6,height=1.5*2,units="in",res=600) 
					}else{
					dev.new(width=1.85+0.6,height=1.5*2)
					}	
				
				par(mfrow=c(2,1),mar=c(0.25,0,0,1.2),oma = c(2.1, 1.8, 0.2, 2.8),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
						
					# calls
						plot(u$m~u$type_j, xlim=c(0,8), ylim=c(0,7),xaxt='n',  ylab = "Number of calls",xlab = NULL,type='n')
						
						mtext("Number of calls",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						for(i in 1:length(unique(u$nest_ID))){
									ui=u[u$nest_ID==unique(u$nest_ID)[i],]
									if(nrow(ui)==2){lines(ui$type_j, ui$m, col='grey90')}
									}
						arrows(x0=u$type_j, y0=u$q1,x1=u$type_j, y1=u$q2, code = 0, col=col_p, angle = 90, length = .025, lwd=0.5, lty=1)
						
						symbols(u$type_j, u$m, circles=sqrt(u$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) #
						
						
						
						# add predictions + 95%CI
							lines(c(2,6), c(pt_$pred,pc_$pred), col='red')
							
							arrows(x0=2, y0=pt_$lwr,x1=2, y1=pt_$upr, code = 0, col="red", angle = 90, length = .025, lwd=1.5, lty=1)
							arrows(x0=6, y0=pc_$lwr,x1=6, y1=pc_$upr, code = 0, col="red", angle = 90, length = .025, lwd=1.5, lty=1)
													
							points(y=pt_$pred,x=2, pch=19, cex=0.9,col="red")
							points(y=pc_$pred,x=6, pch=19, cex=0.9,col="red")
							
						# legend
							mtext(expression(italic('N')*' observations:'),side = 4,line=-0.3, padj=-7,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
							symbols(c(8.5,8.5,8.5),c(6.5,5.5,4.5)-0.5,circles=sqrt(c(1,10,20)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
							text(c(8.5,8.5,8.5)+1,c(6.5,5.5,4.5)-0.5,labels=c(1,10,20), xpd=TRUE, cex=0.5,col='grey30') 
							
							text(c(7.1),c(4),labels=c('Median & IQR'), xpd=TRUE, cex=0.5,col='grey30', srt=90, xpd=FALSE) 
							mtext('Predictions &\n95%CrI',side = 4,line=-0.3,padj=2, cex=0.5,las=1,col='red', xpd=TRUE)
							
							#points(8.5, 1.5, pch=19,cex=0.9, col='red',xpd=NA)
							#arrows(x0=8.5,x1=8.5, y0=1.5-0.3, y1=1.5+0.3,  code = 0, col="red", angle = 90, length = .025, lwd=1.5, lty=1)
							#arrows(x0=7.75,x1=6.3, y0=2.2, y1=1.8,  code = 2, col=col_p, angle = 30, length = .025, lwd=1, lty=1)
							#mtext('Prediction &\n95%CrI',side = 4,line=-0.3,padj=2, cex=0.5,las=1,col='grey30', xpd=TRUE) 
									# use if plotting within RData
									#mtext('Weighted\nmedian',side = 4,line=3, cex=0.5,padj=-3.25,adj=0.5, las=1,col='grey30',xpd=TRUE)
									#points(12.5, 85, pch=19, col='black',xpd=NA)
								
					# fly-offs
						plot(x$m~x$type_j, xlim=c(0,8), ylim=c(0,4),xaxt='n',  ylab = "Number of calls",xlab = NULL,type='n')
						
												
						axis(1, at=c(2,6), label=c('Before exchange', 'Regular'), mgp=c(0,-0.20,0))
						mtext("Incubation",side=1,line=0.4, cex=0.5, las=1, col='grey30')
											
						axis(2, at=seq(0,100,by=20))
						mtext("Number of fly-offs",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						for(i in 1:length(unique(x$nest_ID))){
									ui=x[x$nest_ID==unique(x$nest_ID)[i],]
									if(nrow(ui)==2){lines(ui$type_j, ui$m, col='grey90')}
									}
						
						arrows(x0=x$type_j, y0=x$q1,x1=x$type_j, y1=x$q2, code = 0, col=col_p, angle = 90, length = .025, lwd=0.5, lty=1)
						
						symbols(x$type_j, x$m, circles=sqrt(x$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) #
						
						
						# add predictions + 95%CI
							lines(c(2,6), c(pe$pred,pn$pred), col='red')
							
							arrows(x0=2, y0=pe$lwr,x1=2, y1=pe$upr, code = 0, col="red", angle = 90, length = .025, lwd=1.5, lty=1)
							arrows(x0=6, y0=pn$lwr,x1=6, y1=pn$upr, code = 0, col="red", angle = 90, length = .025, lwd=1.5, lty=1)
							
							points(y=pe$pred,x=2, pch=19, cex=0.9,col="red")
							points(y=pn$pred,x=6, pch=19, cex=0.9,col="red")
							
			if(PNG == TRUE) {dev.off()}
				}			
						
			{# Supplementery Table 1 - if you keep only one random slope - do not report it - just state it in the Table notes
			  {# prepare table data
				{# calling - type
				m= glmer(call_i ~ type + (1|nest_ID), offset = log(obs_time/10),family = poisson,  bb_)
					pred=c('Intercept (ex)','Type(non)')
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model='1',dependent = 'calling rate / 10min', type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model='1',dependent = 'calling rate / 10min', type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o1=rbind(oii,ri)
				}
				{# calling type * incubation period, type * sex
					m= glmer(call_i ~ type*sex +  type*scale(day_j) + (1|nest_ID),offset = log(obs_time/10), family = poisson,  bb_) #model with random slope for day did not converge, but model controlled for date put without offset generated similar results
					#m= glmer(call_i ~ type*sex +  type*scale(day_j) + (day_j|nest_ID), family = poisson,  bb_)	
						pred=c('Intercept (ex & f)','Type(non)','Sex (m)', 'Day of incubation', 'Sex x type', 'Day x type')
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model='2',dependent = 'calling rate / 10min', type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model='2',dependent = 'calling rate / 10min', type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o2=rbind(oii,ri)
				}	
				{# fly-off - type
					m= glmer(fly_bin ~ scale(log(obs_time))+type + (1|nest_ID), family = binomial,  bb_)
						pred=c('Intercept (ex)','obs_timep','Type(non)')
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model='3',dependent = 'fly-off rate / 10min', type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model='2',dependent = 'fly-off rate / 10min', type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o3=rbind(oii,ri)
				}
				{# fly-off type * incubation period	
					m= glmer(fly_bin ~ scale(log(obs_time)) + type*sex +  type*scale(day_j) +  (1|nest_ID),family = binomial,  bb_)
						pred=c('Intercept (ex & f)','obs_time','Type(non)','Sex (m)', 'Day of incubation', 'Sex x type', 'Day x type')
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model='4',dependent = 'fly-off rate / 10min', type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model='4',dependent = 'fly-off rate / 10min', type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o4=rbind(oii,ri)
				}	
			  }
			  {# create xlsx table		
						o=rbind(o1,o2,o3,o4)
						sname = tempfile(fileext='.xls')
						wb = loadWorkbook(sname,create = TRUE)	
						createSheet(wb, name = "output")
						writeWorksheet(wb, o, sheet = "output")
						#createSheet(wb, name = "output_AIC")
						#writeWorksheet(wb, rbind(o), sheet = "output_AIC")
						saveWorkbook(wb)
						shell(sname)
				}
			}
				{# model assumptions
					{# calling - type
						m= glmer(call_i ~ type + (1|nest_ID), offset = log(obs_time/10),family = poisson,  bb_)
									#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 								  
									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
									  qqline(resid(m))
									  
									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = " intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))
									  
									  #qqnorm(unlist(ranef(m)$nest_ID[2]), main = " slope",col='red')
									  #qqline(unlist(ranef(m)$nest_ID[2]))
									  
									  #scatter.smooth(resid(m)~x2$date[x2$sum>720]);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
									  
									   mtext("glmer(call_i ~ type + (1|nest_ID), offset = log(obs_time/10),family = poisson,  bb_)", side = 3, line = 0.5, cex=0.8,outer = TRUE)
									   
									   mtext(paste("overdispersion:", round(dispersion_glmer(m),3)), side = 3, line = -2, cex=0.8,outer = TRUE)
																	  
									  acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=bb_$lon, y=bb_$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
											
											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								
							dev.off()
					}
					{# calling type * incubation period, type * sex
						m= glmer(call_i ~ type*sex +  type*scale(day_j) + (scale(day_j)|nest_ID),offset = log(obs_time/10), family = poisson,  bb_) 
						#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 								  
									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
									  qqline(resid(m))
									  
									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = " intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))
									  
									  #qqnorm(unlist(ranef(m)$nest_ID[2]), main = " slope",col='red')
									  #qqline(unlist(ranef(m)$nest_ID[2]))
									  
									  scatter.smooth(resid(m)~bb_$day_j);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
									  
									  scatter.smooth(resid(m)~bb_$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~bb_$sex);abline(h=0, lty=2, col='red')
									  
									   mtext("glmer(call_i ~ type*sex +  type*scale(day_j) + (1|nest_ID),offset = log(obs_time/10), family = poisson) ", side = 3, line = 0.5, cex=0.8,outer = TRUE)
									   
									   mtext(paste("overdispersion:", round(dispersion_glmer(m),3)), side = 3, line = -2, cex=0.8,outer = TRUE)
																	  
									  acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=bb_$lon, y=bb_$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
											
											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								
							dev.off()
					}
					{# fly-off - type
						m= glmer(fly_bin ~ scale(log(obs_time))+type + (1|nest_ID), family = binomial,  bb_)
						  #dev.new(width=6,height=9)
						  png(paste(outdir,"model_ass/Supplementary_Table_2c.png", sep=""), width=6,height=9,units="in",res=600)
						  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 								  
							scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
							scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  
							plot(fitted(m), jitter(bb_$fly_bin, amount=0.05), xlab="Fitted values", ylab="Probability of left", las=1, cex.lab=1.2, cex=0.8)
							abline(0,1, lty=3)
							t.breaks <- cut(fitted(m), quantile(fitted(m)))
							means <- tapply(bb_$fly_bin, t.breaks, mean)
							semean <- function(x) sd(x)/sqrt(length(x))
							means.se <- tapply(bb_$fly_bin, t.breaks, semean)
							points(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means, pch=16, col="orange")
							segments(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means-2*means.se, quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means+2*means.se,lwd=2, col="orange")
							
							qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
							qqline(resid(m))
									  
							qqnorm(unlist(ranef(m)$nest_ID [1]), main = " intercept",col='red')
							qqline(unlist(ranef(m)$nest_ID [1]))
									  
									  #qqnorm(unlist(ranef(m)$nest_ID[2]), main = " slope",col='red')
									  #qqline(unlist(ranef(m)$nest_ID[2]))
									  
							scatter.smooth(resid(m)~log(bb_$obs_time));abline(h=0, lty=2, col='red')
							scatter.smooth(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
							boxplot(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
									  
							mtext("glmer(fly_bin ~ scale(log(obs_time))+type + (1|nest_ID), family = binomial", side = 3, line = 0.5, cex=0.8,outer = TRUE)
									   
							#mtext(paste("overdispersion:", round(dispersion_glmer(m),3)), side = 3, line = -2, cex=0.8,outer = TRUE)
																	  
							acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=bb_$lon, y=bb_$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
											
											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								
							dev.off()
					}
					{# fly-off type * incubation period	
						m= glmer(fly_bin ~ scale(log(obs_time)) + type*sex +  type*scale(day_j) +  (1|nest_ID),family = binomial,  bb_)
						
						#dev.new(width=6,height=9)
						  png(paste(outdir,"model_ass/Supplementary_Table_2d.png", sep=""), width=6,height=9,units="in",res=600)
						  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 								  
									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  
									  plot(fitted(m), jitter(bb_$fly_bin, amount=0.05), xlab="Fitted values", ylab="Probability of left", las=1, cex.lab=1.2, cex=0.8)
							abline(0,1, lty=3)
							t.breaks <- cut(fitted(m), quantile(fitted(m)))
							means <- tapply(bb_$fly_bin, t.breaks, mean)
							semean <- function(x) sd(x)/sqrt(length(x))
							means.se <- tapply(bb_$fly_bin, t.breaks, semean)
							points(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means, pch=16, col="orange")
							segments(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means-2*means.se, quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means+2*means.se,lwd=2, col="orange")
							
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
									  qqline(resid(m))
									  
									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = " intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))
									  
									  #qqnorm(unlist(ranef(m)$nest_ID[2]), main = " slope",col='red')
									  #qqline(unlist(ranef(m)$nest_ID[2]))
									  
									  scatter.smooth(resid(m)~log(bb_$obs_time));abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~bb_$day_j);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
									  
									  scatter.smooth(resid(m)~bb_$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~bb_$sex);abline(h=0, lty=2, col='red')
									  
									   mtext("glmer(fly_bin ~ scale(log(obs_time)) + type*sex +  type*scale(day_j) +  (1|nest_ID),family = binomial) ", side = 3, line = 0.5, cex=0.8,outer = TRUE)
									   
									   #mtext(paste("overdispersion:", round(dispersion_glmer(m),3)), side = 3, line = -2, cex=0.8,outer = TRUE)
																	  
									  acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=bb_$lon, y=bb_$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
											
											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								
							dev.off()
					}
				}
				
				{# NOT USED separate plot FOR PAPER
				#png(paste(outdir,"distribution_call_i_before.png", sep=""), width=1.85+0.6,height=1.85*2,units="in",res=600)
				dev.new(width=1.85+0.6,height=1.85)
						
				
				par(,mar=c(0.0,0,0,1.2),oma = c(2.1, 1.8, 0.2, 2.8),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
						plot(v$m~v$type_j, xlim=c(0,8), ylim=c(0,4),xaxt='n',  ylab = "Number of calls",xlab = NULL,type='n')
						
												
						axis(1, at=c(2,6), label=c('Before exchange', 'Regular'), mgp=c(0,-0.20,0))
						mtext("Incubation",side=1,line=1, cex=0.5, las=1, col='grey30')
											
						#axis(2, at=seq(0,100,by=20))
						mtext("Number of fly-offs",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						for(i in 1:length(unique(v$nest_ID))){
									ui=v[v$nest_ID==unique(v$nest_ID)[i],]
									if(nrow(ui)==2){lines(ui$type_j, ui$m, col='grey90')}
									}
						
						symbols(v$type_j, v$m, circles=sqrt(v$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) #
						
						
						# add medians
						
						points(2, weightedMedian(v$m[v$type=='ex'], v$n[v$type=='ex']), pch=19, col='red')
						points(6, weightedMedian(v$m[v$type=='non'], v$n[v$type=='non']), pch=19, col='red')
												
						#lines(c(1.5,2.5), c(median(u1$s), median(u1$s)), lwd=2, col='black')
						#lines(c(5.5,6.5), c(median(u2$s), median(u2$s)), lwd=2, col='black')
						{# legend
									mtext(expression(italic('N')*' nests:'),side = 4,line=-0.3, padj=-9,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
									symbols(c(8.5,8.5,8.5),c(6.5,5.5,4.5),circles=sqrt(c(1,10,20)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
									text(c(8.5,8.5,8.5)+1,c(6.5,5.5,4.5),labels=c(1,10,20), xpd=TRUE, cex=0.5,col='grey30') 
																		
									mtext('Weighted\nmedian',side = 4,line=-0.3,padj=1, cex=0.5,las=1,col='grey30', xpd=TRUE) 
															
									points(8.5, 2, pch=19, col='red',xpd=NA)
									# use if plotting within RData
									#mtext('Weighted\nmedian',side = 4,line=3, cex=0.5,padj=-3.25,adj=0.5, las=1,col='grey30',xpd=TRUE)
									#points(12.5, 85, pch=19, col='black',xpd=NA)
						}
								
						dev.off()
					
				{# plot FOR PAPER
						#png(paste(outdir,"distribution_call_i_before.png", sep=""), width=1.85+0.6,height=1.85,units="in",res=600)
						dev.new(width=1.85+0.6,height=1.85)
						
						par(mar=c(0.0,0,0,1.2),oma = c(2.1, 1.8, 0.2, 2.8),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
						plot(u$m~u$type_j, xlim=c(0,8), ylim=c(0,7),xaxt='n',  ylab = "Number of calls",xlab = NULL,type='n')
						
												
						axis(1, at=c(2,6), label=c('Before exchange', 'Regular'), mgp=c(0,-0.20,0))
						mtext("Incubation",side=1,line=1, cex=0.5, las=1, col='grey30')
											
						#axis(2, at=seq(0,100,by=20))
						mtext("Number of calls",side=2,line=1, cex=0.6, las=3, col='grey30')
						
						for(i in 1:length(unique(u$nest_ID))){
									ui=u[u$nest_ID==unique(u$nest_ID)[i],]
									if(nrow(ui)==2){lines(ui$type_j, ui$m, col='grey90')}
									}
						
						symbols(u$type_j, u$m, circles=sqrt(u$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) #
						
						
						# add medians
						
						points(2, weightedMedian(u$m[u$type=='ex'], u$n[u$type=='ex']), pch=19, col='red')
						points(6, weightedMedian(u$m[u$type=='non'], u$n[u$type=='non']), pch=19, col='red')
												
						#lines(c(1.5,2.5), c(median(u1$s), median(u1$s)), lwd=2, col='black')
						#lines(c(5.5,6.5), c(median(u2$s), median(u2$s)), lwd=2, col='black')
						{# legend
									mtext(expression(italic('N')*' nests:'),side = 4,line=-0.3, padj=-9,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
									symbols(c(8.5,8.5,8.5),c(6.5,5.5,4.5),circles=sqrt(c(1,10,20)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
									text(c(8.5,8.5,8.5)+1,c(6.5,5.5,4.5),labels=c(1,10,20), xpd=TRUE, cex=0.5,col='grey30') 
																		
									mtext('Weighted\nmedian',side = 4,line=-0.3,padj=1, cex=0.5,las=1,col='grey30', xpd=TRUE) 
															
									points(8.5, 2, pch=19, col='red',xpd=NA)
									# use if plotting within RData
									#mtext('Weighted\nmedian',side = 4,line=3, cex=0.5,padj=-3.25,adj=0.5, las=1,col='grey30',xpd=TRUE)
									#points(12.5, 85, pch=19, col='black',xpd=NA)
						}
								
						dev.off()
					
			}
						
			}
				{# LATER DELETE no overall difference between calls  
				{# Supplementary table
					m= glmer(call_i ~ type + (1|nest_ID), offset = log(obs_time),family = poisson,  bb_)
					m= glmer(call_i ~ type*sex + (1|nest_ID), offset = log(obs_time),family = poisson,  bb_)
					#fm= glmer(call_i ~ type + (1|nest_ID), family = poisson,  bb_)
				   			
      					summary(m)
      					summary(glht(m))
      					plot(allEffects(m))
						dispersion_glmer(m) # values over 1.4 suggest serious overdisperison
						
					# this does not change over the incubation period
							m= glmer(call_i ~ type*sex +  type*scale(day_j) + (day_j|nest_ID),offset = log(obs_time), family = poisson,  bb_)
												
						{# I think this does not belong here, but WE CAN KEEP THIS IN CASE WE HAVE A PREDICTION HERE 
							# the finding says that regularly incubating birds tend to call less during mid-day and more during night whereas before exchange this is more regular
							fm= glmer(call_i ~ type*sin(rad)+type*cos(rad)+ (1|nest_ID), family = poisson,  bb_) 
							 summary(glht(fm))
							 plot(allEffects(fm))
							
						}	
				}
					{# old plots
				ggplot(bb_,aes(x=type, y=call_i))+geom_boxplot()
				
				#ggplot(bb_,aes(x=reorder(nest_ID, call_i, FUN=median), y=call_i, fill=type))+geom_boxplot()

				ggplot(bb_,aes(x=type, y=call_i, col=sex))+geom_boxplot()
  				ggplot(bb_,aes(x=call_i, col=type))+geom_density()
				ggplot(bb_,aes(x=call_i, fill=type))+geom_histogram(position="dodge" ,  alpha=0.4)
				
				
				}
			}
				{# LATER DELETE less fly-offs before exchange
				{# Supplementary table
					# Poisson
						fm= glmer(fly_i ~ type + (1|bird_ID) + (1|nest_ID),offset = log(obs_time), family = poisson,  bb_)
      					summary(fm)
						summary(glht(fm))
      					plot(allEffects(fm))
						dispersion_glmer(fm) 
						
						# overall fly-offs increase over the incubation period
						fm= glmer(fly_i ~ type*scale(day_j) + (day_j|nest_ID),offset = log(obs_time), family = poisson,  bb_)
						fm= glmer(fly_i ~ type+scale(day_j) + (day_j|nest_ID),offset = log(obs_time), family = poisson,  bb_)
					
					
									
		
				}
				{# binary (gives same results as Poisson
						bb_$fly_bin=ifelse(bb_$fly_i==0,0,1)
						fm= glmer(fly_bin ~ type + (1|nest_ID), family = binomial,  bb_)
							summary(fm)
							summary(glht(fm))
							plot(allEffects(fm))
				}			
				{# old plots
				ggplot(bb_,aes(x=type, y=fly_i))+geom_boxplot()
				#ggplot(bb_,aes(x=reorder(nest_ID, fly_i, FUN=median), y=fly_i, fill=type))+geom_boxplot()
  				ggplot(bb_,aes(x=type, y=fly_i, col=sex))+geom_boxplot()
				ggplot(bb_,aes(x=fly_i, col=type))+geom_density()
				ggplot(bb_,aes(x=fly_i, fill=type))+geom_histogram(position="dodge" ,  alpha=0.4)
				}
      		}	
						
	 }
	 {# For cases where calling/flying occured - was it closer to the exchange start? and if so is this sex specific
				{# run first	  
					# time difference of each call/fly occurance to the end of observations session (only for those observatins sessions where calling occured)
						ba = b[, deltaT := difftime(end_pr, dt_behaviour, units = 'mins')%>% as.integer]
						bo = b_[, deltaT := difftime(end_pr, dt_behaviour, units = 'mins')%>% as.integer] # contains only cases where incubating bird left after its partner was present
          			
          			bo$hour= as.numeric(difftime(bo$dt_behaviour, trunc(bo$dt_behaviour,"day"), units = "hours"))
          			bo$rad=as.numeric(bo$hour)*pi/12
					bo$sex = as.factor(bo$sex)
          			#h$sin_=sin(h$rad)
          			#h$cos_=cos(h$rad)
					bo_ = bo[bo$behaviour == 'c' & bo$who == 'o' & bo$type == 'ex',]
					length(unique(bo_$obs_ID))
					bf = bo[bo$behaviour == 'f' & bo$who == 'o' & bo$type == 'ex',]
					length(unique(bf$obs_ID))
					
					{# DELETE if u and w not used in distributions
						bo_$n = 1
						u=ddply(bo_,.(nest_ID,obs_ID, type), summarise,m=median(deltaT/obs_time), q1=quantile(deltaT/obs_time,0.25), q2= quantile(deltaT/obs_time,0.75), n = sum(n))
						u$type_j=2
						u$type_j=jitter(u$type_j)
						
						bf$n = 1
						w=ddply(bf,.(nest_ID,obs_ID, type), summarise,m=median(deltaT/obs_time), q1=quantile(deltaT/obs_time,0.25), q2= quantile(deltaT/obs_time,0.75), n = sum(n))
						w$type_j=2
						w$type_j=jitter(w$type_j)
					
					
					}
          		}	
				{# distributions
					# calling	
						ggplot(bo_, aes(y = deltaT, x = obs_time))+geom_point()
						ggplot(bo_, aes(x = deltaT))+geom_density()
						ggplot(bo_, aes(x = deltaT/obs_time))+geom_density()
						ggplot(bo_, aes(x = deltaT/obs_time, fill = bird_ID))+geom_density(alpha=0.3)+ theme(legend.position="none")
						ggplot(bo_, aes(x = deltaT/obs_time, col = bird_ID))+geom_density() + theme(legend.position="none")
						
						ggplot(bo_, aes(x = deltaT/obs_time))+geom_histogram()
						ggplot(u, aes(x = m))+geom_histogram()
						ggplot(u, aes(x = m))+geom_density()+ theme(legend.position="none")
						ggplot(u, aes(x = m, fill = nest_ID))+geom_density(alpha=0.3)+ theme(legend.position="none")
						
						
						ggplot(bo_, aes(y = deltaT/obs_time, x = who))+geom_boxplot()
						ggplot(bo_, aes(y = deltaT/obs_time, x = nest_ID))+geom_boxplot()
					
					# fly-offs
						ggplot(bf, aes(y = deltaT, x = obs_time))+geom_point()
						ggplot(bf, aes(x = deltaT/obs_time))+geom_density()
						ggplot(bf, aes(x = deltaT/obs_time,fill = bird_ID))+geom_density(alpha=0.3) +  theme(legend.position="none")
						
						ggplot(w, aes(x = m))+geom_histogram()
						ggplot(w, aes(x = m))+geom_density()+ theme(legend.position="none")
						ggplot(w, aes(x = m, fill = nest_ID))+geom_density(alpha=0.3)+ theme(legend.position="none")
						
						
						ggplot(bf, aes(x = deltaT/obs_time, col = sex))+geom_density()
						
						ggplot(bf, aes(y = deltaT/obs_time, x = who))+geom_boxplot()
						ggplot(bf, aes(y = deltaT/obs_time, x = nest_ID))+geom_boxplot()
				}				
				{# Figure 2ab
				   {# run first 
					{# model estimates
						# calling
						 m = lmer(deltaT/obs_time ~ 1+(1|nest_ID) , bo_)
							nsim <- 5000
							bsim <- sim(m, n.sim=nsim)  
						v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
						ci = apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
						# fly-offs
						 m = lmer(deltaT/obs_time ~ 1+(1|nest_ID) , bf)
							nsim <- 5000
							bsim <- sim(m, n.sim=nsim)  
						vf = apply(bsim@fixef, 2, quantile, prob=c(0.5))
						cif = apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))
					}		
					{# raw data
						bo_$n = 1
						u=ddply(bo_,.(nest_ID,obs_ID, type), summarise,m=median(deltaT/obs_time), q1=quantile(deltaT/obs_time,0.25), q2= quantile(deltaT/obs_time,0.75), n = sum(n))
						u$type_j=2
						u$type_j=jitter(u$type_j)
						
						bf$n = 1
						w=ddply(bf,.(nest_ID,obs_ID, type), summarise,m=median(deltaT/obs_time), q1=quantile(deltaT/obs_time,0.25), q2= quantile(deltaT/obs_time,0.75), n = sum(n))
						w$type_j=2
						w$type_j=jitter(w$type_j)
							# dummy variable to keep bubbles in the fly off plot of same size as in calling plot
							w2=w[1,]
							w2$nest_ID=w2$obs_ID='xxx'
							w2$m = w2$q1 = w2$q2 = 2
							w2$n = 10
							w=rbind(w,w2)
						}
				   }
				   {# plot
				 if(PNG == TRUE) {
					png(paste(outdir,"Figure_2ab.png", sep=""), width=1.85+0.6,height=1.5*2,units="in",res=600) 
					}else{
					dev.new(width=1.85+0.6,height=1.5*2)
					}	
				
				 par(mfrow=c(2,1),mar=c(0.25,0,0,3.5),oma = c(2.1, 2.3, 0.2, 0),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
				 {# calls
					plot(u$m~u$type_j, xlim=c(2-0.044,2+0.044), ylim=c(0,1),xaxt='n',  ylab = "Number of calls",xlab = NULL,type='n')
						
						lines(y=c(1,1),x=c(2-0.048,2+0.044), lty = 3,col="grey70")
						text(y=c(1),x=c(2+0.036), labels = 'Start of observation', col="grey70",xpd=TRUE, cex=0.5,pos=4)
												
						lines(y=c(0,0),x=c(2-0.048,2+0.044), lty = 3,col="grey70")
						text(y=c(0),x=c(2+0.036), labels = 'Start of exchange', col="grey70",xpd=TRUE, cex=0.5,pos=4)
						
						mtext("Calling time\n[proportion of observed time]",side=2,line=1, cex=0.6, las=3, col='grey30')
						#axis(1, at=c(2,6), label=c('Before exchange', 'Regular'), mgp=c(0,-0.20,0))
											
						#axis(1, at=c(2,6), label=c('Before exchange', 'Regular'), mgp=c(0,-0.20,0))
						#mtext("Observation [median",side=1,line=0.4, cex=0.5, las=1, col='grey30')
						
						arrows(x0=u$type_j, y0=u$q1,x1=u$type_j, y1=u$q2, code = 0, col=col_p, angle = 90, length = .025, lwd=0.5, lty=1)
						
						symbols(u$type_j, u$m, circles=sqrt(u$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) #
						
						lines(y=c(0.5,0.5),x=c(2-0.044,2+0.044), lty = 3)						
						
						# add predictions + 95%CI
							arrows(x0=2, y0=ci[1],x1=2, y1=ci[2], code = 0, col="red", angle = 90, length = .025, lwd=1.5, lty=1)
							points(y=v,x=2, pch=19, cex=0.9,col="red")
													
						# legend
							mtext(expression(italic('N')*' cases:'),side = 4,line=0, padj=-7,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
							symbols(c(2+0.044,2+0.044,2+0.0445)+0.01,c(0.93,0.8,0.66)-0.11,circles=sqrt(c(1,5,10)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
							text(c(2+0.044,2+0.044,2+0.0445)+0.02,c(0.94,0.8,0.66)-0.11,labels=c(1,5,10), xpd=TRUE, cex=0.5,col='grey30') 
							
							mtext('Median & IQR',side = 4,line=0,padj=2.6, cex=0.5,las=1,col='grey30', xpd=TRUE) 
							mtext('Estimate &\n95%CrI',side = 4,line=0,padj=2.6, cex=0.5,las=1,col='red', xpd=TRUE)
							
				} 					
				 {# fly-offs
					plot(w$m~w$type_j, xlim=c(2-0.044,2+0.044), ylim=c(0,1),xaxt='n',  ylab = "Number of calls",xlab = NULL,type='n')
						
						#lines(y=c(1,1),x=c(2-0.048,2+0.044), lty = 3,col="grey70")
						#text(y=c(1),x=c(2+0.036), labels = 'Start of observation', col="grey30",xpd=TRUE, cex=0.5,pos=4)
												
						#lines(y=c(0,0),x=c(2-0.048,2+0.044), lty = 3,col="grey70")
						#text(y=c(0),x=c(2+0.036), labels = 'Start of exchange', col="grey30",xpd=TRUE, cex=0.5,pos=4)
						
						mtext("Fly-off time\n[proportion of observed time]",side=2,line=1, cex=0.6, las=3, col='grey30')
						#axis(1, at=c(2,6), label=c('Before exchange', 'Regular'), mgp=c(0,-0.20,0))
											
						#axis(1, at=c(2,6), label=c('Before exchange', 'Regular'), mgp=c(0,-0.20,0))
						#mtext("Observation [median",side=1,line=0.4, cex=0.5, las=1, col='grey30')
						
						arrows(x0=w$type_j, y0=w$q1,x1=w$type_j, y1=w$q2, code = 0, col=col_p, angle = 90, length = .025, lwd=0.5, lty=1)
						
						symbols(w$type_j, w$m, circles=sqrt(w$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) #
						
						lines(y=c(0.5,0.5),x=c(2-0.044,2+0.044), lty = 3)						
						
						# add predictions + 95%CI
							arrows(x0=2, y0=cif[1],x1=2, y1=cif[2], code = 0, col="red", angle = 90, length = .025, lwd=1.5, lty=1)
							points(y=vf,x=2, pch=19, cex=0.9,col="red")
													
				} 					
				 if(PNG == TRUE) {dev.off()}
				}			
				}
				{# Supplementary Table 2
				  {# prepare table data	
					{# calling simple
					m = lmer(deltaT/obs_time ~ 1+(1|nest_ID) , bo_)
						pred=c('Intercept')
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
					# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model='1',dependent = 'proportion of observation period calling occured', type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model='1',dependent = 'proportion of observation period calling occured', type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o1=rbind(oii,ri)
				}
					{# calling sex
					m = lmer(deltaT/obs_time ~ sex+(1|nest_ID) , bo_)
						pred=c('Intercept (f)', 'sex(m)')
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
					# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model='2',dependent = 'proportion of observation period calling occured', type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model='2',dependent = 'proportion of observation period calling occured', type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o2=rbind(oii,ri)
				}
					{# fly-off simple
					m = lmer(deltaT/obs_time ~ 1+(1|nest_ID) , bf)
						pred=c('Intercept')
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
					# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model='3',dependent = 'proportion of observation period fly-off occured', type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model='3',dependent = 'proportion of observation period fly-off occured', type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o3=rbind(oii,ri)
				}
					{# calling sex
					m = lmer(deltaT/obs_time ~ sex+(1|nest_ID) , bf)
						pred=c('Intercept (f)', 'sex(m)')
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
					# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model='4',dependent = 'proportion of observation period fly-off occured', type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model='4',dependent = 'proportion of observation period fly-off occured', type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o4=rbind(oii,ri)
				}
			      }
				  {# create xlsx table		
						o=rbind(o1,o2,o3,o4)
						sname = tempfile(fileext='.xls')
						wb = loadWorkbook(sname,create = TRUE)	
						createSheet(wb, name = "output")
						writeWorksheet(wb, o, sheet = "output")
						#createSheet(wb, name = "output_AIC")
						#writeWorksheet(wb, rbind(o), sheet = "output_AIC")
						saveWorkbook(wb)
						shell(sname)
				}
				}
				   {# model assumptions
					{# calling simple
						m = lmer(deltaT/obs_time ~ 1+(1|nest_ID) , bo_)
									#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 								  
									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
									  qqline(resid(m))
									  
									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = " intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))
									  
									  #scatter.smooth(resid(m)~bo_$type);abline(h=0, lty=2, col='red')
									  #boxplot(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
									  
									   mtext("lmer(deltaT/obs_time ~ 1+(1|nest_ID) , bo_)", side = 3, line = 0.5, cex=0.8,outer = TRUE)
									   
									   #mtext(paste("overdispersion:", round(dispersion_glmer(m),3)), side = 3, line = -2, cex=0.8,outer = TRUE)
																	  
									  acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=bo_$lon, y=bo_$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
											
											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								
							dev.off()
					}
					{# calling sex
						m = lmer(deltaT/obs_time ~ sex+(1|nest_ID) , bo_)
									#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 								  
									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
									  qqline(resid(m))
									  
									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = " intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))
									  
									  scatter.smooth(resid(m)~bo_$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~bo_$sex);abline(h=0, lty=2, col='red')
									  
									   mtext("lmer(deltaT/obs_time ~ sex+(1|nest_ID) , bo_)", side = 3, line = 0.5, cex=0.8,outer = TRUE)
									   
									   #mtext(paste("overdispersion:", round(dispersion_glmer(m),3)), side = 3, line = -2, cex=0.8,outer = TRUE)
																	  
									  acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=bo_$lon, y=bo_$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
											
											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								
							dev.off()
					}
					{# calling simple
						m = lmer(deltaT/obs_time ~ 1+(1|nest_ID) , bf)
									#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 								  
									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
									  qqline(resid(m))
									  
									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = " intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))
									  
									  #scatter.smooth(resid(m)~bo_$type);abline(h=0, lty=2, col='red')
									  #boxplot(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
									  
									   mtext("lmer(deltaT/obs_time ~ 1+(1|nest_ID) , bf)", side = 3, line = 0.5, cex=0.8,outer = TRUE)
									   
									   #mtext(paste("overdispersion:", round(dispersion_glmer(m),3)), side = 3, line = -2, cex=0.8,outer = TRUE)
																	  
									  acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=bf$lon, y=bf$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
											
											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								
							dev.off()
					}
					{# calling sex
						m = lmer(deltaT/obs_time ~ sex+(1|nest_ID) , bf)
									#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 								  
									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
									  qqline(resid(m))
									  
									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = " intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))
									  
									  scatter.smooth(resid(m)~bf$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~bf$sex);abline(h=0, lty=2, col='red')
									 
									  mtext("lmer(deltaT/obs_time ~ sex+(1|nest_ID) , bf)", side = 3, line = 0.5, cex=0.8,outer = TRUE)
									   	  
									  acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=bf$lon, y=bf$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
											
											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								
							dev.off()
					}
					}
				{# OLD - LATER DELETE
				    {# calling in before exchange bouts
					{# model output for text
						m = lmer(deltaT/obs_time ~ 1+(1|nest_ID) , bo_)
						pred=c('Intercept')
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
					# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model='1',dependent = 'proportion of observation period calling occured', type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model='1',dependent = 'proportion of observation period calling occured', type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o=rbind(oii,ri)
					sname = tempfile(fileext='.xls')
						wb = loadWorkbook(sname,create = TRUE)	
						createSheet(wb, name = "output")
						writeWorksheet(wb, o, sheet = "output")
						#createSheet(wb, name = "output_AIC")
						#writeWorksheet(wb, rbind(o), sheet = "output_AIC")
						saveWorkbook(wb)
						shell(sname)
				}	
					{# TO DO sex model output for Supplementary Table
						m = lmer(deltaT/obs_time ~ sex+(1|nest_ID) , bo_)
					}
					{# Figure 2a
						{# run first 
			    {# model estimates
					m = lmer(deltaT/obs_time ~ 1+(1|nest_ID) , bo_)
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci = apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
				
				}		
			 	{# raw data
					bo_$n = 1
					u=ddply(bo_,.(nest_ID,obs_ID, type), summarise,m=median(deltaT/obs_time), q1=quantile(deltaT/obs_time,0.25), q2= quantile(deltaT/obs_time,0.75), n = sum(n))
					u$type_j=2
					u$type_j=jitter(u$type_j)
					}
				}
					if(PNG == TRUE) {
					png(paste(outdir,"Figure_2.png", sep=""), width=1.85+0.6,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85+0.6,height=1.5)
					}	
				
				par(mar=c(0.25,0,0,2.8),oma = c(1, 1.8+0.5, 1, 1),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
						
						
					# calls
						plot(u$m~u$type_j, xlim=c(2-0.044,2+0.044), ylim=c(0,1),xaxt='n',  ylab = "Number of calls",xlab = NULL,type='n')
						
						lines(y=c(0,0),x=c(2-0.048,2+0.044), lty = 1,col="grey70")
						text(y=c(0),x=c(2+0.038), labels = 'Start of exchange', col="grey30",xpd=TRUE, cex=0.5,pos=4)
						
						mtext("Call occurance within observation\n[proportion of observed time]",side=2,line=1, cex=0.6, las=3, col='grey30')
						#axis(1, at=c(2,6), label=c('Before exchange', 'Regular'), mgp=c(0,-0.20,0))
											
						#axis(1, at=c(2,6), label=c('Before exchange', 'Regular'), mgp=c(0,-0.20,0))
						#mtext("Observation [median",side=1,line=0.4, cex=0.5, las=1, col='grey30')
						
						arrows(x0=u$type_j, y0=u$q1,x1=u$type_j, y1=u$q2, code = 0, col=col_p, angle = 90, length = .025, lwd=0.5, lty=1)
						
						symbols(u$type_j, u$m, circles=sqrt(u$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) #
						
						lines(y=c(0.5,0.5),x=c(2-0.044,2+0.044), lty = 3)
)						
						# add predictions + 95%CI
							arrows(x0=2, y0=ci[1],x1=2, y1=ci[2], code = 0, col="red", angle = 90, length = .025, lwd=1.5, lty=1)
							points(y=v,x=2, pch=19, cex=0.9,col="red")
													
						# legend
							mtext(expression(italic('N')*' call cases:'),side = 4,line=0, padj=-7,cex=0.5,las=1,col='grey30', xpd=TRUE) # for printing into device use padj=-7.5
							symbols(c(2+0.044,2+0.044,2+0.0445)+0.01,c(0.94,0.8,0.66)-0.05,circles=sqrt(c(1,5,10)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
							text(c(2+0.044,2+0.044,2+0.0445)+0.02,c(0.94,0.8,0.66)-0.05,labels=c(1,5,10), xpd=TRUE, cex=0.5,col='grey30') 
							
							mtext('Median & IQR',side = 4,line=0,padj=1.5, cex=0.5,las=1,col='grey30', xpd=TRUE) 
							mtext('Estimate &\n95%CrI',side = 4,line=0,padj=2, cex=0.5,las=1,col='red', xpd=TRUE) 
							#text(c(7.1),c(2.05),labels=c('Median & IQR'), xpd=TRUE, cex=0.5,col='grey30', srt=90) 
							
							
			if(PNG == TRUE) {dev.off()}
					
					}
				{# not used 
          			
					bo_ = bo[bo$behaviour == 'c' & bo$who == 'o',]
					hist(bo_$deltaT)
          			m= lmer(deltaT ~ type + (1|bird_ID)+(1|nest_ID) , bo_[bo_$behaviour == 'c' & bo_$who == 'o',])
          			#fm= lmer(deltaT ~ type+sin(rad)+type+cos(rad)+(1|nest_ID) , bo_[bo_$behaviour == 'c' & bo_$who == 'o',])
          			#fm= lmer(deltaT ~ type*sin(rad)+type*cos(rad)+(1|nest_ID) , bo_[bo_$behaviour == 'c' & bo_$who == 'o',])
          			#fm= lmer(deltaT ~ type*scale(day_j)+ (1|nest_ID) , bo_[bo_$behaviour == 'c' & bo_$who == 'o',])
          			summary(m)
          			glht(m) %>% summary
          			plot(allEffects(m))
          			ggplot(bo_[bo_$behaviour == 'c' & bo_$who == 'o',], aes(x=type, y=deltaT, col=type))+geom_bo_xplot()
          		       
					# distribution over time
						ggplot(bo_[bo_$behaviour == 'c' & bo_$who == 'o',], aes(x=deltaT, col=type))+geom_density()
						ggplot(bo_, aes(y=deltaT/obs_time, x = type))+geom_boxplot()
						# includes also those that left before presence of the coming bird - USE IN THE MANUSCRIPT
						ggplot(b[b$behaviour == 'c' & b$who == 'o',], aes(x=deltaT, col=type))+geom_density() +geom_vline(xintercept = 0) 
						ggplot(b[b$behaviour == 'c' & b$who == 'o',], aes(x=deltaT, fill=type))+geom_histogram(position="dodge" ,  alpha=0.4) +geom_vline(xintercept = 0)
						
						ggplot(b[b$behaviour == 'c' & b$who == 'o',], aes(x=deltaT, col=type))+geom_histogram(aes(y=..density..),position="dodge" ,  alpha=0.4) +geom_density() +geom_vline(xintercept = 0) 
					 }  			
				}	
					{# fly_off happen less often as the exchange approaches
          			{# model output for text
						m = lmer(deltaT/obs_time ~ 1+(1|nest_ID) , bf)
						pred=c('Intercept')
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
					# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model='1',dependent = 'proportion of observation period fly-off occured', type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model='1',dependent = 'proportion of observation period calling occured', type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o=rbind(oii,ri)
					sname = tempfile(fileext='.xls')
						wb = loadWorkbook(sname,create = TRUE)	
						createSheet(wb, name = "output")
						writeWorksheet(wb, o, sheet = "output")
						#createSheet(wb, name = "output_AIC")
						#writeWorksheet(wb, rbind(o), sheet = "output_AIC")
						saveWorkbook(wb)
						shell(sname)
				}	
					{# TO DO sex model output for Supplementary Table
						m = lmer(deltaT/obs_time ~ sex+(1|nest_ID) , bf)
					}
					{# not used
					hist(bo$deltaT[bo$behaviour == 'f' & bo$who == 'o'])
          			fm= lmer(deltaT ~ type + (1|nest_ID) , bo[bo$behaviour == 'f' & bo$who == 'o',])
          			#fm= lmer(deltaT ~ type*scale(day_j)++ (1|nest_ID) , bo[bo$behaviour == 'f' & bo$who == 'o',])
          			summary(fm)
          			glht(fm) %>% summary
          			plot(allEffects(fm))
          			
          			ggplot(bo[bo$behaviour == 'f' & bo$who == 'o',], aes(x=type, y=deltaT, col=type))+geom_boxplot()
          			
					# distribution over time
          			ggplot(bo[which(bo$behaviour == 'f' & bo$who == 'o'),], aes(x=deltaT, col=type))+geom_density()
          			#ggplot(b[which(b$behaviour == 'f' & b$who == 'o'),], aes(x=deltaT, col=type))+geom_density()
          			ggplot(b[b$behaviour == 'f' & b$who == 'o',], aes(x=deltaT, fill=type))+geom_histogram(position="dodge" ,  alpha=0.4) +geom_vline(xintercept = 0)
          			#ggplot(bo[bo$behaviour == 'f' & bo$who == 'o',], aes(x=deltaT, fill=type))+geom_histogram(position="dodge" ,  alpha=0.4) +geom_vline(xintercept = 0)
					ggplot(b[b$behaviour == 'f' & b$who == 'o',], aes(x=deltaT, col=type))+geom_histogram(aes(y=..density..),position="dodge" ,  alpha=0.4) +geom_density() +geom_vline(xintercept = 0) 
			}
				}	
				}
	}		
	 }				
	{# Exchange procedure - DECIDE WHETHER TO INVOLVE DATETIME LEFT
	  {# run first
      		dd$presence=as.numeric(difftime(dd$dt_on, dd$dt_1st_presence,'secs'))  # how long before the bird sits down on nest is he present)
    		dd$arrival=as.numeric(difftime(dd$dt_on,dd$dt_arrive,'secs'))  # how long before the bird sits down on nest is he close by / anters the picutre (i.e. start the exchange))
      		### dd$presence2=as.numeric(dd$dt_left-dd$dt_1st_presence)
			
			
			dd$pa=dd$presence-dd$arrival
			dd$pa[dd$pa <= 0] = 0.001
			dd$gap=as.numeric(difftime(dd$dt_on,dd$dt_left,'secs'))  # exchange gap - time span between leaving of the incubating bird and sitting down of its partner
			dd$gap[dd$gap == 0] = 0.001 
			dd$left_type = as.factor(ifelse(is.na(dd$arrival), NA,
								ifelse(dd$dt_left < dd$dt_1st_presence, '1 before presence', 
									ifelse(dd$dt_left < dd$dt_arrive, '2 while around','3 during exchange'))))
			
			dd$left_before_presence = as.factor(dd$left_before_presence)	
			dd$sex = as.factor(dd$sex)					
		
    		
			dd_ = dd[!is.na(dd$arrival),]
			ex = dd[dd$left_before_presence=="n",]
			#ex_$obs_ID[is.na(ex_$push)]
			ex_= ex[!is.na(ex$pushoff_int),]
			ex_$push = as.factor(ifelse(ex_$pushoff_int==3,'y','n'))
			ex_$both=as.numeric(difftime(ex_$dt_left,ex_$dt_arrive, 'secs')) # time when both present in the vicinity of the nest
			eb = ex_[ex_$both>0,] # only for cases where incubating bird left after the returning initiated the exchange 
			eb_ = eb[!is.na(eb$call_int_1),]  
			
			e= ex_[!is.na(ex_$current_bout),]
			dx = ex_[!is.na(ex_$with_calling),]
			dx$w_call = ifelse(dx$with_calling == 'y', 1, 0)
			dx_ = dx[dx$with_calling == 'y',]
			dx_$reply = ifelse(dx_$o_replies == 'y', 1, 0)
			
	  }
	  {# durations
		 {# descriptive
		   {# first presence - DELETE
			summary(dd$presence)
			length(dd$presence) # number of exchanges
			length(unique(dd$nest_ID)) # number of nests
			
			densityplot(dd$presence/60)
			dd$obs_ID[dd$presence/60 > 4]
			ggplot(dd, aes(x = sex, y = log(presence), fill = sex)) + geom_boxplot() 
			ggplot(dd, aes(x = day_j, y = log(presence), fill = sex)) + geom_point() + stat_smooth() 
			
			m = lmer(log(presence)~ sex*day_j+(day_j|nest_ID), dd_)
			plot(allEffects(m))
			summary(glht(m))
		  }
		  
		  {# presence before exchange
			summary(dd_$pa)
			length(dd_$pa) # number of exchanges
			length(unique(dd_$nest_ID)) # number of nests
			
			densityplot(dd$pa/60)
			densityplot(log(dd$pa/60))
			densityplot(asin(dd$pa/60))
			dd$obs_ID[dd$pa/60 > 2]
			ggplot(dd_, aes(x = sex, y = log(pa), fill = sex)) + geom_boxplot() 
			ggplot(dd, aes(x = day_j, y = log(pa), fill = sex)) + geom_point() + stat_smooth() 
			ggplot(dd, aes(x = day_j, y = (pa), fill = sex)) + geom_point() + stat_smooth() 
			table(dd_$pa)
			m = lmer(log(pa)~ sex*day_j+(day_j|nest_ID), dd_)
			plot(allEffects(m))
			summary(glht(m))
		  }
		  {# arrival
			summary(dd$arrival)
			length(dd$arrival[!is.na(dd$arrival)]) # number of exchanges
			length(unique(dd$nest_ID[!is.na(dd$arrival)])) # number of nests
						
			densityplot((ex$arrival-ex$gap)/60)
			densityplot(dd$arrival/60)
			densityplot(log(dd$arrival))
			dd$obs_ID[is.na(dd$arrival)]
			dd$obs_ID[dd$arrival/60 > 1]
			dd$obs_ID[dd$arrival>dd$presence]
			
			ggplot(dd_, aes(x = left_type, y = log(arrival), col = sex)) + geom_boxplot(outlier.shape = 1) + 
				geom_dotplot(aes(fill=sex),binaxis="y",stackdir="center",dotsize=0.5)
				geom_point(position = position_jitter(width = 0.2))
					
			ggplot(dd_, aes(x = left_type, y = log(arrival), fill = sex)) + geom_boxplot() 
				m = lmer(log(arrive)~ left_type + sex*day_j+(day_j|nest_ID), dd_)
				plot(allEffects(m))
				summary(glht(m))
			ggplot(dd[!is.na(dd$arrival),], aes(x = sex, y = log(arrival), fill = sex)) + geom_boxplot() 
			ggplot(dd[!is.na(dd$arrival),], aes(x = day_j, y = log(arrival), fill = sex)) + geom_point() + stat_smooth() 
			
			summary(factor(dd$left_before_presence))
				
				
			cor(dd$arrival[!is.na(dd$arrival)],dd$presence[!is.na(dd$arrival)],method = 'pearson')
			cor(dd$arrival[!is.na(dd$arrival)],dd$presence[!is.na(dd$arrival)],method = 'spearman')
				cor(dd_$arrival,dd_$pa,method = 'spearman')
				cor(dd_$arrival,dd_$pa,method = 'pearson')
			ggplot(dd[!is.na(dd$arrival),], aes(x = log(presence), y = log(arrival), fill = sex)) + geom_point() + stat_smooth()
			ggplot(dd[!is.na(dd$arrival),], aes(x = log(presence), y = log(arrival))) + geom_point() + stat_smooth()
			ggplot(dd[!is.na(dd$arrival),], aes(x = log(presence), y = log(arrival))) + geom_point() + stat_smooth(method = 'lm')
		  
		  
			
			{# dont use
				# is time span between present and arrival dependent on whether incubating bird was around
				summary(dd_$pa)
				densityplot(dd_$pa)
				ggplot(dd_, aes(x = left_before_presence, y = log(pa+0.01), fill = sex)) + geom_boxplot()
				ggplot(dd[!is.na(dd$arrival),], aes(x = left_before_presence, y = log(arrival), fill = sex)) + geom_boxplot()
				m = lmer(log(pa+0.01)~ left_before_presence +(1|nest_ID), dd_)
				plot(allEffects(m))
				summary(glht(m))
			
			}
		  }
		  {# exchange gap
			summary(dd$gap)
			length(dd$gap[dd$gap>60])/nrow(dd)
			length(dd$gap[!is.na(dd$gap)]) # number of exchanges
			length(unique(dd$nest_ID[!is.na(dd$arrival)])) # number of nests
						
			densityplot(dd$gap/60)
			densityplot(log(dd$gap))
			dd$obs_ID[is.na(dd$gap)]
			dd$obs_ID[dd$gap/60 > 1]
			dd$obs_ID[dd$gap/60 > 4]
			#dd[dd$gap/60 > 4,]
		
			
			ggplot(dd[!is.na(dd$gap),], aes(x = sex, y = log(gap), fill = sex)) + geom_boxplot() 
			ggplot(dd[!is.na(dd$gap),], aes(x = day_j, y = log(gap), fill = sex)) + geom_point() + stat_smooth() 
			
    		m = lmer(log(gap)~ sex*day_j+(day_j|nest_ID), dd_)
			plot(allEffects(m))
			summary(glht(m))
				
			}
		  {# start to left
			summary(eb$both)
			length(eb$both[!is.na(eb$both)]) # number of exchanges
			length(unique(eb$nest_ID[!is.na(eb$both)])) # number of nests
			densityplot(eb$both)		
			densityplot(log(eb$both))		
					{# correlation between push off and calling intensity
					cor(eb_$pushoff_int,eb_$call_int_1,method = 'spearman')
					cor(eb_$pushoff_int,eb_$call_int_1,method = 'pearson')
					ggplot(eb_,aes(x=call_int_1, y=pushoff_int))+geom_point()+stat_smooth(method='lm')
					ggplot(eb_,aes(x=call_int_1, y=as.numeric(as.factor(push))))+geom_point()+stat_smooth(method='lm')
					}
					
		}
		 }
		 {# Supplementary Table 3			
			  {# prepare table data
				{# presence before exchange
				 m = lmer(pa ~ sex*scale(day_j)+(day_j|nest_ID), dd_)
							# binomial gives same results
								#dd_$pa_bin=ifelse(dd_$pa == 0.001, 0,1)
								#m = glmer(pa_bin~ sex*day_j+(day_j|nest_ID),family='binomial', dd_)
					pred=c('Intercept (f)','Sex(m)', 'Day', 'Day:sex')
					dep = 'presence'
					mod = 1
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,2)
						oi$lwr_r=round(oi$lwr,2)
						oi$upr_r=round(oi$upr,2)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o1=rbind(oii,ri)
				}
				{# arrival
				   m = lmer(log(arrival) ~ left_type + sex*scale(day_j)+(scale(day_j)|nest_ID), dd_)
					pred=c('Intercept (f & before)','Left between', 'Left after', 'Sex(m)', 'Day', 'Day:sex')
					dep = 'log(arrival)'
					mod = 2
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,2)
						oi$lwr_r=round(oi$lwr,2)
						oi$upr_r=round(oi$upr,2)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o2=rbind(oii,ri)
				}
				{# gap
				  m = lmer(log(gap)~ sex*scale(day_j)+(day_j|nest_ID), dd)
				  #dd$capture=as.factor(dd$capture)
				  #dd$cap=as.factor(ifelse(dd$capture%in%c(1,2,3,4), 'y','n'))
				  #ggplot(dd,aes(x=capture, y=log(gap))) + geom_point()
				  #m = lmer(gap~ cap+sex*scale(day_j)+(day_j|nest_ID), dd)
				  #m = lmer(log(gap)~ cap+sex*scale(day_j)+(day_j|nest_ID), dd)
				  #m = lmer(log(gap)~ capture+sex*scale(day_j)+(day_j|nest_ID), dd)
					pred=c('Intercept (f)','Sex(m)', 'Day', 'Day:sex')
					dep = 'log(gap)'
					mod = 3
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,2)
						oi$lwr_r=round(oi$lwr,2)
						oi$upr_r=round(oi$upr,2)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o3=rbind(oii,ri)
				}
				{# start to left
				 m = lmer(both ~ sex*push + sex*scale(day_j)+(as.numeric(push)|bird_ID) + (1|nest_ID), eb)
							
					pred=c('Intercept (f & n)','Sex(m)','Push(y)', 'Day', 'Day:push', 'Day:sex')
					dep = 'both'
					mod = 4
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,2)
						oi$lwr_r=round(oi$lwr,2)
						oi$upr_r=round(oi$upr,2)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o4=rbind(oii,ri)
				
				
				
			 }
			  }
			  {# create xlsx table		
						o=rbind(o1,o2,o3,o4)
						sname = tempfile(fileext='.xls')
						wb = loadWorkbook(sname,create = TRUE)	
						createSheet(wb, name = "output")
						writeWorksheet(wb, o, sheet = "output")
						#createSheet(wb, name = "output_AIC")
						#writeWorksheet(wb, rbind(o), sheet = "output_AIC")
						saveWorkbook(wb)
						shell(sname)
				}
		 }
			 	{# model assumptions
					{# presence before exchange
						 m = lmer(pa~ sex*day_j+(day_j|nest_ID), dd_)
								
						
									#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 
									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
									  qqline(resid(m))
									  
									  x = data.frame(fit =fitted(m),res = resid(m))
									  scatter.smooth(x$fit[x$fit<400],x$res[x$fit<400],col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  
									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))
									  
									  qqnorm(unlist(ranef(m)$nest_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$nest_ID[2]))
									  
									  scatter.smooth(resid(m)~dd_$day_j);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~dd_$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~dd_$sex);abline(h=0, lty=2, col='red')
									  
									   mtext("lmer(log(pad)~ sex*day_j+(day_j|nest_ID), dd)", side = 3, line = 0.5, cex=0.8,outer = TRUE)
									   
									    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=dd_$lon, y=dd_$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
											
											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								
							dev.off()
					}
					{# arrival
						m = lmer(log(arrival) ~ left_type + sex*day_j+(day_j|nest_ID), dd_)
														
									  dev.new(width=6,height=9)
									  #png(paste(outdir,"model_ass/Supplementary_Table_3b_.png", sep=""), width=6,height=9,units="in",res=600)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 								  
									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
									  qqline(resid(m))
									  
									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))
									  
									  qqnorm(unlist(ranef(m)$nest_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$nest_ID[2]))
									  
									  scatter.smooth(resid(m)~dd_$day_j);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~dd_$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~dd_$sex);abline(h=0, lty=2, col='red')
									  
									  scatter.smooth(resid(m)~dd_$left_type);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~dd_$left_type);abline(h=0, lty=2, col='red')
									   mtext("lmer(log(arrival) ~ left_type + sex*day_j+(day_j|nest_ID), dd_)", side = 3, line = 0.5, cex=0.8,outer = TRUE)
									   
									    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=dd_$lon, y=dd_$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
											
											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								
							dev.off()
					}
					{# gap
						 m = lmer(log(gap)~ sex*day_j+(day_j|nest_ID), dd)
									 dev.new(width=6,height=9)
									#png(paste(outdir,"model_ass/Supplementary_Table_3c.png", sep=""), width=6,height=9,units="in",res=600)
									  
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 								  
									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
									  qqline(resid(m))
									  
									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))
									  
									  qqnorm(unlist(ranef(m)$nest_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$nest_ID[2]))
									  
									  scatter.smooth(resid(m)~dd$day_j);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~dd$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~dd$sex);abline(h=0, lty=2, col='red')
									  
									   mtext("lmer(log(gap)~ sex*day_j+(day_j|nest_ID), dd)", side = 3, line = 0.5, cex=0.8,outer = TRUE)
									   
									    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=dd$lon, y=dd$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
											
											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								
							dev.off()
					}
					{# presence before exchange
						 m = lmer(both ~ sex*push + sex*day_j+(as.numeric(push)|bird_ID) + (1|nest_ID), eb)
								 dev.new(width=6,height=9)
								#png(paste(outdir,"model_ass/Supplementary_Table_3d.png", sep=""), width=6,height=9,units="in",res=600)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 
									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red');abline(h=0, lty=2)
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
									  qqline(resid(m))
									  
									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))
									  
									  qqnorm(unlist(ranef(m)$bird_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$bird_ID [1]))
									  
									  qqnorm(unlist(ranef(m)$bird_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$bird_ID[2]))
									  
									  scatter.smooth(resid(m)~eb$day_j);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~eb$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~eb$sex);abline(h=0, lty=2, col='red')
									  
									  scatter.smooth(resid(m)~eb$push);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~eb$push);abline(h=0, lty=2, col='red')
									  
									   mtext("lmer(both ~ sex*push + sex*day_j+(as.numeric(push)|bird_ID) + (1|nest_ID), eb)", side = 3, line = 0.5, cex=0.8,outer = TRUE)
									   
									    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=eb$lon, y=eb$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
											
											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								
							dev.off()
					}
					{# presence before exchange log - worse fit
						 m = lmer(log(both) ~ sex*push + sex*day_j+(as.numeric(push)|bird_ID) + (1|nest_ID), eb)
								 dev.new(width=6,height=9)
								#png(paste(outdir,"model_ass/Supplementary_Table_3d_log.png", sep=""), width=6,height=9,units="in",res=600)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 
									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red');abline(h=0, lty=2)
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
									  qqline(resid(m))
									  
									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))
									  
									  qqnorm(unlist(ranef(m)$bird_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$bird_ID [1]))
									  
									  qqnorm(unlist(ranef(m)$bird_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$bird_ID[2]))
									  
									  scatter.smooth(resid(m)~eb$day_j);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~eb$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~eb$sex);abline(h=0, lty=2, col='red')
									  
									  scatter.smooth(resid(m)~eb$push);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~eb$push);abline(h=0, lty=2, col='red')
									  
									   mtext("lmer(log(both) ~ sex*push + sex*day_j+(as.numeric(push)|bird_ID) + (1|nest_ID), eb)", side = 3, line = 0.5, cex=0.8,outer = TRUE)
									   
									    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=eb$lon, y=eb$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
											
											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								
							dev.off()
					}
				}
		 {# Figure 3
			{# prepare for plotting
				k=0.1 # distance
				kk=k*3# distance
				kkk=k*2
				dd$left_sex=ifelse(dd$left_type=='1 before presence',ifelse(dd$sex=='f', 1-k,k+1),
								ifelse(dd$left_type=='2 while around',ifelse(dd$sex=='f', 4-k,k+4),
									ifelse(dd$sex=='f', 7-k,k+7)))
				x = dd
				x$at=ifelse(dd$left_type=='1 before presence',ifelse(dd$sex=='f', 1-kkk,2+kkk),
								ifelse(dd$left_type=='2 while around',ifelse(dd$sex=='f', 3.7-kkk,4.7+kkk),
									ifelse(dd$sex=='f', 6.4-kkk,7.4+kkk)))					
				dd$col_=ifelse(dd$sex=='f','#FCB42C', '#535F7C')	
				}	
			{# prepare model predictions
				m = lmer(log(arrival) ~ left_type + sex*day_j+(scale(day_j)|nest_ID), dd)
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim) 
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
				# values to predict for		
					newD=data.frame(left_type=c('1 before presence','2 while around','3 during exchange'),
									sex = 0.5,
									day_j = mean(dd$day_j)
									)
						
				# exactly the model which was used has to be specified here
				X <- model.matrix(~ left_type + sex*day_j,data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v) 
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pp=newD	
					pt_=pp[pp$type=='ex',]
					pc_=pp[pp$type=='non',]
						
			}
			{# plot
			if(PNG == TRUE) {
					png(paste(outdir,"Figure_3.png", sep=""), width=1.85+0.6,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85+0.6,height=1.5)
					}	
			par(mar=c(0.8,0,0,2.5),oma = c(0.7, 2, 0, 0),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey40", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
			
						
				boxplot(log(arrival) ~ left_sex, data = dd, 
										#ylab =NULL, 
										xaxt='n',
										yaxt='n',
										par(bty='n'),
										#at=c(1,2,3.5,4.5),
										at=c(1,2,3.7,4.7, 6.4,7.4), type='n',
										outcex=0.5, outpch=20,boxwex=0.25,whisklty=1,staplelty=0,#medlwd=1, 
										lwd = 0.25, 
										#ylim=c(0,1),
										outcol="white",boxcol='white',whiskcol='white',staplecol='white',medcol='white'
										) # col=z_g$cols, border=z_g$cols
										
								
				for (i in 1:nrow(x)){stripchart(log(x$arrival[i])~ factor(x$left_sex[i]), at = x$at[i],
											bg = x$col_[i],
											col="gray63",
											#col = x$col_[i],
											#bg = adjustcolor(x$col_[i], alpha.f = 0.4),
											pch =21, cex=0.5,
											vertical = TRUE, add = TRUE, method = "jitter") 
											}
					
				boxplot(log(arrival) ~ left_sex, data = dd,
										ylab = NULL,xaxt='n', yaxt='n',
										#at=c(1,2,3.5,4.5),
										at=c(1+kk,2-kk,3.7+kk,4.7-kk, 6.4+kk,7.4-kk),
										type='n',
										outcex=0.5,outpch=20,boxwex=0.25,whisklty=1,staplelty=0,#medlwd=1, 
										lwd = 1,
										border=c('#FCB42C','#535F7C','#FCB42C','#535F7C','#FCB42C','#535F7C'),
										col = adjustcolor("white", alpha.f = 0), # trick for PNGs, to show what is underneath the boxplot else can be taken out
										#outcol="darkgrey",boxcol='darkgrey',whiskcol='darkgrey',staplecol='darkgrey',medcol='darkgrey', 
										#par(bty='l'),
										add=TRUE
										)					
					
					text(x=1*0.6,y=log(480)*0.97, labels='\u2640', col='#FCB42C', cex=0.6)
					text(x=1,y=log(480), labels='\u2642', col='#535F7C', cex=0.6)
					mtext("Prediction\n95%CrI",side=3,line=-1.5, cex=0.5, las=1, col='red')
					#axis(1, at=c(1.5,4.2, 6.9), labels=FALSE)
					
					#text(c(1,2), par("usr")[3]+0.07, labels = c('\u2640','\u2642'), font=4, xpd = TRUE, cex=0.6, col=c('#FCB42C','#535F7C'))#col="grey30") #labels 
					text(c(1.5,4.2, 6.9), par("usr")[3]-0.25, labels = c('Before\npresence','While\naround', 'During\nexchange'),  xpd = TRUE, cex=0.5, col="grey30")
					mtext("Incubating parent left",side=1,line=0.6, cex=0.5, las=1, col='grey30')
					#text(c(2.85), par("usr")[3]-0.18, labels = c('Period'),  xpd = TRUE, cex=0.6, col="grey30")
					at_=c(5,15,30,60,120,240,480)
					
					#for(i in 1:6){axis(2, at=log(seq(at_[i],at_[i+1],length.out=15)), labels=FALSE,tcl=-0.05, lwd=0.5)}
					for(i in 1:6){axis(2, at=log(seq(at_[i],at_[i+1],length.out=10)), labels=FALSE,tcl=-0.075, lwd=0.5)}
					axis(2, at=log(at_), labels=c('5 s','15 s','30 s','1 min','2 min','4 min','8 min'))
					mtext("Excahnge length",side=2,line=1.3, cex=0.55, las=3, col='grey30')
					#mtext("Duration",side=2,line=1, cex=0.6, las=3, col='grey30')
					
					# predictions	
						points(y=pp$pred,x=c(1.5,4.2,6.9), pch=20, cex=0.9,col="red")
					# 95%CI 
						arrows(x0=c(1.5,4.2,6.9), y0=pp$lwr,x1=c(1.5,4.2,6.9), y1=pp$upr,
						code = 0, col="red", angle = 90, length = .025, lwd=1, lty=1)
								
				 if(PNG == TRUE) {dev.off()}
			}
		}
					{# not used
					stripchart(log(arrival)~ factor(left_sex), vertical = TRUE, data = dd, method = "jitter", add = TRUE, 
										at=c(1-kkk,2+kkk,3.7-kkk,4.7+kkk, 6.4-kkk,7.4+kkk),
										pch = 21,cex=0.5, 
										#col=dd$col_#c('#FCB42C','#535F7C','#FCB42C','#535F7C','#FCB42C','#535F7C'),
										#bg=dd$col_
										#bg=adjustcolor(dd$col_, alpha.f = 0.4)
										col="gray80",
										#bg=adjustcolor("gray63", alpha.f = 0.4)
										bg=c('#FCB42C','#535F7C','#FCB42C','#535F7C','#FCB42C','#535F7C')
										)
					
					
					}
		{# Figure 3x
			{# prepare for plotting
				k=0.1 
				kk=k*2# distance for boxplots
				kkk=k*2 # distance for points
				eb$push_sex=ifelse(eb$push=='y',ifelse(eb$sex=='f', 1-k,k+1),
								ifelse(eb$sex=='f', 4-k,k+4))
				x = eb
				x$at=ifelse(eb$push=='y',ifelse(eb$sex=='f', 1-kkk,2+kkk),
								ifelse(eb$sex=='f', 3.7-kkk,4.7+kkk))					
				eb$col_=ifelse(eb$sex=='f','#FCB42C', '#535F7C')	
				}	
			{# prepare model predictions
				 m = lmer(both ~ sex*push + sex*day_j+(as.numeric(push)|bird_ID) + (1|nest_ID), eb)
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim) 
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
				# values to predict for		
					newD=data.frame(push=c('y','n'),
									sex = 0.5,
									day_j = mean(eb$day_j)
									)
						
				# exactly the model which was used has to be specified here
				X <- model.matrix(~ sex*push + sex*day_j,data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v) 
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pp=newD	
					pt_=pp[pp$push=='y',]
					pc_=pp[pp$push=='n',]
						
			}
			{# plot
			if(PNG == TRUE) {
					png(paste(outdir,"Figure_3x.png", sep=""), width=1.85+0.6,height=1.5,units="in",res=600) 
					}else{
					dev.new(width=1.85+0.6,height=1.5)
					}	
			par(mar=c(0.8,0,0,2.5),oma = c(0.7, 2, 0, 0),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey40", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
			
						
				boxplot(both ~ push_sex, data = eb, 
										#ylab =NULL, 
										xaxt='n',
										#yaxt='n',
										ylim=c(0,100),
										par(bty='n'),
										#at=c(1,2,3.5,4.5),
										at=c(1,2,3.7,4.7), type='n',
										outcex=0.5, outpch=20,boxwex=0.25,whisklty=1,staplelty=0,#medlwd=1, 
										lwd = 0.25, 
										#ylim=c(0,1),
										outcol="white",boxcol='white',whiskcol='white',staplecol='white',medcol='white'
										) # col=z_g$cols, border=z_g$cols
										
								
				for (i in 1:nrow(x)){stripchart(x$both[i]~ factor(x$push_sex[i]), at = x$at[i],
											bg = x$col_[i],
											col="gray63",
											#col = x$col_[i],
											#bg = adjustcolor(x$col_[i], alpha.f = 0.4),
											pch =21, cex=0.5,
											vertical = TRUE, add = TRUE, method = "jitter") 
											}
					
				boxplot(both ~ push_sex, data = eb, 
										ylab = NULL,xaxt='n', yaxt='n',
										#at=c(1,2,3.5,4.5),
										at=c(1+kk,2-kk,3.7+kk,4.7-kk),
										type='n',
										outcex=0.5,outpch=20,boxwex=0.25,whisklty=1,staplelty=0,#medlwd=1, 
										lwd = 1,
										border=c('#FCB42C','#535F7C','#FCB42C','#535F7C'),
										col = adjustcolor("white", alpha.f = 0), # trick for PNGs, to show what is underneath the boxplot else can be taken out
										#outcol="darkgrey",boxcol='darkgrey',whiskcol='darkgrey',staplecol='darkgrey',medcol='darkgrey', 
										#par(bty='l'),
										add=TRUE
										)					
					
					text(x=0.3,y=100*0.97, labels='\u2640', col='#FCB42C', cex=0.6, pos=4)
					text(x=0.3+0.2,y=100, labels='\u2642', col='#535F7C', cex=0.6, pos=4)
					text(x=0.3,y=100*0.88, labels="Prediction & 95%CrI", col='red', cex=0.5, pos=4)
					#mtext("Prediction\n95%CrI",side=3,line=-1.5, cex=0.5, las=1, col='red')
					#axis(1, at=c(1.5,4.2, 6.9), labels=FALSE)
					
					#text(c(1,2), par("usr")[3]+0.07, labels = c('\u2640','\u2642'), font=4, xpd = TRUE, cex=0.6, col=c('#FCB42C','#535F7C'))#col="grey30") #labels 
					text(c(1.5,4.2), par("usr")[3]-0.25, labels = c('Yes','No'),  xpd = TRUE, cex=0.5, col="grey30")
					mtext("Pushed from the nest",side=1,line=0, cex=0.6, las=1, col='grey30')
					
					mtext("From initiation to leaving [s]",side=2,line=1.3, cex=0.55, las=3, col='grey30')
					
					# predictions	
						points(y=pp$pred,x=c(1.5,4.2), pch=20, cex=0.9,col="red")
					# 95%CI 
						arrows(x0=c(1.5,4.2), y0=pp$lwr,x1=c(1.5,4.2), y1=pp$upr,
						code = 0, col="red", angle = 90, length = .025, lwd=1, lty=1)
								
				 if(PNG == TRUE) {dev.off()}
			}
		}
					
	  }
	  {# calling - how laud is the exchange?
		  {# run first
			d1 = subset(ex_,select = c('obs_ID','sound_ok','nest_ID','bird_ID', 'sex', 'day_j', 'call_int_1'))
				colnames(d1)[7] = 'call_int'
				d1$type = '1 both present'
				#d1$obs_ID[is.na(d1$call_int) & d1$sound_ok == 'y']
				#d1[is.na(d1$call_int), ]
			d01 = subset(ex_,select = c('obs_ID','sound_ok','nest_ID','bird_ID', 'sex', 'day_j', 'call_c_int'))
				colnames(d01)[7] = 'call_int'
				d01$type = '1b both present coming'
				#d1$obs_ID[is.na(d1$call_int) & d1$sound_ok == 'y']
				#d1[is.na(d1$call_int), ]	
			d02 = subset(ex_,select = c('obs_ID','sound_ok','nest_ID','bird_ID', 'sex', 'day_j', 'call_o_int'))
				colnames(d02)[7] = 'call_int'
				d02$type = '1a both present incubating'
				#d1$obs_ID[is.na(d1$call_int) & d1$sound_ok == 'y']
				#d1[is.na(d1$call_int), ]		
			d2 = subset(ex_,select = c('obs_ID','sound_ok','nest_ID','bird_ID', 'sex', 'day_j', 'call_int_c2'))
				colnames(d2)[7] = 'call_int'
				d2$type = '2 exchange gap'	
				#d2$obs_ID[is.na(d2$call_int) & d2$sound_ok == 'y']
				#d2[is.na(d2$call_int), ]
			
			d3 = subset(ex_,select = c('obs_ID','sound_ok','nest_ID','bird_ID', 'sex', 'day_j', 'call_int_c3'))
				colnames(d3)[7] = 'call_int'
				d3$type = '3 after on nest'			
				#d3$obs_ID[is.na(d3$call_int) & d3$sound_ok == 'y']
				#d3[is.na(d2$call_int), ]
			di = rbind(d1,d01,d02,d2,d3)
			di = di[!is.na(di$call_int),]
			#di$left_before = d$left_before_presence [match(di$obs_ID, d$obs_ID)]
		}	
		  {# run first TEMP
			d1 = subset(ex_,select = c('obs_ID','sound_ok','nest_ID','bird_ID', 'sex', 'day_j', 'call_int_1'))
				colnames(d1)[7] = 'call_int'
				d1$type = '1 both present'
				d1$who = 'both'
				#d1$obs_ID[is.na(d1$call_int) & d1$sound_ok == 'y']
				#d1[is.na(d1$call_int), ]
			d01 = subset(ex_,select = c('obs_ID','sound_ok','nest_ID','bird_ID', 'sex', 'day_j', 'call_c_int'))
				colnames(d01)[7] = 'call_int'
				d01$type = '1 both present'
				d01$who = 'returning'
				#d1$obs_ID[is.na(d1$call_int) & d1$sound_ok == 'y']
				#d1[is.na(d1$call_int), ]	
			d02 = subset(ex_,select = c('obs_ID','sound_ok','nest_ID','bird_ID', 'sex', 'day_j', 'call_o_int'))
				colnames(d02)[7] = 'call_int'
				d02$type = '1 both present'
				d02$who = 'incubating'
				#d1$obs_ID[is.na(d1$call_int) & d1$sound_ok == 'y']
				#d1[is.na(d1$call_int), ]		
			d2 = subset(ex_,select = c('obs_ID','sound_ok','nest_ID','bird_ID', 'sex', 'day_j', 'call_int_c2'))
				colnames(d2)[7] = 'call_int'
				d2$type = '2 exchange gap'
				d2$who = 'returning'				
				#d2$obs_ID[is.na(d2$call_int) & d2$sound_ok == 'y']
				#d2[is.na(d2$call_int), ]
			
			d3 = subset(ex_,select = c('obs_ID','sound_ok','nest_ID','bird_ID', 'sex', 'day_j', 'call_int_c3'))
				colnames(d3)[7] = 'call_int'
				d3$type = '3 after on nest'
				d3$who = 'returning'					
				#d3$obs_ID[is.na(d3$call_int) & d3$sound_ok == 'y']
				#d3[is.na(d2$call_int), ]
			di = rbind(d1,d01,d02,d2,d3)
			di = di[!is.na(di$call_int),]
			#di$left_before = d$left_before_presence [match(di$obs_ID, d$obs_ID)]
		}	
		  
		  {# distribution
			  {# calling while arriving
				
				nrow(dx)
				summary(factor(dx$with_calling))
				length(dx$with_calling[dx$with_calling=='y'])/length(dx$with_calling)
				table(dx$with_calling,dx$sex)
				length(dx$with_calling[dx$with_calling=='y' & dx$sex=='f'])/length(dx$with_calling[dx$sex=='f'])
				length(dx$with_calling[dx$with_calling=='y' & dx$sex=='m'])/length(dx$with_calling[dx$sex=='m'])
				
				
				m = glmer(w_call ~ sex*scale(day_j) + (1|bird_ID) + (1|nest_ID), dx, family = 'binomial')
				m = glmer(w_call ~ sex + (1|bird_ID) + (1|nest_ID), dx, family = 'binomial')
				plot(allEffects(m))
				summary(glht(m))
				}
			  {# reply 
				dx_ = dx[dx$with_calling == 'y',]
				nrow(dx)
				summary(factor(dx$with_calling))
				length(dx_$o_replies[dx_$o_replies=='y'])/length(dx_$o_replies)
				table(dx_$o_replies,dx_$sex)
				length(dx_$o_replies[dx_$o_replies=='y' & dx_$sex=='f'])/length(dx_$o_replies[dx_$sex=='f'])
				length(dx_$o_replies[dx_$o_replies=='y' & dx_$sex=='m'])/length(dx_$o_replies[dx_$sex=='m'])
				
				dx_$reply = ifelse(dx_$o_replies == 'y', 1, 0)
				m = glmer(reply ~ sex*scale(day_j) + (1|bird_ID) + (1|nest_ID), dx_, family = 'binomial')
				m = glmer(reply ~ sex + (1|bird_ID) + (1|nest_ID), dx_, family = 'binomial')
				plot(allEffects(m))
				summary(glht(m))
				}
				cor(subset(ex_,select = c('current_bout','next_bout')),use="pairwise.complete.obs", method="pearson") 
				cor(subset(ex_,select = c('current_bout','next_bout')),use="pairwise.complete.obs", method="spearman") 

				f = ex_[-which(is.na(ex_$call_c_int) | is.na(ex_$call_o_int)),]
				cor(subset(f,select = c('day_j','call_o_int')),use="pairwise.complete.obs", method="spearman") 
				cor(subset(f,select = c('day_j','call_o_int')),use="pairwise.complete.obs", method="pearson") 
				
			  ggplot(di, aes(x = call_int, fill = as.factor(call_int))) + 
				geom_bar(position=position_dodge()) + 
				facet_wrap(~type, nrow=5) + 
				scale_fill_brewer(palette = "Blues")
				
				
			
			ggplot(di, aes(y = call_int, x = as.numeric(as.factor(type)), col=factor(obs_ID))) + geom_line()
			
			densityplot(~ex_$call_int_1)
			
			ggplot(ex_,aes(y=call_int_1, x=day_j, fill=sex, col=sex))+geom_point()+stat_smooth()
			ggplot(ex_,aes(y=call_int_1, x=day_j, fill=sex, col=sex))+geom_point()+stat_smooth(method="lm")
			ggplot(ex_[ex_$day_j<19,],aes(y=call_int_1, x=day_j, fill=sex, col=sex))+geom_point()+stat_smooth(method="lm")
			ggplot(ex_[ex_$day_j<19,],aes(y=call_int_1, x=day_j, fill=sex, col=sex))+geom_point()+stat_smooth()

			ggplot(ex_[ex_$day_j<19,],aes(y=call_int_1, x=day_j))+geom_point()+stat_smooth(method="lm")
			ggplot(ex_[ex_$day_j<22,],aes(y=call_int_1, x=day_j))+geom_point()+stat_smooth(method="lm")
			ggplot(ex_[ex_$day_j<22,],aes(y=call_int_1, x=day_j))+geom_point()+stat_smooth()
			ggplot(ex_,aes(y=call_int_1, x=day_j))+geom_point()+stat_smooth()

			ggplot(ex_,aes(y=call_int_1, x=day_j, col=nest))+geom_point()+stat_smooth(method="lm", se=FALSE)
		}
		  {# Figure 4
			  
			  labels_ <- c(
                    `1 both present` = "From initiation to leaving",
                    `2 exchange gap` = "Exchange gap",
                    `3 after on nest` = "After on nest"
                    )
			 dev.new(width=1.85+1,height=1.5*2)
			 ggplot(di[!di$who =='both',], aes(x = call_int, fill = who)) + 
				geom_bar(position=position_dodge()) + 
				facet_wrap(~type, nrow=5, labeller = as_labeller(labels_)) + 
				ylab("Number of observations") +
				xlab("Calling intensity") +
				coord_cartesian(ylim = c(0, 100))+ 
				guides(fill=guide_legend(title="Which parent:")) +
				scale_fill_manual(values=c('#FCB42C','#535F7C')) + 
				theme_light()+
									theme(	axis.line=element_line(colour="grey70", size=0.25),
											#panel.border=element_rect(colour="white"),
											panel.border=element_rect(colour="grey70", size=0.25),
											panel.grid = element_blank(),
											
											axis.title=element_text(size=7, colour="grey30"),
											axis.title.y = element_text(vjust=1),
											axis.title.x = element_text(vjust=0.2),
											axis.text=element_text(size=6),# margin=units(0.5,"mm")),
											axis.ticks.length=unit(0.5,"mm"),
											#axis.ticks.margin,
											
											strip.text.x =element_text(size = 6, color="grey30",  margin=margin(1,1,1,1,"mm")), #grey50
											strip.background=element_rect(fill="grey99",colour="grey70", size=0.25),
											#strip.background = element_blank(), 
											#strip.text = element_blank(),
											panel.margin = unit(0, "mm"),
											#legend.position="none"
											legend.background=element_rect(colour="white"),
											legend.key=element_rect(fill="grey99", colour="white"),
											legend.text=element_text(size=7, colour="grey30"),
											legend.title=element_text(size=7, colour="grey30")
											)
				ggsave(paste(outdir,"Figure_4.png", sep=""),width=1.85+1,height=1.5*2, units = "in")							
				#ggsave(paste(outdir,"Figure_4.eps", sep=""),width=1.85+1,height=1.5*2, units = "in")							
				
		  }
		  {# Supplementary Table 4
			{# prepare table data
				{# calling while arriving
					m = glmer(w_call ~ sex*scale(day_j) + (1|bird_ID) + (1|nest_ID), dx, family = 'binomial')
							# binomial gives same results
								#dd_$pa_bin=ifelse(dd_$pa == 0.001, 0,1)
								#m = glmer(pa_bin~ sex*day_j+(day_j|nest_ID),family='binomial', dd_)
					pred=c('Intercept (f)','Sex(m)', 'Day', 'Day:sex')
					dep = 'calling while arriving (bin)'
					mod = 1
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o1=rbind(oii,ri)
				}
				{# reply
				 
				 m = glmer(reply ~ sex*scale(day_j) + (1|bird_ID) + (1|nest_ID), dx_, family = 'binomial')
					pred=c('Intercept (f)','Sex(m)', 'Day', 'Day:sex')
					dep = 'reply (bin)'
					mod = 2
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o2=rbind(oii,ri)
				}
				{# calling coming ~ calling incubating
					f = ex_[-which(is.na(ex_$call_c_int) | is.na(ex_$call_o_int)),]
					m = lmer(call_c_int ~ sex + scale(call_o_int)*sex + scale(day_j)*sex + (day_j|bird_ID) + (1|nest_ID), f)
					
					pred=c('Intercept (f)','Sex(m)', 'Call_incub', 'Day', 'Call_incub:sex','Day:sex')
					dep = 'call_c_int'
					mod = 3
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o3=rbind(oii,ri)
				}
			    {# exchange gap calling
					f = ex_[-which(is.na(ex_$call_c_int) | is.na(ex_$call_int_c2)),]
					m = lmer(call_int_c2 ~ sex + scale(call_c_int)*sex + scale(day_j)*sex + (call_c_int|bird_ID) + (1|nest_ID), f)
					pred=c('Intercept (f)','Sex(m)', 'Call_com', 'Day', 'Call_com:sex','Day:sex')
					dep = 'call during gap'
					mod = 4
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o4=rbind(oii,ri)
				}
				{# after exchange
					f = ex_[-which(is.na(ex_$call_c_int) | is.na(ex_$call_int_c3)),]
					m = lmer(call_int_c3 ~ sex + scale(call_c_int)*sex + scale(day_j)*sex + (call_c_int|bird_ID) + (1|nest_ID), f)
					pred=c('Intercept (f)','Sex(m)', 'Call_com', 'Day', 'Call_com:sex','Day:sex')
					dep = 'call after exchange'
					mod = 5
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o5=rbind(oii,ri)
				}
			}
			{# create xlsx table		
						o=rbind(o1,o2,o3,o4,o5)
						sname = tempfile(fileext='.xls')
						wb = loadWorkbook(sname,create = TRUE)	
						createSheet(wb, name = "output")
						writeWorksheet(wb, o, sheet = "output")
						#createSheet(wb, name = "output_AIC")
						#writeWorksheet(wb, rbind(o), sheet = "output_AIC")
						saveWorkbook(wb)
						shell(sname)
				}
		  }
			 	{# model assumptions TO DO
					{# after ~ exchange gap calling
						m = lmer(call_int_c3 ~ sex + scale(call_int_c2) + scale(day_j) + (call_int_c2|bird_ID) + (1|nest_ID), f)
							
									#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 
									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
									  qqline(resid(m))
									  
									 								  
									  qqnorm(unlist(ranef(m)$bird_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$bird_ID [1]))
									  
									  qqnorm(unlist(ranef(m)$bird_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$bird_ID[2]))
									  
									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))
									  
									  
									  scatter.smooth(resid(m)~f$day_j);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$call_int_c2);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~f$sex);abline(h=0, lty=2, col='red')
									  
									   mtext("lmer(call_int_c3 ~ sex + scale(call_int_c2) + scale(day_j) + (call_int_c2|bird_ID) + (1|nest_ID), f)", side = 3, line = 0.5, cex=0.8,outer = TRUE)
									   
									    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=f$lon, y=f$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
											
											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								
							dev.off()
					}
				}			  
				{# not used
			f = ex_[-which(is.na(ex_$call_int_c3) | is.na(ex_$call_int_c2) | is.na(ex_$call_int_1)),]
			f = ex_[-which(is.na(ex_$call_int_c3) | is.na(ex_$call_int_c2) | is.na(ex_$call_int_c3)),]
			nrow(f)
			m = lmer(call_int_c3 ~ sex + scale(call_int_c2)*sex + scale(day_j)*sex + (call_int_c2|bird_ID) + (1|nest_ID), f)
			m = lmer(call_int_c3 ~ sex + scale(call_int_c2) + scale(day_j) + (call_int_c2|bird_ID) + (1|nest_ID), f)
			m = lmer(call_int_c2 ~ sex + scale(call_int_1) + scale(day_j) + (call_int_1|bird_ID) + (1|nest_ID), f)
			m = lmer(call_int_c2 ~ sex + scale(call_int_1)*sex + scale(day_j)*sex + (call_int_1|bird_ID) + (1|nest_ID), f)
			
			m = lmer(call_int_c2 ~ sex + scale(call_c_int)*sex + scale(day_j)*sex + (call_c_int|bird_ID) + (1|nest_ID), f)
			m = lmer(call_int_c3 ~ sex + scale(call_c_int)*sex + scale(day_j)*sex + (call_c_int|bird_ID) + (1|nest_ID), f)
			m = lmer(call_int_c2 ~ sex + scale(call_o_int)*sex + scale(day_j)*sex + (call_o_int|bird_ID) + (1|nest_ID), f)
			m = lmer(call_o_int ~ sex + scale(call_c_int)*sex + scale(day_j)*sex + (call_c_int|bird_ID) + (1|nest_ID), f)
			m = lmer(call_c_int ~ sex + scale(call_o_int)*sex + scale(day_j)*sex + (call_o_int|bird_ID) + (1|nest_ID), f)
			m = lmer(call_o_int ~ sex + scale(next_bout)*sex + scale(call_c_int)*sex + scale(day_j)*sex + (scale(call_c_int)|bird_ID) + (1|nest_ID), f)
			m = lmer(call_c_int ~ sex + scale(current_bout)*sex + scale(call_c_int)*sex + scale(day_j)*sex + (scale(call_c_int)|bird_ID) + (1|nest_ID), f)
			m = lmer(call_c_int ~ scale(next_bout)*sex +  (scale(current_bout)|bird_ID) + (1|nest_ID), f)
			plot(allEffects(m))
			summary(glht(m))
			}
		  {# Supplementary Table 5
			{# prepare table data
				{# calling while arriving
					m = glmer(w_call ~ sex*scale(current_bout) + (1|bird_ID) , dx, family = 'binomial')
							# binomial gives same results
								#dd_$pa_bin=ifelse(dd_$pa == 0.001, 0,1)
								#m = glmer(pa_bin~ sex*day_j+(day_j|nest_ID),family='binomial', dd_)
					pred=c('Intercept (f)','Sex(m)', 'Current bout', 'Current bout:sex')
					dep = 'calling while arriving (bin)'
					mod = 1
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o1=rbind(oii,ri)
				}
				{# reply
				 
				 m = glmer(reply ~ sex*scale(current_bout) + (1|bird_ID) + (1|nest_ID), dx_, family = 'binomial')
					pred=c('Intercept (f)','Sex(m)', 'current_bout', 'current_bout:sex')
					dep = 'reply (bin)'
					mod = 2
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o2=rbind(oii,ri)
				}
				{# calling coming
					f = ex_[-which(is.na(ex_$call_c_int)),]
					m = lmer(call_c_int ~ sex*scale(current_bout) +(scale(current_bout)|bird_ID) + (1|nest_ID), f)
					#m = lmer(next_bout ~ sex*scale(call_c_int) + (scale(call_c_int)|bird_ID) + (1|nest_ID), f)
					
					pred=c('Intercept (f)','Sex(m)', 'current_bout', 'Current_bout:sex')
					dep = 'call_c_int'
					mod = 3
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o3=rbind(oii,ri)
				}
			    {# calling incubating
					f = ex_[-which(is.na(ex_$call_o_int)),]
					m = lmer(call_o_int ~ sex*scale(current_bout) + (scale(current_bout)|bird_ID) + (1|nest_ID), f)
					#m = lmer(next_bout ~ sex*scale(call_o_int) + (scale(call_o_int)|bird_ID) + (1|nest_ID), f)
					
					pred=c('Intercept (f)','Sex(m)', 'current_bout', 'Current_bout:sex')
					dep = 'call_c_int'
					mod = 4
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o4=rbind(oii,ri)
				}
				{# exchange gap calling
					f = ex_[-which(is.na(ex_$call_int_c2)),]
					m = lmer(call_int_c2 ~ sex*scale(current_bout) + (scale(current_bout)|bird_ID) + (1|nest_ID), f)
					#m = lmer(next_bout ~ sex*scale(call_int_c2) + (scale(call_int_c2)|bird_ID) + (1|nest_ID), f)
					pred=c('Intercept (f)','Sex(m)', 'current_bout', 'current_bout:sex')
					dep = 'call during gap'
					mod = 5
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o5=rbind(oii,ri)
				}
				{# after exchange
					f = ex_[-which(is.na(ex_$call_int_c3)),]
					m = lmer(call_int_c3 ~ sex*scale(current_bout) + (scale(current_bout)|bird_ID) + (1|nest_ID), f)
					#m = lmer(next_bout ~ sex*scale(call_int_c3) + (scale(call_int_c3)|bird_ID) + (1|nest_ID), f)
					pred=c('Intercept (f)','Sex(m)', 'current_bout', 'current_bout:sex')
					dep = 'call after exchange'
					mod = 6
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o6=rbind(oii,ri)
				}
			}
			{# create xlsx table		
						o=rbind(o1,o2,o3,o4,o5,o6)
						sname = tempfile(fileext='.xls')
						wb = loadWorkbook(sname,create = TRUE)	
						createSheet(wb, name = "output")
						writeWorksheet(wb, o, sheet = "output")
						#createSheet(wb, name = "output_AIC")
						#writeWorksheet(wb, rbind(o), sheet = "output_AIC")
						saveWorkbook(wb)
						shell(sname)
				}
		  }
					{# model assumptions TO DO
					{# after ~ exchange gap calling
						m = lmer(call_int_c3 ~ sex + scale(call_int_c2) + scale(day_j) + (call_int_c2|bird_ID) + (1|nest_ID), f)
							
									#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 
									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
									  qqline(resid(m))
									  
									 								  
									  qqnorm(unlist(ranef(m)$bird_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$bird_ID [1]))
									  
									  qqnorm(unlist(ranef(m)$bird_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$bird_ID[2]))
									  
									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))
									  
									  
									  scatter.smooth(resid(m)~f$day_j);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$call_int_c2);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~f$sex);abline(h=0, lty=2, col='red')
									  
									   mtext("lmer(call_int_c3 ~ sex + scale(call_int_c2) + scale(day_j) + (call_int_c2|bird_ID) + (1|nest_ID), f)", side = 3, line = 0.5, cex=0.8,outer = TRUE)
									   
									    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=f$lon, y=f$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
											
											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								
							dev.off()
					}
							  
		
	  }
	      {# Supplementary Table 6
			{# prepare table data
				{# calling while arriving
					m = lmer(next_bout ~as.factor(w_call)*sex+(1|bird_ID)+(1|nest_ID),dx)
					
					pred=c('Intercept (f, no)','return call','Sex(m)', 'Call:sex')
					dep = 'next bout'
					mod = 1
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,1)
						oi$lwr_r=round(oi$lwr,1)
						oi$upr_r=round(oi$upr,1)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o1=rbind(oii,ri)
				}
				{# reply
				 m = lmer(next_bout ~ as.factor(reply)*sex+(1|bird_ID)+(1|nest_ID),dx_)
					pred=c('Intercept (f, no)','reply call','Sex(m)', 'Call:sex')
					dep = 'next bout'
					mod = 2
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,1)
						oi$lwr_r=round(oi$lwr,1)
						oi$upr_r=round(oi$upr,1)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o2=rbind(oii,ri)
				}
				{# calling 
					f = ex_[-which(is.na(ex_$call_c_int) | is.na(ex_$call_o_int) | is.na(ex_$call_int_c2) | is.na(ex_$call_int_c3)),]
						#cor(subset(f,select = c('call_c_int','call_o_int','call_int_c2','call_int_c3')),use="pairwise.complete.obs", method="pearson") 
						#cor(subset(f,select = c('call_c_int','call_o_int','call_int_c2','call_int_c3')),use="pairwise.complete.obs", method="spearman") 
					m = lmer(next_bout ~sex*scale(call_c_int)+sex*scale(call_o_int)+sex*call_int_c2 + sex*call_int_c3 + (call_int_c2|bird_ID) + (1|nest_ID),f)
					#summary(m)
					#summary(glht(m))
					#plot(allEffects(m))
					
					pred=c('Intercept (f)','Sex(m)', 'call_arriving', 'call_incub', 'call_gap', 'call_after', 'sex:call_arriving','sex:call_incub','sex:call_gap', 'sex:call_after')
					dep = 'next_bout'
					mod = 3
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,1)
						oi$lwr_r=round(oi$lwr,1)
						oi$upr_r=round(oi$upr,1)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o3=rbind(oii,ri)
				}
			}
			{# create xlsx table		
						o=rbind(o1,o2,o3)
						sname = tempfile(fileext='.xls')
						wb = loadWorkbook(sname,create = TRUE)	
						createSheet(wb, name = "output")
						writeWorksheet(wb, o, sheet = "output")
						#createSheet(wb, name = "output_AIC")
						#writeWorksheet(wb, rbind(o), sheet = "output_AIC")
						saveWorkbook(wb)
						shell(sname)
				}
		  }
					{# model assumptions TO DO
					{# after ~ exchange gap calling
						m = lmer(call_int_c3 ~ sex + scale(call_int_c2) + scale(day_j) + (call_int_c2|bird_ID) + (1|nest_ID), f)
							
									#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 
									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
									  qqline(resid(m))
									  
									 								  
									  qqnorm(unlist(ranef(m)$bird_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$bird_ID [1]))
									  
									  qqnorm(unlist(ranef(m)$bird_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$bird_ID[2]))
									  
									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))
									  
									  
									  scatter.smooth(resid(m)~f$day_j);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$call_int_c2);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~f$sex);abline(h=0, lty=2, col='red')
									  
									   mtext("lmer(call_int_c3 ~ sex + scale(call_int_c2) + scale(day_j) + (call_int_c2|bird_ID) + (1|nest_ID), f)", side = 3, line = 0.5, cex=0.8,outer = TRUE)
									   
									    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=f$lon, y=f$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
											
											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								
							dev.off()
					}
							  
		
	  }
	   }		
	  {# push offs
			
		{# distribution
		  	summary(factor(e$push))
			length(e$push[e$push=='1'])/length(e$push)
			table(e$push, e$sex)
				length(e$push[e$push==1 & e$sex=='f'])/length(e$push[e$sex=='f'])
				length(e$push[e$push==1 & e$sex=='m'])/length(e$push[e$sex=='m'])
				
				densityplot(~ex_$pushoff_int)
    			densityplot(~ex_$push)
    			
    	}		
    	 {# Supplementary Table 7
			{# prepare table data
					m=glmer(push ~ sex + scale(day_j)+scale(current_bout) + (scale(current_bout)|bird_ID) + (1|nest_ID), family='binomial',e)
							# binomial gives same results
								#dd_$pa_bin=ifelse(dd_$pa == 0.001, 0,1)
								#m = glmer(pa_bin~ sex*day_j+(day_j|nest_ID),family='binomial', dd_)
					pred=c('Intercept (f)','Day','Sex(m)', 'Current bout', 'Day:sex','Current bout:sex')
					dep = 'push'
					mod = 1
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,2)
						oi$lwr_r=round(oi$lwr,2)
						oi$upr_r=round(oi$upr,2)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o=rbind(oii,ri)
				}
			{# create xlsx table		
						
						sname = tempfile(fileext='.xls')
						wb = loadWorkbook(sname,create = TRUE)	
						createSheet(wb, name = "output")
						writeWorksheet(wb, o, sheet = "output")
						#createSheet(wb, name = "output_AIC")
						#writeWorksheet(wb, rbind(o), sheet = "output_AIC")
						saveWorkbook(wb)
						shell(sname)
				}
		  }
					{# model assumptions
							m=glmer(push ~ scale(day_j)*sex+scale(current_bout)*sex + (scale(current_bout)|bird_ID) + (1|nest_ID), family='binomial',e)
									#png(paste(out_,"model_ass/Supplementary_Table_6.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
									  qqline(resid(m))
									  
									  plot(fitted(m), jitter(e$push, amount=0.05), xlab="Fitted values", ylab="Probability of left", las=1, cex.lab=1.2, cex=0.8)
									  abline(0,1, lty=3)
									  t.breaks <- cut(fitted(m), quantile(fitted(m)))
									  means <- tapply(e$push, t.breaks, mean)
									  semean <- function(x) sd(x)/sqrt(length(x))
									  means.se <- tapply(e$push, t.breaks, semean)
									  points(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means, pch=16, col="orange")
									  segments(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means-2*means.se, quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means+2*means.se,lwd=2, col="orange")
									  
									  e$fit = fitted(m)
									  ee = e[e$fit<0.01,]
									  plot(ee$fit, jitter(ee$push, amount=0.05), xlab="Fitted values", ylab="Probability of left", las=1, cex.lab=1.2, cex=0.8)
									  abline(0,1, lty=3)
									  points(quantile(fitted(m),c(0.125,0.375,0.625,0.875))[1:3], means[1:3], pch=16, col="orange")
									  segments(quantile(fitted(m),c(0.125,0.375,0.625,0.875))[1:3], means[1:3]-2*means.se[1:3], quantile(fitted(m),c(0.125,0.375,0.625,0.875))[1:3], means[1:3]+2*means.se[1:3],lwd=2, col="orange")
									  
									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))
									  
									  #qqnorm(unlist(ranef(m)$nest_ID[2]), main = "ran slope",col='red')
									  #qqline(unlist(ranef(m)$nest_ID[2]))
									  
									  scatter.smooth(resid(m)~e$current_bout);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~e$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~e$sex);abline(h=0, lty=2, col='red')
									  
									  mtext("m=glmer(push ~ scale(day_j)*sex+scale(current_bout)*sex + (scale(current_bout)|bird_ID) + (1|nest_ID), family='binomial',e)", side = 3, line = 0.5, cex=0.8,outer = TRUE)
									   
									    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=e$lon, y=e$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
											
											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								
							dev.off()
					}
		 {# Figure xxx
		 
		 }
	  }
	  {# how leaving the nest? - create supplementary table
		  
  			summary(factor(dd$type_l))
  			summary(factor(dd$type_l[dd$cage=='n']))
  			dn=dd[dd$cage=='n',]
			length(dn$type_l[dn$type_l=='f'])/length(dn$type_l)
  			ggplot(dn,aes(y = current_bout, x=type_l))+geom_boxplot()
  			g
			
			# called while leaving?
				summary(factor(dn$call_left))
				summary(factor(dd$call_left))
				length(dd$call_left[dd$call_left=='y'])/length(dd$call_left[!is.na(dd$call_left)])
				table(dd$call_left,dd$sex)
				dc = dd[!is.na(dd$call_left),]
				dc$c_left = ifelse(dc$call_left=='y', 1,0)
				
				m = glmer(c_left ~ sex*scale(day_j) + (1|bird_ID) + (1|nest_ID), dc, family = 'binomial')
				m = glmer(c_left ~ sex + (1|bird_ID) + (1|nest_ID), dc, family = 'binomial')
				plot(allEffects(m))
				summary(glht(m))
		}

	  {#Relationship between durations and callings
		  
		  #a.	Duration of arrival minus leaving ~ push off or calling intensity
		  		ex_$both=as.numeric(difftime(ex_$dt_left,ex_$dt_arrive, 'secs')) 
				densityplot(~ex_$both)
				ex_[ex_$both<0,]
				eb = ex_[ex_$both>0,] 
				densityplot(log(eb$arrival-eb$gap))
				eb$
				
				
				
				
				m = lmer(both ~ pushoff_int + call_int_1 + (1|bird_ID) + (1|nest_ID), eb)
				#m = lmer(arrival ~ pushoff_int + call_int_1 + (1|bird_ID) + (1|nest_ID), eb)
				m = lmer(both ~ push + call_int_1 + (1|bird_ID) + (1|nest_ID), eb)
				m = lmer(both ~ sex*push + sex*call_int_1 + (1|bird_ID) + (1|nest_ID), eb)
				#m = lmer(arrival-gap ~ push + call_int_1 + (1|bird_ID) + (1|nest_ID), eb)
				
				
				plot(eb$arrival~eb$both)
				plot(allEffects(m))
				summary(glht(m))
				
				# for both as in Table 3#
					m = lmer(log(both) ~ sex*scale(day_j)+(scale(day_j)|bird_ID), eb)
					
		  #b.	How are leaving ~ arrival ~ first  presence related (see distributions as there might be no variation)		
		}
	}		 
}		
		 
	  
	{# OLD  
		
		{#2 calling - only from present until left - either use time_series.csv to derive or scored intensities from observations.csv
		  
		  #a.	what is the distribution of calling intensity in the three periods (both changeover, changeover, after changeover)
		  
		  ####DT
		  
        		  ##    Calling bouts
        		  
        		  ###   Description        
        		  
        		  # Calling bout 1
        		  stat.desc(dd$call_o_alone[!is.na(dd$call_o_alone)])
        		  length(dd$call_o_alone[!is.na(dd$call_o_alone)&dd$call_o_alone=="0"]) 
        		  length(dd$call_o_alone[is.na(dd$call_o_alone)]) 
        		  
        		  
        		  ###   Plots
        		  
        		  # Calling bout 1, number of calls
        		  barplot(table(dd$call_o_alone),main=("Calling bout 1"),xlab=("Number of calls"),ylab=("Number of observations"))
        		  
        		  # Calling bout 1, calling intensity
        		  barplot(table(dd$call_int_0),main=("Calling bout 1"),xlab=("Calling intensity"),ylab=("Number of observations"),ylim=c(0,100))
        		  
        		  #present-arrive
        		  par(mfrow=c(1,2))
        		  barplot(table(dd$call_c_pres_arrive),main=("a) coming bird"),xlab=("Calling intensity"),ylab=("Number of observations"),ylim=c(0,100),cex.lab=1.5,cex.names=1.5,cex.axis=1.5)
        		  barplot(table(dd$call_o_pres_arrive),main=("b) incubationg bird"),xlab=("Calling intensity"),ylim=c(0,100),cex.lab=1.5,cex.names=1.5,cex.axis=1.5)
        		  
        		  
        		  # Calling bout 2, intensity for coming, incubating and both
        		  par(mfrow=c(1,3))
        		  barplot(table(dd$call_int_1),main=("a) Calling bout 2"),xlab=("Calling intensity"),ylab=("Number of observations"),ylim=c(0,100),cex.lab=1.5,cex.names=1.5,cex.axis=1.5)
        		  barplot(table(dd$call_c_int),main=("b) Coming bird"),xlab=("Calling intensity"),ylim=c(0,100),cex.lab=1.5,cex.names=1.5,cex.axis=1.5)
        		  barplot(table(dd$call_o_int),main=("c) Incubating bird"),xlab=("Calling intensity"),ylim=c(0,100),cex.lab=1.5,cex.names=1.5,cex.axis=1.5)
        		  
        		  # Calling bout 3
        		  par(mfrow=c(1,1))
        		  barplot(table(dd$call_int_c2),main=("Calling bout 3"),xlab=("Calling intensity"),ylab=("Frequency"),ylim=c(0,100))
        		  
        		  # Calling bout 4
        		  barplot(table(dd$call_int_c3),main=("Calling bout 4"),xlab=("Calling intensity"),ylab=("Frequency"))
        		  
        		  # All calling bouts (Figure)
        		  par(mfrow=c(1,4))
        		  barplot(table(dd$call_int_0),main=("a) Calling bout 1"),xlab=("Calling intensity"),ylab=("Number of observations"),ylim=c(0,120),cex.lab=1.7,cex.names=1.7,cex.axis=1.7,cex.main=1.7)
        		  barplot(table(dd$call_int_1),main=("b) Calling bout 2"),xlab=("Calling intensity"),ylab=("Number of observations"),ylim=c(0,120),cex.lab=1.7,cex.names=1.7,cex.axis=1.7,cex.main=1.7)
        		  barplot(table(dd$call_int_c2),main=("c) Calling bout 3"),xlab=("Calling intensity"),ylab=("Number of observations"),ylim=c(0,120),cex.lab=1.7,cex.names=1.7,cex.axis=1.7,cex.main=1.7)
        		  barplot(table(dd$call_int_c3),main=("d) Calling bout 4"),xlab=("Calling intensity"),ylab=("Number of observations"),ylim=c(0,120),cex.lab=1.7,cex.names=1.7,cex.axis=1.7,cex.main=1.7)
        		  
        		  # Correlation of number of calls within a pair
        		  #restructure data to have means of the measurements for every individual
        		  mean.calls<-tapply(dd$call_o_alone,dd$bird_ID,mean);mean.calls
        		  dd_melt<-aggregate(dd,by=list(nest_ID=dd$nest_ID,sex=dd$sex),FUN=mean,na.rm=T)
        		  ggplot(data=dd_melt,aes(x=sex,y=call_o_alone,group=nest_ID))+geom_point()+geom_line() # Maybe not a good plot to visualize data in this case
        		  
        		  # Correlation, f on y-axis, m on x-axis
        		  par(mfrow=c(1,1))
        		  dd$bird_ID=tolower(dd$bird_ID)
        		  dd$nest_ID=tolower(dd$nest_ID)
        		  z=ddply(dd,.(year,nest_ID,sex,bird_ID), summarise, call_med=median(call_o_alone, na.rm=TRUE))
        		  zz=z[!z$nest_ID%in%z$nest_ID[is.na(z$call_med)],]
        		  zz=zz[zz$nest_ID%in%zz$nest_ID[zz$sex=='m'] ,]
        		  zz=zz[zz$nest_ID%in%zz$nest_ID[zz$sex=='f'] ,]
        		  plot(zz$call_med[zz$sex=='f' ]~zz$call_med[zz$sex=='m' ],
        		       ylim=c(0,15), xlim=c(0,15),xlab="\u2642 number of calls", ylab="\u2640 number of calls",
        		       pch=21,col=rgb(20,20,20,100,maxColorValue = 255),bg=rgb(100,100,100,100,maxColorValue = 255),cex=1)
        		  abline(a=0,b=1,lty=2)
        		  
        		  ###  Statistics - not sure which test to use here, maybe markov chain model? These models don't fit yet!
        		  
        		  # Is calling intensity of a given calling bout predicted by calling intensity of a previous bout?  
        		  #Coming bird: Calling bout 4 predicted by 3 or 2?
        		  b4=lmer(call_int_c3~call_int_c2+call_c_int+(1|year)+(1|nest_ID)+(1|bird_ID),data=dd)
        		  plot(b4)
        		  b4=lmer(call_int_c3~call_int_c2+call_c_int+(1|year)+(1|nest_ID)+(1|bird_ID),REML=F,data=dd)
        		  b4.null.c2=lmer(call_int_c3~call_int_c2+(1|year)+(1|nest_ID)+(1|bird_ID),REML=F,data=dd)
        		  #anova(b4.null.c2,b4) #not working, not same size of dataset
        		  AIC(b4.null.c2,b4) # delta>2 -> significant difference! Warning: models are not all fitted to the same number of observations
        		  b4.null.c=lmer(call_int_c3~call_c_int+(1|year)+(1|nest_ID)+(1|bird_ID),REML=F,data=dd)
        		  #anova(b4.null.c,b4) # calling bout 4 depends on calling of coming bird during calling bout 2 (Chisq=4.25,p=0.0323)
        		  AIC(b4.null.c,b4) # delta>2 -> significant difference!
        		  b4.int=lmer(call_int_c3~call_int_c2*call_c_int+(1|year)+(1|nest_ID)+(1|bird_ID),  data=dd)
        		  plot(b4.int)
        		  summary(b4.int)
        		  b4.int=lmer(call_int_c3~call_int_c2*call_c_int+(1|year)+(1|nest_ID)+(1|bird_ID),data=dd)
        		  summary(b4.int)
        		  
        		  #Incubating bird: Calling bout 2 predicted by 1?
        		  b2=lmer(call_o_int~call_int_0+(1|year)+(1|nest_ID)+(1|bird_ID),  data=dd)
        		  plot(b2)
        		  summary(b2)
        		  b2=lmer(call_o_int~call_int_0+(1|year)+(1|nest_ID)+(1|bird_ID),REML=F,  data=dd)
        		  b2.null=lmer(call_o_int~1+(1|year)+(1|nest_ID)+(1|bird_ID),REML=F,  data=dd)
        		  #anova(b2.null,b2) #does not work, not same size of dataset
        		  AIC(b2.null,b2)# delta>2 -> significant difference! Warning: models are not all fitted to the same number of observations
        		  mb2<-aov(call_o_alone~call_o_int*bird_ID*nest_ID,data=dd)
        		  plot(mb2)
        		  summary(mb2)
		  }
		  ####
		   #b.	what is the exchange procedure (graph with call intensity on Y and calling bout on x-axis; lines show various strategies, and N next to the lines number of exchanges with such strategy
		  
		  #c.	are the calling intensities related to sex, incubation period, length of current and next incubation bout
		  
      ####DT
      
                {# what predicts calling intensity during exchange
                {# when on-nest bird allone (i.e. before presence)
                {#run first 
                  dd=d[d$type=='ex' ,]
                  dd$left_bin=ifelse(dd$left_before_presence=='y',1,0)
                  b_ = b[dt_behaviour<=end_pr]
                  bb=b_[b$obs_ID%in%dd$obs_ID,]	
                  bb_=ddply(b,.(obs_ID, nest_ID, sex), summarise, call_i=length(behaviour[which(behaviour=='c' & who=='o')]),fly_i=length(behaviour[which(behaviour=='f' & who=='o')]))
                  dd$call_i=bb_$call_i[match(dd$obs_ID, bb_$obs_ID)]
                  dd$fly_i=bb_$fly_i[match(dd$obs_ID, bb_$obs_ID)]
                  dd$fly=as.factor(ifelse(dd$fly_i==0,'n','y'))
                }
                {#MIHAI HELP
                  plot(next_bout~current_bout,dd)
                  cor.test(dd$next_bout,dd$current_bout)
                  m = glmer(call_i~sex+scale(current_bout)+scale(day_j)+(current_bout|nest_ID), dd, family='poisson') #### CONTROL 
                  plot(allEffects(m))
                  summary(glht(m))
                }
                }
                {# when boht there but exchange has not started
                {#run first 
                  dd=d[d$type=='ex' ,]
                  b_ = b[dt_behaviour>end_pr]
                  bb=b_[b$obs_ID%in%dd$obs_ID,]	
                  
                  bb_=ddply(b,.(obs_ID, nest_ID, sex), summarise, call_i=length(behaviour[which(behaviour=='c' & who=='o')]),fly_i=length(behaviour[which(behaviour=='f' & who=='o')])) # on bird
                  dd$call_i=bb_$call_i[match(dd$obs_ID, bb_$obs_ID)]
                  dd$fly_i=bb_$fly_i[match(dd$obs_ID, bb_$obs_ID)]
                  dd$fly=as.factor(ifelse(dd$fly_i==0,'n','y'))
                  
                  bb_=ddply(b,.(obs_ID, nest_ID, day_j, sex, who), summarise, call_i=length(behaviour[which(behaviour=='c')]),fly_i=length(behaviour[which(behaviour=='f' & who=='o')])) # both
                  bb_$current_bout=dd$current_bout[match(bb_$obs_ID,dd$obs_ID)]
                  bb_$next_bout=dd$next_bout[match(bb_$obs_ID,dd$obs_ID)]
                  bb_$sex=as.factor(bb_$sex)
                  bb_$who=as.factor(bb_$who)
                }
                {# only on bird MIHAI HELP
                  plot(next_bout~current_bout,dd)
                  cor.test(dd$next_bout,dd$current_bout)
                  m = glmer(call_i~sex+scale(current_bout)+scale(next_bout)*who+scale(day_j)+(current_bout|nest_ID), dd, family='poisson') #### CONTROL 
                  plot(allEffects(m))
                  summary(glht(m))
                  
                }
                {# both birds
                  m = glmer(call_i~sex+scale(current_bout)+scale(next_bout)*who+scale(day_j)+(current_bout|nest_ID), bb_, family='poisson') #### CONTROL 
                  m = glmer(call_i~who+(1|nest_ID), bb_, family='poisson') #### CONTROL 
                  plot(allEffects(m))
                  summary(glht(m))
                }
                }
		####
		
                }
		  
		  
		  
		{#3 push off
		  
		  
    			
		}
		  
		  
		{#4 Relationship between durations and callings
		  
		  #a.	Duration of arrival minus leaving ~ push off or calling intensity
		  
				dd$both=as.numeric(dd$dt_left-dd$dt_arrive) 
		  
		  #b.	How are leaving ~ arrival ~ first  presence related (see distributions as there might be no variation)
		  
		  
		  
		}
	
	}

	

{# DONE
	{# prepare nests - start/end
	load("C:\\Users\\mbulla\\Documents\\Dropbox\\Science\\Projects\\MS\\exchanges_SESA\\stats\\exchanges_with-bird_ID-nest_state-inc_start_2016-04-21.Rdata")
	bb=unique(b[,c('year', 'nest', 'inc_start','hatch_start','end_state','end_datetime')])
	write.csv(bb, file=paste(wd,'nests.csv', sep=''), row.names=FALSE)	
	}
	{# prepare bird_IDs
		# establish database connections
		  require('RMySQL')
		  con=dbConnect(MySQL(),user='root',host='127.0.0.1', password='',dbname='')
		  dbq=dbGetQuery
		
		# load nests
			nn= dbq(con,"SELECT*FROM avesatbarrow.nests  where species = 'SESA' and year_ in ('2011','2012','2013')")
			nn=nn[tolower(paste(nn$nest,nn$year_))%in%tolower(paste(n$nest,n$year)),]
			
			nrow(nn)
			nrow(n) #n[!(tolower(paste(n$nest,n$year))%in%tolower(paste(nn$nest,nn$year_))),]
			
			n1=nn
			n2=nn
			n1$bird_ID=n1$IDfemale
			n1$sex="f"
			n2$bird_ID=n2$IDmale
			n2$sex="m"
			x=rbind(n1,n2)
				s= dbq(con,"SELECT*FROM avesatbarrow.sex")
				x$sex_g=s$sex[match(x$bird_ID, s$ID)]
				x$sex_g=ifelse(x$sex_g==1,"m","f")
								
				x[x$sex_g!=x$sex,]
			x$year=x$year_	
			
			x=x[,c('year', 'nest', 'sex','bird_ID')]
			x=x[order(x$year,x$nest,x$sex),]
			x$nest=tolower(x$nest)
			x=x[-which(x$nest=='s608'),]
			write.csv(x, file=paste(wd,'birds.csv', sep=''), row.names=FALSE)	
		
	}
	{# check whether bird was recorded on multiple nests/years
			bb<-read.csv(file=paste(wd, "birds.csv", sep=''),header=T,sep=",", fill=T, stringsAsFactors=FALSE)
			
			d$bird_ID=bb$bird_ID[match(paste(d$nest, d$year, d$sex), paste(bb$nest,bb$year, bb$sex))]
			dd=distinct(d[,c('year',
			table(d$bird_ID[d$year==2013], d$nest[d$year==2013])
}	
	{# check current/next_bout_bias	
		d[d$obs_ID%in%e$obs_ID[which(e$current_bout_biased=='y')],c('obs_ID','nest','current_bout')]
		d[d$obs_ID%in%e$obs_ID[which(e$next_bout_biased=='y')],c('obs_ID','nest','next_bout')]
	}
}
	