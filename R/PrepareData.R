# TO DO 


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

{# TOOLS
	{# define datetime
		Sys.setenv(TZ="UTC")	
	}
	
	{# define working directory
	     wd = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/exchanges_SESA/Database/"	
	     outdir = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/exchanges_SESA/Analyses/"	
       
       ####DT
	           wd = "C:/Users/OWNER/Dropbox/exchanges_SESA/Database/"	
	     ####
	     
       
	}
	
	{# load packages
		require(arm)
		require(effects)
		require(ggplot2)
		require(grid)
		require(multcomp)
		require(plyr)
		require(raster)
		require(lattice)
		require(xlsx)
   
    ####DT
          require(data.table)
          require(magrittr)
	        require(pastecs)
    ####
	}
	
	{# define constants
		varnames = c("tag", "datetime_", "x", "y","z", "temp", "batt")
		
		min_ = -0.1
		max_ = 6
		
		wr_col="grey50"
		ln_col="grey80"
		disturb='#5eab2b'
		cv_x = "#99c978"
		cv_y = "#f0b2b2"
		cv_z = "#ADD8E6"
		tem = 'dodgerblue'
	}
	
	{# load functions
	
		
	}
}	

{# LOAD data
	   # nest metadata
			n<-read.csv(file=paste(wd, "nests.csv", sep=''),header=T,sep=",", fill=T, stringsAsFactors=FALSE)
			n$inc_start <- as.POSIXct(n$inc_start,format='%Y-%m-%d %H:%M:%S')
			n$hatch_start <- as.POSIXct(n$hatch_start,format='%Y-%m-%d %H:%M:%S')
			n$end_datetime <- as.POSIXct(n$end_datetime,format='%Y-%m-%d %H:%M:%S')
			n$end_=as.POSIXct(ifelse(!is.na(n$hatch_start),as.character(n$hatch_start-6*60*60), 
							ifelse(n$end_state%in%c('fl','hd'), as.character(n$end_datetime-12*60*60), as.character(n$end_datetime))))
      
	   # exclusions
			e<-read.csv(file=paste(wd, "exclusions.csv", sep=''),header=T,sep=",", fill=T, stringsAsFactors=FALSE)
			e$nest = tolower(e$nest)
      

	   # observations
			d<-read.csv(file=paste(wd, "observations.csv", sep=''),header=T,sep=",", fill=T, stringsAsFactors=FALSE)
			d=d[!is.na(d$obs_ID),]
			d$nest = tolower(d$nest)
			d$reality_dt <- as.POSIXct(d$reality_dt,format='%Y-%b-%d %H:%M:%S')
			d$video_dt <- as.POSIXct(d$video_dt,format='%Y-%b-%d %H:%M:%S')
			
			d$t_delta=as.numeric(difftime(d$reality_dt,d$video_dt, units="secs"))
			d$t_delta=ifelse(is.na(d$t_delta),0,d$t_delta)
			
			d$dt_on = as.POSIXct(d$dt_on,format='%Y-%b-%d %H:%M:%S') +d$t_delta
			d$dt_left <- as.POSIXct(d$dt_left,format='%Y-%b-%d %H:%M:%S') +d$t_delta
			d$dt_1st_presence <- as.POSIXct(d$dt_1st_presence,format='%Y-%b-%d %H:%M:%S') +d$t_delta
			d$dt_arrive <- as.POSIXct(d$dt_arrive,format='%Y-%b-%d %H:%M:%S')+d$t_delta
			d$dt_1st_call <- as.POSIXct(d$dt_1st_call,format='%Y-%b-%d %H:%M:%S')+d$t_delta
			d$dt_video <- as.POSIXct(d$start_time_video,format='%Y-%b-%d %H:%M:%S')+d$t_delta
			d$inc_start=n$inc_start[match(paste(d$nest, d$year), paste(n$nest,n$year))]
			d$end_=n$end_[match(paste(d$nest, d$year), paste(n$nest,n$year))]
			d$nest_ID=paste(d$nest,d$year)
			d$sex=as.factor(d$sex)
			d$day_j = as.numeric(format(d$dt_on ,"%j")) - as.numeric(format(d$inc_start,"%j"))+1
			d$presence=as.numeric(d$dt_left-d$dt_1st_presence) 
			
			dd=d[d$type=='ex',]
      
      
	   # time_series
			b<-read.csv(file=paste(wd, "time_series.csv", sep=''),header=T,sep=",", fill=T, stringsAsFactors=FALSE)
			b=b[!is.na(b$obs_ID),]
			b$nest = tolower(b$nest)
			b$t_delta=d$t_delta[match(b$obs_ID, d$obs_ID)]
			b$dt_behaviour <- as.POSIXct(b$dt_behaviour,format='%Y-%b-%d %H:%M:%S')+b$t_delta
			b$t_delta=NULL
			b$inc_start=n$inc_start[match(toupper(paste(b$nest, b$year)), toupper(paste(n$nest,n$year)))]
			b$end_=n$end_[match(toupper(paste(b$nest, b$year)), toupper(paste(n$nest,n$year)))]
			b$nest_ID=paste(b$nest,b$year)
      
      
      ####DT
      
      			# time_series
      			b<-read.csv(file=paste(wd, "time_series.csv", sep=''),header=T,sep=",", fill=T, stringsAsFactors=FALSE)
      			b=b[!is.na(b$obs_ID),]
      			b$nest = tolower(b$nest)
      			b$t_delta=d$t_delta[match(b$obs_ID, d$obs_ID)]
      			b$dt_behaviour <- as.POSIXct(b$dt_behaviour,format='%Y-%b-%d %H:%M:%S')+b$t_delta
      			b$end_dt = as.POSIXct(b$end_dt,format='%Y-%b-%d %H:%M:%S')+b$t_delta
      			b$end_pr = d$dt_1st_presence[match(b$obs_ID,d$obs_ID)]
      			b$end_pr=as.POSIXct(ifelse(is.na(b$end_pr), as.character(b$end_dt), as.character(b$end_pr)))
      			# end of exchange observation before bird is present
      			
      			b$inc_start=n$inc_start[match(toupper(paste(b$nest, b$year)), toupper(paste(n$nest,n$year)))]
      			b$end_=n$end_[match(toupper(paste(b$nest, b$year)), toupper(paste(n$nest,n$year)))]
      			b$nest_ID=paste(b$nest,b$year)
      			b$type = as.factor(b$type)
      			
      			b$inc_start=n$inc_start[match(paste(b$nest, b$year), paste(n$nest,n$year))]
      			b$day_j = as.numeric(format(b$dt_behaviour ,"%j")) - as.numeric(format(b$inc_start,"%j"))+1
      
     ####
      
      
	   # observations excluded from all analyses
			d=d[!d$obs_ID%in%e$obs_ID[which(e$total_exclusion=='y')],]
			d=d[!d$obs_ID%in%e$obs_ID[which(e$close_to_exchange=="y")],]
			
			b=b[!b$obs_ID%in%e$obs_ID[which(e$total_exclusion=='y')],]
			b=b[!b$obs_ID%in%e$obs_ID[which(e$close_to_exchange=="y")],]	
		
		# no nests excluded because of inc start or end date ( two nests have observations toward hatching, but we keep those for now)
				#d$check=as.POSIXct(ifelse(is.na(d$dt_on), as.character(d$dt_video),as.character(d$dt_on))) 
				#d=d[-which(d$check>d$inc_start & d$check<d$end_),] # two nests have observations toward hatching, but we keep those for now

     ####DT
		
            b = data.table(b)
        		d = data.table(d)
    
     ####
}


{# RESULTS
	{# behaviour prior to return 
		{# run first 
			dd=d[d$type=='ex',] ####DT already defined in LOAD DATA, we don't need it here anymore
			dd$left_bin=ifelse(dd$left_before_presence=='y',1,0) ####DT This should also be added to LOAD DATA
		}
		{# stats # control for capture
		  
		  
		  #1a.	Do the incubating birds call and fly off more prior to exchange than during regular incubation bout 
		  
				  	bb_=ddply(b,.(obs_ID, nest_ID, sex, type), summarise, call_i=length(behaviour[which(behaviour=='c' & who=='o')]),fly_i=length(behaviour[which(behaviour=='f' & who=='o')]))
					
          ####DT
          
      					b_ = b[dt_behaviour<=end_pr] # only observations before a coming bird was present
      					bb_=ddply(b_[!is.na(b_$sex),],.(obs_ID, nest_ID, sex, type), summarise, call_i=length(behaviour[which(behaviour=='c' & who=='o')]),fly_i=length(behaviour[which(behaviour=='f' & who=='o')]))
      					bb_$dt_video=d$dt_video[match(bb_$obs_ID,d$obs_ID)]
      					bb_$hour= as.numeric(difftime(bb_$dt_video, trunc(bb_$dt_video,"day"), units = "hours"))
      					bb_$rad=as.numeric(bb_$hour)*pi/12
          
          ####
          
            ggplot(bb_,aes(x=type, y=call_i))+geom_boxplot()
  					ggplot(bb_,aes(x=call_i, col=type))+geom_density()
  					ggplot(bb_,aes(x=type, y=fly_i))+geom_boxplot()
  					ggplot(bb_,aes(x=fly_i, col=type))+geom_density()
            
          ####DT
          
      					# calling
      					ggplot(bb_,aes(x=type, y=call_i))+geom_boxplot()
      					ggplot(bb_,aes(x=nest_ID, y=call_i, fill=type))+geom_boxplot()
      					ggplot(bb_,aes(x=type, y=call_i, col=sex))+geom_boxplot() #### how to order this quickly
      					ggplot(bb_,aes(x=call_i))+geom_histogram()
      					
      					fm= glmer(call_i ~ type + (1|nest_ID), family = poisson,  bb_)
      					#fm= glmer(call_i ~ type*sin(rad)+type*cos(rad)+ (1|nest_ID), family = poisson,  bb_)
      					#fm= glmer(N ~ type + (1|nest_ID), family = poisson,  x[x$behaviour %in% c('n', 'c'),])
      					summary(fm)
      					summary(glht(fm))
      					plot(allEffects(fm))
          
      					# fly_off - NOTHING
      					ggplot(bb_,aes(x=type, y=fly_i))+geom_boxplot()
      					ggplot(bb_,aes(x=type, y=fly_i, col=sex))+geom_boxplot()
      					ggplot(bb_,aes(x=fly_i))+geom_histogram()
      					
      					fm= glmer(fly_i ~ type + (1|nest_ID), family = poisson,  bb_)
      					#fm= glmer(N ~ type + (1|nest_ID), family = poisson,  x[x$behaviour %in% c('n', 'c'),])
      					summary(fm)
      					plot(allEffects(fm))
          
          ####
					
					# TIME SERIES
					
					
								
			#1b. 	If so is this sex specific and related to time before actual exchange (i.e. does the probability of calling increase as leaving/exchange is nearing)? 
					  
          ####DT
          			# ex -non ex time difference from presence - only for those where calling occured
          			b[, deltaT := difftime(end_pr, dt_behaviour, units = 'mins')%>% as.integer]
          			b_ = b[dt_behaviour<=end_pr] # only observations before a coming bird was present
          			b_$hour= as.numeric(difftime(b_$dt_behaviour, trunc(b_$dt_behaviour,"day"), units = "hours"))
          			b_$rad=as.numeric(b_$hour)*pi/12
          			#h$sin_=sin(h$rad)
          			#h$cos_=cos(h$rad)
          			
                # calling
          			hist(b_$deltaT[b_$behaviour == 'c' & b_$who == 'o'])
          			fm= lmer(deltaT ~ type + (1|nest_ID) , b_[b_$behaviour == 'c' & b_$who == 'o',])
          			fm= lmer(deltaT ~ type+sin(rad)+type+cos(rad)+(1|nest_ID) , b_[b_$behaviour == 'c' & b_$who == 'o',])
          			fm= lmer(deltaT ~ type*sin(rad)+type*cos(rad)+(1|nest_ID) , b_[b_$behaviour == 'c' & b_$who == 'o',])
          			#fm= lmer(deltaT ~ type*scale(day_j)+ (1|nest_ID) , b_[b_$behaviour == 'c' & b_$who == 'o',])
          			summary(fm)
          			glht(fm) %>% summary
          			plot(allEffects(fm))
          			
          			ggplot(b_[b_$behaviour == 'c' & b_$who == 'o',], aes(x=type, y=deltaT, col=type))+geom_boxplot()
          			# distribution over time
          			ggplot(b_[b_$behaviour == 'c' & b_$who == 'o',], aes(x=deltaT, col=type))+geom_density()
          			ggplot(b[b$behaviour == 'c' & b$who == 'o',], aes(x=deltaT, col=type))+geom_density() +geom_vline(xintercept = 0)
          			ggplot(b[b$behaviour == 'c' & b$who == 'o',], aes(x=deltaT, fill=type))+geom_histogram(position="dodge" ,  alpha=0.4) +geom_vline(xintercept = 0)
          			
          			# fly_off
          			hist(b_$deltaT[b_$behaviour == 'f' & b_$who == 'o'])
          			fm= lmer(deltaT ~ type + (1|nest_ID) , b_[b_$behaviour == 'f' & b_$who == 'o',])
          			#fm= lmer(deltaT ~ type*scale(day_j)++ (1|nest_ID) , b_[b_$behaviour == 'f' & b_$who == 'o',])
          			summary(fm)
          			glht(fm) %>% summary
          			plot(allEffects(fm))
          			
          			ggplot(b_[b_$behaviour == 'f' & b_$who == 'o',], aes(x=type, y=deltaT, col=type))+geom_boxplot()
          			# distribution over time
          			ggplot(b_[which(b_$behaviour == 'f' & b_$who == 'o'),], aes(x=deltaT, col=type))+geom_density()
          			ggplot(b[b$behaviour == 'f' & b$who == 'o',], aes(x=deltaT, fill=type))+geom_histogram(position="dodge" ,  alpha=0.4) +geom_vline(xintercept = 0)
        
          ####
								
  					#	Figure boxplot of call intensities/fly offs for each nests prior to exchange and during regular incubation; order based on exchange intensities from most to least
  					ggplot(bb_,aes(x=nest_ID, y=call_i, col=type))+geom_boxplot()
  					ggplot(bb_,aes(x=nest_ID, y=fly_i, col=type))+geom_boxplot()
  					# show it in non ex plot connecting medians of the two and having circel size indicating sample size
  					#	Sex difference mentioned only in the text
  					#	The time difference also mentioned only in the text (but check whether the probability of calling with time is not specific to whether the bird left before partner was present)
						
  					
  					
			#2.  Proportion of nests (also according to sex) where incubating bird leaves before its partner is present (description in the text, if space allows, figure)
					
  					length(dd$left_before_presence[dd$left_before_presence=='y'])/nrow(dd)
					  summary(factor(dd$left_before_presence)) 
					  table(dd$sex,dd$left_before_presence)
						
						m=glmer(left_bin~sex+(1|nest_ID),dd, family='binomial')
						plot(allEffects(m))
						summary(glht(m))
						
						dd$n=1
						x=ddply(dd,. (nest_ID,sex),summarise, p=sum(left_bin)/ sum(n), n=sum(n))
						densityplot(~x$p)
						
				####DT
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
				####		
     
				
						
			#3.	Can we explain why some birds leave before presence by different calling intensity, fly offs or current bout length?
				
						bb=b[b$obs_ID%in%dd$obs_ID,]	
				    bb_=ddply(b,.(obs_ID, nest_ID, sex), summarise, call_i=length(behaviour[which(behaviour=='c' & who=='o')]),fly_i=length(behaviour[which(behaviour=='f' & who=='o')]))
			    	dd$call_i=bb_$call_i[match(dd$obs_ID, bb_$obs_ID)]
			    	dd$fly_i=bb_$fly_i[match(dd$obs_ID, bb_$obs_ID)]
			    	dd$fly=as.factor(ifelse(dd$fly_i==0,'n','y'))
				
  					densityplot(bb_$call_i)
  					densityplot(bb_$fly_i)
  					densityplot(dd$current_bout)
  				
  					densityplot(~dd$call_i, groups=dd$left_bin, auto.key=TRUE)			
  					densityplot(~dd$fly_i, groups=dd$left_bin, auto.key=TRUE)			
  					densityplot(~dd$current_bout, groups=dd$left_bin, auto.key=TRUE)			
				
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
  						
  																				
  					m = glmer(left_bin~fly+scale(call_i)+scale(current_bout)+(1|nest_ID), dd, family='binomial') #### CONTROL FOR REMOVAL exPER???
  					plot(allEffects(m))
  					summary(glht(m))
					
				  	# model ass
      
				####DT
      
              {# left before presence 
              {# run first 
                dd=d[d$type=='ex' ,]
                dd$left_bin=ifelse(dd$left_before_presence=='y',1,0)
                bb=b[b$obs_ID%in%dd$obs_ID,]	
                bb_=ddply(b,.(obs_ID, nest_ID, sex), summarise, call_i=length(behaviour[which(behaviour=='c' & who=='o')]),fly_i=length(behaviour[which(behaviour=='f' & who=='o')]))
                dd$call_i=bb_$call_i[match(dd$obs_ID, bb_$obs_ID)]
                dd$fly_i=bb_$fly_i[match(dd$obs_ID, bb_$obs_ID)]
                dd$fly=as.factor(ifelse(dd$fly_i==0,'n','y'))
              }
              {#	stats
                m = glmer(left_bin~fly+scale(call_i)+scale(current_bout)+(1|nest_ID), dd, family='binomial') #### CONTROL FOR REMOVAL exPER???
                plot(allEffects(m))
                summary(glht(m))
                
                #Description
                
                  # How long before presence did birds leave?
                  stat.desc(dd$presence[dd$left_bin==0])/60
                  
                  # How long were birds present before partner left?
                  stat.desc(dd$presence[dd$left_bin==1])/60
          
                #Plots 
                  
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
                  
                #Statistics - both not working, don't know why
                  
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
                  
                  
                  
           ####        
                  
              
                
		}
	}
	
	{# exchange procedure - DECIDE WHETHER TO INVOLVE DATETIME LEFT
		
      	 {# run first
      			
      		dd=d[d$type=='ex',]
      			
    			dd$presence=as.numeric(dd$dt_on-dd$dt_1st_presence)  # how long before the bird sits down on nest is he present)
    			dd$arrival=as.numeric(dd$dt_on-dd$dt_arrive)  # how long before the bird sits down on nest is he close by / anters the picutre (i.e. start the exchange))
      			
    			### dd$presence2=as.numeric(dd$dt_left-dd$dt_1st_presence)
    			dd$gap=as.numeric(dd$dt_on-dd$dt_left)  # 
      		densityplot(~log(dd$gap))
      		densityplot(~dd$gap[dd$gap<60])
    			length(dd$gap[dd$gap>60])/nrow(dd)
      			
      	 }
	  
	  
		{#1 durations
		  
		  # -first presence
		  # -arrival
		  # -leaving
		  # -exchange gap
		  
		  ####DT
      		  require(pastecs)
      		  stat.desc(dd$gap[dd$gap])
		  ####
		  
		  #1b.are they related to sex or incubation period? 
		  
		  ####DT
      		  # Do sexes differ in gap length?
      		  
      		  library(lme4)
      		  library(lmerTest)
      		  m2=lmer(rank(gap)~as.factor(sex)+as.factor(cage)+(1|year)+(1|bird_ID)+(1|nest_ID),data=dd) # default REML=T for model interpretation
      		  plot(m2)
      		  summary(m2)
      		  summary(glht(m2))
      		  
      		  m2=lmer(rank(gap)~as.factor(sex)+(1|cage)+(1|year)+(1|nest_ID)+(1|bird_ID),REML=F,data=dd) #have to set REML=F for comparing the two models
      		  m3=lmer(rank(gap)~1+(1|cage)+(1|year)+(1|nest_ID)+(1|bird_ID), REML=F, data=dd)
      		  AIC(m2,m3) #better use AIC instead of anova, AIC is more conservative. A difference in AIC greater than 2 (2.5 or 3 or even higher, if want to be very conservative) is significant. 
      		  anova(m2,m3)
		  
			
		}
	  
	  
		
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
        		  data$bird_ID=tolower(data$bird_ID)
        		  data$nest_ID=tolower(data$nest_ID)
        		  z=ddply(data,.(year,nest_ID,sex,bird_ID), summarise, call_med=median(call_o_alone, na.rm=TRUE))
        		  zz=z[!z$nest_ID%in%z$nest_ID[is.na(z$call_med)],]
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
		  
		  #a.	What is the distribution of push-offs?
		  
    			densityplot(~d$pushoff_int)
    			dd$push=ifelse(is.na(dd$pushoff_int), NA, ifelse(dd$pushoff_int==3, 1,0))
    			m=glmer(push~scale(current_bout)*sex + (1|nest_ID), family='binomial',dd)
    			m=glmer(push~sex + (1|nest_ID), family='binomial',dd)
    			plot(allEffects(m))
    			summary(glht(m))
    			
    	#b.	Are they correlated with 
    			
    			#i.	calling intensity (if yes, push offs likely reflect calling intensity)
    			#ii.	length of current and next incubation bout
    			#iii.	calling intensity in present period
    			#iv.	incubation period or sex
    			
    			
		}
		  
		  
		{#4 Relationship between durations and callings
		  
		  #a.	Duration of arrival minus leaving ~ push off or calling intensity
		  
				dd$both=as.numeric(dd$dt_left-dd$dt_arrive) 
		  
		  #b.	How are leaving ~ arrival ~ first  presence related (see distributions as there might be no variation)
		  
		  
		  
		}
		{#5 How leaving the nest?
		  
  			summary(factor(dd$type_l))
  			summary(factor(dd$type_l[dd$cage=='n']))
  			dn=dd[dd$cage=='n',]
  			ggplot(dn,aes(y = current_bout, x=type_l))+geom_boxplot()
			
		}
	
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
	