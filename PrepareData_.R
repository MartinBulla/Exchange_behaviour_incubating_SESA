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
	     wd = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Exchanges_SESA/Database/"	
	     outdir = "C:/Users/mbulla/Documents/Dropbox/Science/Projects/MS/Exchanges_SESA/Analyses/"	
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
	
	   # observations excluded from all analyses
			d=d[!d$obs_ID%in%e$obs_ID[which(e$total_exclusion=='y')],]
			d=d[!d$obs_ID%in%e$obs_ID[which(e$close_to_exchange=="y")],]
			
			b=b[!b$obs_ID%in%e$obs_ID[which(e$total_exclusion=='y')],]
			b=b[!b$obs_ID%in%e$obs_ID[which(e$close_to_exchange=="y")],]	
		
		# no nests excluded because of inc start or end date ( two nests have observations toward hatching, but we keep those for now)
				#d$check=as.POSIXct(ifelse(is.na(d$dt_on), as.character(d$dt_video),as.character(d$dt_on))) 
				#d=d[-which(d$check>d$inc_start & d$check<d$end_),] # two nests have observations toward hatching, but we keep those for now
}


{# RESULTS
	{# behaviour prior to return 
		{# run first 
			dd=d[d$type=='ex',]
			dd$left_bin=ifelse(dd$left_before_presence=='y',1,0)
		}
		{# stats # control for capture
			#1a.	Do the incubating birds call and fly off more prior to exchange than during regular incubation bout 
					bb_=ddply(b,.(obs_ID, nest_ID, sex, type), summarise, call_i=length(behaviour[which(behaviour=='c' & who=='o')]),fly_i=length(behaviour[which(behaviour=='f' & who=='o')]))
					ggplot(bb_,aes(x=type, y=call_i))+geom_boxplot()
					ggplot(bb_,aes(x=call_i, col=type))+geom_density()
					ggplot(bb_,aes(x=type, y=fly_i))+geom_boxplot()
					ggplot(bb_,aes(x=fly_i, col=type))+geom_density()
					
					# TIME SERIES
					
					
								
			#1b. 	If so is this sex specific and related to time before actual exchange (i.e. does the probability of calling increases as leaving/exchange is nearing)? 
								
								
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
					ex_<-subset(dd,dd$left_before_presence=="n")
					ggplot(ex_,aes(x=presence, fill=sex))+geom_histogram(position="dodge" ,  alpha=0.4)+scale_x_log10(breaks = c(1,5,10,20,60,120,1000))
					ggplot(ex_,aes(x=presence, fill=sex))+geom_histogram(position="dodge" ,  alpha=0.4)+scale_x_log10(breaks = c(1,5,10,20,60,120,1000))
					ggplot(ex_,aes(x=day_j, y=presence, col=sex))+geom_point()+stat_smooth()+scale_y_log10(breaks = c(1,5,10,20,60,120,1000))
					
					
					table(dd$fly, dd$left_before_presence)
						
					ggplot(dd,aes(x=current_bout, y=left_bin))+geom_point()+stat_smooth()
					ggplot(dd,aes(x=current_bout, y=left_bin))+geom_point()+stat_smooth(method='lm')
					ggplot(dd,aes(x=call_i, y=left_bin))+geom_point()+stat_smooth()
						
																				
					m = glmer(left_bin~fly+scale(call_i)+scale(current_bout)+(1|nest_ID), dd, family='binomial') #### CONTROL FOR REMOVAL EXPER???
					plot(allEffects(m))
					summary(glht(m))
					
					# model ass
				
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
			
		}
		
		{#2 calling - only from present until left - either use time_series.csv to derive or scored intensities from observations.csv
		}
		{#3 push off
			densityplot(~d$pushoff_int)
			dd$push=ifelse(is.na(dd$pushoff_int), NA, ifelse(dd$pushoff_int==3, 1,0))
			m=glmer(push~scale(current_bout)*sex + (1|nest_ID), family='binomial',dd)
			m=glmer(push~sex + (1|nest_ID), family='binomial',dd)
			plot(allEffects(m))
			summary(glht(m))
		}
		{#4 Relationship between durations and callings
				dd$both=as.numeric(dd$dt_left-dd$dt_arrive)  
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
	load("C:\\Users\\mbulla\\Documents\\Dropbox\\Science\\Projects\\MS\\Exchanges_SESA\\stats\\exchanges_with-bird_ID-nest_state-inc_start_2016-04-21.Rdata")
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
	{# prepare lat/long
		# establish database connections
		  require('RMySQL')
		  con=dbConnect(MySQL(),user='root',host='127.0.0.1', password='',dbname='')
		  dbq=dbGetQuery
		
			nn= dbq(con,"SELECT year_, nest, latit, longit FROM avesatbarrow.nests  where species = 'SESA' and year_ in ('2011','2012','2013')")
		
			n<-read.csv(file=paste(wd, "nests.csv", sep=''),header=T,sep=",", fill=T, stringsAsFactors=FALSE)
			nrow(n)
			n$lat = nn$latit[match(tolower(paste(n$year, n$nest)), tolower(paste(nn$year_,nn$nest)))]
			n$lon = nn$longit[match(tolower(paste(n$year, n$nest)), tolower(paste(nn$year_,nn$nest)))]
			nrow(n)
			summary(n)
			write.csv(n, file=paste(wd,'nests.csv', sep=''), row.names=FALSE)	
			
			
	}	
	{# check current/next_bout_bias	
		d[d$obs_ID%in%e$obs_ID[which(e$current_bout_biased=='y')],c('obs_ID','nest','current_bout')]
		d[d$obs_ID%in%e$obs_ID[which(e$next_bout_biased=='y')],c('obs_ID','nest','next_bout')]
	}
}
	