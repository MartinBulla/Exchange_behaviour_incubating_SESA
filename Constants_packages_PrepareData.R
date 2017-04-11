{# TOOLS
	{# define datetime
		Sys.setenv(TZ="UTC")	
	}
	
	{# load packages
		sapply(c('arm','blmeco','effects', 'data.table','ggplot2','grid', 'lattice','magrittr','matrixStats','multcomp','pastecs','plyr','raster','stringr','XLConnect'),
    function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE) ))

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
		
		col_p="gray53"  # color of point's outline
		col_pb="gray98"  # color of point's background
		col_l="gray73"  # line color of prediction '#FCB42C'
		col_lb="gray92"  # line color of prediction '#FCB42C'
				
		# writing color
				wr_col="grey50"
				ln_col="grey80"

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
			# observations excluded from all analyses
				d=d[!d$obs_ID%in%e$obs_ID[which(e$total_exclusion=='y')],]
				d=d[!d$obs_ID%in%e$obs_ID[which(e$close_to_exchange=="y")],]
			
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
			d$lat = n$lat[match(tolower(paste(d$year, d$nest)),tolower(paste(n$year, n$nest)))] 
			d$lon = n$lon[match(tolower(paste(d$year, d$nest)),tolower(paste(n$year, n$nest)))] 
			
	   # time_series
				b<-read.csv(file=paste(wd, "time_series.csv", sep=''),header=T,sep=",", fill=T, stringsAsFactors=FALSE)
      			b=b[!is.na(b$obs_ID),]
				# observations excluded from all analyses
					b=b[!b$obs_ID%in%e$obs_ID[which(e$total_exclusion=='y')],]
					b=b[!b$obs_ID%in%e$obs_ID[which(e$close_to_exchange=="y")],]
					b=b[!b$obs_ID %in% c(7, 182),] # 7 - excluded because of before presence disturbance, 182 - excluded because of poor sound quality					
      			b$nest = tolower(b$nest)
      			b$t_delta=d$t_delta[match(b$obs_ID, d$obs_ID)]
      			b$dt_behaviour <- as.POSIXct(b$dt_behaviour,format='%Y-%b-%d %H:%M:%S')+b$t_delta
      			b$end_dt = as.POSIXct(b$end_dt,format='%Y-%b-%d %H:%M:%S')+b$t_delta
      			b$start_time_video = as.POSIXct(b$start_time_video,format='%Y-%b-%d %H:%M:%S')+b$t_delta
				
				b$obs_time=as.numeric(difftime(b$end_dt, b$start_time_video, 'min'))
				#densityplot(~b$obs_time)
				#b[b$obs_time>30,]
				
      			b$end_pr = as.character(d$dt_1st_presence[match(b$obs_ID,d$obs_ID)])
				#b$end_pr[is.na(b$end_pr) & b$type == 'ex'] = as.character(d$dt_arrive[match(b$obs_ID[is.na(b$end_pr) & b$type == 'ex'],d$obs_ID)]) #dt_arrive for obs_ID 1 and 7 as dt_presence unknown; as of now still one NA
				#b$obs_ID[is.na(b$end_pr) & b$type == 'ex'] 
      			
				b$end_pr=as.POSIXct(ifelse(is.na(b$end_pr), ifelse(b$type == 'ex',b$end_pr, as.character(b$end_dt)), b$end_pr))
      			# end of exchange observation before bird is present
      			
      			b$end_=n$end_[match(toupper(paste(b$nest, b$year)), toupper(paste(n$nest,n$year)))]
      			b$nest_ID=paste(b$nest,b$year)
				b$bird_ID = paste(b$year, b$nest_ID)
      			b$type = as.factor(b$type)
      			
      			b$inc_start=n$inc_start[match(paste(b$nest, b$year), paste(n$nest,n$year))]
      			b$day_j = as.numeric(format(b$dt_behaviour ,"%j")) - as.numeric(format(b$inc_start,"%j"))+1
				b$day_j[is.na(b$day_j)] = as.numeric(format(b$start_time_video[is.na(b$day_j)] ,"%j")) - as.numeric(format(b$inc_start[is.na(b$day_j)],"%j"))+1
				b$lat = n$lat[match(tolower(paste(b$year, b$nest)),tolower(paste(n$year, n$nest)))] 
				b$lon = n$lon[match(tolower(paste(b$year, b$nest)),tolower(paste(n$year, n$nest)))] 
      
	  
		# no nests excluded because of inc start or end date ( two nests have observations toward hatching, but we keep those for now)
				#d$check=as.POSIXct(ifelse(is.na(d$dt_on), as.character(d$dt_video),as.character(d$dt_on))) 
				#d=d[-which(d$check>d$inc_start & d$check<d$end_),] # two nests have observations toward hatching, but we keep those for now

       b = data.table(b)
       d = data.table(d)
	   
	   # only observations before a coming bird was present
		b1 = b[dt_behaviour<=end_pr]
		b2 = b[is.na(b$dt_behaviour),]
		b_ = rbind(b1,b2) # obs_ID 1, 7, 50, 182 not in (50 shall be included, and 1 perhaps too)
			# check
				#length(unique(b_$obs_ID[order(b_$obs_ID)]))
				#x = unique(d$obs_ID)
				#x[!x%in%unique(b_$obs_ID[order(b_$obs_ID)])]
		# number of calls and fly-offs per observation
			bb_=ddply(b_[!is.na(b_$sex),],.(obs_ID, nest_ID, sex, type, day_j, obs_time), summarise, call_i=length(behaviour[which(behaviour=='c' & who=='o')]),fly_i=length(behaviour[which(behaviour=='f' & who=='o')]))
			
			# 111 not in YET as it is unclear what is going on
			bb_=ddply(b_[!is.na(b_$who),],.(obs_ID, nest_ID, type, day_j, obs_time), summarise, call_i=length(behaviour[which(behaviour=='c' & who == 'o' & !is.na(dt_behaviour) & dt_behaviour<=end_pr)]),fly_i=length(behaviour[which(behaviour=='f' & who == 'o' & !is.na(dt_behaviour) & dt_behaviour<=end_pr)]))
			bb_$sex = d$sex[match(bb_$obs_ID, d$obs_ID)]
			#bb_[bb_$obs_ID %in% bb_$obs_ID[duplicated(bb_$obs_ID)],]
			#length(unique(bb_$obs_ID))
			
	   # only exchange observations
	   dd=d[d$type=='ex',]
	   dd$left_bin=ifelse(dd$left_before_presence=='y',1,0)	
}
