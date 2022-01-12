### INFO discussed within METHODS section ###

# SETTINGS & DATA
    # do you want plots in R (PNG=FALSE) or as PNG (PNG = TRUE)?
		PNG=FALSE #PNG = TRUE
	
	# define working directory
	     wd = "C:/Users/mbulla/Documents/Dropbox/Science/MS/Exchanges_SESA/Database/"
		 wd2 = "C:/Users/mbulla/Documents/Dropbox/Science/MS/Exchanges_SESA/Analyses/"		 
	     outdir = "C:/Users/mbulla/Documents/Dropbox/Science/MS/Exchanges_SESA/Analyses/plots/" 
		#wd = "C:/Users/OWNER/Dropbox/exchanges_SESA/Database/"	
	    #wd = paste0(getwd(), '/DATA/')
	
	# load packages, constants and data
		source(paste(wd2, 'Constants_packages_PrepareData.R',sep=""))
		bb_$pair_ID = d$pair_ID[match(bb_$obs_ID,d$obs_ID)]

# number of nests protected by cage (at least for some time) in a given year
	ddply(d[d$cage == 'y'],. (year), summarise, n = length(unique(nest_ID)))

# number of observed nests in a given year	
	ddply(d,. (year), summarise, n = length(unique(nest_ID)))

# N for before arrival observations
	sel = dd[dd$left_type %in% c('2 while around','3 during exchange'),]
	dx = bb_[-which(bb_$type == 'ex' & !bb_$obs_ID %in% sel$obs_ID),]
	dx = dx[dx$nest_ID %in% unique(dx$nest_ID[dx$type == 'ex']),]
	nrow(dx)
	dx$n = 1
	nrow(dx[dx$type=='ex',]) # before arrival
	nrow(dx[dx$type=='non',]) # control
	
	length(unique(dx$nest_ID)) # N nest overall
	length(unique(dx$nest_ID[dx$type=='ex'])) # N nest before arrival
	length(unique(dx$nest_ID[dx$type=='non'])) # N nest control 
	
	de = ddply(dx[dx$type=='ex',],.(nest_ID), summarise, n = length(type)) 
	summary(de$n) # N before arrival obs per nest
	dn = ddply(dx[dx$type=='non',],.(nest_ID), summarise, n = length(type))
	summary(dn$n) # N control obs per nest

	table(dx$type, dx$nest_ID) # one nest with 0 ex and 2 non
	table(dx$type, dx$pair_ID)
	
# number of resampled individuals - none
	b<-read.csv(file=paste(wd, "birds.csv", sep=''),header=T,sep=",", fill=T, stringsAsFactors=FALSE)
	b[duplicated(b$bird_ID),]

# END	