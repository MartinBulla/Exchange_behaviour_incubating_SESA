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

# number of nests protected by cage (at least for some time) in a given year
	ddply(d[d$cage == 'y'],. (year), summarise, n = length(unique(nest_ID)))

# number of observed nests in a given year	
	ddply(d,. (year), summarise, n = length(unique(nest_ID)))

# N for before arrival observations
	d$n = 1
	nrow(d[d$type=='ex',]) # before arrival
	nrow(d[d$type=='non',]) # control
	
	length(unique(d$nest_ID)) # N nest overall
	length(unique(d$nest_ID[d$type=='ex'])) # N nest before arrival
	length(unique(d$nest_ID[d$type=='non'])) # N nest control 
	
	de = ddply(d[d$type=='ex',],.(nest), summarise, n = length(type)) 
	summary(de$n) # N before arrival obs per nest
	dn = ddply(d[d$type=='non',],.(nest), summarise, n = length(type))
	summary(dn$n) # N control obs per nest

# END	