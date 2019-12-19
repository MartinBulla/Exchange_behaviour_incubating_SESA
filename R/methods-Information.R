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

# N observations per nest
	d$n = 1
	nrow(d[d$type=='ex',])
	nrow(d[d$type=='non',])
	length(unique(d$nest_ID))
	
	d_= ddply(d,.(nest, year ,type), summarise, n = sum(n))
	d_$id = paste(d_$nest, d_$year)
	
	ggplot(d_, aes(x = n, col = type)) + geom_density()
	summary(d_$n[which(d_$type == 'ex')])
	summary(d_$n[which(d_$type == 'non')])
	length(d_$n[which(d_$type == 'ex')])
	length(d_$n[which(d_$type == 'non')])
# check whether all nests with exchanges have also non-exchange observation and vice versa
	d_= ddply(d,.(nest, year ,type), summarise, n = sum(n))
	d_$id = paste(d_$nest, d_$year)

	x = d_$id[which(d_$type == 'ex')]
	y= d_$id[which(d_$type == 'non')]
	x[!x%in%d_$id[which(d_$type == 'non')]]
	y[!y%in%d_$id[which(d_$type == 'ex')]]
# number of observed nests in a given year	
	ddply(d,. (year), summarise, n = length(unique(nest_ID)))
# number of nests protected by cage (at least for some time) in a given year
	ddply(d[d$cage == 'y'],. (year), summarise, n = length(unique(nest_ID)))