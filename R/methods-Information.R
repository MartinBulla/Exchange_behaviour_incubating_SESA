### INFO discussed within METHODS section ###

# SETTINGS & DATA
    # do you want plots in R (PNG=FALSE) or as PNG (PNG = TRUE)?
		PNG=FALSE #PNG = TRUE
	
	# load packages, constants and data
	    require(here)
		source(here::here('R/Constants_packages_PrepareData.R'))
		require(pracma)
		bb_$pair_ID = d$pair_ID[match(bb_$obs_ID,d$obs_ID)]

# field of view at nest distance
	summary(n$view_at_nest)
	summary(n$view_at_nest/2/tan(60*pi/180)) # approximate distance of a camera to the nest

	ggplot(n, aes(x = dist, y = view_at_nest)) +
			geom_point()
			abline(1)
	ggplot(n, aes(x = dist, y = view_at_nest/2/tan(60*pi/180))) +
			geom_point() +
			geom_abline(slope = 1)

# field of view and nest parameters
	ggplot(dd, aes(x = view, y = pa/60))+
		stat_smooth() + 
		geom_point()	

	ggplot(dd, aes(x = view, y = pa/60))+
		stat_smooth(method = 'lm') + 
		geom_point()

	ggplot(dd, aes(x = view, y = pa/60))+
		stat_smooth() + 
		geom_point()	+
		scale_y_continuous(trans='log10')

	ggplot(dd, aes(x = view, y = pa/60))+
		stat_smooth(method = 'lm') + 
		geom_point()	+
		scale_y_continuous(trans='log10')	

	dd$arrival[dd$left_type == '3 during exchange']	

	ggplot(dd[left_type == '3 during exchange'], aes(x = view, y = arrival))+
		stat_smooth() + 
		geom_point()

	ggplot(dd[left_type == '3 during exchange'], aes(x = view, y = arrival))+
		stat_smooth() + 
		geom_point() + 	scale_y_continuous(trans='log10')	

	ggplot(dd[left_type == '3 during exchange'], aes(x = view, y = arrival))+
		stat_smooth(method = 'lm') + 
		geom_point()

	ggplot(dd[left_type == '3 during exchange'], aes(x = view, y = arrival))+
		stat_smooth(method = 'lm') + 
		geom_point()	+
		scale_y_continuous(trans='log10')	

	ddx = dd[left_type == '3 during exchange']
	m = lmer(arrival~view +(1|nest_ID), ddx)
	summary(glht(m))	
	plot(allEffects(m))	

# number of nests protected by cage (at least for some time) in a given year
	ddply(d[d$cage == 'y'],. (year), summarise, n = length(unique(nest_ID)))

# number of observed nests in a given year	
	ddply(d,. (year), summarise, n = length(unique(nest_ID)))
	#unique(d$nest[d$year==2013])[order(unique(d$nest[d$year==2013]))]
	#length(unique(d$nest[d$year==2013]))
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