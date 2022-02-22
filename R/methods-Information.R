### INFO discussed within METHODS section ###

# SETTINGS & DATA
    # do you want plots in R (PNG=FALSE) or as PNG (PNG = TRUE)?
		PNG=FALSE #PNG = TRUE
	
	# load packages, constants and data
	    require(here)
		source(here::here('R/Constants_packages_PrepareData.R'))
		require(pracma)
		require(ggpubr)
		require(gridExtra)
		bb_$pair_ID = d$pair_ID[match(bb_$obs_ID,d$obs_ID)]

# field of view at nest distance
	summary(n$view_at_nest)
	summary(n$view_at_nest/2/tan(60*pi/180)) # approximate distance of a camera to the nest

	ggplot(n, aes(x = dist, y = view_at_nest)) +
			stat_smooth(method = "lm") + 
			stat_cor(aes(label = ..r.label..),  label.x = 0.5, size = 2) + 
			geom_point() 

	ggplot(n, aes(x = dist, y = view_at_nest/2/tan(60*pi/180))) +
			stat_smooth(method = "lm") + 
			stat_cor(aes(label = ..r.label..),  label.x = 0.5, size = 2) + 
			geom_point() 

# field of view and calling rate parameters for exchange observation
	m= glmer(call_i ~ view  + (1|nest_ID), offset = log(obs_time/10),family = poisson,  bb_[bb_$type %in% 'ex',])# rate per 10 minute
	bsim <- sim(m, n.sim=5000) 
	apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.5, 0.975))
	plot(allEffects(m))

# field of view and timing parameters
	ggplot(dd, aes( x = pa)) +geom_density()
	ggplot(dd, aes( x = pa)) +geom_density()
	ggplot(dd, aes( x = arrival)) +geom_density()
	ggplot(dd, aes( x = log(arrival)))
	 +geom_density()
	ggplot(dd, aes(x = view, y = pa/60))+
		stat_smooth() + 
		geom_point()	

	g1=
	ggplot(dd, aes(x = view, y = pa/60))+
		stat_smooth(method = 'lm') + 
		stat_cor(aes(label = ..r.label..),  label.x = 0.5, size = 2) + 
		labs(x = "field of view at nest [m]", y = 'from arrival to approach [min]')+
		geom_point(alpha = 0.5) +
		theme_bw()

	ggplot(dd, aes(x = view, y = pa/60))+
		stat_smooth() + 
		geom_point()	+
		scale_y_continuous(trans='log10')

	g2=
	ggplot(dd, aes(x = view, y = pa))+
		stat_smooth(method = 'lm') + 
		geom_point(alpha = 0.5)	+
		stat_cor(aes(label = ..r.label..),  label.x = 0.7, size = 2) + 
		labs(x = "field of view at nest [m]", y = 'from arrival to approach [s]')+
		#scale_y_continuous(trans='log10')
		scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
              labels = trans_format("log10", math_format(10^.x)))+
		theme_bw()	+
        annotation_logticks(side = "l")


	ggplot(dd[left_type == '3 during exchange'], aes(x = view, y = arrival))+
		stat_smooth() + 
		geom_point()

	ggplot(dd[left_type == '3 during exchange'], aes(x = view, y = arrival))+
		stat_smooth() + 
		geom_point() + 	scale_y_continuous(trans='log10')	

	g3 = 
	ggplot(dd[left_type == '3 during exchange'], aes(x = view, y = arrival))+
		stat_smooth(method = 'lm') + 
		geom_point(alpha = 0.5)	+
		stat_cor(aes(label = ..r.label..),  label.x = 0.7, size = 2) + 
		labs(x = "field of view at nest [m]", y = 'from initiation to leaving [s]')+
		theme_bw()

	g4 = 
	ggplot(dd[left_type == '3 during exchange'], aes(x = view, y = arrival))+
		stat_smooth(method = 'lm') + 
		geom_point(alpha = 0.5)	+
		stat_cor(aes(label = ..r.label..),  label.x = 0.7, size = 2) + 
		labs(x = "field of view at nest [m]", y = 'from initiation to leaving [s]')	+
		scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
              labels = trans_format("log10", math_format(10^.x)))+
		theme_bw()	+
        annotation_logticks(side = "l")

    grid.draw(rbind(cbind(ggplotGrob(g1), ggplotGrob(g2)), cbind(ggplotGrob(g3),ggplotGrob(g4))))
	# arrange and export

	  ggALL = arrangeGrob(rbind(cbind(ggplotGrob(g1), ggplotGrob(g2)), cbind(ggplotGrob(g3),ggplotGrob(g4))))
	  ggsave(file = 'Plots/Fig_for_review.png', ggALL, dpi = 300, width = 12, height = 12, units = 'cm')    

    m = lmer(pa~view +(1|nest_ID), dd)
    summary(m)
	summary(glht(m))	
	plot(allEffects(m))	
	nsim <- 5000
	bsim <- sim(m, n.sim=nsim)
   	apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.5, 0.975))	

    ddx = dd[left_type == '3 during exchange']
	m = lmer(log(arrival)~view +(1|nest_ID), ddx)
	summary(m)
	summary(glht(m))	
	plot(allEffects(m))
	nsim <- 5000
	bsim <- sim(m, n.sim=nsim)
   	apply(bsim@fixef, 2, quantile, prob=c(0.025, 0.5, 0.975))	

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