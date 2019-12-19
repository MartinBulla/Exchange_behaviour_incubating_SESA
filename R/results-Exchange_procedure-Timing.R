### RESULTS section Exchange procedure - time components ###

# SETTINGS & DATA
    # do you want plots in R (PNG=FALSE) or as PNG (PNG = TRUE)?
		PNG=FALSE #PNG = TRUE

	# define working directory
	     wd = "/Users/martinbulla/Dropbox/Science/MS/Exchanges_SESA/Analyses/Data/"# "C:/Users/mbulla/Documents/Dropbox/Science/MS/Exchanges_SESA/Database/"
		 wd2 = "/Users/martinbulla/Dropbox/Science/MS/Exchanges_SESA/Analyses/R/"
	     outdir = "/Users/martinbulla/Dropbox/Science/MS/Exchanges_SESA/Analyses/plots/"
	     ta = "/Users/martinbulla/Dropbox/Science/MS/Exchanges_SESA/Analyses/Tables/"
		 #wd2 = "C:/Users/mbulla/Documents/Dropbox/Science/MS/Exchanges_SESA/Analyses/"
	     #outdir = "C:/Users/mbulla/Documents/Dropbox/Science/MS/Exchanges_SESA/Analyses/plots/"
		#wd = "C:/Users/OWNER/Dropbox/exchanges_SESA/Database/"
	    #wd = paste0(getwd(), '/DATA/')

	# load packages, constants and data
		source(paste0(wd2, 'Constants_packages_PrepareData.R'))

	# add colors for Figure Temp
		  col_bef = col_m
		  col_aro = col_f
		  col_dur = 'dark green'
		  xx = data.frame(left_type = c('1 before presence','2 while around','3 during exchange'), colr = c(col_bef,col_aro,col_dur), stringsAsFactors = FALSE)
		  dd = merge(dd,xx, by = 'left_type')
	
	# adjust variable and prepare datasets
		dd = dd[!is.na(dd$arrival),]
	
# description
  # presence before exchange
		summary(dd$pa/60)
		summary(dd$pa)
		length(dd$pa) # number of exchanges
		length(unique(dd$nest_ID)) # number of nests

		densityplot(dd$pa/60)
		densityplot(log(dd$pa/60))
		densityplot(asin(dd$pa/60))
		dd$obs_ID[dd$pa/60 > 2]
		ggplot(dd, aes(x = sex, y = log(pa), fill = sex)) + geom_boxplot()
		ggplot(dd, aes(x = day_j, y = log(pa), fill = sex)) + geom_point() + stat_smooth()
		ggplot(dd, aes(x = day_j, y = (pa), fill = sex)) + geom_point() + stat_smooth()
		table(dd$pa)
		m = lmer(log(pa)~ sex*day_j+(day_j|nest_ID), dd)
		plot(allEffects(m))
		summary(glht(m))
  # arrival
		summary(dd$arrival)
		length(dd$arrival[!is.na(dd$arrival)]) # number of exchanges
		length(unique(dd$nest_ID[!is.na(dd$arrival)])) # number of nests

		ex = dd[dd$left_before_presence=="n",]
		densityplot((ex$arrival-ex$gap)/60)
		densityplot(dd$arrival/60)
		densityplot(log(dd$arrival))
		dd$obs_ID[is.na(dd$arrival)]
		dd$obs_ID[dd$arrival/60 > 1]
		dd$obs_ID[dd$arrival>dd$presence]

		ggplot(dd, aes(x = left_type, y = log(arrival), col = sex)) + geom_boxplot(outlier.shape = 1) +
			geom_dotplot(aes(fill=sex),binaxis="y",stackdir="center",dotsize=0.5)
			geom_point(position = position_jitter(width = 0.2))

		ggplot(dd, aes(x = left_type, y = log(arrival), fill = sex)) + geom_boxplot()
			m = lmer(log(arrive)~ left_type + sex*day_j+(day_j|nest_ID), dd)
			plot(allEffects(m))
			summary(glht(m))
		ggplot(dd[!is.na(dd$arrival),], aes(x = sex, y = log(arrival), fill = sex)) + geom_boxplot()
		ggplot(dd[!is.na(dd$arrival),], aes(x = day_j, y = log(arrival), fill = sex)) + geom_point() + stat_smooth()

		summary(factor(dd$left_before_presence))


		cor(dd$arrival[!is.na(dd$arrival)],dd$presence[!is.na(dd$arrival)],method = 'pearson')
		cor(dd$arrival[!is.na(dd$arrival)],dd$presence[!is.na(dd$arrival)],method = 'spearman')
			cor(dd$arrival,dd$pa,method = 'spearman')
			cor(dd$arrival,dd$pa,method = 'pearson')
		ggplot(dd[!is.na(dd$arrival),], aes(x = log(presence), y = log(arrival), fill = sex)) + geom_point() + stat_smooth()
		ggplot(dd[!is.na(dd$arrival),], aes(x = log(presence), y = log(arrival))) + geom_point() + stat_smooth()
		ggplot(dd[!is.na(dd$arrival),], aes(x = log(presence), y = log(arrival))) + geom_point() + stat_smooth(method = 'lm')

		# not used
			# is time span between present and arrival dependent on whether incubating bird was around
			summary(dd$pa)
			densityplot(dd$pa)
			ggplot(dd, aes(x = left_before_presence, y = log(pa+0.01), fill = sex)) + geom_boxplot()
			ggplot(dd[!is.na(dd$arrival),], aes(x = left_before_presence, y = log(arrival), fill = sex)) + geom_boxplot()
			m = lmer(log(pa+0.01)~ left_before_presence +(1|nest_ID), dd)
			plot(allEffects(m))
			summary(glht(m))
  # when incubating left
  	 summary(factor(dd$left_type))
  	 table(dd$sex, dd$left_type)
  # exchange gap
	 summary(dd$gap)
	 summary(dd$gap/60)

	 summary(dd$gap[dd$left_type == '3 during exchange'])
	 summary(dd$gap[dd$left_type == '3 during exchange']/60)

	 xo = dd$obs_ID[dd$left_type == '3 during exchange']

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
	 ggplot(dd[!is.na(dd$gap),], aes(x = nest_ID, y = log(gap), fill = sex)) + geom_boxplot()
	 ggplot(dd[!is.na(dd$gap),], aes(x = day_j, y = log(gap), fill = sex)) + geom_point() + stat_smooth()

	 m = lmer(log(gap)~ sex*day_j+(day_j|nest_ID), dd)
	 plot(allEffects(m))
	 summary(glht(m))
  # start to left
		eb = dd[dd$left_before_presence=="n" & dd$left_type %in%c('3 during exchange'),]
		summary(eb$both)
		length(eb$both[!is.na(eb$both)]) # number of exchanges
		length(unique(eb$nest_ID[!is.na(eb$both)])) # number of nests
  # please-leave
  		pl = dd[dd$left_type %in%c('3 during exchange'),]
		
		summary(factor(pl$push)) # number of cases
		nrow(pl[pl$push == 'y'])/nrow(pl) # proportion of cases
		length(unique(pl$nest_ID[pl$push == 'y'])) # number of nests with please leave
		length(unique(pl$nest_ID[pl$push == 'y']))/length(unique(pl$nest_ID)) # proportion of nests

		# do both sexes please leave display at a given nest
		ee = pl[pl$push == 'y',]
		table(ee$sex_returning, ee$nest)	 # 5 nest with female only, 8 nests with male only
		
		# distribution across incubating sex
		table(pl$push, pl$sex) 	
		length(pl$push[pl$push=='y' & pl$sex=='f'])/length(pl$push[pl$sex=='f']) # female incubates
		length(pl$push[pl$push=='y' & pl$sex=='m'])/length(pl$push[pl$sex=='m'])

		# correlation between push off and calling intensity
				en = pl[!is.na(pl$call_int_1),]
				cor(en$pushoff_int,en$call_int_1,method = 'spearman')
				cor(en$pushoff_int,en$call_int_1,method = 'pearson')
				ggplot(en,aes(x=call_int_1, y=pushoff_int))+geom_point()+stat_smooth(method='lm')
				ggplot(en,aes(x=call_int_1, y=as.numeric(as.factor(push))))+geom_point()+stat_smooth(method='lm')
  # nest relief duration
  		nr = dd[dd$left_type %in% c('3 during exchange'),]
  		nr$nr_dur = as.numeric(difftime(nr$dt_on, nr$dt_arrive, unit = 'sec'))
  		summary(nr$nr_dur)
  		summary(nr$nr_dur/60)
  		nrow(nr)
		length(unique(nr$nest_ID))
# Table S7 - CHANGE NAME
	# prepare table data
		# current bout
			e = dd[dd$left_type %in%c('3 during exchange') & !is.na(dd$current_bout),]
			nrow(e)
			length(unique(e$bird_ID))
			length(unique(e$nest_ID))

			m=glmer(push01 ~ sex*scale(current_bout)+ (scale(current_bout)|bird_ID)+(1|nest_ID), family='binomial',e)
			#m=glmer(push_ ~ sex_returning*scale(current_bout)+ (scale(current_bout)|bird_ID)+(1|nest_ID), family='binomial',e)
			#m=lmer(push_ ~ sex*scale(current_bout)+ (scale(current_bout)|bird_ID)+(1|nest_ID), e)
			#m=lmer(push_ ~ sex_returning*scale(current_bout)+ (scale(current_bout)|bird_ID)+(1|nest_ID), e)
			#m=lmer(current_bout ~ push*sex_returning+ (1|bird_ID)+(1|nest_ID), e)
			####m=lmer(pushoff_int ~ sex*scale(current_bout)+ (scale(current_bout)|bird_ID)+(1|nest_ID), e)
			###densityplot(~e$pushoff_int)
			#m=glmer(push ~ scale(current_bout)+ (scale(current_bout)|bird_ID)+(1|nest_ID), family='binomial',e) # not confounded by sex
						# binomial gives same results
						#dd$pa_bin=ifelse(dd$pa == 0.001, 0,1)
						#m = glmer(pa_bin~ sex*day_j+(day_j|nest_ID),family='binomial', dd)
			pred=c('Intercept (f)','Sex(m)', 'Current bout', 'Current bout:sex')
			dep = 'push'
			mod = 1
				nsim <- 5000
				bsim <- sim(m, n.sim=nsim)
				#apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5,0.975))
				#plot(allEffects(m))
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
				ri$estimate_r = paste(ri$estimate_r,"%",sep='')
			o1=rbind(oii,ri)
		# day of incubation
			e = dd[dd$left_type %in%c('3 during exchange') & !is.na(dd$current_bout),]
			m=glmer(push01 ~ sex*scale(day_j)+ (scale(day_j)|bird_ID), family='binomial',e)
						# binomial gives same results
						#dd$pa_bin=ifelse(dd$pa == 0.001, 0,1)
						#m = glmer(pa_bin~ sex*day_j+(day_j|nest_ID),family='binomial', dd)
			pred=c('Intercept (f)','Sex(m)', 'Day', 'Day:sex')
			dep = 'push'
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
				ri$estimate_r = paste(ri$estimate_r,"%",sep='')
			o2=rbind(oii,ri)
	# create xlsx table
		o=rbind(o1,o2)
		sname = 'Table_S7'
		tmp = write_xlsx(o, paste0(ta,sname,'.xlsx'))
		openFile(tmp)
# Figure pleaseLeave	
	# raw data
		e = dd[dd$left_type %in%c('3 during exchange') & !is.na(dd$current_bout),]
		e$n = 1
				#x = ddply(f,.(nest_ID, sex_returning_returning),summarise, mo=median(call_o_int), q1o=quantile(call_o_int,0.25), q2o= quantile(call_o_int,0.75), mc=median(call_c_int), q1c=quantile(call_c_int,0.25), q2c= quantile(call_c_int,0.75), n = sum(n))

		e$col_=ifelse(e$sex_returning=='f', '#FCB42C', '#535F7C') # female color = #FCB42C)

		x = ddply(e,.(sex_returning,day_j),summarise, push = mean(push01), n = sum(n))
				#x$call_o_int = ifelse(x$sex_returning == 'f', x$call_o_int-0.2, x$call_o_int+0.2)
				# call based on x-axis - incubating parent
				x$col_=ifelse(x$sex_returning=='f', '#FCB42C', '#535F7C') # female color = #FCB42C)
	# predict
		e = dd[dd$left_type %in%c('3 during exchange') & !is.na(dd$current_bout),]
		m=glmer(push01 ~ sex_returning*day_j+ (day_j|bird_ID), family='binomial',e) 
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)
		v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
	  	# values to predict for
			l = list()
			for(i in c('m','f')){
				ei = e[e$sex_returning == i]
				l[[i]]=data.frame(	sex_returning = i,
									day_j = seq(min(ei$day_j), max(ei$day_j), length.out = 100)
									)
					}
			newD = do.call(rbind,l)

		# exactly the model which was used has to be specified here
			X <- model.matrix(~ sex_returning*day_j,data=newD)

			# calculate predicted values and creditability intervals
				newD$pred <-plogis(X%*%v)
						predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
						for(i in 1:nsim) predmatrix[,i] <- plogis(X%*%bsim@fixef[i,])
						newD$lwr <- (apply(predmatrix, 1, quantile, prob=0.025))
						newD$upr <- (apply(predmatrix, 1, quantile, prob=0.975))
				pp=newD
				ppf = pp[pp$sex_returning == 'f',]
				ppm = pp[pp$sex_returning == 'm',]
	# plot
		if(PNG == TRUE){
			png(paste(outdir,"Figure_plsLeave.png", sep=""), width=1.85+0.3,height=1.5,units="in",res=600)
				}else{ dev.new(width=1.85+0.3,height=1.5)}
		par(mar=c(0.4,0.1,0.6,2.5),oma = c(1, 2, 0, 0),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #

		plot(pred ~ day_j, data = pp,
									#ylab =NULL,
									xaxt='n',
									yaxt='n',
									#xaxs = 'i',
									#yaxs = 'i',
									ylim = c(0,1),
									xlim = c(0,30),
									type='n'
									) # col=z_g$cols, border=z_g$cols

			
			#points(e$push) ~ jitter(dd$day_j), col = adjustcolor(dd$colr, alpha.f = 0.8), bg = adjustcolor(dd$colr, alpha.f = 0.4), pch =21, cex = 0.4)

			# predictions
				polygon(c(ppf$day_j, rev(ppf$day_j)), c(ppf$lwr,
								rev(ppf$upr)), border=NA, col=adjustcolor(col_f,alpha.f = 0.4)) #0,0,0 black 0.5 is transparents RED
				lines(ppf$day_j, ppf$pred, col=col_f,lwd=1)

				polygon(c(ppm$day_j, rev(ppm$day_j)), c(ppm$lwr,
								rev(ppm$upr)), border=NA, col=adjustcolor(col_m,alpha.f = 0.4)) #0,0,0 black 0.5 is transparents RED
				lines(ppm$day_j, ppm$pred, col=col_m,lwd=1)

				#text(x=-2,y=0.725, labels='Before', col='#FCB42C', cex=0.5)
						#text(x=2,y=0.725, labels='After', col='#535F7C', cex=0.5)

			symbols(x$day_j, x$push, circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) #
			axis(1, at=seq(0,30,by = 10), label=seq(0,30,by = 10), mgp=c(0,-0.20,0), lwd = 0.35)
			axis(2, at=seq(0,1,by = 0.2), label=c('0%', '20%', '40%', '60%', '80%','100%'), lwd = 0.35)

			mtext("Incubation day",side=1,line=0.4, cex=0.55, las=1, col='black')
			mtext("Please-leave display",side=2,line=1.1, cex=0.55, las=3, col='black')

	
			 if(PNG == TRUE) {dev.off()}
# Table S3 + text data for S3d
  # prepare table data
	 # a. presence before exchange
		    m = lmer(pa ~ sex_returning*scale(day_j)+(day_j|nest_ID), dd)
					# binomial gives same results
						#dd$pa_bin=ifelse(dd$pa == 0.001, 0,1)
						#m = glmer(pa_bin~ sex*day_j+(day_j|nest_ID),family='binomial', dd)
			#'plot(allEffects(m))
			pred=c('Intercept (f)','sex_returning(m)', 'Day', 'Day:sex') # note that sex of returning bird is oposite of this
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
				ri$estimate_r = paste(ri$estimate_r,"%",sep='')
			o1=rbind(oii,ri)
	 # b. arrival
		 	m = lmer(log(arrival) ~ left_type + sex_returning*scale(day_j)+(scale(day_j)|nest_ID), dd)
		 	#'plot(allEffects(m))
			pred=c('Intercept (f & before)','Left between', 'Left after', 'sex_returning(m)', 'Day', 'Day:sex') # note that sex of returning bird is oposite of this
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
				ri$estimate_r = paste(ri$estimate_r,"%",sep='')
			o2=rbind(oii,ri)
	 # c. gap
		  m = lmer(log(gap)~ sex_returning*scale(day_j)+(day_j|nest_ID), dd)
		  #'plot(allEffects(m))
		  #dd$capture=as.factor(dd$capture)
		  #dd$cap=as.factor(ifelse(dd$capture%in%c(1,2,3,4), 'y','n'))
		  #ggplot(dd,aes(x=capture, y=log(gap))) + geom_point()
		  #m = lmer(gap~ cap+sex*scale(day_j)+(day_j|nest_ID), dd)
		  #m = lmer(log(gap)~ cap+sex*scale(day_j)+(day_j|nest_ID), dd)
		  #m = lmer(log(gap)~ capture+sex*scale(day_j)+(day_j|nest_ID), dd)
			pred=c('Intercept (f)','sex_returning(m)', 'Day', 'Day:sex') # note that sex of returning bird is oposite of this
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
				ri$estimate_r = paste(ri$estimate_r,"%",sep='')
			o3=rbind(oii,ri)
	 # d. start to left
		 eb = dd[dd$left_before_presence=="n" & dd$left_type %in%c('3 during exchange'),]
		 m = lmer(both ~ sex_returning*push + sex_returning*scale(day_j)+(push01|bird_ID) + (1|nest_ID), eb)
		 #'plot(allEffects(m))
			pred=c('Intercept (f & n)','sex_returning(m)','Push(y)', 'Day', 'Day:push', 'Day:sex') # sex of the pusing bird is the oposite of this
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
				ri$estimate_r = paste(ri$estimate_r,"%",sep='')
			o4=rbind(oii,ri)
  # create xlsx table
			o=rbind(o1,o2,o3,o4)
			sname = 'Table_S3'
			tmp = write_xlsx(o, paste0(ta,sname,'.xlsx'))
			openFile(tmp)
  # text data for 'd'
  		m = lmer(both ~ sex_returning*push + day_j+(push01|bird_ID) + (1|nest_ID), eb)
  		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)
  		apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5,0.975))

  		m = lmer(both ~ sex_returning*push + sex_returning*day_j+(push01|bird_ID) + (1|nest_ID), eb)
 		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)
		v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
  	 	# values to predict for
			newD=data.frame(push=0.5,
							sex_returning = 0.5,
							day_j = seq(min(eb$day_j), max(eb$day_j), length.out = 100)
							)

		# exactly the model which was used has to be specified here
		X <- model.matrix(~ sex_returning*push + sex_returning*day_j,data=newD)

		# calculate predicted values and creditability intervals
			newD$pred <-(X%*%v)
					predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
					for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
					newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
					newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
			pp=newD
			plot(pp$pred ~ pp$day_j)

			polygon(c(pp$day_j, rev(pp$day_j)), c(pp$lwr,
							rev(pp$upr)), border=NA, col=adjustcolor(col_m ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
			lines(pp$day_j, pp$pred, col=col_m,lwd=1)
# Figure Temp
  # prepare data
	  m = lmer(log(arrival) ~ left_type*day_j+(scale(day_j)|nest_ID), dd)
		nsim <- 5000
		bsim <- sim(m, n.sim=nsim)
		v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
  	 	# values to predict for
			l = list()
			for(i in c('1 before presence','2 while around','3 during exchange')){
				ddi = dd[dd$left_type == i]
				l[[i]]=data.frame(left_type = i,
							day_j = seq(min(ddi$day_j), max(ddi$day_j), length.out = 100)
							)
			}
			newD = do.call(rbind,l)

		# exactly the model which was used has to be specified here
		X <- model.matrix(~ left_type*day_j,data=newD)

		# calculate predicted values and creditability intervals
			newD$pred <-(X%*%v)
					predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
					for(i in 1:nsim) predmatrix[,i] <- ((X%*%bsim@fixef[i,]))
					newD$lwr <- (apply(predmatrix, 1, quantile, prob=0.025))
					newD$upr <- (apply(predmatrix, 1, quantile, prob=0.975))
			pp=newD
			pp1 = pp[pp$left_type == '1 before presence',]
			pp2 = pp[pp$left_type == '2 while around',]
			pp3 = pp[pp$left_type == '3 during exchange',]
  # plot
		if(PNG == TRUE){
			png(paste(outdir,"Figure_Temp.png", sep=""), width=1.85+0.3,height=1.5,units="in",res=600)
				}else{ dev.new(width=1.85+0.3,height=1.5)}
		par(mar=c(0.4,0.1,0.6,2.5),oma = c(1, 2, 0, 0),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #

		plot((pred) ~ day_j, data = pp,
									#ylab =NULL,
									xaxt='n',
									yaxt='n',
									#xaxs = 'i',
									#yaxs = 'i',
									ylim = log(c(2, max(dd$arrival))),
									xlim = c(0,30),
									type='n'
									) # col=z_g$cols, border=z_g$cols

			points(log(dd$arrival) ~ jitter(dd$day_j), col = adjustcolor(dd$colr, alpha.f = 0.8), bg = adjustcolor(dd$colr, alpha.f = 0.4), pch =21, cex = 0.4)

			# predictions
				polygon(c(pp1$day_j, rev(pp1$day_j)), c(pp1$lwr,
								rev(pp1$upr)), border=NA, col=adjustcolor(col_bef ,alpha.f = 0.4)) #0,0,0 black 0.5 is transparents RED
				lines(pp1$day_j, pp1$pred, col=col_bef,lwd=1)

				polygon(c(pp2$day_j, rev(pp2$day_j)), c(pp2$lwr,
								rev(pp2$upr)), border=NA, col=adjustcolor(col_aro ,alpha.f = 0.4)) #0,0,0 black 0.5 is transparents RED
				lines(pp2$day_j, pp2$pred, col=col_aro,lwd=1)

				polygon(c(pp3$day_j, rev(pp3$day_j)), c(pp3$lwr,
								rev(pp3$upr)), border=NA, col=adjustcolor(col_dur ,alpha.f = 0.4)) #0,0,0 black 0.5 is transparents RED
				lines(pp3$day_j, pp3$pred, col=col_dur,lwd=1)


				#text(x=-2,y=0.725, labels='Before', col='#FCB42C', cex=0.5)
						#text(x=2,y=0.725, labels='After', col='#535F7C', cex=0.5)


			axis(1, at=seq(0,30,by = 10), label=seq(0,30,by = 10), mgp=c(0,-0.20,0), lwd = 0.35)
			axis(2, at=log(c(2,5,10,20,40,80,180)), label=c(' 2 s',' 5 s','10 s','20 s', '40 s', '80 s','3 min'), lwd = 0.35)

			mtext("Incubation day",side=1,line=0.4, cex=0.55, las=1, col='black')
			mtext("Nest relief",side=2,line=1.1, cex=0.55, las=3, col='black')


				text(x=7,y=log(180)+0.3, labels='Incubating bird left:', cex=0.5, pos=4)
				text(x=7,y=log(180), labels='Before return', col=col_bef, cex=0.5, pos=4)
				text(x=7,y=log(180)-0.3, labels='While returned around', col=col_aro, cex=0.5, pos=4)
				text(x=7,y=log(180)-0.3*2, labels='After relief initiated', col=col_dur, cex=0.5, pos=4)


			 if(PNG == TRUE) {dev.off()}
# Figure X + text data
	# prepare for plotting
			k=0.1 # distance
			kk=k*3# distance
			kkk=k*2
			dd$left_sex=ifelse(dd$left_type=='1 before presence',ifelse(dd$sex=='f', 1-k,k+1),
							ifelse(dd$left_type=='2 while around',ifelse(dd$sex=='f', 4-k,k+4),
								ifelse(dd$sex=='f', 7-k,k+7)))
			dd$col_=ifelse(dd$sex=='f','#FCB42C', '#535F7C')
			x = dd
			x$at=ifelse(dd$left_type=='1 before presence',ifelse(dd$sex=='f', 1-kkk,2+kkk),
							ifelse(dd$left_type=='2 while around',ifelse(dd$sex=='f', 3.7-kkk,4.7+kkk),
								ifelse(dd$sex=='f', 6.4-kkk,7.4+kkk)))
	# prepare model predictions + generate text results
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
				pp$pred_back = exp(pp$pred)
				pp$lwr_back = exp(pp$lwr)
				pp$upr_back = exp(pp$upr)

		    # text results
				pp$pred_back[3]- pp$pred_back[1]
				pp$lwr_back[3]- pp$lwr_back[1]
				pp$upr_back[3]- pp$upr_back[1]
	# plot
		if(PNG == TRUE) {
				png(paste(outdir,"Figure_X.png", sep=""), width=1.85+0.6,height=1.5,units="in",res=600)
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

				text(x=1*0.6,y=log(max(dd$arrival))*0.97, labels='\u2640', col='#FCB42C', cex=0.6)
				text(x=1,y=log(max(dd$arrival)), labels='\u2642', col='#535F7C', cex=0.6)
				mtext("Predictions\n95%CrI",side=3,line=-1.5, cex=0.5, las=1, col='red')
				#axis(1, at=c(1.5,4.2, 6.9), labels=FALSE)

				#text(c(1,2), par("usr")[3]+0.07, labels = c('\u2640','\u2642'), font=4, xpd = TRUE, cex=0.6, col=c('#FCB42C','#535F7C'))#col="grey30") #labels
				text(c(1.5,4.2, 6.9), par("usr")[3]-0.25, labels = c('Before\npresence','While\naround', 'After\nrelief initiated'),  xpd = TRUE, cex=0.5, col="grey30")
				mtext("Incubating parent left",side=1,line=0.6, cex=0.5, las=1, col='grey30')
				#text(c(2.85), par("usr")[3]-0.18, labels = c('Period'),  xpd = TRUE, cex=0.6, col="grey30")
				at_=c(5,15,30,60,120,180)

				#for(i in 1:6){axis(2, at=log(seq(at_[i],at_[i+1],length.out=15)), labels=FALSE,tcl=-0.05, lwd=0.5)}
				for(i in 1:5){axis(2, at=log(seq(at_[i],at_[i+1],length.out=10)), labels=FALSE,tcl=-0.075, lwd=0.5)}
				axis(2, at=log(at_), labels=c('5 s','15 s','30 s','1 min','2 min','3 min'))
				mtext("Nest relief",side=2,line=1.3, cex=0.55, las=3, col='grey30')
				#mtext("Duration",side=2,line=1, cex=0.6, las=3, col='grey30')

				# predictions
					points(y=pp$pred,x=c(1.5,4.2,6.9), pch=20, cex=0.9,col="red")
				# 95%CI
					arrows(x0=c(1.5,4.2,6.9), y0=pp$lwr,x1=c(1.5,4.2,6.9), y1=pp$upr,
					code = 0, col="red", angle = 90, length = .025, lwd=1, lty=1)

			 if(PNG == TRUE) {dev.off()}
# Figure 2
	  # prepare data
	  		ebb = dd[dd$left_before_presence=="n" & dd$left_type %in%c('3 during exchange'),]
			ebb$push_sex=ifelse(ebb$push=='y',ifelse(ebb$sex_returning=='f', 1-k,k+1),
							ifelse(ebb$sex_returning=='f', 4-k,k+4))
	  # prepare for plotting
			k=0.1
			kk=k*2# distance for boxplots
			kkk=k*2 # distance for points 
			
			x = ebb
			x$col_=ifelse(x$sex_returning=='f','#FCB42C', '#535F7C')
			x$at=ifelse(x$push=='y',ifelse(x$sex_returning=='f', 1-kkk,2+kkk),
							ifelse(x$sex_returning=='f', 3.7-kkk,4.7+kkk))
	  # prepare model predictions
	  		m = lmer(both ~ sex_returning*push + sex_returning*day_j+(push01|bird_ID) + (1|nest_ID), ebb)
					nsim <- 5000
					bsim <- sim(m, n.sim=nsim)
			# coefficients
				v = apply(bsim@fixef, 2, quantile, prob=c(0.5))

			# values to predict for
				newD=data.frame(push=c('y','n'),
								sex_returning = 0.5,
								day_j = mean(ebb$day_j)
								)

			# exactly the model which was used has to be specified here
			X <- model.matrix(~ sex_returning*push + sex_returning*day_j,data=newD)

			# calculate predicted values and creditability intervals
				newD$pred <-(X%*%v)
						predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
						for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
						newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
						newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
				pp=newD
				pt_=pp[pp$push=='y',]
				pc_=pp[pp$push=='n',]
	  # plot
		 if(PNG == TRUE) {
				#png(paste(outdir,"Figure_2_new_mac.png", sep=""), width=1.85+0.6,height=1.5,units="in",res=600)
				quartz(file = paste(outdir,"Figure_2_new_mac.png", sep=""), type = "png", width=1.85+0.6,height=1.5, dpi = 600)
				}else{
				dev.new(width=1.85+0.6,height=1.5)
				}
		 #par(mar=c(0.8,0,0,2.5),oma = c(0.7, 2, 0, 0),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey40", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
		 par(mar=c(0.3,0,0,1.2),oma = c(1.2, 1.7, 0.1, 2.4),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE, lwd=0.5)
		 #par(mfrow=c(2,1),mar=c(0.25,0,0,1.2),oma = c(2.1, 2.2, 0.2, 2.4),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE, lwd=0.5)
			boxplot(both ~ push_sex, data = ebb,
									#ylab =NULL,
									xaxt='n',
									yaxt='n',
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
										#bg = x$col_[i],
										col="gray63",
										#col = x$col_[i],
										bg = adjustcolor(x$col_[i], alpha.f = 0.4),
										pch =21, cex=0.5,
										vertical = TRUE, add = TRUE, method = "jitter")
										}

			boxplot(both ~ push_sex, data = ebb,
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
				axis(2, at=seq(0,100,by=20), lwd = 0.35)
				mtext("From initiation to leaving [s]",side=2,line=1.1, cex=0.55, las=3, col='black')

				text(c(1.5,4.2), par("usr")[3]-0.45, labels = c('Yes','No'),  xpd = TRUE, cex=0.5, col="black")
				text(c(1,2,3.7,4.7), par("usr")[3]-0.8, labels = c('\u2640','\u2642'), font=4, xpd = TRUE, cex=0.6, col=c('#FCB42C','#535F7C'))
				mtext("Please-leave display\n[returning parent]",side=1,line=0.6,cex=0.55, las=1, col='black')

				#text(x=0.3,y=100*0.97, labels='\u2640', col='#FCB42C', cex=0.6, pos=4)
				#text(x=0.3+0.2,y=100, labels='\u2642', col='#535F7C', cex=0.6, pos=4)
				#text(x=0.3+0.6,y=100, labels='incubating', col='grey30', cex=0.6, pos=4)
				text(x=0.3,y=100*0.88, labels="Predictions &\n95%CI", col='red', cex=0.5, pos=4)
				#mtext("Prediction\n95%CrI",side=3,line=-1.5, cex=0.5, las=1, col='red')
				#axis(1, at=c(1.5,4.2, 6.9), labels=FALSE)

				#text(c(1,2), par("usr")[3]+0.07, labels = c('\u2640','\u2642'), font=4, xpd = TRUE, cex=0.6, col=c('#FCB42C','#535F7C'))#col="grey30") #labels


				# predictions
					points(y=pp$pred,x=c(1.5,4.2), pch=20, cex=0.9,col="red")
				# 95%CI
					arrows(x0=c(1.5,4.2), y0=pp$lwr,x1=c(1.5,4.2), y1=pp$upr,
					code = 0, col="red", angle = 90, length = .025, lwd=1, lty=1)

			 if(PNG == TRUE) {dev.off()}

# model assumptions
	# TABLE S7
		# a. current bout
			e = dd[dd$left_type %in%c('3 during exchange') & !is.na(dd$current_bout),]
			m=glmer(push01 ~ sex*scale(current_bout)+ (scale(current_bout)|bird_ID)+(1|nest_ID), family='binomial',e)
			if(PNG == TRUE){png(paste(outdir,"model_ass/Table_S7a.png", sep=""), width=6,height=9,units="in",res=600)}else{dev.new(width=6,height=9)}
					  
			  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
			  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
			  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
			  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')
			  qqline(resid(m))

			  plot(fitted(m), jitter(e$push01, amount=0.05), xlab="Fitted values", ylab="Probability of left", las=1, cex.lab=1.2, cex=0.8)
			  abline(0,1, lty=3)
			  t.breaks <- cut(fitted(m), quantile(fitted(m)))
			  means <- tapply(e$push01, t.breaks, mean)
			  semean <- function(x) sd(x)/sqrt(length(x))
			  means.se <- tapply(e$push01, t.breaks, semean)
			  points(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means, pch=16, col="orange")
			  segments(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means-2*means.se, quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means+2*means.se,lwd=2, col="orange")

			  e$fit = fitted(m)
			  ee = e[e$fit<0.01,]
			  plot(ee$fit, jitter(ee$push01, amount=0.05), xlab="Fitted values", ylab="Probability of left", las=1, cex.lab=1.2, cex=0.8)
			  abline(0,1, lty=3)
			  points(quantile(fitted(m),c(0.125,0.375,0.625,0.875))[1:3], means[1:3], pch=16, col="orange")
			  segments(quantile(fitted(m),c(0.125,0.375,0.625,0.875))[1:3], means[1:3]-2*means.se[1:3], quantile(fitted(m),c(0.125,0.375,0.625,0.875))[1:3], means[1:3]+2*means.se[1:3],lwd=2, col="orange")

			  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept nest",col='red')
			  qqline(unlist(ranef(m)$nest_ID [1]))
			  qqnorm(unlist(ranef(m)$bird_ID [1]), main = "ran intercept bird",col='red')
			  qqline(unlist(ranef(m)$bird_ID [1]))
			  qqnorm(unlist(ranef(m)$bird_ID[2]), main = "ran slope bout|bird",col='red')
			  qqline(unlist(ranef(m)$bird_ID[2]))

			  scatter.smooth(resid(m)~e$current_bout);abline(h=0, lty=2, col='red')
			  boxplot(resid(m)~e$sex);abline(h=0, lty=2, col='red')

			  mtext("m=glmer(push ~ sex*scale(current_bout)+ (scale(current_bout)|bird_ID)+(1|nest_ID), family='binomial',e)", side = 3, line = 0.5, cex=0.6,outer = TRUE)

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

			if(PNG == TRUE){dev.off()}
		# day of incubation
			e = dd[dd$left_type %in%c('3 during exchange') & !is.na(dd$current_bout),]
			m=glmer(push01 ~ sex*scale(day_j)+ (scale(day_j)|bird_ID), family='binomial',e)
			if(PNG == TRUE){png(paste(outdir,"model_ass/Table_S7b.png", sep=""), width=6,height=9,units="in",res=600)}else{dev.new(width=6,height=9)}
					  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
					  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
					  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
					  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')
					  qqline(resid(m))

					  plot(fitted(m), jitter(e$push01, amount=0.05), xlab="Fitted values", ylab="Probability of left", las=1, cex.lab=1.2, cex=0.8)
					  abline(0,1, lty=3)
					  t.breaks <- cut(fitted(m), quantile(fitted(m)))
					  means <- tapply(e$push01, t.breaks, mean)
					  semean <- function(x) sd(x)/sqrt(length(x))
					  means.se <- tapply(e$push01, t.breaks, semean)
					  points(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means, pch=16, col="orange")
					  segments(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means-2*means.se, quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means+2*means.se,lwd=2, col="orange")

					  e$fit = fitted(m)
					  ee = e[e$fit<0.01,]
					  plot(ee$fit, jitter(ee$push01, amount=0.05), xlab="Fitted values", ylab="Probability of left", las=1, cex.lab=1.2, cex=0.8)
					  abline(0,1, lty=3)
					  points(quantile(fitted(m),c(0.125,0.375,0.625,0.875))[1:3], means[1:3], pch=16, col="orange")
					  segments(quantile(fitted(m),c(0.125,0.375,0.625,0.875))[1:3], means[1:3]-2*means.se[1:3], quantile(fitted(m),c(0.125,0.375,0.625,0.875))[1:3], means[1:3]+2*means.se[1:3],lwd=2, col="orange")


					  qqnorm(unlist(ranef(m)$bird_ID [1]), main = "ran intercept",col='red')
					  qqline(unlist(ranef(m)$bird_ID [1]))
					  qqnorm(unlist(ranef(m)$bird_ID[2]), main = "ran slope",col='red')
					  qqline(unlist(ranef(m)$bird_ID[2]))

					  scatter.smooth(resid(m)~e$day_j);abline(h=0, lty=2, col='red')
					  boxplot(resid(m)~e$sex);abline(h=0, lty=2, col='red')

					  mtext("m=glmer(push01 ~ sex*scale(day)+ (scale(day)|bird_ID)), family='binomial',e)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

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

			if(PNG == TRUE){dev.off()}			 
	# TABLE S3
		# a. presence before exchange
			 m = lmer(pa ~ sex_returning*scale(day_j)+(day_j|nest_ID), dd)
			 if(PNG == TRUE){png(paste(outdir,"model_ass/Table_S3a.png", sep=""), width=6,height=9,units="in",res=600)}else{dev.new(width=6,height=9)}
			 
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

				  scatter.smooth(resid(m)~dd$day_j);abline(h=0, lty=2, col='red')
				  #scatter.smooth(resid(m)~dd$sex);abline(h=0, lty=2, col='red')
				  boxplot(resid(m)~dd$sex);abline(h=0, lty=2, col='red')

				   mtext("lmer(log(pad)~ sex_returning*day_j+(day_j|nest_ID), dd)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

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

				if(PNG == TRUE){dev.off()}
		# b. arrival
			m = lmer(log(arrival) ~ left_type + sex_returning*scale(day_j)+(scale(day_j)|nest_ID), dd)
			if(PNG == TRUE){png(paste(outdir,"model_ass/Table_S3b.png", sep=""), width=6,height=9,units="in",res=600)}else{dev.new(width=6,height=9)}
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
			  #scatter.smooth(resid(m)~dd$sex);abline(h=0, lty=2, col='red')
			  boxplot(resid(m)~dd$sex_returning);abline(h=0, lty=2, col='red')

			  #scatter.smooth(resid(m)~dd$left_type);abline(h=0, lty=2, col='red')
			  boxplot(resid(m)~dd$left_type);abline(h=0, lty=2, col='red')
			   mtext("lmer(log(arrival) ~ left_type + sex_returning*day_j+(day_j|nest_ID), dd)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

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

				if(PNG == TRUE){dev.off()}
		# c. gap
			m = lmer(log(gap)~ sex_returning*scale(day_j)+(day_j|nest_ID), dd)
			 if(PNG == TRUE){png(paste(outdir,"model_ass/Table_S3c.png", sep=""), width=6,height=9,units="in",res=600)}else{dev.new(width=6,height=9)}
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
			  #scatter.smooth(resid(m)~dd$sex_returning);abline(h=0, lty=2, col='red')
			  boxplot(resid(m)~dd$sex_returning);abline(h=0, lty=2, col='red')

			   mtext("lmer(log(gap)~ sex_returning*day_j+(day_j|nest_ID), dd)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

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

				if(PNG == TRUE){dev.off()}
		# d. start to left
			eb = dd[dd$left_before_presence=="n" & dd$left_type %in%c('3 during exchange'),]
		    m = lmer(both ~ sex_returning*push + sex_returning*scale(day_j)+(push01|bird_ID) + (1|nest_ID), eb)
			  if(PNG == TRUE){png(paste(outdir,"model_ass/Table_S3d.png", sep=""), width=6,height=9,units="in",res=600)}else{dev.new(width=6,height=9)}
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
			  #scatter.smooth(resid(m)~eb$sex_returning);abline(h=0, lty=2, col='red')
			  boxplot(resid(m)~eb$sex_returning);abline(h=0, lty=2, col='red')

			  #scatter.smooth(resid(m)~eb$push);abline(h=0, lty=2, col='red')
			  boxplot(resid(m)~eb$push);abline(h=0, lty=2, col='red')

			   mtext("lmer(both ~ sex_returning*push + sex_returning*day_j+(as.numeric(push)|bird_ID) + (1|nest_ID), eb)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

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

			   if(PNG == TRUE){dev.off()}
