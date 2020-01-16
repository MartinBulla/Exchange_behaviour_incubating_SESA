### RESULTS section Exchange procedure - how leaving the nest? ###

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

	# adjust variable and prepare datasets
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
		dd$sex_returning = ifelse(dd$sex == 'f','m','f')

		# add colors for Figure Temp
		  col_bef = col_m
		  col_aro = col_f
		  col_dur = 'dark green'
		  xx = data.frame(left_type = c('1 before presence','2 while around','3 during exchange'), colr = c(col_bef,col_aro,col_dur), stringsAsFactors = FALSE)
		  dd = merge(dd,xx, by = 'left_type')
		
		dd = dd[!is.na(dd$arrival),]
		ex = dd[dd$left_before_presence=="n",]
		#ex_$obs_ID[is.na(ex_$push)]
		ex_= ex[!is.na(ex$pushoff_int),]
		ex_$push = ifelse(ex_$pushoff_int==3,'y','n')
		ex_$push01 = ifelse(ex_$push == 'y', 1, 0)
		ex_$both=as.numeric(difftime(ex_$dt_left,ex_$dt_arrive, 'secs')) # time when both present in the vicinity of the nest
		eb = ex_[ex_$both>=0,] # only for cases where incubating bird left after the returning initiated the exchange
		#eb$sex = ifelse(eb$sex == 'f', 'm','f') # sex of pushing parent
		eb_ = eb[!is.na(eb$call_int_1),]

		e= ex_[!is.na(ex_$current_bout),]
		dx = ex_[!is.na(ex_$with_calling),]
		dx$w_call = ifelse(dx$with_calling == 'y', 1, 0)
		dx_ = dx[which(dx$with_calling == 'y'),]
		dx_ = dx_[-which(is.na(dx_$o_replies)),]
		dx_$reply = ifelse(is.na(dx_$o_replies), NA, ifelse(dx_$o_replies == 'y', 1, 0))

# distributions - for cases without enclosure 
	#summary(factor(dd$type_l))
	dn=dd[dd$cage=='n',]
	summary(factor(dn$type_l))

	length(dn$type_l[dn$type_l=='f'])/length(dn$type_l) # %
	length(unique(dn$nest))  # number of nests

	length(unique(dn$nest[dn$type_l=='f']))
				length(unique(dn$nest[dn$type_l=='wf']))
	length(unique(dn$nest[dn$type_l=='w']))


	ggplot(dn,aes(y = current_bout, x=type_l))+geom_boxplot()
# distributions - called while leaving?
	# cases without enclosure
		dn=dd[dd$cage=='n',]
		dnc = dn[!is.na(dn$call_left),]
		summary(factor(dnc$call_left))
		length(dnc$call_left[dnc$call_left%in%c('y')])/length(dnc$call_left[])

		table(dnc$call_left, dnc$sex)
		length(dnc$call_left[dnc$call_left%in%c('y') & dnc$sex == 'm'])/length(dnc$call_left[ dnc$sex == 'm'])
		length(dnc$call_left[dnc$call_left%in%c('y') & dnc$sex == 'f'])/length(dnc$call_left[ dnc$sex == 'f'])
	# all cases
		dc = dd[!is.na(dd$call_left),]
		summary(factor(dc$call_left))
		length(dc$call_left[dc$call_left%in%c('y')])/length(dc$call_left[])

		table(dc$call_left, dc$sex)
		length(dc$call_left[dc$call_left%in%c('y') & dc$sex == 'm'])/length(dc$call_left[ dc$sex == 'm'])
		length(dc$call_left[dc$call_left%in%c('y') & dc$sex == 'f'])/length(dc$call_left[ dc$sex == 'f'])

		dc = dd[-which(is.na(dd$call_left)),]
		table(dc$call_left,dc$sex)
# Table S8
	# prepare table data
		dc = dd[!is.na(dd$call_left),]
		dc$c_left = ifelse(dc$call_left=='y', 1,0)
		nrow(dc)
		length(unique(dc$bird_ID))
		length(unique(dc$nest_ID))
		m = glmer(c_left ~ sex*scale(day_j) + (1|bird_ID) + (1|nest_ID), dc, family = 'binomial')
		#m = glmer(c_left ~ sex + (1|bird_ID) + (1|nest_ID), dc, family = 'binomial')
		#plot(allEffects(m))
		#summary(glht(m))
		#ggplot(dc, aes(y = c_left, x = sex))+geom_boxplot()
	
		pred=c('Intercept (f)','Sex(m)', 'Day', 'Day:sex')
			dep = 'call_left'
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
	# create xlsx table
		sname = 'Table_S8'
		tmp = write_xlsx(o1, paste0(ta,sname,'.xlsx'))
		openFile(tmp)
# model assumptions
			dc = dd[!is.na(dd$call_left),]
			dc$c_left = ifelse(dc$call_left=='y', 1,0)

			m = glmer(c_left ~ sex*scale(day_j) + (1|bird_ID) + (1|nest_ID), dc, family = 'binomial')
			if(PNG == TRUE){png(paste(outdir,"model_ass/Table_S8.png", sep=""), width=6,height=9,units="in",res=600)}else{dev.new(width=6,height=9)}	
			  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
			  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
			  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
			  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')
			  qqline(resid(m))

			  plot(fitted(m), jitter(dc$c_left, amount=0.05), xlab="Fitted values", ylab="Probability of left", las=1, cex.lab=1.2, cex=0.8)
			  abline(0,1, lty=3)
			  t.breaks <- cut(fitted(m), quantile(fitted(m)))
			  means <- tapply(dc$c_left, t.breaks, mean)
			  semean <- function(x) sd(x)/sqrt(length(x))
			  means.se <- tapply(dc$c_left, t.breaks, semean)
			  points(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means, pch=16, col="orange")
			  segments(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means-2*means.se, quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means+2*means.se,lwd=2, col="orange")

			  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
			  qqline(unlist(ranef(m)$nest_ID [1]))

			  #qqnorm(unlist(ranef(m)$nest_ID[2]), main = "ran slope",col='red')
			  #qqline(unlist(ranef(m)$nest_ID[2]))

			  scatter.smooth(resid(m)~dc$day);abline(h=0, lty=2, col='red')
			  #scatter.smooth(resid(m)~dc$sex);abline(h=0, lty=2, col='red')
			  boxplot(resid(m)~dc$sex);abline(h=0, lty=2, col='red')

			  mtext("glmer(c_left ~ sex*scale(day_j) + (1|bird_ID) + (1|nest_ID), dc, family = 'binomial')", side = 3, line = 0.5, cex=0.8,outer = TRUE)

			    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
			  # spatial autocorrelations - nest location
				spdata=data.frame(resid=resid(m), x=dc$lon, y=dc$lat)
					spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
					#cex_=c(1,2,3,3.5,4)
					cex_=c(1,1.5,2,2.5,3)
					spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
					plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
					legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)

					plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
					plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))

				if(PNG == TRUE){dev.off()}

# END				