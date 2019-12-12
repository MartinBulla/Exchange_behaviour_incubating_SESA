# INFO
	# SEX in the loaded data refers to the INCUBATING PARENT (in time series of the one showing the behaviour

##### add this to Supp table ri$estimate_r = paste(ri$estimate_r,"%",sep='')
##### check descriptive part - calling correlations
##### check and likely delete ex, and ex_ datasets

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

# RESULTS section Exchange procedure
	# DECIDE WHETHER TO INVOLVE DATETIME LEFT
	# NOTE that sex factor line is silanced out now
  # run first
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
		#dd$sex = as.factor(dd$sex)


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

  # durations
		# descriptive
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
		    # model assumptions
				# current bout
							m=glmer(push01 ~ sex*scale(current_bout)+ (scale(current_bout)|bird_ID)+(1|nest_ID), family='binomial',e)
								#png(paste(out_,"model_ass/Supplementary_Table_6.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')
									  qqline(resid(m))

									  plot(fitted(m), jitter(e$push_, amount=0.05), xlab="Fitted values", ylab="Probability of left", las=1, cex.lab=1.2, cex=0.8)
									  abline(0,1, lty=3)
									  t.breaks <- cut(fitted(m), quantile(fitted(m)))
									  means <- tapply(e$push_, t.breaks, mean)
									  semean <- function(x) sd(x)/sqrt(length(x))
									  means.se <- tapply(e$push_, t.breaks, semean)
									  points(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means, pch=16, col="orange")
									  segments(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means-2*means.se, quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means+2*means.se,lwd=2, col="orange")

									  e$fit = fitted(m)
									  ee = e[e$fit<0.01,]
									  plot(ee$fit, jitter(ee$push_, amount=0.05), xlab="Fitted values", ylab="Probability of left", las=1, cex.lab=1.2, cex=0.8)
									  abline(0,1, lty=3)
									  points(quantile(fitted(m),c(0.125,0.375,0.625,0.875))[1:3], means[1:3], pch=16, col="orange")
									  segments(quantile(fitted(m),c(0.125,0.375,0.625,0.875))[1:3], means[1:3]-2*means.se[1:3], quantile(fitted(m),c(0.125,0.375,0.625,0.875))[1:3], means[1:3]+2*means.se[1:3],lwd=2, col="orange")

									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))
									  qqnorm(unlist(ranef(m)$bird_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$bird_ID [1]))
									  qqnorm(unlist(ranef(m)$bird_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$bird_ID[2]))

									  scatter.smooth(resid(m)~e$current_bout);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~e$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~e$sex);abline(h=0, lty=2, col='red')

									  mtext("m=glmer(push ~ sex*scale(current_bout)+ (scale(current_bout)|bird_ID)+(1|nest_ID), family='binomial',e)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

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

							dev.off()
				# day of incubation
							m=glmer(push01 ~ sex*scale(day_j)+ (scale(day_j)|bird_ID), family='binomial',e)
									#png(paste(out_,"model_ass/Supplementary_Table_6.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									 scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')
									  qqline(resid(m))

									  plot(fitted(m), jitter(e$push_, amount=0.05), xlab="Fitted values", ylab="Probability of left", las=1, cex.lab=1.2, cex=0.8)
									  abline(0,1, lty=3)
									  t.breaks <- cut(fitted(m), quantile(fitted(m)))
									  means <- tapply(e$push_, t.breaks, mean)
									  semean <- function(x) sd(x)/sqrt(length(x))
									  means.se <- tapply(e$push_, t.breaks, semean)
									  points(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means, pch=16, col="orange")
									  segments(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means-2*means.se, quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means+2*means.se,lwd=2, col="orange")

									  e$fit = fitted(m)
									  ee = e[e$fit<0.01,]
									  plot(ee$fit, jitter(ee$push_, amount=0.05), xlab="Fitted values", ylab="Probability of left", las=1, cex.lab=1.2, cex=0.8)
									  abline(0,1, lty=3)
									  points(quantile(fitted(m),c(0.125,0.375,0.625,0.875))[1:3], means[1:3], pch=16, col="orange")
									  segments(quantile(fitted(m),c(0.125,0.375,0.625,0.875))[1:3], means[1:3]-2*means.se[1:3], quantile(fitted(m),c(0.125,0.375,0.625,0.875))[1:3], means[1:3]+2*means.se[1:3],lwd=2, col="orange")


									  qqnorm(unlist(ranef(m)$bird_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$bird_ID [1]))
									  qqnorm(unlist(ranef(m)$bird_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$bird_ID[2]))

									  scatter.smooth(resid(m)~e$day_j);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~e$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~e$sex);abline(h=0, lty=2, col='red')

									  mtext("m=glmer(push ~ sex*scale(day)+ (scale(day)|bird_ID)+(1|nest_ID), family='binomial',e)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

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

							dev.off()
		
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
				
		# Table S3
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
		  # model assumptions
				# presence before exchange
					 m = lmer(pa~ sex*day_j+(day_j|nest_ID), dd)


								#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
								  dev.new(width=6,height=9)
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
								  scatter.smooth(resid(m)~dd$sex);abline(h=0, lty=2, col='red')
								  boxplot(resid(m)~dd$sex);abline(h=0, lty=2, col='red')

								   mtext("lmer(log(pad)~ sex*day_j+(day_j|nest_ID), dd)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

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

						dev.off()
				# arrival
					m = lmer(log(arrival) ~ left_type + sex*day_j+(day_j|nest_ID), dd)

								  dev.new(width=6,height=9)
								  #png(paste(outdir,"model_ass/Supplementary_Table_3b_.png", sep=""), width=6,height=9,units="in",res=600)
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
								  scatter.smooth(resid(m)~dd$sex);abline(h=0, lty=2, col='red')
								  boxplot(resid(m)~dd$sex);abline(h=0, lty=2, col='red')

								  scatter.smooth(resid(m)~dd$left_type);abline(h=0, lty=2, col='red')
								  boxplot(resid(m)~dd$left_type);abline(h=0, lty=2, col='red')
								   mtext("lmer(log(arrival) ~ left_type + sex*day_j+(day_j|nest_ID), dd)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

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

						dev.off()
				# gap
					 m = lmer(log(gap)~ sex*day_j+(day_j|nest_ID), dd)
								 dev.new(width=6,height=9)
								#png(paste(outdir,"model_ass/Supplementary_Table_3c.png", sep=""), width=6,height=9,units="in",res=600)

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
								  scatter.smooth(resid(m)~dd$sex);abline(h=0, lty=2, col='red')
								  boxplot(resid(m)~dd$sex);abline(h=0, lty=2, col='red')

								   mtext("lmer(log(gap)~ sex*day_j+(day_j|nest_ID), dd)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

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

						dev.off()
				# presence before exchange
					 m = lmer(both ~ sex*push + sex*day_j+(as.numeric(push)|bird_ID) + (1|nest_ID), eb)
							 dev.new(width=6,height=9)
							#png(paste(outdir,"model_ass/Supplementary_Table_3d.png", sep=""), width=6,height=9,units="in",res=600)
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
								  scatter.smooth(resid(m)~eb$sex);abline(h=0, lty=2, col='red')
								  boxplot(resid(m)~eb$sex);abline(h=0, lty=2, col='red')

								  scatter.smooth(resid(m)~eb$push);abline(h=0, lty=2, col='red')
								  boxplot(resid(m)~eb$push);abline(h=0, lty=2, col='red')

								   mtext("lmer(both ~ sex*push + sex*day_j+(as.numeric(push)|bird_ID) + (1|nest_ID), eb)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

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

						dev.off()
				# presence before exchange log - worse fit
					 m = lmer(log(both) ~ sex*push + sex*day_j+(as.numeric(push)|bird_ID) + (1|nest_ID), eb)
							 dev.new(width=6,height=9)
							#png(paste(outdir,"model_ass/Supplementary_Table_3d_log.png", sep=""), width=6,height=9,units="in",res=600)
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
								  scatter.smooth(resid(m)~eb$sex);abline(h=0, lty=2, col='red')
								  boxplot(resid(m)~eb$sex);abline(h=0, lty=2, col='red')

								  scatter.smooth(resid(m)~eb$push);abline(h=0, lty=2, col='red')
								  boxplot(resid(m)~eb$push);abline(h=0, lty=2, col='red')

								   mtext("lmer(log(both) ~ sex*push + sex*day_j+(as.numeric(push)|bird_ID) + (1|nest_ID), eb)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

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

						dev.off()
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

			# prepare model predictions
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
		  # prepare for plotting
				k=0.1
				kk=k*2# distance for boxplots
				kkk=k*2 # distance for points
				ebb = eb
				ebb$sex = ifelse(ebb$sex == 'f', 'm','f') # sex of pushing parent
				ebb$push_sex=ifelse(ebb$push=='y',ifelse(ebb$sex=='f', 1-k,k+1),
								ifelse(ebb$sex=='f', 4-k,k+4))
				x = ebb
				x$col_=ifelse(x$sex=='f','#FCB42C', '#535F7C')
				x$at=ifelse(x$push=='y',ifelse(x$sex=='f', 1-kkk,2+kkk),
								ifelse(x$sex=='f', 3.7-kkk,4.7+kkk))


				}
		  # prepare model predictions
				m = lmer(both ~ sex*push + sex*day_j+(as.numeric(as.factor(push))|bird_ID) + (1|nest_ID), ebb)
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))

				# values to predict for
					newD=data.frame(push=c('y','n'),
									sex = 0.5,
									day_j = mean(ebb$day_j)
									)

				# exactly the model which was used has to be specified here
				X <- model.matrix(~ sex*push + sex*day_j,data=newD)

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

  # calling - how laud is the exchange?
		# distributions
			# calling while initiating nest exchange
				# overall
				summary(factor(dd$with_calling))
				ddn = dd[!is.na(dd$with_calling),]
				length(ddn$with_calling[ddn$with_calling=='y'])/length(ddn$with_calling)

				# only for cases where incubating bird left the nest only after returning one appeared
				summary(factor(dd$with_calling[dd$left_type %in% c('2 while around', '3 during exchange')]))
				ddn2 = ddn[ddn$left_type %in% c('2 while around', '3 during exchange')]
				length(ddn2$with_calling[ddn2$with_calling=='y'])/length(ddn2$with_calling)

				# sexes
				table(ddn$with_calling,ddn$sex_returning)
				table(ddn2$with_calling,ddn2$sex_returning)

				length(ddn$with_calling[ddn$with_calling=='y' & ddn$sex_returning=='f'])/length(ddn$with_calling[ddn$sex_returning=='f'])
				length(ddn$with_calling[ddn$with_calling=='y' & ddn$sex_returning=='m'])/length(ddn$with_calling[ddn$sex_returning=='m'])
				length(ddn2$with_calling[ddn2$with_calling=='y' & ddn2$sex_returning=='f'])/length(ddn2$with_calling[ddn2$sex_returning=='f'])
				length(ddn2$with_calling[ddn2$with_calling=='y' & ddn2$sex_returning=='m'])/length(ddn2$with_calling[ddn2$sex_returning=='m'])
				
				#ddn2$w_call = ifelse(ddn2$with_calling == 'y', 1, 0)
				#m = glmer(w_call ~ sex_returning*scale(day_j) + (1|bird_ID) + (1|nest_ID), ddn2, family = 'binomial')
				#m = glmer(w_call ~ sex_returning + (1|bird_ID) + (1|nest_ID), ddn2, family = 'binomial')
				#plot(allEffects(m))
				#summary(glht(m))

			# reply (has 1 observation less, as reply was NA)
				dr= dd[dd$with_calling %in% c('y') & !is.na(dd$o_replies) & dd$left_type %in% c('3 during exchange')]
				summary(factor(dr$o_replies))

				dry = dr[dr$o_replies %in% c('y')]
				nrow(dry)/nrow(dr) # % of cases with reply
				summary(factor(dry$sex))
				length(unique(dry$nest[dry$sex == 'f']))
				length(unique(dry$nest[dry$sex == 'm']))
		    # returning parent is quiet
		    	# all cases
		    	x = dd[-which(is.na(dd$with_calling) | is.na(dd$call_c_int) | is.na(dd$call_int_c2) | is.na(dd$call_int_c3)),]
				nrow(x)
					
				nrow(x[which(x$with_calling %in% c('n') & x$call_c_int %in% c(0) & x$call_int_c2 %in% c(0)  & x$call_int_c3 %in% c(0)),])/nrow(x)
				
				# cases were incubating left only after the returning arrived
				x = dd[-which(is.na(dd$with_calling) | is.na(dd$call_c_int) | is.na(dd$call_int_c2) | is.na(dd$call_int_c3)),]
				x = x[x$left_before_presence=="n",]
				nrow(x)
				nrow(x[which(x$with_calling %in% c('n') & x$call_c_int %in% c(0) & x$call_int_c2 %in% c(0)  & x$call_int_c3 %in% c(0)),])/nrow(x)

		   # correlations - TO BE CHECKED
				cor(subset(ex_,select = c('current_bout','next_bout')),use="pairwise.complete.obs", method="pearson")
				cor(subset(ex_,select = c('current_bout','next_bout')),use="pairwise.complete.obs", method="spearman")

				f = ex_[-which(is.na(ex_$call_c_int) | is.na(ex_$call_o_int)),]
				cor(subset(f,select = c('day_j','call_o_int')),use="pairwise.complete.obs", method="spearman")
				cor(subset(f,select = c('day_j','call_o_int')),use="pairwise.complete.obs", method="pearson")
				  	
				ggplot(di, aes(x = call_int, fill = as.factor(call_int))) +
				geom_bar(position=position_dodge()) +
				facet_wrap(~type, nrow=5) +
				scale_fill_brewer(palette = "Blues")

				ggplot(di, aes(y = call_int, x = as.numeric(as.factor(type)), col=factor(obs_ID))) + geom_line()

				densityplot(~ex_$call_int_1)

				ggplot(ex_,aes(y=call_int_1, x=day_j, fill=sex, col=sex))+geom_point()+stat_smooth()
				ggplot(ex_,aes(y=call_int_1, x=day_j, fill=sex, col=sex))+geom_point()+stat_smooth(method="lm")
				ggplot(ex_[ex_$day_j<19,],aes(y=call_int_1, x=day_j, fill=sex, col=sex))+geom_point()+stat_smooth(method="lm")
				ggplot(ex_[ex_$day_j<19,],aes(y=call_int_1, x=day_j, fill=sex, col=sex))+geom_point()+stat_smooth()

				ggplot(ex_[ex_$day_j<19,],aes(y=call_int_1, x=day_j))+geom_point()+stat_smooth(method="lm")
				ggplot(ex_[ex_$day_j<22,],aes(y=call_int_1, x=day_j))+geom_point()+stat_smooth(method="lm")
				ggplot(ex_[ex_$day_j<22,],aes(y=call_int_1, x=day_j))+geom_point()+stat_smooth()
				ggplot(ex_,aes(y=call_int_1, x=day_j))+geom_point()+stat_smooth()

				ggplot(ex_,aes(y=call_int_1, x=day_j, col=nest))+geom_point()+stat_smooth(method="lm", se=FALSE)

	    # Figure 3abc
			# prepare labels
				labels_ <- c(
	                    `1 both present` = "From initiation to leaving",
	                    `2 exchange gap` = "Exchange gap",
	                    `3 after on nest` = "After on nest"
	                    )
				labels_abc <- c(
	                    `1 both present` = "a",
	                    `2 exchange gap` = "b",
	                    `3 after on nest` = "c"
	                    )
			# prepare data
		    	dp = dd[-which(is.na(dd$call_c_int)|is.na(dd$call_int_c2)|is.na(dd$call_int_c3) | dd$left_before_presence=="y"),]
				d1 = subset(dp,select = c('obs_ID','sound_ok','nest_ID','bird_ID', 'sex', 'day_j', 'call_int_1'))
					colnames(d1)[7] = 'call_int'
					d1$type = '1 both present'
					d1$who = 'both'
					#d1$obs_ID[is.na(d1$call_int) & d1$sound_ok == 'y']
					#d1[is.na(d1$call_int), ]
				d01 = subset(dp,select = c('obs_ID','sound_ok','nest_ID','bird_ID', 'sex', 'day_j', 'call_c_int'))
					colnames(d01)[7] = 'call_int'
					d01$type = '1 both present'
					d01$who = 'returning'
					#d1$obs_ID[is.na(d1$call_int) & d1$sound_ok == 'y']
					#d1[is.na(d1$call_int), ]
				d02 = subset(dp,select = c('obs_ID','sound_ok','nest_ID','bird_ID', 'sex', 'day_j', 'call_o_int'))
					colnames(d02)[7] = 'call_int'
					d02$type = '1 both present'
					d02$who = 'incubating'
					#d1$obs_ID[is.na(d1$call_int) & d1$sound_ok == 'y']
					#d1[is.na(d1$call_int), ]
				d2 = subset(dp,select = c('obs_ID','sound_ok','nest_ID','bird_ID', 'sex', 'day_j', 'call_int_c2'))
					colnames(d2)[7] = 'call_int'
					d2$type = '2 exchange gap'
					d2$who = 'returning'
					#d2$obs_ID[is.na(d2$call_int) & d2$sound_ok == 'y']
					#d2[is.na(d2$call_int), ]

				d3 = subset(dp,select = c('obs_ID','sound_ok','nest_ID','bird_ID', 'sex', 'day_j', 'call_int_c3'))
					colnames(d3)[7] = 'call_int'
					d3$type = '3 after on nest'
					d3$who = 'returning'
					#d3$obs_ID[is.na(d3$call_int) & d3$sound_ok == 'y']
					#d3[is.na(d2$call_int), ]
				di = rbind(d1,d01,d02,d2,d3)
				di = di[!is.na(di$call_int),]
				#di$left_before = d$left_before_presence [match(di$obs_ID, d$obs_ID)]
 			# plot
 				#geom_text(aes(x, y,label=lab),	data=data.frame(x=3.5, y=100, who = c("both"), lab=c("a", "b", "c"), type=c("1 both present","2 exchange gap","3 after on nest")), size=2, colour = "grey30")
				dev.new(width=1.85+1,height=1.5*2)
				ggplot(di[!di$who =='both',], aes(x = call_int, fill = who)) +
					geom_bar(position=position_dodge()) +
					facet_wrap(~type, nrow=5, labeller = as_labeller(labels_)) +
					ylab("Number of observations") +
					xlab("Calling intensity") +
					coord_cartesian(ylim = c(0, 100))+
					guides(fill=guide_legend(title="Which parent:")) +
					scale_fill_manual(values=c('#FCB42C','#535F7C')) +
					theme_light()+
										theme(	axis.line=element_line(colour="grey70", size=0.25),
												#panel.border=element_rect(colour="white"),
												panel.border=element_rect(colour="grey70", size=0.25),
												panel.grid = element_blank(),

												axis.title=element_text(size=7, colour="grey30"),
												axis.title.y = element_text(vjust=1),
												axis.title.x = element_text(vjust=0.2),
												axis.text=element_text(size=6),# margin=units(0.5,"mm")),
												axis.ticks.length=unit(0.5,"mm"),
												#axis.ticks.margin,

												strip.text.x =element_text(size = 6, color="grey30",  margin=margin(1,1,1,1,"mm")), #grey50
												strip.background=element_rect(fill="grey99",colour="grey70", size=0.25),
												#strip.background = element_blank(),
												#strip.text = element_blank(),
												panel.margin = unit(0, "mm"),
												#legend.position="none"
												legend.background=element_rect(colour="white"),
												legend.key=element_rect(fill="grey99", colour="white"),
												legend.text=element_text(size=7, colour="grey30"),
												legend.title=element_text(size=7, colour="grey30")
												)
				if(PNG==TRUE){ggsave(paste(outdir,"Figure_3abc.png", sep=""),width=1.85+1,height=1.5*2, units = "in")}
					#ggsave(paste(outdir,"Figure_4.eps", sep=""),width=1.85+1,height=1.5*2, units = "in")
		    # legend
		    	dp = dd[-which(is.na(dd$call_c_int)|is.na(dd$call_int_c2)|is.na(dd$call_int_c3) | dd$left_before_presence=="y"),]
		    	nrow(dp)
		    	length(unique(dp$nest))
	    # Figure 3d
			ex = dd[-which(is.na(dd$call_c_int)|is.na(dd$call_int_c2)|is.na(dd$call_int_c3) | dd$left_before_presence=="y"),]
			ex$nn=1
			a = ddply(xx,.(call_c_int,call_int_c2,call_int_c3, sex), summarise, n=sum(nn))
			a$sex = ifelse(a$sex == 'f', 'Male', 'Female') # to make sex of returning bird

			if(PNG == TRUE) {
					png(paste(outdir,"Alluvial_sex.png", sep=""), width=5,height=2.5,units="in",res=600)
					}else{
					dev.new(width=5,height=2.5)
					#dev.new(width=3.55,height=1.75)
					}

			alluvial(a[,1:3], freq=a$n,
				axis_labels = c('Initiation\nto leaving','Exchange\ngap', 'After\nexchange'),
				col = ifelse(a$sex == "Female", col_f, col_m), alpha = 0.7, #change alpha to 1 if printing to esp
				border="white",
				#border = ifelse(tit$Survived == "Yes", "orange", "grey"),
				#hide = tit$Freq == 0,
				cex = 0.5, cex.axis = 0.7
				)
			 if(PNG == TRUE) {dev.off()}

		# Table S4
			# prepare table data
				# a. calling while arriving
					dx = dd[dd$left_before_presence=="n" & !is.na(dd$with_calling),]
					dx$w_call = ifelse(dx$with_calling == 'y', 1, 0)
					nrow(dx)
					length(unique(dx$bird_ID))
					length(unique(dx$nest_ID))
					m = glmer(w_call ~ sex_returning*scale(day_j) + (1|bird_ID) + (1|nest_ID), dx, family = 'binomial')
							# binomial gives same results
								#dd$pa_bin=ifelse(dd$pa == 0.001, 0,1)
								#m = glmer(pa_bin~ sex*day_j+(day_j|nest_ID),family='binomial', dd)
					pred=c('Intercept (f)','sex_returning(m)', 'Day', 'Day:sex')
					dep = 'calling while arriving (bin)'
					mod = 1
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)

				    # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]

				   # Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
						ri$estimate_r = paste(ri$estimate_r,"%",sep='')
					o1=rbind(oii,ri)
				# b. reply
					dr= dd[dd$with_calling %in% c('y') & !is.na(dd$o_replies) & dd$left_type %in% c('3 during exchange')]
					dr$reply = ifelse(dr$o_replies == 'y', 1, 0)
					nrow(dr)
					length(unique(dr$bird_ID))
					length(unique(dr$nest_ID))
					m = glmer(reply ~ sex*scale(day_j) + (1|bird_ID) + (1|nest_ID), dr, family = 'binomial')
					#m = glm(reply ~ scale(day_j) , dr, family = 'binomial') # gives same results
					pred=c('Intercept (inc f)','Sex(m)','Day','Sex:Day')
					dep = 'reply (bin)'
					mod = 2
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)
				 	# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]
					# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					o2=rbind(oii, ri)
				# c. calling coming ~ calling incubating
					f = dd[-which(is.na(dd$call_c_int) | is.na(dd$call_o_int) | dd$left_before_presence=="y"),]
					nrow(f)
					length(unique(f$bird_ID))
					length(unique(f$nest_ID))
					
					m = lmer(call_c_int ~ sex + scale(call_o_int)*sex + scale(day_j)*sex + (call_o_int|bird_ID) + (1|nest_ID), f)

					pred=c('Intercept (f)','Sex(m)', 'Call_incub', 'Day', 'Call_incub:sex','Day:sex')
					dep = 'call_c_int'
					mod = 3
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)
				 	# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]

					# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
						ri$estimate_r = paste(ri$estimate_r,"%",sep='')
					o3=rbind(oii,ri)
				# d. exchange gap calling
					f = dd[-which(is.na(dd$call_c_int) | is.na(dd$call_int_c2) | dd$left_before_presence=="y"),]
					nrow(f)
					length(unique(f$bird_ID))
					length(unique(f$nest_ID))
					
					m = lmer(call_int_c2 ~ sex_returning + scale(call_c_int)*sex_returning + scale(day_j)*sex_returning + (call_c_int|bird_ID) + (1|nest_ID), f)
					pred=c('Intercept (f)','sex_returning(m)', 'Call_com', 'Day', 'Call_com:sex','Day:sex')
					dep = 'call during gap'
					mod = 4
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)
				 	# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]
					# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
						ri$estimate_r = paste(ri$estimate_r,"%",sep='')
					o4=rbind(oii,ri)
				# e. after exchange
					f = dd[-which(is.na(dd$call_c_int) | is.na(dd$call_int_c3) | dd$left_before_presence=="y"),]
					nrow(f)
					length(unique(f$bird_ID))
					length(unique(f$nest_ID))

					m = lmer(call_int_c3 ~ sex_returning + scale(call_c_int)*sex_returning + scale(day_j)*sex_returning + (call_c_int|bird_ID) + (1|nest_ID), f)
					pred=c('Intercept (f)','sex_returning(m)', 'Call_com', 'Day', 'Call_com:sex','Day:sex')
					dep = 'call after exchange'
					mod = 5
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)
				 	# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]
					# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
						ri$estimate_r = paste(ri$estimate_r,"%",sep='')
					o5=rbind(oii,ri)
			# create xlsx table
				o=rbind(o1,o2,o3,o4,o5)
				sname = 'Table_S4'
				tmp = write_xlsx(o, paste0(ta,sname,'.xlsx'))
				openFile(tmp)
		# model assumptions
				# a. calling while arriving
						m = glmer(w_call ~ sex*scale(day_j) + (1|bird_ID) + (1|nest_ID), dx, family = 'binomial')

						scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
						scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')

						plot(fitted(m), jitter(dx$w_call, amount=0.05), xlab="Fitted values", ylab="Probability of calling", las=1, cex.lab=1.2, cex=0.8)
							abline(0,1, lty=3)
							t.breaks <- cut(fitted(m), seq(0,1, by=0.1))
							means <- tapply(dx$w_call, t.breaks, mean)
							semean <- function(x) sd(x)/sqrt(length(x))
							means.se <- tapply(dx$w_call, t.breaks, semean)
							points(seq(0.05, 0.95, by=0.1), means, pch=16, col="orange")
							segments(seq(0.05, 0.95, by=0.1), means-2*means.se, seq(0.05, 0.95,by=0.1), means+2*means.se,lwd=2, col="orange")

						plot(fitted(m), jitter(dx$w_call, amount=0.05), xlab="Fitted values", ylab="Probability of presence", las=1, cex.lab=1.2, cex=0.8) # what to do with the miss-fit
							abline(0,1, lty=3)
							t.breaks <- cut(fitted(m), seq(0,1, by=0.2))
							means <- tapply(dx$w_call, t.breaks, mean)
							semean <- function(x) sd(x)/sqrt(length(x))
							means.se <- tapply(dx$w_call, t.breaks, semean)
							points(seq(0.05, 0.95, by=0.2), means, pch=16, col="orange")
							segments(seq(0.05, 0.95, by=0.2), means-2*means.se, seq(0.05, 0.95,by=0.2), means+2*means.se,lwd=2, col="orange")
						qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')  # what to do with the miss-fit
							qqline(resid(m))
						qqnorm(unlist(ranef(m)$nest[1]), main = "nest",col='red')
						qqline(unlist(ranef(m)$nest[1]))

						qqnorm(unlist(ranef(m)$bird_ID[1]), main = "bird",col='red')
						qqline(unlist(ranef(m)$bird_ID[1]))

						scatter.smooth(resid(m)~dx$day_j);abline(h=0, lty=2, col='red')
						boxplot(resid(m)~dx$sex, xlab = 'sex');abline(h=0, lty=2, col='red')
						scatter.smooth(resid(m)~dx$sex);abline(h=0, lty=2, col='red')

						acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))

						# spatial autocorrelations - nest location
							spdata=data.frame(resid=resid(m), x=dx$lon, y=dx$lat)
								spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
								#cex_=c(1,2,3,3.5,4)
								cex_=c(1,1.5,2,2.5,3)
								spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
								plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)

								plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8),xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
								plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8), xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
								#mtext("glmer(success_bin~prop_ip+uni_last+(1|sp),data=g,family='binomial')", side = 3, line = -1.2, cex=0.8,outer = TRUE)
				# b. reply
					dx__=dx_[!is.na(dx_$reply),]
					m = glmer(reply ~ sex*scale(day_j) + (1|bird_ID) + (1|nest_ID), dx__, family = 'binomial')

					scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
					scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')

					plot(fitted(m), jitter(dx__$reply, amount=0.05), xlab="Fitted values", ylab="Probability of calling", las=1, cex.lab=1.2, cex=0.8)
						abline(0,1, lty=3)
						t.breaks <- cut(fitted(m), seq(0,1, by=0.1))
						means <- tapply(dx__$reply, t.breaks, mean)
						semean <- function(x) sd(x)/sqrt(length(x))
						means.se <- tapply(dx__$reply, t.breaks, semean)
						points(seq(0.05, 0.95, by=0.1), means, pch=16, col="orange")
						segments(seq(0.05, 0.95, by=0.1), means-2*means.se, seq(0.05, 0.95,by=0.1), means+2*means.se,lwd=2, col="orange")
					plot(fitted(m), jitter(dx__$reply, amount=0.05), xlab="Fitted values", ylab="Probability of presence", las=1, cex.lab=1.2, cex=0.8) # what to do with the miss-fit
						abline(0,1, lty=3)
						t.breaks <- cut(fitted(m), seq(0,1, by=0.2))
						means <- tapply(dx__$reply, t.breaks, mean)
						semean <- function(x) sd(x)/sqrt(length(x))
						means.se <- tapply(dx__$reply, t.breaks, semean)
						points(seq(0.05, 0.95, by=0.2), means, pch=16, col="orange")
						segments(seq(0.05, 0.95, by=0.2), means-2*means.se, seq(0.05, 0.95,by=0.2), means+2*means.se,lwd=2, col="orange")

					qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')  # what to do with the miss-fit
					qqline(resid(m))
					qqnorm(unlist(ranef(m)$nest[1]), main = "nest",col='red')
					qqline(unlist(ranef(m)$nest[1]))
					qqnorm(unlist(ranef(m)$bird_ID[1]), main = "bird",col='red')
					qqline(unlist(ranef(m)$bird_ID[1]))
					scatter.smooth(resid(m)~dx__$day_j);abline(h=0, lty=2, col='red')
					boxplot(resid(m)~dx__$sex, xlab = 'sex');abline(h=0, lty=2, col='red')
					scatter.smooth(resid(m)~dx__$sex);abline(h=0, lty=2, col='red')

					acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))

					# spatial autocorrelations - nest location
						spdata=data.frame(resid=resid(m), x=dx__$lon, y=dx__$lat)
						spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
							#cex_=c(1,2,3,3.5,4)
						cex_=c(1,1.5,2,2.5,3)
						spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
						plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
						legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)

						plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8),xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
						plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8), xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
						#mtext("glmer(success_bin~prop_ip+uni_last+(1|sp),data=g,family='binomial')", side = 3, line = -1.2, cex=0.8,outer = TRUE)
				# c. calling coming ~ calling incubating
						f = ex_[-which(is.na(ex_$call_c_int) | is.na(ex_$call_o_int)),]
						m = lmer(call_c_int ~ sex + scale(call_o_int)*sex + scale(day_j)*sex + (call_o_int|bird_ID) + (1|nest_ID), f)
									#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									  par(mfrow=c(5,3))

									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')
									  qqline(resid(m))

									  qqnorm(unlist(ranef(m)$bird_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$bird_ID [1]))

									  qqnorm(unlist(ranef(m)$bird_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$bird_ID[2]))

									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))


									  scatter.smooth(resid(m)~f$day_j);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$call_o_int);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~f$sex);abline(h=0, lty=2, col='red')

									   mtext("lmer(call_int_c3 ~ sex + scale(call_o_int) + scale(day_j) + (call_o_int|bird_ID) + (1|nest_ID), f)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

									    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=f$lon, y=f$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)

											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))

							dev.off()
				# d. exchange gap calling
						f = ex_[-which(is.na(ex_$call_c_int) | is.na(ex_$call_int_c2)),]
						m = lmer(call_int_c2 ~ sex + scale(call_c_int)*sex + scale(day_j)*sex + (call_c_int|bird_ID) + (1|nest_ID), f)
									#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									  par(mfrow=c(5,3))

									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')
									  qqline(resid(m))

									  qqnorm(unlist(ranef(m)$bird_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$bird_ID [1]))

									  qqnorm(unlist(ranef(m)$bird_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$bird_ID[2]))

									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))


									  scatter.smooth(resid(m)~f$day_j);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$call_c_int);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~f$sex);abline(h=0, lty=2, col='red')

									   mtext("lmer(call_int_c2 ~ sex + scale(call_c_int) + scale(day_j) + (call_c_int|bird_ID) + (1|nest_ID), f)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

									    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=f$lon, y=f$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)

											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))

							dev.off()
				# e. after exchange
						f = ex_[-which(is.na(ex_$call_c_int) | is.na(ex_$call_int_c3)),]
						m = lmer(call_int_c3 ~ sex + scale(call_c_int)*sex + scale(day_j)*sex + (call_c_int|bird_ID) + (1|nest_ID), f)
									#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									  par(mfrow=c(5,3))

									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')
									  qqline(resid(m))

									  qqnorm(unlist(ranef(m)$bird_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$bird_ID [1]))

									  qqnorm(unlist(ranef(m)$bird_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$bird_ID[2]))

									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))


									  scatter.smooth(resid(m)~f$day_j);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$call_int_c2);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~f$sex);abline(h=0, lty=2, col='red')

									   mtext("lmer(call_int_c3 ~ sex + scale(call_c_int) + scale(day_j) + (call_c_int|bird_ID) + (1|nest_ID), f)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

									    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=f$lon, y=f$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)

											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))

							dev.off()

 		# Figure 4a
			# prepare data for plotting
				f = dd[-which(is.na(dd$call_c_int) | is.na(dd$call_o_int) | dd$left_before_presence=="y"),]
					nrow(f)
					length(unique(f$bird_ID))
					length(unique(f$nest_ID))
					#f$sex_returning = ifelse(f$sex == 'f','m','f')
				# model predictions
					m = lmer(call_c_int ~ sex + call_o_int*sex  +day_j*sex +(call_o_int|bird_ID) + (1|nest_ID), f)

						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)

				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))

				# values to predict for
					newD=data.frame(sex = c('f','m'),
									call_o_int = seq(min(f$call_o_int),max(f$call_o_int), length.out = 300),
									day_j = mean(f$day_j)
									#current_bout = mean(f$current_bout)
									)

				# exactly the model which was used has to be specified here
				X <- model.matrix(~ sex + call_o_int*sex  +day_j*sex,data=newD)

				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pp=newD
					pf_=pp[pp$sex=='f',]
					pm_=pp[pp$sex=='m',]
			# raw data
						f$n = 1
						#x = ddply(f,.(nest_ID, sex_returning),summarise, mo=median(call_o_int), q1o=quantile(call_o_int,0.25), q2o= quantile(call_o_int,0.75), mc=median(call_c_int), q1c=quantile(call_c_int,0.25), q2c= quantile(call_c_int,0.75), n = sum(n))

						f$col_=ifelse(f$sex=='f', '#FCB42C', '#535F7C') # female color = #FCB42C)

						x = ddply(f,.(sex,call_o_int,call_c_int),summarise, n = sum(n))
						x$call_o_int = ifelse(x$sex == 'f', x$call_o_int-0.2, x$call_o_int+0.2)
						# call based on x-axis - incubating parent
						x$col_=ifelse(x$sex=='f', '#FCB42C', '#535F7C') # female color = #FCB42C)
			# plot
				if(PNG == TRUE) {
					#png(paste(outdir,"Figure_5a_colASxaxis.png", sep=""), width=1.85+0.3,height=1.5,units="in",res=600)
					png(paste(outdir,"Figure_4a.png", sep=""), width=1.85+0.3,height=1.5,units="in",res=600)
					}else{
					dev.new(width=1.85+0.3,height=1.5)
					}
				par(mar=c(0.8,0.1,0.2,2.5),oma = c(1, 2, 0, 0),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #

				plot(call_c_int ~ call_o_int, data = f,
										#ylab =NULL,
										xaxt='n',
										yaxt='n',
										#xaxs = 'i',
										#yaxs = 'i',
										ylim=c(-.2,3),
										xlim=c(-.3,3),
										type='n'
										) # col=z_g$cols, border=z_g$cols


				# predictions
					# male
							polygon(c(pm_$call_o_int, rev(pm_$call_o_int)), c(pm_$lwr,
								rev(pm_$upr)), border=NA, col=adjustcolor(col_m ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pm_$call_o_int, pm_$pred, col=col_m,lwd=1)

					# female
							polygon(c(pf_$call_o_int, rev(pf_$call_o_int)), c(pf_$lwr,
								rev(pf_$upr)), border=NA, col=adjustcolor(col_f ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pf_$call_o_int, pf_$pred, col=col_f,lwd=1)

					#text(x=-2,y=0.725, labels='Before', col='#FCB42C', cex=0.5)
							#text(x=2,y=0.725, labels='After', col='#535F7C', cex=0.5)

				symbols(x$call_o_int, x$call_c_int, circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) #

				axis(1, at=c(0,1,2,3), label=c(0,1,2,3), mgp=c(0,-0.20,0), lwd = 0.35)
				axis(2, at=c(0,1,2,3), label=c(0,1,2,3), lwd = 0.35)

				mtext("Calling of incubating parent\n[while both parents around]",side=1,line=0.85, cex=0.55, las=1, col='black')
				mtext("Calling of retunring parent\n[while both parents around]",side=2,line=0.8, cex=0.55, las=3, col='black')
				mtext(expression(bold('a')),side=3,line=-0.4, cex=0.6,  col='black')
				#text(-0.7,3, expression(bold('a')),cex=0.6, xpd=TRUE)

					#text(x=2.5,y=0.1, labels='\u2640', col='#FCB42C', cex=0.6, pos=4)
					#text(x=2.65,y=0.2, labels='\u2642', col='#535F7C', cex=0.6, pos=4)
					#text(x=0.3,y=100*0.88, labels="Prediction & 95%CrI", col='red', cex=0.5, pos=4)
					#mtext("Prediction\n95%CrI",side=3,line=-1.5, cex=0.5, las=1, col='red')
					#axis(1, at=c(1.5,4.2, 6.9), labels=FALSE)

					#text(c(1,2), par("usr")[3]+0.07, labels = c('\u2640','\u2642'), font=4, xpd = TRUE, cex=0.6, col=c('#FCB42C','#535F7C'))#col="grey30") #labels

				 if(PNG == TRUE) {dev.off()}
		# Figure 4b
			# prepare data for plotting
				f = dd[-which(is.na(dd$call_c_int) | is.na(dd$call_int_c2) | dd$left_before_presence=="y"),]
				nrow(f)
				length(unique(f$bird_ID))
				length(unique(f$nest_ID))
				# model predictions
					m = lmer(call_int_c2 ~ sex_returning + call_c_int*sex_returning + day_j*sex_returning + (call_c_int|bird_ID) + (1|nest_ID), f)

						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)

				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))

				# values to predict for
					newD=data.frame(sex_returning = c('f','m'),
									call_c_int = seq(min(f$call_c_int),max(f$call_c_int), length.out = 300),
									day_j = mean(eb$day_j)
									)

				# exactly the model which was used has to be specified here
				X <- model.matrix(~ sex_returning + call_c_int*sex_returning + day_j*sex_returning,data=newD)

				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pp=newD
					pf_=pp[pp$sex_returning=='f',]
					pm_=pp[pp$sex_returning=='m',]
			# raw data
						f$n = 1

						f$col_=ifelse(f$sex_returning=='f', '#FCB42C', '#535F7C') # female color = #FCB42C)

						x = ddply(f,.(sex_returning,call_int_c2,call_c_int),summarise, n = sum(n))
						x$call_c_int = ifelse(x$sex == 'f', x$call_c_int-0.2, x$call_c_int+0.2)
						x$col_=ifelse(x$sex_returning=='f', '#FCB42C', '#535F7C') # female color = #FCB42C)

						#ggplot(f,aes(x = as.factor(call_o_int), y = call_c_int, col = sex_returning))+geom_boxplot()
						#ggplot(x,aes(x = as.factor(mo), y = mc, col = sex_returning))+geom_boxplot()
						xf=x[x$sex_returning=='f',]
						xm=x[x$sex_returning=='m',]
			# plot
				if(PNG == TRUE) {
					png(paste(outdir,"Figure_4b.png", sep=""), width=1.85+0.3,height=1.5,units="in",res=600)
					}else{
					dev.new(width=1.85+0.3,height=1.5)
					}
				par(mar=c(0.8,0.1,0.2,2.5),oma = c(1, 2, 0, 0),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE)


				plot(call_int_c2 ~ call_c_int, data = f,
										#ylab =NULL,
										xaxt='n',
										yaxt='n',
										#xaxs = 'i',
										#yaxs = 'i',
										ylim=c(-.2,3),
										xlim=c(-.3,3),
										type='n'
										) # col=z_g$cols, border=z_g$cols
				# predictions
					# male
							polygon(c(pm_$call_c_int, rev(pm_$call_c_int)), c(pm_$lwr,
								rev(pm_$upr)), border=NA, col=adjustcolor(col_m ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pm_$call_c_int, pm_$pred, col=col_m,lwd=1)

					# female
							polygon(c(pf_$call_c_int, rev(pf_$call_c_int)), c(pf_$lwr,
								rev(pf_$upr)), border=NA, col=adjustcolor(col_f ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pf_$call_c_int, pf_$pred, col=col_f,lwd=1)

					#text(x=-2,y=0.725, labels='Before', col='#FCB42C', cex=0.5)
							#text(x=2,y=0.725, labels='After', col='#535F7C', cex=0.5)

				symbols(x$call_c_int, x$call_int_c2, circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) #

				axis(1, at=c(0,1,2,3), label=c(0,1,2,3), mgp=c(0,-0.20,0), lwd = 0.35, col = 'black')
				axis(2, at=c(0,1,2,3), label=c(0,1,2,3), lwd = 0.35, col = 'black')

				mtext("Calling of returning parent\n[while partner on nest]",side=1,line=0.85, cex=0.55, las=1, col='black')
				mtext("Calling of returning parent\n[during exchange gap]",side=2,line=0.8, cex=0.55, las=3, col='black')
				mtext(expression(bold('b')),side=3,line=-0.4, cex=0.6,  col='black')

					#text(x=-.3,y=2.9, labels='\u2640', col='#FCB42C', cex=0.6, pos=4)
					#text(x=-.1,y=2.95, labels='\u2642', col='#535F7C', cex=0.6, pos=4)

				 if(PNG == TRUE) {dev.off()}
		# Figure 4c
			dxn = dd[-which(is.na(dd$next_bout) | dd$left_before_presence == 'y' | !dd$with_calling %in% c('y') | is.na(dd$o_replies)),]
			dxn = dxn[dxn$left_type %in% c('3 during exchange'),]

				nrow(dxn)
				length(unique(dxn$bird_ID))
				length(unique(dxn$nest_ID))
			# prepare for plotting
				k=0.1
				kk=k*2# distance for boxplots
				kkk=k*2 # distance for points
				dxn$reply_sex=ifelse(dxn$o_replies=='y',ifelse(dxn$sex=='f', 1-k,k+1),
								ifelse(dxn$sex=='f', 4-k,k+4))
				x = dxn
				x$at=ifelse(dxn$o_replies=='y',ifelse(dxn$sex=='f', 1-kkk,2+kkk),
								ifelse(dxn$sex=='f', 3.7-kkk,4.7+kkk))
				dxn$col_=ifelse(dxn$sex=='f','#FCB42C', '#535F7C')
				x$col_=ifelse(x$sex=='f','#FCB42C', '#535F7C')
			# prepare model predictions
				m = lmer(next_bout ~ o_replies+sex+(1|bird_ID)+(1|nest_ID),dxn)
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)

				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))

				# values to predict for
					newD=data.frame(o_replies=c('y','n'),
									sex = 0.5
									)

				# exactly the model which was used has to be specified here
					X <- model.matrix(~ o_replies*sex,data=newD)

				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pp=newD
			# plot
				if(PNG == TRUE) {
					png(paste(outdir,"Figure_4c.png", sep=""), width=1.85+0.3,height=1.5,units="in",res=600)
					}else{
					dev.new(width=1.85+0.3,height=1.5)
					}
				#par(mar=c(0.8,0,0,2.5),oma = c(0.7, 2, 0, 0),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey40", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #
				par(mar=c(0.8,0.1,0.2,2.5),oma = c(1, 2, 0, 0),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE)

				boxplot(next_bout/60 ~ reply_sex, data = dxn,
										#ylab =NULL,
										xaxt='n',
										yaxt='n',
										ylim=c(0,20),
										par(bty='n'),
										#at=c(1,2,3.5,4.5),
										at=c(1,2,3.7,4.7), type='n',
										outcex=0.5, outpch=20,boxwex=0.25,whisklty=1,staplelty=0,#medlwd=1,
										lwd = 0.25,
										#ylim=c(0,1),
										outcol="white",boxcol='white',whiskcol='white',staplecol='white',medcol='white'
										) # col=z_g$cols, border=z_g$cols


				for (i in 1:nrow(x)){stripchart(x$next_bout[i]/60~ factor(x$reply_sex[i]), at = x$at[i],
											#bg = x$col_[i],
											col="gray63",
											#col = x$col_[i],
											bg = adjustcolor(x$col_[i], alpha.f = 0.4),
											pch =21, cex=0.5,
											vertical = TRUE, add = TRUE, method = "jitter")
											}

				boxplot(next_bout/60 ~ reply_sex, data = dxn,
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

					axis(2, at=seq(0,20,by=5), label=TRUE, lwd = 0.35, col = 'black')
					axis(1, at=c(1.5,4.2), label=c('yes','no'), mgp=c(0,-0.20,0),lwd = 0.35, col = 'white')

					text(c(1,2,3.7,4.7), par("usr")[3]-1.5, labels = c('\u2640','\u2642'), font=4, xpd = TRUE, cex=0.6, col=c('#FCB42C','#535F7C'))

					#text(c(1.5,4.2), par("usr")[3]-0.25, labels = c('Yes','No'),  xpd = TRUE, cex=0.5, col="black")


					mtext("Incubating parent replied",side=1,line=0.45, cex=0.55, las=1, col='black')
					mtext("Next incubation bout [h]",side=2,line=1.1, cex=0.55, las=3, col='black')
					mtext(expression(bold('c')),side=3,line=-0.4, cex=0.6,  col='black')
					text(x=0.5,y=5, labels="Prediction\n& 95%CrI", col='red', cex=0.5, pos=4)

					#text(x=0.3,y=20*0.97, labels='\u2640', col='#FCB42C', cex=0.6, pos=4)
					#text(x=0.3+0.2,y=20, labels='\u2642', col='#535F7C', cex=0.6, pos=4)

					#mtext("Prediction\n95%CrI",side=3,line=-1.5, cex=0.5, las=1, col='red')
					#axis(1, at=c(1.5,4.2, 6.9), labels=FALSE)

					#text(c(1,2), par("usr")[3]+0.07, labels = c('\u2640','\u2642'), font=4, xpd = TRUE, cex=0.6, col=c('#FCB42C','#535F7C'))#col="grey30") #labels


					# predictions
						points(y=pp$pred/60,x=c(1.5,4.2), pch=20, cex=0.9,col="red")
					# 95%CI
						arrows(x0=c(1.5,4.2), y0=pp$lwr/60,x1=c(1.5,4.2), y1=pp$upr/60,
						code = 0, col="red", angle = 90, length = .025, lwd=1, lty=1)

				 if(PNG == TRUE) {dev.off()}
		# Figure 4d
			f = dd[-which(is.na(dd$next_bout) | dd$left_before_presence == 'y' | is.na(dd$call_c_int) | is.na(dd$call_o_int) | is.na(dd$call_int_c2) | is.na(dd$call_int_c3)),]
			f$next_bout = f$next_bout/60
					nrow(f)
					length(unique(f$bird_ID))
					length(unique(f$nest_ID))
			# model predictions
				#summary(f$call_int_c3[f$sex == 'f']) # male calling
				#summary(f$call_int_c3[f$sex == 'm']) # female calling
				m = lmer(next_bout ~sex_returning*call_c_int+sex_returning*call_o_int+sex_returning*call_int_c2 + sex_returning*call_int_c3 + (call_int_c3|bird_ID) + (1|nest_ID),f)
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)

				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))

				# values to predict for
					newD1=data.frame(sex_returning = c('f'),
									call_c_int = mean(f$call_c_int),
									call_o_int = mean(f$call_o_int),
									call_int_c2 = mean(f$call_int_c3) ,
									call_int_c3 = seq(0,max(f$call_int_c3[f$sex_returning == 'f']), length.out = 300)
									#min(f$current_bout_c),max(f$current_bout_c)
									)

					newD2=data.frame(sex_returning = c('m'),
									call_c_int = mean(f$call_c_int),
									call_o_int = mean(f$call_o_int),
									call_int_c2 = mean(f$call_int_c3) ,
									call_int_c3 = seq(0,max(f$call_int_c3[f$sex_returning == 'm']), length.out = 300)
									#min(f$current_bout_c),max(f$current_bout_c)
									)

					newD = rbind(newD1, newD2)
				# exactly the model which was used has to be specified here
				X <- model.matrix(~ sex_returning*call_c_int+sex_returning*call_o_int+sex_returning*call_int_c2 + sex_returning*call_int_c3,data=newD)

				# calculate predicted values and creditability intervals
					newD$pred <-(X%*%v)
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pp=newD
					pf_=pp[pp$sex_returning=='f',]
					pm_=pp[pp$sex_returning=='m',]
			# raw data
						f$n = 1
						f$col_=ifelse(f$sex_returning=='f', '#FCB42C', '#535F7C') # female color = #FCB42C)

						x = ddply(f,.(nest_ID, sex_returning),summarise, mc=median(next_bout), q1c=quantile(next_bout,0.25), q2c= quantile(next_bout,0.75), mo=median(call_int_c3), q1o=quantile(call_int_c3,0.25), q2o= quantile(call_int_c3,0.75), n = sum(n))

						#x$call_o_int = ifelse(x$sex == 'f', x$call_o_int-0.2, x$call_o_int+0.2)
						x$col_=ifelse(x$sex_returning=='f', '#FCB42C', '#535F7C') # female color = #FCB42C)
			# plot
				if(PNG == TRUE) {
					png(paste(outdir,"Figure_4d_after.png", sep=""), width=1.85+0.3,height=1.5,units="in",res=600)
					}else{
					dev.new(width=1.85+0.3,height=1.5)
					}
				par(mar=c(0.8,0.1,0.2,2.5),oma = c(1, 2, 0, 0),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #

				plot(next_bout ~ call_int_c3 , data = f,
										#ylab =NULL,
										xaxt='n',
										yaxt='n',
										#xaxs = 'i',
										#yaxs = 'i',
										ylim=c(0,20),
										xlim=c(-.3,3),
										type='n'
										) # col=z_g$cols, border=z_g$cols

				# predictions
					# call int of returned female
							polygon(c(pf_$call_int_c3, rev(pf_$call_int_c3)), c(pf_$lwr,
								rev(pf_$upr)), border=NA, col=adjustcolor(col_f ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pf_$call_int_c3, pf_$pred, col=col_f,lwd=1)

					# call int of returned male
							polygon(c(pm_$call_int_c3, rev(pm_$call_int_c3)), c(pm_$lwr,
								rev(pm_$upr)), border=NA, col=adjustcolor(col_m ,alpha.f = 0.2)) #0,0,0 black 0.5 is transparents RED
							lines(pm_$call_int_c3, pm_$pred, col=col_m,lwd=1)

						#text(x=-2,y=0.725, labels='Before', col='#FCB42C', cex=0.5)
							#text(x=2,y=0.725, labels='After', col='#535F7C', cex=0.5)

				symbols(jitter(x$mo, amount=0.2), jitter(x$mc), circles=sqrt(x$n/pi),inches=0.14/1.75,bg=adjustcolor(x$col_, alpha.f = 0.5), fg=col_p,add=TRUE) #

				axis(1, at=c(0,1,2,3), label=c(0,1,2,3), mgp=c(0,-0.20,0), lwd = 0.35)
				axis(2, at=seq(0,20, by=5), label = TRUE,lwd = 0.35)
				mtext("Calling of returned parent\n[after sitting down]",side=1,line=0.9, cex=0.55, las=1, col='black') #line=0.85
				mtext("Next incubation bout [h]",side=2,line=1, cex=0.55, las=3, col='black') #line=0.8
				mtext(expression(bold('d')),side=3,line=-0.4, cex=0.6,  col='black')

				#points( jitter(f$call_int_c2), f$next_bout,pch = 21, bg=adjustcolor(f$col_, alpha.f = 0.5), col=col_p, xpd=TRUE) #
						#text(x=-2,y=0.725, labels='Before', col='#FCB42C', cex=0.5)
							#text(x=2,y=0.725, labels='After', col='#535F7C', cex=0.5)

				#	text(x=2.3,y=6, labels='\u2640', col='#FCB42C', cex=0.6, pos=4)
				#	text(x=2.5,y=6.2, labels='\u2642', col='#535F7C', cex=0.6, pos=4)

				 if(PNG == TRUE) {dev.off()}

		# Table S5
			# prepare table data
				# a. calling while arriving
					f = dd[-which(is.na(dd$current_bout) | dd$left_before_presence == 'y' | is.na(dd$with_calling)),]
					f$w_call = ifelse(f$with_calling == 'y', 1, 0)

					nrow(f)
					length(unique(f$bird_ID))
					length(unique(f$nest_ID))
										
					m = glmer(w_call ~ sex_returning*scale(current_bout) + (1|bird_ID)+ (1|nest_ID) , f, family = 'binomial')
							# binomial gives same results
								#dd$pa_bin=ifelse(dd$pa == 0.001, 0,1)
								#m = glmer(pa_bin~ sex*day_j+(day_j|nest_ID),family='binomial', dd)
					pred=c('Intercept (f)','sex_returning(m)', 'Current bout', 'Current bout:sex') # sex of the retirmomg bird is the oposite of this
					dep = 'calling while arriving (bin)'
					mod = 1
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)
				 	# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]
					# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
						ri$estimate_r = paste(ri$estimate_r,"%",sep='')
					o1=rbind(oii,ri)
				# b. reply
				 	f = dd[-which(is.na(dd$current_bout) | dd$left_before_presence == 'y' | !dd$with_calling %in% c('y')  | is.na(dd$o_replies)),]
				 	f = f[f$left_type %in% c('3 during exchange'),]
				 	f$reply = ifelse(f$o_replies == 'n', 0,1)
					nrow(f)
					length(unique(f$bird_ID))
					length(unique(f$nest_ID))
					
				 	m = glmer(reply ~ sex*scale(current_bout) + (1|bird_ID) + (1|nest_ID), f, family = 'binomial')
					pred=c('Intercept (f)','Sex(m)', 'current_bout', 'current_bout:sex')
					dep = 'reply (bin)'
					mod = 2
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)
				 	# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]
					# Random effects var*100 - zero as in Table S4 and hence not reported
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
						ri$estimate_r = paste(ri$estimate_r,"%",sep='')
					o2=oii#rbind(oii,ri)
				# c. calling coming
					f = dd[-which(is.na(dd$current_bout) | dd$left_before_presence == 'y' |  is.na(dd$call_c_int)),]
					nrow(f)
					length(unique(f$bird_ID))
					length(unique(f$nest_ID))
					
					m = lmer(call_c_int ~ sex*scale(current_bout) +(scale(current_bout)|bird_ID) + (1|nest_ID), f)
					#m = lmer(next_bout ~ sex*scale(call_c_int) + (scale(call_c_int)|bird_ID) + (1|nest_ID), f) # sex of the returning bird is the oposite of this

					pred=c('Intercept (f)','Sex(m)', 'current_bout', 'Current_bout:sex')
					dep = 'call_c_int'
					mod = 3
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)
				 	# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]
					# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
						ri$estimate_r = paste(ri$estimate_r,"%",sep='')
					o3=rbind(oii,ri)	
				# d. calling incubating
					f = ex_[-which(is.na(ex_$call_o_int) |is.na(ex_$current_bout)),]
					f = dd[-which(is.na(dd$current_bout) | dd$left_before_presence == 'y' |  is.na(dd$call_o_int)),]
					nrow(f)
					length(unique(f$bird_ID))
					length(unique(f$nest_ID))
				
					m = lmer(call_o_int ~ sex*scale(current_bout) + (scale(current_bout)|bird_ID) + (1|nest_ID), f)
					#m = lmer(next_bout ~ sex*scale(call_o_int) + (scale(call_o_int)|bird_ID) + (1|nest_ID), f)

					pred=c('Intercept (f)','Sex(m)', 'current_bout', 'Current_bout:sex')
					dep = 'call_c_int'
					mod = 4
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)
				 	# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]
					# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
						ri$estimate_r = paste(ri$estimate_r,"%",sep='')
					o4=rbind(oii,ri)
				# e. exchange gap calling
					f = dd[-which(is.na(dd$current_bout) | dd$left_before_presence == 'y' |  is.na(dd$call_int_c2)),]
					nrow(f)
					length(unique(f$bird_ID))
					length(unique(f$nest_ID))
				
					m = lmer(call_int_c2 ~ sex_returning*scale(current_bout) + (scale(current_bout)|bird_ID) + (1|nest_ID), f)
					#m = lmer(next_bout ~ sex*scale(call_int_c2) + (scale(call_int_c2)|bird_ID) + (1|nest_ID), f)
					pred=c('Intercept (f)','sex_returning(m)', 'current_bout', 'current_bout:sex') # sex of the returning bird is the oposite of this
					dep = 'call during gap'
					mod = 5
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)
				 	# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]
					# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
						ri$estimate_r = paste(ri$estimate_r,"%",sep='')
					o5=rbind(oii,ri)
				# f. after exchange
					f = dd[-which(is.na(dd$current_bout) | dd$left_before_presence == 'y' |  is.na(dd$call_int_c3)),]
					nrow(f)
					length(unique(f$bird_ID))
					length(unique(f$nest_ID))
					
					m = lmer(call_int_c3 ~ sex_returning*scale(current_bout) + (scale(current_bout)|bird_ID) + (1|nest_ID), f)
					#m = lmer(next_bout ~ sex*scale(call_int_c3) + (scale(call_int_c3)|bird_ID) + (1|nest_ID), f)
					pred=c('Intercept (f)','sex_returning(m)', 'current_bout', 'current_bout:sex') # sex of the reutrning bird is the oposite of this
					dep = 'call after exchange'
					mod = 6
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)
				 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]
					# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
						ri$estimate_r = paste(ri$estimate_r,"%",sep='')
					o6=rbind(oii,ri)
			# create xlsx table
				o=rbind(o1,o2,o3,o4,o5,o6)
				sname = 'Table_S5'
				tmp = write_xlsx(o, paste0(ta,sname,'.xlsx'))
				openFile(tmp)
		# model assumptions
				# a. calling while arriving
						f = dx[-which(is.na(dx$current_bout)),]
						m = glmer(w_call ~ sex*scale(current_bout) + (1|bird_ID) , f, family = 'binomial')

						scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
						scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')

						plot(fitted(m), jitter(f$w_call, amount=0.05), xlab="Fitted values", ylab="Probability of calling", las=1, cex.lab=1.2, cex=0.8)
							abline(0,1, lty=3)
							t.breaks <- cut(fitted(m), seq(0,1, by=0.1))
							means <- tapply(f$w_call, t.breaks, mean)
							semean <- function(x) sd(x)/sqrt(length(x))
							means.se <- tapply(f$w_call, t.breaks, semean)
							points(seq(0.05, 0.95, by=0.1), means, pch=16, col="orange")
							segments(seq(0.05, 0.95, by=0.1), means-2*means.se, seq(0.05, 0.95,by=0.1), means+2*means.se,lwd=2, col="orange")

						plot(fitted(m), jitter(f$w_call, amount=0.05), xlab="Fitted values", ylab="Probability of presence", las=1, cex.lab=1.2, cex=0.8) # what to do with the miss-fit
							abline(0,1, lty=3)
							t.breaks <- cut(fitted(m), seq(0,1, by=0.2))
							means <- tapply(f$w_call, t.breaks, mean)
							semean <- function(x) sd(x)/sqrt(length(x))
							means.se <- tapply(f$w_call, t.breaks, semean)
							points(seq(0.05, 0.95, by=0.2), means, pch=16, col="orange")
							segments(seq(0.05, 0.95, by=0.2), means-2*means.se, seq(0.05, 0.95,by=0.2), means+2*means.se,lwd=2, col="orange")
						qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')  # what to do with the miss-fit
						qqline(resid(m))
						qqnorm(unlist(ranef(m)$bird_ID[1]), main = "bird",col='red')
						qqline(unlist(ranef(m)$bird_ID[1]))
						scatter.smooth(resid(m)~f$current_bout);abline(h=0, lty=2, col='red')
						boxplot(resid(m)~f$sex, xlab = 'sex');abline(h=0, lty=2, col='red')
						scatter.smooth(resid(m)~f$sex);abline(h=0, lty=2, col='red')

						acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))

						# spatial autocorrelations - nest location
							spdata=data.frame(resid=resid(m), x=f$lon, y=f$lat)
								spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
								#cex_=c(1,2,3,3.5,4)
								cex_=c(1,1.5,2,2.5,3)
								spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
								plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)

								plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8),xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
								plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8), xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
								#mtext("glmer(success_bin~prop_ip+uni_last+(1|sp),data=g,family='binomial')", side = 3, line = -1.2, cex=0.8,outer = TRUE)
				# b. reply
						f = dx_[-which(is.na(dx_$current_bout)| is.na(dx_$reply)),]
						m = glmer(reply ~ sex*scale(current_bout) + (1|bird_ID) + (1|nest_ID), f, family = 'binomial')

						scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
						scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')

						plot(fitted(m), jitter(f$reply, amount=0.05), xlab="Fitted values", ylab="Probability of calling", las=1, cex.lab=1.2, cex=0.8)
							abline(0,1, lty=3)
							t.breaks <- cut(fitted(m), seq(0,1, by=0.1))
							means <- tapply(f$reply, t.breaks, mean)
							semean <- function(x) sd(x)/sqrt(length(x))
							means.se <- tapply(f$reply, t.breaks, semean)
							points(seq(0.05, 0.95, by=0.1), means, pch=16, col="orange")
							segments(seq(0.05, 0.95, by=0.1), means-2*means.se, seq(0.05, 0.95,by=0.1), means+2*means.se,lwd=2, col="orange")

						plot(fitted(m), jitter(f$reply, amount=0.05), xlab="Fitted values", ylab="Probability of presence", las=1, cex.lab=1.2, cex=0.8) # what to do with the miss-fit
							abline(0,1, lty=3)
							t.breaks <- cut(fitted(m), seq(0,1, by=0.2))
							means <- tapply(f$reply, t.breaks, mean)
							semean <- function(x) sd(x)/sqrt(length(x))
							means.se <- tapply(f$reply, t.breaks, semean)
							points(seq(0.05, 0.95, by=0.2), means, pch=16, col="orange")
							segments(seq(0.05, 0.95, by=0.2), means-2*means.se, seq(0.05, 0.95,by=0.2), means+2*means.se,lwd=2, col="orange")

						qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')  # what to do with the miss-fit
						qqline(resid(m))
						qqnorm(unlist(ranef(m)$bird_ID[1]), main = "bird",col='red')
						qqline(unlist(ranef(m)$bird_ID[1]))

						qqnorm(unlist(ranef(m)$nest_ID[1]), main = "nest",col='red')
						qqline(unlist(ranef(m)$nest_ID[1]))

						scatter.smooth(resid(m)~f$current_bout);abline(h=0, lty=2, col='red')
						boxplot(resid(m)~f$sex, xlab = 'sex');abline(h=0, lty=2, col='red')
						scatter.smooth(resid(m)~f$sex);abline(h=0, lty=2, col='red')

						acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))

						# spatial autocorrelations - nest location
							spdata=data.frame(resid=resid(m), x=f$lon, y=f$lat)
								spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
								#cex_=c(1,2,3,3.5,4)
								cex_=c(1,1.5,2,2.5,3)
								spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
								plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)

								plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8),xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
								plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8), xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
								#mtext("glmer(success_bin~prop_ip+uni_last+(1|sp),data=g,family='binomial')", side = 3, line = -1.2, cex=0.8,outer = TRUE)
				# c. calling coming ~ calling incubating
						f = ex_[-which(is.na(ex_$call_c_int) | is.na(ex_$current_bout)),]
						m = lmer(call_c_int ~ sex*scale(current_bout) +(scale(current_bout)|bird_ID) + (1|nest_ID), f)
									#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									  par(mfrow=c(5,3))

									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')
									  qqline(resid(m))

									  qqnorm(unlist(ranef(m)$bird_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$bird_ID [1]))

									  qqnorm(unlist(ranef(m)$bird_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$bird_ID[2]))

									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))


									  scatter.smooth(resid(m)~f$current_bout);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~f$sex);abline(h=0, lty=2, col='red')

									   mtext("lmer(call_c_int ~ sex*scale(current_bout)+(current_bout|bird_ID) + (1|nest_ID), f)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

									    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=f$lon, y=f$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)

											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))

							dev.off()
				# d. calling incubating
						f = ex_[-which(is.na(ex_$call_o_int) |is.na(ex_$current_bout)),]
						m = lmer(call_o_int ~ sex*scale(current_bout) + (scale(current_bout)|bird_ID) + (1|nest_ID), f)
									#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									  par(mfrow=c(5,3))

									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')
									  qqline(resid(m))

									  qqnorm(unlist(ranef(m)$bird_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$bird_ID [1]))

									  qqnorm(unlist(ranef(m)$bird_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$bird_ID[2]))

									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))


									  scatter.smooth(resid(m)~f$current_bout);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~f$sex);abline(h=0, lty=2, col='red')

									   mtext("call_o_int ~ sex*scale(current_bout) + (scale(current_bout)|bird_ID) + (1|nest_ID), f)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

									    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=f$lon, y=f$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)

											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))

							dev.off()
				# e. exchange gap calling
						f = ex_[-which(is.na(ex_$call_int_c2) | is.na(ex_$current_bout)),]
						m = lmer(call_int_c2 ~ sex*scale(current_bout) + (scale(current_bout)|bird_ID) + (1|nest_ID), f)
									#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									  par(mfrow=c(5,3))

									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')
									  qqline(resid(m))

									  qqnorm(unlist(ranef(m)$bird_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$bird_ID [1]))

									  qqnorm(unlist(ranef(m)$bird_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$bird_ID[2]))

									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))


									  scatter.smooth(resid(m)~f$current_bout);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~f$sex);abline(h=0, lty=2, col='red')

									   mtext("lmer(call_int_c2 ~ sex*scale(current_bout) + (scale(current_bout)|bird_ID) + (1|nest_ID), f)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

									    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=f$lon, y=f$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)

											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))

							dev.off()
				# f. after exchange
						f = ex_[-which(is.na(ex_$call_int_c3) | is.na(ex_$current_bout)),]
						m = lmer(call_int_c3 ~ sex*scale(current_bout) + (scale(current_bout)|bird_ID) + (1|nest_ID), f)

									#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									  par(mfrow=c(5,3))

									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')
									  qqline(resid(m))

									  qqnorm(unlist(ranef(m)$bird_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$bird_ID [1]))

									  qqnorm(unlist(ranef(m)$bird_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$bird_ID[2]))

									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))


									  scatter.smooth(resid(m)~f$current_bout);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~f$sex);abline(h=0, lty=2, col='red')

									   mtext("lmer(call_int_c3 ~ sex*scale(current_bout) + (scale(current_bout)|bird_ID) + (1|nest_ID), f)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

									    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=f$lon, y=f$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)

											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))

							dev.off()
		
		# Table XX - complex models including S4 and S5, not used in the manuscript, but gives similar results
			# prepare table data
				# a. calling while arriving
					f = dx[-which(is.na(dx$current_bout)),]
					f$sex_returning = ifelse(f$sex == 'f','m','f')
					m = glmer(w_call ~ sex_returning*scale(day_j) + sex_returning*scale(current_bout)+ (1|bird_ID) +(1|nest_ID), f, family = 'binomial')
							# binomial gives same results
								#dd$pa_bin=ifelse(dd$pa == 0.001, 0,1)
								#m = glmer(pa_bin~ sex*day_j+(day_j|nest_ID),family='binomial', dd)
					pred=c('Intercept (f)','Sex returning(m)','Day', 'Current bout', 'Day:sex', 'Current bout:sex')
					dep = 'calling while arriving (bin)'
					mod = 'a'
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)
				 	# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]
					# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
						ri$estimate_r = paste(ri$estimate_r,"%",sep='')
					o1=rbind(oii,ri)
				# b. reply
				 	f = dx_[-which(is.na(dx_$current_bout)| is.na(dx_$reply)),]
				 	m = glm(reply ~ sex*scale(day_j) + sex*scale(current_bout), f, family = 'binomial')
					pred=c('Intercept (f)','Sex inc(m)', 'Day', 'current_bout','Day:sex', 'current_bout:sex')
					dep = 'reply (bin)'
					mod = 'b'
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)
				 	# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]
					# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
						ri$estimate_r = paste(ri$estimate_r,"%",sep='')
					o2=rbind(oii,ri)
				# c. calling returning when both present (given calling of incubating parent)
					f = ex_[-which(is.na(ex_$call_c_int) | is.na(ex_$current_bout)),]
					#f$sex_returning = ifelse(f$sex == 'f','m','f')
					m = lmer(call_c_int ~ sex + scale(call_o_int)*sex + scale(day_j)*sex +scale(current_bout)*sex +(call_o_int|bird_ID) + (1|nest_ID), f)
					#m = lmer(next_bout ~ sex*scale(call_c_int) + (scale(call_c_int)|bird_ID) + (1|nest_ID), f)
					pred=c('Intercept (f)','Sex inc(m)', 'Call_incub', 'Day', 'current_bout','Call_incub:sex','Day:sex',  'Current_bout:sex')
					dep = 'call_c_int'
					mod = 'c'
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)
				 	# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]
					# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
						ri$estimate_r = paste(ri$estimate_r,"%",sep='')
					o3=rbind(oii,ri)
				# d. exchange gap calling
					f = ex_[-which(is.na(ex_$call_c_int) | is.na(ex_$call_int_c2)),]
					f$sex_returning = ifelse(f$sex == 'f','m','f')
					m = lmer(call_int_c2 ~ sex_returning + scale(call_c_int)*sex_returning + scale(day_j)*sex_returning + (call_c_int|bird_ID) + (1|nest_ID), f)
					pred=c('Intercept (f)','Sex returning(m)', 'Call_com', 'Day', 'Call_com:sex','Day:sex')
					dep = 'call during gap'
					mod = 'e'
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)
					 # Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]
					# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
						ri$estimate_r = paste(ri$estimate_r,"%",sep='')
					o5=rbind(oii,ri)
				# e. after exchange
					f = ex_[-which(is.na(ex_$call_c_int) | is.na(ex_$call_int_c3)),]
					f$sex_returning = ifelse(f$sex == 'f','m','f')
					m = lmer(call_int_c3 ~ sex_returning + scale(call_c_int)*sex_returning + scale(day_j)*sex_returning + (call_c_int|bird_ID) + (1|nest_ID), f)
					pred=c('Intercept (f)','Sex returning(m)', 'Call_com', 'Day', 'Call_com:sex','Day:sex')
					dep = 'call after exchange'
					mod = 'f'
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)
				 	# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,3)
						oi$lwr_r=round(oi$lwr,3)
						oi$upr_r=round(oi$upr,3)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]
					# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
						ri$estimate_r = paste(ri$estimate_r,"%",sep='')
					o6=rbind(oii,ri)
			# create xlsx table
				o=rbind(o1,o2,o3,o4,o5,o6)
				sname = 'Table_XX'
				tmp = write_xlsx(o, paste0(ta,sname,'.xlsx'))
				openFile(tmp)
			# model assumptions
				# a. calling while arriving
						m = glmer(w_call ~ sex*scale(day_j) + (1|bird_ID) + (1|nest_ID), dx, family = 'binomial')

						scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
						scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')

						plot(fitted(m), jitter(dx$w_call, amount=0.05), xlab="Fitted values", ylab="Probability of calling", las=1, cex.lab=1.2, cex=0.8)
							abline(0,1, lty=3)
							t.breaks <- cut(fitted(m), seq(0,1, by=0.1))
							means <- tapply(dx$w_call, t.breaks, mean)
							semean <- function(x) sd(x)/sqrt(length(x))
							means.se <- tapply(dx$w_call, t.breaks, semean)
							points(seq(0.05, 0.95, by=0.1), means, pch=16, col="orange")
							segments(seq(0.05, 0.95, by=0.1), means-2*means.se, seq(0.05, 0.95,by=0.1), means+2*means.se,lwd=2, col="orange")

						plot(fitted(m), jitter(dx$w_call, amount=0.05), xlab="Fitted values", ylab="Probability of presence", las=1, cex.lab=1.2, cex=0.8) # what to do with the miss-fit
							abline(0,1, lty=3)
							t.breaks <- cut(fitted(m), seq(0,1, by=0.2))
							means <- tapply(dx$w_call, t.breaks, mean)
							semean <- function(x) sd(x)/sqrt(length(x))
							means.se <- tapply(dx$w_call, t.breaks, semean)
							points(seq(0.05, 0.95, by=0.2), means, pch=16, col="orange")
							segments(seq(0.05, 0.95, by=0.2), means-2*means.se, seq(0.05, 0.95,by=0.2), means+2*means.se,lwd=2, col="orange")
						qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')  # what to do with the miss-fit
						qqline(resid(m))
						qqnorm(unlist(ranef(m)$nest[1]), main = "nest",col='red')
						qqline(unlist(ranef(m)$nest[1]))

						qqnorm(unlist(ranef(m)$bird_ID[1]), main = "bird",col='red')
						qqline(unlist(ranef(m)$bird_ID[1]))

						scatter.smooth(resid(m)~dx$day_j);abline(h=0, lty=2, col='red')
						boxplot(resid(m)~dx$sex, xlab = 'sex');abline(h=0, lty=2, col='red')
						scatter.smooth(resid(m)~dx$sex);abline(h=0, lty=2, col='red')

						acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))

						# spatial autocorrelations - nest location
							spdata=data.frame(resid=resid(m), x=dx$lon, y=dx$lat)
								spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
								#cex_=c(1,2,3,3.5,4)
								cex_=c(1,1.5,2,2.5,3)
								spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
								plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)

								plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8),xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
								plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8), xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
								#mtext("glmer(success_bin~prop_ip+uni_last+(1|sp),data=g,family='binomial')", side = 3, line = -1.2, cex=0.8,outer = TRUE)
				# b. reply
						dx__=dx_[!is.na(dx_$reply),]
						m = glmer(reply ~ sex*scale(day_j) + (1|bird_ID) + (1|nest_ID), dx__, family = 'binomial')

						scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
						scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')

						plot(fitted(m), jitter(dx__$reply, amount=0.05), xlab="Fitted values", ylab="Probability of calling", las=1, cex.lab=1.2, cex=0.8)
							abline(0,1, lty=3)
							t.breaks <- cut(fitted(m), seq(0,1, by=0.1))
							means <- tapply(dx__$reply, t.breaks, mean)
							semean <- function(x) sd(x)/sqrt(length(x))
							means.se <- tapply(dx__$reply, t.breaks, semean)
							points(seq(0.05, 0.95, by=0.1), means, pch=16, col="orange")
							segments(seq(0.05, 0.95, by=0.1), means-2*means.se, seq(0.05, 0.95,by=0.1), means+2*means.se,lwd=2, col="orange")

						plot(fitted(m), jitter(dx__$reply, amount=0.05), xlab="Fitted values", ylab="Probability of presence", las=1, cex.lab=1.2, cex=0.8) # what to do with the miss-fit
							abline(0,1, lty=3)
							t.breaks <- cut(fitted(m), seq(0,1, by=0.2))
							means <- tapply(dx__$reply, t.breaks, mean)
							semean <- function(x) sd(x)/sqrt(length(x))
							means.se <- tapply(dx__$reply, t.breaks, semean)
							points(seq(0.05, 0.95, by=0.2), means, pch=16, col="orange")
							segments(seq(0.05, 0.95, by=0.2), means-2*means.se, seq(0.05, 0.95,by=0.2), means+2*means.se,lwd=2, col="orange")

						qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')  # what to do with the miss-fit
						qqline(resid(m))

						qqnorm(unlist(ranef(m)$nest[1]), main = "nest",col='red')
						qqline(unlist(ranef(m)$nest[1]))

						qqnorm(unlist(ranef(m)$bird_ID[1]), main = "bird",col='red')
						qqline(unlist(ranef(m)$bird_ID[1]))

						scatter.smooth(resid(m)~dx__$day_j);abline(h=0, lty=2, col='red')
						boxplot(resid(m)~dx__$sex, xlab = 'sex');abline(h=0, lty=2, col='red')
						scatter.smooth(resid(m)~dx__$sex);abline(h=0, lty=2, col='red')

						acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))

						# spatial autocorrelations - nest location
							spdata=data.frame(resid=resid(m), x=dx__$lon, y=dx__$lat)
								spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
								#cex_=c(1,2,3,3.5,4)
								cex_=c(1,1.5,2,2.5,3)
								spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
								plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)

								plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8),xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
								plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8), xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
								#mtext("glmer(success_bin~prop_ip+uni_last+(1|sp),data=g,family='binomial')", side = 3, line = -1.2, cex=0.8,outer = TRUE)
				# c. calling coming ~ calling incubating
						f = ex_[-which(is.na(ex_$call_c_int) | is.na(ex_$call_o_int)),]
						m = lmer(call_c_int ~ sex + scale(call_o_int)*sex + scale(day_j)*sex + (call_o_int|bird_ID) + (1|nest_ID), f)
									#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									  par(mfrow=c(5,3))

									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')
									  qqline(resid(m))

									  qqnorm(unlist(ranef(m)$bird_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$bird_ID [1]))

									  qqnorm(unlist(ranef(m)$bird_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$bird_ID[2]))

									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))


									  scatter.smooth(resid(m)~f$day_j);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$call_o_int);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~f$sex);abline(h=0, lty=2, col='red')

									   mtext("lmer(call_int_c3 ~ sex + scale(call_o_int) + scale(day_j) + (call_o_int|bird_ID) + (1|nest_ID), f)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

									    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=f$lon, y=f$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)

											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))

							dev.off()
				# d. exchange gap calling
						f = ex_[-which(is.na(ex_$call_c_int) | is.na(ex_$call_int_c2)),]
						m = lmer(call_int_c2 ~ sex + scale(call_c_int)*sex + scale(day_j)*sex + (call_c_int|bird_ID) + (1|nest_ID), f)
									#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									  par(mfrow=c(5,3))

									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')
									  qqline(resid(m))

									  qqnorm(unlist(ranef(m)$bird_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$bird_ID [1]))

									  qqnorm(unlist(ranef(m)$bird_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$bird_ID[2]))

									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))


									  scatter.smooth(resid(m)~f$day_j);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$call_c_int);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~f$sex);abline(h=0, lty=2, col='red')

									   mtext("lmer(call_int_c2 ~ sex + scale(call_c_int) + scale(day_j) + (call_c_int|bird_ID) + (1|nest_ID), f)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

									    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=f$lon, y=f$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)

											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))

							dev.off()
				# e. after exchange
						f = ex_[-which(is.na(ex_$call_c_int) | is.na(ex_$call_int_c3)),]
						m = lmer(call_int_c3 ~ sex + scale(call_c_int)*sex + scale(day_j)*sex + (call_c_int|bird_ID) + (1|nest_ID), f)
									#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									  par(mfrow=c(5,3))

									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')
									  qqline(resid(m))

									  qqnorm(unlist(ranef(m)$bird_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$bird_ID [1]))

									  qqnorm(unlist(ranef(m)$bird_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$bird_ID[2]))

									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))


									  scatter.smooth(resid(m)~f$day_j);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$call_int_c2);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~f$sex);abline(h=0, lty=2, col='red')

									   mtext("lmer(call_int_c3 ~ sex + scale(call_c_int) + scale(day_j) + (call_c_int|bird_ID) + (1|nest_ID), f)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

									    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=f$lon, y=f$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)

											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))

							dev.off()

		# Table S6
			# prepare table data
				# a. calling while arriving
					f = dd[-which(is.na(dd$next_bout) | dd$left_before_presence == 'y' | is.na(dd$with_calling)),]
					f$w_call = ifelse(f$with_calling == 'y', 1, 0)
					nrow(f)
					length(unique(f$bird_ID))
					length(unique(f$nest_ID))
				
					m = lmer(next_bout ~as.factor(w_call)*sex_returning+(1|bird_ID)+(1|nest_ID),f)

					pred=c('Intercept (f, no)','return call','sex_returning(m)', 'Call:sex')
					dep = 'next bout'
					mod = 1
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)
				 	# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,1)
						oi$lwr_r=round(oi$lwr,1)
						oi$upr_r=round(oi$upr,1)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]
					# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
						ri$estimate_r = paste(ri$estimate_r,"%",sep='')
					o1=rbind(oii,ri)
				# b. reply
					f = dd[-which(is.na(dd$next_bout) | dd$left_before_presence == 'y' | !dd$with_calling %in% c('y') | is.na(dd$o_replies)),]
					f = f[f$left_type %in% c('3 during exchange'),]

					nrow(f)
					length(unique(f$bird_ID))
					length(unique(f$nest_ID))
				
					m = lmer(next_bout ~ o_replies*sex+(1|bird_ID)+(1|nest_ID),f)
					#m = lmer(next_bout ~ o_replies+sex+(1|bird_ID)+(1|nest_ID),f)
					pred=c('Intercept (f, no)','reply call','Sex(m)', 'Call:sex')
					dep = 'next bout'
					mod = 2
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)
				 	# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,1)
						oi$lwr_r=round(oi$lwr,1)
						oi$upr_r=round(oi$upr,1)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]
					# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
						ri$estimate_r = paste(ri$estimate_r,"%",sep='')
					o2=rbind(oii,ri)
				# c. calling
					f = dd[-which(is.na(dd$next_bout) | dd$left_before_presence == 'y' | is.na(dd$call_c_int) | is.na(dd$call_o_int) | is.na(dd$call_int_c2) | is.na(dd$call_int_c3)),]
					nrow(f)
					length(unique(f$bird_ID))
					length(unique(f$nest_ID))
			
					#ggplot(f, aes(x = as.factor(call_int_c3), y = next_bout, fill = sex_returning))+geom_boxplot()+xlab('calling after exchange')
						#cor(subset(f,select = c('call_c_int','call_o_int','call_int_c2','call_int_c3')),use="pairwise.complete.obs", method="pearson")
						#cor(subset(f,select = c('call_c_int','call_o_int','call_int_c2','call_int_c3')),use="pairwise.complete.obs", method="spearman")
					m = lmer(next_bout ~sex_returning*scale(call_c_int)+sex_returning*scale(call_o_int)+sex_returning*scale(call_int_c2) + sex_returning*scale(call_int_c3) + (call_int_c3|bird_ID) + (1|nest_ID),f)
					#m = lmer(next_bout ~sex*call_int_c3 + (call_int_c3|bird_ID) + (1|nest_ID),f)
					#summary(m)
					#summary(glht(m))
					#plot(allEffects(m))

					pred=c('Intercept (f)','sex_returning(m)', 'call_arriving', 'call_incub', 'call_gap', 'call_after', 'sex_returning:call_arriving','sex_returning:call_incub','sex_returning:call_gap', 'sex_returning:call_after')
					dep = 'next_bout'
					mod = 3
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)
				 	# Fixed effects
					v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
					ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))
					oi=data.frame(model=mod,dependent = dep, type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
					rownames(oi) = NULL
						oi$estimate_r=round(oi$estimate,1)
						oi$lwr_r=round(oi$lwr,1)
						oi$upr_r=round(oi$upr,1)
						#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
					oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]
					# Random effects var*100
					l=data.frame(summary(m)$varcor)
					l=l[is.na(l$var2),]
						ri=data.frame(model=mod,dependent = dep, type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
						ri$estimate_r = paste(ri$estimate_r,"%",sep='')
					o3=rbind(oii,ri)
			# create xlsx table
					o=rbind(o1,o2,o3)
					sname = 'Table_S6'
					tmp = write_xlsx(o, paste0(ta,sname,'.xlsx'))
					openFile(tmp)
		# model assumptions
				# a. calling while arriving
						f = dx[-which(is.na(dx$next_bout) | is.na(dx$w_call)),]
						m = lmer(next_bout ~as.factor(w_call)*sex+(1|bird_ID)+(1|nest_ID),f)

						scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
						scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')

						qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')  # what to do with the miss-fit
							qqline(resid(m))


						qqnorm(unlist(ranef(m)$bird_ID[1]), main = "bird",col='red')
						qqline(unlist(ranef(m)$bird_ID[1]))

						boxplot(resid(m)~f$w_call, xlab = 'sex');abline(h=0, lty=2, col='red')
						scatter.smooth(resid(m)~f$w_call);abline(h=0, lty=2, col='red')
						boxplot(resid(m)~f$sex, xlab = 'sex');abline(h=0, lty=2, col='red')
						scatter.smooth(resid(m)~f$sex);abline(h=0, lty=2, col='red')
						boxplot(resid(m)~f$sex:f$w_call, xlab = 'sex');abline(h=0, lty=2, col='red')

						acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))

						# spatial autocorrelations - nest location
							spdata=data.frame(resid=resid(m), x=f$lon, y=f$lat)
								spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
								#cex_=c(1,2,3,3.5,4)
								cex_=c(1,1.5,2,2.5,3)
								spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
								plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)

								plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8),xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
								plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8), xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
								#mtext("glmer(success_bin~prop_ip+uni_last+(1|sp),data=g,family='binomial')", side = 3, line = -1.2, cex=0.8,outer = TRUE)
				# b. reply
						f = dx_[-which(is.na(dx_$next_bout) | is.na(dx_$o_replies)),]
						m = lmer(next_bout ~ o_replies*sex+(1|bird_ID)+(1|nest_ID),f)

						scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
						scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')

						qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')  # what to do with the miss-fit
							qqline(resid(m))


						qqnorm(unlist(ranef(m)$bird_ID[1]), main = "bird",col='red')
						qqline(unlist(ranef(m)$bird_ID[1]))

						qqnorm(unlist(ranef(m)$nest_ID[1]), main = "nest",col='red')
						qqline(unlist(ranef(m)$nest_ID[1]))

						boxplot(resid(m)~f$sex, xlab = 'sex');abline(h=0, lty=2, col='red')
						scatter.smooth(resid(m)~f$sex);abline(h=0, lty=2, col='red')
						boxplot(resid(m)~f$o_replies, xlab = 'sex');abline(h=0, lty=2, col='red')
						scatter.smooth(resid(m)~as.factor(f$o_replies));abline(h=0, lty=2, col='red')
						boxplot(resid(m)~f$sex:f$o_replies, xlab = 'sex');abline(h=0, lty=2, col='red')

						acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))

						# spatial autocorrelations - nest location
							spdata=data.frame(resid=resid(m), x=f$lon, y=f$lat)
								spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
								#cex_=c(1,2,3,3.5,4)
								cex_=c(1,1.5,2,2.5,3)
								spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
								plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
								legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)

								plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8),xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
								plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8), xlim=c(-156.68,-156.6), ylim=c(71.31,71.33))
								#mtext("glmer(success_bin~prop_ip+uni_last+(1|sp),data=g,family='binomial')", side = 3, line = -1.2, cex=0.8,outer = TRUE)
				# c. calling coming ~ calling incubating
						f = ex_[-which(is.na(ex_$call_c_int) | is.na(ex_$call_o_int) | is.na(ex_$call_int_c2) | is.na(ex_$call_int_c3) | is.na(ex_$next_bout)),]
						m = lmer(next_bout ~sex_returning*scale(call_c_int)+sex_returning*scale(call_o_int)+sex_returning*scale(call_int_c2) + sex_returning*scale(call_int_c3) + (call_int_c3|bird_ID) + (1|nest_ID),f)
									#png(paste(out_,"model_ass/Supplementary_Table_2.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
									  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
									  par(mfrow=c(5,3))

									  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
									  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
									  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red')
									  qqline(resid(m))

									  qqnorm(unlist(ranef(m)$bird_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$bird_ID [1]))

									  qqnorm(unlist(ranef(m)$bird_ID[2]), main = "ran slope",col='red')
									  qqline(unlist(ranef(m)$bird_ID[2]))

									  qqnorm(unlist(ranef(m)$nest_ID [1]), main = "ran intercept",col='red')
									  qqline(unlist(ranef(m)$nest_ID [1]))


									  scatter.smooth(resid(m)~f$call_c_int);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$call_o_int);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$call_int_c2);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$call_int_c3);abline(h=0, lty=2, col='red')
									  scatter.smooth(resid(m)~f$sex);abline(h=0, lty=2, col='red')
									  boxplot(resid(m)~f$sex);abline(h=0, lty=2, col='red')

									   mtext("lmer(next_bout ~sex*scale(call_c_int)+sex*scale(call_o_int)+sex*scale(call_int_c2) + sex*scale(call_int_c3) + (call_int_c3|bird_ID) + (1|nest_ID),f)", side = 3, line = 0.5, cex=0.8,outer = TRUE)

									    acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
									  # spatial autocorrelations - nest location
										spdata=data.frame(resid=resid(m), x=f$lon, y=f$lat)
											spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
											#cex_=c(1,2,3,3.5,4)
											cex_=c(1,1.5,2,2.5,3)
											spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
											plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)

											plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
											plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))

							dev.off()

  # how leaving the nest? - create supplementary table
		# distributions - for cases without enclosure 
  			#summary(factor(dd$type_l))
  			# only those cases where cage was not present
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
									#png(paste(out_,"model_ass/Supplementary_Table_6.png", sep=""), width=6,height=9,units="in",res=600)
									  dev.new(width=6,height=9)
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
									  scatter.smooth(resid(m)~dc$sex);abline(h=0, lty=2, col='red')
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

							dev.off()


