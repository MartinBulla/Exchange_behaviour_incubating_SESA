### RESULTS section Behaviour prior to exchange ###

# SETTINGS & DATA
    # do you want plots in R (PNG=FALSE) or as PNG (PNG = TRUE)?
		PNG=FALSE #PNG = TRUE
	
	# define working directory
	     wd = "/Users/martinbulla/Dropbox/Science/MS/Exchanges_SESA/Analyses/Data/"# "C:/Users/mbulla/Documents/Dropbox/Science/MS/Exchanges_SESA/Database/"
		 wd2 = "/Users/martinbulla/Dropbox/Science/MS/Exchanges_SESA/Analyses/R/"#"C:/Users/mbulla/Documents/Dropbox/Science/MS/Exchanges_SESA/Analyses/"		 
	     outdir = "/Users/martinbulla/Dropbox/Science/MS/Exchanges_SESA/Analyses/plots/"#"C:/Users/mbulla/Documents/Dropbox/Science/MS/Exchanges_SESA/Analyses/plots/" 
	     ta = "/Users/martinbulla/Dropbox/Science/MS/Exchanges_SESA/Analyses/Tables/"
		#wd = "C:/Users/OWNER/Dropbox/exchanges_SESA/Database/"	
	    #wd = paste0(getwd(), '/DATA/')
	
	# load packages, constants and data
		source(paste0(wd2, 'Constants_packages_PrepareData.R'))
		
		dd$call_i=bb_$call_i[match(dd$obs_ID, bb_$obs_ID)]
		dd$fly_i=bb_$fly_i[match(dd$obs_ID, bb_$obs_ID)]
		dd$fly=as.factor(ifelse(dd$fly_i==0,'n','y'))

		bb_$dt_video=d$dt_video[match(bb_$obs_ID,d$obs_ID)]
		bb_$hour= as.numeric(difftime(bb_$dt_video, trunc(bb_$dt_video,"day"), units = "hours"))
		bb_$rad=as.numeric(bb_$hour)*pi/12
		bb_$n=1
		bb_$fly_bin=ifelse(bb_$fly_i==0,0,1)
		bb_$ob_sl = scale(log(bb_$obs_time))

# Calls and fly-offs prior to exchange and during regular incubation
  # distributions
		summary(factor(bb_$call_i))
		ggplot(bb_,aes(x=call_i, fill = type))+geom_density(alpha=0.5)
		ggplot(bb_,aes(x=call_i, fill = type))+geom_histogram(alpha=0.5, position = 'dodge')
		
		summary(factor(bb_$fly_i))
		summary(factor(bb_$fly_i[bb_$type=='ex']))
		ggplot(bb_,aes(x=fly_i, fill = type))+geom_density(alpha=0.5)
		ggplot(bb_,aes(x=fly_i, fill = type))+geom_histogram(alpha=0.5, position = 'dodge')
		
		ggplot(bb_,aes(x=log(obs_time), col = type))+geom_density(alpha=0.5)
		ggplot(bb_,aes(x=obs_time, col = type))+geom_density(alpha=0.5)
		
		ggplot(bb_,aes(x=log(obs_time), y=call_i, fill = type))+geom_point()
		ggplot(bb_,aes(x=log(obs_time), y=fly_i, fill = type))+geom_point()
  # Figure 1ab and within text info 
	  # run first 
	     # model predictions
	     	# calls
				m= glmer(call_i ~ type + (1|nest_ID), offset = log(obs_time/10),family = poisson,  bb_)# rate per 10 minute
					pred=c('Intercept (exchange)','Type (non-exchange)')
					nsim <- 5000
					bsim <- sim(m, n.sim=nsim)  
			
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
				# values to predict for		
					newD=data.frame(type=c('ex','non'))
						
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ type,data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-exp(X%*%v) 
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- exp(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pp=newD	
					pt_=pp[pp$type=='ex',]
					pc_=pp[pp$type=='non',]
			# fly-offs
				m= glmer(fly_bin ~ ob_sl+type + (1|nest_ID), family = binomial,  bb_)
						pred=c('Intercept (exchange)','Type (non-exchange)')
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
				
				# coefficients
					v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
				
				# values to predict for		
					newD=data.frame(ob_sl = mean(bb_$ob_sl),
									type=c('ex','non'))
						
				# exactly the model which was used has to be specified here 
					X <- model.matrix(~ ob_sl+type,data=newD)	
								
				# calculate predicted values and creditability intervals
					newD$pred <-plogis(X%*%v) 
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- plogis(X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					pp=newD	
					pe=pp[pp$type=='ex',]
					pn=pp[pp$type=='non',]			
		 # raw data
			 u=ddply(bb_,.(nest_ID, type), summarise,m=median(10*call_i/obs_time), q1=quantile(10*call_i/obs_time,0.25), q2= quantile(10*call_i/obs_time,0.75), n = sum(n))
			 u$type_j=jitter(ifelse(u$type=='ex',2,6)) 
		
			 x=ddply(bb_,.(nest_ID, type), summarise,m=median(fly_bin), q1=quantile(fly_bin,0.25), q2= quantile(fly_bin,0.75), n = sum(n))
			 x$type_j=jitter(ifelse(x$type=='ex',2,6))
	  # within text info
			pt_ # prediction for calling rate before arrival
			pc_ # prediction for calling rate control
			pe # prediction for fly-off probability before arrival
			pn # prediction for fly-off probability control
	  # plot
		 if(PNG == TRUE) {png(paste(outdir,"Figure_1ab.png", sep=""), width=1.85+0.6,height=1.5*2,units="in",res=600) 
			}else{dev.new(width=1.85+0.6,height=1.5*2)}	
		
		 par(mfrow=c(2,1),mar=c(0.25,0,0,1.2),oma = c(2.1, 2.2, 0.2, 2.4),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE, lwd=0.5) #col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7,
				
		 # calls
				plot(u$m~u$type_j, xlim=c(0,8), ylim=c(0,3),xaxt='n',yaxt='n',  ylab = "Number of calls",xlab = NULL,type='n')
				
				axis(2, at=seq(0,3,by=0.5), lwd = 0.35)
				mtext("Calling rate [calls/10min]",side=2,line=1, cex=0.55, las=3)
				text(0.1,3, expression(bold('a')),cex=0.6)
				
				for(i in 1:length(unique(u$nest_ID))){
							ui=u[u$nest_ID==unique(u$nest_ID)[i],]
							if(nrow(ui)==2){lines(ui$type_j, ui$m, col='grey90')}
							}
				arrows(x0=u$type_j, y0=u$q1,x1=u$type_j, y1=u$q2, code = 0, col=col_p, angle = 90, length = .025, lwd=0.5, lty=1)
				
				symbols(u$type_j, u$m, circles=sqrt(u$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) #
				
				
				
				# add predictions + 95%CI
					lines(c(2,6), c(pt_$pred,pc_$pred), col='red')
					
					arrows(x0=2, y0=pt_$lwr,x1=2, y1=pt_$upr, code = 0, col="red", angle = 90, length = .025, lwd=1.5, lty=1)
					arrows(x0=6, y0=pc_$lwr,x1=6, y1=pc_$upr, code = 0, col="red", angle = 90, length = .025, lwd=1.5, lty=1)
					
					points(y=pt_$pred,x=2, pch=20, cex=0.9,col="red")
					points(y=pc_$pred,x=6, pch=20, cex=0.9,col="red")
				
				# legend
					mtext(expression(italic('N')*' observations:'),side = 4,line=-0.3, padj=-7,cex=0.5,las=1,col='black', xpd=TRUE) # for printing into device use padj=-7.5
					if(PNG == TRUE){
					
					symbols(c(8.5,8.5,8.5),c(3.2,2.8,2.3)-0.75,circles=sqrt(c(1,10,20)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
					text(c(8.5,8.5,8.5)+1,c(3.2,2.8,2.3)-0.75,labels=c(1,10,20), xpd=TRUE, cex=0.5)#,col='grey30') 
					
					text(c(7.1),c(2.05),labels=c('Median & IQR'), xpd=TRUE, cex=0.5, srt=90, xpd=FALSE,col='grey30') #,col='grey30'
					}else{
					symbols(c(8.5,8.5,8.5),c(3.2,2.8,2.3)-0.5,circles=sqrt(c(1,10,20)/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) #bg=alpha(col_p,0.1)
					text(c(8.5,8.5,8.5)+1,c(3.2,2.8,2.3)-0.5,labels=c(1,10,20), xpd=TRUE, cex=0.5) #,col='grey30'
					
					text(c(7.1),c(2.05),labels=c('Median & IQR'), xpd=TRUE, cex=0.5, srt=90, xpd=FALSE,col='grey30') #
					}
					#arrows(x0=8.5,x1=8.5, y0=1-0.1, y1=1+0.1,  code = 0, col=col_p, angle = 90, length = .025, lwd=1.5, lty=1)
					#symbols(c(8.5),c(1),circles=sqrt(c(1)/pi),inches=0.03,bg=col_pb, fg=col_p,add=TRUE, xpd=TRUE) 
					#text(c(8.5)+1,c(1),labels=c('Median & IQR'), xpd=TRUE, cex=0.5,col='grey30', adj = 0, pos=4, xpd=FALSE) 
					
					#points(8.5, 1.5, pch=19,cex=0.9, col='red',xpd=NA)
					#arrows(x0=8.5,x1=8.5, y0=1.5-0.3, y1=1.5+0.3,  code = 0, col="red", angle = 90, length = .025, lwd=1.5, lty=1)
					#arrows(x0=7.75,x1=6.3, y0=2.2, y1=1.8,  code = 2, col=col_p, angle = 30, length = .025, lwd=1, lty=1)
					mtext('Predictions &\n95%CI',side = 4,line=-0.3,padj=2, cex=0.5,las=1,col='red', xpd=TRUE) 
							# use if plotting within RData
							#mtext('Weighted\nmedian',side = 4,line=3, cex=0.5,padj=-3.25,adj=0.5, las=1,col='grey30',xpd=TRUE)
							#points(12.5, 85, pch=19, col='black',xpd=NA)
		 # fly-offs
				plot(x$m~x$type_j, xlim=c(0,8), ylim=c(0,1),xaxt='n', yaxt='n',  ylab = "Number of calls",xlab = NULL,type='n')
				
				text(0.1,1, expression(bold('b')),cex=0.6)						
				axis(1, at=c(2,6), label=c('Before nest relief', 'Control'), mgp=c(0,-0.20,0), lwd= 0, col =NA)
				#mtext("Incubation",side=1,line=0.4, cex=0.5, las=1)#, col='grey30')
									
				axis(2, at=seq(0,1,by=0.2), lwd = 0.35)
				mtext("Fly-off probability",side=2,line=1, cex=0.55, las=3)#, col='grey30')
				
				for(i in 1:length(unique(x$nest_ID))){
							ui=x[x$nest_ID==unique(x$nest_ID)[i],]
							if(nrow(ui)==2){lines(ui$type_j, ui$m, col='grey90')}
							}
				
				arrows(x0=x$type_j, y0=x$q1,x1=x$type_j, y1=x$q2, code = 0, col=col_p, angle = 90, length = .025, lwd=0.5, lty=1)
				
				symbols(x$type_j, x$m, circles=sqrt(x$n/pi),inches=0.14/1.75,bg=col_pb, fg=col_p,add=TRUE) #
				
				
				# add predictions + 95%CI
					lines(c(2,6), c(pe$pred,pn$pred), col='red')
					arrows(x0=2, y0=pe$lwr,x1=2, y1=pe$upr, code = 0, col="red", angle = 90, length = .025, lwd=1.5, lty=1)
					arrows(x0=6, y0=pn$lwr,x1=6, y1=pn$upr, code = 0, col="red", angle = 90, length = .025, lwd=1.5, lty=1)
					
					points(y=pe$pred,x=2, pch=20, cex=0.9,col="red")
					points(y=pn$pred,x=6, pch=20, cex=0.9,col="red")
	  
	 	 if(PNG == TRUE) {dev.off()}
  # Table S1 - if you keep only one random slope - do not report it - just state it in the Table notes
	 # prepare table data
		# calling - type
			m= glmer(call_i ~ type + (1|nest_ID), offset = log(obs_time/10),family = poisson,  bb_)
				pred=c('Intercept (ex)','Type(non)')
					nsim <- 5000
					bsim <- sim(m, n.sim=nsim)  
			# Fixed effects
				v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
				ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
				oi=data.frame(model='1',dependent = 'calling rate / 10min', type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
				rownames(oi) = NULL
					oi$estimate_r=round(oi$estimate,3)
					oi$lwr_r=round(oi$lwr,3)
					oi$upr_r=round(oi$upr,3)
					#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
				oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
			# Random effects var*100
				l=data.frame(summary(m)$varcor)
				l=l[is.na(l$var2),]
					ri=data.frame(model='1',dependent = 'calling rate / 10min', type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
				o1=rbind(oii,ri)
		# calling type * incubation period, type * sex
			m= glmer(call_i ~ type*sex +  type*scale(day_j) + (1|nest_ID),offset = log(obs_time/10), family = poisson,  bb_) #model with random slope for day did not converge, but model controlled for date put without offset generated similar results
			#m= glmer(call_i ~ type*sex +  type*scale(day_j) + (day_j|nest_ID), family = poisson,  bb_)	
				pred=c('Intercept (ex & f)','Type(non)','Sex (m)', 'Day of incubation', 'Sex x type', 'Day x type')
				nsim <- 5000
				bsim <- sim(m, n.sim=nsim)  
			# Fixed effects
				v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
				ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
				oi=data.frame(model='2',dependent = 'calling rate / 10min', type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
				rownames(oi) = NULL
					oi$estimate_r=round(oi$estimate,3)
					oi$lwr_r=round(oi$lwr,3)
					oi$upr_r=round(oi$upr,3)
					#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
				oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
			# Random effects var*100
				l=data.frame(summary(m)$varcor)
				l=l[is.na(l$var2),]
					ri=data.frame(model='2',dependent = 'calling rate / 10min', type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
				o2=rbind(oii,ri)
		# fly-off - type
			m= glmer(fly_bin ~ scale(log(obs_time))+type + (1|nest_ID), family = binomial,  bb_)
				pred=c('Intercept (ex)','obs_timep','Type(non)')
				nsim <- 5000
				bsim <- sim(m, n.sim=nsim)  
			# Fixed effects
				v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
				ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
				oi=data.frame(model='3',dependent = 'fly-off rate / 10min', type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
				rownames(oi) = NULL
					oi$estimate_r=round(oi$estimate,3)
					oi$lwr_r=round(oi$lwr,3)
					oi$upr_r=round(oi$upr,3)
					#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
				oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
			# Random effects var*100
				l=data.frame(summary(m)$varcor)
				l=l[is.na(l$var2),]
					ri=data.frame(model='2',dependent = 'fly-off rate / 10min', type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
				o3=rbind(oii,ri)
		# fly-off type * incubation period	
			m= glmer(fly_bin ~ scale(log(obs_time)) + type*sex +  type*scale(day_j) +  (1|nest_ID),family = binomial,  bb_)
				pred=c('Intercept (ex & f)','obs_time','Type(non)','Sex (m)', 'Day of incubation', 'Sex x type', 'Day x type')
				nsim <- 5000
				bsim <- sim(m, n.sim=nsim)  
			# Fixed effects
				v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
				ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
				oi=data.frame(model='4',dependent = 'fly-off rate / 10min', type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
				rownames(oi) = NULL
					oi$estimate_r=round(oi$estimate,3)
					oi$lwr_r=round(oi$lwr,3)
					oi$upr_r=round(oi$upr,3)
					#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
				oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
			# Random effects var*100
				l=data.frame(summary(m)$varcor)
				l=l[is.na(l$var2),]
					ri=data.frame(model='4',dependent = 'fly-off rate / 10min', type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
				o4=rbind(oii,ri)	
	 # create xlsx table		
			o=rbind(o1,o2,o3,o4)
			sname = 'Table_S1'
			tmp = write_xlsx(o, paste0(ta,sname,'.xlsx'))
			openFile(tmp)	

# Cases where calling/flying occurred - was it closer to the exchange start? and if so is this sex specific
	# run first	  
		# time difference of each call/fly occurance to the end of observations session (only for those observatins sessions where calling occured)
			ba = b[, deltaT := difftime(end_pr, dt_behaviour, units = 'mins')%>% as.integer]
			bo = b_[, deltaT := difftime(end_pr, dt_behaviour, units = 'mins')%>% as.integer] # contains only cases where incubating bird left after its partner was present
  			
		bo$hour= as.numeric(difftime(bo$dt_behaviour, trunc(bo$dt_behaviour,"day"), units = "hours"))
		bo$rad=as.numeric(bo$hour)*pi/12
		bo$sex = as.factor(bo$sex)
			#h$sin_=sin(h$rad)
			#h$cos_=cos(h$rad)
		bo_ = bo[bo$behaviour == 'c' & bo$who == 'o' & bo$type == 'ex',]
		boo = bo[bo$behaviour == 'c' & bo$who == 'o',]
		length(unique(bo_$obs_ID))
			summary(factor(boo$type))
			x = ddply(boo,.(nest, obs_ID,type), summarise, n = length(type))
			summary(factor(x$type))
			length(unique(x$nest[x$type == "ex"]))
			length(unique(x$nest[x$type == "non"]))
			length(unique(x$nest))
		bf = bo[bo$behaviour == 'f' & bo$who == 'o' & bo$type == 'ex',]
		bff = bo[bo$behaviour == 'f' & bo$who == 'o',]
		length(unique(bf$obs_ID))
			summary(factor(bff$type))
			x = ddply(bff,.(nest, obs_ID,type), summarise, n = length(type))
			summary(factor(x$type))
			length(unique(x$nest[x$type == "ex"]))
			length(unique(x$nest[x$type == "non"]))
			length(unique(x$nest))					
	# distributions
		# calling	
			ggplot(bo_, aes(y = deltaT, x = obs_time))+geom_point()
			ggplot(boo, aes(x = deltaT,col = type))+geom_density()
			ggplot(boo, aes(x = deltaT/obs_time,col = type))+geom_density()
			ggplot(boo, aes(y = deltaT/obs_time,x = type))+geom_boxplot()
			ggplot(boo, aes(y = deltaT,x = type))+geom_boxplot()
			ggplot(bo_, aes(x = obs_time))+geom_density()
			ggplot(bo_, aes(x = deltaT/obs_time))+geom_density()
			ggplot(bo_, aes(x = deltaT/obs_time, fill = bird_ID))+geom_density(alpha=0.3)+ theme(legend.position="none")
			ggplot(bo_, aes(x = deltaT/obs_time, col = bird_ID))+geom_density() + theme(legend.position="none")
			ggplot(bo_, aes(x = deltaT/obs_time, y =  col = bird_ID))+geom_density() + theme(legend.position="none")
			
			ggplot(bo_, aes(x = deltaT/obs_time))+geom_histogram()
			
			ggplot(bo_, aes(y = deltaT/obs_time, x = who))+geom_boxplot()
			ggplot(bo_, aes(y = deltaT/obs_time, x = nest_ID))+geom_boxplot()
		
		# fly-offs
			ggplot(bff, aes(y = obs_time,x = type))+geom_boxplot()
			ggplot(bff, aes(x = deltaT,col = type))+geom_density()
			ggplot(bff, aes(x = deltaT/obs_time,col = type))+geom_density()
			ggplot(bff, aes(y = deltaT/obs_time,x = type))+geom_boxplot()
			ggplot(bff, aes(y = deltaT,x = type))+geom_boxplot()
			
			ggplot(bf, aes(y = deltaT, x = obs_time))+geom_point()
			ggplot(bf, aes(x = deltaT/obs_time))+geom_density()
			ggplot(bf, aes(x = deltaT/obs_time,fill = bird_ID))+geom_density(alpha=0.3) +  theme(legend.position="none")
			
			ggplot(bf, aes(x = deltaT/obs_time, col = sex))+geom_density()
			
			ggplot(bf, aes(y = deltaT/obs_time, x = who))+geom_boxplot()
			ggplot(bf, aes(y = deltaT/obs_time, x = nest_ID))+geom_boxplot()
	# Figure 1cd
		agg = FALSE
		# run first 
			# predictions
				# calling
					m = lmer(deltaT ~ type+(1|nest_ID/obs_ID) , boo)
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
						# values to predict for		
						v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
					newD=data.frame(type = c('ex', 'non'))
			
					# exactly the model which was used has to be specified here
					X <- model.matrix(~ type,data=newD)	
									
					# calculate predicted values and creditability intervals
						newD$pred <-(X%*%v) 
								predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
								for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
								newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
								newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
						pc=newD	
				# fly-offs
					 m = lmer(deltaT ~ type + (1|nest_ID/obs_ID) , bff)
						nsim <- 5000
						bsim <- sim(m, n.sim=nsim)  
						v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
						# exactly the model which was used has to be specified here
					 newD=data.frame(type = c('ex', 'non'))
					 X <- model.matrix(~ type,data=newD)	
							
					 # calculate predicted values and creditability intervals
					 newD$pred <-(X%*%v) 
							predmatrix <- matrix(nrow=nrow(newD), ncol=nsim)
							for(i in 1:nsim) predmatrix[,i] <- (X%*%bsim@fixef[i,])
							newD$lwr <- apply(predmatrix, 1, quantile, prob=0.025)
							newD$upr <- apply(predmatrix, 1, quantile, prob=0.975)
					 pp = newD		
			# raw data
					boo$n = 1
					u=ddply(boo,.(nest_ID,obs_ID, type), summarise,m=median(deltaT), q1=quantile(deltaT,0.25), q2= quantile(deltaT,0.75), n = sum(n))
					
					bff$n = 1
					w=ddply(bff,.(nest_ID,obs_ID, type), summarise,m=median(deltaT), q1=quantile(deltaT,0.25), q2= quantile(deltaT,0.75), n = sum(n))
					w$type_j=ifelse(w$type=='ex', 1,2)
					w$type_j=jitter(w$type_j)
						# dummy variable to keep bubbles in the fly off plot of same size as in calling plot
						w2=w[1,]
						w2$nest_ID=w2$obs_ID='xxx'
						w2$m = w2$q1 = w2$q2 = 2
						w2$n = 12
						w=rbind(w,w2)
		# plot
			if(PNG == TRUE) {
				png(paste(outdir,"Figure_1cd_new.png", sep=""), width=1.85+0.6,height=1.5*2,units="in",res=600) 
				}else{
				dev.new(width=1.85+0.6,height=1.5*2)
				}	
			
			# par(mfrow=c(2,1),mar=c(0.25,0,0,3.5),oma = c(2.1, 2.3, 0.2, 0),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="grey30",font.main = 1, col.lab="grey30", col.main="grey30", fg="grey70", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE) #	
			par(mfrow=c(2,1),mar=c(0.25,0,0,1.2),oma = c(2.1, 2.2, 0.2, 2.4),ps=12, mgp=c(1.2,0.35,0), las=1, cex=1, col.axis="black",font.main = 1, col.lab="black", col.main="black", fg="black", cex.lab=0.6,cex.main=0.7, cex.axis=0.5, tcl=-0.1,bty="n",xpd=TRUE,lwd=0.5) #	
			
			# call
				# prepare for plotting
					k=0.1 
					kk=k*2# distance for boxplots
					kkk=k*2 # distance for points
					boo$type_sex=ifelse(boo$type=='ex',ifelse(boo$sex=='f', 1-k,k+1),
									ifelse(boo$sex=='f', 4-k,k+4))
					boo$n = 1
					boo$col_=ifelse(boo$sex=='f','#FCB42C', '#535F7C')	
						boo$at=ifelse(boo$type=='ex',ifelse(boo$sex=='f', 1-kkk,2+kkk),
										ifelse(boo$sex=='f', 3.7-kkk,4.7+kkk))
					if(agg == TRUE){
						
						u=ddply(boo,.(nest_ID,obs_ID, type, type_sex,col_,at), summarise,deltaT=median(deltaT), q1=quantile(deltaT,0.25), q2= quantile(deltaT,0.75), n = sum(n))
						x=u
							
						}else{
						x = boo
						}				
				# plot
					boxplot(deltaT ~ type_sex, data = boo, 
									#ylab =NULL, 
									xaxt='n',
									yaxt='n',
									ylim=c(0,30),
									par(bty='n'),
									#at=c(1,2,3.5,4.5),
									at=c(1,2,3.7,4.7), type='n',
									outcex=0.5, outpch=20,boxwex=0.25,whisklty=1,staplelty=0,#medlwd=1, 
									lwd = 0.25, 
									#ylim=c(0,1),
									outcol="white",boxcol='white',whiskcol='white',staplecol='white',medcol='white'
									) # col=z_g$cols, border=z_g$cols			
					# TRY IT WITH SYMBOL
					for (i in 1:nrow(x)){stripchart(x$deltaT[i]~ factor(x$type_sex[i]), at = x$at[i],
										#bg = x$col_[i],
										col="gray63",
										#col = x$col_[i],
										bg = adjustcolor(x$col_[i], alpha.f = 0.4),
										pch =21, cex=0.5,
										vertical = TRUE, add = TRUE, method = "jitter") 
										}
				
					boxplot(deltaT ~ type_sex, data = boo, 
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
					text(0.5,30, expression(bold('c')),cex=0.6, col = 'black')
					axis(2, at=seq(0,30,by=5), lwd = 0.35)			
					mtext("Call\n[min to observation end]",side=2,line=1, cex=0.55, las=3)
					#mtext('Predictions &\n95%CI',side = 4,line=0.5,padj=-2, cex=0.5,las=1,col='red', xpd=TRUE) 
					
					# predictions + 95%CI 
						points(y=pc$pred,x=c(1.5,4.2), pch=20, cex=0.9,col="red")
						arrows(x0=c(1.5,4.2), y0=pc$lwr,x1=c(1.5,4.2), y1=pc$upr,
						code = 0, col="red", angle = 90, length = .025, lwd=1, lty=1)
			# fly
			 	# prepare for plotting
					k=0.1 
					kk=k*2# distance for boxplots
					kkk=k*2 # distance for points
					bff$type_sex=ifelse(bff$type=='ex',ifelse(bff$sex=='f', 1-k,k+1),
									ifelse(bff$sex=='f', 4-k,k+4))
					x = bff
					x$col_=ifelse(x$sex=='f','#FCB42C', '#535F7C')	
					x$at=ifelse(x$type=='ex',ifelse(x$sex=='f', 1-kkk,2+kkk),
									ifelse(x$sex=='f', 3.7-kkk,4.7+kkk))	
				# plot
				  boxplot(deltaT ~ type_sex, data = bff, 
									#ylab =NULL, 
									xaxt='n',
									yaxt='n',
									ylim=c(0,30),
									par(bty='n'),
									#at=c(1,2,3.5,4.5),
									at=c(1,2,3.7,4.7), type='n',
									outcex=0.5, outpch=20,boxwex=0.25,whisklty=1,staplelty=0,#medlwd=1, 
									lwd = 0.25, 
									#ylim=c(0,1),
									outcol="white",boxcol='white',whiskcol='white',staplecol='white',medcol='white'
									) # col=z_g$cols, border=z_g$cols
									
							
				  for (i in 1:nrow(x)){stripchart(x$deltaT[i]~ factor(x$type_sex[i]), at = x$at[i],
										#bg = x$col_[i],
										col="gray63",
										#col = x$col_[i],
										bg = adjustcolor(x$col_[i], alpha.f = 0.4),
										pch =21, cex=0.5,
										vertical = TRUE, add = TRUE, method = "jitter") 
										}
				
				  boxplot(deltaT ~ type_sex, data = bff, 
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
				
				axis(1, at=c(1.5,4.2), label=c('Before arrival', 'Regular'), cex = 0.5, mgp=c(0,-0.20,0), lwd= 0, col =NA) #text(c(1.5,4.2), par("usr")[3]-2, labels = c('Before nest relief','Control'),  xpd = TRUE, cex=0.5, col="grey30")
				axis(1, at=mean(c(1.5,4.2)), label=c('Part of incubation'), cex = 0.5, mgp=c(0,0.20,0), lwd= 0, col =NA)
				axis(2, at=seq(0,30,by=5), lwd = 0.35)		
				mtext("Fly-off\n[min to observation end]",side=2,line=1, cex=0.55, las=3)
				text(0.5,30, expression(bold('d')),cex=0.6, col = 'black')
				
				
				#labels 
					text(c(1,2,3.7,4.7), par("usr")[3]+0.4, labels = c('\u2640','\u2642'), font=4, xpd = TRUE, cex=0.6, col=c('#FCB42C','#535F7C'))#col="grey30") 

							
				# predictions	+ 95%CI 
					points(y=pp$pred,x=c(1.5,4.2), pch=20, cex=0.9,col="red")
					arrows(x0=c(1.5,4.2), y0=pp$lwr,x1=c(1.5,4.2), y1=pp$upr,
					code = 0, col="red", angle = 90, length = .025, lwd=1, lty=1)
						
			if(PNG == TRUE) {dev.off()}
	# Table S2 and within text info
		# prepare table data	
			# calling simple
				m = lmer(deltaT ~ type+(1|nest_ID/obs_ID) , boo)
					pred=c('Intercept(ex)','non')
					nsim <- 5000
					bsim <- sim(m, n.sim=nsim)  
				# Fixed effects
				v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
				ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
				oi=data.frame(model='1',dependent = 'proportion of observation period calling occured', type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
				rownames(oi) = NULL
					oi$estimate_r=round(oi$estimate,2)
					oi$lwr_r=round(oi$lwr,2)
					oi$upr_r=round(oi$upr,2)
					#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
				oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				
				# Random effects var*100
				l=data.frame(summary(m)$varcor)
				l=l[is.na(l$var2),]
					ri=data.frame(model='1',dependent = 'proportion of observation period calling occured', type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					ri$estimate_r = paste(ri$estimate_r,"%",sep='')
				o1=rbind(oii,ri)
			# calling sex
				m = lmer(deltaT ~ type*sex+(1|nest_ID/obs_ID) , boo)
					pred=c('Intercept (ex,f)', 'type(non)','sex(m)','type:sex')
					nsim <- 5000
					bsim <- sim(m, n.sim=nsim)  
				# Fixed effects
				v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
				ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
				oi=data.frame(model='2',dependent = 'proportion of observation period calling occured', type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
				rownames(oi) = NULL
					oi$estimate_r=round(oi$estimate,2)
					oi$lwr_r=round(oi$lwr,2)
					oi$upr_r=round(oi$upr,2)
					#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
				oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
				l=data.frame(summary(m)$varcor)
				l=l[is.na(l$var2),]
					ri=data.frame(model='2',dependent = 'proportion of observation period calling occured', type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					ri$estimate_r = paste(ri$estimate_r,"%",sep='')
				o2=rbind(oii,ri)
			# fly-off simple
				m = lmer(deltaT ~ type+(1|nest_ID/obs_ID) , bff)
					pred=c('Intercept(ex)','non')
					nsim <- 5000
					bsim <- sim(m, n.sim=nsim)  
				# Fixed effects
				v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
				ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
				oi=data.frame(model='3',dependent = 'proportion of observation period fly-off occured', type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
				rownames(oi) = NULL
					oi$estimate_r=round(oi$estimate,2)
					oi$lwr_r=round(oi$lwr,2)
					oi$upr_r=round(oi$upr,2)
					#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
				oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
				l=data.frame(summary(m)$varcor)
				l=l[is.na(l$var2),]
					ri=data.frame(model='3',dependent = 'proportion of observation period fly-off occured', type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					ri$estimate_r = paste(ri$estimate_r,"%",sep='')
				o3=rbind(oii,ri)
			# fly-off sex
				m = lmer(deltaT ~ type*sex+(1|nest_ID/obs_ID) , bff)
					pred=c('Intercept (ex,f)', 'type(non)','sex(m)','type:sex')
					nsim <- 5000
					bsim <- sim(m, n.sim=nsim)  
				# Fixed effects
				v <- apply(bsim@fixef, 2, quantile, prob=c(0.5))
				ci=apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))	
				oi=data.frame(model='4',dependent = 'proportion of observation period fly-off occured', type='fixed',effect=pred,estimate=v, lwr=ci[1,], upr=ci[2,])
				rownames(oi) = NULL
					oi$estimate_r=round(oi$estimate,2)
					oi$lwr_r=round(oi$lwr,2)
					oi$upr_r=round(oi$upr,2)
					#oi$CI=paste("(", oi$lwr_r, "-", oi$upr_r, ")", sep = "", collapse = NULL)
				oii=oi[c('model','dependent','type',"effect", "estimate_r","lwr_r",'upr_r')]	
				# Random effects var*100
				l=data.frame(summary(m)$varcor)
				l=l[is.na(l$var2),]
					ri=data.frame(model='4',dependent = 'proportion of observation period fly-off occured', type='random (var)',effect=l$var1, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
					ri$estimate_r = paste(ri$estimate_r,"%",sep='')
				o4=rbind(oii,ri)	
		# create xlsx table		
				  o=rbind(o1,o2,o3,o4)
				  sname = 'Table_S2'
				  tmp = write_xlsx(o, paste0(ta,sname,'.xlsx'))
				  openFile(tmp)		
		# within text info
			o1[2,] # difference between timing of calls in control and before arrival
			o3[2,] # difference between timing of calls in control and before arrival			  
			  
# model assumptions
	# Table S1a - calling - type
		  if(PNG == TRUE){png(paste(outdir,"model_ass/Table_S1a.png", sep=""), width=6,height=9,units="in",res=600)}else{dev.new(width=6,height=9)}	
		  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
		  m= glmer(call_i ~ type + (1|nest_ID), offset = log(obs_time/10),family = poisson,  bb_)
							 								  
		  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
		  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
		  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
		  qqline(resid(m))
		  
		  qqnorm(unlist(ranef(m)$nest_ID [1]), main = " intercept",col='red')
		  qqline(unlist(ranef(m)$nest_ID [1]))
		  
		  #qqnorm(unlist(ranef(m)$nest_ID[2]), main = " slope",col='red')
		  #qqline(unlist(ranef(m)$nest_ID[2]))
		  
		  #scatter.smooth(resid(m)~x2$date[x2$sum>720]);abline(h=0, lty=2, col='red')
		  scatter.smooth(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
		  boxplot(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
		  
		  mtext("glmer(call_i ~ type + (1|nest_ID), offset = log(obs_time/10),family = poisson,  bb_)", side = 3, line = 0.5, cex=0.8,outer = TRUE)
		   
		  mtext(paste("overdispersion:", round(dispersion_glmer(m),3)), side = 3, line = -2, cex=0.8,outer = TRUE)
										  
		  acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
		  
		  # spatial autocorrelations - nest location
			spdata=data.frame(resid=resid(m), x=bb_$lon, y=bb_$lat)
				spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
				#cex_=c(1,2,3,3.5,4)
				cex_=c(1,1.5,2,2.5,3)
				spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
				plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
				legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
				
				plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
				plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))

			
			if(PNG == TRUE){dev.off()}
	# Table S1b - calling type * incubation period, type * sex
			  if(PNG == TRUE){png(paste(outdir,"model_ass/Table_S1b.png", sep=""), width=6,height=9,units="in",res=600)}else{dev.new(width=6,height=9)}	
		  	  par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
			  m= glmer(call_i ~ type*sex +  type*scale(day_j) + (1|nest_ID),offset = log(obs_time/10), family = poisson,  bb_) 
			
			  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
			  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
			  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
			  qqline(resid(m))
			  
			  qqnorm(unlist(ranef(m)$nest_ID [1]), main = " intercept",col='red')
			  qqline(unlist(ranef(m)$nest_ID [1]))
			  
			  #qqnorm(unlist(ranef(m)$nest_ID[2]), main = " slope",col='red')
			  #qqline(unlist(ranef(m)$nest_ID[2]))
			  
			  scatter.smooth(resid(m)~bb_$day_j);abline(h=0, lty=2, col='red')
			  scatter.smooth(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
			  boxplot(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
			  
			  scatter.smooth(resid(m)~factor(bb_$sex));abline(h=0, lty=2, col='red')
			  boxplot(resid(m)~bb_$sex);abline(h=0, lty=2, col='red')
			  
			  mtext("glmer(call_i ~ type*sex +  type*scale(day_j) + (1|nest_ID),offset = log(obs_time/10), family = poisson) ", side = 3, line = 0.5, cex=0.8,outer = TRUE)
			   
			  mtext(paste("overdispersion:", round(dispersion_glmer(m),3)), side = 3, line = -2, cex=0.8,outer = TRUE)
											  
			  acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
			  
			  # spatial autocorrelations - nest location
				spdata=data.frame(resid=resid(m), x=bb_$lon, y=bb_$lat)
					spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
					#cex_=c(1,2,3,3.5,4)
					cex_=c(1,1.5,2,2.5,3)
					spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
					plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
					legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
					
					plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
					plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
					
				if(PNG == TRUE){dev.off()}
	# Table S1c - fly-off - type
			if(PNG == TRUE){png(paste(outdir,"model_ass/Table_S1c.png", sep=""), width=6,height=9,units="in",res=600)}else{dev.new(width=6,height=9)}	
		  	par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
			m= glmer(fly_bin ~ scale(log(obs_time))+type + (1|nest_ID), family = binomial,  bb_)
			    
			scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
			scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
					  
			plot(fitted(m), jitter(bb_$fly_bin, amount=0.05), xlab="Fitted values", ylab="Probability of left", las=1, cex.lab=1.2, cex=0.8)
			abline(0,1, lty=3)
			t.breaks <- cut(fitted(m), quantile(fitted(m)))
			means <- tapply(bb_$fly_bin, t.breaks, mean)
			semean <- function(x) sd(x)/sqrt(length(x))
			means.se <- tapply(bb_$fly_bin, t.breaks, semean)
			points(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means, pch=16, col="orange")
			segments(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means-2*means.se, quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means+2*means.se,lwd=2, col="orange")
			
			qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
			qqline(resid(m))
					  
			qqnorm(unlist(ranef(m)$nest_ID [1]), main = " intercept",col='red')
			qqline(unlist(ranef(m)$nest_ID [1]))
					  
					  #qqnorm(unlist(ranef(m)$nest_ID[2]), main = " slope",col='red')
					  #qqline(unlist(ranef(m)$nest_ID[2]))
					  
			scatter.smooth(resid(m)~log(bb_$obs_time));abline(h=0, lty=2, col='red')
			scatter.smooth(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
			boxplot(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
					  
			mtext("glmer(fly_bin ~ scale(log(obs_time))+type + (1|nest_ID), family = binomial", side = 3, line = 0.5, cex=0.8,outer = TRUE)
					   
			#mtext(paste("overdispersion:", round(dispersion_glmer(m),3)), side = 3, line = -2, cex=0.8,outer = TRUE)
													  
			acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
					  # spatial autocorrelations - nest location
						spdata=data.frame(resid=resid(m), x=bb_$lon, y=bb_$lat)
							spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
							#cex_=c(1,2,3,3.5,4)
							cex_=c(1,1.5,2,2.5,3)
							spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
							plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
							legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
							
							plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
							plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
				
			if(PNG == TRUE){dev.off()}	
	# Table S1d - fly-off type * incubation period	
			if(PNG == TRUE){png(paste(outdir,"model_ass/Table_S1d.png", sep=""), width=6,height=9,units="in",res=600)}else{dev.new(width=6,height=9)}	
		  	par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )
		
			m= glmer(fly_bin ~ scale(log(obs_time)) + type*sex +  type*scale(day_j) +  (1|nest_ID),family = binomial,  bb_)
			
			scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
			scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
						  
			plot(fitted(m), jitter(bb_$fly_bin, amount=0.05), xlab="Fitted values", ylab="Probability of left", las=1, cex.lab=1.2, cex=0.8)
				abline(0,1, lty=3)
				t.breaks <- cut(fitted(m), quantile(fitted(m)))
				means <- tapply(bb_$fly_bin, t.breaks, mean)
				semean <- function(x) sd(x)/sqrt(length(x))
				means.se <- tapply(bb_$fly_bin, t.breaks, semean)
				points(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means, pch=16, col="orange")
				segments(quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means-2*means.se, quantile(fitted(m),c(0.125,0.375,0.625,0.875)), means+2*means.se,lwd=2, col="orange")
				
			  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
			  qqline(resid(m))
			  
			  qqnorm(unlist(ranef(m)$nest_ID [1]), main = " intercept",col='red')
			  qqline(unlist(ranef(m)$nest_ID [1]))
			  
			  #qqnorm(unlist(ranef(m)$nest_ID[2]), main = " slope",col='red')
			  #qqline(unlist(ranef(m)$nest_ID[2]))
			  
			  scatter.smooth(resid(m)~log(bb_$obs_time));abline(h=0, lty=2, col='red')
			  scatter.smooth(resid(m)~bb_$day_j);abline(h=0, lty=2, col='red')
			  scatter.smooth(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
			  boxplot(resid(m)~bb_$type);abline(h=0, lty=2, col='red')
			  
			  scatter.smooth(resid(m)~factor(bb_$sex));abline(h=0, lty=2, col='red')
			  boxplot(resid(m)~bb_$sex);abline(h=0, lty=2, col='red')
			  
			   mtext("glmer(fly_bin ~ scale(log(obs_time)) + type*sex +  type*scale(day_j) +  (1|nest_ID),family = binomial) ", side = 3, line = 0.5, cex=0.8,outer = TRUE)
			   
			   #mtext(paste("overdispersion:", round(dispersion_glmer(m),3)), side = 3, line = -2, cex=0.8,outer = TRUE)
											  
			  acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
			  # spatial autocorrelations - nest location
				spdata=data.frame(resid=resid(m), x=bb_$lon, y=bb_$lat)
					spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
					#cex_=c(1,2,3,3.5,4)
					cex_=c(1,1.5,2,2.5,3)
					spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
					plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
					legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
					
					plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
					plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
		
			if(PNG == TRUE){dev.off()}

	# Table S2a - calling simple
	    if(PNG == TRUE){png(paste(outdir,"model_ass/Table_S2a.png", sep=""), width=6,height=9,units="in",res=600)}else{dev.new(width=6,height=9)}
	    par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )

		m = lmer(deltaT ~ type+(1|nest_ID/obs_ID) , boo)
				
												  
		  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
		  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
		  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
		  qqline(resid(m))
		  
		  qqnorm(unlist(ranef(m)$nest_ID [1]), main = " intercept",col='red')
		  qqline(unlist(ranef(m)$nest_ID [1]))
		  
		  scatter.smooth(resid(m)~boo$type);abline(h=0, lty=2, col='red')
		  boxplot(resid(m)~boo$type);abline(h=0, lty=2, col='red')
		  
		  mtext("lmer(deltaT ~ type+(1|nest_ID/obs_ID) , boo)", side = 3, line = 0.5, cex=0.8,outer = TRUE)
		   
		  							  
		  acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
		  # spatial autocorrelations - nest location
			spdata=data.frame(resid=resid(m), x=boo$lon, y=boo$lat)
				spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
				#cex_=c(1,2,3,3.5,4)
				cex_=c(1,1.5,2,2.5,3)
				spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
				plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
				legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
				
				plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
				plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
	
		if(PNG == TRUE){dev.off()}		
	# Table S2b - calling sex
		if(PNG == TRUE){png(paste(outdir,"model_ass/Table_S2b.png", sep=""), width=6,height=9,units="in",res=600)}else{dev.new(width=6,height=9)}
	    par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )		

		m = lmer(deltaT ~ type*sex+(1|nest_ID/obs_ID) , boo)
							
		  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
		  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
		  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
		  qqline(resid(m))
		  
		  qqnorm(unlist(ranef(m)$nest_ID [1]), main = " intercept",col='red')
		  qqline(unlist(ranef(m)$nest_ID [1]))
		  
		  scatter.smooth(resid(m)~boo$sex);abline(h=0, lty=2, col='red')
		  boxplot(resid(m)~boo$sex);abline(h=0, lty=2, col='red')
		  
		   mtext("lmer(deltaT ~ type*sex+(1|nest_ID/obs_ID) , boo)", side = 3, line = 0.5, cex=0.8,outer = TRUE)
		   
		   #mtext(paste("overdispersion:", round(dispersion_glmer(m),3)), side = 3, line = -2, cex=0.8,outer = TRUE)
										  
		  acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
		  # spatial autocorrelations - nest location
			spdata=data.frame(resid=resid(m), x=boo$lon, y=boo$lat)
				spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
				#cex_=c(1,2,3,3.5,4)
				cex_=c(1,1.5,2,2.5,3)
				spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
				plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
				legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
				
				plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
				plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
	
		if(PNG == TRUE){dev.off()}
	# Table S2c - calling simple
		if(PNG == TRUE){png(paste(outdir,"model_ass/Table_S2c.png", sep=""), width=6,height=9,units="in",res=600)}else{dev.new(width=6,height=9)}
	    par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )		

		m = lmer(deltaT ~ type+(1|nest_ID/obs_ID) , bff)
							 								  
		  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
		  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
		  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
		  qqline(resid(m))
		  
		  qqnorm(unlist(ranef(m)$nest_ID [1]), main = " intercept",col='red')
		  qqline(unlist(ranef(m)$nest_ID [1]))
		  
		  scatter.smooth(resid(m)~bff$type);abline(h=0, lty=2, col='red')
		  boxplot(resid(m)~bff$type);abline(h=0, lty=2, col='red')
		  
		   mtext("lmer(deltaT ~ type+(1|nest_ID/obs_ID) , bff)", side = 3, line = 0.5, cex=0.8,outer = TRUE)
		   
		   #mtext(paste("overdispersion:", round(dispersion_glmer(m),3)), side = 3, line = -2, cex=0.8,outer = TRUE)
										  
		  acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
		  # spatial autocorrelations - nest location
			spdata=data.frame(resid=resid(m), x=bff$lon, y=bff$lat)
				spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
				#cex_=c(1,2,3,3.5,4)
				cex_=c(1,1.5,2,2.5,3)
				spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
				plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
				legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
				
				plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
				plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
	
		if(PNG == TRUE){dev.off()}	
	# Table S2d - calling sex
		if(PNG == TRUE){png(paste(outdir,"model_ass/Table_S2d.png", sep=""), width=6,height=9,units="in",res=600)}else{dev.new(width=6,height=9)}
	    par(mfrow=c(5,3),oma = c(0, 0, 1.5, 0) )		

		m = lmer(deltaT ~ type*sex+(1|nest_ID/obs_ID) , bff)
													  
		  scatter.smooth(fitted(m),resid(m),col='red');abline(h=0, lty=2)
		  scatter.smooth(fitted(m),sqrt(abs(resid(m))), col='red')
		  qqnorm(resid(m), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='red') 
		  qqline(resid(m))
		  
		  qqnorm(unlist(ranef(m)$nest_ID [1]), main = " intercept",col='red')
		  qqline(unlist(ranef(m)$nest_ID [1]))
		  
		  scatter.smooth(resid(m)~bff$type);abline(h=0, lty=2, col='red')
		  boxplot(resid(m)~bff$type);abline(h=0, lty=2, col='red')

		  scatter.smooth(resid(m)~bff$sex);abline(h=0, lty=2, col='red')
		  boxplot(resid(m)~bff$sex);abline(h=0, lty=2, col='red')
		 
		  mtext("lmer(deltaT ~ type*sex+(1|nest_ID/obs_ID) , bff)", side = 3, line = 0.5, cex=0.8,outer = TRUE)
		   	  
		  acf(resid(m), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
		  # spatial autocorrelations - nest location
			spdata=data.frame(resid=resid(m), x=bff$lon, y=bff$lat)
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