#' draw a confidence ellipse for cost-effectiveness data
#'
#' This function draws a confidence ellipse using either person-level or summary-level cost-effectiveness data.
#' @param cost a vector for cost of individuals.
#' @param effect a vector for effectiveness of individuals.
#' @param treatment a vector for treatment indicator of individuals (1=considered treatment, 0=comparison).
#' @param dc mean cost difference between considered treatment and comparison.
#' @param se_dc standard error of the mean cost difference.
#' @param de mean effect difference between considered treatment and comparison.
#' @param se_de standard error of the mean effect difference.
#' @param corr_dc_de correlation between the mean cost difference and mean effect difference.
#' @param conf.level a scalar for the level of confidence ellipse. Defaults to 0.95.
#' @param add logicial. If TRUE, will add the ellipse to the existing plot, instead of creating a new plot. Defaults to FALSE. If TRUE, xlim, ylim, xlab, ylab, ce.point will not be used even provided.
#' @param printsum logicial. If TRUE and person-level data are provided, will print the obtained summary statistics. Defaults to TRUE. 
#' @param ce.point logicial. If TRUE and add=FALSE, will add a point in the figure for the point estimates of cost difference and effect difference. Defaults to TRUE. 
#' @param pch a numeric value specifying the point. The 'points' help file contains examples of the possible marks. Defaults to 16.
#' @param n.point number of points used to draw the ellipse. Defaults to 100.
#' @param axis.lwd line width used to draw the axis crossing origin point. Defaults to 1.
#' @param axis.col color used to draw the axis crossing origin point. Defaults to "black".
#' @param col color used to draw the ellipse. Defaults to "gray".
#' @param xlim a vector including 2 numeric values, specifying the limits of x-axis. 
#' @param ylim a vector including 2 numeric values, specifying the limits of y-axis. 
#' @param xlab label given to the x-axis. Defaults to "Incremental Effect".
#' @param ylab label given to the y-axis. Defaults to "Incremental Cost".
#' @param ... other arguments that will be passed forward to the underlying plot.default method for drawing the ellipse. 
#' @keywords confidence ellipse, cost-effectiveness

CEellipse<-function(cost=NULL, effect=NULL, treatment=NULL, dc=NULL, se_dc=NULL, de=NULL, se_de=NULL, corr_dc_de=NULL,
	conf.level=0.95, add=FALSE, printsum=TRUE, ce.point=TRUE, pch=16, n.point=100, axis.lwd=1, axis.col="black", col="gray", xlim=NULL, ylim=NULL, xlab = "Incremental Effect", ylab="Incremental Cost",...)
{

  ##### check data for possible errors in input data

  if(!is.numeric(conf.level)) stop("conf.level must be a numeric value.\n")
  if((conf.level>=1)|(conf.level<=0)) stop("conf.level must be between 0 and 1.\n")

  if(!is.numeric(n.point)) stop("n.point must be an integer.\n")
  n.point=ceiling(n.point)-1
  if(n.point<2) stop("n.point is too small.\n")
  if(n.point<10) warning("n.point is too small and ellipse is not smooth. Recommend to use a larger n.point.\n")

if((!is.null(cost))&(!is.null(cost))&(!is.null(cost)))	#person-level data provided
{
  cost=unlist(cost)
  if(!is.numeric(cost)) stop("cost must be a numeric vector.\n")
  effect=unlist(effect)
  if(!is.numeric(effect)) stop("effect must be a numeric vector.\n")
  treatment=unlist(as.numeric(as.character(treatment)))
  if(!is.numeric(treatment)) stop("treatment must be a numeric vector.\n")
  
  n=length(cost)
  if(length(effect)!=n) stop("Length of effect is different to length of cost.\n")
  if(length(treatment)!=n) stop("Length of treatment is different to length of cost.\n") 

  if(n==0) stop("No cost provided.\n")
  
  if(!identical(levels(as.factor(treatment)),c("0","1"))) stop("Values in treatment must be 0 or 1 and cannot be in one group only.\n")

  if((!is.null(dc))&(!is.null(se_dc))&(!is.null(de))&(!is.null(se_de))&(!is.null(corr_dc_de))) 
	warning("Both person- and summary-level data are provided. Only person-level data are used.\n")
  else cat("Person-level data are used.\n")

  use.person=TRUE

}else{			#summary-level data provided

  if((is.null(dc))|(is.null(se_dc))|(is.null(de))|(is.null(se_de))|(is.null(corr_dc_de))) 
	stop("Either person-level (cost, effect, treatment) or summary-level (dc, se_dc, de, se_de, corr_dc_de) data are required.\n")
  if(!is.numeric(dc)) stop("dc must be numeric.\n")
  if(!is.numeric(se_dc)) stop("se_dc must be numeric.\n")
  if(!is.numeric(de)) stop("de must be numeric.\n")
  if(!is.numeric(se_de)) stop("se_de must be numeric.\n")
  if(!is.numeric(corr_dc_de)) stop("corr_dc_de must be numeric.\n")

  if(se_dc<=0) stop("se_dc must be positive.\n")
  if(se_de<=0) stop("se_de must be positive.\n")

  cat("Summary statistics are used.\n")
  use.person=FALSE
}

 ###### use person-level data to obtain summary statistics

 if(use.person){
	n1=sum(treatment==1)
	n0=n-n1
	if((n1<2)|(n0<2)) stop("At least 2 individuals in each treatment are required.\n")

	c1=mean(cost[treatment==1])
	c0=mean(cost[treatment==0])
	cv1=var(cost[treatment==1])
	cv0=var(cost[treatment==0])
	dc=c1-c0
	se_dc=sqrt(cv1/n1+cv0/n0)
	e1=mean(effect[treatment==1])
	e0=mean(effect[treatment==0])
	ev1=var(effect[treatment==1])
	ev0=var(effect[treatment==0])
	de=e1-e0
	se_de=sqrt(ev1/n1+ev0/n0)
	cov_dc_de=cov(cost[treatment==1],effect[treatment==1])/n1+cov(cost[treatment==0],effect[treatment==0])/n0
	corr_dc_de=cov_dc_de/se_dc/se_de

	# print summary statistics
	if(printsum){
		cat("Summary Statistics:\ndc (mean cost difference):", dc, "\nse_dc (SE of mean cost difference):",
		se_dc, "\nde (mean effect difference):", de, "\nse_de (SE of mean effect difference):",
		se_de, "\ncorr_dc_de (corr(mean cost difference, mean effect difference)):",corr_dc_de,"\n")
	}
  }

  ###### draw ellipse using the summary statistics

  theta=seq(0,2*pi+0.001,2*pi/(n.point+1))
  dc_ell=de_ell=rep(NA,length(theta))
  for(i in 1:length(theta))
  {
	cons1=sqrt(-2*log(1-conf.level))
	cons2=acos(corr_dc_de)/2
	dc_ell[i]=cons1*se_dc*cos(theta[i]-cons2)+dc
	de_ell[i]=cons1*se_de*cos(theta[i]+cons2)+de
  }

  if(!add){		# create a new plot, include origin point, add vertical and horizantal axis lines

	if (is.null(ylim)) {
		ylim=c(min(0,min(dc_ell)),max(0,max(dc_ell)))
		ylim[1]=ylim[1]-0.1*(ylim[2]-ylim[1])
		ylim[2]=ylim[2]+0.1*(ylim[2]-ylim[1])
	}
	if (is.null(xlim)) {
		xlim=c(min(0,min(de_ell)),max(0,max(de_ell)))
		xlim[1]=xlim[1]-0.1*(xlim[2]-xlim[1])
		xlim[2]=xlim[2]+0.1*(xlim[2]-xlim[1])
	}

	graphics::plot(c(1.2*xlim[1]-0.2*xlim[2],-0.2*xlim[1]+1.2*xlim[2]),c(0,0), type="l", lwd=axis.lwd, col=axis.col,			# horizantal axis line
             xlab = xlab, ylab=ylab,xlim = xlim, ylim=ylim)
	graphics::lines(c(0,0),c(1.2*ylim[1]-0.2*ylim[2],-0.2*ylim[1]+1.2*ylim[2]), lwd=axis.lwd, col=axis.col)			# vertical axis line

	if(ce.point) graphics::points(de,dc,pch=pch,...)		# add a point at the center of ellipse 

  }

  # add ellipse 
  graphics::lines(de_ell,dc_ell,col=col,...)

  # save values in case needed later
  results <- list(ellipse=data.frame(de_ell,dc_ell),
	conf.level=conf.level, dc=dc, se_dc=se_dc, de=de, se_de=se_de, corr_dc_de=corr_dc_de)	#return values of summary statistics and points on ellipse
  if(use.person) results <-c(results,n=n,n0=n0,n1=n1)	#also return sample sizes if person-level data are provided

  invisible(results)
}





