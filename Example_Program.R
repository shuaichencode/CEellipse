###### Example program to illustrate how to draw a confidence ellipse for cost-effective data ###
###### tested under R version 4.0.4

############### preparation ###########

# set the path to the folder with CEellipse.R (use "/" instead of "\" in path)
setwd("Your Path")

# load function for drawing a confidence ellipse
source("CEellipse.R")	

############### use person-level cost-effectiveness data to draw a confidence ellipse ###########

## use a toy person-level data as an example
exampledata=data.frame(id=1:6,
	tx=c(0,0,0,1,1,1),
	cost=c(50,90,40,200,400,600),
	effect=c(4,5,1,5,2,4)
)
exampledata	

##  draw a 95% (default level) confidence ellipse
CEellipse(cost=exampledata$cost,		# cost for each individual
	effect=exampledata$effect,		# effectiveness for each individual
	treatment=exampledata$tx		# treatment indicator for each individual (1=considered treatment, 0=comparison)
)

##  draw an 80% confidence ellipse using conf.level option, and customize label, limit of axis, and line type of ellipse, etc. 
CEellipse(cost=exampledata$cost, effect=exampledata$effect, treatment=exampledata$tx, conf.level=0.8,
	xlim=c(-4,5), ylim=c(-200,700),					# new limit of x-axis and y-axis
	xlab="Saved life of years", ylab="Cost difference ($)",	# new labels
	lty=2, lwd=2, col="blue",						# new line type, line width, and color for the ellipse
	axis.lwd=2, axis.col="gray",						# new width, and color for the axis
	pch=17									# new shape of point 
)

##  add an additional 95% confidence ellipse to the above plot using add option (instead of creating a new figure with the ellipse)
##  also save values to be used later, such as summary statistics and points on ellipse used in drawing
result <- CEellipse(cost=exampledata$cost, effect=exampledata$effect, treatment=exampledata$tx, 
	add=TRUE)	

# check the saved values
result
# add a line for ICER using the saved summary statistics
lines(c(0,100),c(0,100*result$dc/result$de))	


############### use summary-level cost-effectiveness data to draw a confidence ellipse ###########

# use the summary statistics from Nixon et al. (2010) to draw a 95% confidence ellipse
CEellipse(dc=186,			# mean cost difference between considered treatment and comparison
	se_dc=sqrt(2920),		# standard error of the mean cost difference
	de=0.018,			# mean effect difference between considered treatment and comparison
	se_de=sqrt(0.00021),	# standard error of the mean effect difference
	corr_dc_de=-0.222		# correlation between the mean cost difference and mean effect difference
)
