#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  dataInput <- eventReactive(input$Run.model, {
    
    #observeEvent(input$Run.model, {
    
    #eventReactive(input$Run.model, {
    
    #input<-data.frame(startYear=1996, endYear=2018,min_h.levels=0, med_h.levels=15,max_h.levels=30 )
    
    d <- read.table("data-raw/Lynx_monitoring_data_2018.txt", header=T, sep="\t", dec=",")
    
    #check that year range is within data - model will run either way
    d$Aar<-as.numeric(d[,1])
    
    data<-subset(d, d$Aar <= input$endYear & d$Aar >= input$startYear)
    
    h.levels <- c(input$min_h.levels, input$mid_h.levels,input$max_h.levels)
    str(h.levels)
    
    ### SETTING UP THE DATA; sum for Norway models	
    
    FG <- matrix(NA, ncol=1, nrow=length(input$startYear:input$endYear))
    HV <- matrix(NA, ncol=1, nrow=length(input$startYear:input$endYear)-1)
    
    for (i in input$startYear:(input$endYear-1)){
      temp1 <- subset(data, data[1]==i)
      FG[i-input$startYear+1] <- sum(temp1[,"FG"])
      HV[i-input$startYear+1] <- sum(temp1[,"V.Hunner.belastet.kvoten"])
    }	
    
    # DEFINING THE "moment matching" PROCEDURE. ADOPTED FROM THE SWEDISH SCRIPT
    shape_from_stats <- function(mu = mu.global, sigma = sigma.global){
      a <-(mu^2-mu^3-mu*sigma^2)/sigma^2
      b <- (mu-2*mu^2+mu^3-sigma^2+mu*sigma^2)/sigma^2
      shape_ps <- c(a,b)
      return(shape_ps)
    }
    
    #get shape parameters for population multiplier, 1/p
    
    shapes=shape_from_stats(.44,.03)
    #check prior on p
    x = seq(0,1,.001)
    p=dbeta(x,shapes[1],shapes[2])
    #plot(x,p,typ="l",xlim=c(.1,.9))
    
    
    #Specify model 
    ########################################################################################
    #### HERE ENTERS THE BUGS-MODEL; 									####
    ########################################################################################
    
    sink("ssm_lynx1.bug")
    cat("
	model {
	# Priors and constraints
	N.est[1] ~ dnorm(69, 0.01)     # Prior for initial population size
	X.est[1] <- N.est[1]/beta
	


	mean.lambda ~ dnorm(1.2, 0.01)I(0,)     # Prior for mean growth rate
	
	sigma.proc ~ dunif(0, 5)              # Prior sd of state process
	sigma2.proc <- pow(sigma.proc, 2)
	tau.proc <- pow(sigma.proc, -2)
	
	sigma.obs ~ dunif(0, 100)             # Prior sd of observation process
	sigma2.obs <- pow(sigma.obs, 2)
	tau.obs <- pow(sigma.obs, -2)
	
	beta ~ dbeta(y.a, y.b) 			  # Prior for the b-parameter determining the linear effect of harvest
		
	

	# Process equations
   	for (t in 1:(T-1)){
   	lambda[t] ~ dnorm(mean.lambda, tau.proc)
   	X.est[t+1] <- max(1,(X.est[t]-hv[t]) * lambda[t])
	N.est[t+1] <- X.est[t+1]*beta
	}

	# Observation equations
	for (t in 1:T) {
   	y[t] ~ dnorm(N.est[t], tau.obs)
   	}
	

	# Prediction based on harvest in year T

	lam ~ dnorm(mean.lambda, tau.proc)

	for (i in 1:I){
		X.pred[i] <- max(1,(X.est[T]-h[i]) * lam)
		N.pred[i] <- X.pred[i]*beta
			}


	# Derived paramter

 	HR1 <- (1-(1/mean.lambda))
	HR2 <- HR1/beta
	HR3 <- HR2*N.est[T]
 	

	}
	",fill = TRUE)
    sink()
    
    ## Harvest in year t
    
    h <- h.levels
    
    # Bundle data
    bugs.data <- list(y.a=shapes[1], y.b=shapes[2], y = as.vector(FG), hv=as.vector(HV), T = length(input$startYear:input$endYear), h=as.vector(h), I=length(h))
    
    # Initial values
    inits <- function(){list(sigma.proc = runif(1, 0, 0.1), mean.lambda = 1.15, sigma.obs = runif(1, 5, 10), N.est = c(FG[1], rep(NA, (length(FG)-1))))}
    
    # Define parameters to be monitored
    parameters <- c("lambda", "mean.lambda", "sigma2.obs", "sigma2.proc", "N.est", "beta", "HR1", "HR2", "HR3", "X.est", "N.pred", "lam")
    
    
    # run model in JAGS
    out1<-R2jags::jags(data=bugs.data, inits=inits, parameters.to.save=parameters, 
               model.file="ssm_lynx1.bug",n.chains=2, n.iter=5000, 
               n.burnin=1000, n.thin=1)
  }) 
  
  #Estimated FG
  dataInput1<-reactive({
    
    # d <- read.table("RawData/Lynx_monitoring_data_2018.txt", header=T, sep="\t", dec=",")
    # 
    # #check that year range is within data - model will run either way
    # d$Aar<-as.numeric(d$Aar)
    # 
    # data<-subset(d, d$Aar <= input$endYear & d$Aar >= input$startYear)
    # 
    out3<-coda::as.mcmc(dataInput())
    # out3<-as.mcmc(out1)
    tidy_out<-tidybayes::tidy_draws(out3)
    EstN<-tidy_out %>% 
      select(starts_with("N.est")) %>% 
      gather() %>% 
      mutate(tmp=gsub("N.est", "", key)) %>% 
      mutate(tmp=gsub("\\[|\\]", "", tmp)) %>% 
      mutate(year=as.integer(tmp)) %>% 
      select(! tmp) %>% 
      mutate(year=year+input$startYear-1) %>% 
      #input$startYear-1) %>% 
      group_by(year) %>%
      summarise(smean = mean(value, na.rm = TRUE),
                ssd = sd(value, na.rm = TRUE),
                count = n())
    
  })
  dataInput2<-reactive({
    out2<-coda::as.mcmc(dataInput())
    #out2<-as.mcmc(out1)
    tidy_out<-tidybayes::tidy_draws(out2)
    
    TotalN<-tidy_out %>% 
      select(starts_with("x.est")) %>% 
      gather() %>% 
      mutate(tmp=gsub("X.est", "", key)) %>% 
      mutate(tmp=gsub("\\[|\\]", "", tmp)) %>% 
      mutate(year=as.integer(tmp)) %>% 
      select(! tmp) %>% 
      mutate(year=year+input$startYear-1) %>% 
      #input$startYear-1) %>% 
      group_by(year) %>%
      summarise(smean = mean(value, na.rm = TRUE),
                ssd = sd(value, na.rm = TRUE),
                count = n())
  })
  
  dataInput3<-reactive({
    fitted <- lower50 <- lower75 <- upper50 <- upper75 <- numeric()
    h.levels <- c(input$min_h.levels, input$mid_h.levels,input$max_h.levels)
    h<-h.levels
    for (i in 1:length(h)){
      fitted[i] <- round(quantile(dataInput()$BUGSout$sims.list$N.pred[,i], 0.5))
      lower50[i] <- quantile(dataInput()$BUGSout$sims.list$N.pred[,i], 0.25)
      lower75[i] <- quantile(dataInput()$BUGSout$sims.list$N.pred[,i], 0.125)
      upper50[i] <- quantile(dataInput()$BUGSout$sims.list$N.pred[,i], 0.75)
      upper75[i] <- quantile(dataInput()$BUGSout$sims.list$N.pred[,i], 0.875)}
    
    dat<-data.frame(h,fitted, lower50, lower75, upper50, upper75)
    
    
  })
  
  output$table2 <- renderTable({
    
    #need to format year
    
    dataInput1()
  })  
  
  output$table3 <- renderTable({
    
    #need to format year
    
    dataInput2() 
  })
  
  
  #Ugly table Needs improvement
  #output$table4 <- DT::renderDataTable({
  #  
  #  x<-data.frame(out1$BUGSoutput$summary)
  #  datatable(x,rownames=TRUE)%>%
  #    formatRound(c(1:8), digits=3)
  #})  
  
  #Need to tidy and add True FG
  output$plot1<-renderPlot({
    dataInput1()%>%
      ggplot(aes(year, smean))+
      geom_pointrange(aes(y=smean, ymax=smean+ssd, ymin=smean-ssd))
  })
  #need to tidy this plot
  output$plot2<-renderPlot({
    dataInput2() %>% 
      ggplot(aes(year, smean))+
      geom_pointrange(aes(y=smean, ymax=smean+ssd, ymin=smean-ssd))
  })
  
  output$plot3<-renderPlot({
    dataInput3() %>% 
      ggplot(aes(h))+
      geom_pointrange(aes(y=fitted, ymin=lower75, ymax=upper75))+
      geom_hline(yintercept=66, linetype=2)
  })
  
}
