#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  National_licenses<-get_full_quotas()
  
  # List the first level callModules here
  dataInput <- eventReactive(input$Run.model, {
    
    #observeEvent(input$Run.model, {
    
    #eventReactive(input$Run.model, {
    
    #input<-data.frame(startYear=1996, endYear=2018,min_h.levels=0, med_h.levels=15,max_h.levels=30 )
    
    d <- readRDS("C:/Users/matthew.grainger/Documents/Projects_in_development/HarvestGolem/data-raw/Lynx_monitoring_data_2018.RDS")
    
    d<-d%>% 
      filter(Region==input$model)
    
    
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
    
    n.iter=as.numeric(input$n_its)
    n.chains=as.numeric(input$n_chains)
    n.burnin=as.numeric(input$burn_in)
    n.thin=as.numeric(input$n_thin)
    
    # run model in JAGS
    out1<-R2jags::jags(data=bugs.data, inits=inits, parameters.to.save=parameters, 
               model.file="ssm_lynx1.bug",n.chains=n.chains, n.iter=n.iter, 
               n.burnin=n.burnin, n.thin=n.thin)
  }) 
  
  # n.years<-length(input$startYear:input$endYear)
  # P_LessThanTarget <- ecdf(out1$BUGSout$sims.list$N.est[,n.years])(65)
  # print(P_LessThanTarget)
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
    #print(EstN)
    
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
    harvest_level<-h.levels
    for (i in 1:length(harvest_level)){
      fitted[i] <- round(quantile(dataInput()$BUGSout$sims.list$N.pred[,i], 0.5))
      lower50[i] <- quantile(dataInput()$BUGSout$sims.list$N.pred[,i], 0.25)
      lower75[i] <- quantile(dataInput()$BUGSout$sims.list$N.pred[,i], 0.125)
      upper50[i] <- quantile(dataInput()$BUGSout$sims.list$N.pred[,i], 0.75)
      upper75[i] <- quantile(dataInput()$BUGSout$sims.list$N.pred[,i], 0.875)}
    
    dat<-data.frame(harvest_level,fitted, lower50, lower75, upper50, upper75)
    
    
  })
  
  output$plotQ<-renderPlot({
    # The palette with black:
    cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    
   National_licenses %>% 
     ggplot(aes(Year,value)) +
    geom_bar(stat="identity",fill=cbPalette[3])+
    labs(y="Number of licenses issued")+
    ggpubr::theme_pubr()+
     ggtitle("The number of licenses issued for Lynx Harvest (SSB data)")
    #theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  output$plotR<-renderPlot({
    Lynx_reproduction <- readRDS("C:/Users/matthew.grainger/Documents/Projects_in_development/HarvestGolem/data-raw/Lynx_reproduction.RDS")
        facet_labs<-list("1"="Male","2"= "Female")
    facet_labeller <- function(variable,value){
      return(facet_labs[value])
    }
    
    Lynx_reproduction %>% 
      mutate(FellingsMnth=lubridate::month(Fellingsdato)) %>% 
      filter(FellingsMnth==02 | FellingsMnth==03) %>% 
      select(Aar, Kjonn, Alder) %>%
      mutate(Age_cat=ifelse(Alder<1,1,2)) %>% 
      filter(Aar>2007&Aar<as.numeric(format(Sys.Date(),"%Y"))-1) %>% 
      drop_na(Age_cat) %>% 
      mutate(Age_sex=ifelse(Age_cat==1 & Kjonn==1, "Male_Kitten", ifelse(Age_cat==1 & Kjonn==2, "Female_Kitten", ifelse(Age_cat==2 & Kjonn==1, "Male_Adult", "Female_Adult")) )) %>% 
      group_by(Aar, Age_sex) %>% 
      summarise(n=n()) %>% 
      mutate(freq = n / sum(n)) %>% 
      mutate(Year=as.integer(Aar)) %>% 
      ggplot(aes(Year,freq, fill=as.factor(Age_sex)))+
      geom_bar(stat="identity",position = "stack")+
      scale_x_continuous(breaks=c(2007,2008,2009,2010,2011,2012,2013,2014,2015,
                                  2016,2017,2018, 2019,2020, 2021,2022, 2023,2024,2025))+
      labs(y= "Proportion of harvested") + 
      ggpubr::theme_pubr()+
      theme(legend.title = element_blank())+
      ggtitle("Harvested Lynx by Sex & Age")
    
    
  })

  
  output$table2 <- renderTable({
    
    #need to format year
    
    dataInput1()
  })  
  
  # output$downloadData <- downloadHandler(
  #   filename = function(){"table2.csv"}, 
  #   content = function(fname){
  #     write.csv(table2(), fname)
  #   }
  # )
  
  output$table3 <- renderTable({
    
    #need to format year
    
    dataInput2() 
  })
  
  output$table<-renderTable({
    dataInput3()
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
  
   # output$downloadBtn <- downloadHandler(
   #   filename = function(){
   #     paste("lynx",input$fileType,sep = ".")
   #   },
   #   content = function(file){
   #     if(input$fileType=="png")
   #       png(file)
   #     else
   #       pdf(file)
   #     print(plot(x(),y()))
   #     dev.off()
   #  }
   # )
  #need to tidy this plot
  output$plot2<-renderPlot({
    dataInput2() %>% 
      ggplot(aes(year, smean))+
      geom_pointrange(aes(y=smean, ymax=smean+ssd, ymin=smean-ssd))
  })
  
  Region=c(1,2,3,4,5,6,7,8)
  RegTar=c(0,12,5,6,10,12,10,10)
  RegTars=data.frame(Region,RegTar)
  
  
  output$plot3<-renderPlot({
    RegTars<-RegTars %>% 
      filter(Region==input$model)
    dataInput3() %>% 
      ggplot(aes(harvest_level))+
      geom_pointrange(aes(y=fitted, ymin=lower75, ymax=upper75))+
      geom_hline(yintercept=sum(RegTars$RegTar), linetype=2)
  })
  
  output$plotMap<-renderLeaflet({
    map=build_map()
    map
  })
  ##############################################################################################################
  Ldata2<-reactive({
    dattable2 %>%
      group_by(region,`interval (year)`) %>% 
      filter(region==input$regionL) })
  
  # output$picture<-renderGrViz({
  #   
  #   Net<-HydeNetwork(~ M01S|M01
  #                    + M02S|M02
  #                    + M03S|M03
  #                    + M04S|M04
  #                    + F01S|F01
  #                    + F02S|F02
  #                    + F03S|F03
  #                    + F04S|F04
  #                    + M11|M01S
  #                    + M12|M02S
  #                    + M13|M03S
  #                    + M14|M04S
  #                    + F11|F01S
  #                    + F12|F02S
  #                    + F13|F03S
  #                    + F14|F04S
  #                    + RF2|F12
  #                    + RF3|F13
  #                    + RF4|F14
  #                    + R|RF2*RF3*RF4
  #                    + HRF1|R*Hv
  #                    + HRF2|F11*Hv
  #                    + HRF3|F12*Hv
  #                    + HRF4|F13*F14*Hv
  #                    + HRM1|R*Hv
  #                    + HRM2|M11*Hv
  #                    + HRM3|M12*Hv
  #                    + HRM4|M13*M14*Hv
  #                    + N1|HRM1*HRM2*HRM3*HRM4*HRF1*HRF2*HRF3*HRF4
  #                    + FG|F12*F13*F14
  #                    +F21|HRF1
  #                    +F22|HRF2
  #                    +F23|HRF3
  #                    +F24|HRF4
  #                    +M21|HRM1
  #                    +M22|HRM2
  #                    +M23|HRM3
  #                    +M24|HRM4
  #                    +F21S|F21
  #                    +F22S|F22
  #                    +F23S|F23
  #                    +F24S|F24
  #                    +M21S|M21
  #                    +M22S|M22
  #                    +M23S|M23
  #                    +M24S|M24
  #                    +F31|F21S
  #                    +F32|F22S
  #                    +F33|F23S
  #                    +F34|F24S
  #                    +M31|M21S
  #                    +M32|M22S
  #                    +M33|M23S
  #                    +M34|M24S
  #                    + FG1|F32*F33*F34
  #   )
  #   
  #   Net<-setDecisionNodes(Net, Hv)
  #   plot(Net)
  # })
  
  policies<-reactive({data.frame(Hv = as.numeric(input$quota1))})
  FG0<-reactive({input$fg})
  output$tablex<-renderTable(policies())
  
  
  samples<-eventReactive(input$go, {
    policies<-data.frame(Hv = policies())
    #  print(policies)
    FG0<-as.numeric(FG0())
    #print(FG0)
    Net<-HydeNetwork(~ M01S|M01
                     + M02S|M02
                     + M03S|M03
                     + M04S|M04
                     + F01S|F01
                     + F02S|F02
                     + F03S|F03
                     + F04S|F04
                     + M11|M01S
                     + M12|M02S
                     + M13|M03S
                     + M14|M04S
                     + F11|F01S
                     + F12|F02S
                     + F13|F03S
                     + F14|F04S
                     + RF2|F12
                     + RF3|F13
                     + RF4|F14
                     + R|RF2*RF3*RF4
                     + HRF1|R*Hv
                     + HRF2|F11*Hv
                     + HRF3|F12*Hv
                     + HRF4|F13*F14*Hv
                     + HRM1|R*Hv
                     + HRM2|M11*Hv
                     + HRM3|M12*Hv
                     + HRM4|M13*M14*Hv
                     + N1|HRM1*HRM2*HRM3*HRM4*HRF1*HRF2*HRF3*HRF4
                     + FG|F12*F13*F14
                     +F21|HRF1
                     +F22|HRF2
                     +F23|HRF3
                     +F24|HRF4
                     +M21|HRM1
                     +M22|HRM2
                     +M23|HRM3
                     +M24|HRM4
                     +F21S|F21
                     +F22S|F22
                     +F23S|F23
                     +F24S|F24
                     +M21S|M21
                     +M22S|M22
                     +M23S|M23
                     +M24S|M24
                     +F31|F21S
                     +F32|F22S
                     +F33|F23S
                     +F34|F24S
                     +M31|M21S
                     +M32|M22S
                     +M33|M23S
                     +M34|M24S
                     + FG1|F32*F33*F34
    )
    
    
    ### Using deterministic nodes for initial population - could be clever by working out what spread of population would be with known FG
    # Net<-setNode(Net, M01, nodeType="determ",
    #              define=fromFormula(), nodeFormula = M01 ~83)
    # 
    # Net<-setNode(Net, M02, nodeType="determ",
    #              define=fromFormula(), nodeFormula = M02 ~52)
    # Net<-setNode(Net, M03, nodeType="determ",
    #              define=fromFormula(), nodeFormula = M03 ~31)
    # Net<-setNode(Net, M04, nodeType="determ",
    #              define=fromFormula(), nodeFormula = M04 ~23)
    # 
    # Net<-setNode(Net, F01, nodeType="determ",
    #              define=fromFormula(), nodeFormula = F01 ~81)
    # 
    # Net<-setNode(Net, F02, nodeType="determ",
    #              define=fromFormula(), nodeFormula = F02 ~47)
    # Net<-setNode(Net, F03, nodeType="determ",
    #              define=fromFormula(), nodeFormula = F03 ~41)
    # Net<-setNode(Net, F04, nodeType="determ",
    #              define=fromFormula(), nodeFormula = F04 ~31)
    # Net <- setNode(Net, M01,
    #                nodeType = "dpois", 
    #                lambda = 80)
    # Net <- setNode(Net, M02,
    #                nodeType = "dpois", 
    #                lambda = 40)
    # Net <- setNode(Net, M03,
    #                nodeType = "dpois", 
    #                lambda = 30)
    # Net <- setNode(Net, M04,
    #                nodeType = "dpois", 
    #                lambda =  28)
    # 
    # 
    # 
    # Net <- setNode(Net, F01,
    #                nodeType = "dpois", 
    #                lambda =  80)
    # 
    # 
    # Net <- setNode(Net, F02,
    #                nodeType = "dpois", 
    #                lambda = 40)
    # Net <- setNode(Net, F03,
    #                nodeType = "dpois", 
    #                lambda = 30)
    # Net <- setNode(Net, F04,
    #                nodeType = "dpois", 
    #                lambda =  28)
    
    
    #FG0=55
    
    a1=0.42*((FG0*6.09+-12.56)/2)
    b1= a1+5
    a2=0.12*((FG0*6.09+-12.56)/2)
    b2=a2+5
    a3=0.13*((FG0*6.09+-12.56)/2)
    b3=a3+5
    a4=0.48*((FG0*6.09+-12.56)/2)
    b4=a4+5
    
    a5=0.48*((FG0*6.09+-12.56)/2)
    b5= a5+5
    a6=0.20*((FG0*6.09+-12.56)/2)
    b6=a6+5
    a7=0.13*((FG0*6.09+-12.56)/2)
    b7=a7+5
    a8=0.16*((FG0*6.09+-12.56)/2)
    b8=a8+5
    
    Net<-setNode(Net, F01, nodeType="dunif",a=a1,b=b1)
    Net<-setNode(Net, F02, nodeType="dunif",a=a2,b=b2)
    Net<-setNode(Net, F03, nodeType="dunif",a=a3,b=b3)
    Net<-setNode(Net, F04, nodeType="dunif",a=a4,b=b4)
    
    Net<-setNode(Net, M01, nodeType="dunif",a=a5,b=b5)
    Net<-setNode(Net, M02, nodeType="dunif",a=a6,b=b6)
    Net<-setNode(Net, M03, nodeType="dunif",a=a7,b=b7)
    Net<-setNode(Net, M04, nodeType="dunif",a=a8,b=b8)
    
    
    
    
    
    
    
    
    
    
    # Net <- setNode(Net, F01,
    #                nodeType = "dpois", 
    #                lambda =  80)
    # Net <- setNode(Net, F02,
    #                nodeType = "dpois", 
    #                lambda =  40)
    # Net <- setNode(Net, F03,
    #                nodeType = "dpois", 
    #                lambda = 30)
    # Net <- setNode(Net, F04,
    #                nodeType = "dpois", 
    #                lambda = 28)
    
    Net <- setNode(Net, M01S,
                   nodeType = "dnorm", 
                   mean = .795, sd = 0.107)
    Net <- setNode(Net, M02S,
                   nodeType = "dnorm", 
                   mean = .667, sd = 0.192)
    Net <- setNode(Net, M03S,
                   nodeType = "dnorm", 
                   mean = .741, sd = 0.129)
    Net <- setNode(Net, M04S,
                   nodeType = "dnorm", 
                   mean = .8, sd = 0.1)
    Net <- setNode(Net, F01S,
                   nodeType = "dnorm", 
                   mean = .56, sd = 0.149)
    Net <- setNode(Net, F02S,
                   nodeType = "dnorm", 
                   mean = .75, sd = 0.217)
    Net <- setNode(Net, F03S,
                   nodeType = "dnorm", 
                   mean = .84, sd = 0.1)
    Net <- setNode(Net, F04S,
                   nodeType = "dnorm", 
                   mean = .8, sd = 0.1)
    
    # nodeType = "determ",
    # define = fromFormula(),
    # nodeFormula = diceSum ~ di1 + di2)
    
    Net <- setNode(Net, M11, nodeType="determ",
                   define=fromFormula(), nodeFormula = M11 ~ M01*M01S)
    Net <- setNode(Net, M12, nodeType="determ",
                   define=fromFormula(), nodeFormula = M12 ~ M02*M02S)
    Net <- setNode(Net, M13, nodeType="determ",
                   define=fromFormula(), nodeFormula = M13 ~ M03*M03S)
    
    Net <- setNode(Net, M14, nodeType="determ",
                   define=fromFormula(), nodeFormula = M14 ~ M04*M04S)
    
    
    Net <- setNode(Net, F11, nodeType="determ",
                   define=fromFormula(), nodeFormula = F11 ~ F01*F01S)
    Net <- setNode(Net, F12, nodeType="determ",
                   define=fromFormula(), nodeFormula = F12 ~ F02*F02S)
    Net <- setNode(Net, F13, nodeType="determ",
                   define=fromFormula(), nodeFormula = F13 ~ F03*F03S)
    Net <- setNode(Net, F14, nodeType="determ",
                   define=fromFormula(), nodeFormula = F14 ~ F04*F04S)
    
    Net <- setNode(Net, N1, nodeType="determ",
                   define=fromFormula(), nodeFormula = 
                     N1 ~ HRF1 + HRF2 + HRF3+ HRF4 + HRM1 + HRM2 + HRM3 + HRM4)
    #######################################################################
    #######################Reproduction####################################
    Net <- setNode(Net, RF2, nodeType="determ",
                   define=fromFormula(), nodeFormula = RF2 ~ F12*0.65)
    Net <- setNode(Net, RF3, nodeType="determ",
                   define=fromFormula(), nodeFormula =  RF3 ~F13*1.65)
    Net <- setNode(Net, RF4, nodeType="determ",
                   define=fromFormula(), nodeFormula =  RF4 ~F14*1.34)
    Net <- setNode(Net, R, nodeType="determ",
                   define=fromFormula(), nodeFormula = R ~RF3+RF2+RF4)
    
    #######################################################################
    ######################################################################
    #### UPDATE WITH BETTER VALUES FOR HARVEST % CHECK MONTH FOR KITTENS
    #####
    
    Net <- setNode(Net, HRM1, nodeType="determ",
                   define=fromFormula(), nodeFormula = HRM1 ~R*0.5-(Hv*0.54))
    Net <- setNode(Net, HRM2, nodeType="determ",
                   define=fromFormula(), nodeFormula =  HRM2 ~M11-Hv*(0.19))
    Net <- setNode(Net, HRM3, nodeType="determ",
                   define=fromFormula(), nodeFormula =  HRM3 ~M12-Hv*(0.11))
    Net <- setNode(Net, HRM4, nodeType="determ",
                   define=fromFormula(), nodeFormula =  HRM4 ~M13+M14-(Hv*0.15))
    Net <- setNode(Net, HRF1, nodeType="determ",
                   define=fromFormula(), nodeFormula = HRF1~ R*0.5-Hv*(0.45))
    Net <- setNode(Net, HRF2, nodeType="determ",
                   define=fromFormula(), nodeFormula = HRF2 ~F11-Hv*(0.14))
    Net <- setNode(Net, HRF3, nodeType="determ",
                   define=fromFormula(), nodeFormula =  HRF3 ~F12-Hv*(0.11))
    Net <- setNode(Net, HRF4, nodeType="determ",
                   define=fromFormula(), nodeFormula =  HRF4 ~F13+F14-Hv*(0.28))
    Net <- setNode(Net,Hv,
                   nodeType = "dnorm", mean=50, sd=0.0001)
    
    
    
    # 
    # 
    # Net <- setNode(Net, HRM1, nodeType="determ",
    #                define=fromFormula(), nodeFormula = HRM1 ~R*0.5-(Hv*0.54))
    # Net <- setNode(Net, HRM2, nodeType="determ",
    #                define=fromFormula(), nodeFormula =  HRM2 ~M11-Hv*(0.19))
    # Net <- setNode(Net, HRM3, nodeType="determ",
    #                define=fromFormula(), nodeFormula =  HRM3 ~M12-Hv*(0.11))
    # Net <- setNode(Net, HRM4, nodeType="determ",
    #                define=fromFormula(), nodeFormula =  HRM4 ~M13+M14-(Hv*0.15))
    # Net <- setNode(Net, HRF1, nodeType="determ",
    #                define=fromFormula(), nodeFormula = HRF1~ R*0.5-Hv*(0.45))
    # Net <- setNode(Net, HRF2, nodeType="determ",
    #                define=fromFormula(), nodeFormula = HRF2 ~F11-Hv*(0.14))
    # Net <- setNode(Net, HRF3, nodeType="determ",
    #                define=fromFormula(), nodeFormula =  HRF3 ~F12-Hv*(0.11))
    # Net <- setNode(Net, HRF4, nodeType="determ",
    #                define=fromFormula(), nodeFormula =  HRF4 ~F13+F14-Hv*(0.28))
    # Net <- setNode(Net,Hv,
    #                nodeType = "dnorm", mean=50, sd=0.0001)
    # 
    # 
    #females probability of having at least one kitten
    Net <- setNode(Net, FG, nodeType="determ",
                   define=fromFormula(), nodeFormula =  FG ~ F12*0.20+F13*0.99+F14*0.91)
    
    Net <- setNode(Net, FG, nodeType="determ",
                   define=fromFormula(), nodeFormula = FG ~ F12*0.20+F13*0.99+F14*0.91)
    
    #Net<-setNode(Net,FG,nodeType = "dnorm", mean=66, sd=5)
    
    #################################################################################################
    ## Probability that female has >1 Kittens at heel
    #################################################################################################
    
    # plot(density(rnorm(100,0.65,0.42)))
    # plot(density(rnorm(100,1.65,0.158)))
    # plot(density(rnorm(100,1.34,0.251)))
    # 
    # pnorm(1, mean=0.65, sd=0.42, lower.tail=FALSE) 
    # pnorm(1, mean=1.65, sd=0.158, lower.tail=FALSE) 
    # pnorm(1, mean=1.34, sd=0.251, lower.tail=FALSE) 
    #
    #################################################################################################
    
    
    Net<-setNode(Net,F21, nodeType = "determ",
                 define=fromFormula(), nodeFormula = F21~HRF1)
    Net<-setNode(Net,F22, nodeType = "determ",
                 define=fromFormula(), nodeFormula =  F22~HRF2)
    Net<-setNode(Net,F23, nodeType = "determ",
                 define=fromFormula(), nodeFormula =  F23~HRF3)
    Net<-setNode(Net,F24, nodeType = "determ",
                 define=fromFormula(), nodeFormula =  F24~HRF4)
    Net<-setNode(Net,M21, nodeType = "determ",
                 define=fromFormula(), nodeFormula =  M21~HRM1)
    Net<-setNode(Net,M22, nodeType = "determ",
                 define=fromFormula(), nodeFormula =  M22~HRM2)
    Net<-setNode(Net,M23, nodeType = "determ",
                 define=fromFormula(), nodeFormula = M23~HRM3)
    Net<-setNode(Net,M24, nodeType = "determ",
                 define=fromFormula(), nodeFormula = M24~HRM4)
    
    Net <- setNode(Net, M21S,
                   nodeType = "dnorm", 
                   mean = .795, sd = 0.107)
    Net <- setNode(Net, M22S,
                   nodeType = "dnorm", 
                   mean = .667, sd = 0.192)
    Net <- setNode(Net, M23S,
                   nodeType = "dnorm", 
                   mean = .741, sd = 0.129)
    Net <- setNode(Net, M24S,
                   nodeType = "dnorm", 
                   mean = .8, sd = 0.1)
    Net <- setNode(Net, F21S,
                   nodeType = "dnorm", 
                   mean = .56, sd = 0.149)
    Net <- setNode(Net, F22S,
                   nodeType = "dnorm", 
                   mean = .75, sd = 0.217)
    Net <- setNode(Net, F23S,
                   nodeType = "dnorm", 
                   mean = .84, sd = 0.1)
    Net <- setNode(Net, F24S,
                   nodeType = "dnorm", 
                   mean = .8, sd = 0.1)
    
    Net <- setNode(Net, M31, nodeType = "determ",
                   define=fromFormula(), nodeFormula =  M31 ~ M21*M21S)
    Net <- setNode(Net, M32, nodeType = "determ",
                   define=fromFormula(), nodeFormula = M32 ~ M22*M22S)
    Net <- setNode(Net, M33, nodeType = "determ",
                   define=fromFormula(), nodeFormula = M33 ~ M23*M23S)
    Net <- setNode(Net, M34, nodeType = "determ",
                   define=fromFormula(), nodeFormula = M34 ~ M24*M24S)
    Net <- setNode(Net, F31, nodeType = "determ",
                   define=fromFormula(), nodeFormula = F31 ~ F21*F21S)
    Net <- setNode(Net, F32, nodeType = "determ",
                   define=fromFormula(), nodeFormula =  F32 ~ F22*F22S)
    Net <- setNode(Net, F33, nodeType = "determ",
                   define=fromFormula(), nodeFormula = F33 ~ F23*F23S)
    Net <- setNode(Net, F34, nodeType = "determ",
                   define=fromFormula(), nodeFormula = F34 ~ F24*F24S)
    
    # Net <- setNode(Net, F34, nodeType="dpois",
    #                lambda=fromFormula(), #sigma^2 = 30
    #                nodeFormula = F34 ~ F24*F24S)
    
    Net <- setNode(Net, FG1, nodeType="determ",
                   mean=fromFormula(), 
                   nodeFormula = FG1 ~ F32*0.20+F33*0.99+F34*0.91)
    
    
    
    #writeNetworkModel(Net, pretty = TRUE)
    #Net1 <- compileJagsModel(Net, n.chains=3)
    #writeNetworkModel(Net, pretty = TRUE)
    trackedVars <- c("FG1")
    compiledNets <- compileDecisionModel(Net, policyMatrix = policies, n.chains=5)  
    
    samples <- lapply(compiledNets,
                      HydeSim,
                      variable.names = trackedVars,
                      n.iter=120000, trace=F)
    #lapply(samples, function(l) mean(as.numeric(l$FG1)))
    
  })
  
  
  output$plot11<-renderPlot({
    df_list<-bind_rows(samples(), .id = "id")
    df_list<-as.data.frame(df_list)
    lower_ci <- function(mean, se, n, conf_level = 0.95){
      lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
    }
    upper_ci <- function(mean, se, n, conf_level = 0.95){
      upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
    }
    
    df_list%>%
      group_by(Hv) %>%
      summarise(smean = mean(FG1, na.rm = TRUE),
                ssd = sd(FG1, na.rm = TRUE),
                count = n()) %>%
      mutate(se = ssd / sqrt(count),
             lower_ci = lower_ci(smean, se, count),
             upper_ci = upper_ci(smean, se, count))%>% 
      ggplot(., aes(as.numeric(Hv), smean))+
      geom_pointrange(aes(x=as.numeric(Hv),y=smean, ymin=lower_ci, ymax=upper_ci), colour="white")+
      scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150)) +
      labs(x="Number of harvested lynx", y= "mean Family Group estimate and 95% CI")+
      geom_hline(yintercept=66,size=1,linetype=4,color="white")+
      theme_dark()
    # print(df_list%>%
    #         group_by(Hv) %>%
    #         summarise(smean = mean(FG1, na.rm = TRUE),
    #                   ssd = sd(FG1, na.rm = TRUE),
    #                   count = n()) %>%
    #         mutate(se = ssd / sqrt(count),
    #                lower_ci = lower_ci(smean, se, count),
    #                upper_ci = upper_ci(smean, se, count)))
  })
  
  points <- reactive({
    plotdat %>% 
      filter(Aar==as.numeric(input$year))
  })
  
  #****************************************
  #* Reactive Values
  
  model<-reactive({
    RegTars<-RegTars %>% 
      filter(Region==input$model)
    RegTars$Region
  })
  
  table <- reactive({
    dataInput3() %>% kableExtra::kable(., caption = "The projected Family Group estimate for different levels of female-only harvest")
  })
  
  plot <- reactive({
    RegTars<-RegTars %>% 
      filter(Region==input$model)
    dataInput3() %>% 
      ggplot(aes(harvest_level))+
      geom_pointrange(aes(y=fitted, ymin=lower75, ymax=upper75))+
      geom_hline(yintercept=sum(RegTars$RegTar), linetype=2)
  })
  
  #****************************************
  #****************************************
  #* Download Handlers
  
  output$downloader <- 
    downloadHandler(
      "results_from_shiny.pdf",
      content = 
        function(file)
        {
          rmarkdown::render(
            input = "report_file.Rmd",
            output_file = "built_report.pdf",
            params = list(table=table(),
              plot = plot(),
              model=model())
          ) 
          readBin(con = "built_report.pdf", 
                  what = "raw",
                  n = file.info("built_report.pdf")[, "size"]) %>%
            writeBin(con = file)
        }
    )

}
