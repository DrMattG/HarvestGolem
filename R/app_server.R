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
    
   
    
    d <-HarvestGolem::Lynx_monitoring_data
      #readRDS("data-raw/Lynx_monitoring_data.RDS")
    
    validate(
      need(!is.null(input$model), "Velg data")
    )

    d<-d%>% 
      filter(Region==input$model)
    
    
    data<-subset(d, d$Aar <= input$endYear & d$Aar >= input$startYear)
    data<-as.data.frame(data)
    
    h.levels <- c(input$min_h.levels, input$mid_h.levels,input$max_h.levels)
    #str(h.levels)
    
    ### SETTING UP THE DATA; sum for Norway models	
    
    FG <- matrix(NA, ncol=1, nrow=length(input$startYear:input$endYear))
    HV <- matrix(NA, ncol=1, nrow=length(input$startYear:input$endYear)-1)
    
    for (i in input$startYear:(input$endYear-1)){
      temp1 <- subset(data, data[1]==i)
      FG[i-input$startYear+1] <- sum(temp1[,"FG"])
      HV[i-input$startYear+1] <- sum(temp1[,"V.Hunner.belastet.kvoten"])
    }	
    #print(FG)
    #print(HV)
    #print(input$startYear)
    #print(input$endYear)
    
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
    
    
    
    #Specify model 
    ########################################################################################
    #### HERE ENTERS THE BUGS-MODEL; 									####
    ########################################################################################
    withProgress(message = 'running model', value = 0, {  
      incProgress(50);
#     sink("ssm_lynx1.bug")
#     cat("
# 	model {
# 	# Priors and constraints
# 	N.est[1] ~ dnorm(69, 0.01)     # Prior for initial population size
# 	X.est[1] <- N.est[1]/beta
# 	
# 
# 
# 	mean.lambda ~ dnorm(1.2, 0.01)I(0,)     # Prior for mean growth rate
# 	
# 	sigma.proc ~ dunif(0, 5)              # Prior sd of state process
# 	sigma2.proc <- pow(sigma.proc, 2)
# 	tau.proc <- pow(sigma.proc, -2)
# 	
# 	sigma.obs ~ dunif(0, 100)             # Prior sd of observation process
# 	sigma2.obs <- pow(sigma.obs, 2)
# 	tau.obs <- pow(sigma.obs, -2)
# 	
# 	beta ~ dbeta(y.a, y.b) 			  # Prior for the b-parameter determining the linear effect of harvest
# 		
# 	
# 
# 	# Process equations
#    	for (t in 1:(T-1)){
#    	lambda[t] ~ dnorm(mean.lambda, tau.proc)
#    	X.est[t+1] <- max(1,(X.est[t]-hv[t]) * lambda[t])
# 	N.est[t+1] <- X.est[t+1]*beta
# 	}
# 
# 	# Observation equations
# 	for (t in 1:T) {
#    	y[t] ~ dnorm(N.est[t], tau.obs)
#    	}
# 	
# 
# 	# Prediction based on harvest in year T
# 
# 	lam ~ dnorm(mean.lambda, tau.proc)
# 
# 	for (i in 1:I){
# 		X.pred[i] <- max(1,(X.est[T]-h[i]) * lam)
# 		N.pred[i] <- X.pred[i]*beta
# 			}
# 
# 
# 	# Derived paramter
# 
#  	HR1 <- (1-(1/mean.lambda))
# 	HR2 <- HR1/beta
# 	HR3 <- HR2*N.est[T]
#  	
# 
# 	}
# 	",fill = TRUE)
#     sink()
    
    ## Harvest in year t
    
    h <- h.levels
    
    # Bundle data
    bugs.data <- list(y.a=shapes[1], y.b=shapes[2], y = as.vector(FG), hv=as.vector(HV), T = length(input$startYear:input$endYear), h=as.vector(h), I=length(h))
    
    # Initial values
    inits <- function(){list(sigma.proc = runif(1, 0, 0.1), mean.lambda = 1.15, sigma.obs = runif(1, 5, 10), N.est = c(FG[1], rep(NA, (length(input$startYear:input$endYear)-1))))}
    
    # Define parameters to be monitored
    parameters <- c("lambda", "mean.lambda", "sigma2.obs", "sigma2.proc", "N.est", "beta", "HR1", "HR2", "HR3", "X.est", "N.pred", "lam")
    
    if(input$speed=="Rask"){
      
      # numericInput("n_its", 
      #             label = "Antall iterasjoner:",
      #             2500000,min=100, max=30000000),
      # numericInput("n_chains", 
      #              label = "Antall rekker:",
      #              3,min=2, max=5),
      # numericInput("burn_in", 
      #              label = "Burn in:",
      #              1500000,min=1, max=25000000),
      # numericInput("n_thin", 
    #              label = "Thinning:",
    #              2,min=1, max=1000)
    n.iter=as.numeric( 25000)
    n.chains=as.numeric(3)
    n.burnin=as.numeric(15000)
    n.thin=as.numeric(2)
    }else{
    n.iter=as.numeric( 2500000)
    n.chains=as.numeric(3)
    n.burnin=as.numeric(1500000)
    n.thin=as.numeric(2)
    }
    
    # run model in JAGS
    out1<-R2jags::jags(data=bugs.data, inits=inits, parameters.to.save=parameters, 
               model.file=system.file("JAGs", "ssm_lynx1.bug", package = "HarvestGolem")
                 #"inst/JAGs/ssm_lynx1.bug"
               ,n.chains=n.chains, n.iter=n.iter, 
               n.burnin=n.burnin, n.thin=n.thin)
  }) 
  
  })
  
  # n.years<-length(input$startYear:input$endYear)
  # P_LessThanTarget <- ecdf(out1$BUGSout$sims.list$N.est[,n.years])(65)
  # print(P_LessThanTarget)
  #Estimated FG
  dataInput1<-reactive({
    Region=c(1,2,3,4,5,6,7,8)
    RegTar=c(0,12,5,6,10,12,10,10)
    RegTars=data.frame(Region,RegTar)
    d <-HarvestGolem::Lynx_monitoring_data
      #readRDS("data-raw/Lynx_monitoring_data.RDS")
    d<-d %>%
      inner_join(RegTars)
    d<-d%>% 
      filter(Region==input$model)
    d<-subset(d, d$Aar <= as.numeric(input$endYear) & d$Aar >= as.numeric(input$startYear))
    d<-as.data.frame(d)
    ###
    
    #out3<<-coda::as.mcmc(dataInput())
    #tidy_out<<-tidybayes::tidy_draws(out3)
     #out3<-coda::as.mcmc(dataInput())
     out3<<-coda::as.mcmc(dataInput())
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
                CI75=bayestestR::hdi(value,0.75))
    
   #print(EstN)
    
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
                CI75=bayestestR::hdi(value,0.75))
    
      EstN=EstN %>% 
      filter(year==max(year))
      TotalN=TotalN %>% 
        filter(year==max(year))
    
    d=d %>% filter(Aar==max(Aar)) %>% 
      summarise("Bestandsmål"=sum(RegTar), "Antall familiegrupper av gaupe påvist"=sum(FG), year=Aar[1])
    
    tabdat=EstN %>% 
      full_join(d)
    tabdat_T=TotalN %>% 
      full_join(d)
    
    
    Bestandsmål=tabdat$Bestandsmål
    Antall=tabdat$`Antall familiegrupper av gaupe påvist`
    prognosis=paste0(round(tabdat$smean,0)," [", round(tabdat$CI75$CI_low[1],2), " - ",round(tabdat$CI75$CI_high[1],2), "]" )
    prognosis=prognosis[1]
    prognosis2=paste0(round(tabdat_T$smean,2)," [", round(tabdat_T$CI75$CI_low[1],2), " - ",round(tabdat_T$CI75$CI_high[1],2), "]" )
    prognosis2=prognosis2[1]
    tab=data.frame(Bestandsmål,Antall,prognosis,prognosis2, dataInput4())
    names(tab)=c("Bestandsmål","Antall familiegrupper av gaupe påvist", "Prognose for antall familiegrupper [75% CI]","Prognose for antall gaupe [75% CI]", " P_lessThanTarget")
    tab %>% drop_na()
    
  })

  dataInput3<-reactive({
    fitted <- lower50 <- lower75 <- upper50 <- upper75 <- numeric()
    h.levels <- c(input$min_h.levels, input$mid_h.levels,input$max_h.levels)
    harvest_level<-h.levels
    for (i in 1:length(harvest_level)){
      fitted[i] <- round(quantile(dataInput()$BUGSout$sims.list$N.pred[,i], 0.5),1)
      lower50[i] <- round(quantile(dataInput()$BUGSout$sims.list$N.pred[,i], 0.25),1)
      lower75[i] <- round(quantile(dataInput()$BUGSout$sims.list$N.pred[,i], 0.125),1)
      upper50[i] <- round(quantile(dataInput()$BUGSout$sims.list$N.pred[,i], 0.75),1)
      upper75[i] <- round(quantile(dataInput()$BUGSout$sims.list$N.pred[,i], 0.875),1)
      }
       dat<-data.frame("kvotealtternativ"=harvest_level,"prognose"=fitted, lower50, lower75, upper50, upper75) #, P_LessThanTarget)
   
  })
  
  #  Region=c(1,2,3,4,5,6,7,8)
  #  RegTar=c(0,12,5,6,10,12,10,10)
  #  RegTars=data.frame(Region,RegTar)
  #  RegTars<-RegTars %>% 
  #      filter(Region==input$model)
  #  P_LessThanTarget <- ecdf(dataInput()$BUGSout$sims.list$N.est[,T])(RegTars)
  # print(P_LessThanTarget)
   
   
  output$table2 <- DT::renderDataTable({
      dataInput1()},
      
      extensions = 'Buttons',
      
      options = list(
        paging = FALSE,
        searching = FALSE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'tB',
        buttons = c('copy', 'csv', 'excel')
      ),
      
      class = "display"
  )  
  
  # output$downloadData <- downloadHandler(
  #   filename = function(){"table2.csv"}, 
  #   content = function(fname){
  #     write.csv(table2(), fname)
  #   }
  # )
  
  
  output$table<-DT::renderDataTable({
    dataInput3() %>% 
      mutate("75% CI"= paste0(lower75, " - ", upper75)) %>% 
      select("kvotealtternativ","prognose","75% CI")
  },
  
  extensions = 'Buttons',
  
  options = list(
    paging = FALSE,
    searching = FALSE,
    fixedColumns = TRUE,
    autoWidth = TRUE,
    ordering = TRUE,
    dom = 'tB',
    buttons = c('copy', 'csv', 'excel')
  ),
  
  class = "display"
  )
  
  #Need to tidy and add True FG
   output$plot1<-plotly::renderPlotly({
     out3<-coda::as.mcmc(dataInput())
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
                 CI75=bayestestR::hdi(value,0.75),
                 CI50=bayestestR::hdi(value,0.50))
     
     Region=c(1,2,3,4,5,6,7,8)
     RegTar=c(0,12,5,6,10,12,10,10)
     RegTars=data.frame(Region,RegTar)
     d <- HarvestGolem::Lynx_monitoring_data
       #readRDS("data-raw/Lynx_monitoring_data.RDS")
     d<-d %>%
       inner_join(RegTars)
     d<-d%>% 
       filter(Region==input$model)
     d<-subset(d, d$Aar <= input$endYear & d$Aar >= input$startYear)
     d<-d %>% 
       group_by(Aar) %>% 
       summarise(TotalFG=sum(FG), TotalRegTar=sum(RegTar))
     
     EstNlast=EstN %>% filter(row_number()==n()) %>% rename("Aar"=year) %>% rename("TotalFG"=smean)
     
     d%>%
      ggplot(aes(Aar, TotalFG))+
      geom_point(size=6, colour="darkgoldenrod4")+
      geom_line(colour="darkgoldenrod4", size=1)+
       labs(y="Antall familiegrupper")+
       geom_segment(x=2005, xend= input$endYear-2,y = d$TotalRegTar,yend = d$TotalRegTar, lty=2, size=2)+
       geom_point(data=EstNlast, aes(Aar+1,TotalFG), colour="darkred", size=6, shape=15 )+
       geom_segment(data=EstNlast, aes(x=input$endYear+1.2, xend=input$endYear+1.2, y= CI50$CI_low, yend=CI50$CI_high),
         size=5, lineend='round'
       ) +
       geom_segment(data=EstNlast, aes(x=input$endYear+1.4, xend=input$endYear+1.4, y= CI75$CI_low, yend=CI75$CI_high),
                    size=5, lineend='round', colour="grey"
       ) +
       
       #geom_linerange(data=EstNlast, aes(x=input$endYear+1.2,ymin= CI50$CI_low, ymax=CI50$CI_high), size=2)+
       #geom_linerange(data=EstNlast, aes(x=input$endYear+1.3,ymin= CI75$CI_low, ymax=CI75$CI_high), size=2, colour="grey")+
       theme_classic()+
       theme(axis.text.x = element_text(color = "grey20", size = 15, face = "plain"),
               axis.text.y = element_text(color = "grey20", size = 15, face = "plain"),  
               axis.title.x = element_text(color = "grey20", size = 20, face = "plain"),
               axis.title.y = element_text(color = "grey20", size = 20, face = "plain"))
   })
   
  # 
  # #need to tidy this plot
  # output$plot2<-renderPlot({
  #   dataInput2() %>% 
  #     ggplot(aes(year, smean))+
  #     geom_pointrange(aes(y=smean, ymax=smean+ssd, ymin=smean-ssd))
  # })
  # 
  # Region=c(1,2,3,4,5,6,7,8)
  # RegTar=c(0,12,5,6,10,12,10,10)
  # RegTars=data.frame(Region,RegTar)
  # 
  
  output$plot3<-plotly::renderPlotly({
     RegTar=c(0,12,5,6,10,12,10,10)
     Region=c(1,2,3,4,5,6,7,8)
   RegTars=data.frame(Region,RegTar)
   RegTars<-RegTars %>% 
     filter(Region==input$model)
   
    dataInput3() %>% 
      ggplot()+
      geom_pointrange(mapping=aes(x=kvotealtternativ, y=prognose, ymin=upper75, ymax=lower75), fatten=1, size=6, color="grey")+
      geom_pointrange(mapping=aes(x=kvotealtternativ, y=prognose, ymin=upper50, ymax=lower50), fatten=1, size=6, color="dark orange")+
      geom_point(aes(kvotealtternativ,prognose), colour="black", size=3)+
      labs(x="Uttak voksne hunndyr", y="Prognose antall familiegrupper")+
      geom_hline(yintercept=sum(RegTars$RegTar), linetype=2)+
      theme_classic()+
      theme(axis.text.x = element_text(color = "grey20", size = 15, face = "plain"),
            axis.text.y = element_text(color = "grey20", size = 15, face = "plain"),  
            axis.title.x = element_text(color = "grey20", size = 20, face = "plain"),
            axis.title.y = element_text(color = "grey20", size = 20, face = "plain"))
    
    
  })
  
  
  points <- reactive({
    plotdat %>% 
      filter(Aar==as.numeric(input$year))
  })
  
  National_data<-reactive({
    
    d <- HarvestGolem::Lynx_monitoring_data
    #readRDS(paste0(here::here(),"/data-raw/Lynx_monitoring_data.RDS"))
    
    Region=c(1,2,3,4,5,6,7,8)
    RegTar=c(0,12,5,6,10,12,10,10)
    RegTars=data.frame(Region,RegTar)
    d<-d %>%
      inner_join(RegTars)
    validate(
      need(!is.null(input$histReg), "Velg data")
    )
    d=d %>% 
      filter(Region %in% input$histReg)
    
    
  })
    
  output$National<-renderPlotly({
    
      plotd=National_data() %>% 
      group_by(Aar) %>% 
      select(!kommentar) %>%
      summarise(FG=sum(FG), uttak=sum(Antall.belastet.kvoten)) %>% 
      ggplot(aes(Aar,FG, label=FG))+
      geom_line(colour="dark green", size=3)+
      geom_bar(aes(Aar, uttak), stat="identity", fill="dark cyan",colour="black", size=1, alpha=0.4)+
      geom_hline(yintercept = sum(National_data()$RegTar[which(National_data()$Aar==2020)]), size=2, lty=2)+
      geom_point(aes(Aar,FG),size=8, colour="dark green")+
      geom_text(size = 2.5, colour="white")+
      labs(x="År", y="Antall familiegrupper / felte gauper"
           #,
           #caption = paste0("Antall familiegrupper av gaupe (sirkler) og uttak av gauper (stolpediagram) i Norge i perioden ",
           #inyear, "–", outyear, ".\nAntall familiegrupper i 2014 og senere år er ikke direkte sammenlignbart med tidligere år, da overvåkingsmetodikken \ner endret i forbindelse med samordningen med Sverige.")
      )+
      theme_classic()+
      theme(axis.line = element_line(colour = 'black', size = 2),
            axis.title = element_text(size=18, face="bold"),
            axis.text = element_text(size=14)
            #,
            #plot.caption = element_text(hjust = 0, size=14)
      )

    p=plotly::ggplotly(plotd, tooltip=NULL)   
    p
  })
  
  output$Legend <- renderText({
    d <-HarvestGolem::Lynx_monitoring_data 
      #readRDS(paste0(here::here(),"/data-raw/Lynx_monitoring_data.RDS"))
    inyear=min(d$Aar)
    outyear=max(d$Aar)
    paste0("Antall familiegrupper av gaupe (sirkler) og uttak av gauper (stolpediagram) i Norge i perioden ",
                  "1996", "–", 
                  "2010", 
                  ". Antall familiegrupper i 2014 og senere år er ikke direkte sammenlignbart med tidligere år, da overvåkingsmetodikken 
                  er endret i forbindelse med samordningen med Sverige.")
  })
  
  #****************************************
  #* Reactive Values
  
  model<-reactive({
    Region=c(1,2,3,4,5,6,7,8)
    RegTar=c(0,12,5,6,10,12,10,10)
    RegTars=data.frame(Region,RegTar)
    RegTars<-RegTars %>% 
      filter(Region==input$model)
    RegTars$Region
  })
  
  table <- reactive({
    dataInput3() 
  })
  
  plot <- reactive({
    Region=c(1,2,3,4,5,6,7,8)
    RegTar=c(0,12,5,6,10,12,10,10)
    RegTars=data.frame(Region,RegTar)
    RegTars<-RegTars %>% 
      filter(Region==input$model)
    dataInput3() %>% 
      ggplot()+
      geom_pointrange(mapping=aes(x=kvotealtternativ, y=prognose, ymin=upper75, ymax=lower75), fatten=1, size=6, color="grey")+
      geom_pointrange(mapping=aes(x=kvotealtternativ, y=prognose, ymin=upper50, ymax=lower50), fatten=1, size=6, color="dark orange")+
      geom_point(aes(kvotealtternativ,prognose), colour="black", size=3)+
      labs(x="Uttak voksne hunndyr", y="Prognose antall familiegrupper")+
      geom_hline(yintercept=sum(RegTars$RegTar), linetype=2)
    
      })
  
  plotx<-reactive({
    d <-HarvestGolem::Lynx_monitoring_data 
      #readRDS(paste0(here::here(),"/data-raw/Lynx_monitoring_data.RDS"))
    year <- input$startYear:input$endYear
    n.years <- length(year)
    d<-d%>% 
      filter(Region==input$model)
    d<-subset(d, d$Aar <= input$endYear & d$Aar >= input$startYear)
    d<-as.data.frame(d)
    ### SETTING UP THE DATA; sum for Norway models	
    
    FG <- matrix(NA, ncol=1, nrow=length(year))
    HV <- matrix(NA, ncol=1, nrow=length(year)-1)
    
    for (i in 1996:(max(year)-1)){
      temp1 <- subset(d, d[1]==i)
      FG[i-min(year)+1] <- sum(temp1[,"FG"])
    }	
    
    dat=data.frame("FG"=FG, "År"=year)
    
    Pred.res <- as.matrix(quantile(dataInput()$BUGSout$sims.list$N.est[,n.years], c(0.125, 0.25, 0.5, 0.75, 0.875)))
    Pred.res<-data.frame(lower=c(NA,Pred.res[2,1], Pred.res[1,1]),upper=c(NA,Pred.res[4,1], Pred.res[5,1]), Med=c(Pred.res[3,1],NA,NA), Group=c(NA,"50%", "75%"), "År"=c(2021,2021.3,2021.5))
    rownames(Pred.res)<-NULL
    
    dat=dat %>% full_join(Pred.res)
    
    list(dat,
         Pred.res,
         n.years,
         year)
    
  })
  
  dataInput4=reactive({
    Region=c(1,2,3,4,5,6,7,8)
    RegTar=c(0,12,5,6,10,12,10,10)
    RegTars=data.frame(Region,RegTar)
    RegTars<-RegTars %>% 
      filter(Region==input$model)
    n.years<-length(input$startYear:input$endYear)
    P_LessThanTarget <- ecdf(dataInput()$BUGSout$sims.list$N.est[,n.years])(sum(as.numeric(RegTars$RegTar)))
    round(P_LessThanTarget,2)
    
  })
  
  #****************************************
  #****************************************
  #* Screenshot
  # 
  # observeEvent(input$go, {
  #   screenshot(scale=1.5)
  # })
  
  # output$report <- downloadHandler(
  #   # For PDF output, change this to "report.pdf"
  #   filename = "report.html",
  #   content = function(file) {
  #     # Copy the report file to a temporary directory before processing it, in
  #     # case we don't have write permissions to the current working dir (which
  #     # can happen when deployed).
  #     tempReport <- file.path(tempdir(), "report.rmd")
  #     file.copy(HarvestGolem::app_sys("report.rmd"), tempReport, overwrite = TRUE)
  #     
  #     # Set up parameters to pass to Rmd document
  #     print(plot())
  #     print(model())
  #     print(table())
  #     print(plotx())
  #     params <- list(plot=plot(),
  #                         model=model(),
  #                         table=table(),
  #                         plotx=plotx())
  #     
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
 
}



