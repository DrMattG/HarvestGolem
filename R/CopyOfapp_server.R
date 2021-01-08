#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server2 <- function( input, output, session ) {
  # List the first level callModules here
  #some data manipulation to derive the values of boxes (take last year in the dataset)
  #Make a reactive dataframe
  data<-reactive({
    dattable %>%
      mutate(region=gsub("\\s*\\([^\\)]+\\)","",region)) %>% 
      mutate(Region2=case_when(
        region %in%  c("Østfold", "Akershus", "Oslo") ~ "Østfold, Akershus, Oslo",
        region %in%  c("Buskerud", "Vestfold","Telemark", "Aust-Agder") ~ "Buskerud, Vestfold, Telemark, Aust-Agder", 
        region %in%  c("Hedmark") ~ "Hedmark",
        region %in%  c("Møre og Romsdal", "Trøndelag","Sør-Trøndelag (-2017)" , "Nord-Trøndelag (-2017)") ~ "Møre og Romsdal, Trøndelag",
        region %in%  c("Nordland") ~ "Nordland",
        region %in%  c("Oppland") ~ "oppland",
        region %in%  c("Troms", "Finnmark","Troms - Romsa","Finnmark - Finnmørku") ~ "Troms, Finmark",
        region %in%  c("Vest-Agder", "Rogaland","Hordaland","Sogn og Fjordane" ) ~ "Vest-Agder, Rogaland,Hordaland, Sogn og Fjordane"))%>% 
      group_by(Region2,reason, `interval (year)`) %>% 
      filter(reason==input$reason)
  })
  
  Rdata<-reactive({
    dattable %>%
      mutate(region=gsub("\\s*\\([^\\)]+\\)","",region)) %>% 
      mutate(Region2=case_when(
        region %in%  c("Østfold", "Akershus", "Oslo") ~ "Østfold, Akershus, Oslo",
        region %in%  c("Buskerud", "Vestfold","Telemark", "Aust-Agder") ~ "Buskerud, Vestfold, Telemark, Aust-Agder", 
        region %in%  c("Hedmark") ~ "Hedmark",
        region %in%  c("Møre og Romsdal", "Trøndelag","Sør-Trøndelag (-2017)" , "Nord-Trøndelag (-2017)") ~ "Møre og Romsdal, Trøndelag",
        region %in%  c("Nordland") ~ "Nordland",
        region %in%  c("Oppland") ~ "oppland",
        region %in%  c("Troms", "Finnmark","Troms - Romsa","Finnmark - Finnmørku") ~ "Troms, Finmark",
        region %in%  c("Vest-Agder", "Rogaland","Hordaland","Sogn og Fjordane" ) ~ "Vest-Agder, Rogaland,Hordaland, Sogn og Fjordane"))%>% 
      group_by(Region2,reason, `interval (year)`) %>% 
      filter(reason==input$reason) %>% 
      filter(Region2==input$region)
  })
  
  Ldata1<-reactive({
    dattable2 %>% 
      filter(contents=="Quota hunting, lynx, licenses issued") %>% 
      group_by(region, `interval (year)`)
  })
  
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
  output$table<-renderTable(policies())

  
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
    
    Net <- setNode(Net, M01,
                   nodeType = "dpois", 
                   lambda = 80)
    Net <- setNode(Net, M02,
                   nodeType = "dpois", 
                   lambda = 40)
    Net <- setNode(Net, M03,
                   nodeType = "dpois", 
                   lambda = 30)
    Net <- setNode(Net, M04,
                   nodeType = "dpois", 
                   lambda =  28)
    
    
    
    Net <- setNode(Net, F01,
                   nodeType = "dpois", 
                   lambda =  80)
    
    
    Net <- setNode(Net, F02,
                   nodeType = "dpois", 
                   lambda = 40)
    Net <- setNode(Net, F03,
                   nodeType = "dpois", 
                   lambda = 30)
    Net <- setNode(Net, F04,
                   nodeType = "dpois", 
                   lambda =  28)
    #FG0=55
    #a1=FG0*.09
    #b1= (FG0*.09)+10
    #a2=FG0*.47
    #b2=(FG0*.47)+10
    #a3=FG0*.43
    #b3=(FG0*.43)+10
    
    #Net<-setNode(Net, F02, nodeType="dunif",a=a1,b=b1)
    #Net<-setNode(Net, F03, nodeType="dunif",a=a2,b=b2)
    #Net<-setNode(Net, F04, nodeType="dunif",a=a3,b=b3)
    
    
    
    
    
    
    
    
    
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
  
  
  output$plot1<-renderPlot({
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
      labs(x=" Harvest rate", y= "mean Family Group estimate and 95% CI")+
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
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addPolygons(data=shape, weight = 1, color = "black") %>%
      addCircleMarkers(data = points(),lng = ~x, lat = ~y ,radius = ~n,label = ~Name) 
  })
  
  
  output$mortality <- renderPlotly({
    print(ggplotly(
      ggplot(data = data(),
             aes(x=`interval (year)`, y=value) )+
        geom_bar(position = "dodge", stat = "identity", fill="red") +
        labs(x="Year", y="Number of recorded mortalities")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        ggtitle(as.character(input$reason))+
        theme(plot.title = element_text(hjust = 0.5))))
  })
  output$region <- renderPlot({
    ggplot(data = Rdata(),
           aes(x=`interval (year)`, y=value) )+
      geom_bar(position = "dodge", stat = "identity", fill="red") +
      labs(x="Year", y="Number of recorded mortalities")+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      ggtitle(as.character(input$region))+
      theme(plot.title = element_text(hjust = 0.5))
  })
  output$license <- renderPlotly({
    print(ggplotly(
      ggplot(data = Ldata1(),
             aes(x=`interval (year)`, y=value))+
        geom_bar(position = "dodge", stat = "identity", fill="blue") +
        labs(x="Year", y="Number of licenses issued")+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        ggtitle("Number of licenses issued for Lynx")+
        theme(plot.title = element_text(hjust = 0.5))))
  })
  output$regionL <- renderPlot({
    ggplot(data = Ldata2(),
           aes(x=`interval (year)`, y=value))+
      geom_bar(position = "dodge", stat = "identity", fill="blue" ) +
      labs(x="Year", y="Number of licenses issued")+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      ggtitle(as.character(input$regionL))+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
}