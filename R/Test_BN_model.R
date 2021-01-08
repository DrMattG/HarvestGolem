# library(tidyverse)
# library(HydeNet)
# 
# runmod<-function(iterations=10000){
#   
# policies<-data.frame(Hv = c(0))
# FG0<-55
# Net<-HydeNetwork(~ M01S|M01
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
# plot(Net)
# Net <- setNode(Net, M01,
#                  nodeType = "dpois", 
#                  lambda = 80)
#   Net <- setNode(Net, M02,
#                  nodeType = "dpois", 
#                  lambda = 40)
#   Net <- setNode(Net, M03,
#                  nodeType = "dpois", 
#                  lambda = 30)
#   Net <- setNode(Net, M04,
#                  nodeType = "dpois", 
#                  lambda =  28)
#   
#   
#   
#   Net <- setNode(Net, F01,
#                  nodeType = "dpois", 
#                  lambda =  80)
#   
#   
#   Net <- setNode(Net, F02,
#                  nodeType = "dpois", 
#                  lambda = 40)
#   Net <- setNode(Net, F03,
#                  nodeType = "dpois", 
#                  lambda = 30)
#   Net <- setNode(Net, F04,
#                  nodeType = "dpois", 
#                  lambda =  28)
#   Net <- setNode(Net, M01S,
#                  nodeType = "dnorm", 
#                  mean = .795, sd = 0.107)
#   Net <- setNode(Net, M02S,
#                  nodeType = "dnorm", 
#                  mean = .667, sd = 0.192)
#   Net <- setNode(Net, M03S,
#                  nodeType = "dnorm", 
#                  mean = .741, sd = 0.129)
#   Net <- setNode(Net, M04S,
#                  nodeType = "dnorm", 
#                  mean = .8, sd = 0.1)
#   Net <- setNode(Net, F01S,
#                  nodeType = "dnorm", 
#                  mean = .56, sd = 0.149)
#   Net <- setNode(Net, F02S,
#                  nodeType = "dnorm", 
#                  mean = .75, sd = 0.217)
#   Net <- setNode(Net, F03S,
#                  nodeType = "dnorm", 
#                  mean = .84, sd = 0.1)
#   Net <- setNode(Net, F04S,
#                  nodeType = "dnorm", 
#                  mean = .8, sd = 0.1)
#   
#   Net <- setNode(Net, M11, nodeType="determ",
#                  define=fromFormula(), nodeFormula = M11 ~ M01*M01S)
#   Net <- setNode(Net, M12, nodeType="determ",
#                  define=fromFormula(), nodeFormula = M12 ~ M02*M02S)
#   Net <- setNode(Net, M13, nodeType="determ",
#                  define=fromFormula(), nodeFormula = M13 ~ M03*M03S)
#   
#   Net <- setNode(Net, M14, nodeType="determ",
#                  define=fromFormula(), nodeFormula = M14 ~ M04*M04S)
#   
#   
#   Net <- setNode(Net, F11, nodeType="determ",
#                  define=fromFormula(), nodeFormula = F11 ~ F01*F01S)
#   Net <- setNode(Net, F12, nodeType="determ",
#                  define=fromFormula(), nodeFormula = F12 ~ F02*F02S)
#   Net <- setNode(Net, F13, nodeType="determ",
#                  define=fromFormula(), nodeFormula = F13 ~ F03*F03S)
#   Net <- setNode(Net, F14, nodeType="determ",
#                  define=fromFormula(), nodeFormula = F14 ~ F04*F04S)
#   
#   Net <- setNode(Net, N1, nodeType="determ",
#                  define=fromFormula(), nodeFormula = 
#                    N1 ~ HRF1 + HRF2 + HRF3+ HRF4 + HRM1 + HRM2 + HRM3 + HRM4)
#   #######################################################################
#   #######################Reproduction####################################
#   Net <- setNode(Net, RF2, nodeType="determ",
#                  define=fromFormula(), nodeFormula = RF2 ~ F12*0.65)
#   Net <- setNode(Net, RF3, nodeType="determ",
#                  define=fromFormula(), nodeFormula =  RF3 ~F13*1.65)
#   Net <- setNode(Net, RF4, nodeType="determ",
#                  define=fromFormula(), nodeFormula =  RF4 ~F14*1.34)
#   Net <- setNode(Net, R, nodeType="determ",
#                  define=fromFormula(), nodeFormula = R ~RF3+RF2+RF4)
#   
#   #######################################################################
#   ######################################################################
#   Net <- setNode(Net, HRM1, nodeType="determ",
#                  define=fromFormula(), nodeFormula = HRM1 ~R*0.5-(Hv*0.54))
#   Net <- setNode(Net, HRM2, nodeType="determ",
#                  define=fromFormula(), nodeFormula =  HRM2 ~M11-Hv*(0.19))
#   Net <- setNode(Net, HRM3, nodeType="determ",
#                  define=fromFormula(), nodeFormula =  HRM3 ~M12-Hv*(0.11))
#   Net <- setNode(Net, HRM4, nodeType="determ",
#                  define=fromFormula(), nodeFormula =  HRM4 ~M13+M14-(Hv*0.15))
#   Net <- setNode(Net, HRF1, nodeType="determ",
#                  define=fromFormula(), nodeFormula = HRF1~ R*0.5-Hv*(0.45))
#   Net <- setNode(Net, HRF2, nodeType="determ",
#                  define=fromFormula(), nodeFormula = HRF2 ~F11-Hv*(0.14))
#   Net <- setNode(Net, HRF3, nodeType="determ",
#                  define=fromFormula(), nodeFormula =  HRF3 ~F12-Hv*(0.11))
#   Net <- setNode(Net, HRF4, nodeType="determ",
#                  define=fromFormula(), nodeFormula =  HRF4 ~F13+F14-Hv*(0.28))
#   Net <- setNode(Net,Hv,
#                  nodeType = "dnorm", mean=50, sd=0.0001)
#   
#   
#   #females probability of having at least one kitten
#   Net <- setNode(Net, FG, nodeType="determ",
#                  define=fromFormula(), nodeFormula =  FG ~ F12*0.20+F13*0.99+F14*0.91)
#   
#   Net <- setNode(Net, FG, nodeType="determ",
#                  define=fromFormula(), nodeFormula = FG ~ F12*0.20+F13*0.99+F14*0.91)
#   
#   Net<-setNode(Net,F21, nodeType = "determ",
#                define=fromFormula(), nodeFormula = F21~HRF1)
#   Net<-setNode(Net,F22, nodeType = "determ",
#                define=fromFormula(), nodeFormula =  F22~HRF2)
#   Net<-setNode(Net,F23, nodeType = "determ",
#                define=fromFormula(), nodeFormula =  F23~HRF3)
#   Net<-setNode(Net,F24, nodeType = "determ",
#                define=fromFormula(), nodeFormula =  F24~HRF4)
#   Net<-setNode(Net,M21, nodeType = "determ",
#                define=fromFormula(), nodeFormula =  M21~HRM1)
#   Net<-setNode(Net,M22, nodeType = "determ",
#                define=fromFormula(), nodeFormula =  M22~HRM2)
#   Net<-setNode(Net,M23, nodeType = "determ",
#                define=fromFormula(), nodeFormula = M23~HRM3)
#   Net<-setNode(Net,M24, nodeType = "determ",
#                define=fromFormula(), nodeFormula = M24~HRM4)
#   
#   Net <- setNode(Net, M21S,
#                  nodeType = "dnorm", 
#                  mean = .795, sd = 0.107)
#   Net <- setNode(Net, M22S,
#                  nodeType = "dnorm", 
#                  mean = .667, sd = 0.192)
#   Net <- setNode(Net, M23S,
#                  nodeType = "dnorm", 
#                  mean = .741, sd = 0.129)
#   Net <- setNode(Net, M24S,
#                  nodeType = "dnorm", 
#                  mean = .8, sd = 0.1)
#   Net <- setNode(Net, F21S,
#                  nodeType = "dnorm", 
#                  mean = .56, sd = 0.149)
#   Net <- setNode(Net, F22S,
#                  nodeType = "dnorm", 
#                  mean = .75, sd = 0.217)
#   Net <- setNode(Net, F23S,
#                  nodeType = "dnorm", 
#                  mean = .84, sd = 0.1)
#   Net <- setNode(Net, F24S,
#                  nodeType = "dnorm", 
#                  mean = .8, sd = 0.1)
#   
#   Net <- setNode(Net, M31, nodeType = "determ",
#                  define=fromFormula(), nodeFormula =  M31 ~ M21*M21S)
#   Net <- setNode(Net, M32, nodeType = "determ",
#                  define=fromFormula(), nodeFormula = M32 ~ M22*M22S)
#   Net <- setNode(Net, M33, nodeType = "determ",
#                  define=fromFormula(), nodeFormula = M33 ~ M23*M23S)
#   Net <- setNode(Net, M34, nodeType = "determ",
#                  define=fromFormula(), nodeFormula = M34 ~ M24*M24S)
#   Net <- setNode(Net, F31, nodeType = "determ",
#                  define=fromFormula(), nodeFormula = F31 ~ F21*F21S)
#   Net <- setNode(Net, F32, nodeType = "determ",
#                  define=fromFormula(), nodeFormula =  F32 ~ F22*F22S)
#   Net <- setNode(Net, F33, nodeType = "determ",
#                  define=fromFormula(), nodeFormula = F33 ~ F23*F23S)
#   Net <- setNode(Net, F34, nodeType = "determ",
#                  define=fromFormula(), nodeFormula = F34 ~ F24*F24S)
#   
#   # Net <- setNode(Net, F34, nodeType="dpois",
#   #                lambda=fromFormula(), #sigma^2 = 30
#   #                nodeFormula = F34 ~ F24*F24S)
#   
#   Net <- setNode(Net, FG1, nodeType="determ",
#                  mean=fromFormula(), 
#                  nodeFormula = FG1 ~ F32*0.20+F33*0.99+F34*0.91)
#   
#   
# 
#   trackedVars <- c("FG1")
#   compiledNets <- compileDecisionModel(Net, policyMatrix = policies, n.chains=5)  
#   
#   samples <- lapply(compiledNets,
#                     HydeSim,
#                     variable.names = trackedVars,
#                     n.iter=iterations, trace=F)
# 
# 
# df_list<-bind_rows(samples, .id = "id")
#   df_list<-as.data.frame(df_list)
#   lower_ci <- function(mean, se, n, conf_level = 0.95){
#     lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
#   }
#   upper_ci <- function(mean, se, n, conf_level = 0.95){
#     upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
#   }
#   
#   df_list%>%
#     group_by(Hv) %>%
#     summarise(smean = mean(FG1, na.rm = TRUE),
#               ssd = sd(FG1, na.rm = TRUE),
#               count = n()) %>%
#     mutate(se = ssd / sqrt(count),
#            lower_ci = lower_ci(smean, se, count),
#            upper_ci = upper_ci(smean, se, count))%>% 
#     ggplot(., aes(as.numeric(Hv), smean))+
#     geom_pointrange(aes(x=as.numeric(Hv),y=smean, ymin=lower_ci, ymax=upper_ci), colour="white")+
#     scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150)) +
#     labs(x=" Harvest rate", y= "mean Family Group estimate and 95% CI")+
#     geom_hline(yintercept=66,size=1,linetype=4,color="white")+
#     theme_dark()
# out=mean(df_list$FG1)
# #mean(df_list$FG1)
# return(out)
# } 
# 
# repx={replicate(1000,runmod(iterations = 20000), simplify = FALSE)}
# 
# head(repx)
# #system.time({runmod()})
# repl1 = {replicate(100, runmod(iterations = 8000), simplify=FALSE)}
# 
# Rep_8000=data.frame(Rep_8000=unlist(repl1))
# 
# 
# repl = {replicate(100, runmod(10000), simplify=FALSE)}
# 
# Rep_10000=data.frame(Rep_10000=unlist(repl))
# 
# repl2 = {replicate(100, runmod(20000), simplify=FALSE)}
# 
# Rep_20000=data.frame(Rep_20000=unlist(repl2))
# 
# repl3 = {replicate(100, runmod(30000), simplify=FALSE)}
# 
# Rep_30000=data.frame(Rep_30000=unlist(repl3))
# 
# repl4 = {replicate(100, runmod(40000), simplify=FALSE)}
# 
# Rep_40000=data.frame(Rep_40000=unlist(repl4))
# 
# repl5 = {replicate(100, runmod(iterations = 50000), simplify=FALSE)}
# 
# Rep_50000=data.frame(Rep_50000=unlist(repl5))
# 
# repl6 = {replicate(100, runmod(iterations = 60000), simplify=FALSE)}
# 
# Rep_60000=data.frame(Rep_60000=unlist(repl6))
# 
# repl7 = {replicate(100, runmod(iterations = 70000), simplify=FALSE)}
# 
# Rep_70000=data.frame(Rep_70000=unlist(repl7))
# repl8 = {replicate(100, runmod(iterations = 80000), simplify=FALSE)}
# 
# Rep_80000=data.frame(Rep_80000=unlist(repl8))
# 
# repl9 = {replicate(100, runmod(iterations = 90000), simplify=FALSE)}
# 
# Rep_90000=data.frame(Rep_90000=unlist(repl9))
# 
# repl10 = {replicate(100, runmod(iterations = 100000), simplify=FALSE)}
# 
# Rep_100000=data.frame(Rep_100000=unlist(repl10))
# 
# repl11 = {replicate(100, runmod(iterations = 110000), simplify=FALSE)}
# 
# Rep_110000=data.frame(Rep_110000=unlist(repl11))
# 
# repl12 = {replicate(100, runmod(iterations = 120000), simplify=FALSE)}
# 
# Rep_120000=data.frame(Rep_120000=unlist(repl12))
# 
# 
# bind<-cbind(Rep_8000,Rep_10000, Rep_20000, Rep_30000,
#             Rep_40000,Rep_50000, Rep_60000, Rep_70000,
#             Rep_80000,Rep_90000, Rep_100000,Rep_110000,
#             Rep_120000)
# #bind
# 
# bind %>% 
#   rowid_to_column("Iteration") %>% 
#   gather(key=key, value=value, -Iteration) %>% 
#   mutate(key=gsub("Rep_", "",key)) %>% 
#   ggplot(aes(x=value,y=reorder(key,as.numeric(key)), fill=key)) +
#   labs(x="FG1", y="N. iters.")+
#   ggridges::geom_density_ridges()+
#   ggpubr::theme_pubr()+
#   theme(legend.position = "none")
# 
