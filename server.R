library(shiny)
library(heatmaply)
library(plotly)
library(networkD3)
library(visNetwork)

shinyServer(function(input, output) {
  output$text00 <- renderDataTable({
    if(input$analysis == "Basic_EDA1"){
      
          
      req(input$file1)
      
      if(input$sep2 == "Separator_Comma"){sep <- ","}
      if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
      if(input$sep2 == "Separator_Tab"){sep <- "\t"}
      Data <- read.csv(input$file1$datapath, header=T,sep = sep)
      if(input$DoNotUseFirst == 1){
        Data[,1] <- NULL
      }
          
      summary(Data)
    }
  })
  #output$plot03 <- renderPlot({
  output$plot03 <- renderPlotly({
    if(input$analysis ==  "Heat_map1"){
          
      req(input$file1)
      
      if(input$sep2 == "Separator_Comma"){sep <- ","}
      if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
      if(input$sep2 == "Separator_Tab"){sep <- "\t"}
      
      if(input$Use_one_row_as_sample_name1 == 1){
        Data <- read.csv(input$file1$datapath, header=T,sep = sep, row.names=input$sample_row1)
      } else {
        Data <- read.csv(input$file1$datapath, header=T,sep = sep)
      }
      
      
      library(dummies)
      library(heatmaply)
      
      if(input$Change_categorical_data_into == "Dummy_Varaiables1"){
        Data2 <- dummy.data.frame(Data)
      } else {
        Data2 <- Data
        n <- ncol(Data)
        for (i in 1:n) {
          if(class(Data[,i]) == "character"){
            Data2[,i] <- as.numeric(as.factor(Data[,i]))
          }
        }
      }
      
      Data3 <- Data2
      if(input$Normalization_use == 1){
        n <- ncol(Data2)
        for (i in 1:n) {
          Data3[,i] <- (Data2[,i]-min(Data2[,i]))/(max(Data2[,i])-min(Data2[,i]))
        }
      }
      
      if(input$Use_decreasing1 == 1){
        Data3 <- Data3[order(Data3[,input$Row_number_decreasing1], decreasing=T),]
        
      }
      Data4 <- as.matrix(Data3)
      if(input$AddClusteringRow == 0){
        if(input$AddClusteringCol == 0){
          heatmaply(Data4, Colv = NA, Rowv = NA)
        } else {
          heatmaply(Data4, Colv = NA)
        }
      } else {
        if(input$AddClusteringCol == 0){
          heatmaply(Data4, Rowv = NA)
        } else {
          heatmaply(Data4)
        }
      }
    }
  })
  
  output$plot01 <- renderPlotly({
    if(input$analysis == "Similarity_of_Variables_and_Categories1"){
      if(input$Similarity_of_Variables_and_Categories == "Among_all_columns1"){
        if(input$Among_all_columns == "Line_graph1"){
      
          req(input$file1)
          
          if(input$sep2 == "Separator_Comma"){sep <- ","}
          if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
          if(input$sep2 == "Separator_Tab"){sep <- "\t"}
          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
          if(input$DoNotUseFirst == 1){
            Data[,1] <- NULL
          }
          library(dummies)
          library(ggplot2)
          library(tidyr)
          Data2 <- dummy.data.frame(Data)
          Data2$No <-as.numeric(row.names(Data2))
          Data_long <- tidyr::gather(Data2, key="Yno", value = Ys, -No)
          
          if(input$Line_graph == "Box_Integrated1"){
            gplot <- ggplot(Data_long, aes(x=No,y=Ys, colour=Yno)) + geom_line()
          } else {
            gplot <- ggplot(Data_long, aes(x=No,y=Ys)) + geom_line() + facet_wrap(~Yno,scales="free")
          }
          ggplotly(gplot)
        }
      }  
    }
  })
  
  
  output$plot14 <- renderPlotly({
    if(input$analysis == "Similarity_of_Variables_and_Categories1"){
      if(input$Similarity_of_Variables_and_Categories == "Among_all_columns1"){
        if(input$Among_all_columns == "Stratifeid_graph1"){
      
          req(input$file1)
          library(lme4)
          library(dplyr)
          library(plotly)
          
          if(input$sep2 == "Separator_Comma"){sep <- ","}
          if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
          if(input$sep2 == "Separator_Tab"){sep <- "\t"}
          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
          if(input$DoNotUseFirst == 1){
            Data[,1] <- NULL
          }
          Data1 <- Data
          library(ggplot2)
          if(input$Xcol > 0){Xname <- names(Data1[input$Xcol])} else {Xname <- "None"}
          
          if(input$Lcol > 0){Lname<-names(Data1[input$Lcol])} else {Lname<-"None"}
          if(input$Ccol > 0){Cname<-names(Data1[input$Ccol])} else {Cname<-"None"}
          if(input$Scol > 0){Sname<-names(Data1[input$Scol])} else {Sname<-"None"}
          if(input$Scol2 > 0){Sname2<-names(Data1[input$Scol2])} else {Sname2<-"None"}
          
          
          
          if(input$Gtype == "scatter"){
            if(input$Scol != 0 && input$Ccol != 0){
              if(class(Data1[,input$Scol]) == "numeric" || class(Data1[,input$Scol]) == "integer"){Data1[,input$Scol] <- droplevels(cut(Data1[,input$Scol], breaks = input$NumericalToCategorcalS, include.lowest = TRUE))}
              if(input$NumericalToCategorcalSColor2 == 1){
                if(class(Data1[,input$Ccol]) == "numeric" || class(Data1[,input$Ccol]) == "integer"){Data1[,input$Ccol] <- droplevels(cut(Data1[,input$Ccol], breaks = input$NumericalToCategorcalS, include.lowest = TRUE))}
              }
              if(input$NumericalToCategorcalSColor1 == 1){Data1[,input$Ccol] <- as.factor(Data1[,input$Ccol])}
              
              if(input$Using_GLM == 1){
                output$text532 <- renderPrint(anova(step(lm(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Scol]*Data1[,input$Ccol], data=Data1))))
                output$text535 <- renderPrint(paste("Data1[,input$Lcol] =" ,Lname,"   Data1[,input$Xcol] =" ,Xname,"   Data1[,input$Scol] =" ,Sname,"   Data1[,input$Ccol] =" ,Cname))
                if(input$family_link2 == "gaussian_identity"){
                  output$text533 <- renderPrint(anova(step(glm(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Scol]*Data1[,input$Ccol], data=Data1, family= gaussian(link = "identity")))))
                } else if(input$family_link2 == "poisson_log"){
                  output$text533 <- renderPrint(anova(step(glm(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Scol]*Data1[,input$Ccol], data=Data1, family= poisson(link = "log")))))
                } else if(input$family_link2 == "binomial_logit"){
                  output$text533 <- renderPrint(anova(step(glm(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Scol]*Data1[,input$Ccol], data=Data1, family= binomial(link = "logit")))))
                } else if(input$family_link2 == "binomial_probit"){
                  output$text533 <- renderPrint(anova(step(glm(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Scol]*Data1[,input$Ccol], data=Data1, family= binomial(link = "probit")))))
                } 
                #output$text533 <- renderPrint(anova(step(glm(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Scol]*Data1[,input$Ccol], data=Data1, family= input$family2))))
                output$text534 <- renderPrint(paste("Data1[,input$Lcol] =" ,Lname,"   Data1[,input$Xcol] =" ,Xname,"   Data1[,input$Scol] =" ,Sname,"   Data1[,input$Ccol] =" ,Cname))
              }  
              gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol],y=Data1[,input$Lcol])) + geom_point(aes(colour=Data1[,input$Ccol])) + facet_wrap(~Data1[,input$Scol],scales="free")+ labs(x=Xname,y=Lname,color=Cname,subtitle = Sname)
            } else if(input$Scol == 0 && input$Ccol != 0){
              if(input$NumericalToCategorcalSColor2 == 1){
                if(class(Data1[,input$Ccol]) == "numeric" || class(Data1[,input$Ccol]) == "integer"){Data1[,input$Ccol] <- droplevels(cut(Data1[,input$Ccol], breaks = input$NumericalToCategorcalS, include.lowest = TRUE))}
              }
              if(input$NumericalToCategorcalSColor1 == 1){Data1[,input$Ccol] <- as.factor(Data1[,input$Ccol])}
              
              if(input$Using_GLM == 1){
                output$text526 <- renderPrint(anova(step(lm(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Ccol], data=Data1))))
                if(input$family_link2 == "gaussian_identity"){
                  output$text527 <- renderPrint(anova(step(glm(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Ccol], data=Data1, family= gaussian(link = "identity")))))
                } else if(input$family_link2 == "poisson_log"){
                  output$text527 <- renderPrint(anova(step(glm(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Ccol], data=Data1, family= poisson(link = "log")))))
                } else if(input$family_link2 == "binomial_logit"){
                  output$text527 <- renderPrint(anova(step(glm(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Ccol], data=Data1, family= binomial(link = "logit")))))
                } else if(input$family_link2 == "binomial_probit"){
                  output$text527 <- renderPrint(anova(step(glm(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Ccol], data=Data1, family= binomial(link = "probit")))))
                } 
                #output$text527 <- renderPrint(anova(step(glm(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Ccol], data=Data1, family= input$family2))))
                output$text528 <- renderPrint(paste("Data1[,input$Lcol] =" ,Lname,"   Data1[,input$Xcol] =" ,Xname,"   Data1[,input$Ccol] =" ,Cname))
              }
              gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol],y=Data1[,input$Lcol])) + geom_point(aes(colour=Data1[,input$Ccol])) + labs(x=Xname,y=Lname,color=Cname)
            } else if(input$Scol != 0 && input$Ccol == 0){
              if(class(Data1[,input$Scol]) == "numeric" || class(Data1[,input$Scol]) == "integer"){Data1[,input$Scol] <- droplevels(cut(Data1[,input$Scol], breaks = input$NumericalToCategorcalS, include.lowest = TRUE))}
              
              if(input$Using_GLM == 1){
                output$text529 <- renderPrint(anova(step(lm(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Scol], data=Data1))))
                if(input$family_link2 == "gaussian_identity"){
                  output$text530 <- renderPrint(anova(step(glm(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Scol], data=Data1, family= gaussian(link = "identity")))))
                } else if(input$family_link2 == "poisson_log"){
                  output$text530 <- renderPrint(anova(step(glm(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Scol], data=Data1, family= poisson(link = "log")))))
                } else if(input$family_link2 == "binomial_logit"){
                  output$text530 <- renderPrint(anova(step(glm(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Scol], data=Data1, family= binomial(link = "logit")))))
                } else if(input$family_link2 == "binomial_probit"){
                  output$text530 <- renderPrint(anova(step(glm(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Scol], data=Data1, family= binomial(link = "probit")))))
                } 
                #output$text530 <- renderPrint(anova(step(glm(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Scol], data=Data1, family= input$family2))))
                output$text531 <- renderPrint(paste("Data1[,input$Lcol] =" ,Lname,"   Data1[,input$Xcol] =" ,Xname,"   Data1[,input$Scol] =" ,Sname))
              }
              gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol],y=Data1[,input$Lcol])) + geom_point() + facet_wrap(~Data1[,input$Scol],scales="free")+ labs(x=Xname,y=Lname,subtitle = Sname)
            } else {
              output$text518 <- renderPrint(paste("Correlation coefficient =" ,cor(Data1[,input$Xcol],Data1[,input$Lcol])))
              if(input$Using_GLM == 1){
                if(input$family_link2 == "gaussian_identity"){
                  output$text519 <- renderPrint(anova(step(glm(Data1[,input$Lcol]~Data1[,input$Xcol], data=Data1, family= gaussian(link = "identity")))))
                } else if(input$family_link2 == "poisson_log"){
                  output$text519 <- renderPrint(anova(step(glm(Data1[,input$Lcol]~Data1[,input$Xcol], data=Data1, family= poisson(link = "log")))))
                } else if(input$family_link2 == "binomial_logit"){
                  output$text519 <- renderPrint(anova(step(glm(Data1[,input$Lcol]~Data1[,input$Xcol], data=Data1, family= binomial(link = "logit")))))
                } else if(input$family_link2 == "binomial_probit"){
                  output$text519 <- renderPrint(anova(step(glm(Data1[,input$Lcol]~Data1[,input$Xcol], data=Data1, family= binomial(link = "probit")))))
                } 
                #output$text519 <- renderPrint(summary(glm(Data1[,input$Lcol]~Data1[,input$Xcol], data=Data1, family= input$family2)))
                output$text520 <- renderPrint(paste("Data1[,input$Lcol] =" ,Lname,"   Data1[,input$Xcol] =" ,Xname,"   input$family2 =" ,input$family2))
              }
              if(input$Check_difference_between_two_variablesl == 1){
                output$text537 <- renderPrint(t.test(x=Data1[,input$Xcol],y=Data1[,input$Lcol],paired=T))
                output$plot537 <- renderPlot(ggplot(Data1, aes(x=(Data1[,input$Lcol] - Data1[,input$Xcol]))) + geom_histogram() + labs(x=paste("Difference between",Lname,"and",Xname)))
              }
              if(input$Using_Prediction_Interval == 1){
                lm <- lm(Data1[,input$Lcol] ~ Data1[,input$Xcol], data=Data1)
                Data0 <- predict(lm, Data1, interval="prediction", level = input$Prediction_Interval_Probability)
                Data2 <- cbind(Data, Data0)
                ggplot(data = Data2, aes(x=Data1[,input$Xcol],y=Data1[,input$Lcol])) +geom_point()+geom_line(aes(y=lwr), color = "red")+geom_line(aes(y=upr), color = "red")+geom_line(aes(y= fit), color = "blue")
              } else {
                #if(input$Using_interactive_graph1 == 1){
                #  #output$plot538 <- renderPlotly(plot_ly(Data1, x=~Data1[,input$Xcol], y=~Data1[,input$Lcol], type = 'scatter'))
                #  output$plot538 <- renderPlotly(ggplotly(ggplot(Data1, aes(x=Data1[,input$Xcol],y=Data1[,input$Lcol])) + geom_point() + labs(x=Xname,y=Lname)))
                #}
                gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol],y=Data1[,input$Lcol])) + geom_point() + labs(x=Xname,y=Lname)

              }
              
            } 
            ggplotly(gplot)
            
          } else if(input$Gtype == "line_graph"){
            Data1$No <-as.numeric(row.names(Data1)) 
            
            if(input$Scol != 0 && input$Ccol != 0){
              if(class(Data1[,input$Scol]) == "numeric" || class(Data1[,input$Scol]) == "integer"){Data1[,input$Scol] <- droplevels(cut(Data1[,input$Scol], breaks = input$NumericalToCategorcalS, include.lowest = TRUE))}
              if(input$NumericalToCategorcalSColor2 == 1){
                if(class(Data1[,input$Ccol]) == "numeric" || class(Data1[,input$Ccol]) == "integer"){Data1[,input$Ccol] <- droplevels(cut(Data1[,input$Ccol], breaks = input$NumericalToCategorcalS, include.lowest = TRUE))}
              }
              if(input$NumericalToCategorcalSColor1 == 1){Data1[,input$Ccol] <- as.factor(Data1[,input$Ccol])}
              
              gplot <- ggplot(Data1, aes(x=Data1$No,y=Data1[,input$Lcol])) + geom_line(aes(colour=Data1[,input$Ccol])) + facet_wrap(~Data1[,input$Scol],scales="free")+ labs(x="Index",y=Lname,color=Cname,subtitle = Sname)
            } else if(input$Scol == 0 && input$Ccol != 0){
              if(input$NumericalToCategorcalSColor2 == 1){
                if(class(Data1[,input$Ccol]) == "numeric" || class(Data1[,input$Ccol]) == "integer"){Data1[,input$Ccol] <- droplevels(cut(Data1[,input$Ccol], breaks = input$NumericalToCategorcalS, include.lowest = TRUE))}
              }
              if(input$NumericalToCategorcalSColor1 == 1){Data1[,input$Ccol] <- as.factor(Data1[,input$Ccol])}
  
              gplot <- ggplot(Data1, aes(x=Data1$No,y=Data1[,input$Lcol])) + geom_line(aes(colour=Data1[,input$Ccol])) + labs(x="Index",y=Lname,color=Cname)
            } else if(input$Scol != 0 && input$Ccol == 0){
              if(class(Data1[,input$Scol]) == "numeric" || class(Data1[,input$Scol]) == "integer"){Data1[,input$Scol] <- droplevels(cut(Data1[,input$Scol], breaks = input$NumericalToCategorcalS, include.lowest = TRUE))}
              
              gplot <- ggplot(Data1, aes(x=Data1$No,y=Data1[,input$Lcol])) + geom_line() + facet_wrap(~Data1[,input$Scol],scales="free")+ labs(x="Index",y=Lname,subtitle = Sname)
            } else {
              
              gplot <- ggplot(Data1, aes(x=Data1$No,y=Data1[,input$Lcol])) + geom_line() + labs(x="Index",y=Lname)
            }
            ggplotly(gplot)
            
          } else if(input$Gtype == "box_plot"){
            if(class(Data1[,input$Xcol]) == "numeric" || class(Data1[,input$Xcol]) == "integer"){Data1[,input$Xcol] <- droplevels(cut(Data1[,input$Xcol], breaks = input$NumericalToCategorcalS, include.lowest = TRUE))}
            
            if(input$Xcol != 0){
              if(input$Scol != 0){
                if(class(Data1[,input$Scol]) == "numeric" || class(Data1[,input$Scol]) == "integer"){Data1[,input$Scol] <- droplevels(cut(Data1[,input$Scol], breaks = input$NumericalToCategorcalS, include.lowest = TRUE))}
                if(input$Scol2 == 0){
                  mean01 <- aggregate(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Scol],data=Data1,FUN=mean)
                  names(mean01) <- c(names(Data1[input$Xcol]),names(Data1[input$Scol]),paste("mean of",names(Data1[input$Lcol])))
                  output$text522 <- renderPrint(mean01)
                  if(input$Using_hypothesis_testing2 ==1){
                    output$text513 <- renderPrint(summary(aov(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Scol],data=Data)))
                    output$text514 <- renderPrint(paste("Data1[,input$Xcol] =" ,Xname,"   Data1[,input$Scol] =" ,Sname))
                    output$text515 <- renderPrint(summary(aov(Data1[,input$Lcol]~Data1[,input$Xcol]+Data1[,input$Scol],data=Data)))
                    output$text516 <- renderPrint(paste("Data1[,input$Xcol] =" ,Xname,"   Data1[,input$Scol] =" ,Sname))
                  }
                  gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol],y=Data1[,input$Lcol])) + geom_boxplot() + facet_wrap(~Data1[,input$Scol],scales="free")+ labs(x=Xname,y=Lname,subtitle = Sname)
                } else {
                  
                  mean01 <- aggregate(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Scol]*Data1[,input$Scol2],data=Data1,FUN=mean)
                  names(mean01) <- c(names(Data1[input$Xcol]),names(Data1[input$Scol]),names(Data1[input$Scol2]),paste("mean of",names(Data1[input$Lcol])))
                  output$text541 <- renderPrint(mean01)
                  if(input$Using_hypothesis_testing2 ==1){
                    output$text542 <- renderPrint(summary(aov(Data1[,input$Lcol]~Data1[,input$Xcol]*Data1[,input$Scol]*Data1[,input$Scol2],data=Data)))
                    output$text543 <- renderPrint(paste("Data1[,input$Xcol] =" ,Xname,"   Data1[,input$Scol] =" ,Sname,"   Data1[,input$Scol2] =" ,Sname2))
                    output$text544 <- renderPrint(summary(aov(Data1[,input$Lcol]~Data1[,input$Xcol]+Data1[,input$Scol]+Data1[,input$Scol2],data=Data)))
                    output$text545 <- renderPrint(paste("Data1[,input$Xcol] =" ,Xname,"   Data1[,input$Scol] =" ,Sname,"   Data1[,input$Scol2] =" ,Sname2))
                  }
                  gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol],y=Data1[,input$Lcol])) + geom_boxplot() + facet_grid(Data1[,input$Scol]~Data1[,input$Scol2])+ labs(x=Xname,y=Lname,subtitle = paste("columns:",Sname," rows:",Sname2))
                  
                  
                }    
              } else {
                mean01 <- aggregate(Data1[,input$Lcol]~Data1[,input$Xcol],data=Data1,FUN=mean)
                names(mean01) <- c(names(Data1[input$Xcol]),paste("mean of",names(Data1[input$Lcol])))
                output$text517 <- renderPrint(mean01)
                sd01 <- aggregate(Data1[,input$Lcol]~Data1[,input$Xcol],data=Data1,FUN=sd)
                names(sd01) <- c(names(Data1[input$Xcol]),paste("sd of",names(Data1[input$Lcol])))
                output$text521 <- renderPrint(sd01)
                
                if(input$Using_hypothesis_testing2 ==1){
                  output$text505 <- renderPrint(summary(aov(Data1[,input$Lcol]~Data1[,input$Xcol],data=Data)))
                  output$text506 <- renderPrint(paste("Data1[,input$Xcol] =" ,Xname))
                  
                  output$text507 <- renderPrint(bartlett.test(formula=Data1[,input$Lcol]~Data1[,input$Xcol]))
                  output$text508 <- renderPrint(paste("Data1[,input$Lcol] =" ,Lname,"   Data1[,input$Xcol] =" ,Xname))
                }
                gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol],y=Data1[,input$Lcol])) + geom_boxplot() + labs(x=Xname,y=Lname)
              }
            } else {
              gplot <- ggplot(Data1, aes(y=Data1[,input$Lcol])) + geom_boxplot() + labs(y=Lname)
            }
            ggplotly(gplot)
            
          } else if(input$Gtype == "histgram"){
            if(input$Scol != 0){
              if(class(Data1[,input$Scol]) == "numeric" || class(Data1[,input$Scol]) == "integer"){Data1[,input$Scol] <- droplevels(cut(Data1[,input$Scol], breaks = input$NumericalToCategorcalS, include.lowest = TRUE))}
              
              if(input$Scol2 != 0){
                if(class(Data1[,input$Scol2]) == "numeric" || class(Data1[,input$Scol2]) == "integer"){Data1[,input$Scol2] <- droplevels(cut(Data1[,input$Scol2], breaks = input$NumericalToCategorcalS, include.lowest = TRUE))}
                
                  mean01 <- aggregate(Data1[,input$Lcol]~Data1[,input$Scol]*Data1[,input$Scol2],data=Data1,FUN=mean)
                  names(mean01) <- c(names(Data1[input$Scol]),names(Data1[input$Scol2]),paste("mean of",names(Data1[input$Lcol])))
                  output$text525 <- renderPrint(mean01)
                  
                  if(input$Using_hypothesis_testing1 ==1){
                    output$text509 <- renderPrint(summary(aov(Data1[,input$Lcol]~Data1[,input$Scol]*Data1[,input$Scol2],data=Data)))
                    output$text510 <- renderPrint(paste("Data1[,input$Scol] =" ,Sname,"   Data1[,input$Scol2] =" ,Sname2))
                    output$text511 <- renderPrint(summary(aov(Data1[,input$Lcol]~Data1[,input$Scol]+Data1[,input$Scol2],data=Data)))
                    output$text512 <- renderPrint(paste("Data1[,input$Scol] =" ,Sname,"   Data1[,input$Scol2] =" ,Sname2))
                  }
                  gplot <- ggplot(Data1, aes(x=Data1[,input$Lcol])) + geom_histogram() + facet_grid(Data1[,input$Scol]~Data1[,input$Scol2])+ labs(x=Lname,subtitle = paste("columns:",Sname," rows:",Sname2))
                } else {
                  
                  mean01 <- aggregate(Data1[,input$Lcol]~Data1[,input$Scol],data=Data1,FUN=mean)
                  names(mean01) <- c(names(Data1[input$Scol]),paste("mean of",names(Data1[input$Lcol])))
                  output$text523 <- renderPrint(mean01)
                  sd01 <- aggregate(Data1[,input$Lcol]~Data1[,input$Scol],data=Data1,FUN=sd)
                  names(sd01) <- c(names(Data1[input$Scol]),paste("sd of",names(Data1[input$Lcol])))
                  output$text524 <- renderPrint(sd01)
                  
                  if(input$Using_hypothesis_testing1 ==1){
                    output$text501 <- renderPrint(summary(aov(Data1[,input$Lcol]~Data1[,input$Scol],data=Data)))
                    output$text502 <- renderPrint(paste("Data1[,input$Scol] =" ,Sname))
                    
                    output$text503 <- renderPrint(bartlett.test(formula=Data1[,input$Lcol]~Data1[,input$Scol]))
                    output$text504 <- renderPrint(paste("Data1[,input$Lcol] =" ,Lname,"   Data1[,input$Scol] =" ,Sname))
                  }
                  gplot <- ggplot(Data1, aes(x=Data1[,input$Lcol])) + geom_histogram() + facet_grid(Data1[,input$Scol]~.)+ labs(x=Lname,subtitle = Sname)
                }
            } else {
              M <- mean(Data1[,input$Lcol])
              n <- nrow(Data1)
              t <- -qt((1-input$Prediction_Interval_Probability2)/2, n-1)
              V <- var(Data1[,input$Lcol])
              Upper <- M + t * sqrt(V * (1 + 1/n))
              Lower <- M - t * sqrt(V * (1 + 1/n))
              output$text546<- renderPrint(cbind(Upper,Lower))
              gplot <- ggplot(Data1, aes(x=Data1[,input$Lcol])) + geom_histogram() + labs(x=Lname)
            }
            ggplotly(gplot)
              
          } else if(input$Gtype == "bar"){
            if(class(Data1[,input$Lcol]) == "numeric" || class(Data1[,input$Lcol]) == "integer"){Data1[,input$Lcol] <- droplevels(cut(Data1[,input$Lcol], breaks = input$NumericalToCategorcalS, include.lowest = TRUE))}
            if(input$Scol != 0){
              if(class(Data1[,input$Scol]) == "numeric" || class(Data1[,input$Scol]) == "integer"){Data1[,input$Scol] <- droplevels(cut(Data1[,input$Scol], breaks = input$NumericalToCategorcalS, include.lowest = TRUE))}
                if(input$Scol2 == 0){
                  Data2 <- cbind(Data1[input$Xcol],Data1[input$Scol])
                  Data3 <- count(group_by(Data2,Data2[,1:2],.drop=FALSE)) 
                  output$text536 <- renderPrint(anova(step(glm(n~.^2, data=Data3,family=poisson))))
                  Data4 <- tidyr::spread(Data3, key=colnames(Data3[1]), value ="n")
                  Data4[1] <- NULL
                  Data4[is.na(Data4)] <- 0
                  output$text5362 <- renderPrint(chisq.test(Data4))
                  gplot <- ggplot(Data1, aes(x=Data1[,input$Lcol])) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1) + facet_grid(.~Data1[,input$Scol])+ labs(x=Lname,subtitle = Sname)
                } else {
                  if(class(Data1[,input$Scol2]) == "numeric" || class(Data1[,input$Scol2]) == "integer"){Data1[,input$Scol2] <- droplevels(cut(Data1[,input$Scol2], breaks = input$NumericalToCategorcalS, include.lowest = TRUE))}
                  Data2 <- cbind(Data1[input$Lcol],Data1[input$Scol],Data1[input$Scol2])
                  
                  Data3 <- count(group_by(Data2,Data2[,1:3],.drop=FALSE))
                  output$text536 <- renderPrint(anova(step(glm(n~.^2, data=Data3,family=poisson))))
                  
                  gplot <- ggplot(Data1, aes(x=Data1[,input$Lcol])) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1) + facet_grid(Data1[,input$Scol2]~Data1[,input$Scol])+ labs(x=Lname,subtitle = paste("columns:",Sname," rows:",Sname2))
                }
              } else {
                gplot <- ggplot(Data1, aes(x=Data1[,input$Lcol])) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(x=Lname)
              }
            ggplotly(gplot)
          }else if(input$Gtype == "3D-scatter"){
            if(input$Xcol12 > 0){Xname12 <- names(Data1[input$Xcol12])} else {Xname12 <- "None"}
            
            if(input$Ycol12 > 0){Yname12<-names(Data1[input$Ycol12])} else {Yname12<-"None"}
            if(input$Zcol12 > 0){Zname12<-names(Data1[input$Zcol12])} else {Zname12<-"None"}
            if(input$Ccol12 > 0){Cnameas<-names(Data1[input$Ccol12])} else {Cname12<-"None"}
            if(input$Ccol12 != 0){
              fig <- plot_ly(Data, x=~Data1[,input$Xcol12], y=~Data1[,input$Ycol12], z=~Data1[,input$Zcol12], type = 'scatter3d', color=~Data1[,input$Ccol12], marker = list(size = 3))
              
            } else {
              fig <- plot_ly(Data, x=~Data1[,input$Xcol12], y=~Data1[,input$Ycol12], z=~Data1[,input$Zcol12], type = 'scatter3d', marker = list(size = 3))
            }
            fig <- fig %>% layout(scene = list(xaxis = list(title = Xname12),
                                               yaxis = list(title = Yname12),
                                               zaxis = list(title = Zname12)))
            fig
          }
        }
      }  
    }
  })
  
  
  output$Data_Output2 <- renderDataTable({
    if(input$analysis == "Similarity_of_Variables_and_Categories1"){
      if(input$Similarity_of_Variables_and_Categories == "Among_all_columns1"){
        if(input$Among_all_columns == "Variable_Network1"){
          if(input$Variable_Network == 'Correlation_Network1') {
      
            req(input$file1)
            
            if(input$sep2 == "Separator_Comma"){sep <- ","}
            if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
            if(input$sep2 == "Separator_Tab"){sep <- "\t"}
            Data <- read.csv(input$file1$datapath, header=T,sep = sep)
            if(input$DoNotUseFirst == 1){
              Data[,1] <- NULL
            }
            
            library(dummies)
            library(igraph)
            library(networkD3)
            library(tidyr)
            Data1 <- dummy.data.frame(Data)
            DataM <- as.matrix(Data1)
            GM1 <- cor(DataM)
            diag(GM1) <- 0
            GM2 <- abs(GM1)
            GM2[GM2<input$correlation_limit] <- 0
            GM3 <- GM2*10
            GM4 <- graph.adjacency(GM3,weighted=T, mode = "undirected")
            
            
            GM1_DF <- as.data.frame(GM1)
            Name <- row.names(GM1_DF)
            GM1_DF2 <- cbind(Name, GM1_DF)
            Data_Output1 <- tidyr::gather(GM1_DF2, key="Name2", value = corr, -Name)
            ABS_corr <- abs(Data_Output1[,3])
            Data_Output1 <- cbind(Data_Output1, ABS_corr)
            Data_Output1 <- Data_Output1[order(Data_Output1$ABS_corr, decreasing=T),]
            output$downloadData2 <- downloadHandler(
              filename = function() {
                paste("Correlation_Network_data", ".csv", sep = "")
              },
              content = function(file) {
                write.csv(Data_Output1, file, row.names = FALSE)
              }
            )
            if(input$network_library1 == 'igraph'){
              output$plot06 <- renderPlot(plot(GM4, edge.width=E(GM4)$weight))
            }else{
              DM.g2 <- data.frame(as_edgelist(GM4))
              output$plot06b <- renderSimpleNetwork(simpleNetwork(DM.g2, fontSize = 14, nodeColour = "0000A2", opacity = 1, fontFamily = "Meiryo UI") )
            }
            Data_Output1
            
          }
        }
      }  
    }
  })
  
  output$Data_Output3 <- renderDataTable({
    if(input$analysis == "Similarity_of_Variables_and_Categories1"){
      if(input$Similarity_of_Variables_and_Categories == "Among_all_columns1"){
        if(input$Among_all_columns == "Variable_Network1"){
          if(input$Variable_Network == 'Graphical_Lasso1') {
            
            req(input$file1)
            
            if(input$sep2 == "Separator_Comma"){sep <- ","}
            if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
            if(input$sep2 == "Separator_Tab"){sep <- "\t"}
            Data <- read.csv(input$file1$datapath, header=T,sep = sep)
            if(input$DoNotUseFirst == 1){
              Data[,1] <- NULL
            }
            
            library(dummies)
            library(glasso)
            library(igraph)
            Data1 <- dummy.data.frame(Data)
            DataM <- as.matrix(Data1)
            COR <- cor(DataM)
            GM1 <- glasso(COR,input$RHO)$wi
            rownames(GM1) <- rownames(COR)
            colnames(GM1) <- colnames(COR)
            diag(GM1) <- 0
            GM2 <- abs(GM1)
            GM2[GM2<input$inverse_covarianve_limit] <- 0
            GM3 <- GM2*10
            GM4 <- graph.adjacency(GM3,weighted=T, mode = "undirected")
            
            
            GM1_DF <- as.data.frame(GM1)
            Name <- row.names(GM1_DF)
            GM1_DF2 <- cbind(Name, GM1_DF)
            Data_Output1 <- tidyr::gather(GM1_DF2, key="Name2", value = wi, -Name)
            ABS_wi <- abs(Data_Output1[,3])
            Data_Output1 <- cbind(Data_Output1, ABS_wi)
            Data_Output1 <- Data_Output1[order(Data_Output1$ABS_wi, decreasing=T),]
            
            output$downloadData3 <- downloadHandler(
              filename = function() {
                paste("Graphical_Lasso_data", ".csv", sep = "")
              },
              content = function(file) {
                write.csv(Data_Output1, file, row.names = FALSE)
              }
            )
            
            
            
            
            if(input$network_library2 == 'igraph'){
              output$plot07 <- renderPlot(plot(GM4, edge.width=E(GM4)$weight))
            }else{
              DM.g2 <- data.frame(as_edgelist(GM4))
              output$plot07b <- renderSimpleNetwork(simpleNetwork(DM.g2, fontSize = 14, nodeColour = "0000A2", opacity = 1, fontFamily = "Meiryo UI") )
            }
            Data_Output1
          }
        }
      }
    }
  })
  
  
  output$Data_Output4 <- renderDataTable({
    if(input$analysis == "Similarity_of_Variables_and_Categories1"){
      if(input$Similarity_of_Variables_and_Categories == "Among_all_columns1"){
        if(input$Among_all_columns == "Variable_Network1"){
          if(input$Variable_Network == 'Cramer_Network1') {
      
            req(input$file1)
            
            if(input$sep2 == "Separator_Comma"){sep <- ","}
            if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
            if(input$sep2 == "Separator_Tab"){sep <- "\t"}
            Data <- read.csv(input$file1$datapath, header=T,sep = sep)
            if(input$DoNotUseFirst == 1){
              Data[,1] <- NULL
            }
            
            
            library(igraph)
            library(vcd)
            
            Data1 <- Data
            n <- ncol(Data1)
            for (i in 1:n) {
              if (class(Data1[,i]) == "numeric" || class(Data1[,i]) == "integer") {
                Data1[,i] <- droplevels(cut(Data1[,i], breaks = input$NumericalToCategorcalC, include.lowest = TRUE))
              }
            }
            GM2 <- matrix(0,nrow=n,ncol=n)
            for (i in 1:(n-1)) {
              for (j in (i+1):n) {
                cross<-xtabs(~Data1[,i]+Data1[,j],data=Data1)
                res<-assocstats(cross)
                cramer_v<-res$cramer
                GM2[i,j] <- cramer_v
                GM2[j,i] <- cramer_v
              }
            }
            rownames(GM2)<-colnames(Data1)
            colnames(GM2)<-colnames(Data1)
            GM_Out <- GM2
            GM2[GM2<input$association_limit] <- 0
            GM3 <- GM2*10
            GM4 <- graph.adjacency(GM3,weighted=T, mode = "undirected")
            
            
            
            
            GM1_DF <- as.data.frame(GM_Out)
            Name <- row.names(GM1_DF)
            GM1_DF2 <- cbind(Name, GM1_DF)
            Data_Output1 <- tidyr::gather(GM1_DF2, key="Name2", value = cramer_v, -Name)
            
            Data_Output1 <- Data_Output1[order(Data_Output1$cramer_v, decreasing=T),]
            output$downloadData4 <- downloadHandler(
              filename = function() {
                paste("Cramer_Network_data", ".csv", sep = "")
              },
              content = function(file) {
                write.csv(Data_Output1, file, row.names = FALSE)
              }
            )
            if(input$network_library3 == 'igraph'){
              output$plot09 <- renderPlot(plot(GM4, edge.width=E(GM4)$weight))
            }else{
              DM.g2 <- data.frame(as_edgelist(GM4))
              output$plot09b <- renderSimpleNetwork(simpleNetwork(DM.g2, fontSize = 14, nodeColour = "0000A2", opacity = 1, fontFamily = "Meiryo UI") )
            }
            Data_Output1
            
          }
        }
      }
    }
  })
  
  output$plot16 <- renderPlot({
    if(input$analysis == "Similarity_of_Variables_and_Categories1"){
      if(input$Similarity_of_Variables_and_Categories == "Among_all_columns1"){
        if(input$Among_all_columns == "Variable_Network1"){
          if(input$Variable_Network == 'Bayesian_Network1') {
            
            req(input$file1)
            
            if(input$sep2 == "Separator_Comma"){sep <- ","}
            if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
            if(input$sep2 == "Separator_Tab"){sep <- "\t"}
            Data <- read.csv(input$file1$datapath, header=T,sep = sep)
            if(input$DoNotUseFirst == 1){
              Data[,1] <- NULL
            }
            
            library(bnlearn)
            library(MASS)
            
            if (input$NumericalToCategorcalB > 0){
              for (i in 1:ncol(Data)) {
                if (class(Data[,i]) == "numeric" || class(Data[,i]) == "integer") {
                  Data[,i] <- droplevels(cut(Data[,i], breaks = input$NumericalToCategorcalB, include.lowest = TRUE))
                }
              }
            }
            if(input$Structure_Learning == "stable_version1"){
              BNstructure <- pc.stable(Data)
            } else if(input$Structure_Learning == "Grow_Shrink1"){
              BNstructure <- gs(Data)
            } else if(input$Structure_Learning == "Incremental_Association_Markov_Blanket1"){
              BNstructure <- iamb(Data)
            } else if(input$Structure_Learning == "Fast_Incremental_Association1"){
              BNstructure <- fast.iamb(Data)
            } else if(input$Structure_Learning == "Interleaved_Incremental_Association1"){
              BNstructure <- inter.iamb(Data)
            } else if(input$Structure_Learning == "Incremental_Association_with_FDR_Correction1"){
              BNstructure <- iamb.fdr(Data)
            } else if(input$Structure_Learning == "Max_Min_Parents_and_Children1"){
              BNstructure <- mmpc(Data)
            } else if(input$Structure_Learning == "Semi_Interleaved_Hiton_PC1"){
              BNstructure <- si.hiton.pc(Data)
            } else if(input$Structure_Learning == "Hybrid_Parents_and_Children1"){
              BNstructure <- hpc(Data)
            } else if(input$Structure_Learning == "Hill_Climbing1"){
              BNstructure <- hc(Data)
            } else if(input$Structure_Learning == "Tabu_Search1"){
              BNstructure <- tabu(Data)
            } else if(input$Structure_Learning == "Max_Min_Hill_Climbing1"){
              BNstructure <- mmhc(Data)
            } else if(input$Structure_Learning == "Hybrid_HPC1"){
              BNstructure <- h2pc(Data)
            } else if(input$Structure_Learning == "General_2_Phase_Restricted_Maximization1"){
              BNstructure <- rsmax2(Data)
            } else if(input$Structure_Learning == "Chow_Liu1"){
              BNstructure <- chow.liu(Data)
            } else if(input$Structure_Learning == "ARACNE1"){
              BNstructure <- aracne(Data)
            }
            
            plot(BNstructure,main = input$Structure_Learning)
          }
        }
      }  
    }
  })
  
  #output$plot17 <- renderPlot({
  #  if(input$analysis == "Similarity_of_Variables_and_Categories1"){
  #    if(input$Similarity_of_Variables_and_Categories == "Among_all_columns1"){
  #      if(input$Among_all_columns == "Variable_Network1"){
  #        if(input$Variable_Network == 'LiNGAM1') {
  #          
  #          req(input$file1)
  #          
  #          if(input$sep2 == "Separator_Comma"){sep <- ","}
  #          if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
  #          if(input$sep2 == "Separator_Tab"){sep <- "\t"}
  #          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
  #             if(input$DoNotUseFirst == 1){
  #               Data[,1] <- NULL
  #             }
  #          library(pcalg)
  #          library(igraph)
  #          library(dummies)
  #          library(tidyr)
  #          for (i in 1:ncol(Data)) {
  #            if (class(Data[,i]) == "numeric" || class(Data[,i]) == "integer") {
  #              Data[,i] <- (Data[,i] - mean(Data[,i]))/sd(Data[,i])
  #            }
  #          }
  #          Str <- lingam(Data)$Bpruned
  #          rownames(Str) <- colnames(Data)
  #          colnames(Str) <- colnames(Data)
  #          GM2 <- abs(Str)
  #          GM3 <- t(GM2*5)
  #          GM4 <- graph.adjacency(GM3,weighted=T, mode = "directed")
  #          
            
            
  #          GM1_DF <- as.data.frame(Str)
  #          Name <- row.names(GM1_DF)
  #          GM1_DF2 <- cbind(Name, GM1_DF)
  #          Data_Output1 <- tidyr::gather(GM1_DF2, key="Name2", value = Bpruned, -Name)
  #          ABS_Bpruned <- abs(Data_Output1[,3])
  #          Data_Output1 <- cbind(Data_Output1, ABS_Bpruned)
  #          Data_Output1 <- Data_Output1[order(Data_Output1$ABS_Bpruned, decreasing=T),]
  #          output$Data_Output17 <- renderDataTable(Data_Output1)
            
  #          output$downloadData17 <- downloadHandler(
  #            filename = function() {
  #              paste("LiNGAM_data", ".csv", sep = "")
  #            },
  #            content = function(file) {
  #              write.csv(Data_Output1, file, row.names = FALSE)
  #            }
  #          )
            
  #          plot(GM4, edge.width=E(GM4)$weight)
  #          
  #        }
  #      }
  #    }  
  #  }
  #})
  
  output$ap231 <- renderDataTable({
    if(input$analysis == "Similarity_of_Variables_and_Categories1"){
      if(input$Similarity_of_Variables_and_Categories == "Among_all_columns1"){
        if(input$Among_all_columns == "Variable_Network1"){
          if(input$Variable_Network == 'Association1') {
      
            req(input$file1)
            
            if(input$sep2 == "Separator_Comma"){sep <- ","}
            if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
            if(input$sep2 == "Separator_Tab"){sep <- "\t"}
            Data <- read.csv(input$file1$datapath, header=T,sep = sep)
            if(input$DoNotUseFirst == 1){
              Data[,1] <- NULL
            }
            
            library(arules)
            library(dummies)
            library(ggplot2)
            library(igraph) 
            
            for (i in 1:ncol(Data)) {
              if (class(Data[,i]) == "numeric" || class(Data[,i]) == "integer"){
                Data[,i] <- droplevels(cut(Data[,i], breaks = input$NumericalToCategorcalA, include.lowest = TRUE))
              }
            }
            Data <- dummy.data.frame(Data)
            Data3 <- as(Data, "matrix")
            Data4 <- as(Data3, "transactions")
            ap <- apriori(Data4, parameter = list(support = 5/nrow(Data), maxlen = 2, minlen = 2))
            ap_inspect <- inspect(ap)
            ap_inspect$set <- paste(ap_inspect$lhs,"->",ap_inspect$rhs)
            
            ap21 <- head(ap_inspect[order(ap_inspect$support, decreasing=T),], input$association_limit2)
            output$ap211 <- renderDataTable(ap21[,1:8])
            output$downloadData211 <- downloadHandler(
              filename = function() {
                paste("Association_support_data", ".csv", sep = "")
              },
              content = function(file) {
                write.csv(ap21, file, row.names = FALSE)
              }
            )
            ap22 <- head(ap_inspect[order(ap_inspect$confidence, decreasing=T),], input$association_limit2)
            output$ap221 <- renderDataTable(ap22[,1:8])
            output$downloadData221 <- downloadHandler(
              filename = function() {
                paste("Association_confidence_data", ".csv", sep = "")
              },
              content = function(file) {
                write.csv(ap22, file, row.names = FALSE)
              }
            )
            ap23 <- (head(ap_inspect[order(ap_inspect$lift, decreasing=T),], input$association_limit2))
            output$ap231 <- renderDataTable(ap23[,1:8])
            output$downloadData231 <- downloadHandler(
              filename = function() {
                paste("Association_lift_data", ".csv", sep = "")
              },
              content = function(file) {
                write.csv(ap23, file, row.names = FALSE)
              }
            )
            
            
            #ap32<- graph.data.frame(ap22[,c(1,3)])
            
            ap31<- graph.data.frame(ap21[,c(1,3)], directed = F)
            ap32<- graph.data.frame(ap22[,c(1,3)])
            ap33<- graph.data.frame(ap23[,c(1,3)])
            output$plotap31a <- renderPlot(plot(ap31))
            output$plotap32a <- renderPlot(plot(ap32))
            output$plotap33a <- renderPlot(plot(ap33))
            #plot(ap32)
            
            
            
            
            if(input$network_library4 == 'igraph'){
              output$plotap31a <- renderPlot(plot(ap31))
              output$plotap32a <- renderPlot(plot(ap32))
              output$plotap33a <- renderPlot(plot(ap33))
            }else{
              
              #DM.g21 <- data.frame(as_edgelist(ap31))
              DM.g21 <- igraph_to_networkD3(ap31)
              Linksap <- DM.g21$links
              Linksap$from <- Linksap$source
              Linksap$to <- Linksap$target
              #Linksap$arrows <- 'from'
              Linksap$value <- 1
              Nodesap <- DM.g21$nodes
              Nodesap$label <- Nodesap$name
              Nodesap$id <- as.numeric(row.names(Nodesap))-1
              output$plotap31b <- renderVisNetwork(visNetwork(Nodesap,Linksap))
              #output$plotap31b <- renderSimpleNetwork(simpleNetwork(DM.g21, fontSize = 14, nodeColour = "0000A2", opacity = 1, fontFamily = "Meiryo UI") )
              
              DM.g22 <- igraph_to_networkD3(ap32)
              Linksap <- DM.g22$links
              Linksap$from <- Linksap$source
              Linksap$to <- Linksap$target
              Linksap$arrows <- 'to'
              Linksap$value <- 1
              Nodesap <- DM.g22$nodes
              Nodesap$label <- Nodesap$name
              Nodesap$id <- as.numeric(row.names(Nodesap))-1
              output$plotap32b <- renderVisNetwork(visNetwork(Nodesap,Linksap))
              
              DM.g23 <- igraph_to_networkD3(ap33)
              Linksap <- DM.g22$links
              Linksap$from <- Linksap$source
              Linksap$to <- Linksap$target
              Linksap$arrows <- 'to'
              Linksap$value <- 1
              Nodesap <- DM.g22$nodes
              Nodesap$label <- Nodesap$name
              Nodesap$id <- as.numeric(row.names(Nodesap))-1
              output$plotap33b <- renderVisNetwork(visNetwork(Nodesap,Linksap))
            }
            
            ap23[,1:8]
          }
        }
      }
    }
  })
  
  
  
  output$plot08 <- renderPlotly({
    if(input$analysis == "Similarity_of_Variables_and_Categories1"){
      if(input$Similarity_of_Variables_and_Categories == "Among_all_columns1"){
        if(input$Among_all_columns == "Using_MDS1"){
          if(input$Using_MDS == 'PCA_MDS1') {
      
            req(input$file1)
            
            if(input$sep2 == "Separator_Comma"){sep <- ","}
            if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
            if(input$sep2 == "Separator_Tab"){sep <- "\t"}
            Data <- read.csv(input$file1$datapath, header=T,sep = sep)
            if(input$DoNotUseFirst == 1){
              Data[,1] <- NULL
            }
            
            library(dummies)
            library(ggplot2)
            library(MASS)
            Data1 <- dummy.data.frame(Data)
            pc <- prcomp(Data1, scale=TRUE,tol=0.01)
            output$text410 <- renderPrint(summary(pc))
            pc2 <- sweep(pc$rotation, MARGIN=2, pc$sdev, FUN="*")
            Data11 <- pc2
            Data11_dist <- dist(Data11)
            sn <- sammon(Data11_dist)
            Data2 <- sn$points
            name1 <-  row.names(Data11)
            Data2 <- cbind.data.frame(Data2 ,name1)
            ggplotly(ggplot(Data2, aes(x=Data2[,1], y=Data2[,2],label=name1)) + geom_text()+ labs(y="axis2",x="axis1"))
          }
        }
      }
    }
  })
  
  
  output$plot13 <- renderPlotly({
    if(input$analysis == "Similarity_of_Variables_and_Categories1"){
      if(input$Similarity_of_Variables_and_Categories == "Among_all_columns1"){
        if(input$Among_all_columns == "Using_MDS1"){
          if(input$Using_MDS == 'Correspondence_MDS_Categories1') {
      
            req(input$file1)
            
            if(input$sep2 == "Separator_Comma"){sep <- ","}
            if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
            if(input$sep2 == "Separator_Tab"){sep <- "\t"}
            Data <- read.csv(input$file1$datapath, header=T,sep = sep)
            if(input$DoNotUseFirst == 1){
              Data[,1] <- NULL
            }
            
            
            library(dummies)
            library(MASS)
            library(ggplot2)
            for (i in 1:ncol(Data)) {
              if (class(Data[,i]) == "numeric"|| class(Data[,i]) == "integer"){
                Data[,i] <- droplevels(cut(Data[,i], breaks = input$NumericalToCategorcalU, include.lowest = TRUE))
              }
            }
            Data_dmy <- dummy.data.frame(Data)
            pc <- corresp(Data_dmy,nf=min(ncol(Data),nrow(Data)))
            pc1 <- data.frame(pc$cscore)
            pc1 <- transform(pc1 ,name1 = rownames(pc1))
            #output$text409 <- renderPrint(summary(pc))
            ei1 <- round(pc$cor^2/sum(pc$cor^2),2)
            output$text131 <- renderText(ei1)
            for (i in 1: length(ei1)){
              if(ei1[i] > 0.1){
                n1 <- i
              }
            }
            output$text132 <- renderText(n1)
            Data11 <- pc1[,1:n1]
            Data11_dist <- dist(Data11)
            sn <- sammon(Data11_dist)
            output <- sn$points
            Data2 <- cbind(output, pc1)
            ggplotly(ggplot(Data2, aes(x=Data2[,1], y=Data2[,2],label=name1)) + geom_text()+ labs(y="axis2",x="axis1"))
          }
        }
      }
    }
  })
  
  
  output$text406 <- renderPrint({
    if(input$analysis == "Similarity_of_Variables_and_Categories1"){
      if(input$Similarity_of_Variables_and_Categories == "Among_all_columns1"){
        if(input$Among_all_columns == "Using_MDS1"){
          if(input$Using_MDS == "Factor_Analysis1"){
            
            req(input$file1)
            
            if(input$sep2 == "Separator_Comma"){sep <- ","}
            if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
            if(input$sep2 == "Separator_Tab"){sep <- "\t"}
            Data <- read.csv(input$file1$datapath, header=T,sep = sep)
            if(input$DoNotUseFirst == 1){
              Data[,1] <- NULL
            }
            
            
            library(psych)
            library(GPArotation)
            library(heatmaply)
            library(fastDummies) 
            library(ggplot2)
            
            
            Check_variable <- "0"
            for (i in 1:ncol(Data)) {
              if (class(Data[,i]) != "numeric") {
                if (class(Data[,i]) != "integer") {
                  Check_variable <- "1"
                }
              }  
            }
            if(Check_variable == "1"){
              Data1 <- dummy_cols(Data,remove_first_dummy = TRUE,remove_selected_columns = TRUE)
            } else {
              Data1 <- Data
            }
            
            fa_result <- fa(Data1, nfactors = input$Factors, fm = "ml", rotate = input$Factor_Rotation)
            output$plot406 <- renderPlotly(heatmaply(fa_result$loadings, scale="none"))
            
            fa_loadings <- fa_result$loadings[,1:input$Factors]
            Data11 <- as.data.frame(fa_loadings)
            Data11_dist <- dist(Data11)
            sn <- sammon(Data11_dist)
            Data2 <- sn$points
            name1 <-  row.names(Data11)
            Data2 <- cbind.data.frame(Data2 ,name1)
            output$plot408 <- renderPlotly(ggplotly(ggplot(Data2, aes(x=Data2[,1], y=Data2[,2],label=name1)) + geom_text()+ labs(y="axis2",x="axis1")))
            fa_result$loadings
          }
        }
      }
    }
  })
  
  output$text404 <- renderPrint({
    if(input$analysis == "Similarity_of_Variables_and_Categories1"){
      if(input$Similarity_of_Variables_and_Categories == "Among_all_columns1"){
        if(input$Among_all_columns == "Log_Linear1"){
            
          req(input$file1)
          
          if(input$sep2 == "Separator_Comma"){sep <- ","}
          if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
          if(input$sep2 == "Separator_Tab"){sep <- "\t"}
          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
          if(input$DoNotUseFirst == 1){
            Data[,1] <- NULL
          }
          
          
          
          library(dplyr)
          library(MASS)
          library(ggplot2)
          
          
          Data1 <- Data
          nc <- ncol(Data1)
          for (i in 1:nc) {
            if (class(Data1[,i]) == "numeric" || class(Data1[,i]) == "integer") {
              Data1[,i] <- droplevels(cut(Data1[,i], breaks = input$NumericalToCategorcalL, include.lowest = TRUE))
            }
          }
          Data2 <- count(group_by(Data1,Data1[,1:nc],.drop=FALSE))
          anova(step(glm(n~.^2, data=Data2,family=poisson)))
        }
      }
    }
  })
  
  
  output$plot05 <- renderPlotly({
    if(input$analysis == "Similarity_of_Variables_and_Categories1"){
      if(input$Similarity_of_Variables_and_Categories == "Between_label_column_and_others1"){
        if(input$Between_label_column_and_others == "Scatter_plot1"){
      
          req(input$file1)
          
          if(input$sep2 == "Separator_Comma"){sep <- ","}
          if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
          if(input$sep2 == "Separator_Tab"){sep <- "\t"}
          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
          if(input$DoNotUseFirst == 1){
            Data[,1] <- NULL
          }
          
          library(dummies)
          library(ggplot2)
          library(tidyr)
          Data1 <- Data
          Y <- names(Data1[input$Label_column])
          Ydata <- Data1[,input$Label_column]
          Data1[,input$Label_column] <- NULL
          Data2 <- dummy.data.frame(Data1)
          Data4 <- cbind(Ydata, Data2)
          Data_long <- tidyr::gather(Data4, key="X", value = Xs, -Ydata)
          if(class(Ydata) == "numeric") {
            gplot <- ggplot(Data_long, aes(x=Xs,y=Data_long[,1])) + geom_point() + facet_wrap(~X,scales="free")+ labs(y=Y)
          } else {
            gplot <- ggplot(Data_long, aes(x=Data_long[,1],y=Xs)) + geom_jitter(size=1, position=position_jitter(0.1)) + facet_wrap(~X,scales="free")+ labs(x=Y)
          }
          ggplotly(gplot)
        }
      }
    }
  })
  
  
  output$text113 <- renderPrint({
    if(input$analysis == "Similarity_of_Variables_and_Categories1"){
      if(input$Similarity_of_Variables_and_Categories == "Between_label_column_and_others1"){
        if(input$Between_label_column_and_others == "GLMM1"){
      
          req(input$file1)
          
          if(input$sep2 == "Separator_Comma"){sep <- ","}
          if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
          if(input$sep2 == "Separator_Tab"){sep <- "\t"}
          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
          if(input$DoNotUseFirst == 1){
            Data[,1] <- NULL
          }
          
          library(MASS)
          Data1 <- Data
          Y <- names(Data1[input$Label_column])
          Ydata <- Data1[,input$Label_column]
          Data1[,input$Label_column]<-NULL
          if(input$family_link == "gaussian_identity"){
            anova(step(glm(Ydata~.^2, data=Data1, family= gaussian(link = "identity"))))
          } else if(input$family_link == "poisson_log"){
            anova(step(glm(Ydata~.^2, data=Data1, family= poisson(link = "log"))))
          } else if(input$family_link == "binomial_logit"){
            anova(step(glm(Ydata~.^2, data=Data1, family= binomial(link = "logit"))))
          } else if(input$family_link == "binomial_probit"){
            anova(step(glm(Ydata~.^2, data=Data1, family= binomial(link = "probit"))))
          } 
          
        }
      }
    }
  })
  
  
  output$text114 <- renderPrint({
    if(input$analysis == "Similarity_of_Variables_and_Categories1"){
      if(input$Similarity_of_Variables_and_Categories == "Between_label_column_and_others1"){
        if(input$Between_label_column_and_others == "PCRA1"){
          
          req(input$file1)
          
          if(input$sep2 == "Separator_Comma"){sep <- ","}
          if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
          if(input$sep2 == "Separator_Tab"){sep <- "\t"}
          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
          if(input$DoNotUseFirst == 1){
            Data[,1] <- NULL
          }
          
          Data1 <- Data
          Ydata <- Data[,input$Label_column]
          Data1[,input$Label_column]<-NULL
          pc <- prcomp(Data1, scale=TRUE, tol=0.01)
          pcd <- as.data.frame(pc$x)
          pcr <- lm(Ydata ~ . ,pcd)
          
          library(MASS)
          pc2 <- sweep(pc$rotation, MARGIN=2, pc$sdev, FUN="*") 
          
          Data11 <- pc2
          Data11_dist <- dist(Data11)
          library(igraph) 
          library(sigmoid) 
          Data1p = Data11
          colnames(Data1p) = paste(colnames(Data1p),"+",sep="")
          DM.matp = apply(Data1p,c(1,2),relu)
          Data1m = -Data11
          colnames(Data1m) = paste(colnames(Data1m),"-",sep="")
          DM.matm = apply(Data1m,c(1,2),relu)
          DM.mat =cbind(DM.matp,DM.matm)
          DM.mat <- DM.mat / max(DM.mat) * 3
          DM.mat[DM.mat < 1] <- 0 
          DM.g<-graph_from_incidence_matrix(DM.mat,weighted=T) 
          V(DM.g)$color <- c("steel blue", "orange")[V(DM.g)$type+1] 
          V(DM.g)$shape <- c("square", "circle")[V(DM.g)$type+1] 
          output$plot18<-renderPlot(plot(DM.g, edge.width=E(DM.g)$weight))
          
          summary(pcr) 
        }
      }
    }
  })
  
  output$plot15 <- renderPlot({
    if(input$analysis == "Similarity_of_Variables_and_Categories1"){
      if(input$Similarity_of_Variables_and_Categories == "Between_label_column_and_others1"){
        if(input$Between_label_column_and_others == "Decision_Tree1"){
          if (input$Decision_Tree != "C50_based_RandomForest1"){
          
            req(input$file1)
            
            if(input$sep2 == "Separator_Comma"){sep <- ","}
            if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
            if(input$sep2 == "Separator_Tab"){sep <- "\t"}
            Data <- read.csv(input$file1$datapath, header=T,sep = sep)
            DataR <- Data
            if(input$DoNotUseFirst == 1){
              Data[,1] <- NULL
            }
            library(C50)
            library(partykit)
            library(randomForest) 
            #if (input$Numerical_to_Categorical == "Numerical_to_Categorical1") {
            #  for (i in 2:ncol(Data)) {
            #    if (class(Data[,i]) == "numeric" || class(Data[,i]) == "integer"){
            #      Data[,i] <- as.factor(droplevels(cut(Data[,i], breaks = 3,include.lowest = TRUE)))
            #    }
            #  }
            #}
            Data[,input$Label_column] <- as.factor(Data[,input$Label_column])
            for (i in 1:ncol(Data)) {
              if (class(Data[,i]) == "logical") {
                Data[,i] <- as.factor(Data[,i])
              }
            }
            
            Ydata <- Data[,input$Label_column]
            Data[,input$Label_column]<-NULL
            
            
            if (input$Decision_Tree == "C501"){
                if(input$Use_minCases == 1){
                  treeModel <- C5.0(Ydata ~ ., data = Data,control = C5.0Control(minCase = floor(nrow(Data)*input$Ratio_of_columns)))
                } else {
                  treeModel <- C5.0(Ydata ~ ., data = Data)
                }
              
              plot(as.party(treeModel))
            } else {
              treeModel <- randomForest(Ydata ~ ., datset = Data, ntree = 30)
              varImpPlot(treeModel) 
            } 
          }
        }
      }
    }
  })
  
  output$plotDT05 <- renderPlot({
    if(input$analysis == "Similarity_of_Variables_and_Categories1"){
      if(input$Similarity_of_Variables_and_Categories == "Between_label_column_and_others1"){
        if(input$Between_label_column_and_others == "Decision_Tree1"){
          if (input$Decision_Tree == "C50_based_RandomForest1"){
          
            req(input$file1)
            
            if(input$sep2 == "Separator_Comma"){sep <- ","}
            if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
            if(input$sep2 == "Separator_Tab"){sep <- "\t"}
            Data <- read.csv(input$file1$datapath, header=T,sep = sep)
            DataR <- Data
            if(input$DoNotUseFirst == 1){
              Data[,1] <- NULL
            }
            library(C50)
            library(partykit)
            library(randomForest) 
            #if (input$Numerical_to_Categorical == "Numerical_to_Categorical1") {
            #  for (i in 2:ncol(Data)) {
            #    if (class(Data[,i]) == "numeric" || class(Data[,i]) == "integer"){
            #      Data[,i] <- as.factor(droplevels(cut(Data[,i], breaks = 3,include.lowest = TRUE)))
            #    }
            #  }
            #}
            Data[,input$Label_column] <- as.factor(Data[,input$Label_column])
            for (i in 1:ncol(Data)) {
              if (class(Data[,i]) == "logical") {
                Data[,i] <- as.factor(Data[,i])
              }
            }
            
            Ydata <- Data[,input$Label_column]
            Data[,input$Label_column]<-NULL
            
            
            ncolMax <- ncol(Data)
            nrowMax <- nrow(Data)
            
            output$plotDT01 <- renderPlot({
              if(input$Use_sampling_variables == 1){
                DataX <- Data[,floor(runif (floor(sqrt(ncolMax)), min=1,max=ncolMax))]
              } else {
                DataX <- Data
              }
              Data1 <- transform(DataX, Y = Ydata)
              if(input$Use_sampling_samples == 1){
                Data2 <- Data1[floor(runif (floor(sqrt(nrowMax)), min=1,max=nrowMax)),]
              } else {
                Data2 <- Data1
              }
              if(input$Use_minCases == 1){
                treeModel <- C5.0(Y ~ ., data = Data2,control = C5.0Control(minCase = floor(nrow(Data2)*input$Ratio_of_columns)))
              } else {
                treeModel <- C5.0(Y ~ ., data = Data2)
              }
              plot(as.party(treeModel))
            })
            output$plotDT02 <- renderPlot({
              if(input$Use_sampling_variables == 1){
                DataX <- Data[,floor(runif (floor(sqrt(ncolMax)), min=1,max=ncolMax))]
              } else {
                DataX <- Data
              }
              Data1 <- transform(DataX, Y = Ydata)
              if(input$Use_sampling_samples == 1){
                Data2 <- Data1[floor(runif (floor(sqrt(nrowMax)), min=1,max=nrowMax)),]
              } else {
                Data2 <- Data1
              }
              if(input$Use_minCases == 1){
                treeModel <- C5.0(Y ~ ., data = Data2,control = C5.0Control(minCase = floor(nrow(Data2)*input$Ratio_of_columns)))
              } else {
                treeModel <- C5.0(Y ~ ., data = Data2)
              }
              plot(as.party(treeModel))
            })
            output$plotDT03 <- renderPlot({
              if(input$Use_sampling_variables == 1){
                DataX <- Data[,floor(runif (floor(sqrt(ncolMax)), min=1,max=ncolMax))]
              } else {
                DataX <- Data
              }
              Data1 <- transform(DataX, Y = Ydata)
              if(input$Use_sampling_samples == 1){
                Data2 <- Data1[floor(runif (floor(sqrt(nrowMax)), min=1,max=nrowMax)),]
              } else {
                Data2 <- Data1
              }
              if(input$Use_minCases == 1){
                treeModel <- C5.0(Y ~ ., data = Data2,control = C5.0Control(minCase = floor(nrow(Data2)*input$Ratio_of_columns)))
              } else {
                treeModel <- C5.0(Y ~ ., data = Data2)
              }
              plot(as.party(treeModel))
            })
            output$plotDT04 <- renderPlot({
              if(input$Use_sampling_variables == 1){
                DataX <- Data[,floor(runif (floor(sqrt(ncolMax)), min=1,max=ncolMax))]
              } else {
                DataX <- Data
              }
              Data1 <- transform(DataX, Y = Ydata)
              if(input$Use_sampling_samples == 1){
                Data2 <- Data1[floor(runif (floor(sqrt(nrowMax)), min=1,max=nrowMax)),]
              } else {
                Data2 <- Data1
              }
              if(input$Use_minCases == 1){
                treeModel <- C5.0(Y ~ ., data = Data2,control = C5.0Control(minCase = floor(nrow(Data2)*input$Ratio_of_columns)))
              } else {
                treeModel <- C5.0(Y ~ ., data = Data2)
              }
              plot(as.party(treeModel))
            })
              if(input$Use_sampling_variables == 1){
                DataX <- Data[,floor(runif (floor(sqrt(ncolMax)), min=1,max=ncolMax))]
              } else {
                DataX <- Data
              }
              Data1 <- transform(DataX, Y = Ydata)
              if(input$Use_sampling_samples == 1){
                Data2 <- Data1[floor(runif (floor(sqrt(nrowMax)), min=1,max=nrowMax)),]
              } else {
                Data2 <- Data1
              }
              if(input$Use_minCases == 1){
                treeModel <- C5.0(Y ~ ., data = Data2,control = C5.0Control(minCase = floor(nrow(Data2)*input$Ratio_of_columns)))
              } else {
                treeModel <- C5.0(Y ~ ., data = Data2)
              }
              plot(as.party(treeModel))
              
            
          }
        }
      }
    }
  })
  
  
  output$plot301 <- renderPlotly({
    if(input$analysis == "Similarity_of_Variables_and_Categories1"){
      if(input$Similarity_of_Variables_and_Categories == "Between_label_column_and_others1"){
        if(input$Between_label_column_and_others == "One_class1"){
      
          req(input$file1)
          
          if(input$sep2 == "Separator_Comma"){sep <- ","}
          if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
          if(input$sep2 == "Separator_Separator_Tab"){sep <- "\t"}
          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
          if(input$DoNotUseFirst == 1){
            Data[,1] <- NULL
          }
          
          library(ggplot2)
          library(fastDummies)
          library(dummies)
          library(kernlab)
          library(tidyr)
          
          if(input$One_class == "MT_All_Varaiables1") {
            Data1 <- Data
          }else{
            Data1 <- dummy.data.frame(Data)
          }
          
          Data2 <- subset(Data1, Data1[,input$Label_column] == 0)
          Data2[,input$Label_column] <- NULL
          Data1[,input$Label_column] <- NULL
          
          if (input$One_class == "Basic_test_All_Varaiables1"){
            
            t_test_var_equal <- data.frame()
            for (i in 1:ncol(Data1)) {
              p_value <- t.test(x=subset(Data1[,i], Data[,input$Label_column] == 0),y=subset(Data1[,i], Data[,input$Label_column] == 1),var.equal=T,paired=F)$p.value
              FeEr <- cbind(colnames(Data1[i]),p_value)
              t_test_var_equal <- rbind(t_test_var_equal,FeEr)
            }
            output$plot601<-renderPlot(ggplot(t_test_var_equal, aes(x=t_test_var_equal[,1],y=t_test_var_equal[,2])) + geom_bar(stat = "identity") +  labs(title="Check difference of means",subtitle="Student's t-Test:var.equal=T",x="Variables",y="p.value"))
            
            t_test_var_Notequal <- data.frame()
            for (i in 1:ncol(Data1)) {
              p_value <- t.test(x=subset(Data1[,i], Data[,input$Label_column] == 0),y=subset(Data1[,i], Data[,input$Label_column] == 1),var.equal=F,paired=F)$p.value
              FeEr <- cbind(colnames(Data1[i]),p_value)
              t_test_var_Notequal <- rbind(t_test_var_Notequal,FeEr)
            }
            output$plot602<-renderPlot(ggplot(t_test_var_Notequal, aes(x=t_test_var_Notequal[,1],y=t_test_var_Notequal[,2])) + geom_bar(stat = "identity") +  labs(title="Check difference of means",subtitle="Welch's t-Test:var.equal=F",x="Variables",y="p.value"))
            
            var_test <- data.frame()
            for (i in 1:ncol(Data1)) {
              p_value <- var.test(x=subset(Data1[,i], Data[,input$Label_column] == 0),y=subset(Data1[,i], Data[,input$Label_column] == 1))$p.value
              FeEr <- cbind(colnames(Data1[i]),p_value)
              var_test <- rbind(var_test,FeEr)
            }
            output$plot603<-renderPlot(ggplot(var_test, aes(x=var_test[,1],y=var_test[,2])) + geom_bar(stat = "identity") +  labs(title="Check difference of variance",subtitle="F-Test",x="Variables",y="p.value"))
            
            RowName <- colnames(Data[input$Label_column])
            Data_long <- tidyr::gather(Data, key="Variable", value = val, -RowName)
            ggplotly(ggplot(Data_long, aes(x=as.factor(Data_long[,1]),y=Data_long[,3])) + geom_boxplot() + facet_wrap(~Data_long[,2],scales="free")+ labs(x="Label",y="Value",subtitle = "Variables"))
            
            
          } else if (input$One_class == "MT_All_Varaiables1"){
            n <- nrow(Data2)
            Ave1 <- colMeans(Data2)
            Var1 <- var(Data2)*(n-1)/n
            k <- ncol(Data2)
            
            Data3 <- Data1
            Data4 <- Data2
            
            n <- nrow(Data4)
            Ave1 <- colMeans(Data4)
            Var1 <- var(Data4)*(n-1)/n
            k <- ncol(Data4)
            
            MD <- mahalanobis(Data3, Ave1, Var1)/k
            Data5 <- cbind(Data, MD)
            
            Data5[,input$Label_column] <- factor(Data5[,input$Label_column])
            ggplotly(ggplot(Data5, aes(x=Data5[,input$Label_column], y=MD)) + geom_jitter(size=3, position=position_jitter(0.1))+labs(x="Label column", y="Distance from average of label='0' samples"))
            
          } else if (input$One_class == "MT_Selected_Varaiables1"){
            k2 <- ncol(Data2)
            n <- nrow(Data2)
            Error_Analysis <- data.frame()
            
            for (i in 1:k2) {
              Data3 <- Data1[,i]
              Data4 <- Data2[,i]
              Ave1 <- mean(Data4)
              Var1 <- var(Data4)*(n-1)/n
              MD <- ((Data3 - Ave1)^2)/Var1
              Data5 <- cbind(Data, MD)
              MaxMDinUnit <- max(subset(Data5, Data5[,input$Label_column] == 0)$MD)
              Data6 <- subset(Data5, Data5[,input$Label_column] == 1)
              Error_Number <- nrow(subset(Data6, Data6$MD < MaxMDinUnit))
              Features <- colnames(Data1[i])
              FeEr <- cbind(Features,Error_Number)
              
              Error_Analysis <- rbind(Error_Analysis,FeEr)
              
            }
            for (i1 in 1:k2) {
              for (i2 in 1:k2) {
                if(i2 > i1){
                  Data3 <- cbind(Data1[i1],Data1[i2])
                  Data4 <- cbind(Data2[i1],Data2[i2])
                  Ave1 <- colMeans(Data4)
                  Var1 <- var(Data4)*(n-1)/n
                  k <- 2
                  
                  e<-try(mahalanobis(Data3, Ave1, Var1)/k, silent = FALSE)
                  if( class(e) == "try-error") {
                    MD <- 1
                  }else{
                    MD <- mahalanobis(Data3, Ave1, Var1)/k
                  }
                  #MD <- mahalanobis(Data3, Ave1, Var1)/k
                  if (MD != 1){
                    Data5 <- cbind(Data, MD)
                    MaxMDinUnit <- max(subset(Data5, Data5[,input$Label_column] == 0)$MD)
                    Data6 <- subset(Data5, Data5[,input$Label_column] == 1)
                    Error_Number <- nrow(subset(Data6, Data6$MD < MaxMDinUnit))
                    Features <- paste(colnames(Data1[i1]),"_",colnames(Data1[i2]))
                    FeEr <- cbind(Features,Error_Number)
                    
                    Error_Analysis <- rbind(Error_Analysis,FeEr)
                  }
                }
              }
            }
            for (i1 in 1:k2) {
              for (i2 in 1:k2) {
                if(i2 > i1){
                  for (i3 in 1:k2) {
                    if(i3 > i2){
                      Data3 <- cbind(Data1[i1],Data1[i2],Data1[i3])
                      Data4 <- cbind(Data2[i1],Data2[i2],Data2[i3])
                      Ave1 <- colMeans(Data4)
                      Var1 <- var(Data4)*(n-1)/n
                      k <- 3
                      e<-try(mahalanobis(Data3, Ave1, Var1)/k, silent = FALSE)
                      if( class(e) == "try-error") {
                        MD <- 1
                      }else{
                        MD <- mahalanobis(Data3, Ave1, Var1)/k
                      }
                      #MD <- mahalanobis(Data3, Ave1, Var1)/k
                      if (MD != 1){
                        Data5 <- cbind(Data, MD)
                        MaxMDinUnit <- max(subset(Data5, Data5[,input$Label_column] == 0)$MD)
                        Data6 <- subset(Data5, Data5[,input$Label_column] == 1)
                        Error_Number <- nrow(subset(Data6, Data6$MD < MaxMDinUnit))
                        Features <- paste(colnames(Data1[i1]),"_",colnames(Data1[i2]),"_",colnames(Data1[i3]))
                        FeEr <- cbind(Features,Error_Number)
                        
                        Error_Analysis <- rbind(Error_Analysis,FeEr)
                      }
                    }
                  }
                }
              }
            }
            Error_Analysis <- Error_Analysis[order(Error_Analysis$Error_Number, decreasing=F),]
            output$Data_OutputMTselected <- renderDataTable(Error_Analysis)
            Error_Analysis2 <- head(Error_Analysis,30)
            Error_Analysis2[,2]<- as.numeric(Error_Analysis2[,2])
            ggplotly(ggplot(Error_Analysis2, aes(x=Error_Analysis2[,2], y=reorder(Error_Analysis2[,1],-Error_Analysis2[,2]))) + geom_bar(stat = "identity") +labs(x="Error_Number (Samples of label='1' that distances are shorter than maximum of label='0')", y="Features (Set of variables used in the model)"))
            
          } else if (input$One_class == "PCA_MT_All_Varaiables1"){
            n <- nrow(Data2)
            Ave1 <- colMeans(Data2)
            Var1 <- var(Data2)*(n-1)/n
            k <- ncol(Data2)
            
            Data3 <- Data1
            Data4 <- Data2
            
            model <- prcomp(Data2, scale=TRUE,tol = 0.0001)
            Data3 <- predict(model, Data1)
            Data4 <- subset(Data3, Data[,input$Label_column] == 0)
            pc2 <- sweep(model$rotation, MARGIN=2, model$sdev, FUN="*")
            output$Data_OutputPCAMT <- renderDataTable(cbind(colnames(Data2),round(pc2, digits = 3)))
            #output$Data_OutputPCAMT <- renderDataTable(round(pc2, digits = 3))
            
            
            n <- nrow(Data4)
            Ave1 <- colMeans(Data4)
            Var1 <- var(Data4)*(n-1)/n
            k <- ncol(Data4)
            
            Var2 <- Ave1
            
            MD <- mahalanobis(Data3, Ave1, Var1)/k
            
            Data5 <- cbind(Data, MD,Data3)
            for (i in 1:k) {
              Var2[i] <- var(Data4[,i]*(n-1)/n)
            }
            Data6 <- Data3
            for (i in 1:k) {
              Data6[,i] <- (((Data3[,i] - Ave1[i])/sqrt(Var2[i]))^2)/k
            }
            Data7 <- cbind(Data[input$Label_column], Data6)
            Data7[,1]<- as.character(Data7[,1])
            
            RowName <- colnames(Data7[1])
            Data8 <- tidyr::gather(Data7, key="PC", value = Val, -RowName)
            output$plotPCAMT <- renderPlotly(ggplotly(ggplot(Data8, aes(x=PC, y=Val)) + geom_jitter(size=3, position=position_jitter(0.1),aes(colour=as.factor(Data8[,1])))+labs(x="PC", y="Value",color=RowName)))
            
            Data5[,input$Label_column] <- factor(Data5[,input$Label_column])
            ggplotly(ggplot(Data5, aes(x=Data5[,input$Label_column], y=MD)) + geom_jitter(size=3, position=position_jitter(0.1))+labs(x="Label column", y="Distance from average of label='0' samples"))
            
          } else if (input$One_class == "PCA_MT_Selected_Varaiables1"){
            k2 <- ncol(Data2)
            n <- nrow(Data2)
            Error_Analysis <- data.frame()
            
            for (i in 1:k2) {
              Data11 <- Data1[i]
              Data12 <- Data2[i]
              
              model <- prcomp(Data12, scale=TRUE,tol = 0.0001)
              Data3 <- predict(model, Data11)
              Data4 <- subset(Data3, Data[,input$Label_column] == 0)
              Ave1 <- colMeans(Data4)
              Var1 <- var(Data4)*(n-1)/n
              k <- 1
              
              MD <- mahalanobis(Data3, Ave1, Var1)/k
              
              Data5 <- cbind(Data, MD)
              MaxMDinUnit <- max(subset(Data5, Data5[,input$Label_column] == 0)$MD)
              Data6 <- subset(Data5, Data5[,input$Label_column] == 1)
              Error_Number <- nrow(subset(Data6, Data6$MD < MaxMDinUnit))
              Features <- colnames(Data1[i])
              FeEr <- cbind(Features,Error_Number)
              
              Error_Analysis <- rbind(Error_Analysis,FeEr)
              
            }
            for (i1 in 1:k2) {
              for (i2 in 1:k2) {
                if(i2 > i1){
                  Data11 <- cbind(Data1[i1],Data1[i2])
                  Data12 <- cbind(Data2[i1],Data2[i2])
                  
                  model <- prcomp(Data12, scale=TRUE,tol = 0.0001)
                  Data3 <- predict(model, Data11)
                  Data4 <- subset(Data3, Data[,input$Label_column] == 0)
                  Ave1 <- colMeans(Data4)
                  Var1 <- var(Data4)*(n-1)/n
                  k <- 2
                  MD <- mahalanobis(Data3, Ave1, Var1)/k
                  Data5 <- cbind(Data, MD)
                  MaxMDinUnit <- max(subset(Data5, Data5[,input$Label_column] == 0)$MD)
                  Data6 <- subset(Data5, Data5[,input$Label_column] == 1)
                  Error_Number <- nrow(subset(Data6, Data6$MD < MaxMDinUnit))
                  Features <- paste(colnames(Data1[i1]),"_",colnames(Data1[i2]))
                  FeEr <- cbind(Features,Error_Number)
                  
                  Error_Analysis <- rbind(Error_Analysis,FeEr)
                }
              }
            }
            for (i1 in 1:k2) {
              for (i2 in 1:k2) {
                if(i2 > i1){
                  for (i3 in 1:k2) {
                    if(i3 > i2){
                      Data11 <- cbind(Data1[i1],Data1[i2],Data1[i3])
                      Data12 <- cbind(Data2[i1],Data2[i2],Data2[i3])
                      
                      model <- prcomp(Data12, scale=TRUE,tol = 0.0001)
                      Data3 <- predict(model, Data11)
                      Data4 <- subset(Data3, Data[,input$Label_column] == 0)
                      Ave1 <- colMeans(Data4)
                      Var1 <- var(Data4)*(n-1)/n
                      k <- 3
                      MD <- mahalanobis(Data3, Ave1, Var1)/k
                      Data5 <- cbind(Data, MD)
                      MaxMDinUnit <- max(subset(Data5, Data5[,input$Label_column] == 0)$MD)
                      Data6 <- subset(Data5, Data5[,input$Label_column] == 1)
                      Error_Number <- nrow(subset(Data6, Data6$MD < MaxMDinUnit))
                      Features <- paste(colnames(Data1[i1]),"_",colnames(Data1[i2]),"_",colnames(Data1[i3]))
                      FeEr <- cbind(Features,Error_Number)
                      
                      Error_Analysis <- rbind(Error_Analysis,FeEr)
                    }
                  }
                }
              }
            }
            Error_Analysis <- Error_Analysis[order(Error_Analysis$Error_Number, decreasing=F),]
            output$Data_OutputPCAMTselected <- renderDataTable(Error_Analysis)
            Error_Analysis2 <- head(Error_Analysis,30)
            Error_Analysis2[,2]<- as.numeric(Error_Analysis2[,2])
            ggplotly(ggplot(Error_Analysis2, aes(x=Error_Analysis2[,2], y=reorder(Error_Analysis2[,1],-Error_Analysis2[,2]))) + geom_bar(stat = "identity") +labs(x="Error_Number (Samples of label='1' that distances are shorter than maximum of label='0')", y="Features (Set of variables used in the model)"))
            
          } else if (input$One_class == "Kernel_PCA_MT1"){
            n <- nrow(Data2)
            Ave1 <- colMeans(Data2)
            Var1 <- var(Data2)*(n-1)/n
            k <- ncol(Data2)
            
            Data3 <- Data1
            Data4 <- Data2
            model <- kpca(as.matrix(Data2),kernel=input$Kernel2 ,kpar=list(sigma=input$kpar_value))
            
            Data3 <- predict(model, as.matrix(Data1))
            Data4 <- subset(Data3, Data[,input$Label_column] == 0)
            
            
            n <- nrow(Data4)
            Ave1 <- colMeans(Data4)
            Var1 <- var(Data4)*(n-1)/n
            k <- ncol(Data4)
            
            Var2 <- Ave1
            
            MD <- mahalanobis(Data3, Ave1, Var1)/k
            
            Data5 <- cbind(Data, MD,Data3)
            
            Data5[,input$Label_column] <- factor(Data5[,input$Label_column])
            ggplotly(ggplot(Data5, aes(x=Data5[,input$Label_column], y=MD)) + geom_jitter(size=3, position=position_jitter(0.1))+labs(x="First column", y="Distance from average of label='0' samples"))
            
          } else if (input$One_class == "One_Class_SVM_All_Varaiables1"){
            k2 <- ncol(Data2)
            n <- nrow(Data2)
            
              
            Data3 <- transform(Data1, type=1)
            Data4 <- transform(Data2, type=1)
            
            
            OC <- ksvm(type~.,data=Data4,type='one-svc', kernel=input$Kernel4, nu = input$nu4)
            clust <- predict(OC,Data3)
            Data5 <- cbind(Data, clust)
            ggplotly(ggplot(Data5, aes(x=clust)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1) + facet_grid(. ~ as.factor(Data5[,input$Label_column]))+labs(x="Label column", y="Frequency"))
            
          } else if (input$One_class == "One_Class_SVM_Selected_Varaiables1"){
            k2 <- ncol(Data2)
            n <- nrow(Data2)
            Error_Analysis <- data.frame()
            nu1 <- input$nu2
            
            for (j1 in 1:8) {
              if (j1 == 1) {Kernel3 <- "anovadot"}
              if (j1 == 2) {Kernel3 <- "rbfdot"}
              if (j1 == 3) {Kernel3 <- "polydot"}
              if (j1 == 4) {Kernel3 <- "vanilladot"}
              if (j1 == 5) {Kernel3 <- "tanhdot"}
              if (j1 == 6) {Kernel3 <- "laplacedot"}
              if (j1 == 7) {Kernel3 == "besseldot"}
              if (j1 == 8) {Kernel3 <- "splinedot"}
              
              for (i1 in 1:k2) {
                Data3 <- transform(Data1[,i1], type=1)
                Data4 <- transform(Data2[,i1], type=1)
                
                
                OC <- ksvm(type~.,data=Data4,type='one-svc', kernel=Kernel3, nu = nu1)
                clust <- predict(OC,Data3)
                
                Data5 <- cbind(Data, clust)
                Data6 <- subset(Data5, Data5[,input$Label_column] == 1)
                Error_Number <- nrow(subset(Data6, Data6$clust == "FALSE"))
                Features <- paste(Kernel3,"_",colnames(Data1[i1]))
                FeEr <- cbind(Features,Error_Number)
                
                Error_Analysis <- rbind(Error_Analysis,FeEr)
                
              }
              for (i1 in 1:k2) {
                for (i2 in 1:k2) {
                  if(i2 > i1){
                    Data3 <- transform(cbind(Data1[i1],Data1[i2]), type=1)
                    Data4 <- transform(cbind(Data2[i1],Data2[i2]), type=1)
                    
                    
                    OC <- ksvm(type~.,data=Data4,type='one-svc', kernel=Kernel3, nu = nu1)
                    clust <- predict(OC,Data3)
                    
                    Data5 <- cbind(Data, clust)
                    Data6 <- subset(Data5, Data5[,input$Label_column] == 1)
                    Error_Number <- nrow(subset(Data6, Data6$clust == "FALSE"))
                    Features <- paste(Kernel3,"_",colnames(Data1[i1]),"_",colnames(Data1[i2]))
                    FeEr <- cbind(Features,Error_Number)
                    
                    Error_Analysis <- rbind(Error_Analysis,FeEr)
                  }
                }
              }
              for (i1 in 1:k2) {
                for (i2 in 1:k2) {
                  if(i2 > i1){
                    for (i3 in 1:k2) {
                      if(i3 > i2){
                        Data3 <- transform(cbind(Data1[i1],Data1[i2],Data1[i3]), type=1)
                        Data4 <- transform(cbind(Data2[i1],Data2[i2],Data2[i3]), type=1)
                        
                        
                        OC <- ksvm(type~.,data=Data4,type='one-svc', kernel=Kernel3, nu = nu1)
                        clust <- predict(OC,Data3)
                        
                        Data5 <- cbind(Data, clust)
                        Data6 <- subset(Data5, Data5[,input$Label_column] == 1)
                        Error_Number <- nrow(subset(Data6, Data6$clust == "FALSE"))
                        Features <- paste(Kernel3,"_",colnames(Data1[i1]),"_",colnames(Data1[i2]),"_",colnames(Data1[i3]))
                        FeEr <- cbind(Features,Error_Number)
                        
                      }
                    }
                  }
                }
              }
            }
            
            Error_Analysis <- Error_Analysis[order(Error_Analysis$Error_Number, decreasing=F),]
            output$Data_OutputOneClassSVMselected <- renderDataTable(Error_Analysis)
            Error_Analysis2 <- head(Error_Analysis,30)
            Error_Analysis2[,2]<- as.numeric(Error_Analysis2[,2])
            ggplotly(ggplot(Error_Analysis2, aes(x=Error_Analysis2[,2], y=reorder(Error_Analysis2[,1],-Error_Analysis2[,2]))) + geom_bar(stat = "identity") +labs(x="Error_Number (Samples of label='1' that distances are shorter than maximum of label='0')", y="Features (Set of variables used in the model)"))
            
          
          } else if (input$One_class == "Minimum_Distance_All_Varaiables1"){
            model <- prcomp(Data2, scale=TRUE, tol=0.01)
            Data3 <- predict(model, as.matrix(Data1))
            std <- apply(model$x, 2, sd)
            Data4 <- Data3
            for (i in 1:ncol(Data3)) {
              Data4[,i] <- Data3[,i]/std[i]
            }
            Data5 <- dist(Data4, diag = TRUE, upper = TRUE)
            Data5 <-as.matrix(Data5)
            diag(Data5) <- 1000 
            Data5 <-as.data.frame(Data5)
            Data5 <- data.frame(cbind(Data[,input$Label_column],Data5))
            Data6 <- subset(Data5, Data5[,1] == 0)
            Data6[,1] <- NULL
            Data7 <- as.data.frame(t(Data6))
            
            min0 <- apply(Data7, 1,min)
            Data8 <- as.data.frame(cbind(Data[,input$Label_column],min0))
            Data8[,1] <- factor(Data8[,1])
            ggplotly(ggplot(Data8, aes(x=Data8[,1], y=min0)) + geom_jitter(size=3, position=position_jitter(0.1))+labs(x="Label column", y="Distance from nearest label='0' sample"))
            
          } else {
            
            
            
            k2 <- ncol(Data2)
            Error_Analysis <- data.frame()
            
            
            for (i1 in 1:k2) {
              Data101 <- cbind(Data1[i1])
              Data201 <- cbind(Data2[i1])
              
              model <- prcomp(Data201, scale=TRUE, tol=0.01)
              Data3 <- predict(model, as.matrix(Data101))
              std <- apply(model$x, 2, sd)
              Data4 <- Data3
              for (i in 1:ncol(Data3)) {
                Data4[,i] <- Data3[,i]/std[i]
              }
              Data5 <- dist(Data4, diag = TRUE, upper = TRUE)
              Data5 <-as.matrix(Data5)
              diag(Data5) <- 1000 
              Data5 <-as.data.frame(Data5)
              Data5 <- data.frame(cbind(Data[,input$Label_column],Data5))
              Data6 <- subset(Data5, Data5[,1] == 0)
              Data6[,1] <- NULL
              Data7 <- as.data.frame(t(Data6))
              
              min0 <- apply(Data7, 1,min)
              Data8 <- as.data.frame(cbind(Data[input$Label_column],min0))
              MaxMDinUnit <- max(subset(Data8, Data8[,1] == 0)$min0)
              Data9 <- subset(Data8, Data8[,1] == 1)
              
              Error_Number <- nrow(subset(Data9, Data9$min0 < MaxMDinUnit))
              Features <- paste(colnames(Data1[i1]))
              FeEr <- cbind(Features,Error_Number)
              
              Error_Analysis <- rbind(Error_Analysis,FeEr)
            }
            
            for (i1 in 1:k2) {
              for (i2 in 1:k2) {
                if(i2 > i1){
                  Data101 <- cbind(Data1[i1],Data1[i2])
                  Data201 <- cbind(Data2[i1],Data2[i2])
                  
                  model <- prcomp(Data201, scale=TRUE, tol=0.01)
                  Data3 <- predict(model, as.matrix(Data101))
                  std <- apply(model$x, 2, sd)
                  Data4 <- Data3
                  for (i in 1:ncol(Data3)) {
                    Data4[,i] <- Data3[,i]/std[i]
                  }
                  Data5 <- dist(Data4, diag = TRUE, upper = TRUE)
                  Data5 <-as.matrix(Data5)
                  diag(Data5) <- 1000 
                  Data5 <-as.data.frame(Data5)
                  Data5 <- data.frame(cbind(Data[,input$Label_column],Data5))
                  Data6 <- subset(Data5, Data5[,1] == 0)
                  Data6[,1] <- NULL
                  Data7 <- as.data.frame(t(Data6))
                  
                  min0 <- apply(Data7, 1,min)
                  Data8 <- as.data.frame(cbind(Data[input$Label_column],min0))
                  MaxMDinUnit <- max(subset(Data8, Data8[,1] == 0)$min0)
                  Data9 <- subset(Data8, Data8[,1] == 1)
                
                  Error_Number <- nrow(subset(Data9, Data9$min0 < MaxMDinUnit))
                  Features <- paste(colnames(Data1[i1]),"_",colnames(Data1[i2]))
                  FeEr <- cbind(Features,Error_Number)
                  
                  Error_Analysis <- rbind(Error_Analysis,FeEr)
                }
              }
            }
            
            for (i1 in 1:k2) {
              for (i2 in 1:k2) {
                if(i2 > i1){
                  for (i3 in 1:k2) {
                    if(i3 > i2){
                      Data101 <- cbind(Data1[i1],Data1[i2],Data1[i3])
                      Data201 <- cbind(Data2[i1],Data2[i2],Data2[i3])
                      
                      model <- prcomp(Data201, scale=TRUE, tol=0.01)
                      Data3 <- predict(model, as.matrix(Data101))
                      std <- apply(model$x, 2, sd)
                      Data4 <- Data3
                      for (i in 1:ncol(Data3)) {
                        Data4[,i] <- Data3[,i]/std[i]
                      }
                      Data5 <- dist(Data4, diag = TRUE, upper = TRUE)
                      Data5 <-as.matrix(Data5)
                      diag(Data5) <- 1000 
                      Data5 <-as.data.frame(Data5)
                      Data5 <- data.frame(cbind(Data[,input$Label_column],Data5))
                      Data6 <- subset(Data5, Data5[,1] == 0)
                      Data6[,1] <- NULL
                      Data7 <- as.data.frame(t(Data6))
                      
                      min0 <- apply(Data7, 1,min)
                      Data8 <- as.data.frame(cbind(Data[input$Label_column],min0))
                      MaxMDinUnit <- max(subset(Data8, Data8[,1] == 0)$min0)
                      Data9 <- subset(Data8, Data8[,1] == 1)
                      
                      Error_Number <- nrow(subset(Data9, Data9$min0 < MaxMDinUnit))
                      Features <- paste(colnames(Data1[i1]),"_",colnames(Data1[i2]),"_",colnames(Data1[i3]))
                      FeEr <- cbind(Features,Error_Number)
                      
                      Error_Analysis <- rbind(Error_Analysis,FeEr)
                    }
                  }
                }
              }
            }
            
            Error_Analysis <- Error_Analysis[order(Error_Analysis$Error_Number, decreasing=F),]
            output$Data_OutputMDselected <- renderDataTable(Error_Analysis)
            Error_Analysis2 <- head(Error_Analysis,30)
            Error_Analysis2[,2]<- as.numeric(Error_Analysis2[,2])
            ggplotly(ggplot(Error_Analysis2, aes(x=Error_Analysis2[,2], y=reorder(Error_Analysis2[,1],-Error_Analysis2[,2]))) + geom_bar(stat = "identity") +labs(x="Error_Number (Samples of label='1' that distances are shorter than maximum of label='0')", y="Features (Set of variables used in the model)"))
            
          }
          
        }
      }
    }
  })
  
  
  output$plot10 <- renderPlotly({
    if(input$analysis == "Similarity_of_Variables_and_Categories1"){
      if(input$Similarity_of_Variables_and_Categories == "Between_label_column_and_others1"){
        if(input$Between_label_column_and_others == "Hidden1"){
          if(input$finder == "Hidden_PCA1"){
      
            req(input$file1)
            
            if(input$sep2 == "Separator_Comma"){sep <- ","}
            if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
            if(input$sep2 == "Separator_Tab"){sep <- "\t"}
            Data <- read.csv(input$file1$datapath, header=T,sep = sep)
            if(input$DoNotUseFirst == 1){
              Data[,1] <- NULL
            }
            
            library(dummies)
            library(ggplot2)
            library(tidyr)
            Data1 <- Data
            Y <- names(Data1[input$Label_column])
            Ydata <- Data1[,input$Label_column]
            Data1[,input$Label_column] <- NULL
            Data2 <- dummy.data.frame(Data1)
            pc <- prcomp(Data2, scale=TRUE)
            Data4 <- cbind(Ydata, pc$x, Data2)
            Data_long <- tidyr::gather(Data4, key="X", value = Xs, -Ydata)
            if(class(Ydata) == "numeric") {
              ggplotly(ggplot(Data_long, aes(x=Xs,y=Data_long[,1])) + geom_point() + facet_wrap(~X,scales="free")+ labs(y=Y))
            } else {
              ggplotly(ggplot(Data_long, aes(x=Data_long[,1],y=Xs)) + geom_jitter(size=1, position=position_jitter(0.1)) + facet_wrap(~X,scales="free")+ labs(x=Y))
            }
          }
        }
      }
    }
  })
  
  output$plot11 <- renderPlotly({
    if(input$analysis == "Similarity_of_Variables_and_Categories1"){
      if(input$Similarity_of_Variables_and_Categories == "Between_label_column_and_others1"){
        if(input$Between_label_column_and_others == "Hidden1"){
          if(input$finder == "Hidden_ICA1"){
      
            req(input$file1)
            
            if(input$sep2 == "Separator_Comma"){sep <- ","}
            if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
            if(input$sep2 == "Separator_Tab"){sep <- "\t"}
            Data <- read.csv(input$file1$datapath, header=T,sep = sep)
            if(input$DoNotUseFirst == 1){
              Data[,1] <- NULL
            }
            
            library(fastICA)
            library(dummies)
            library(ggplot2)
            library(tidyr)
            Data1 <- Data
            Y <- names(Data1[input$Label_column])
            Ydata <- Data1[,input$Label_column]
            Data1[,input$Label_column] <- NULL
            Data2 <- dummy.data.frame(Data1)
            pc <- prcomp(Data2, scale=TRUE, tol=0.01)
            ic <- fastICA(Data2, ncol(pc$x))$S
            Data4 <- cbind(Ydata, ic, Data2)
            Data_long <- tidyr::gather(Data4, key="X", value = Xs, -Ydata)
            if(class(Ydata) == "numeric") {
              ggplotly(ggplot(Data_long, aes(x=Xs,y=Data_long[,1])) + geom_point() + facet_wrap(~X,scales="free")+ labs(y=Y))
            } else {
              ggplotly(ggplot(Data_long, aes(x=Data_long[,1],y=Xs)) + geom_jitter(size=1, position=position_jitter(0.1)) + facet_wrap(~X,scales="free")+ labs(x=Y))
            }
          }
        }
      }
    }
  })
  
  
  
    
  output$plot201 <- renderPlotly({
    if(input$analysis == "Similarity_of_Samples1"){
      if(input$Dimension_for_clustering == "Dimension_2"){
        if(input$Dimension_Reduction == "MDS1" || input$Dimension_Reduction == "MDS2" || input$Dimension_Reduction == "nMDS1") {
        
      
          req(input$file1)
          
          if(input$sep2 == "Separator_Comma"){sep <- ","}
          if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
          if(input$sep2 == "Separator_Tab"){sep <- "\t"}
          
          
          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
          #if(input$DoNotUseFirst == 1){
          #  Data[,1] <- NULL
          #}
          
        
          library(dummies)
          library(ggplot2)
          library(som)
          library(MASS)
          library(Rtsne)
          library(mclust)
          library(dbscan)
          library(e1071)
          library(kernlab)
          
          Data1 <- Data
          if(input$Use_one_row_as_sample_name2 == 1){
            Y <- names(Data1[input$sample_row2])
            Ydata <- Data1[,input$sample_row2]
            Data1[,input$sample_row2] <- NULL
          } else {
            Y <- "Index_No"
            Ydata <- row.names(Data)
          }
          
          Data2 <- dummy.data.frame(Data1)
          Data3 <- Data2
          n <- ncol(Data2)
          for (i in 1:n) {
            Data3[,i] <- (Data2[,i]-min(Data2[,i]))/(max(Data2[,i])-min(Data2[,i]))
          }
          
          
          if(input$Dimension_Reduction != "nMDS1") {
            if(input$Dimension_Reduction == "MDS1") {
              Data4 <- dist(Data3)
              sn <- sammon(Data4)
              Data5 <- sn$points
            }else{
              ts <- Rtsne(Data3, perplexity = input$perplexity_value)
              Data5 <- ts$Y
            }
            Data51 <- Data5
            for (i in 1:ncol(Data5)) {
              Data51[,i] <- (Data5[,i] - min(Data5[,i]))/(max(Data5[,i]) - min(Data5[,i]))
            }
            Data6 <- cbind(Data51,Data,Index = row.names(Data))
            
            
            if(input$AddClustering == 0){
              if(input$plot_type2 == "G11") {
                ggplotly(ggplot(Data6, aes(x=Data6[,1], y=Data6[,2],label=Ydata)) + geom_text() + labs(y="axis2",x="axis1"))
              } else if(input$plot_type2 == "G12") {
                ggplotly(ggplot(Data6, aes(x=Data6[,1], y=Data6[,2],label=Index)) + geom_text() + labs(y="axis2",x="axis1"))
              } else if(input$plot_type2 == "G13") {
                Data6$Index <- as.numeric(Data6$Index)
                ggplotly(ggplot(Data6, aes(x=Data6[,1], y=Data6[,2])) + geom_point(aes(colour=Ydata)) + labs(y="axis2",x="axis1"))
              } else if(input$plot_type2 == "G14") {
                Data6$Index <- as.numeric(Data6$Index)
                ggplotly(ggplot(Data6, aes(x=Data6[,1], y=Data6[,2])) + geom_point(aes(colour=Index)) + scale_color_viridis_c(option = "D")+ labs(y="axis2",x="axis1"))
              } else {
                ggplotly(ggplot(Data6, aes(x=Data6[,1], y=Data6[,2])) + geom_point()+ labs(y="axis2",x="axis1"))
              }
            } else {
            
              if(input$Clustering == "clust1") {
                mc <- Mclust(Data6[,1:2],input$k)
                Data7 <- transform(Data6 ,clust = mc$classification)
              }else if(input$Clustering == "clust2"){
                dbs <- dbscan(Data6[,1:2], eps = input$eps_value)
                Data7 <- transform(Data6 ,clust = dbs$cluster)
              }else if(input$Clustering == "clust3"){
                km <- kmeans(Data6[,1:2],input$k)
                Data7 <- transform(Data6 ,clust = km$cluster)
              }else if(input$Clustering == "One_class_SVM_Clustering1"){
                if(input$Kernel_library == "anovadot" || input$Kernel_library == "rbfdot" || input$Kernel_library == "polydot" || input$Kernel_library == "vanilladot" || 
                   input$Kernel_library == "tanhdot" || input$Kernel_library == "laplacedot" || input$Kernel_library == "besseldot" || input$Kernel_library == "splinedot"){
                  Data8 <- transform(Data6[,1:2], type=1)
                  
                  OC <- ksvm(type~.,data=Data8,type='one-svc', kernel=input$Kernel_library, nu = input$nu1)
                  clust <- predict(OC,Data8)
                  Data7 <- cbind(clust, Data6)
                }else {
                  Data8 <- transform(Data6[,1:2])
                  OC <- svm(x=Data8, y=NULL, type='one-classification', kernel=input$Kernel_library, nu = input$nu1)
                  clust <- predict(OC,Data8)
                  Data7 <- cbind(clust, Data6)
                }
              }
              Data7$clust <- as.factor(Data7$clust)
              
              output$downloadData <- downloadHandler(
                filename = function() {
                  paste("cluster_data", ".csv", sep = "")
                },
                content = function(file) {
                  write.csv(Data7, file, row.names = FALSE)
                }
              )
              
              if(input$plot_type == "G1") {
                ggplotly(ggplot(Data7, aes(x=Data7[,1], y=Data7[,2],label=Ydata)) + geom_text() + geom_point(aes(colour=clust))+ labs(y="axis2",x="axis1"))
              } else if(input$plot_type == "G2") {
                ggplotly(ggplot(Data7, aes(x=Data7[,1], y=Data7[,2],label=Index)) + geom_text() + geom_point(aes(colour=clust))+ labs(y="axis2",x="axis1"))
              } else if(input$plot_type == "G3") {
                Data7$Index <- as.numeric(Data7$Index)
                ggplotly(ggplot(Data7, aes(x=Data7[,1], y=Data7[,2],label=clust)) + geom_point(aes(colour=Index)) + scale_color_viridis_c(option = "D")+ geom_text()+ labs(y="axis2",x="axis1"))
              } else {
                ggplotly(ggplot(Data7, aes(x=Data7[,1], y=Data7[,2])) + geom_point(aes(colour=clust))+ labs(y="axis2",x="axis1"))
              }
            }
          
          } else {
            library(igraph)
            library(networkD3)
            rownames(Data3) <- Ydata
            Data12 <- as.matrix(dist(Data3))
            Data12 <- (max(Data12) - Data12)
            diag(Data12) <- 0
            Data12 <- Data12 / max(Data12) * 10
            Data12[Data12< input$Distance_to_use] <- 0
            GM4 <- graph.adjacency(Data12,weighted=T, mode = "undirected")
            if(input$network_library5 == 'igraph'){
              output$plot203 <- renderPlot(plot(GM4, edge.width=E(GM4)$weight))
            }else{
              DM.g2 <- data.frame(as_edgelist(GM4))
              output$plot203b <- renderSimpleNetwork(simpleNetwork(DM.g2, fontSize = 14, nodeColour = "0000A2", opacity = 1, fontFamily = "Meiryo UI") )
            }
          }
        }
      }
    }
  })
  
  output$plot204 <- renderPlotly({
    if(input$analysis == "Similarity_of_Samples1"){
      if(input$Dimension_for_clustering == "Dimension_2"){
        if(input$Dimension_Reduction == "None1") {
          
          
          req(input$file1)
          
          if(input$sep2 == "Separator_Comma"){sep <- ","}
          if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
          if(input$sep2 == "Separator_Tab"){sep <- "\t"}
          
          
          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
          #if(input$DoNotUseFirst == 1){
          #  Data[,1] <- NULL
          #}
          
          
          library(dummies)
          library(ggplot2)
          #Data1 <- Data
          #Y <- names(Data1[1])
          #Ydata <- Data1[,1]
          #Data1[,1] <- NULL
          
          Data1 <- Data
          if(input$Use_one_row_as_sample_name2 == 1){
            Y <- names(Data1[input$sample_row2])
            Ydata <- Data1[,input$sample_row2]
            Data1[,input$sample_row2] <- NULL
          } else {
            Y <- "Index_No"
            Ydata <- row.names(Data)
          }
          Data1$Index = row.names(Data1)
          
          if(input$Xcol11 > 0){Xname <- names(Data1[input$Xcol11])} else {Xname <- "None"}
          if(input$Ycol11 > 0){Lname <- names(Data1[input$Ycol11])} else {Lname <- "None"}
          if(input$Scol11 > 0){Sname <- names(Data1[input$Scol11])} else {Sname <- "None"}
          
          if(input$Scol11 != 0 ){
            if(class(Data1[,input$Scol11]) == "numeric" || class(Data1[,input$Scol11]) == "integer"){Data1[,input$Scol11] <- droplevels(cut(Data1[,input$Scol11], breaks = input$NumericalToCategorcalS11, include.lowest = TRUE))}
            #gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol11],y=Data1[,input$Ycol11])) + geom_point() + facet_wrap(~Data1[,input$Scol11],scales="free")+ labs(x=Xname,y=Lname,subtitle = Sname)
            
            if(input$plot_type2 == "G11") {
              gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol11],y=Data1[,input$Ycol11],label=Ydata)) + geom_text() + facet_wrap(~Data1[,input$Scol11],scales="free") + labs(x=Xname,y=Lname,subtitle = Sname)
            } else if(input$plot_type2 == "G12") {
              gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol11],y=Data1[,input$Ycol11],label=Index)) + geom_text() + facet_wrap(~Data1[,input$Scol11],scales="free") + labs(x=Xname,y=Lname,subtitle = Sname)
            } else if(input$plot_type2 == "G13") {
              Data1$Index <- as.numeric(Data1$Index)
              gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol11],y=Data1[,input$Ycol11])) + geom_point(aes(colour=Ydata)) + facet_wrap(~Data1[,input$Scol11],scales="free") + labs(x=Xname,y=Lname,subtitle = Sname)
            } else if(input$plot_type2 == "G14") {
              Data1$Index <- as.numeric(Data1$Index)
              gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol11],y=Data1[,input$Ycol11])) + geom_point(aes(colour=Index)) + facet_wrap(~Data1[,input$Scol11],scales="free") + scale_color_viridis_c(option = "D")+ labs(x=Xname,y=Lname,subtitle = Sname)
            } else {
              gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol11],y=Data1[,input$Ycol11])) + geom_point() + facet_wrap(~Data1[,input$Scol11],scales="free")+ labs(x=Xname,y=Lname,subtitle = Sname)
            }
          } else {
            #gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol11],y=Data1[,input$Ycol11])) + geom_point() + labs(x=Xname,y=Lname)
            if(input$plot_type2 == "G11") {
              gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol11],y=Data1[,input$Ycol11],label=Ydata)) + geom_text()  + labs(x=Xname,y=Lname)
            } else if(input$plot_type2 == "G12") {
              gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol11],y=Data1[,input$Ycol11],label=Index)) + geom_text() + labs(x=Xname,y=Lname)
            } else if(input$plot_type2 == "G13") {
              Data1$Index <- as.numeric(Data1$Index)
              gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol11],y=Data1[,input$Ycol11])) + geom_point(aes(colour=Ydata)) + labs(x=Xname,y=Lname)
            } else if(input$plot_type2 == "G14") {
              Data1$Index <- as.numeric(Data1$Index)
              gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol11],y=Data1[,input$Ycol11])) + geom_point(aes(colour=Index))  + scale_color_viridis_c(option = "D")+ labs(x=Xname,y=Lname)
            } else {
              gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol11],y=Data1[,input$Ycol11])) + geom_point() + labs(x=Xname,y=Lname)
            }
          } 
          #gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol11],y=Data1[,input$Ycol11])) + geom_point()
          ggplotly(gplot)
          
        }
      }
    }
  })
    
  output$plot205 <- renderPlotly({
    if(input$analysis == "Similarity_of_Samples1"){
      if(input$Dimension_for_clustering == "Dimension_2"){
        if(input$Dimension_Reduction == "Factor1") {
          
          
          req(input$file1)
          
          if(input$sep2 == "Separator_Comma"){sep <- ","}
          if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
          if(input$sep2 == "Separator_Tab"){sep <- "\t"}
          
          
          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
          #if(input$DoNotUseFirst == 1){
          #  Data[,1] <- NULL
          #}
          
          library(psych)
          library(GPArotation)
          library(heatmaply)
          library(fastDummies) 
          library(psych)
          library(ggplot2)
          
          
          
          
          #Y <- names(Data[1])
          #Ydata <- Data[,1]
          #Data[,1] <- NULL
          
          #Data1 <- Data
          if(input$Use_one_row_as_sample_name2 == 1){
            Y <- names(Data[input$sample_row2])
            Ydata <- Data[,input$sample_row2]
            Data[,input$sample_row2] <- NULL
          } else {
            Y <- "Index_No"
            Ydata <- row.names(Data)
          }
          
          
          Check_variable <- "0"
          for (i in 1:ncol(Data)) {
            if (class(Data[,i]) != "numeric") {
              if (class(Data[,i]) != "integer") {
                Check_variable <- "1"
              }
            }  
          }
          if(Check_variable == "1"){
            Data1 <- dummy_cols(Data,remove_first_dummy = TRUE,remove_selected_columns = TRUE)
          } else {
            Data1 <- Data
          }
          
          fa_result <- fa(Data1, nfactors = input$Factors, fm = "ml", rotate = input$Factor_Rotation2)
          output$plot407 <- renderPlotly(heatmaply(fa_result$loadings, scale="none"))
          output$text407 <- renderPrint(fa_result$loadings)
          
          
          fa_loadings <- fa_result$loadings[,1:input$Factors]
          Data11 <- as.data.frame(fa_loadings)
          Data11_dist <- dist(Data11)
          sn <- sammon(Data11_dist)
          Data2 <- sn$points
          name1 <-  row.names(Data11)
          Data2 <- cbind.data.frame(Data2 ,name1)
          output$plot409 <- renderPlotly(ggplotly(ggplot(Data2, aes(x=Data2[,1], y=Data2[,2],label=name1)) + geom_text()+ labs(y="axis2",x="axis1")))
          
          
          
          Data1 <- fa_result$scores
          Data1 <- as.data.frame(Data1)
          Data1$Index = row.names(Data1)
          
          if(input$Xcol13 > 0){Xname <- names(Data1[input$Xcol13])} else {Xname <- "None"}
          if(input$Ycol13 > 0){Lname <- names(Data1[input$Ycol13])} else {Lname <- "None"}
          
          
          if(input$plot_type2 == "G11") {
            gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol13],y=Data1[,input$Ycol13],label=Ydata)) + geom_text()  + labs(x=Xname,y=Lname)
          } else if(input$plot_type2 == "G12") {
            gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol13],y=Data1[,input$Ycol13],label=Index)) + geom_text() + labs(x=Xname,y=Lname)
          } else if(input$plot_type2 == "G13") {
            Data1$Index <- as.numeric(Data1$Index)
            gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol13],y=Data1[,input$Ycol13])) + geom_point(aes(colour=Ydata)) + labs(x=Xname,y=Lname)
          } else if(input$plot_type2 == "G14") {
            Data1$Index <- as.numeric(Data1$Index)
            gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol13],y=Data1[,input$Ycol13])) + geom_point(aes(colour=Index))  + scale_color_viridis_c(option = "D")+ labs(x=Xname,y=Lname)
          } else {
            gplot <- ggplot(Data1, aes(x=Data1[,input$Xcol13],y=Data1[,input$Ycol13])) + geom_point() + labs(x=Xname,y=Lname)
          }
          
          ggplotly(gplot)
          
        }
      }
    }
  })
    
  output$plot202 <- renderPlotly({
    if(input$analysis == "Similarity_of_Samples1"){
      if(input$Dimension_for_clustering == "Dimension_All"){
        
        req(input$file1)
        
        if(input$sep2 == "Separator_Comma"){sep <- ","}
        if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
        if(input$sep2 == "Separator_Tab"){sep <- "\t"}
        
        library(dummies)
        library(ggplot2)
        
        if(input$Use_one_row_as_sample_name2 == 1){
          Data <- read.csv(input$file1$datapath, header=T,sep = sep, row.names=input$sample_row2)
        } else {
          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
        }
        
        
        if(input$Method_Dimension_All == "hclust1"){
          
          library(ggdendro)
          
          Data10 <- Data
          Data11 <- Data10
          for (i in 1:ncol(Data10)) {
            Data11[,i] <- (Data10[,i] - min(Data10[,i]))/(max(Data10[,i]) - min(Data10[,i]))
          } 
          Data11_dist <- dist(Data11)
          hc <- hclust(Data11_dist, "ward.D2")
          ggplotly(ggdendrogram(hc, segments = TRUE, labels = TRUE, leaf_labels = TRUE, rotate = FALSE, theme_dendro = TRUE))
          
        } else{
          library(dbscan)
          
          Data2 <- dummy.data.frame(Data)
          Data3 <- Data2
          for (i in 1:ncol(Data2)) {
            Data3[,i] <- (Data2[,i]-min(Data2[,i]))/(max(Data2[,i])-min(Data2[,i]))
          }
          dbs <- dbscan(Data3, eps = input$eps_value2)
          Data7 <- transform(clust = dbs$cluster, Data)
          
          
          output$downloadData5 <- downloadHandler(
            filename = function() {
              paste("cluster_data", ".csv", sep = "")
            },
            content = function(file) {
              write.csv(Data7, file, row.names = TRUE)
            }
          )
          
          Data7$clust <- as.factor(Data7$clust)
          ggplotly(ggplot(Data7, aes(x=clust, y=clust)) + geom_bar(stat = "identity") +labs(x="Cluster name", y="Frequency"))
        }
      }
    }
  })
  
  output$plot401 <- renderPlot({
    if(input$analysis == "Similarity_of_Names_in_Rows_and_Columns1"){
      if(input$Similarity_of_Names_in_Rows_and_Columns == 'Bipartite_graph1') {
      
        req(input$file1)
        
        library(igraph)
        
        if(input$sep2 == "Separator_Comma"){sep <- ","}
        if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
        if(input$sep2 == "Separator_Tab"){sep <- "\t"}
        
        
        
        if(input$Use_one_row_as_sample_name2 == 1){
          DM <- read.csv(input$file1$datapath, header=T,sep = sep, row.names=input$sample_row2)
        } else {
          DM <- read.csv(input$file1$datapath, header=T,sep = sep)
        }
        
        DM.mat = as.matrix(DM)/max(DM)*5
        DM.mat[DM.mat<1] <- 0
        DM.g<-graph_from_incidence_matrix(DM.mat,weighted=T)
        V(DM.g)$color <- c("steel blue", "orange")[V(DM.g)$type+1]
        V(DM.g)$shape <- c("square", "circle")[V(DM.g)$type+1]
        if(input$Layout2 == "layout_default1"){
          plot(DM.g, edge.width=E(DM.g)$weight) 
        }else if(input$Layout2 == "layout_as_bipartite1"){
          plot(DM.g, edge.width=E(DM.g)$weight, layout = layout_as_bipartite)
        }else if(input$Layout2 == "layout_as_star1"){
          plot(DM.g, edge.width=E(DM.g)$weight, layout = layout_as_star) 
        }
      }
    }
  })
  
  output$plot405 <- renderPlotly({
    if(input$analysis == "Similarity_of_Names_in_Rows_and_Columns1"){
      if(input$Similarity_of_Names_in_Rows_and_Columns == 'Correspondence_MDS_Names1') {
      
        req(input$file1)
        
        if(input$sep2 == "Separator_Comma"){sep <- ","}
        if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
        if(input$sep2 == "Separator_Tab"){sep <- "\t"}
        
        
        if(input$Use_one_row_as_sample_name2 == 1){
          Data <- read.csv(input$file1$datapath, header=T,sep = sep, row.names=input$sample_row2)
        } else {
          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
        }
        library(MASS)
        library(ggplot2)
        pc <- corresp(Data,nf=min(ncol(Data),nrow(Data)))
        pc1 <- data.frame(pc$cscore)
        pc1 <- transform(pc1 ,name1 = rownames(pc1), name2 = "A")
        pc2 <- data.frame(pc$rscore)
        pc2 <- transform(pc2 ,name1 = rownames(pc2), name2 = "B")
        Data1 <- rbind(pc1,pc2)
        
        ei1 <- round(pc$cor^2/sum(pc$cor^2),2)
        output$text4051 <- renderText(ei1)
        for (i in 1: length(ei1)){
          if(ei1[i] > 0.001){
            n1 <- i
          }
        }
        output$text4052 <- renderText(n1)
        
        Data11 <- Data1[,1:n1]
        Data11_dist <- dist(Data11)
        sn <- sammon(Data11_dist)
        output <- sn$points
        Data2 <- cbind(output, Data1)
        ggplotly(ggplot(Data2, aes(x=Data2[,1], y=Data2[,2],label=name1)) + geom_text(aes(colour=name2)) + labs(y="axis2",x="axis1"))
      }
    }
  })
  
  output$text403 <- renderPrint({
    if(input$analysis == "Similarity_of_Names_in_Rows_and_Columns1"){
      if(input$Similarity_of_Names_in_Rows_and_Columns == 'Independence_Test1') {
        
        req(input$file1)
        
        if(input$sep2 == "Separator_Comma"){sep <- ","}
        if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
        if(input$sep2 == "Separator_Tab"){sep <- "\t"}
        
        if(input$Use_one_row_as_sample_name2 == 1){
          Data <- read.csv(input$file1$datapath, header=T,sep = sep, row.names=input$sample_row2)
        } else {
          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
        }
        
        chisq.test(Data)
      }
    }
  })
  output$text402 <- renderPrint({
    if(input$analysis == "Similarity_of_Names_in_Rows_and_Columns1"){
      if(input$Similarity_of_Names_in_Rows_and_Columns == 'Two_way_GLM1') {
      
        req(input$file1)
        
        if(input$sep2 == "Separator_Comma"){sep <- ","}
        if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
        if(input$sep2 == "Separator_Tab"){sep <- "\t"}
        
        Data <- read.csv(input$file1$datapath, header=T,sep = sep)
        
        if(input$Use_one_row_as_sample_name2 == 1){
          RowName <- colnames(Data[input$sample_row2])
        } else {
          RowName <- "Index_No"
          Data$Index_No <- row.names(Data)
        }
        
        library(tidyr)
        library(MASS)
        library(lme4)
        
        Data1 <- tidyr::gather(Data, key="ColName", value = Val, -RowName)
        if(input$family_link3 == "gaussian_identity"){
          anova(step(glm(Val~., data=Data1,family=gaussian))) 
        } else {
          anova(step(glm(Val~.^2, data=Data1,family=poisson))) 
        }
      }
    }
  })
  
  #output$plot403 <- renderPlotly({
  #  if(input$analysis == "Similarity_of_Names_in_Rows_and_Columns1"){
  #    if(input$Similarity_of_Names_in_Rows_and_Columns == 'Heat_map_Clustering1') {
        
  #      req(input$file1)
        
  #      library(heatmaply)
  #      if(input$sep2 == "Separator_Comma"){sep <- ","}
  #      if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
  #      if(input$sep2 == "Separator_Tab"){sep <- "\t"}
  #      Data <- read.csv(input$file1$datapath, header=T,sep = sep, row.names=1)
        
  #      DataM <- as.matrix(Data)
  #      heatmaply(DataM, scale="none")
  #    }
  #  }
  #})
  
  output$plot702 <- renderPlotly({
    if(input$analysis == "Time_series1"){
      if(input$Dimension_type == "Multi_variable"){
        if(input$Method5 == "Stratifeid_graph3"){
          
          req(input$file1)
          library(lme4)
          library(dplyr)
          library(plotly)
          library(ggplot2)
          
          if(input$sep2 == "Separator_Comma"){sep <- ","}
          if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
          if(input$sep2 == "Separator_Tab"){sep <- "\t"}
          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
          if(input$DoNotUseFirst == 1){
            Data[,1] <- NULL
          }
          Data1 <- Data
          
          if(input$Lcol3 > 0){Lname<-names(Data1[input$Lcol3])} else {Lname<-"None"}
          if(input$Ccol3 > 0){Cname<-names(Data1[input$Ccol3])} else {Cname<-"None"}
          if(input$Scol3 > 0){Sname<-names(Data1[input$Scol3])} else {Sname<-"None"}
          
          Data1$No <-as.numeric(row.names(Data1)) 
          
          if(input$Scol3 != 0 && input$Ccol3 != 0){
            if(class(Data1[,input$Scol3]) == "numeric" || class(Data1[,input$Scol3]) == "integer"){Data1[,input$Scol3] <- droplevels(cut(Data1[,input$Scol3], breaks = input$NumericalToCategorcalS3, include.lowest = TRUE))}
            if(input$NumericalToCategorcalSColor23 == 1){
              if(class(Data1[,input$Ccol3]) == "numeric" || class(Data1[,input$Ccol3]) == "integer"){Data1[,input$Ccol3] <- droplevels(cut(Data1[,input$Ccol3], breaks = input$NumericalToCategorcalS3, include.lowest = TRUE))}
            }
            if(input$NumericalToCategorcalSColor13 == 1){Data1[,input$Ccol3] <- as.factor(Data1[,input$Ccol3])}
            
            gplot <- ggplot(Data1, aes(x=Data1$No,y=Data1[,input$Lcol3])) + geom_line(aes(colour=Data1[,input$Ccol3])) + facet_wrap(~Data1[,input$Scol3],scales="free")+ labs(x="Index",y=Lname,color=Cname,subtitle = Sname)
          } else if(input$Scol3 == 0 && input$Ccol3 != 0){
            if(input$NumericalToCategorcalSColor23 == 1){
              if(class(Data1[,input$Ccol3]) == "numeric" || class(Data1[,input$Ccol3]) == "integer"){Data1[,input$Ccol3] <- droplevels(cut(Data1[,input$Ccol3], breaks = input$NumericalToCategorcalS3, include.lowest = TRUE))}
            }
            if(input$NumericalToCategorcalSColor13 == 1){Data1[,input$Ccol3] <- as.factor(Data1[,input$Ccol3])}
            
            gplot <- ggplot(Data1, aes(x=Data1$No,y=Data1[,input$Lcol3])) + geom_line(aes(colour=Data1[,input$Ccol3])) + labs(x="Index",y=Lname,color=Cname)
          } else if(input$Scol != 0 && input$Ccol == 0){
            if(class(Data1[,input$Scol3]) == "numeric" || class(Data1[,input$Scol3]) == "integer"){Data1[,input$Scol3] <- droplevels(cut(Data1[,input$Scol3], breaks = input$NumericalToCategorcalS3, include.lowest = TRUE))}
            
            gplot <- ggplot(Data1, aes(x=Data1$No,y=Data1[,input$Lcol3])) + geom_line() + facet_wrap(~Data1[,input$Scol3],scales="free")+ labs(x="Index",y=Lname,subtitle = Sname)
          } else {
            
            gplot <- ggplot(Data1, aes(x=Data1$No,y=Data1[,input$Lcol3])) + geom_line() + labs(x="Index",y=Lname)
          }
          ggplotly(gplot)
        }
      }
    }
  })
  
  output$plot701 <- renderPlotly({
    if(input$analysis == "Time_series1"){
      if(input$Dimension_type == "Multi_variable"){
        if(input$Method5 == "Dimension_reduction3"){
          
          req(input$file1)
          
          if(input$sep2 == "Separator_Comma"){sep <- ","}
          if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
          if(input$sep2 == "Separator_Tab"){sep <- "\t"}
          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
          if(input$DoNotUseFirst == 1){
            Data[,1] <- NULL
          }
          library(ggplot2)
          library(tidyr)
          library(dummies)
          library(fastICA)
          library(psych)
          library(GPArotation)
          
          if(input$Dimension_reduction_type3 == "None3"){
            Data2 <- dummy.data.frame(Data)
          } else if(input$Dimension_reduction_type3 == "PCA3"){
            pc <- prcomp(Data, scale=TRUE,tol=0.01)
            Data2 <- as.data.frame(pc$x)
            output$text702 <- renderPrint(summary(pc))
            
          } else if(input$Dimension_reduction_type3 == "ICA3"){
            pc <- prcomp(Data, scale=TRUE, tol=0.01)
            ic <- fastICA(Data, ncol(pc$x))
            Data2 <- as.data.frame(ic$S)
            output$text702 <- renderPrint(summary(ic))
            
          } else if(input$Dimension_reduction_type3 == "Factor3"){
            fa_result <- fa(Data, nfactors = input$Factors3, fm = "ml", rotate = input$Factor_Rotation3)
            Data2 <- as.data.frame(fa_result$scores)
            output$text702 <- renderPrint(fa_result$loadings)
          }
          Data2$No <-as.numeric(row.names(Data2))
          
          output$downloadData714 <- downloadHandler(
            filename = function() {
              paste("Dimension_reduction", ".csv", sep = "")
            },
            content = function(file) {
              write.csv(Data2, file, row.names = FALSE)
            }
          )
          
          Data_long <- tidyr::gather(Data2, key="Yno", value = Ys, -No)
          
          if(input$Line_graph2 == "Box_Integrated1"){
            gplot <- ggplot(Data_long, aes(x=No,y=Ys, colour=Yno)) + geom_line()
          } else {
            gplot <- ggplot(Data_long, aes(x=No,y=Ys)) + geom_line() + facet_wrap(~Yno,scales="free")
          }
          ggplotly(gplot)
        }
      }  
    }
  })
  
  
  output$plot725 <- renderPlot({
    if(input$analysis == "Time_series1"){
      if(input$Dimension_type == "Multi_variable"){
        if(input$Method5 == "Cross_correlation"){
          
          req(input$file1)
          
          library(plotly)
          library(ggplot2)
          library(tidyr)
          if(input$sep2 == "Separator_Comma"){sep <- ","}
          if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
          if(input$sep2 == "Separator_Tab"){sep <- "\t"}
          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
          if(input$DoNotUseFirst == 1){
            Data[,1] <- NULL
          }
          
          output$plot726 <- renderPlotly({
            Data$No <-as.numeric(row.names(Data))
            Data2 <- Data$No
            Data2 <- as.data.frame(Data2)
            Data2$X <- Data[,input$Xcol4]
            Data2$Y <- Data[,input$Ycol4]
            Data_long <- tidyr::gather(Data2, key="Yno", value = Ys, -Data2)
            gplot <- ggplot(Data_long, aes(x=Data2,y=Ys)) + geom_line() + facet_grid(Yno~.)+labs(x="No",y="Ys")
            ggplotly(gplot)
          })
          Value_to_analyze_X <- input$Xcol4
          Value_to_analyze_Y <- input$Ycol4
          ccf(Data[,Value_to_analyze_X],Data[,Value_to_analyze_Y])   
        }
      }  
    }
  })
  
  
  output$plot501 <- renderPlotly({
    if(input$analysis == "Time_series1"){
      if(input$Dimension_type == "One_variable"){
        if(input$Method4 == "Difference_previous"){
            
          req(input$file1)
          
          if(input$sep2 == "Separator_Comma"){sep <- ","}
          if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
          if(input$sep2 == "Separator_Tab"){sep <- "\t"}
          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
          if(input$DoNotUseFirst == 1){
            Data[,1] <- NULL
          }
          
          library(ggplot2)
          
          
          Data_diff <- diff(Data[,input$Value_to_analyze],input$Lag_of_diff)
          Data_diff <- as.data.frame(Data_diff)
          Data_diff$No <-as.numeric(row.names(Data_diff))+1
          Lname<-names(Data[input$Value_to_analyze])
          gplot <- ggplot(Data_diff, aes(x = No,y=Data_diff)) + geom_line()
          
          output$plot502 <- renderPlotly({
            gplot2 <- ggplot(Data_diff, aes(x =Data_diff)) + geom_histogram()
            ggplotly(gplot2)
          })
          output$plot503 <- renderPlotly({
            Data$No <-as.numeric(row.names(Data))
            gplot2 <- ggplot(Data, aes(x =No, y=Data[,input$Value_to_analyze])) + geom_line()+ labs(x="No",y=Lname)
            ggplotly(gplot2)
          })
          ggplotly(gplot)
          
        }
      }
    }
  })
  
  output$plot721 <- renderPlotly({
    if(input$analysis == "Time_series1"){
      if(input$Dimension_type == "One_variable"){
        if(input$Method4 == "fft"){
          
          req(input$file1)
          
          library(plotly)
          library(ggplot2)
          if(input$sep2 == "Separator_Comma"){sep <- ","}
          if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
          if(input$sep2 == "Separator_Tab"){sep <- "\t"}
          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
          if(input$DoNotUseFirst == 1){
            Data[,1] <- NULL
          }
          n <- nrow(Data)
          ft <- log10(abs(fft(Data[,input$Value_to_analyze]^2)))
          Data2 <- as.data.frame(ft)
          Data2$Index <- as.numeric(row.names(Data2))
          Data3 <- Data2[2:floor(n/2),]
          Data3$Wavelength <- n / (Data3$Index-1)
          
          gplot2 <- ggplot(Data3, aes(x =Wavelength, y=ft)) + geom_line()+labs(x="Wavelength",y="Power")
          
          
          
          output$plot722 <- renderPlotly({
            Lname<-names(Data[input$Value_to_analyze])
            Data$No <-as.numeric(row.names(Data))
            gplot <- ggplot(Data, aes(x =No, y=Data[,input$Value_to_analyze])) + geom_line()+ labs(x="No",y=Lname)
            ggplotly(gplot)
          })
          
          ggplotly(gplot2)            
        }
      }  
    }
  })
  output$plot723 <- renderPlot({
    if(input$analysis == "Time_series1"){
      if(input$Dimension_type == "One_variable"){
        if(input$Method4 == "Auto_correlation"){
          
          req(input$file1)
          
          library(plotly)
          library(ggplot2)
          if(input$sep2 == "Separator_Comma"){sep <- ","}
          if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
          if(input$sep2 == "Separator_Tab"){sep <- "\t"}
          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
          if(input$DoNotUseFirst == 1){
            Data[,1] <- NULL
          }
          
          output$plot724 <- renderPlotly({
            Lname<-names(Data[input$Value_to_analyze])
            Data$No <-as.numeric(row.names(Data))
            gplot <- ggplot(Data, aes(x =No, y=Data[,input$Value_to_analyze])) + geom_line()+ labs(x="No",y=Lname)
            ggplotly(gplot)
          })
          Value_to_analyze <- input$Value_to_analyze
          acf(Data[,Value_to_analyze])   
        }
      }  
    }
  })
  
  output$text701 <- renderDataTable({
    if(input$analysis == "Time_series1"){
      if(input$Dimension_type == "One_variable"){
        if(input$Method4 == "Quasi_periodic"){
          
          req(input$file1)
          
          if(input$sep2 == "Separator_Comma"){sep <- ","}
          if(input$sep2 == "Separator_Semicolon"){sep <- ";"}
          if(input$sep2 == "Separator_Tab"){sep <- "\t"}
          Data <- read.csv(input$file1$datapath, header=T,sep = sep)
          if(input$DoNotUseFirst == 1){
            Data[,1] <- NULL
          }
          
          library(ggplot2)
          library(dplyr)
          
          
          if(input$Using_01variable_in_table == "Using_01variable_in_table_Y"){
            Data$Value01 <- Data[,input$Row_of_01variable]
          } else {
            Data$Value01 <- ifelse(Data[,input$Row_to_divide01] > input$Value_to_0or1, 1, 0)
          }
          Data$Index <-as.numeric(row.names(Data)) 
          Data$Value02 <- Data[,input$Value_to_analyze]
          
          Data$Index_Set1to0 <-Data[,1]
          Data$Set1to0 <-Data[,1]
          Data$Index_Set0to1 <-Data[,1]
          Data$Set0to1 <-Data[,1]
          n <- nrow(Data)
          Data$Index_Set1to0 <- 0
          Data$Set1to0 <- 0
          Data$Index_Set0to1 <- 0
          Data$Set0to1 <- 0
          
          for (i in 2:n) {
            if (Data$Value01[i-1] == 0 &&  Data$Value01[i] == 1) {
              Data$Index_Set1to0[i] <- 0
              Data$Set1to0[i] <- Data$Set1to0[i-1] +1
              Data$Index_Set0to1[i] <- Data$Index_Set0to1[i-1] +1
              Data$Set0to1[i] <- Data$Set0to1[i-1]
            } else if (Data$Value01[i-1] == 1 && Data$Value01[i] == 0){
              Data$Index_Set1to0[i] <- Data$Index_Set1to0[i-1] +1
              Data$Set1to0[i] <- Data$Set1to0[i-1]
              Data$Index_Set0to1[i] <- 0
              Data$Set0to1[i] <- Data$Set0to1[i-1] +1
            } else {
              Data$Index_Set1to0[i] <- Data$Index_Set1to0[i-1] +1
              Data$Set1to0[i] <- Data$Set1to0[i-1]
              Data$Index_Set0to1[i] <- Data$Index_Set0to1[i-1]
              Data$Set0to1[i] <- Data$Set0to1[i-1]
            }
          }
          
          Data42 <- Data %>% group_by(Set0to1)
          Data43 <- summarize(Data42, n_01 = n(), Max_01 = max(Value02), Min_01 = min(Value02), Mean_01 = mean(Value02))
          
          
          Data21 <- Data[Data$Value01 == "0",]
          Data22 <- Data21 %>% group_by(Set0to1)
          Data23 <- summarize(Data22, n_0 = n(), Max_0 = max(Value02), Min_0 = min(Value02), Mean_0 = mean(Value02))
          #Data23 <- summarize(Data22, n_0 = n(), Max_0 = max(Value02), Min_0 = min(Value02))
          Data31 <- Data[Data$Value01 == "1",]
          Data32 <- Data31 %>% group_by(Set0to1)
          Data33 <- summarize(Data32, n_1 = n(), Max_1 = max(Value02), Min_1 = min(Value02), Start_Index_1 = min(Index), End_Index_1 = max(Index), Mean_1 = mean(Value02))
          #Data33 <- summarize(Data32, n_1 = n(), Max_1 = max(Value02), Min_1 = min(Value02), Start_Index_1 = min(Index), End_Index_1 = max(Index))
          #Data33 <- summarize(Data32, n_1 = n())
          Data4 <- merge(Data23, Data33, all=T)
          Data4 <- merge(Data4, Data43, all=T)
          
          
          output$downloadData700 <- downloadHandler(
            filename = function() {
              paste("Quasi_periodic_type1", ".csv", sep = "")
            },
            content = function(file) {
              write.csv(Data, file, row.names = FALSE)
            }
          )
          output$downloadData704 <- downloadHandler(
            filename = function() {
              paste("Quasi_periodic_type2", ".csv", sep = "")
            },
            content = function(file) {
              write.csv(Data4, file, row.names = FALSE)
            }
          )
          Data4
        }
      }
    }
  })
  
  

  
  
})
