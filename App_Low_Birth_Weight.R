library(shiny)
library(ggplot2)
library(AICcmodavg)
library(ROCR)

ui <- fluidPage(
  titlePanel("Regression Model (Dataset: Low Birth Weight)"),
  
  #Upload the file
  sidebarLayout(
    sidebarPanel(
      # Input: Select a file
      fileInput("file1", "Select File : (Only well formatted .csv)",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      # Horizontal line
      tags$hr(),
      # Input: Select separator
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Space = " "),
                   selected = ",")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  # Tab-1: All the Box Plots of the Project
                  tabPanel("Plots", 
                           tabsetPanel(type = "tabs",
                                       tabPanel("Response vs Co-variates",
                                                fluidRow(column(10,plotOutput(outputId = "bprc1"))),
                                                fluidRow(column(10,plotOutput(outputId = "bprc2"))),
                                                fluidRow(column(10,plotOutput(outputId = "bprc3"))),
                                                fluidRow(column(10,plotOutput(outputId = "bprc4"))),
                                                fluidRow(column(10,plotOutput(outputId = "bprc5"))),
                                                fluidRow(column(10,plotOutput(outputId = "bprc6"))),
                                                fluidRow(column(10,plotOutput(outputId = "bprc7"))),
                                                fluidRow(column(10,plotOutput(outputId = "bprc8")))
                                       ),
                                       tabPanel("Interaction Co-variates",
                                                fluidRow(column(10,plotOutput(outputId = "bpic1"))),
                                                fluidRow(column(10,plotOutput(outputId = "bpic2"))),
                                                fluidRow(column(10,plotOutput(outputId = "bpic3"))),
                                                fluidRow(column(10,plotOutput(outputId = "bpic4"))),
                                                fluidRow(column(10,plotOutput(outputId = "bpic5"))),
                                                fluidRow(column(10,plotOutput(outputId = "bpic6"))),
                                                fluidRow(column(10,plotOutput(outputId = "bpic7"))),
                                                fluidRow(column(10,plotOutput(outputId = "bpic8")))
                                       )
                           )), 
                  
                  # Tab-2: AIC of the Model 
                  tabPanel("AIC", 
                           tabsetPanel(type = "tabs",
                                       tabPanel("Half Model",
                                                fluidRow(column(10, verbatimTextOutput("aic1")))),
                                       tabPanel("Full Model",
                                                fluidRow(column(10, verbatimTextOutput("aic2"))))
                           )),
                  
                  # Tab-3: Summary of the Model
                  tabPanel("Model Summary",  
                           tabsetPanel(type = "tabs",
                                       tabPanel("Half Models",
                                                fluidRow(column(10,h4("|Half-Model|"),verbatimTextOutput(outputId = "hs1"))),
                                                fluidRow(column(10,h4("|Half-Model|-FTV:LWT"),verbatimTextOutput(outputId = "hs2"))),
                                                fluidRow(column(10,h4("|Half-Model|-RACE"),verbatimTextOutput(outputId = "hs3")))
                                       ),
                                       tabPanel("Full Models",
                                                fluidRow(column(10,h4("|Full-Model1|"),verbatimTextOutput(outputId = "fs1"))),
                                                fluidRow(column(10,h4("|Full-Model2|"),verbatimTextOutput(outputId = "fs2"))),
                                                fluidRow(column(10,h4("|Full-Model3|"),verbatimTextOutput(outputId = "fs3"))),
                                                fluidRow(column(10,h4("|Full-Model4|"),verbatimTextOutput(outputId = "fs4"))),
                                                fluidRow(column(10,h4("|Full-Model5|"),verbatimTextOutput(outputId = "fs5")))
                                       )
                           )
                  ),
                  # Tab-4: Raw data of the model
                  tabPanel("Data", tableOutput("tbl")),# Data as datatable
                  
                  # Tab-5: Correlation Analysis
                  tabPanel("Correlation R",
                           fluidRow(column(4,h4("Matrix"),verbatimTextOutput("correlation")))),
                  
                  # Tab-6: Final Results
                  tabPanel("Final Models",
                           fluidRow(column(10,h4("Half Model Summary"),verbatimTextOutput(outputId = "hms"))),
                           fluidRow(column(10,h4("Half Model Coefficients"),verbatimTextOutput(outputId = "hme"))),
                           fluidRow(column(10,plotOutput(outputId = "hm1"))),
                           fluidRow(column(10,h4("Full Model Summary"),verbatimTextOutput(outputId = "fms"))),
                           fluidRow(column(10,h4("Full Model Coefficients"),verbatimTextOutput(outputId = "fme"))),
                           fluidRow(column(10,plotOutput(outputId = "fm1")))
                           )
      )      
    )
  )
)

# SERVER
server <- function(input, output) {
  
  # All  Rendering ------------~~~~~~~~~~~~~~~~~~~~-------------------
  
  #1. Display the >>>>DATA<<<< that has been uploaded:
  output$tbl <- renderTable({
    req(input$file1)
    fileData()
  })
  
  #2. Function to read >>>>DATA<<<< from the input file: Reads and persists data
  fileData <- eventReactive(input$file1,{
    read.csv(input$file1$datapath)
  })
  
  #3. Create >>>>BOX PLOTS<<<< - Interactions: LWT vs Co-variates - supported by Function: generatePlots
  output$bprc1 <- renderPlot({
    if(is.null(fileData()))
      return(NULL)
    theData = formatCategory(fileData())
    #Supply Plots -
    output[["bprc1"]] <- renderPlot({generatePlots("UI","BWT",theData)})
    output[["bprc2"]] <- renderPlot({generatePlots("HT","BWT",theData)})
    output[["bprc3"]] <- renderPlot({generatePlots("RACE","BWT",theData)})
    output[["bprc4"]] <- renderPlot({generatePlots("SMOKE","BWT",theData)}) 
    output[["bprc5"]] <- renderPlot({generatePlots("PTL","BWT",theData)})
    output[["bprc6"]] <- renderPlot({generatePlots("FTV","BWT",theData)})
    output[["bprc7"]] <- renderPlot({generatePlots("AGE","BWT",theData)})
    output[["bprc8"]] <- renderPlot({generatePlots("LWT","BWT",theData)})
    
  })
  
  #4. Create >>>>BOX PLOTS | LINE PLOTS<<<< to Self Interactions: supported by Function - generatePlots
  output$bpic1 <- renderPlot({
    if(is.null(fileData()))
      return(NULL)
    #Supply Plots - 
    theData = formatCategory(fileData())
    output[["bpic1"]] <- renderPlot({generatePlots("PTL","BWT",theData,"UI")})
    output[["bpic2"]] <- renderPlot({generatePlots("AGE","BWT",theData,"FTV")})
    output[["bpic3"]] <- renderPlot({generatePlots("LWT","BWT",theData,"FTV")})
    output[["bpic4"]] <- renderPlot({generatePlots("AGE","BWT",theData,"SMOKE")})
    output[["bpic5"]] <- renderPlot({generatePlots("PTL","BWT",theData,"UI")})
    output[["bpic6"]] <- renderPlot({generatePlots("RACE","BWT",theData,"SMOKE")})
    #
    output[["bpic7"]] <- renderPlot({generateLinePlots(theData$UI,theData$PTL,theData$BWT,c( "#bdc9e1", "#74a9cf", "#2b8cbe", "#045a8d"),
                                                       "Distribution For BWT By UI Status \n For Each PTL Count",
                                                       "UI","Mean BWT", "PTL")})
    output[["bpic8"]] <- renderPlot({generateLinePlots(theData$RACE,theData$SMOKE,theData$BWT,c("#74a9cf", "#045a8d"),
                                                       "Distribution For BWT By Age \n For Smokers and Non-Smokers",
                                                       "RACE","Mean BWT", "SMOKE")})
  })
  
  #5. Regression output - >>>>HALF MODELS<<<< - supported by Function: 
  output$hs1 <- renderPrint({
    generateHalfModels(fileData())
  })
  
  #6. Regression output- >>>>FULL MODELS<<<< - supported by Function: 
  output$fs1 <- renderPrint({
    generateFullModels(fileData())
  })
  
  #7. >>>>AIC<<< for Half Models
  output$aic1 <- renderPrint({
    half_mod=glm(LOW~AGE+RACE+FTV+LWT+AGE:FTV+LWT:FTV, data=fileData(), family=binomial(link="logit"))
    step.model <- step(half_mod, direction = "both")#using stepwise selection using AIC
    low.list=list()
    low.list[[1]]=glm(LOW~AGE+RACE+FTV+LWT, data=fileData(), family=binomial(link="logit"))#Model of main predictors only -- full
    low.list[[2]]=glm(LOW~AGE+RACE+FTV+LWT+AGE:FTV+LWT:FTV, data=fileData(), family=binomial(link="logit"))#full model 
    low.list[[3]]=glm(LOW~AGE+RACE+FTV+LWT+AGE:FTV, data=fileData(), family=binomial(link="logit"))#Main effects + AGE:FTV interaction -- full_AgeFtv
    low.list[[4]]=glm(LOW~AGE+RACE+FTV+LWT+LWT:FTV, data=fileData(), family=binomial(link="logit")) #Main effects + LWT:FTV interaction --full_LwtFtv
    low.list[[5]]=glm(LOW~AGE*FTV+LWT, data=fileData(), family=binomial(link="logit"))#Main effects without Race -- noRace
    low.list[[6]]=glm(LOW~AGE*FTV+RACE, data=fileData(), family=binomial(link="logit"))#Main effects without LWT -- noLWT
    low.modnames=c("full", "full_interaction", "full_AgeFtv", "full_LwtFtv", "noRACE", "noLWT")
    low.aictab=aictab(cand.set = low.list,modnames = low.modnames)
    low.aictab
  })
  
  #8. >>>>AIC<<<< for Full Models
  output$aic2 <- renderPrint({
    full.model=glm(LOW ~ AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+FTV:LWT+SMOKE:RACE+AGE:SMOKE+PTL:UI, data=fileData(), family=binomial(link="logit"))
    step.model <- step(full.model, direction = "both")#using stepwise selection using AIC
    low.list.full=list()
    low.list.full[[1]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE, data=fileData(), family=binomial(link="logit"))#full
    low.list.full[[2]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+FTV:LWT+SMOKE:RACE+AGE:SMOKE+PTL:UI+HT:LWT, data=fileData(), family=binomial(link="logit")) #full_interaction
    low.list.full[[3]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV, data=fileData(), family=binomial(link="logit")) #full_AgeFtv
    low.list.full[[4]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+FTV:LWT, data=fileData(), family=binomial(link="logit"))#full_FtvLwt
    low.list.full[[5]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+SMOKE:RACE, data=fileData(), family=binomial(link="logit"))#full_SmokeRace
    low.list.full[[6]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+AGE:SMOKE, data=fileData(), family=binomial(link="logit"))#full_AgeSmoke
    low.list.full[[7]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+PTL:UI, data=fileData(), family=binomial(link="logit"))#full_PtlUi
    low.list.full[[8]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+HT:LWT, data=fileData(), family=binomial(link="logit"))#full_HtLwt
    low.list.full[[9]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+PTL:UI, data=fileData(), family=binomial(link="logit"))#noAGEFTV 
    low.list.full[[10]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+SMOKE+PTL:UI+AGE:FTV, data=fileData(), family=binomial(link="logit")) #noRACE
    low.list.full[[11]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+PTL:UI+AGE:FTV, data=fileData(), family=binomial(link="logit"))#noSmokeRACE
    low.list.full[[12]]=glm(LOW~FTV+PTL+LWT+UI+HT+SMOKE+RACE+PTL:UI, data=fileData(), family=binomial(link="logit"))#noAGE
    low.list.full[[13]]=glm(LOW~AGE+FTV+LWT+SMOKE+RACE+PTL+AGE:FTV, data=fileData(), family=binomial(link="logit"))#noUiHt
    low.modnames.full=c("full", "full_interaction", "full_AgeFtv", "full_FtvLwt", "full_SmokeRace", "full_AgeSmoke", "full_PtlUi", "full_HtLwt", "noAGEFTV", "noRACE", "noSmokeRACE", "noAGE", "noUiHt")
    low.aictab.full=aictab(cand.set = low.list.full,modnames = low.modnames.full)
    low.aictab.full
  })
  
  #9. >>>>Correlations<<<<
  output$correlation <- renderPrint({
    lowbwt = fileData()
    p_age <- cor.test(lowbwt$BWT, lowbwt$AGE, method="spearman",exact = FALSE)$p.value
    p_lwt <- cor.test(lowbwt$BWT, lowbwt$LWT, method="spearman",exact = FALSE)$p.value
    p_ftv <- cor.test(lowbwt$BWT, lowbwt$FTV, method="spearman",exact = FALSE)$p.value
    p_ptl <- cor.test(lowbwt$BWT, lowbwt$PTL, method="spearman",exact = FALSE)$p.value
    rho_age <- cor.test(lowbwt$BWT, lowbwt$AGE, method="spearman",exact = FALSE)$estimate
    rho_lwt <- cor.test(lowbwt$BWT, lowbwt$LWT, method="spearman",exact = FALSE)$estimate
    rho_ftv <- cor.test(lowbwt$BWT, lowbwt$FTV, method="spearman",exact = FALSE)$estimate
    rho_ptl <- cor.test(lowbwt$BWT, lowbwt$PTL, method="spearman",exact = FALSE)$estimate
    correlations.df <- data.frame(c(rho_age, rho_lwt, rho_ftv, rho_ptl),c(p_age, p_lwt, p_ftv, p_ptl), row.names=c("AGE", "LWT", "FTV", "PTL"))
    colnames(correlations.df) <- c("rho", "p")
    round(correlations.df,3)
  })
  
  #10. >>>>Final Models<<<<
  output$hms <- renderPrint({
    generateResults(fileData())
  })
  
  #------------~~~~~~~~~~~~--------------------------------------------
  
  #All Helper functions
  
  #1. Function to factorize all the categorical variable
  formatCategory <- function(dataSet,check=NULL){
    dataSet$HT=as.factor(dataSet$HT)
    dataSet$LOW=as.factor(dataSet$LOW)
    dataSet$RACE=as.factor(dataSet$RACE)
    dataSet$SMOKE=as.factor(dataSet$SMOKE)
    if(is.null(check)){
    dataSet$PTL=as.factor(dataSet$PTL)
    dataSet$FTV=as.factor(dataSet$FTV)
    dataSet$UI=as.factor(dataSet$UI)
    }
    return(dataSet)
  }
  
  
  #2. Function to process Data for Box Plots: should return the plotVar variable
  generatePlots <- function(independentVar,dependentVar,studyData,based=NULL){
    plotVar = ggplot(studyData, aes_string(x = independentVar, y = dependentVar)) +
      geom_boxplot(colour = "#1F3552", fill = "#4271AE",size = 1)+
      scale_y_continuous(name = dependentVar)+
      scale_x_discrete(name = independentVar)+
      #ggtitle(paste("Boxplot of",independentVar, "vs",dependentVar," ")) +
      theme_bw() +
      theme(panel.grid.major = element_line(colour = "#d3d3d3"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
            text=element_text(family = "Tahoma"),
            axis.title = element_text(face="bold"),
            axis.text.x = element_text(colour="black", size = 11),
            axis.text.y = element_text(colour="black", size = 9),
            axis.line = element_line(size=0.5, colour = "black"))
            if(is.null(based)){
              print(plotVar+ggtitle(paste("Boxplot of",independentVar, "vs",dependentVar," "))+geom_jitter()) 
            }else{ 
              print(plotVar+ggtitle(paste("Boxplot of",independentVar, "vs",dependentVar,"based on",based))+facet_wrap(based)+geom_jitter())
            }
      
  }
  
  #3. Method to Generate Half MOdels
  generateHalfModels <- function(studyData){
    studyData = formatCategory(studyData,1)
    half_mod1=glm(LOW~AGE+RACE+FTV+LWT+AGE:FTV+LWT:FTV, data=studyData, family=binomial(link="logit")) #model 1: All 4 variables
    half_mod2=glm(LOW~AGE+RACE+FTV+LWT+AGE:FTV, data=studyData, family=binomial(link="logit")) #model 2: All 4 variable - FTV:LWT
    half_mod3=glm(LOW~AGE+FTV+LWT+AGE:FTV, data=studyData, family=binomial(link="logit")) #model 3: All 4 variable - RACE
    
    output[["hs1"]] <- renderPrint({summary(half_mod1)})
    output[["hs2"]] <- renderPrint({summary(half_mod2)})
    output[["hs3"]] <- renderPrint({summary(half_mod3)})
    
  }
  
  #4. Function to Generate Full Models
  generateFullModels <- function(studyData){
    studyData = formatCategory(studyData,1)
    full_mod1=glm(LOW ~ AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+FTV:LWT+SMOKE:RACE+AGE:SMOKE+PTL:UI, data=studyData, family=binomial(link="logit")) #All 
    full_mod2=glm(LOW ~ AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+FTV:LWT+AGE:SMOKE+PTL:UI, data=studyData, family=binomial(link="logit")) #model 2 without SMOKE:RACE
    full_mod3=glm(LOW ~ AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+FTV:LWT+PTL:UI, data=studyData, family=binomial(link="logit")) #model 3 without SMOKE:AGE
    full_mod4=glm(LOW ~ AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+PTL:UI, data=studyData, family=binomial(link="logit"))#model 4 without FTV:LWT
    full_mod5=glm(LOW ~ AGE+FTV+PTL+LWT+UI+HT+SMOKE+AGE:FTV+PTL:UI, data=studyData, family=binomial(link="logit")) #model 5 without RACE
    
    output[["fs1"]] <- renderPrint({summary(full_mod1)})
    output[["fs2"]] <- renderPrint({summary(full_mod2)})
    output[["fs3"]] <- renderPrint({summary(full_mod3)})
    output[["fs4"]] <- renderPrint({summary(full_mod4)})
    output[["fs5"]] <- renderPrint({summary(full_mod5)})
    
  }
  
  #5. Function to Generate Results
  generateResults <- function(studyData){
    final_half_model=glm(LOW~AGE+FTV+LWT+AGE:FTV,data=studyData,family=binomial(link="logit"))
    final_full_model=glm(LOW~AGE*FTV+PTL*UI+LWT+HT+AGE:FTV,data=studyData,family=binomial(link="logit"))
    
    output[["hms"]] <- renderPrint({summary(final_half_model)})
    output[["hme"]] <- renderPrint({exp(final_half_model$coefficients)})
    output[["hm1"]] <- renderPlot({
      predict<-fitted(final_half_model)
      pred<- prediction(predict,studyData$LOW)
      perf<-performance(pred,measure="tpr",x.measure="fpr")
      plot(perf,main="|Half Model| -sensitivity vs false positive rate",colorize=TRUE, colorkey.relwidth=0.5,lwd=4.5)
    })
    #
    output[["fms"]] <- renderPrint({summary(final_full_model)})
    output[["fme"]] <- renderPrint({exp(final_full_model$coefficients)})
    output[["fm1"]] <- renderPlot({
      predict<-fitted(final_full_model)
      pred<- prediction(predict,studyData$LOW)
      perf<-performance(pred,measure="tpr",x.measure="fpr")
      plot(perf,main="|FUll Model| -sensitivity vs false positive rate",colorize=TRUE, colorkey.relwidth=0.5,lwd=4.5)
    })
  }
  
  #6. Function to process Data for Line Plots
  generateLinePlots <- function(x,y,z,Lcol,Lmain,Lxlab,Lylab,Ltrace){
    interaction.plot(x,y,z, 
                     col = Lcol, lty = 1, lwd = 3,
                     main = Lmain,
                     xlab = Lxlab, ylab = Lylab, trace.label = Ltrace,
                     cex.lab = 1.2, 
                     cex.main = 1, 
                     font.main=2,
                     fixed=T, 
                     xpd = FALSE)
  }
}
shinyApp(ui = ui, server = server)
