##### load useful libraries #####
#---------------------------#
#install.packages("car")
#install.packages("ggplot2")
#install.packages("psych")
#install.packages("readxl")
#install.packages("corpcor")
#install.packages("GPArotation")

library(car)
library(ggplot2)
library(psych)
library(readxl)
library(corpcor)
library(GPArotation)


##### open files #####
#--------------------#
setwd("M:/Memory_project_fMRI/sample_constructiveAging/databases/help.dbs/Fremtid")

dbase_name="Enriched_db.xlsx";    ### input name ###
db.raw=read_excel(dbase_name, sheet = "db", col_names = TRUE) 

#changes in data type
db.raw <- transform(db.raw, Project_Name = as.factor(Project_Name), 
                    Project_Wave=as.factor(Project_Wave),
                    Sex=as.factor(Sex))

# add filters if you wish, and select working database (as db.current)
db.current <- db.raw



#### descriptives.preparations. Selection all questionaire variables ####
#---------------------------------#
nums <- sapply(db.current, is.numeric)

F_Vars=21	#select first questionaire var
L_Vars=94
Vars <- nums; 
Vars[1:(F_Vars)]<-FALSE; 
Vars[(L_Vars):length(db.raw)]<-FALSE; 
lnum=sum(Vars);


#### descriptives ####
#--------------------#
# add whatever you like here 



##AGE
j = 9
db.current.ggplot <- data.frame(db.current$Age, rep(1,length(db.current$Age)), db.current$Project_Name)
db.current.ggplot <- setNames(db.current.ggplot,c("Age", "Int", "Project"))

##violin_plots
p <- ggplot(db.current.ggplot, aes(x = Project, y = Age, colour=Project, fill=Project))
  p +geom_violin(aes(alpha = 0.8)) + geom_jitter(height = 0, width = 0.15, size = 3, alpha = 0.8) 


  
  
  #### PCA v.1 ####
  #--------------------#
  
  # readme: Procedure: Following two guidelines   
      # PCA v.2 - As in Andy Fields # and       
      # see https://rpubs.com/hauselin/reliabilityanalysis 
      # see that our aim it two obtain a single component thus deviating from 
      # typical PCA analysis that want to explain theunderlying structure of data. 
      # as in Ostby (PNAS, 2010) the 13th item -specficic for the future- has not been taken into account. This item 
      # has been inconsistely been recorded for the participants (ask Ostby for more info on this item)
      # the main a priori decision has been to average for each participant the scores on each questionaire item (regardless it was future or past)
      # then, several items with poor contribution or that lead to lower scale reliability have been dismissed from the PCA. 
      # The procedure is detailed in the following lines. 
  
  
  
  
  # item reversals - not strictly necesary as it can be explicited later; but minimizes errors notheless
  db.current <- db.current[Vars]
  db.current <- transform(db.current, Past_1_01 = ((-1*Past_1_01)+6),
                          Past_2_01 = ((-1*Past_2_01)+6),
                          Past_3_01 = ((-1*Past_3_01)+6),
                          Future_1_01 = ((-1*Future_1_01)+6),
                          Future_2_01 = ((-1*Future_2_01)+6),
                          Future_3_01 = ((-1*Future_3_01)+6),
                          Past_1_03 = ((-1*Past_1_03)+6),
                          Past_2_03 = ((-1*Past_2_03)+6),
                          Past_3_03 = ((-1*Past_3_03)+6),
                          Future_1_03 = ((-1*Future_1_03)+6),
                          Future_2_03 = ((-1*Future_2_03)+6),
                          Future_3_03 = ((-1*Future_3_03)+6),
                          Past_1_07 = ((-1*Past_1_07)+6),
                          Past_2_07 = ((-1*Past_2_07)+6),
                          Past_3_07 = ((-1*Past_3_07)+6),
                          Future_1_07 = ((-1*Future_1_07)+6),
                          Future_2_07 = ((-1*Future_2_07)+6),
                          Future_3_07 = ((-1*Future_3_07)+6),
                          Past_1_08 = ((-1*Past_1_08)+6),
                          Past_2_08 = ((-1*Past_2_08)+6),
                          Past_3_08 = ((-1*Past_3_08)+6),
                          Future_1_08 = ((-1*Future_1_08)+6),
                          Future_2_08 = ((-1*Future_2_08)+6),
                          Future_3_08 = ((-1*Future_3_08)+6))
  
  
  
  ## MEAN DATA for each FUTURE / PAST question
  db.current.12Mitems <- data.frame(a=1:length(db.current[[1]]))
  db.current.12Mitems$a <- NULL
  nnames <- c()
  for (j in 1:12) {
    grot1 <- rowMeans(data.frame(db.current[j],db.current[j+12],db.current[j+12*2],db.current[j+12*3],db.current[j+12*4],db.current[j+12*5]), na.rm = TRUE)
    db.current.12Mitems <- data.frame(db.current.12Mitems, grot1, check.rows = FALSE)  
    nnames <- c(nnames, names(db.current[j]))
  }
  db.current.12Mitems <- setNames(db.current.12Mitems,nnames)
  
  
  # plot descriptives - data check for  impossible data values)
  grot1 <- c();grot2 <- c();
  for (j in 1:12) {
    grot1 <- c(grot1, db.current.12Mitems[[j]])  
    grot2 <- c(grot2, rep(j,length(db.current[[j]]))) 
  }
  db.current.12Mitems.long <- data.frame(grot1,as.factor(grot2))
  db.current.12Mitems.long <- setNames(  db.current.12Mitems.long ,c("scores", "itemN"))
  p <- ggplot(db.current.12Mitems.long, aes(x = itemN, y = scores, group = itemN, color = itemN, fill = itemN))
  p +geom_violin(aes(alpha = 0.8)) + geom_jitter(height = 0, width = 0.15, size = 3, alpha = 1) 
  
  
  ##first iteration
    # requirements
    db.mat <- cor(db.current.12Mitems)            # correlation matrix
    cortest.bartlett(db.current.12Mitems)         # testing whether matrix is identity matrix. Need to reject null ho. 
    KMO(db.current.12Mitems)                      # measure of samplimng adecuacy >.5 mediocre >.7 good >.8 great >.9 superb
    det(db.mat)                                   # > 0.0000001
    #Item n# 8 do not meet criteria KMO = 0.32; redo without item

  ##second iteration
    db.current.11Mitems <- db.current.12Mitems[,-8]            # exclude item 8 
    db.mat <- cor(db.current.11Mitems)                       # as in the first iteration
    KMO(db.current.11Mitems)                                  # measure of samplimng adecuacy >.5 mediocre >.7 good >.8 great >.9 superb
    det(db.mat)                                   # > 0.0000001
    
    db.pca2 <- principal(db.mat, nfactors = 11, rotate = "none")
    alpha(db.mat)
    #Item n# 7 negatively correlated to factor; redo analysis AND REDUCED scale reliability
  
  
  ##Third iteration
    db.current.10Mitems <- db.current.11Mitems[,-7]            # exclude item 7 
    db.mat <- cor(db.current.10Mitems)                       # as in the first iteration
    KMO(db.current.10Mitems)                                  # measure of samplimng adecuacy >.5 mediocre >.7 good >.8 great >.9 superb
    det(db.mat)                                   # > 0.0000001
    
    
    db.pca3 <- principal(db.mat, nfactors = 1, rotate = "none")
    alpha(db.mat)
    res.id <- factor.residuals(db.mat,db.pca3$loadings)
    res.id <- as.matrix(res.id[upper.tri(res.id)]) 
    large.resid <- abs(res.id) > 0.05   
    sum(large.resid)/nrow(res.id)
    sqrt(mean(res.id^2))
    #Item n# 3 worsening factor reliability --> check what happens without it
    #Fit based upon off diagonal values = 0.91
    
 ##Four iteration
    db.current.9Mitems <- db.current.10Mitems[,-3]            # exclude item 3
    db.mat <- cor(db.current.9Mitems)                       # as in the first iteration
    KMO(db.current.9Mitems)                                  # measure of samplimng adecuacy >.5 mediocre >.7 good >.8 great >.9 superb
    det(db.mat)                                   # > 0.0000001
    
    
    db.pca4 <- principal(db.mat, nfactors = 1, rotate = "none", scores = TRUE)
    alpha(db.mat)
    
    res.id <- factor.residuals(db.mat,db.pca4$loadings)
    res.id <- as.matrix(res.id[upper.tri(res.id)]) 
    large.resid <- abs(res.id) > 0.05   
    sum(large.resid)/nrow(res.id)
    sqrt(mean(res.id^2))
    # ~46% loading. A good solution if we want to work with a single component. 
    # Note, however, that the inclusions of additional factors is recommended 
    
    

  ##### Prediction function for PCA #####
  # Apply PCA to part of the data or to additional participants
  # in the current case apply it to the MemC participants and save databse as csv. 
    
    db.apply <- db.current.9Mitems[which (db.raw$Project_Name == "MemC"), ] 
    db.applyID <- db.raw[which (db.raw$Project_Name == "MemC"),7 ] 
    db.AutonoFactor <- predict.psych(db.pca4,db.apply,db.current.9Mitems)
    
    db.factor <- data.frame(db.applyID, db.apply, db.AutonoFactor)
    setwd("M:/Memory_project_fMRI/sample_constructiveAging/databases/help.dbs/Fremtid")
    db.factor <- setNames(db.factor,c("ID", "Q1R", "Q2", "Q4", "Q5", "Q6", "Q9", "Q10", "Q11", "Q12","Autono.Fac9var"))
    write.csv(db.factor, "Fremtid.Factor.com.csv")    
    
    
    
    
    
  
  #### debugging#####
  
  # find typing errors    
  jj <- c(); mm <- c()
  for (j in 1:72) {
    jj <- c(jj, j)
    mm <- c(mm, max(db.current[j],na.rm= TRUE))
  }
    
    
    # parallel analysis for optimal component selection#
    #https://www.r-bloggers.com/determining-the-number-of-factors-with-parallel-analysis-in-r/
    install.packages("paran")
    install.packages("relimp")
    library(relimp, pos = 4)
    library(paran)
  