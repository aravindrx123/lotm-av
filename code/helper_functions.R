padgap <- function(vec,maxlength){
  return(c(rep("Gap2",maxlength-length(vec)),vec))
}

stripstartgaps <- function(vec){
  vec <- vec[min(which(!vec=='Gap')):length(vec)]
  return(vec)
}

countgaps<- function(vec){
  return(sum(vec=='Gap'))
}

Collapsing_Gaps <- function(Sequence,Gap_Unit){
  ### Positions where non-Gap
  Original <- (which(Sequence!="Gap"))
  
  ### Lengths of Gaps
  Gaps_Length <- Original[2:length(Original)]-Original[1:(length(Original)-1)]
  
  ### Collapsed Gaps
  Collapsed_Gaps_Count  <- floor((Gaps_Length)/(Gap_Unit))
  
  ### First Element
  Final_Sequence <- Sequence[Original[1]]
  
  if(length(Original)>1)
  {
    for(i in 2:length(Original))
    {
      if(Collapsed_Gaps_Count[i-1]!=0)
      {
        Final_Sequence <- c(Final_Sequence,rep("Gap",Collapsed_Gaps_Count[i-1]))
      }
      Final_Sequence <- c(Final_Sequence,Sequence[Original[i]])
    }
  }
  return(Final_Sequence)
}

pj_saveplot <- function(filename, pj_list, events, colors, opt = 'journey'){
  maxlen = max(sapply(pj_list,length))
  
  qq = lapply(pj_list,padgap,maxlength=maxlen)
  
  qq = data.frame(matrix(unlist(qq), nrow=length(qq), byrow=T))
  qq[qq=='Gap2'] <- NA
  
  unique_vals <- sort(unique(unlist(apply(qq,2,unique))))
  k = which(events %in% unique_vals)
  
  df.seq = seqdef(qq,labels = events[k])
  
  if(opt == 'journey'){
    png(filename=filename,width = 1200,height = 960)
    seqiplot(df.seq, with.legend=F,idxs=0, space=0,border=NA,cpal=colors[k])
    dev.off()
  }
  else if(opt == 'density'){
    png(filename=filename,width = 1200,height = 960)
    seqdplot(df.seq, with.legend=F,border=NA,cpal=colors[k])
    dev.off()
  }
  else{
    print("Invalid option!")
  }
  
}

pj_saveplot_v2 <- function(filename, pj_list, pred_probs, events, colors, opt = 'journey'){
  maxlen = max(sapply(pj_list,length))
  
  qq = lapply(pj_list,padgap,maxlength=maxlen)
  
  qq = data.frame(matrix(unlist(qq), nrow=length(qq), byrow=T))
  
  qq$response <- ifelse(pred_probs$CLASS_1_FLAG==1,'Yes','No')
  
  for(i in 1:5){
    qq[,paste0('y',i)] <- qq$response
  }
  
  unique_vals <- sort(unique(unlist(apply(qq,2,unique))))
  k = which(events %in% unique_vals)
  
  df.seq = seqdef(qq,labels = events[k])
  
  if(opt == 'journey'){
    png(filename=filename,width = 1200,height = 960)
    seqiplot(df.seq, with.legend=F,idxs=0, space=0,border=NA,cpal=colors[k])
    dev.off()
  }
  else if(opt == 'density'){
    png(filename=filename,width = 1200,height = 960)
    seqdplot(df.seq, with.legend=F,border=NA,cpal=colors[k])
    dev.off()
  }
  else{
    print("Invalid option!")
  }
  
}

pj_list_gaps <- function(df, PIDcol, eventcol, datecol, startdate, enddate,gaps=T, min_gap_unit = 7, sort=T){
  
  all_dates <- data.frame(seq.Date(startdate,enddate,by = 1))
  names(all_dates) <- datecol
  
  pj_list <- list()
  patients <- unique(df[,PIDcol])
  
  fill <- ifelse(gaps,'Gap',NA)
  
  for(i in 1:length(patients)){
    print(i)
    temp <- df[df[,PIDcol]==patients[i],]
    
    temp = merge(all_dates,temp[,c(datecol,eventcol)],all.x=T)
    temp[is.na(temp[,eventcol]),eventcol] <- fill
    
    pj_list[[i]] <- temp[,eventcol]
  }
  
  #Strip starting gaps
  pj_list <- lapply(pj_list,stripstartgaps)
  
  #Collapse gaps
  pj_list <- lapply(pj_list,function(x)Collapsing_Gaps(x,min_gap_unit))
  
  if(sort==T){
    pj_list <- pj_list[order(sapply(pj_list,length),decreasing=T)]
  }
  return(pj_list)
  
}

pj_list_gaps_v2 <- function(df,pred_probs, PIDcol, eventcol, datecol, startdate, enddate,gaps=T, min_gap_unit = 7){
  
  all_dates <- data.frame(seq.Date(startdate,enddate,by = 1))
  names(all_dates) <- datecol
  
  patients <- pred_probs[,PIDcol]
  
  pj_list <- list()
  
  fill <- ifelse(gaps,'Gap',NA)
  
  for(i in 1:length(patients)){
    print(i)
    temp <- df[df[,PIDcol]==patients[i],]
    
    temp = merge(all_dates,temp[,c(datecol,eventcol)],all.x=T)
    temp[is.na(temp[,eventcol]),eventcol] <- fill
    
    pj_list[[i]] <- temp[,eventcol]
  }
  
  #Strip starting gaps
  pj_list <- lapply(pj_list,stripstartgaps)
  
  #Collapse gaps
  pj_list <- lapply(pj_list,function(x)Collapsing_Gaps(x,min_gap_unit))
  
  return(pj_list)
  
}

max_pr <- function(probs,labels, recall_threshold){
  
  min_p = min(probs)
  max_p = max(probs)
  
  cutoffs <- seq(min_p,max_p, 0.001)
  Recall <- rep(0,length(cutoffs))
  Precision  <- rep(0,length(cutoffs))
  for(i in 1:length(cutoffs)){
    preds <- ifelse(probs>cutoffs[i],1,0)
    TP <- sum(preds==1&labels==1)
    FP <- sum(preds==1&labels==0)
    FN <- sum(preds==0&labels==1)
    Recall[i] <- TP/(TP+FN)
    Precision[i] <- TP/(TP+FP)
  }
  
  max_precision <- max(Precision[which(Recall>=recall_threshold)],na.rm=TRUE)
  return(max_precision)
}

max_precision <- function(probs,labels, recall_threshold){
  
  min_p = min(probs)
  max_p = max(probs)
  
  cutoffs <- seq(min_p,max_p, 0.001)
  Recall <- rep(0,length(cutoffs))
  Precision  <- rep(0,length(cutoffs))
  for(i in 1:length(cutoffs)){
    Final_Predictions <- probs
    Final_Predictions[Final_Predictions>=cutoffs[i]] <- 1
    Final_Predictions[Final_Predictions<cutoffs[i]] <- 0
    
    Temp_PR <- as.data.frame(table(Final_Predictions,labels))
    Recall[i] <- Temp_PR$Freq[4]/(Temp_PR$Freq[3]+Temp_PR$Freq[4])
    Precision[i] <- Temp_PR$Freq[4]/(Temp_PR$Freq[2]+Temp_PR$Freq[4])
  }
  
  max_precision <- max(Precision[which(Recall>=recall_threshold)],na.rm=TRUE)
  c = min(which(Precision==max_precision))
  
  Final_Predictions <- probs
  Final_Predictions[Final_Predictions>=cutoffs[c]] <- 1
  Final_Predictions[Final_Predictions<cutoffs[c]] <- 0
  
  print(table(Final_Predictions,labels))
  print(paste("Cutoff:",cutoffs[c]))
  print(paste("Precision:",max_precision))
  print(paste("Recall:",Recall[c]))
}

RP <- function(Probabilities,Actual,Cut_Off){
  
  Final_Predictions <- Probabilities
  Final_Predictions[Final_Predictions>=Cut_Off] <- 1
  Final_Predictions[Final_Predictions<Cut_Off] <- 0
  
  print(table(Final_Predictions,Actual))
  
  Temp_PR <- as.data.frame(table(Final_Predictions,Actual))
  Recall <- Temp_PR$Freq[4]/(Temp_PR$Freq[3]+Temp_PR$Freq[4])
  Precision <- Temp_PR$Freq[4]/(Temp_PR$Freq[2]+Temp_PR$Freq[4])
  
  print(Precision)
  print(Recall)
  return(table(Final_Predictions,Actual))
}

pr_curve<- function(probs,labels){
  
  min_p = min(probs)
  max_p = max(probs)
  
  cutoffs <- seq(min_p,max_p, 0.01)
  Recall <- rep(0,length(cutoffs))
  Precision  <- rep(0,length(cutoffs))
  for(i in 1:length(cutoffs)){
    Final_Predictions <- probs
    Final_Predictions[Final_Predictions>=cutoffs[i]] <- 1
    Final_Predictions[Final_Predictions<cutoffs[i]] <- 0
    
    Temp_PR <- as.data.frame(table(Final_Predictions,labels))
    Recall[i] <- Temp_PR$Freq[4]/(Temp_PR$Freq[3]+Temp_PR$Freq[4])
    Precision[i] <- Temp_PR$Freq[4]/(Temp_PR$Freq[2]+Temp_PR$Freq[4])
  }
  
  plot(cutoffs,Precision,type="l",col="red",xlim = c(min_p,max_p),
       xlab = 'Cut-off',ylab= 'Precision/Recall',
       panel.first = grid())
  lines(cutoffs,Recall,col="green")
  
}

pr_curve_v2<- function(probs,labels){
  
  min_p = min(probs)
  max_p = max(probs)
  
  cutoffs <- seq(min_p,max_p, 0.01)
  Recall <- rep(0,length(cutoffs))
  Precision  <- rep(0,length(cutoffs))
  for(i in 1:length(cutoffs)){
    preds <- ifelse(probs>cutoffs[i],1,0)
    
    Temp_PR <- as.data.frame(table(preds,labels))
    Recall[i] <- Temp_PR$Freq[4]/(Temp_PR$Freq[3]+Temp_PR$Freq[4])
    Precision[i] <- Temp_PR$Freq[4]/(Temp_PR$Freq[2]+Temp_PR$Freq[4])
  }
  
  df <- data.frame(Probability = cutoffs, Recall = Recall, Precision = Precision)
  
  ggplot(df, aes(Probability)) +
    geom_line(aes(y=Precision, colour="Precision")) +
    geom_line(aes(y=Recall, colour="Recall")) +
    scale_colour_manual("",breaks = c("Precision", "Recall"),values = c("red", "green"))+
    scale_y_continuous(limits = c(0,1)) + scale_x_continuous(limits = c(min(probs),max(probs)))+
    xlab('Probability Cutoff') + ylab('Precision/Recall') +
    ggtitle('Precision and Recall Curves')+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
}

precrecvals <- function(probs,labels){
  
  min_p = floor(min(probs)*100)/100
  max_p = floor(max(probs)*100)/100
  
  cutoffs <- seq(min_p,max_p, 0.01)
  Recall <- rep(0,length(cutoffs))
  Precision  <- rep(0,length(cutoffs))
  for(i in 1:length(cutoffs)){
    preds <- ifelse(probs>cutoffs[i],1,0)
    
    Recall[i] <- sum(preds==1&labels==1)/sum(labels==1)
    Precision[i] <- sum(preds==1 & labels==1)/sum(preds==1)
  }
  
  df <- data.frame(Probability = cutoffs, Recall = Recall, Precision = Precision)
  return(df)
}

trace_preds <- function(probs,cutoff,test,PIDcol = 'PATIENT_ID',responsecol = 'CLASS_1_FLAG'){
  
  preds = ifelse(probs>cutoff,1,0)
  
  ix_tp <- which(preds==1 & test[,responsecol]==1)
  ix_fp <- which(preds==1 & test[,responsecol]==0)
  ix_fn <- which(preds==0 & test[,responsecol]==1)
  ix_tn <- which(preds==0 & test[,responsecol]==0)
  
  tp <- test[,PIDcol][ix_tp]
  fp <- test[,PIDcol][ix_fp]
  fn <- test[,PIDcol][ix_fn]
  tn <- test[,PIDcol][ix_tn]
  
  patients <- list()
  patients[['tn']] <- tn
  patients[['tn_probs']] <- probs[ix_tn]
  patients[['fn']] <- fn
  patients[['fn_probs']] <- probs[ix_fn]
  patients[['fp']] <- fp
  patients[['fp_probs']] <- probs[ix_fp]
  patients[['tp']] <- tp
  patients[['tp_probs']] <- probs[ix_tp]
  return(patients)
}

getClaimsDist <- function(filename,claims_df,probs,test,prob_cutoff, datecut,
                          PIDcol = 'PATIENT_ID',
                          eventcol = 'CLAIM_MAPPING',
                          datecol = 'CLAIM_DATE_FORMAT',
                          responsecol = 'CLASS_1_FLAG'){
  
  p1 <- trace_preds(probs = probs,cutoff = prob_cutoff,test = test,PIDcol = 'PATIENT_ID',responsecol = 'CLASS_1_FLAG')
  
  patient_claimcount <- as.data.frame(claims_df[claims_df[,datecol]<datecut,] %>% 
                                        group_by_(PIDcol) %>% 
                                        summarise(num_claim_types = n_distinct(CLAIM_MAPPING)))
  
  tp_claimcount = patient_claimcount[patient_claimcount[,PIDcol] %in% p1[['tp']],]
  fp_claimcount = patient_claimcount[patient_claimcount[,PIDcol] %in% p1[['fp']],]
  tn_claimcount = patient_claimcount[patient_claimcount[,PIDcol] %in% p1[['tn']],]
  fn_claimcount = patient_claimcount[patient_claimcount[,PIDcol] %in% p1[['fn']],]
  
  ch1 <- tp_claimcount %>% group_by(num_claim_types) %>% summarise(tp = n())
  ch2 <- fp_claimcount %>% group_by(num_claim_types) %>% summarise(fp = n())
  ch3 <- tn_claimcount %>% group_by(num_claim_types) %>% summarise(tn = n())
  ch4 <- fn_claimcount %>% group_by(num_claim_types) %>% summarise(fn = n())
  
  chk = merge(merge(merge(ch1,ch2,all.x = T,all.y = T),ch3,all.x=T,all.y=T),ch4,all.x = T,all.y = T)
  
  chk[is.na(chk)]<-0
  write.xlsx(chk,filename, sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)
  
  cl1 <- claims_df[claims_df[,datecol]<datecut & claims_df[,PIDcol] %in% p1[['tp']],] %>% 
    group_by_(eventcol) %>% 
    summarise(num_claims_tp = n())
  
  cl2 <- claims_df[claims_df[,datecol]<datecut & claims_df[,PIDcol] %in% p1[['fp']],] %>% 
    group_by_(eventcol) %>% 
    summarise(num_claims_fp = n())
  
  
  cl3 <- claims_df[claims_df[,datecol]<datecut & claims_df[,PIDcol] %in% p1[['tn']],] %>% 
    group_by_(eventcol) %>% 
    summarise(num_claims_tn = n())
  
  cl4 <- claims_df[claims_df[,datecol]<datecut & claims_df[,PIDcol] %in% p1[['fn']],] %>% 
    group_by_(eventcol) %>% 
    summarise(num_claims_fn = n())
  
  g2 <- merge(merge(merge(cl1,cl2,all.x = T,all.y = T),cl3,all.x=T,all.y=T),cl4,all.x = T,all.y = T)
  
  g2[is.na(g2)] <- 0
  write.xlsx(g2,filename, sheetName="Sheet2", col.names=TRUE, row.names=TRUE, append=FALSE)
  
  library(xlsx)
  write.xlsx(chk, file=filename, sheetName="sheet1", row.names=FALSE)
  write.xlsx(g2, file=filename, sheetName="sheet2", append=TRUE, row.names=FALSE)
  return(list(chk,g2))
}


plotfun = function(data,varlist){
  for (i in varlist){
    d = ggplot(data, aes(x = data[,i], fill = as.factor(CLASS_1_FLAG))) + geom_density(alpha = 0.5)+
      ggtitle(paste(i))+labs(x = varlist[i])
    mydpath <- file.path(paste("desnsityplot", paste(i), ".png", sep = ""))
    ggsave(filename=mydpath, d)
  }
}

bplotfun = function(data,varlist){
  for (i in varlist){
    d = ggplot(data, aes(as.factor(CLASS_1_FLAG),data[,i])) + geom_boxplot() +
      ggtitle(paste(i))+labs(x = paste(i))
    mypath <- file.path(paste("boxplot", paste(i), ".png", sep = ""))
    ggsave(filename=mypath, d)
  }
}

chk_dist <- function(data,sample, featurenames){
  sig <- c()
  for(i in featurenames){
    sig[i] <- t.test(data[,i],sample[,i],conf.level = 0.99)$p.value
  }
  return(sig)
}

chk_stats <- function(data,featurenames){
  stats <- list()
  mean <- colMeans(data[,featurenames],na.rm=T)
  #med <- sapply(data[,featurenames],median,na.rm=T)
  stats$means <- mean
  #stats$median <- med
  return(stats)
}

getKappa <- function(mat){
  s = sum(mat)
  oa <- (mat[1,1]+mat[2,2])/s
  
  c0 <- (mat[1,1]+mat[2,1])/s*(mat[1,1]+mat[1,2])
  c1 <- (mat[2,2]+mat[2,1])/s*(mat[2,2]+mat[1,2])
  
  ea <- (c0+c1)/s
  
  kappa <- (oa-ea)/(1-ea)
  
  return(kappa)
}


bin <- function(vec,num_bins=10){
  ord <- order(vec,decreasing = T)
  ordered_vec <- vec[ord]
  out <- data.frame(val = ordered_vec,cumsum_1 = cumsum(ordered_vec),cumsum = cumsum(ordered_vec)/sum(vec))
  
  cuts <- seq(0,1,1/num_bins)
  out$bin <- 1
  
  for(i in 1:num_bins){
    out$bin <- ifelse(out$cumsum <= cuts[i+1] & out$cumsum > cuts[i],num_bins-i+1,out$bin)
  }
  
  out[which(ordered_vec==0),'bin'] <- 0
  
  out <- out[order(ord),c('val','bin')]
  row.names(out) <- NULL
  return(out)
  
}
