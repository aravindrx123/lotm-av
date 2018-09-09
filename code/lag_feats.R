
##################################################################################################################################################

#Lag features

#Days since last email was opened
df <- df %>% arrange(user_id,date)

open_seq <- aggregate(df$ix[df$is_open==1], list(df$user_id[df$is_open==1]), c )
names(open_seq) <- c('user_id','open_seq')

click_seq <- aggregate(df$ix[df$is_click==1], list(df$user_id[df$is_click==1]), c )
names(click_seq) <- c('user_id','click_seq')

age_seq <- aggregate(df$acc_age[df$is_open==1], list(df$user_id[df$is_open==1]),c)
names(age_seq) <- c('user_id','age_seq')

df$communication_type <- as.character(df$communication_type)
email_type_seq <- aggregate(df$communication_type[df$is_open==1], list(df$user_id[df$is_open==1]),paste)
names(email_type_seq) <- c('user_id','email_type_seq')

num_emails <- df %>% group_by(user_id) %>% summarise(num_emails = n())

qq <- Reduce(function(x, y) merge(x, y), list(open_seq, age_seq, email_type_seq, num_emails))

df$days_seq <- 0

start <- Sys.time()
for(uid in unique(df$user_id)){
  print(uid)
  ix <- which(qq$user_id==uid)
  
  vec <- rep(NA,qq$open_seq[[ix]][1])
  if(length(qq$open_seq[[ix]])==1){
    vec <- c(vec,rep(qq$age_seq[[ix]],qq$num_emails[ix]-qq$open_seq[[ix]]))
  }
  else{
    for(j in 1:length(qq$age_seq[[ix]])-1){
      vec <- c(vec,rep(qq$age_seq[[ix]][j],qq$open_seq[[ix]][j+1]-qq$open_seq[[ix]][j]))
    }
    vec <- c(vec,rep(qq$age_seq[[ix]][length(qq$age_seq[[ix]])],qq$num_emails[ix]-qq$open_seq[[ix]][length(qq$open_seq[[ix]])]))
    
  }
  
  df$days_seq[which(df$user_id==uid)] <- vec
  
}

end <- Sys.time()
print(end-start)

##################################################################################################################################################

#Email type that was previously opened

df$last_open_type <- 'None'

start <- Sys.time()
for(uid in unique(df$user_id)){
  
  ix <- which(qq$user_id==uid)
  
  vec <- rep('None',qq$open_seq[[ix]][1])
  
  if(length(qq$open_seq[[ix]])==1){
    vec <- c(vec,rep(qq$email_type_seq[[ix]],qq$num_emails[ix]-qq$open_seq[[ix]]))
  }else{
    for(j in 1:length(qq$email_type_seq[[ix]])-1){
      vec <- c(vec,rep(qq$email_type_seq[[ix]][j],qq$open_seq[[ix]][j+1]-qq$open_seq[[ix]][j]))
    }
    vec <- c(vec,rep(qq$email_type_seq[[ix]][length(qq$email_type_seq[[ix]])],
                     qq$num_emails[ix]-qq$open_seq[[ix]][length(qq$open_seq[[ix]])]))
  }  
    df$last_open_type[which(df$user_id==uid)] <- vec
  

  
}

end <- Sys.time()
print(end-start)


##################################################################################################################################################

#Lag features

#Days since last email was clicked
df <- df %>% arrange(user_id,date)

click_seq <- aggregate(df$ix[df$is_click==1], list(df$user_id[df$is_click==1]), c )
names(click_seq) <- c('user_id','click_seq')

age_seq <- aggregate(df$acc_age[df$is_click==1], list(df$user_id[df$is_click==1]),c)
names(age_seq) <- c('user_id','age_seq')

df$communication_type <- as.character(df$communication_type)
email_type_seq <- aggregate(df$communication_type[df$is_click==1], list(df$user_id[df$is_click==1]),paste)
names(email_type_seq) <- c('user_id','email_type_seq')

num_emails <- df %>% group_by(user_id) %>% summarise(num_emails = n())

qq <- Reduce(function(x, y) merge(x, y), list(click_seq, age_seq, email_type_seq, num_emails))

df$days_seq2 <- NA

start <- Sys.time()
for(uid in unique(qq$user_id)){
  print(uid)
  ix <- which(qq$user_id==uid)
  
  vec <- rep(NA,qq$click_seq[[ix]][1])
  if(length(qq$click_seq[[ix]])==1){
    vec <- c(vec,rep(qq$age_seq[[ix]],qq$num_emails[ix]-qq$click_seq[[ix]]))
  }
  else{
    for(j in 1:length(qq$age_seq[[ix]])-1){
      vec <- c(vec,rep(qq$age_seq[[ix]][j],qq$click_seq[[ix]][j+1]-qq$click_seq[[ix]][j]))
    }
    vec <- c(vec,rep(qq$age_seq[[ix]][length(qq$age_seq[[ix]])],qq$num_emails[ix]-qq$click_seq[[ix]][length(qq$click_seq[[ix]])]))
    
  }
  
  df$days_seq2[which(df$user_id==uid)] <- vec
  
}

end <- Sys.time()
print(end-start)

##################################################################################################################################################

#Email type that was previously clicked

df$last_click_type <- 'None'

start <- Sys.time()
for(uid in unique(qq$user_id)){
  
  ix <- which(qq$user_id==uid)
  
  vec <- rep('None',qq$click_seq[[ix]][1])
  
  if(length(qq$click_seq[[ix]])==1){
    vec <- c(vec,rep(qq$email_type_seq[[ix]],qq$num_emails[ix]-qq$click_seq[[ix]]))
  }else{
    for(j in 1:length(qq$email_type_seq[[ix]])-1){
      vec <- c(vec,rep(qq$email_type_seq[[ix]][j],qq$click_seq[[ix]][j+1]-qq$click_seq[[ix]][j]))
    }
    vec <- c(vec,rep(qq$email_type_seq[[ix]][length(qq$email_type_seq[[ix]])],
                     qq$num_emails[ix]-qq$click_seq[[ix]][length(qq$click_seq[[ix]])]))
  }  
  df$last_click_type[which(df$user_id==uid)] <- vec
  
}

end <- Sys.time()
print(end-start)



