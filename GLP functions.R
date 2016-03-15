acs.glp<-function(directory,starting.year=2005){
  setwd(directory)
  file_names<-list.files()
  n<-length(file_names)
  y<-starting.year
  for (i in 1:n){
    data<-read.csv(file_names[i],header=TRUE, skip=1)
    names(data)[names(data) == 'Id2'] <- 'FIPS'
    all.peers <-subset(data, data$FIPS == 1073 |data$FIPS == 37119
                       |data$FIPS == 39061 |data$FIPS == 39049
                       |data$FIPS == 26081 |data$FIPS == 37081
                       |data$FIPS == 45045 |data$FIPS == 18097
                       |data$FIPS == 29095 |data$FIPS == 47093
                       |data$FIPS == 21111 |data$FIPS == 47157
                       |data$FIPS == 47037 |data$FIPS == 40109
                       |data$FIPS == 31055 |data$FIPS == 29189
                       |data$FIPS == 29510
                       |data$FIPS == 40143 |data$FIPS == 39113
                       |data$FIPS == 12031 |data$FIPS == 37183
                       |data$FIPS == 37183 |data$FIPS == 51760)
    
    all.peers$year<-y
    all.peers$baseline<-1
    all.peers$current<-1
    all.peers$baseline[all.peers$FIPS==26081|all.peers$FIPS==29189
                       |all.peers$FIPS==29510|all.peers$FIPS==40109
                       |all.peers$FIPS==40143|all.peers$FIPS==45045
                       |all.peers$FIPS==47093]<-0
    all.peers$current[all.peers$FIPS== 12031|all.peers$FIPS==37183|
                        all.peers$FIPS==39113|all.peers$FIPS==51760]<-0
    y<-y+1
    
    if(i==1){
     df<-all.peers 
    }
    else{
      names(all.peers)<-names(df)
      df<-rbind(df, all.peers)
    }
  }
  df
}

data.frame<-acs.glp("C:/Users/natek/Documents/ACS")

##St. Louis
stl.glp<-function(df,var){
  df$var <- df[[var]]
  stl.14<-subset(df, year==2014 & (FIPS==29189|FIPS==29510))
  stl.wgt.14<-weighted.mean(stl.14$var, c(.759,.241))
  
  stl.13<-subset(df, year==2013 & (FIPS==29189|FIPS==29510))
  stl.wgt.13<-weighted.mean(stl.13$var, c(.759,.241))
  
  stl.12<-subset(df, year==2012 & (FIPS==29189|FIPS==29510))
  stl.wgt.12<-weighted.mean(stl.12$var, c(.759,.241))
  
  stl.11<-subset(df, year==2011 & (FIPS==29189|FIPS==29510))
  stl.wgt.11<-weighted.mean(stl.11$var, c(.758,.242))
  
  stl.10<-subset(df, year==2010 & (FIPS==29189|FIPS==29510))
  stl.wgt.10<-weighted.mean(stl.10$var, c(.758,.242))
  
  stl.09<-subset(df, year==2009 & (FIPS==29189|FIPS==29510))
  stl.wgt.09<-weighted.mean(stl.09$var, c(.736,.264))
  
  stl.08<-subset(df, year==2008 & (FIPS==29189|FIPS==29510))
  stl.wgt.08<-weighted.mean(stl.08$var, c(.737,.263))
  
  stl.07<-subset(df, year==2007 & (FIPS==29189|FIPS==29510))
  stl.wgt.07<-weighted.mean(stl.07$var, c(.739,.261))
  
  stl.06<-subset(df, year==2006 & (FIPS==29189|FIPS==29510))
  stl.wgt.06<-weighted.mean(stl.06$var, c(.742,.258))
  
  stl.05<-subset(df, year==2005 & (FIPS==29189|FIPS==29510))
  stl.wgt.05<-weighted.mean(stl.05$var, c(.747,.253))
  
  stl<-c(stl.wgt.14,stl.wgt.13,stl.wgt.12,stl.wgt.11,stl.wgt.10,stl.wgt.09,
         stl.wgt.08,stl.wgt.07,stl.wgt.06,stl.wgt.05)
  year<-2014:2005
  FIPS<-rep(0,10)
  current<-rep(1,10)
  stl.df<-cbind(FIPS,year,stl,current)
  colnames(stl.df)<-c("FIPS","year",var,"current")
  stl.df<-as.data.frame(stl.df)
  stl.df
}
  

##Graph data
data.graph<-function(df,var,peers){
  df$var<-df[[var]]
  df<-subset(df,FIPS!=29189&FIPS!=29510)
  if(peers=="current"){
  df.wol<-subset(df,current==1 &FIPS!=21111)
  }
  if(peers=="baseline"){
    df.wol<-subset(df,baseline==1 &FIPS!=21111)
  }
  output.wol<-aggregate(df.wol$var, by=list(df.wol$year),FUN=summary)
  lville<-subset(df,FIPS==21111)
  Louisville<-lville$var
  output<-cbind(output.wol,Louisville)
  file_name<-paste(var,".csv")
  write.csv(output,file=file_name)
  output
}

