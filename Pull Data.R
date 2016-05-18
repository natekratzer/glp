#This file contains several functions for pulling GLP data

#acs.time is a function for taking multiple years of ACS data
#It requires a file folder of csvs and a starting year
#The default starting year is 2005, since that's how far back most ACS data goes
#Example:
#data.frame<-acs.time("C:/Users/natek/Documents/ACS")
#The file path should be a folder with one csv per year downloaded directly from acs

acs.time<-function(directory,starting.year=2005){
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

#St. Louis is a population weighted average of Stl City and Stl County
#This function only works one variable at a time
#I plan to rewrite it sometime to work on a whole dataset. 
#If you're feeling ambitious, you could try to do that. 
#For now, you need to specify the data frame and the variable within it
#The variable should be in "", the data frame should not
#It returns a dataframe with 4 columns
#1. A FIPS code of 0 (since it has no real FIPS code)
#Example:
#educ<-st.wgt(dataset, "educ_attainment")

#St. Louis
stl.wgt<-function(df,var){
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

#merging in the stl data. You could also do this in excel if easier
#unfortunately, you still have to do this for every variable
#and it assumes you have all ten years of data
#for individual years, use the correct year from within the function
newdata<-merge(df, stl.wgt, by=c("FIPS", "year"))

#Then you can merge with the GLP basic county ids found in the github repo
labeled_data<-merge(newdata,id_data, by=c("FIPS", "year"))

#And finally, take out the two individual St. Louis categories
#That way we only have the weighted one
labeled_data_no_stl<-subset(labeleddata, FIPS!=29189 & FIPS!=29150)



#subsetting one year of data from FIPS
#If you have RWJF or some other data for all U.S. counties, and just want peers
#Example: newdata<-pull_peers_FIPS(olddata)
pull_peers_FIPS<-function(data){
all.peers<-subset(data, data$FIPS == 1073 | data$FIPS == 37119
                  |data$FIPS == 39061 | data$FIPS == 39049
                  |data$FIPS == 26081 |data$FIPS == 37081
                  |data$FIPS == 45045 |data$FIPS == 18097
                  |data$FIPS == 29095 |data$FIPS == 47093
                  |data$FIPS == 21111 |data$FIPS == 47157
                  |data$FIPS == 47037 |data$FIPS == 40109
                  |data$FIPS == 31055 |data$FIPS == 29189
                  |data$FIPS == 29510 |data$FIPS == 40143
                  |data$FIPS == 12031 |data$FIPS == 37183
                  |data$FIPS == 39113 |data$FIPS == 51760)
all.peers$baseline<-1
all.peers$current<-1
all.peers$baseline[all.peers$FIPS==26081|all.peers$FIPS==29189
                   |all.peers$FIPS==29510|all.peers$FIPS==40109
                   |all.peers$FIPS==40143|all.peers$FIPS==45045
                   |all.peers$FIPS==47093]<-0
all.peers$current[all.peers$FIPS== 12031|all.peers$FIPS==37183|
                    all.peers$FIPS==39113|all.peers$FIPS==51760]<-0
all.peers
}

#If instead of FIPS, you have state and county listed in data
#Rename the appropriate columns to "County" and "State"
#You may wind up with two Richmond, Virginias. Or None
#There is both a Richmond County and a Richmond City in VA
#We want Richmond City - which is not part of any county
#Richmond City has a different FIPS code 51760, as used above
#Also Richmond county has less than 10,000 people
#If under county the data set calls it "Richmond City" it will pull correctly
#If not, change it to "Richmond city" or whatever it is called in that data
#If both are referred to as "Richmond" you'll have to choose the right one manually
#The same problem applies to St. Louis. It needs to be "St. Louis City"
#And "St. Louis" If it's "St. Louis County" you'll need to change it. 
#This is why I prefer FIPS. No capitalization or other idiosyncracies to worry about
pull_peers_names<-function(data){
  current.peers<-subset(data, data$County == "Jefferson" & data$State == "Alabama" |
                          data$County == "Mecklenburg" & data$State == "North Carolina"|
                          data$County == "Hamilton" & data$State == "Ohio"| 
                          data$County=="Franklin"& data$State == "Ohio" |
                          data$County=="Kent" & data$State == "Michigan"| 
                          data$County=="Guilford" & data$State == "North Carolina"|
                          data$County=="Greenville" & data$State == "South Carolina"|
                          data$County=="Marion" & data$State == "Indiana"|
                          data$County=="Jackson" & data$State == "Missouri"|
                          data$County=="Knox" & data$State == "Tennessee"|
                          data$County== "Jefferson"& data$State== "Kentucky"|
                          data$County== "Shelby" & data$State== "Tennessee"|
                          data$County== "Davidson" & data$State== "Tennessee"|
                          data$County== "Oklahoma" & data$State== "Oklahoma"|
                          data$County== "Douglas" & data$State == "Nebraska"|
                          data$County== "St. Louis" & data$State == "Missouri"|
                          data$County== "St. Louis City" & data$State == "Missouri"|
                          data$County == "Tulsa" & data$State == "Oklahoma"|
                          data$County=="Duval" & data$State == "Florida"|
                          data$County== "Wake" & data$State == "North Carolina"|
                          data$County=="Montgomery" & data$State == "Ohio"|
                          data$County == "Richmond City" & data$State == "Virginia")

all.peers$baseline<-1
all.peers$current<-1
all.peers$baseline[data$County=="Kent" & data$State == "Michigan"|
                     data$County== "St. Louis" & data$State == "Missouri"|
                     data$County== "St. Louis City" & data$State == "Missouri"|
                     data$County== "Oklahoma" & data$State== "Oklahoma"|
                     data$County == "Tulsa" & data$State == "Oklahoma"|
                     data$County=="Greenville" & data$State == "South Carolina"|
                     data$County=="Knox" & data$State == "Tennessee"]<-0

all.peers$current[data$County=="Duval" & data$State == "Florida"|
                    data$County== "Wake" & data$State == "North Carolina"|
                    data$County=="Montgomery" & data$State == "Ohio"|
                    data$County == "Richmond City" & data$State == "Virginia"]<-0
all.peers
}
  
#After pulling, you still need to go merge the STL data using the appropriate weights.
#For just one year, it may be easier to do this in excel. 


