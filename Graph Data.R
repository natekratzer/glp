#Graphing
#There are two main graph styles. Ranking and Trendline
#You will need to install some packages to make the graphs
#The first time instead of library() use install.packages("")
#The name needs to be inside quotation marks for install.packages()
library(ggplot2)
library(classInt)
library(ggthemes)
library(reshape2)

###Ranking Graphs
#It is meant to work with only one year
#It defaults to current peers in descending order
#Make sure the units are already what you want. 
#Example with defaults set:
#rank_and_nb_group(df,"bachelorsplus.25to64")
#Example changing defaults:
#rank_and_nb_group(df, "lessthanhighschool.25to64", order="Ascending", peers="Baseline",
#                  plot_title="Population without a High School Degree", 
#                  y_title="Percent of Population")
#For a multline title, use "\n", e.g. plot_title="Title on\ntwo lines"
rank_and_nb_group<-function(df, var, order="Descending", peers="Current",
                            plot_title="",y_title=""){
  df$var <- df[[var]]
  if(peers=="Current"){
    df<-subset(df,Current.Peer==1)
  }
  if(peers=="Baseline"){
    df<-subset(df,Baseline.Peer==1)
  }
  if(order=="Descending"){
    d.order<-df[order(-df$var),]
  }
  if(order=="Ascending"){
    d.order<-df[order(df$var),]
  }
  ranks<-1:length(df$var)
  d.rank<-cbind(d.order,ranks)
  names<-paste(d.rank$ranks,".",sep="")
  names<-paste(names,d.rank$City)
  d.graph<-cbind(d.rank,names)
  
  breaks<-classIntervals(d.graph$var,3,style="jenks")
  d.graph$color<-NA
  d.graph$color[d.graph$var<=breaks$brks[2]]<-"green"
  d.graph$color[d.graph$var>breaks$brks[2] & d.graph$var<=breaks$brks[3]]<-"yellow"
  d.graph$color[d.graph$var>breaks$brks[3]]<-"red"
  d.graph$round<-format(round(d.graph$var,1),nsmall=1)
  d.graph$textfont<-"plain"
  d.graph$textfont[d.graph$City=="Louisville"]<-"bold"
  d.graph$linecolor<-"white"
  d.graph$linecolor[d.graph$City=="Louisville"]<-"black"

  p<-ggplot(data=d.graph,aes(x=factor(names, levels=rev(unique(names))),
                             y=var,fill=factor(color)))+guides(fill=FALSE)
  p<-p+geom_bar(stat="identity",color=rev(d.graph$linecolor))+coord_flip()+theme_tufte()
  if(order=="Ascending"){
    p<-p+scale_fill_manual(values=c("green3","red2","yellow2"))
  }
  if(order=="Descending"){
    p<-p+scale_fill_manual(values=c("red2","green3","yellow2"))
  }
  p<-p+theme(axis.text.y=element_text(hjust=0,face=rev(d.graph$textfont),
                                      size=12))
  p<-p+theme(axis.ticks=element_blank(),axis.text.x=element_blank())
  p<-p+geom_text(aes(label=round),hjust=1.1,size=5,fontface="bold")
  p<-p+labs(title=plot_title,x="",
            y=y_title)
  p<-p+theme(plot.title=element_text(color="black",size=18,face="bold",hjust=.5,
                                                      margin=margin(b=10,unit="pt")))
  p
}

##Making the trendline graphs
#This will at one point write a csv to your working directory
#Because the aggregate function is strange
#Example: df<-data_trendline(data,"bachelorsplus.25to64","Current")
data_trendline<-function(df,var,peers="Current"){
  df$var<-df[[var]]
  df<-subset(df,FIPS!=29189&FIPS!=29510)
  if(peers=="Current"){
    df.wol<-subset(df,Current.Peer==1 &FIPS!=21111)
  }
  if(peers=="Baseline"){
    df.wol<-subset(df,Baseline.Peer==1 &FIPS!=21111)
  }
  output.wol<-aggregate(df.wol$var, by=list(df.wol$year),FUN=summary)
  lville<-subset(df,FIPS==21111)
  Louisville<-lville[[var]]
  year<-lville$year
  Lville_df<-cbind(Louisville,year)
  print(Lville_df)
  file_name<-paste(var,".csv")
  write.csv(output.wol,file=file_name)
  newdata<-read.csv(file_name,header=T)
  year<-newdata$Group.1
  percentile25<-newdata$x.1st.Qu.
  mean<-newdata$x.Mean
  percentile75<-newdata$x.3rd.Qu.
  data<-data.frame(year,percentile25,mean,percentile75)
  data<-merge(data,Lville_df, by="year")
}
#From this function you get the aggregated data needed for plotting
#You may wish to take a rolling mean if the data is noisy.
#Code for a 3 or 5 year rolling mean is after code for graphing


#The data for this function needs to be formatted as it comes out of data_trendline
#Five variables, in order, year, percentile25, mean, percentile75, Louisville
#So if you do aggregation, retitle columns to match
graph_trendline(df,"Working Age Population with a Bachelor's Degree or Higher",
                "Percent of Population")


graph_trendline<-function(df,plot_title,y_title){
data_long<-melt(df, id="year")
p<-ggplot(data=data_long,aes(x=year,y=value,colour=variable,linetype=variable))+
  geom_point()+
  geom_line()
p<-p+theme_tufte()
p<-p+scale_x_continuous(breaks=seq(2005, 2014, 2))
cPalette<-c("grey50","black","grey50","skyblue4")
p<-p+scale_colour_manual(values=cPalette,
                         labels=c("75th percentile", "Peer City Mean", "25th percentile", "Louisville"))+
  scale_linetype_manual(values=c("dashed","dashed","dashed","solid"),
                        labels=c("75th percentile", "Peer City Mean", "25th percentile", "Louisville"))
p<-p+theme(legend.title=element_blank())
p<-p+labs(title=plot_title,x="",
          y=y_title)
p<-p+theme(axis.text=element_text(size=12))
p<-p+theme(axis.ticks.y=element_blank())
p<-p+theme(plot.title=element_text(color="black",size=18,face="bold",hjust=0,
                                   margin=margin(b=10,unit="pt")))
p<-p+theme(legend.text=element_text(size=12))
p
}

#This will take a three year centered rolling mean of a variable
rollmean3<-function(x){
  n<-length(x)
  y<-NA
  for(i in 1:n){
    y[i]<-mean(c(x[i-1],x[i],x[i+1]))
    y[1]<-NA
  }
  y
}
#And this is for 5 years
rollmean5<-function(x){
  n<-length(x)
  y<-NA
  for(i in 1:n){
    y[i]<-mean(c(x[i-2],x[i-1],x[i],x[i+1],x[i+2]))
    y[1]<-NA
    y[2]<-NA
  }
  y
}