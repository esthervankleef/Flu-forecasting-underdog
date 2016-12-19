##############################################
# PLOT OUR PERFORMANCE 
##############################################

rm(list = ls())
script_name <- "plot_performance"

# libraries
library(ggplot2)
library(dplyr)
########################################
#### load data
# flu data
load("./Data/data_manip_2.Rda")
source("./multiplot.R")

# Grab the submitted files from the submission folder
filenames <- list.files("Forecasts/Submissions", pattern="*.csv", full.names=TRUE) 
ldf <- lapply(filenames, read.csv)

# Create a new data from for plotting
datas_point=NULL
datas_seas=NULL
for(i in 1:length(ldf)){
  data = ldf[[i]]
  if(i<6){
  data = data[,-which(names(data)=="X")]
  }
  #print(data)
  data_point = data[data$Type =="Point" & data$Location == "US National"&
                      data$Target %in% c("1 wk ahead","2 wk ahead","3 wk ahead","4 wk ahead"),]
   
  data_point$pred_week = c(42+i+c(1:4)) # Add the prediction week for plotting reference
  data_point$week = rep(42+i,length(data_point$Location))
  datas_point = rbind(datas_point, data_point) 
  data_seas = data[data$Location == "US National"&
                      data$Target %in% c("Season onset","Season peak week","Season peak percentage"),]
  data_seas$week = rep(42+i,length(data_seas$Location))
  datas_seas = rbind(datas_seas, data_seas)
}

DF1 = DF %>% filter(hyear==2016&hweek>=40|hyear==2017)
DF1$plot_week = c(1:length(DF1$hweek)) # Create a dummy variable so the weeks can plotted chronologically, then plot without axis and add labels manually after
datas_point$plot_week = ifelse(datas_point$week%in%c(43:52),datas_point$pred_week-39,datas_point$pred_week+13) # This is to make sure that the actual week is aligning with the plotted week
datas_seas$plot_week = ifelse(datas_seas$Value%in%c(43:52),datas_seas$Value-39,datas_seas$Value+13) # This is to make sure that the actual week is aligning with the plotted week

pdf("./Forecasts/National predictions.pdf", width=7,height=5)  
par(mfrow=c(1,1))
plot(DF1$plot_week, DF1$x.weighted.ili, ylim=c(0,8), axes=F, ylab="Cases",xlab="Week",type="l",
     lwd=6, main=paste("National Influenza Forecast 2016/2017"),
     col="grey")
lines(rep(2.2, 52), lty=2) # Plot treshold
axis(2, at=0:8, labels=c(0:8))
axis(1, at=1:length(DF1$hweek), labels=DF1$hweek)
for(i in 1:length(unique(datas_point$week))){ # Plot for each prediction week the 4 mean point predictions
  plot = datas_point[datas_point$week==i+42,]
  points(plot$plot_week,plot$Value, col=i, pch=16)
  lines(plot$plot_week,plot$Value, col=i, pch=16)
  text(x=42, y=8-i+0.8,labels = paste("week",i+42), col=i)
}
for(i in 1:length(unique(datas_seas$week))){ # Plot for each prediction week the mean season targets
  for(f in c("Season onset","Season peak week")){
    p = ifelse(f=="Season onset",1,2)
    plot = datas_seas[datas_seas$week==i+42&datas_seas$Target==f&datas_seas$Type=="Point",]
    intense = datas_seas[datas_seas$week==i+42&datas_seas$Target=="Season peak percentage"&datas_seas$Type=="Point",]
    points(x=plot$plot_week,y=intense$Value, col=i, pch=p)
  }
}
legend(x = 1, y= 8, pch=c(1,2), legend=c("Season onset", "Season peak week"))
dev.off()

#gsub("2016-","",most_current_week)
#current_pred =as.numeric(gsub("2016-","",most_current_week))-1

# Create seperate data frame for season onset and season peak week for plotting
DF2 = datas_seas %>% filter(Target=="Season onset"&Type!="Point"&
                        data_seas$Bin_start_incl!="none")%>%mutate(Bin_start_incl_plot = as.numeric(as.character(Bin_start_incl)))%>%
                                                            mutate(Bin_start_incl_plot = ifelse(Bin_start_incl_plot%in%c(40:52),Bin_start_incl_plot-39,Bin_start_incl_plot+13))
DF3 = datas_seas %>% filter(Target=="Season peak week"&Type!="Point"&
                              data_seas$Bin_start_incl!="none")%>%mutate(Bin_start_incl_plot = as.numeric(as.character(Bin_start_incl)))%>%
  mutate(Bin_start_incl_plot = ifelse(Bin_start_incl_plot%in%c(40:52),Bin_start_incl_plot-39,Bin_start_incl_plot+13))

# Plot predicted season onset distribution for each prediction week
g_onset = ggplot(DF2, aes(x=Bin_start_incl_plot,y=Value,col=factor(week),group=factor(week)))+geom_point()+scale_x_discrete("week",limits=unique(DF2$Bin_start_incl))+theme_bw()+
  ggtitle(paste("National Influenza Forecast 2016/2017\n Season onset week"))+ylab("Probability")+ylim(0,1)+geom_line()+
  theme(legend.position = c(0.1, 0.7))+
  guides(col=guide_legend(title="Prediction week"))
  
# Plot predicted season peak week distribution for each prediction week
g_peak = ggplot(DF3, aes(x=Bin_start_incl_plot,y=Value,col=factor(week),group=factor(week)))+geom_point()+scale_x_discrete("week",limits=unique(DF2$Bin_start_incl))+theme_bw()+
  ggtitle(paste("National Influenza Forecast 2016/2017\n Season peak week"))+ylab("Probability")+ylim(0,1)+geom_line()+
  theme(legend.position = c(0.1, 0.7))+
  guides(col=guide_legend(title="Prediction week"))

# Save season targets
pdf("./Forecasts/Season_predictions.pdf", width=8, height=8)
multiplot(g_onset,g_peak)
dev.off()
