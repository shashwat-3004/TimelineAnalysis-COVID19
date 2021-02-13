# Attaching the packages
library(tidyverse)
library(plotly)
library(lubridate)
library(gganimate)
library(scales)
library(ggrepel)
library(maps)
library(ggthemes)

########################################################
# Pre-prcoessing
data<-read.csv("owid-covid-data.csv")

country<-c("China","India","United Kingdom","Japan","United States")

data$date<-ymd(data$date)

data_country<-data%>%filter(location %in% country)
###########################################################
# Total-Cases (Specific Countries animated)

plot1<-data_country%>%ggplot(aes(x=date,y=total_cases,group=location,colour=location))+geom_line()+
  geom_point()+transition_reveal(date)+ scale_y_continuous(labels = scales::comma)+
  geom_text(aes(x=date,y=total_cases+3,label=location),size=4)+xlab("Date")+ylab("Total Cases(Country-Wise)")+
  ggtitle("Total cases vs Month")+coord_cartesian(clip = 'off') + 
  ease_aes('cubic-in-out')+theme(legend.position = "none")

animate(plot1, fps = 8, width = 800, height = 480)

anim_save("country_cases2.gif",plot1)

############################################################
## World plot

world_country<-read.csv("world_country.csv")
world_country<-world_country[,1:4]

colnames(world_country)<-c("Country_code","Lat","Long","location")
req_data<-semi_join(data,world_country)
req_data<-inner_join(req_data,world_country)
req_data$date<-ymd(req_data$date)


world_map <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() 

world_covid<-world_map+geom_point(data=req_data,aes(x=Long,y=Lat,size=total_cases,group=location),colour="red",alpha=0.5,size=4)+
  theme(legend.position = "none",plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1))+ 
  transition_states(date)+
  labs(title = 'COVID-19 WORLD : {closest_state}',  
      caption  = "Source:https://ourworldindata.org/coronavirus-source-data")


animate(world_covid,fps = 3,  width = 850,height = 450, 
        renderer = gifski_renderer("world_covid1.gif"))
#############################################################
# Animated bar plot(Total cases(date-wise))

data_filtered<-req_data%>% group_by(date) %>% mutate(rank=rank(-total_cases)) %>% filter(rank<=10 & !is.na(total_cases)) 

my_theme<-theme(axis.line=element_blank(),
                axis.text.x=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position="none",
                plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
                plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
                plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
                plot.background=element_blank(),
                plot.margin = margin(2,2, 2, 4, "cm"))


top_country<- data_filtered %>% ggplot(aes(rank,group=location,colour=as.factor(location),
                                    fill=as.factor(location)))+
  geom_tile(aes(y=total_cases/2,height=total_cases),width=0.9,alpha=1,color=NA)+
  geom_text(y=0,aes(label=paste(location," "),hjust=1,vjust=0.2))+
  geom_text(aes(y=total_cases,label=as.character(total_cases)),hjust=0,vjust=0.2)+
  coord_flip(expand=FALSE,clip="off")+
  scale_x_reverse()+my_theme+guides(color = FALSE, fill = FALSE)

ten_country<-top_country+transition_states(date)+
  ease_aes("cubic-in-out")+
  labs(title = 'COVID-19 WORLD : {closest_state}',  
       subtitle  =  "Top 10 worst affected countries",
       caption  = "Source:https://ourworldindata.org/coronavirus-source-data")

animate(ten_country, nframes = 200,fps = 20,  width = 900, height = 700, 
        renderer = gifski_renderer("topten.gif"),duration = 90)
####################################################################

# Removing some columns
req_data<-req_data[,c(-7,-(10:19),-21,-23,-25,-(28:31),-33,-37,-(39:44),-46,-51,-52,-56,-58,-59)]
#####################################################################

# Specific countries (new cases) plot


new_case_country<-req_data%>%filter(location %in% country & !is.na(total_cases) )%>% 
  ggplot(aes(x=date,y=new_cases,group=location,colour=as.factor(location)))+
  geom_line(size=1,alpha=0.5)+
  geom_point()+scale_y_continuous(labels = scales::comma)+geom_text(aes(x=date,y=new_cases+3,label=location),size=4)+
  theme(legend.position = "none")+transition_reveal(date)+
  ease_aes('cubic-in-out')+xlab("Date")+ylab("New cases")+ggtitle("New cases per day(Specific countries)")+
  theme(plot.title = element_text(hjust=0.5))

animate(new_case_country, nframes = 200,fps = 20,  width = 900, height = 700, 
        renderer = gifski_renderer("newcases.gif"),duration = 60)

#####################################################################

## The plots (EDA)

req_data%>% group_by(location)%>%summarise(total_death=sum(new_cases,na.rm = TRUE))%>% slice_max(total_death,n=10)%>%
  mutate(location=reorder(location,total_death))%>%
  ggplot()+geom_col(aes(x=location,y=total_death,fill=location))+coord_flip()+scale_y_continuous(labels = scales::comma)+
  ylab("Total Death")+xlab("Countries")+theme_gdocs()+theme(legend.position = "none",plot.title = element_text(hjust=0.5))+
  ggtitle("Total Deaths(Top 10 Countries)")

req_data%>% group_by(location)%>%summarise(total_death=sum(new_cases,na.rm = TRUE))%>% slice_min(total_death,n=10)%>%
  mutate(location=reorder(location,total_death))%>%
  ggplot()+geom_col(aes(x=location,y=total_death,fill=location))+coord_flip()+scale_y_continuous(labels = scales::comma)+
  ylab("Total Death")+xlab("Countries")+theme_gdocs()+theme(legend.position = "none",plot.title = element_text(hjust=0.5))+
  ggtitle("Total Deaths(Bottom 10 Countries)")



req_data%>%group_by(location)%>%summarise(vaccination=sum(new_vaccinations,na.rm=TRUE))%>% slice_max(vaccination,n=10)%>%
  mutate(location=reorder(location,vaccination))%>%ggplot()+geom_col(aes(x=location,y=vaccination,fill=location))+
  coord_flip()+ylab("Total Vaccinations")+xlab("Countries")+theme_gdocs()+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+ggtitle("Total vaccinations(Top 10 Countries)")


req_data%>%group_by(location)%>%summarise(total_case=sum(new_cases,na.rm=TRUE))%>% slice_min(total_case,n=10)%>%
  mutate(location=reorder(location,total_case))%>%ggplot()+geom_col(aes(x=location,y=total_case,fill=location))+
  coord_flip()+ylab("Total Cases")+xlab("Countries")+theme_gdocs()+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+ggtitle("Total Cases(Bottom 10 Countries)")


req_data%>%filter(continent=="Asia")%>%group_by(location)%>%
  summarise(cases=sum(new_cases,na.rm=TRUE),deaths=sum(new_deaths,na.rm=TRUE))%>%slice_max(cases,n=10)%>%
  mutate(location=reorder(location,cases))%>%
  ggplot()+geom_col(aes(x=location,y=cases),fill="red")+
  coord_flip()+xlab("Countries")+ylab("Cases")+ggtitle("Top 10 countires affected in Asia(Total Cases)")+
  theme(plot.title = element_text(hjust=0.5))+scale_y_continuous(labels = scales::comma)

req_data%>%filter(continent=="Asia")%>%group_by(location)%>%
  summarise(cases=sum(new_cases,na.rm=TRUE),deaths=sum(new_deaths,na.rm=TRUE))%>%slice_max(deaths,n=10)%>%
  mutate(location=reorder(location,deaths))%>%
  ggplot()+geom_col(aes(x=location,y=deaths),fill="black")+
  coord_flip()+xlab("Countries")+ylab("Cases")+ggtitle("Top 10 countires affected in Asia(Deaths)")+
  theme(plot.title = element_text(hjust=0.5))+scale_y_continuous(labels = scales::comma)


############################################################
# Filtering the needed 4 countires
#############################################################
india<-req_data%>%filter(location=="India")
china<-req_data%>%filter(location=="China")
usa<-req_data%>%filter(location=="United States")
uk<-req_data%>%filter(location=="United Kingdom")
#############################################################
#Some EDA

india%>%ggplot()+geom_line(aes(x=date,y=total_cases),colour="red")+scale_y_continuous(labels=scales::comma)+
  xlab("Date")+ylab("Total Cases")+ggtitle("Total cases with date(India)")+theme(plot.title = element_text(hjust = 0.5))

china%>%ggplot()+geom_line(aes(x=date,y=total_cases),colour="red")+scale_y_continuous(labels=scales::comma)+
  xlab("Date")+ylab("Total Cases")+ggtitle("Total cases with date(China)")+theme(plot.title = element_text(hjust = 0.5))

usa%>%ggplot()+geom_line(aes(x=date,y=total_cases),colour="red")+scale_y_continuous(labels=scales::comma)+
  xlab("Date")+ylab("Total Cases")+ggtitle("Total cases with date(USA)")+theme(plot.title = element_text(hjust = 0.5))

uk%>%ggplot()+geom_line(aes(x=date,y=total_cases),colour="red")+scale_y_continuous(labels=scales::comma)+
  xlab("Date")+ylab("Total Cases")+ggtitle("Total cases with date(UK)")+theme(plot.title = element_text(hjust = 0.5))



###############################################################
# Animated plots for new cases and new death (4 countries)


ind<-india%>%ggplot()+geom_line(aes(x=date,y=new_cases),colour="red")+geom_line(aes(x=date,y=new_deaths))+
  scale_y_continuous(labels=scales::comma)+
  xlab("Date")+ylab("New Cases/Death")+ggtitle(" New cases and death with date(India)")+
    theme(plot.title = element_text(hjust = 0.5))+transition_reveal(date)

anim_save("ind.gif",ind)

chi<-china%>%ggplot()+geom_line(aes(x=date,y=new_cases),colour="red")+geom_line(aes(x=date,y=new_deaths))+
  scale_y_continuous(labels=scales::comma)+
  xlab("Date")+ylab("New Cases/Death")+ggtitle(" New cases and death with date(China)")+
  theme(plot.title = element_text(hjust = 0.5))+transition_reveal(date)
anim_save("chi.gif",chi)

us<-usa%>%ggplot()+geom_line(aes(x=date,y=new_cases),colour="red")+geom_line(aes(x=date,y=new_deaths))+
  scale_y_continuous(labels=scales::comma)+
  xlab("Date")+ylab("New Cases/Death")+ggtitle(" New cases and death with date(USA)")+
  theme(plot.title = element_text(hjust = 0.5))+transition_reveal(date)
anim_save("us.gif",us)

u<-uk%>%ggplot()+geom_line(aes(x=date,y=new_cases),colour="red")+geom_line(aes(x=date,y=new_deaths))+
  scale_y_continuous(labels=scales::comma)+
  xlab("Date")+ylab("New Cases/Death")+ggtitle(" New cases and death with date(UK)")+
  theme(plot.title = element_text(hjust = 0.5))+transition_reveal(date)
anim_save("u.gif",u)



#Data-source:  
  
# https://ourworldindata.org/coronavirus-source-data


#https://www.kaggle.com/paultimothymooney/latitude-and-longitude-for-every-country-and-state



