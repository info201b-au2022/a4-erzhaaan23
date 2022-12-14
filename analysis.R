names(df)
library(rlang)
library(ggplot2)
library("reshape2") 
library(plotly)

# Define function to get county wise avg
get_county_wise_avg<-function(df,variable,yearx){
  df1<-df%>%
    filter(year==yearx,!is.na(!!sym(variable)))%>%
    group_by(county_name)%>%
    summarize(total=sum((!!sym(variable))))
  
    round(mean(df1$total),2)
}

#define function to get maximum county
get_max_county<-function(df=df,variable,yearx=1980){
  
  df1<-df%>%
    filter(year==yearx,!is.na(!!sym(variable)),!!sym(variable)>0)
  
  if(nrow(df1)==0){NULL}else{
  df1$county_name[df1[,variable]==max(df1[,variable],na.rm = T)]
  }
}

#define function to get minimum county
get_min_county<-function(df=df,variable,yearx=1980){
  
  df1<-df%>%
    filter(year==yearx,!is.na(!!sym(variable)),!!sym(variable)>0)
  
  if(nrow(df1)==0){NULL}else{
    df1$county_name[df1[,variable]==min(df1[,variable],na.rm = T)]
  }
}

# Define function to get yearly jail populations
get_year_jail_pop<-function(){
  
  df1<-readRDS("data/df.Rds")%>%
    filter(!is.na(total_jail_pop))%>%
    group_by(year)%>%
    summarise(total_jail_pop=sum(total_jail_pop))
  
  df1
}

# define the function to generate the plot for yearly jail populations
plot_jail_pop_for_us<-function(){
  df1<-get_year_jail_pop()
  
  ggplot(data=df1, aes(x=year, y=total_jail_pop)) +
    geom_bar(stat="identity")+ylab("Total Jail Population")+
    labs(title = "Total Population growth in Jails in US", caption = "The total population across all countys")
}

#define the function to get state wise jail population

get_jail_pop_by_state<-function(states){
  
  df1<-readRDS("data/df.Rds")%>%
    filter(state%in%states)%>%
    group_by(year,state)%>%
    summarise(jail_pop=sum(total_jail_pop,na.rm = T))%>%
    tidyr::spread(key = state,value = jail_pop)
  
}

# define function to plot state wise yearly jail population

plot_jail_pop_by_state<-function(states){
  df1<-get_jail_pop_by_state(states=states)
  
  class(df)
  plot_dt <- melt(df1, id = "year")
  
  gfg_plot <- ggplot(plot_dt,            
                     aes(x = year,
                         y = value,
                         color = variable)) +  geom_line() +
    
    labs(title = "State wise jail population", caption = "State wise jail population is plotted here for selected 5 states")
  gfg_plot
}

get_wb_plot<-function(df){
  
  df1<-df%>%
    group_by(year)%>%
    summarise(black_jail_pop=sum(black_jail_pop,na.rm = T),white_jail_pop=sum(white_jail_pop,na.rm = T))
  
  df1 <- melt(df1, id = "year")
  
  ggplot(df1,            
         aes(x = year,
             y = value,
             color = variable)) +  geom_line() +
    
    labs(title = "Black/White Jail population", caption = "Black/ White jail population is yearly plotted.")
}


# Get State wise black/white ratio


get_bw_ratio<-function(df){
  df1<-df%>%
    select(year,state,black_jail_pop,white_jail_pop)%>%
    group_by(year,state)%>%
    summarise(black_jail_pop=sum(black_jail_pop,na.rm = T),white_jail_pop=sum(white_jail_pop,na.rm = T))%>%
    mutate(ratio=black_jail_pop/white_jail_pop)%>%
    filter(!is.na(ratio),!is.infinite(ratio),ratio!=0)%>%
    arrange(desc(ratio))%>%
    select(year,state,ratio)
  
}


# Map of the black/white ratio

get_bw_map<-function(df=toMap){
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
  )
  
  fig <- plot_ly(df, locationmode='USA-states',type='choropleth', locations=df$state, z=df$ratio, text=df$state, colorscale="reds")%>% layout(
    geo = g
  )
  
  fig
}



