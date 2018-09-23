#Lab1 Part2
#Name : Vigneshwaran Vasanthakumar UBID#: 50248708
#Team Member's name: Siddharth Selvaraj UBID#:5024
library(plotly)

plotdata <- read.csv(file.choose()) #Choose Book1
plot_data_1<- data.frame(plotdata)

week_1 <- sprintf("%d %02d",as.integer(plot_data_1$Week/100),plot_data_1$Week%%100)
percent_pos_B <- as.list(plot_data_1$Percent.Positive.B)
percent_pos_A <- as.list(plot_data_1$Percent.Positive.A)
percent_pos <- as.list(plot_data_1[,7])
totalB <- as.list(plot_data_1$Total.B)
totalA <- as.list(plot_data_1$Total.A)

plot_1 <- plot_ly(plot_data_1)%>%
  add_trace(x = ~week_1,y = ~percent_pos_B, name = '% Positive Flu B', type = 'scatter', mode = 'lines',
            yaxis = 'y2', line = list(color = 'green', width = 3, dash = 'dot')) %>%
  add_trace(x = ~week_1,y = ~percent_pos_A, name = '% Positive Flu A', type = 'scatter', mode = 'lines',
            yaxis = 'y2',line = list(color = 'yellow', width = 3, dash = 'dash')) %>%
  add_trace(x = ~week_1,y = ~percent_pos, name = 'Percent Positive', type = 'scatter', mode = 'lines', 
            yaxis = 'y2',line = list(color = 'black', width = 3)) %>%
  add_trace(x = ~week_1,y = ~totalB, name = 'B', type = 'bar', yaxis='y',
            marker = list(color = 'forestgreen',line = list(color = 'black',width = 1.5))) %>%
  add_trace(x = ~week_1,y = ~totalA, name = 'A', type = 'bar', yaxis='y',
            marker = list(color = 'gold',line = list(color = 'black',width = 1.5))) %>%
  layout(title = "<b>Influenza Positive Tests Reported to CDC by U.S Clinical Laboratories, \n National Summary, 2017-2018 Season</b>",
         xaxis = list(title = "<b>Week</b>",tickangle = -45,ticks = "outside"),
         yaxis = list (side = 'left',title = "<b>Number of Positive Specimens</b>",showline = TRUE,ticks = "outside",showgrid=FALSE), barmode = 'stack',
         yaxis2 = list (side = 'right', overlaying = "y", title = "<b>Percent Positive</b>",range=c(0,30),showline = TRUE,ticks = "outside",showgrid=FALSE), barmode = 'stack',legend = list(x = 50, y = 1),margin = list(b = 80)
  )
plot_1


#Positive Tested
plotdata <- read.csv(file.choose()) #choose Book2
plot_data_2<- data.frame(plotdata)

week_2<- sprintf("%d %02d",as.integer(plot_data_2$Week/100),plot_data_2$Week%%100)
byam <- as.list(plot_data_2$BYAM)
bvic <- as.list(plot_data_2$BVIC)
b <- as.list(plot_data_2$B)
a_h3n2v <- as.list(plot_data_2$A.H3N2v.)
a_h3 <- as.list(plot_data_2$A.H3.)
a_h1n1 <- as.list(plot_data_2$A.H1N1.pdm09)
a_sub_not_performed <- as.list(plot_data_2$A.Subtyping.not.performed.)

plot_2 <- plot_ly(plot_data_2, x = ~week_2)%>% 
  add_trace(y = ~byam, name = 'B(Yamagata Lineage)', type = 'bar',
            marker = list(color = 'green',line = list(color = 'black',width = 1.5))) %>%
  add_trace(y = ~bvic, name = 'B(Victoria Lineage)', type = 'bar',
            marker = list(color = 'yellowgreen',line = list(color = 'black',width = 1.5))) %>%
  add_trace(y = ~b, name = 'B(lineage not performed)', type = 'bar',
            marker = list(color = 'darkgreen',line = list(color = 'black',width = 1.5))) %>%
  add_trace(y = ~a_h3n2v, name = 'H3N2v', type = 'bar',
            marker = list(color = 'blue',line = list(color = 'black',width = 1.5))) %>%
  add_trace(y = ~a_h3, name = 'A(H3N2)', type = 'bar',
            marker = list(color = 'red',line = list(color = 'black',width = 1.5))) %>%
  add_trace(y = ~a_h1n1, name = 'A(H1N1)pdm09',type = 'bar',
            marker = list(color = 'orange',line = list(color = 'black',width = 1.5))) %>%
  add_trace(y = ~a_sub_not_performed, name = 'A(subtyping not performed)', type = 'bar',
            marker = list(color = 'yellow',line = list(color = 'black',width = 1.5))) %>%
  layout(title = "<b>Influenza Positive Tests Reported to CDC by U.S Public Health Laboratories,\n National Summary, 2017-2018 Season</b>",
         xaxis = list(title = "<b>Week</b>", tickangle = -45,ticks = "outside"),
         yaxis = list (title = "<b>Number of Positive Specimens</b>",showline = TRUE,ticks = "outside",showgrid=FALSE), barmode = 'stack',margin = list(b = 80))

plot_2



#Pediatric deaths 
plotdata <- read.csv(file.choose()) #choose Book3

week_4 <- plotdata$WEEK.NUMBER
previous <- plotdata$PREVIOUS.WEEKS.DEATHS
current <- plotdata$CURRENT.WEEK.DEATHS

plot_data_4 <- data.frame(week_4, previous, current)

plot_4 <- plot_ly(plot_data_4, x = ~week_4)%>%
  add_trace(y = ~previous, name = 'Deaths Reported Previous Week', type = 'bar',width=1, 
            marker = list(color = 'green',line = list(color = 'black',width = 0.15))) %>%
  add_trace(y = ~current, name = 'Deaths Reported Current Week', type = 'bar',width=1, 
            marker = list(color = 'cyan',line = list(color = 'black',width = 0.15))) %>%
  layout(xaxis = list(title = "<b>Week of Death</b>",ticks="outside",tickangle = -90, tickfont = list(size=8)),
         yaxis = list (side = 'left',title = "<b>Number of Deaths</b>",showline = TRUE,ticks = "outside",showgrid=FALSE), barmode = 'stack',legend = list(orientation = 'h',x = 0.15, y = -0.2, borderwidth = 2))%>%
  
  add_annotations(
    x=1,
    y=18.5,
    xref = "x",
    yref = "y",
    text = "<b>2014-2015\nNumber of Deaths\nReported=148</b>",
    xanchor = 'left',
    showarrow = F
  )%>%
  add_annotations(
    x=55,
    y=18.5,
    xref = "x",
    yref = "y",
    text = "<b>2015-2016\nNumber of Deaths\nReported=93</b>",
    xanchor = 'left',
    showarrow = F
  )%>%
  add_annotations(
    x=105,
    y=18.5,
    xref = "x",
    yref = "y",
    text = "<b>2016-2017\nNumber of Deaths\nReported=110</b>",
    xanchor = 'left',
    showarrow = F
  )%>%
  add_annotations(
    x=155,
    y=18.5,
    xref = "x",
    yref = "y",
    text = "<b>2017-2018\nNumber of Deaths\nReported=114</b>",
    xanchor = 'left',
    showarrow = F
  )%>%
  add_annotations(
    x=40,
    y=22.5,
    xref = "x",
    yref = "y",
    text = "<b>Number of Influenza-Associated Pediatric Deaths\n by Week of Death:2014-2015 season to present</b>",
    xanchor = 'left',
    showarrow = F, font=list(size=16),face="bold"
  )

plot_4



#Flu heat map of USA 
library(plotly)
library(ggmap)
library(ggplot2)
library(RColorBrewer)
library(maps)

data_maps <- data.table::as.data.table(read.csv(file.choose()))  #choose StateDataforMap_2017-18week4
data_map1s <-data_maps
data_map1s$ACTIVITY.LEVEL <- gsub("Level 10",10, data_map1s$ACTIVITY.LEVEL)
data_map1s$ACTIVITY.LEVEL <- gsub("Level 9",9, data_map1s$ACTIVITY.LEVEL)
data_map1s$ACTIVITY.LEVEL <- gsub("Level 8",8, data_map1s$ACTIVITY.LEVEL)
data_map1s$ACTIVITY.LEVEL <- gsub("Level 7",7, data_map1s$ACTIVITY.LEVEL)
data_map1s$ACTIVITY.LEVEL <- gsub("Level 6",6, data_map1s$ACTIVITY.LEVEL)
data_map1s$ACTIVITY.LEVEL <- gsub("Level 5",5, data_map1s$ACTIVITY.LEVEL)
data_map1s$ACTIVITY.LEVEL <- gsub("Level 4",4, data_map1s$ACTIVITY.LEVEL)
data_map1s$ACTIVITY.LEVEL <- gsub("Level 3",3, data_map1s$ACTIVITY.LEVEL)
data_map1s$ACTIVITY.LEVEL <- gsub("Level 2",2, data_map1s$ACTIVITY.LEVEL)
data_map1s$ACTIVITY.LEVEL <- gsub("Level 1",1, data_map1s$ACTIVITY.LEVEL)
data_map1s$ACTIVITY.LEVEL <- gsub("Level 0",0, data_map1s$ACTIVITY.LEVEL)


data_map1s$region <- tolower(data_map1s$STATENAME)

geo_data <-data.table::as.data.table(map_data("state"))

data.table::setkey(geo_data,region)
data.table::setkey(data_map1s,region)

data_for_map <-geo_data[data_map1s]

fills <- c('limegreen','yellow','red3','green','lawngreen','greenyellow','yellow2','orange','tomato')
map <- ggplot() + geom_polygon(data = data_for_map,aes(x = long,y = lat, group = group,fill=factor(ACTIVITY.LEVEL))
                               ,color="black")+ labs(fill = "ILI Activity Level",title = "2017-18 Influenza Season Week 4 ending Jan 27, 2018"
                                                     , x="", y="")+coord_fixed(1.3)+ scale_fill_manual(values = fills)
map+ theme(panel.background = element_blank(),axis.text.x=element_blank(),
           axis.ticks.x=element_blank(),axis.text.y=element_blank(),
           axis.ticks.y=element_blank())




#Influenza sub-type pie-charts
piechart_data <- c(18068,1896,348,228,2292,921)
pie_labels <- paste(piechart_data)
pie_chart_color <- c("red","orange","yellow","lightgreen","green","darkgreen")
pie(piechart_data,main="Influenza Positive Specimens Reported by U.S. Public Health Laboratories, Cumulative, 2017-2018 season",
    cex.main=0.7,labels=pie_labels,radius=0.5,col=pie_chart_color,clockwise=1)
legend("bottom", c("Influenza A(H3N2)","Influenza A(H3N2)","Influenza A(subtype unknown)","Influenza B Victoria","Influenza B Yamagata
                   ","Influenza B(lineage not determined)"), cex = 0.4, fill=pie_chart_color)



subtype_comp_data <- read.csv(file.choose(),sep=",")  #choose Genetic04 file

pie_subplot1_data <- subtype_comp_data[9:11,3]
pie_subplot1_labels1 <- subtype_comp_data[9:11,2]
pie_subplot1_labels2 <- subtype_comp_data[9:11,3]
pie_subplot1_labels3 <- subtype_comp_data[9:11,4]
pie_subplot1_labels <- paste(pie_subplot1_labels1,pie_subplot1_labels2,pie_subplot1_labels3,sep="\n")
pie_subplot1_color <- c("tomato2","pink","red2")


pie_subplot2_data <- subtype_comp_data[6,3]
pie_subplot2_labels1 <- subtype_comp_data[6,2]
pie_subplot2_labels2 <- subtype_comp_data[6,3]
pie_subplot2_labels3 <- subtype_comp_data[6,4]
pie_subplot2_labels <- paste(pie_subplot2_labels1,pie_subplot2_labels2,pie_subplot2_labels3,sep="\n")
pie_subplot2_color <- c("lightgoldenrod1")


pie_subplot3_data <- subtype_comp_data[1:2,3]
pie_subplot3_labels1 <- subtype_comp_data[1:2,2]
pie_subplot3_labels2 <- subtype_comp_data[1:2,3]
pie_subplot3_labels3 <- subtype_comp_data[1:2,4]
pie_subplot3_labels <- paste(pie_subplot3_labels1,pie_subplot3_labels2,pie_subplot3_labels3,sep="\n")
pie_subplot3_color <- c("olivedrab1","khaki1")


pie_subplot4_data <- subtype_comp_data[3,3]
pie_subplot4_labels1 <- subtype_comp_data[3,2]
pie_subplot4_labels2 <- subtype_comp_data[3,3]
pie_subplot4_labels3 <- subtype_comp_data[3,4]
pie_subplot4_labels <- paste(pie_subplot4_labels1,pie_subplot4_labels2,pie_subplot4_labels3,sep="\n")
pie_subplot4_color <- c("seagreen3")

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
pie(pie_subplot1_data,main="Influenza A(H3N2)",cex.main=0.8,col=pie_subplot1_color,radius=1,labels=pie_subplot1_labels,cex=0.8,clockwise=1)
pie(pie_subplot2_data,main="Influenza A(H1N1)pdm09",cex.main=0.8,col=pie_subplot2_color,radius=1,labels=pie_subplot2_labels,cex=0.8,clockwise=1)
pie(pie_subplot3_data,main="Influenza B Victoria",cex.main=0.8,col=pie_subplot3_color,radius=1,labels=pie_subplot3_labels,cex=0.8,clockwise=1)
pie(pie_subplot4_data,main="Influenza B Yamagata",cex.main=0.8,col=pie_subplot4_color,radius=1,labels=pie_subplot4_labels,cex=0.8,clockwise=1)


