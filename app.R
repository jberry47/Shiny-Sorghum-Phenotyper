library(ggplot2)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(data.table)
library(dplyr)
library(scales)
library(colorRamps)
library(lme4)

nitrogen <- read.csv("data/sorg_nitrogen_shiny.csv",header = T)
nitrogen$Treatment <- ordered(nitrogen$Treatment,levels=c(100,50,10),labels=c("100% / 100%","50% / 10%","10% / 10%"))

drought <- read.csv("data/sorg_drought_shiny.csv",header = T)
drought$Treatment <- ordered(drought$Treatment,levels=c(100,70,50),labels=c("100%","70%","Recovery"))

number_ticks <- function(n) {function(limits) pretty(limits, n)}
beginning <- 8; end <- 26
area_convert <- 13.2*3.7/46856
line_convert <- sqrt(area_convert)
shapes <- c("area", "Hull.area", "solidity", "perimeter", "width", "height", "longest_axis", "Center.of.mass.x", "Center.of.mass.y", "hull_verticies", "ellipse_center_x", "ellipse_center_y", "ellipse_major_axis", "ellipse_minor_axis", "ellipse_angle", "ellipse_eccentricity")
genotypes <- c('Atlas','B.Az9504','BTx623','China 17','Chinese Amber','CK60B','Della','Grassl','ICSV700','Leoti','M81e','NTJ2','PI_152730','PI_195754','PI_213900','PI_229841','PI_297130','PI_297155','PI_329311','PI_329632','PI_35038','PI_505735','PI_506069','PI_508366','PI_510757','PI_585954','PI_642998','PI_655972','Rio','San Chi San')

hist_avg <- function(data,day){
  sub <- data[data$DAP==day,]
  test <- data.frame(do.call("rbind",
            lapply(split(sub,sub$Treatment),function(t){
                colMeans(t[,12:(ncol(t)-1)])
            })
          ))
  return(test)
}
hist_sd <- function(data,day){
  sub <- data[data$DAP==day,]
  test <- data.frame(do.call("rbind",lapply(split(sub,sub$Treatment),function(t){
      apply(t[,12:(ncol(t)-1)],2,function(i){sd(i)/sqrt(length(i))})})
  ))
  return(test)
}

plot_histo_nitrogen <- function(channel,color,genotype,day=""){
  sub <- channel[channel$DAP==day,]
  test <- hist_avg(sub,day)
  test_sd <- hist_sd(sub,day)
  
  if(color=="hue"){
    df <- data.frame("one"=t(test[1,1:180]),"two"=t(test[2,1:180]),"three"=t(test[3,1:180]),"bin"=0:179*2)
    names(df) <- c("10","50","100","bin")
    df <- melt(df,id="bin")
    df$variable <- ordered(df$variable,levels=c("100","50","10"),labels=c("100% / 100%","50% / 10%","10% / 10%"))
    
    df_sd <- data.frame("one"=t(test_sd[1,1:180]),"two"=t(test_sd[2,1:180]),"three"=t(test_sd[3,1:180]),"bin"=0:179*2)
    names(df_sd) <- c("10","50","100","bin")
    df_sd <- melt(df_sd,id="bin")
    df_sd$variable <- ordered(df_sd$variable,levels=c("100","50","10"),labels=c("100% / 100%","50% / 10%","10% / 10%"))
    
    df$sd <- df_sd$value
    limits <- aes(ymax = value + 1.96*sd, ymin=value - 1.96*sd)
    
    ggplot(data=df,aes(bin,value))+
      ggtitle(paste(as.character(genotype)," (Day ",day,")",sep="",collapse = ""))+
      facet_wrap(~variable)+
      geom_ribbon(limits,fill="gray80")+
      geom_line(aes(color=bin),size=2)+
      scale_color_gradientn(colors=hue_pal(l=65)(180))+
      scale_x_continuous(breaks=number_ticks(5),limits = c(0,120))+
      #scale_y_continuous(limits = c(0,12))+
      ylab("Percentage of Mask Explained")+
      xlab("")+
      xlab("Hue Channel")+
      theme_light()+
      theme(legend.position='none')+
      theme(strip.background=element_rect(fill="gray50"),
            strip.text.x=element_text(size=14,color="white"),
            strip.text.y=element_text(size=14,color="white"))+
      theme(plot.title = element_text(hjust = 0.5),
            axis.text = element_text(size = 12),
            axis.title= element_text(size = 18))
  }else{
    df <- data.frame("one"=t(test[1,]),"two"=t(test[2,]),"three"=t(test[3,]),"bin"=0:255)
    names(df) <- c("10","50","100","bin")
    df <- melt(df,id="bin")
    df$variable <- ordered(df$variable,levels=c("100","50","10"),labels=c("100% / 100%","50% / 10%","10% / 10%"))
    
    df_sd <- data.frame("one"=t(test_sd[1,]),"two"=t(test_sd[2,]),"three"=t(test_sd[3,]),"bin"=0:255)
    names(df_sd) <- c("10","50","100","bin")
    df_sd <- melt(df_sd,id="bin")
    df_sd$variable <- ordered(df_sd$variable,levels=c("100","50","10"),labels=c("100% / 100%","50% / 10%","10% / 10%"))
    
    df$sd <- df_sd$value
    limits <- aes(ymax = value + 1.96*sd, ymin=value - 1.96*sd)
    
    if(color %in% c("red","blue","green")){
      my_col <- paste(toupper(substring(color,1,1)),substring(color,2),"s",sep="")
      my_grad <- scale_color_gradient(low = "black",high = as.character(color))
      my_line <- geom_line(aes(color=bin),size=2)
    }else if(color == "green-magenta"){
      my_grad <- scale_color_gradientn(colors=colorRamps::magenta2green(255))
      my_line <- geom_line(aes(color=bin),size=2)
    }else if(color == "blue-yellow"){
      my_grad <- scale_color_gradientn(colors=colorRamps::blue2yellow(255))
      my_line <- geom_line(aes(color=bin),size=2)
    }else{
      my_grad <- NULL
      my_line <- geom_line(color="black",size=1)
    }
    ggplot(data=df,aes(bin,value))+
      ggtitle(paste(as.character(genotype)," (Day ",day,")",sep="",collapse = ""))+
      facet_wrap(~variable)+
      geom_ribbon(limits,fill="gray80")+
      my_line+
      my_grad+
      scale_x_continuous(breaks=number_ticks(5))+
      ylab("Percentage of Mask Explained")+
      xlab(paste(color,"channel"))+
      theme_light()+
      theme(legend.position='none')+
      theme(plot.title = element_text(hjust = 0.5),
            strip.background=element_rect(fill="gray50"),
            strip.text.x=element_text(size=14,color="white"),
            strip.text.y=element_text(size=14,color="white"))+
      theme(axis.text = element_text(size = 12),
            axis.title= element_text(size = 18))
  }
}
plot_histo_drought <- function(channel,color,genotype,day=""){
  sub <- channel[channel$DAP==day,]
  test <- hist_avg(sub,day)
  test_sd <- hist_sd(sub,day)
  
  if(color=="hue"){
    df <- data.frame("one"=t(test[1,1:180]),"two"=t(test[2,1:180]),"three"=t(test[3,1:180]),"bin"=0:179*2)
    names(df) <- c("Recovery","70","100","bin")
    df <- melt(df,id="bin")
    df$variable <- ordered(df$variable,levels=c("100","70","Recovery"),labels=c("100%","70%","Recovery"))
    
    df_sd <- data.frame("one"=t(test_sd[1,1:180]),"two"=t(test_sd[2,1:180]),"three"=t(test_sd[3,1:180]),"bin"=0:179*2)
    names(df_sd) <- c("Recovery","70","100","bin")
    df_sd <- melt(df_sd,id="bin")
    df_sd$variable <- ordered(df_sd$variable,levels=c("100%","70%","Recovery"))
    
    df$sd <- df_sd$value
    limits <- aes(ymax = value + 1.96*sd, ymin=value - 1.96*sd)
    
    ggplot(data=df,aes(bin,value))+
      ggtitle(paste(as.character(genotype)," (Day ",day,")",sep="",collapse = ""))+
      facet_wrap(~variable)+
      geom_ribbon(limits,fill="gray80")+
      geom_line(aes(color=bin),size=2)+
      scale_color_gradientn(colors=hue_pal(l=65)(180))+
      scale_x_continuous(breaks=number_ticks(5),limits = c(0,120))+
      #scale_y_continuous(limits = c(0,12))+
      ylab("Percentage of Mask Explained")+
      xlab("")+
      xlab("Hue Channel")+
      theme_light()+
      theme(legend.position='none')+
      theme(strip.background=element_rect(fill="gray50"),
            strip.text.x=element_text(size=14,color="white"),
            strip.text.y=element_text(size=14,color="white"))+
      theme(plot.title = element_text(hjust = 0.5),
            axis.text = element_text(size = 12),
            axis.title= element_text(size = 18))
  }else{
    df <- data.frame("one"=t(test[1,]),"two"=t(test[2,]),"three"=t(test[3,]),"bin"=0:254)
    names(df) <- c("Recovery","70","100","bin")
    df <- melt(df,id="bin")
    df$variable <- ordered(df$variable,levels=c("100","70","Recovery"),labels=c("100%","70%","Recovery"))
    
    df_sd <- data.frame("one"=t(test_sd[1,]),"two"=t(test_sd[2,]),"three"=t(test_sd[3,]),"bin"=0:254)
    names(df_sd) <- c("Recovery","70","100","bin")
    df_sd <- melt(df_sd,id="bin")
    df_sd$variable <- ordered(df_sd$variable,levels=c("100","70","Recovery"),labels=c("100%","70%","Recovery"))
    
    df$sd <- df_sd$value
    limits <- aes(ymax = value + 1.96*sd, ymin=value - 1.96*sd)
    
    if(color %in% c("red","blue","green")){
      my_col <- paste(toupper(substring(color,1,1)),substring(color,2),"s",sep="")
      my_grad <- scale_color_gradient(low = "black",high = as.character(color))
      my_line <- geom_line(aes(color=bin),size=2)
    }else if(color == "green-magenta"){
      my_grad <- scale_color_gradientn(colors=colorRamps::magenta2green(255))
      my_line <- geom_line(aes(color=bin),size=2)
    }else if(color == "blue-yellow"){
      my_grad <- scale_color_gradientn(colors=colorRamps::blue2yellow(255))
      my_line <- geom_line(aes(color=bin),size=2)
    }else{
      my_grad <- NULL
      my_line <- geom_line(color="black",size=1)
    }
    ggplot(data=df,aes(bin,value))+
      ggtitle(paste(as.character(genotype)," (Day ",day,")",sep="",collapse = ""))+
      facet_wrap(~variable)+
      geom_ribbon(limits,fill="gray80")+
      my_line+
      my_grad+
      scale_x_continuous(breaks=number_ticks(5))+
      ylab("Percentage of Mask Explained")+
      xlab(paste(color,"channel"))+
      theme_light()+
      theme(legend.position='none')+
      theme(plot.title = element_text(hjust = 0.5),
            strip.background=element_rect(fill="gray50"),
            strip.text.x=element_text(size=14,color="white"),
            strip.text.y=element_text(size=14,color="white"))+
      theme(axis.text = element_text(size = 12),
            axis.title= element_text(size = 18))
  }
}
histoDiff_nitrogen <- function(channel,color,genotype,day,compare){
  sub <- channel[channel$DAP==day,]
  test <- hist_avg(sub,day)
  test_sd <- hist_sd(sub,day)
  if(compare == "100%/100% vs 10%/10%"){
    x <- 1; y <- 3
  }else if(compare == "100%/100% vs 50%/10%"){
    x <- 2; y <- 3
  }else{
    x <- 1; y <- 2
  }
  
  if(color=="hue"){
    df <- data.frame("one"=t(test[x,1:180]),"two"=t(test[y,1:180]),"bin"=0:179*2)
    df_sd <- data.frame("one"=t(test_sd[x,1:180]),"two"=t(test_sd[y,1:180]))
    names(df) <- c(strsplit(compare," vs ")[[1]],"bin")
    df$diff <- df[,1] - df[,2]
    df$sd <- apply(df_sd,1,mean)
    df$Title <- "Difference"
    limits <- aes(ymax = diff + 1.96*sd, ymin=diff - 1.96*sd)
    
    ggplot(data=df,aes(bin,diff))+
      ggtitle(paste(as.character(genotype)," (Day ",day,")",sep="",collapse = ""))+
      facet_wrap(~Title)+
      geom_ribbon(limits,fill="gray80")+
      geom_line(aes(color=bin),size=2)+
      scale_color_gradientn(colors=hue_pal(l=65)(180))+
      scale_x_continuous(breaks=number_ticks(5),limits = c(0,120))+
      scale_y_continuous(limits = c(-8,8))+
      ylab(compare)+
      xlab("Hue Channel")+
      theme_light()+
      theme(legend.position='none')+
      theme(plot.title = element_text(hjust = 0.5),
            strip.background=element_rect(fill="gray50"),
            strip.text.x=element_text(size=14,color="white"),
            strip.text.y=element_text(size=14,color="white"))+
      theme(axis.text = element_text(size = 12),
            axis.title= element_text(size = 18))
  }else{
    if(color %in% c("red","blue","green")){
      my_col <- paste(toupper(substring(color,1,1)),substring(color,2),"s",sep="")
      my_grad <- scale_color_gradient(low = "black",high = as.character(color))
      my_line <- geom_line(aes(color=bin),size=2)
    }else if(color == "green-magenta"){
      my_grad <- scale_color_gradientn(colors=colorRamps::magenta2green(255))
      my_line <- geom_line(aes(color=bin),size=2)
    }else if(color == "blue-yellow"){
      my_grad <- scale_color_gradientn(colors=colorRamps::blue2yellow(255))
      my_line <- geom_line(aes(color=bin),size=2)
    }else{
      my_grad <- NULL
      my_line <- geom_line(color="black",size=1)
    }
    df <- data.frame("one"=t(test[x,]),"two"=t(test[y,]),"bin"=0:255)
    df_sd <- data.frame("one"=t(test_sd[x,]),"two"=t(test_sd[y,]))
    names(df) <- c(strsplit(compare," vs ")[[1]],"bin")
    df$diff <- df[,1] - df[,2]
    df$sd <- apply(df_sd,1,mean)
    df$Title <- "Difference"
    limits <- aes(ymax = diff + 1.96*sd, ymin=diff - 1.96*sd)
    
    ggplot(data=df,aes(bin,diff))+
      ggtitle(paste(as.character(genotype)," (Day ",day,")",sep="",collapse = ""))+
      facet_wrap(~Title)+
      geom_ribbon(limits,fill="gray80")+
      my_line+
      my_grad+
      scale_x_continuous(breaks=number_ticks(5))+
      ylab(compare)+
      xlab(paste(color,"channel"))+
      theme_light()+
      theme(legend.position='none')+
      theme(plot.title = element_text(hjust = 0.5),
            strip.background=element_rect(fill="gray50"),
            strip.text.x=element_text(size=14,color="white"),
            strip.text.y=element_text(size=14,color="white"))+
      theme(axis.text = element_text(size = 12),
            axis.title= element_text(size = 18))
  }  
}
histoDiff_drought <- function(channel,color,genotype,day,compare){
  sub <- channel[channel$DAP==day,]
  test <- hist_avg(sub,day)
  test_sd <- hist_sd(sub,day)
  if(compare == "100% vs Recovery"){
    x <- 1; y <- 3
  }else if(compare == "100% vs 70%"){
    x <- 2; y <- 3
  }else{
    x <- 1; y <- 2
  }
  
  if(color=="hue"){
    df <- data.frame("one"=t(test[x,1:180]),"two"=t(test[y,1:180]),"bin"=0:179*2)
    df_sd <- data.frame("one"=t(test_sd[x,1:180]),"two"=t(test_sd[y,1:180]))
    names(df) <- c(strsplit(compare," vs ")[[1]],"bin")
    df$diff <- df[,1] - df[,2]
    df$sd <- apply(df_sd,1,mean)
    df$Title <- "Difference"
    limits <- aes(ymax = diff + 1.96*sd, ymin=diff - 1.96*sd)
    
    ggplot(data=df,aes(bin,diff))+
      ggtitle(paste(as.character(genotype)," (Day ",day,")",sep="",collapse = ""))+
      facet_wrap(~Title)+
      geom_ribbon(limits,fill="gray80")+
      geom_line(aes(color=bin),size=2)+
      scale_color_gradientn(colors=hue_pal(l=65)(180))+
      scale_x_continuous(breaks=number_ticks(5),limits = c(0,120))+
      scale_y_continuous(limits = c(-8,8))+
      ylab(compare)+
      xlab("Hue Channel")+
      theme_light()+
      theme(legend.position='none')+
      theme(plot.title = element_text(hjust = 0.5),
            strip.background=element_rect(fill="gray50"),
            strip.text.x=element_text(size=14,color="white"),
            strip.text.y=element_text(size=14,color="white"))+
      theme(axis.text = element_text(size = 12),
            axis.title= element_text(size = 18))
  }else{
    if(color %in% c("red","blue","green")){
      my_col <- paste(toupper(substring(color,1,1)),substring(color,2),"s",sep="")
      my_grad <- scale_color_gradient(low = "black",high = as.character(color))
      my_line <- geom_line(aes(color=bin),size=2)
    }else if(color == "green-magenta"){
      my_grad <- scale_color_gradientn(colors=colorRamps::magenta2green(255))
      my_line <- geom_line(aes(color=bin),size=2)
    }else if(color == "blue-yellow"){
      my_grad <- scale_color_gradientn(colors=colorRamps::blue2yellow(255))
      my_line <- geom_line(aes(color=bin),size=2)
    }else{
      my_grad <- NULL
      my_line <- geom_line(color="black",size=1)
    }
    df <- data.frame("one"=t(test[x,]),"two"=t(test[y,]),"bin"=0:254)
    df_sd <- data.frame("one"=t(test_sd[x,]),"two"=t(test_sd[y,]))
    names(df) <- c(strsplit(compare," vs ")[[1]],"bin")
    df$diff <- df[,1] - df[,2]
    df$sd <- apply(df_sd,1,mean)
    df$Title <- "Difference"
    limits <- aes(ymax = diff + 1.96*sd, ymin=diff - 1.96*sd)
    
    ggplot(data=df,aes(bin,diff))+
      ggtitle(paste(as.character(genotype)," (Day ",day,")",sep="",collapse = ""))+
      facet_wrap(~Title)+
      geom_ribbon(limits,fill="gray80")+
      my_line+
      my_grad+
      scale_x_continuous(breaks=number_ticks(5))+
      ylab(compare)+
      xlab(paste(color,"channel"))+
      theme_light()+
      theme(legend.position='none')+
      theme(plot.title = element_text(hjust = 0.5),
            strip.background=element_rect(fill="gray50"),
            strip.text.x=element_text(size=14,color="white"),
            strip.text.y=element_text(size=14,color="white"))+
      theme(axis.text = element_text(size = 12),
            axis.title= element_text(size = 18))
  }  
}

ui <- dashboardPage(skin = "purple",
  dashboardHeader(
    title = "Bart Lab Sorghum Phenotyper Experiments",
    titleWidth = 450
  ),
  dashboardSidebar(
    disable = T
  ),
  dashboardBody(
    tags$style(HTML("
     .tabbable > .nav > li > a[data-value='Nitrogen'] {background-color: #676767;   color:white}
     .tabbable > .nav > li > a[data-value='Drought'] {background-color: #676767;   color:white}
     .tabbable > .nav > li[class=active]    > a {background-color: #444444; color:white}
     .multicol{
       -webkit-column-count: 3; /* Chrome, Safari, Opera */
       -moz-column-count: 3; /* Firefox */
       column-count: 3;
     }
    .twocol{
       -webkit-column-count: 2; /* Chrome, Safari, Opera */
       -moz-column-count: 2; /* Firefox */
       column-count: 2;
      }"
    )
    ),
    tabsetPanel(
      tabPanel("Nitrogen",
        fluidRow(
          h2("Shapes"),
          column(2,
            box(width=13,
              title = "Parameters",status = 'success',solidHeader = T,
              selectInput("n_Shape", 
                      label = "Choose a shape",
                      choices = shapes,
                      selected = "area"),
              selectInput("n_Transform", 
                      label = "Choose a transform",
                      choices = c("No Transform","Log10"),
                      selected = "No Transform"),
              sliderInput("n_xrange","Adjust DAP",min=min(as.numeric(nitrogen$DAP)),max=max(as.numeric(nitrogen$DAP)),value=c(min(as.numeric(nitrogen$DAP)),max(as.numeric(nitrogen$DAP))))
            )),
          column(7,
            box(
              width = 13,
              title = "Data Summaries", status = 'success', solidHeader = TRUE,
              tabBox(
                width = 13,
                tabPanel("Trends",
                  plotOutput("nitrogen_trends",height = 300)
                ),
                tabPanel("Boxplots",
                  plotOutput("nitrogen_box",height = 300)
                ),
                tabPanel("Growth Rates",
                  p("This takes some time to calculate (approximately 1min 10sec for full time range)"),
                  plotOutput("nitrogen_growth",height = 300)
                )
              )
            )
          ),
          column(3,
            fluidRow(
              box(
                width = 12,
                title = "Highlight Genotypes", status = 'success', solidHeader = TRUE,
                tags$div(class = "multicol",checkboxGroupInput("nitrogen_genos",label = "",choices=genotypes)),
                actionButton("n_select_all",label="Select All",inline=TRUE),
                actionButton("n_deselect_all",label="De-Select All",inline=TRUE)
              )),
            fluidRow(
              box(
                width = 12,
                title = "Download CSV", status = 'success', solidHeader = TRUE,
                downloadButton("n_save_shape_button",label="Save All Genotypes",inline=TRUE),
                br(),
                br(),
                uiOutput("n_save_shape")
              ))
              )
        ),
        fluidRow(
          h2("Color"),
          box(
            width = 2,
            title = "Parameters", status = "success", solidHeader = TRUE,
            selectInput("n_dap", 
                        label = "Days after planting",
                        choices = 8:26,
                        selected = 8),
            selectInput("n_channel", 
                        label = "Channels",
                        choices = c("Red","Green","Blue","Hue","Saturation","Value","Lightness","Green-Magenta","Blue-Yellow","NIR"),
                        selected = "Hue"),
            selectInput("n_genotype", 
                        label = "Genotype",
                        choices = unique(nitrogen$Line_name1)[order(unique(nitrogen$Line_name1))],
                        selected = "Atlas"),
            actionButton("n_get_data",label="Load Data",inline=TRUE),
            uiOutput("n_save_color")
          ),
          box(width = 7,
              title="Treatment Histograms", status = "success", solidHeader = TRUE,
              plotOutput("n_hist",height=300)
          ),
          box(width = 3,
              title="Difference", status = "success", solidHeader = TRUE,
              selectInput("n_compare", 
                          label = "Choose a comparison",
                          choices = c("100%/100% vs 50%/10%","100%/100% vs 10%/10%","50%/10% vs 10%/10%"),
                          selected = "100%/100% vs 10%/10%"),
              plotOutput("n_diff",height=300)
          )
        )
       ),
      tabPanel("Drought",
           fluidRow(
             h2("Shapes"),
             column(2,
             box(width=13,
                 title = "Parameters",status = "primary",solidHeader = T,
                 selectInput("d_Shape",
                             label = "Choose a shape",
                             choices = shapes,
                             selected = "area"),
                 selectInput("d_Transform",
                             label = "Choose a transform",
                             choices = c("No Transform","Log10"),
                             selected = "No Transform"),
                 sliderInput("d_xrange","Adjust DAP",min=min(as.numeric(drought$DAP)),max=max(as.numeric(drought$DAP)),value=c(min(as.numeric(drought$DAP)),max(as.numeric(drought$DAP))))
             )),
             column(7,
               box(
                   width = 13,
                   title = "Data Summaries", status = "primary", solidHeader = TRUE,
                   tabBox(
                     width = 13,
                     tabPanel("Trends",
                              plotOutput("drought_trends",height = 300)
                     ),
                     tabPanel("Boxplots",
                              plotOutput("drought_box",height = 300)
                     ),
                     tabPanel("Growth Rates",
                              p("This takes some time to calculate (approximately 1min 10sec for full time range)"),
                              plotOutput("drought_growth",height = 300)
                     )
                   )
               )),
             column(3,
             fluidRow(
               box(
                  width = 12,
                  title = "Highlight Genotypes", status = "primary", solidHeader = TRUE,
                  tags$div(class = "multicol",checkboxGroupInput("drought_genos",label = "",choices=genotypes)),
                  actionButton("d_select_all",label="Select All",inline=TRUE),
                  actionButton("d_deselect_all",label="De-Select All",inline=TRUE)
             )),
             fluidRow(
               box(
               width = 12,
               title = "Download CSV", status = 'primary', solidHeader = TRUE,
               downloadButton("d_save_shape_button",label="Save All Genotypes",inline=TRUE),
               br(),
               br(),
               uiOutput("d_save_shape")
             ))
             )
          ),
          fluidRow(
            h2("Color"),
            box(
              width = 2,
              title = "Parameters", status = "primary", solidHeader = TRUE,
              selectInput("d_dap",
                          label = "Days after planting",
                          choices = 8:26,
                          selected = 8),
              selectInput("d_channel",
                          label = "Channels",
                          choices = c("Red","Green","Blue","Hue","Saturation","Value","Lightness","Green-Magenta","Blue-Yellow","NIR"),
                          selected = "Hue"),
              selectInput("d_genotype",
                          label = "Genotype",
                          choices = unique(nitrogen$Line_name1)[order(unique(nitrogen$Line_name1))],
                          selected = "Atlas"),
              actionButton("d_get_data",label="Load Data",inline=TRUE),
              uiOutput("d_save_color")
            ),
            box(width = 7,
                title="Treatment Histograms", status = "primary", solidHeader = TRUE,
                plotOutput("d_hist",height=300)
            ),
            box(width = 3,
                title="Difference", status = "primary", solidHeader = TRUE,
                selectInput("d_compare",
                            label = "Choose a comparison",
                            choices = c("100% vs 70%","100% vs Recovery","70% vs Recovery"),
                            selected = "100% vs 70%"),
                plotOutput("d_diff",height=300)
            )
          ),
          br(),
          br(),
          hr()
      )
    )
  )
)


server <- function(input, output, session) {
  observeEvent(input$n_select_all,{
    updateCheckboxGroupInput(session, "nitrogen_genos",
                             choices = genotypes,
                             selected = genotypes
    )
  })
  
  observeEvent(input$n_deselect_all,{
    updateCheckboxGroupInput(session, "nitrogen_genos",
                             choices = genotypes,
                             selected = c()
    )
  })
  
  observeEvent(input$d_select_all,{
    updateCheckboxGroupInput(session, "drought_genos",
                             choices = genotypes,
                             selected = genotypes
    )
  })
  
  observeEvent(input$d_deselect_all,{
    updateCheckboxGroupInput(session, "drought_genos",
                             choices = genotypes,
                             selected = c()
    )
  })

  output$n_save_color <- renderUI({
    sub <- n_subset()
    downloadButton("n_save_color_button",label="Save Data (CSV)",inline=TRUE)
  })
  
  n_color_Out <- eventReactive(n_subset(),{
    sub <- n_subset()
    day <- unique(sub$DAP)
    genotype <- unique(sub$Line_name)
    color <- unique(sub$Color)
    return(paste("Nitrogen-",genotype,"-",color,"-Day",day,".csv",sep=""))
  })
  
  output$n_save_color_button <- downloadHandler(
    filename = function() {n_color_Out()},
    content=function(file){
      write.csv(n_subset(), file,row.names = F)
    }
  )
  
  output$d_save_color <- renderUI({
    sub <- d_subset()
    downloadButton("d_save_color_button",label="Save Data",inline=TRUE)
  })
  
  d_color_Out <- eventReactive(d_subset(),{
    sub <- d_subset()
    day <- unique(sub$DAP)
    genotype <- unique(sub$Line_name)
    color <- unique(sub$Color)
    return(paste("Drought-",genotype,"-",color,"-Day",day,".csv",sep=""))
  })
  
  output$d_save_color_button <- downloadHandler(
    filename = function() {d_color_Out()},
    content=function(file){
      write.csv(d_subset(), file,row.names = F)
    }
  )
  
  output$n_diff <- renderPlot({
    sub <- n_subset()
    day <- unique(sub$DAP)
    genotype <- unique(sub$Line_name)
    color <- unique(sub$Color)
    compare <- input$n_compare
    histoDiff_nitrogen(sub,genotype=genotype,color = color,day=day,compare=compare)
  })
  
  output$n_hist <- renderPlot({
    sub <- n_subset()
    day <- unique(sub$DAP)
    genotype <- unique(sub$Line_name)
    color <- unique(sub$Color)
    plot_histo_nitrogen(sub,genotype=genotype,color = color,day=day)
  })
  
  n_subset <- eventReactive(input$n_get_data,{
    filter(read.csv(paste("data/Nitrogen_Color_Files/",input$n_genotype,"_",tolower(input$n_channel),".csv",sep="")),DAP==input$n_dap)
  })
  
  output$d_diff <- renderPlot({
    sub <- d_subset()
    day <- unique(sub$DAP)
    genotype <- unique(sub$Line_name)
    color <- unique(sub$Color)
    compare <- input$d_compare
    histoDiff_drought(sub,genotype=genotype,color = color,day=day,compare=compare)
  })
  
  output$d_hist <- renderPlot({
    sub <- d_subset()
    day <- unique(sub$DAP)
    genotype <- unique(sub$Line_name)
    color <- unique(sub$Color)
    plot_histo_drought(sub,genotype=genotype,color = color,day=day)
  })
  
  d_subset <- eventReactive(input$d_get_data,{
    filter(read.csv(paste("data/Drought_Color_Files/",input$d_genotype,"_",tolower(input$d_channel),".csv",sep="")),DAP==input$d_dap)
  })
  
  
  n_params <- reactive({
    which_shape <- input$n_Shape
    which_trans <- input$n_Transform
    which_convert <- if(which_shape %in% c("area","Hull.area")){area_convert}else if(which_shape %in% c("perimeter","width", "height", "longest_axis", "Center.of.mass.x", "Center.of.mass.y","ellipse_center_x", "ellipse_center_y", "ellipse_major_axis", "ellipse_minor_axis")){line_convert}else{1}
    which_trans_lab <- if(which_trans == "Log10"){"Log10"}else{""}
    which_genos <- input$nitrogen_genos
    which_xrange <- input$n_xrange
    list(which_shape,which_trans,which_convert,which_trans_lab,which_genos,which_xrange)
  })
  
  output$n_save_shape <- renderUI({
    params <- n_params()
    which_genos <- params[[5]]
    if(!is.null(which_genos)){
      downloadButton("n_save_sub_shape_button",label="Save Selected Genotype(s)")
    }
  })
  n_shape_filename_Out <- eventReactive(n_params(),{
    params <- n_params()
    which_shape <- params[[1]]
    which_trans <- params[[2]]
    which_convert <- params[[3]]
    which_trans_lab <- params[[4]]
    which_genos <- params[[5]]
    which_xrange <- params[[6]]
    return(paste("Nitrogen-",which_shape,"-","Days_",which_xrange[1],"-",which_xrange[2],".csv",sep=""))
  })
  n_shape_sub_filename_Out <- eventReactive(n_params(),{
    params <- n_params()
    which_shape <- params[[1]]
    which_trans <- params[[2]]
    which_convert <- params[[3]]
    which_trans_lab <- params[[4]]
    which_genos <- params[[5]]
    which_xrange <- params[[6]]
    return(paste("Nitrogen_",which_shape,"_","Days_",which_xrange[1],"-",which_xrange[2],"_Genotypes_",paste(which_genos,collapse="-"),".csv",sep=""))
  })

  n_shape_data_Out <- eventReactive(n_params(),{
    params <- n_params()
    which_shape <- params[[1]]
    which_trans <- params[[2]]
    which_convert <- params[[3]]
    which_trans_lab <- params[[4]]
    which_genos <- params[[5]]
    which_xrange <- params[[6]]
    out <- nitrogen[,colnames(nitrogen) %in% c(which_shape,"Time","Camera","Type","Line_name1","Treatment","DAP","Trait","Taxa","Line_name1","Photoperiod","Origin","Race")]
    return(dplyr::filter(out,DAP %in% seq(which_xrange[1],which_xrange[2],by=1)))
  })
  n_shape_sub_data_Out <- eventReactive(n_params(),{
    params <- n_params()
    which_shape <- params[[1]]
    which_trans <- params[[2]]
    which_convert <- params[[3]]
    which_trans_lab <- params[[4]]
    which_genos <- params[[5]]
    which_xrange <- params[[6]]
    out <- nitrogen[,colnames(nitrogen) %in% c(which_shape,"Time","Camera","Type","Line_name1","Treatment","DAP","Trait","Taxa","Photoperiod","Origin","Race")]
    out[,1] <- out[,1]*which_convert
    return(dplyr::filter(out,DAP %in% seq(which_xrange[1],which_xrange[2],by=1),Line_name1 %in% which_genos))
  })
  
  output$n_save_shape_button <- downloadHandler(
    filename = function() {n_shape_filename_Out()},
    content=function(file){
      write.csv(n_shape_data_Out(), file,row.names = F,quote = F)
    }
  )
  output$n_save_sub_shape_button <- downloadHandler(
    filename = function() {n_shape_sub_filename_Out()},
    content=function(file){
      write.csv(n_shape_sub_data_Out(), file,row.names = F,quote = F)
    }
  )
  
  output$nitrogen_trends <- renderPlot({
    params <- n_params()
    which_shape <- params[[1]]
    which_trans <- params[[2]]
    which_convert <- params[[3]]
    which_trans_lab <- params[[4]]
    which_genos <- params[[5]]
    which_xrange <- params[[6]]

    nitrogen$Status <- nitrogen$Line_name1 %in% which_genos
    n_plot <- ggplot(nitrogen,aes_string(x="DAP",y=paste(which_convert,"*",which_shape,sep = "")))+
      facet_grid(~Treatment)+
      ggtitle("Genotype LOESS Curves (Adjust DAP interval to change)")+
      xlab("Plant Age (days)")+
      ylab(paste(which_trans_lab," ",toupper(substr(which_shape, 1, 1)), substr(which_shape, 2, nchar(which_shape)), sep=""))+
      scale_x_continuous(breaks=number_ticks(5),limits = which_xrange)+
      geom_smooth(data=nitrogen[!nitrogen$Line_name1 %in% which_genos,],aes(group=Line_name1),color="gray",method = "loess")+
      geom_smooth(data=nitrogen[nitrogen$Line_name1 %in% which_genos,],aes(color=Line_name1),method = "loess")+
      theme_light()+
      theme(axis.text = element_text(size = 14),
            axis.title= element_text(size = 18))+
      theme(strip.background=element_rect(fill="gray50"),
            strip.text.x=element_text(size=14,color="white"),
            strip.text.y=element_text(size=14,color="white"))+
      guides(color = guide_legend(title = "Genotypes"))
    
    if(which_trans == "Log10"){
      n_plot+scale_y_log10()
    }else{
      n_plot+scale_y_continuous()
    }
  })
  
  output$nitrogen_box <- renderPlot({
    params <- n_params()
    which_shape <- params[[1]]
    which_trans <- params[[2]]
    which_convert <- params[[3]]
    which_trans_lab <- params[[4]]
    which_genos <- params[[5]]
    which_xrange <- params[[6]][2]
    sub <- nitrogen[nitrogen$DAP == which_xrange,]
    
    n_plot <- ggplot(sub,aes_string(x="Line_name1",y=paste(which_convert,"*",which_shape,sep = "")))+
      facet_grid(~Treatment)+
      ggtitle("Genotype Boxplots (Adjust DAP interval to change)")+
      ylab(paste(which_trans_lab," ",toupper(substr(which_shape, 1, 1)), substr(which_shape, 2, nchar(which_shape))," (Day ",which_xrange,")", sep=""))+
      geom_boxplot(data=sub[!sub$Line_name1 %in% which_genos,],aes(group=Line_name1),fill="gray80",color="gray50")+
      geom_boxplot(data=sub[sub$Line_name1 %in% which_genos,],aes(fill=Line_name1,group=Line_name1))+
      theme_light()+
      theme(axis.text = element_text(size = 14),
            axis.title= element_text(size = 18),
            axis.title.x = element_blank())+
      theme(strip.background=element_rect(fill="gray50"),
            strip.text.x=element_text(size=14,color="white"),
            strip.text.y=element_text(size=14,color="white"))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5,size=10))+
      guides(fill = guide_legend(title = "Genotypes"))
    

    if(which_trans == "Log10"){
      n_plot+scale_y_log10()
    }else{
      n_plot+scale_y_continuous()
    }
  })
  
  n_slope_data <- reactive({
    which_shape <- input$n_Shape
    which_trans <- input$n_Transform
    which_convert <- if(which_shape %in% c("area","Hull.area")){area_convert}else if(which_shape %in% c("perimeter","width", "height", "longest_axis", "Center.of.mass.x", "Center.of.mass.y","ellipse_center_x", "ellipse_center_y", "ellipse_major_axis", "ellipse_minor_axis")){line_convert}else{1}
    which_trans_lab <- if(which_trans == "Log10"){"Log10"}else{""}
    which_xrange <- input$n_xrange
    sub <-nitrogen[nitrogen$DAP >= which_xrange[1] & nitrogen$DAP <= which_xrange[2],]
    if(which_trans=="Log10"){
      slope_mod <- data.frame(summary(lmer(data=sub,log10(eval(parse(text=which_shape))*which_convert+1)~0+Line_name1:Treatment+Line_name1:as.numeric(DAP-which_xrange[1]):Treatment+(as.numeric(DAP-which_xrange[1])|Barcodes)))$coefficients)[91:180,]      
    }else{
      slope_mod <- data.frame(summary(lmer(data=sub,eval(parse(text=which_shape))*which_convert~0+Line_name1:Treatment+Line_name1:as.numeric(DAP-which_xrange[1]):Treatment+(as.numeric(DAP-which_xrange[1])|Barcodes)))$coefficients)[91:180,] 
    }
    slope_mod$Line_name <- sub("Line_name1","",unlist(lapply(strsplit(rownames(slope_mod),":"),function(i)i[1])))
    slope_mod$Treatment <- sub("Treatment","",unlist(lapply(strsplit(rownames(slope_mod),":"),function(i)i[2])))
    slope_mod$Treatment <- ordered(slope_mod$Treatment, levels=c("100% / 100%","50% / 10%","10% / 10%"))
    slope_mod
  })
  
  
  output$nitrogen_growth <- renderPlot({
    params <- n_params()
    sub <- n_slope_data()
    which_shape <- params[[1]]
    which_trans <- params[[2]]
    which_convert <- params[[3]]
    which_trans_lab <- params[[4]]
    which_genos <- params[[5]]
    which_xrange <- params[[6]]
    
    limits <- aes(ymax = Estimate + 1.96*Std..Error, ymin=Estimate - 1.96*Std..Error)
    
    p <- ggplot(data=sub,aes(Line_name,Estimate))+
      facet_grid(~Treatment)+
      ggtitle(paste(which_trans_lab," ",toupper(substr(which_shape, 1, 1)), substr(which_shape, 2, nchar(which_shape))," rate of change (Days ",which_xrange[1]," to ",which_xrange[2],")",sep = ""))+
      geom_errorbar(limits,width=0.5)+
      geom_point(data=sub[!sub$Line_name %in% which_genos,],color="gray50",size=3)+
      geom_point(data=sub[sub$Line_name %in% which_genos,],aes(color=Line_name),size=3)+
      ylab(paste(which_trans_lab," ",toupper(substr(which_shape, 1, 1)), substr(which_shape, 2, nchar(which_shape))," / Day", sep=""))+
      xlab("")+
      #ggtitle("Comparing 10% Slopes to Average DAP(21-26)")+
      scale_y_continuous(breaks = number_ticks(4))+
      theme_light()+
      theme(axis.text = element_text(size = 14),
            axis.title= element_text(size = 18),
            axis.title.x = element_blank())+
      theme(strip.background=element_rect(fill="gray50"),
            strip.text.x=element_text(size=14,color="white"),
            strip.text.y=element_text(size=14,color="white"))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5,size=10))+
      guides(color = guide_legend(title = "Genotypes"))
    
    
    if(which_trans == "Log10"){
      p+scale_y_log10()
    }else{
      p+scale_y_continuous()
    }
  })
  
  
  
  d_params <- reactive({
    which_shape <- input$d_Shape
    which_trans <- input$d_Transform
    which_convert <- if(which_shape %in% c("area","Hull.area")){area_convert}else if(which_shape %in% c("perimeter","width", "height", "longest_axis", "Center.of.mass.x", "Center.of.mass.y","ellipse_center_x", "ellipse_center_y", "ellipse_major_axis", "ellipse_minor_axis")){line_convert}else{1}
    which_trans_lab <- if(which_trans == "Log10"){"Log10"}else{""}
    which_genos <- input$drought_genos
    which_xrange <- input$d_xrange
    list(which_shape,which_trans,which_convert,which_trans_lab,which_genos,which_xrange)
  })
  
  output$d_save_shape <- renderUI({
    params <- d_params()
    which_genos <- params[[5]]
    if(!is.null(which_genos)){
      downloadButton("d_save_sub_shape_button",label="Save Selected Genotype(s)")
    }
  })
  d_shape_filename_Out <- eventReactive(d_params(),{
    params <- d_params()
    which_shape <- params[[1]]
    which_trans <- params[[2]]
    which_convert <- params[[3]]
    which_trans_lab <- params[[4]]
    which_genos <- params[[5]]
    which_xrange <- params[[6]]
    return(paste("Drought-",which_shape,"-","Days_",which_xrange[1],"-",which_xrange[2],".csv",sep=""))
  })
  d_shape_sub_filename_Out <- eventReactive(d_params(),{
    params <- d_params()
    which_shape <- params[[1]]
    which_trans <- params[[2]]
    which_convert <- params[[3]]
    which_trans_lab <- params[[4]]
    which_genos <- params[[5]]
    which_xrange <- params[[6]]
    return(paste("Drought_",which_shape,"_","Days_",which_xrange[1],"-",which_xrange[2],"_Genotypes_",paste(which_genos,collapse="-"),".csv",sep=""))
  })
  
  d_shape_data_Out <- eventReactive(d_params(),{
    params <- d_params()
    which_shape <- params[[1]]
    which_trans <- params[[2]]
    which_convert <- params[[3]]
    which_trans_lab <- params[[4]]
    which_genos <- params[[5]]
    which_xrange <- params[[6]]
    out <- drought[,colnames(drought) %in% c(which_shape,"Time","Camera","Type","Line_name1","Treatment","DAP","Trait","Taxa","Line_name1","Photoperiod","Origin","Race")]
    return(dplyr::filter(out,DAP %in% seq(which_xrange[1],which_xrange[2],by=1)))
  })
  d_shape_sub_data_Out <- eventReactive(d_params(),{
    params <- d_params()
    which_shape <- params[[1]]
    which_trans <- params[[2]]
    which_convert <- params[[3]]
    which_trans_lab <- params[[4]]
    which_genos <- params[[5]]
    which_xrange <- params[[6]]
    out <- drought[,colnames(drought) %in% c(which_shape,"Time","Camera","Type","Geno","Treatment","DAP","Trait","Taxa","Photoperiod","Origin","Race")]
    out[,1] <- out[,1]*which_convert
    return(dplyr::filter(out,DAP %in% seq(which_xrange[1],which_xrange[2],by=1),Geno %in% which_genos))
  })
  
  output$d_save_shape_button <- downloadHandler(
    filename = function() {d_shape_filename_Out()},
    content=function(file){
      write.csv(d_shape_data_Out(), file,row.names = F,quote = F)
    }
  )
  output$d_save_sub_shape_button <- downloadHandler(
    filename = function() {d_shape_sub_filename_Out()},
    content=function(file){
      write.csv(d_shape_sub_data_Out(), file,row.names = F,quote = F)
    }
  )
  
  
  
  output$drought_trends <- renderPlot({
    params <- d_params()
    which_shape <- params[[1]]
    which_trans <- params[[2]]
    which_convert <- params[[3]]
    which_trans_lab <- params[[4]]
    which_genos <- params[[5]]
    which_xrange <- params[[6]]

    drought$Status <- drought$Geno %in% which_genos
    d_plot <- ggplot(drought,aes_string(x="DAP",y=paste(which_convert,"*",which_shape,sep = "")))+
      facet_grid(~Treatment)+
      xlab("Plant Age (days)")+
      ggtitle("Genotype LOESS Curves (Adjust DAP interval to change)")+
      ylab(paste(which_trans_lab," ",toupper(substr(which_shape, 1, 1)), substr(which_shape, 2, nchar(which_shape)), sep=""))+
      scale_x_continuous(breaks=number_ticks(5),limits = which_xrange)+
      geom_smooth(data=drought[!drought$Geno %in% which_genos,],aes(group=Geno),color="gray",method = "loess")+
      geom_smooth(data=drought[drought$Geno %in% which_genos,],aes(color=Geno),method = "loess")+
      theme_light()+
      theme(axis.text = element_text(size = 14),
            axis.title= element_text(size = 18))+
      theme(strip.background=element_rect(fill="gray50"),
            strip.text.x=element_text(size=14,color="white"),
            strip.text.y=element_text(size=14,color="white"))+
      guides(color = guide_legend(title = "Genotypes"))
    if(which_trans == "Log10"){
      d_plot+scale_y_log10()
    }else{
      d_plot+scale_y_continuous()
    }
    })
  
  output$drought_box <- renderPlot({
    params <- d_params()
    which_shape <- params[[1]]
    which_trans <- params[[2]]
    which_convert <- params[[3]]
    which_trans_lab <- params[[4]]
    which_genos <- params[[5]]
    which_xrange <- params[[6]][2]
    sub <- drought[drought$DAP == which_xrange,]
    d_plot <- ggplot(sub,aes_string(x="Geno",y=paste(which_convert,"*",which_shape,sep = "")))+
      facet_grid(~Treatment)+
      ggtitle("Genotype Boxplots (Adjust DAP interval to change)")+
      ylab(paste(which_trans_lab," ",toupper(substr(which_shape, 1, 1)), substr(which_shape, 2, nchar(which_shape))," (Day ",which_xrange,")", sep=""))+
      geom_boxplot(data=sub[!sub$Geno %in% which_genos,],aes(group=Geno),fill="gray80",color="gray50")+
      geom_boxplot(data=sub[sub$Geno %in% which_genos,],aes(fill=Geno,group=Geno))+
      theme_light()+
      theme(axis.text = element_text(size = 14),
            axis.title= element_text(size = 18),
            axis.title.x = element_blank())+
      theme(strip.background=element_rect(fill="gray50"),
            strip.text.x=element_text(size=14,color="white"),
            strip.text.y=element_text(size=14,color="white"))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5,size=10))+
      guides(fill = guide_legend(title = "Genotypes"))
    
    if(which_trans == "Log10"){
      d_plot+scale_y_log10()
    }else{
      d_plot+scale_y_continuous()
    }
  })
  
  d_slope_data <- reactive({
    which_shape <- input$d_Shape
    which_trans <- input$d_Transform
    which_convert <- if(which_shape %in% c("area","Hull.area")){area_convert}else if(which_shape %in% c("perimeter","width", "height", "longest_axis", "Center.of.mass.x", "Center.of.mass.y","ellipse_center_x", "ellipse_center_y", "ellipse_major_axis", "ellipse_minor_axis")){line_convert}else{1}
    which_trans_lab <- if(which_trans == "Log10"){"Log10"}else{""}
    which_xrange <- input$d_xrange
    sub <-drought[drought$DAP >= which_xrange[1] & drought$DAP <= which_xrange[2],]
    if(which_trans=="Log10"){
      slope_mod <- data.frame(summary(lmer(data=sub,log10(eval(parse(text=which_shape))*which_convert+1)~0+Geno:Treatment+Geno:as.numeric(DAP-which_xrange[1]):Treatment+(as.numeric(DAP-which_xrange[1])|Barcodes)))$coefficients)[91:180,]      
    }else{
      slope_mod <- data.frame(summary(lmer(data=sub,eval(parse(text=which_shape))*which_convert~0+Geno:Treatment+Geno:as.numeric(DAP-which_xrange[1]):Treatment+(as.numeric(DAP-which_xrange[1])|Barcodes)))$coefficients)[91:180,] 
    }
    slope_mod$Line_name <- sub("Geno","",unlist(lapply(strsplit(rownames(slope_mod),":"),function(i)i[1])))
    slope_mod$Treatment <- sub("Treatment","",unlist(lapply(strsplit(rownames(slope_mod),":"),function(i)i[2])))
    slope_mod$Treatment <- ordered(slope_mod$Treatment, levels=c("100%","70%","Recovery"))
    slope_mod
  })
  output$drought_growth <- renderPlot({
    params <- d_params()
    sub <- d_slope_data()
    which_shape <- params[[1]]
    which_trans <- params[[2]]
    which_convert <- params[[3]]
    which_trans_lab <- params[[4]]
    which_genos <- params[[5]]
    which_xrange <- params[[6]]
    
    limits <- aes(ymax = Estimate + 1.96*Std..Error, ymin=Estimate - 1.96*Std..Error)
    
    p <- ggplot(data=sub,aes(Line_name,Estimate))+
      facet_grid(~Treatment)+
      ggtitle(paste(which_trans_lab," ",toupper(substr(which_shape, 1, 1)), substr(which_shape, 2, nchar(which_shape))," rate of change (Days ",which_xrange[1]," to ",which_xrange[2],")",sep = ""))+
      geom_errorbar(limits,width=0.5)+
      geom_point(data=sub[!sub$Line_name %in% which_genos,],color="gray50",size=3)+
      geom_point(data=sub[sub$Line_name %in% which_genos,],aes(color=Line_name),size=3)+
      ylab(paste(which_trans_lab," ",toupper(substr(which_shape, 1, 1)), substr(which_shape, 2, nchar(which_shape))," / Day", sep=""))+
      xlab("")+
      #ggtitle("Comparing 10% Slopes to Average DAP(21-26)")+
      scale_y_continuous(breaks = number_ticks(4))+
      theme_light()+
      theme(axis.text = element_text(size = 14),
            axis.title= element_text(size = 18),
            axis.title.x = element_blank())+
      theme(strip.background=element_rect(fill="gray50"),
            strip.text.x=element_text(size=14,color="white"),
            strip.text.y=element_text(size=14,color="white"))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5,size=10))+
      guides(color = guide_legend(title = "Genotypes"))
    
    if(which_trans == "Log10"){
      p+scale_y_log10()
    }else{
      p+scale_y_continuous()
    }
  })
  
}

shinyApp(ui, server)
