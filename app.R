library(rjson)
#library(jsonlite)
library(data.table)
library(dplyr)

'%!in%' <- function(x,y)!('%in%'(x,y))

competitions <- fromJSON(file="C:/Users/mmeye/Downloads/nogomet/data/competitions.json")

competitions.df <- data.frame(do.call(rbind,competitions),stringsAsFactors = FALSE)


match.files <- list.files(path="C:/Users/mmeye/Downloads/nogomet/data/matches",
                          full.names = TRUE,recursive = TRUE)

matches.list <- list()
for(i in 1:length(match.files)){
    match.temp <- fromJSON(file=match.files[i]) 
    
    matches <- lapply(match.temp, function(x) data.frame(t(unlist(x)),stringsAsFactors = FALSE))
    matches.df <- rbindlist(matches,fill=TRUE) 
    matches.list[[i]] <- matches.df
    
}

all.matches.df <- data.frame(rbindlist(matches.list,fill=TRUE))

columns.to.keep <- names(which(unlist(lapply(all.matches.df,function(x) length(which(is.na(x)))))==0))

all.matches.clean <- all.matches.df[,columns.to.keep] 
all.matches.clean$match_week <- as.numeric(all.matches.clean$match_week)
all.matches.clean$home_score <- as.numeric(all.matches.clean$home_score)
all.matches.clean$away_score <- as.numeric(all.matches.clean$away_score)


event.files <- list.files(path="C:/Users/mmeye/Downloads/nogomet/data/events",
                          full.names = TRUE,recursive = TRUE)

event.list <- list()
for(i in 1:length(event.files)){
    event.temp <- fromJSON(file=event.files[i])
    
    
    teamids <- c() 
    
    starting.x11.index <- which(unlist(lapply(event.temp,function(x) x$type$name))=="Starting XI")
    starting.x11.list <- list()
    for(s in 1:2){
     
        jersey.number.index <- which(names(unlist(event.temp[[s]]$tactics$lineup))=="jersey_number")
        starting.x11 <- t(unlist(event.temp[[s]]$tactics$lineup)[-which(names(unlist(event.temp[[s]]$tactics$lineup))=="jersey_number")])
        starting.x11.team1 <- data.frame(matrix(starting.x11,ncol=4,byrow = TRUE),stringsAsFactors = FALSE)
        colnames(starting.x11.team1) <- names(unlist(event.temp[[s]]$tactics$lineup)[-which(names(unlist(event.temp[[s]]$tactics$lineup))=="jersey_number")])[1:4]
        starting.x11.team1$formation <- event.temp[[s]]$tactics$formation
        starting.x11.team1$team_id <- event.temp[[s]]$team$id
        
        teamids <- c(teamids,event.temp[[s]]$team$id)
        
        starting.x11.team1$team_name <- event.temp[[s]]$team$name
        starting.x11.list[[s]] <- starting.x11.team1
    }
    
    pass.index <- which(unlist(lapply(event.temp,function(x) x$type$name))=="Pass")
    

    pass.team1 <- pass.index[which(unlist(lapply(pass.index,function(x) event.temp[[x]]$team$id))==teamids[1])]
    
    pass.team1.df <- data.frame(matrix(NA,nrow=1,ncol=12))
    colnames(pass.team1.df) <- c("passid","Possession","Passer","X.Pass","Y.Pass",
                                 "Pass.Type","Receiver","X.Receive","Y.Receive",
                                 "Pass.Length","Pass.Angle","Body.Part")
    
    for(p in 1:length(pass.team1)){
        pass.temp <- event.temp[[pass.team1[p]]]
        pass.id <- pass.temp$id
        possession <- pass.temp$possession
        passer <- pass.temp$player$name
        pass.location <- pass.temp$location
        pass.type <- pass.temp$pass$height$name
        receiver <- ifelse("recipient" %!in% names(pass.temp$pass),NA,pass.temp$pass$recipient$name)
        receive.location <- pass.temp$pass$end_location
        pass.length <- pass.temp$pass$length
        pass.angle <- pass.temp$pass$angle
        body.part <- pass.temp$pass$body_part$name
        
        row.toadd <- c(pass.id,possession,passer,pass.location,pass.type,receiver,receive.location,pass.length,pass.angle,body.part)
        pass.team1.df <- rbind(pass.team1.df,row.toadd)
    }
    pass.team1.df <- pass.team1.df[-1,]
    pass.team1.df[,c(2,4,5,8:11)] <- lapply(pass.team1.df[,c(2,4,5,8:11)],as.numeric)
    
    pass.team1.df <- pass.team1.df %>% group_by(Possession) %>% mutate(seq = row_number())
    pass.team1.df$team_id <- teamids[1]
    
    pass.team2 <- pass.index[which(unlist(lapply(pass.index,function(x) event.temp[[x]]$team$id))==teamids[2])]
    pass.team2.df <- data.frame(matrix(NA,nrow=1,ncol=12))
    colnames(pass.team2.df) <- c("passid","Possession","Passer","X.Pass","Y.Pass",
                                 "Pass.Type","Receiver","X.Receive","Y.Receive",
                                 "Pass.Length","Pass.Angle","Body.Part")
    
    for(p in 1:length(pass.team2)){
        pass.temp <- event.temp[[pass.team2[p]]]
        pass.id <- pass.temp$id
        possession <- pass.temp$possession
        passer <- pass.temp$player$name
        pass.location <- pass.temp$location
        pass.type <- pass.temp$pass$height$name
        receiver <- ifelse("recipient" %!in% names(pass.temp$pass),NA,pass.temp$pass$recipient$name)
        receive.location <- pass.temp$pass$end_location
        pass.length <- pass.temp$pass$length
        pass.angle <- pass.temp$pass$angle
        body.part <- pass.temp$pass$body_part$name
        
        row.toadd <- c(pass.id, possession,passer,pass.location,pass.type,receiver,receive.location,pass.length,pass.angle,body.part)
        pass.team2.df <- rbind(pass.team2.df,row.toadd)
    }
    pass.team2.df <- pass.team2.df[-1,]
    pass.team2.df[,c(2,4,5,8:11)] <- lapply(pass.team2.df[,c(2,4,5,8:11)],as.numeric)
    pass.team2.df <- pass.team2.df %>% group_by(Possession) %>% mutate(seq = row_number())
    pass.team2.df$team_id <- teamids[2]
    
    pass.list <- list(pass.team1.df,pass.team2.df)
    
    shot.index <- which(unlist(lapply(event.temp,function(x) x$type$name))=="Shot")
    
    shots.team1 <- shot.index[which(unlist(lapply(shot.index,function(x) event.temp[[x]]$team$id))==teamids[1])]
    shots.team1.df <- data.frame(matrix(NA,nrow=1,ncol=11))
    colnames(shots.team1.df) <- c("Possession","Player","X.Shot","Y.Shot",
                                  "Shot.Type","xG","keypassid","X.KeyPass","Y.KeyPass","Shot.Outcome","Shot.Foot")
    
    if(length(shots.team1)!=0){
        for(p in 1:length(shots.team1)){
            shots.temp <- event.temp[[shots.team1[p]]]
            possession <- shots.temp$possession
            shooter <- shots.temp$player$name
            shots.location <- shots.temp$location
            shots.type <- shots.temp$shot$technique$name
            shots.xg <- ifelse("statsbomb_xg" %!in% names(shots.temp$shot),NA, shots.temp$shot$statsbomb_xg)
            keypass <- ifelse("key_pass_id" %!in% names(shots.temp$shot),NA,shots.temp$shot$key_pass_id)
            keypass.location <- if(!is.na(keypass)){
                as.vector(unlist(pass.team1.df[which(pass.team1.df$passid==keypass),c("X.Pass","Y.Pass")]))
            }else{
                c(NA,NA)
            }
            shots.outcome <- shots.temp$shot$outcome$name
            body.part <- shots.temp$shot$body_part$name
            
            row.toadd <- c(possession,shooter,shots.location,shots.type,shots.xg,keypass,keypass.location[1],keypass.location[2],shots.outcome,body.part)
            shots.team1.df <- rbind(shots.team1.df,row.toadd)
        }
        shots.team1.df <- shots.team1.df[-1,]
        shots.team1.df[,c(1,3,4,6,8,9)] <- lapply(shots.team1.df[,c(1,3,4,6,8,9)],as.numeric)
        shots.team1.df$team_id <- teamids[1]
    }
    
    shots.team2 <- shot.index[which(unlist(lapply(shot.index,function(x) event.temp[[x]]$team$id))==teamids[2])]
    shots.team2.df <- data.frame(matrix(NA,nrow=1,ncol=11))
    colnames(shots.team2.df) <- c("Possession","Player","X.Shot","Y.Shot",
                                  "Shot.Type","xG","keypassid","X.KeyPass","Y.KeyPass","Shot.Outcome","Shot.Foot")
    if(length(shots.team2)!=0){
        for(p in 1:length(shots.team2)){
            shots.temp <- event.temp[[shots.team2[p]]]
            possession <- shots.temp$possession
            shooter <- shots.temp$player$name
            shots.location <- shots.temp$location
            shots.type <- shots.temp$shot$technique$name
            shots.xg <- ifelse("statsbomb_xg" %!in% names(shots.temp$shot),NA, shots.temp$shot$statsbomb_xg)
            keypass <- ifelse("key_pass_id" %!in% names(shots.temp$shot),NA,shots.temp$shot$key_pass_id)
            keypass.location <- if(!is.na(keypass)){
                as.vector(unlist(pass.team2.df[which(pass.team2.df$passid==keypass),c("X.Pass","Y.Pass")]))
            }else{
                c(NA,NA)
            }
            shots.outcome <- shots.temp$shot$outcome$name
            body.part <- shots.temp$shot$body_part$name
            
            row.toadd <- c(possession,shooter,shots.location,shots.type,shots.xg,keypass,keypass.location[1],keypass.location[2],shots.outcome,body.part)
            shots.team2.df <- rbind(shots.team2.df,row.toadd)
        }
        shots.team2.df <- shots.team2.df[-1,]
        shots.team2.df[,c(1,3,4,6,8,9)] <- lapply(shots.team2.df[,c(1,3,4,6,8,9)],as.numeric)
        shots.team2.df$team_id <- teamids[2]
    }
    
    shot.list <- list(shots.team1.df,shots.team2.df)
    
    match.id <- strsplit(basename(event.files[i]),"[.]")[[1]][1]
    
    event.list[[match.id]] <- list(starting.x11.list,pass.list,shot.list)
    
}


shots <- lapply(event.list,function(x) x[[3]])
shots.df <- plyr::ldply(lapply(shots,function(x) rbindlist(x,fill=TRUE)),.id="match_id")
shots.df <- shots.df %>% group_by(match_id,team_id) %>% mutate(xGoal_Total = sum(xG))

shots.shiny <- merge(shots.df,all.matches.df[,c("match_id","competition.competition_name","season.season_name",
                                                "home_score","away_score","home_team.home_team_id","away_team.away_team_id",
                                                "home_team.home_team_name","away_team.away_team_name")],by="match_id") 

shots.shiny$TeamScore <- ifelse(shots.shiny$team_id==shots.shiny$home_team.home_team_id,
                                as.numeric(shots.shiny$home_score),
                                as.numeric(shots.shiny$away_score))

shots.shiny$xG_Diff <- shots.shiny$TeamScore - shots.shiny$xGoal_Total

teams.df <- unique(all.matches.df[,c("home_team.home_team_id","home_team.home_team_name")])

shots.shiny.final <- merge(shots.shiny,teams.df,by.x="team_id",by.y="home_team.home_team_id")
colnames(shots.shiny.final)[25] <- "team_name"

shots.shiny.final$isAssisted <- ifelse(is.na(shots.shiny.final$keypassid),FALSE,TRUE)

shots.shiny.final2 <- shots.shiny.final[,c("Player", "xG")]


campo.x <- seq(from=0,to=120,by=1)
campo.y <- seq(from=0,to=80,by=1)
campo.coordinates <- expand.grid(campo.x,campo.y)
colnames(campo.coordinates) <- c("X","Y")


setwd("C:/Users/mmeye/Downloads/nogomet")
shots.shiny.final<-data.frame(shots.shiny.final)
shots.shiny.final2<- data.frame(shots.shiny.final2)
write.csv2(shots.shiny.final, file = "shots.shiny.final.csv")
write.csv2(shots.shiny.final2, file = "shots.shiny.final2.csv")


source("C:/Users/mmeye/Downloads/nogomet/soccerfield.R") 
#install.packages("shiny")
library(shiny)
library(DT) 
library(mgcv)
library(bslib)
setwd("C:/Users/mmeye/Downloads/nogomet/pictures")

ui <- fluidPage(
    navbarPage(title = "Nogomet",
               theme = bs_theme(bootswatch = "lux"),
                    tabPanel(title = "Home",
                             imageOutput("home_img"
                             )),
               tabPanel(title = "Functioning",
                        tabPanel(title = "Home",
                                 br(),
                                 #hr(),
                                 h4(strong("How the application works?")),
                                 p(style="text-align: justify; font-size = 25px",
                                   "The ", 
                                   em("Nogomet"), 
                                   "application allows to visualize all the shots made according to a selection of filters.
                                   You can choose the desired comptition, the season, the desired team(s) and
                                   the type of shots."),
                                 p(style="text-align: justify; font-size=25px",
                                   "All the shots will be represented on the soccer field on the right 
                                   of the filtering area. The size variation depends directly on the expected goals (also knows as xG).
                                   The xG represent the probability of scoring the goal. You can choose the xG score you want to see 
                                   on the soccer field with the xG slicer. To illustrate how an xG works let's take two simple examples:"),
                                 tags$blockquote("The attacker scores with a 25 meter shot in the middle of a forest of opposing defenders.
                                                 The xG will be very low (close to 0.022 so a probability of scoring of 2.2%) because it is 
                                                 a long shot, with defenders in front of him and a goalkeeper on his line."),
                                 tags$blockquote("The attacker scores while he is in the penalty area with an empty goal. Then the xG will be very high 
                                                 (close to 0.96 so a probability of scoring of 96%) because the goal is simple, 
                                                 close to him and without opponents in front of him."),
                                 br(),
                                 p(style="text-align: justify; font-size=25px",
                                   "The", em("Nogomet"), " application also allows you to directly save the table of scorers (provided according to the selection of filters) in .csv format"),
                                 hr(),
                                 p(style="text-align: justify; font-size=10px",
                                   "The data comes from Data Soccer")
                        )),
               tabPanel(title = "Interactive Shot Map",
                        sidebarLayout(
                            sidebarPanel(width = 3,
                                #column(5,
                                    selectInput("comp",
                                                label="Select Competition",
                                                choices= unique(shots.shiny.final$competition.competition_name),
                                                selected = unique(shots.shiny.final$competition.competition_name)[1]),
                                    
                                    selectInput("season",
                                                label="Select Season",
                                                choices=unique(shots.shiny.final$season.season_name),
                                                selected = unique(shots.shiny.final$season.season_name)[1]),
                                    
                                    selectInput("team",
                                                label="Select Team(s)",
                                                choices=unique(shots.shiny.final$team_name),
                                                multiple=TRUE,
                                                selected = unique(shots.shiny.final$team_name)[1]),
                                    
                                    selectInput("type",
                                                label="Select Shot Type(s)",
                                                choices=unique(shots.shiny.final$Shot.Type),
                                                multiple=TRUE,
                                                selected = "Normal"),
                                    
                                    selectInput("outcome",
                                                label="Select Shot Outcome(s)",
                                                choices=unique(shots.shiny.final$Shot.Outcome),
                                                multiple=TRUE,
                                                selected="Goal"),
                                    
                                    checkboxGroupInput("foot", 
                                                       label="Select Shot Foot", 
                                                       choices = list("Left Foot" = "Left Foot", 
                                                                      "Right Foot" = "Right Foot", 
                                                                      "Head" = "Head",
                                                                      "Other" = "Other"),
                                                       selected = c("Right Foot","Left Foot")),
                                sliderInput("xG",label="Filter Shots Depending on xG",
                                            min=round(min(shots.shiny.final$xG,na.rm = TRUE),1),
                                            max=round(max(shots.shiny.final$xG,na.rm = TRUE),1),value=c(0,1)),
                                downloadButton(outputId = "downloadData", label = "Download the data", width = "160px")
                                #)
                            ),
                            mainPanel(width = 9,
                                      verticalLayout(flowLayout(
                                          #imageOutput("blanc_img", width = "2%", height = "2%"),
                                          plotOutput("shotplot", width = 600, height = 400),
                                          imageOutput("blanc1_img", width = "2%", height = "2%")),
                                              verticalLayout(
                                                  #br(),
                                                 plotOutput("assistmap",brush="assist_brush", height = "150px", width = "180px"),
                                                #actionButton("calculate", "Show Shot Map")),
                                                #verbatimTextOutput("selectedpoints")
                                                #verbatimTextOutput("selectedpoints"),
                                                #tableOutput("mytable"),
                                                dataTableOutput("mytable2"))
                                      )
                 
                            )
                        )))
    
)

server <- function(input, output, clientData,session) {
    
    output$home_img <- renderImage({
        
        list(src = "nogomethomepage2.png",
             width = "100%",
             height = "140%"
        )
        
    }, deleteFile = F)
    
    
    output$blanc_img <- renderImage({
        
        list(src = "fondblanc.png",
             width = "10%",
             height = "10%"
        )
        
    }, deleteFile = F)
    
    
    output$blanc1_img <- renderImage({
        
        list(src = "fondblanc.png",
             width = "10%",
             height = "10%"
        )
        
    }, deleteFile = F)
    
    
    observeEvent(input$comp,{
        comp <- input$comp
        season_options <- unique(shots.shiny.final[which(shots.shiny.final$competition.competition_name==comp),]$season.season_name)
        
        updateSelectInput(session, inputId="season",
                          choices=season_options)
        
    })
    
    observeEvent(input$season,{
        season <- input$season
        team_options <- unique(shots.shiny.final[which(shots.shiny.final$season.season_name==season & shots.shiny.final$competition.competition_name==input$comp),]$team_name)
        updateSelectInput(session, "team",
                          choices=team_options)
    })
    
    values <- reactiveValues()
    observe({
        if(!is.null(input$assist_brush)){
            x <- brushedPoints(campo.coordinates,input$assist_brush,xvar="X",yvar="Y")
            convex.hull <- x[chull(x[,1],x[,2]),]
            shots.to.include <- which(in.out(as.matrix(convex.hull),as.matrix(shots.shiny.final[which(!is.na(shots.shiny.final$X.KeyPass)),c("X.KeyPass","Y.KeyPass")])))
            shots.to.map <- shots.shiny.final[shots.to.include,]
            values$df <-  shots.to.map[which(shots.to.map$season.season_name==input$season & shots.to.map$competition.competition_name==input$comp &
                                                 shots.to.map$Shot.Type %in% input$type & shots.to.map$team_name%in%input$team &
                                                 shots.to.map$Shot.Outcome %in% input$outcome & shots.to.map$xG >= min(input$xG) & shots.to.map$xG <= max(input$xG)),]
            
        }else{
            values$df <-   shots.shiny.final[which(shots.shiny.final$season.season_name==input$season & shots.shiny.final$competition.competition_name==input$comp &
                                                       shots.shiny.final$Shot.Type %in% input$type & shots.shiny.final$team_name%in%input$team &
                                                       shots.shiny.final$Shot.Outcome %in% input$outcome & shots.shiny.final$xG >= min(input$xG) & shots.shiny.final$xG <= max(input$xG)),]

        }  
    })
    
    
    values2 <- reactiveValues()
    observe({
        if(!is.null(input$assist_brush)){
            x <- brushedPoints(campo.coordinates,input$assist_brush,xvar="X",yvar="Y")
            convex.hull <- x[chull(x[,1],x[,2]),]
            shots.to.include <- which(in.out(as.matrix(convex.hull),as.matrix(shots.shiny.final[which(!is.na(shots.shiny.final$X.KeyPass)),c("X.KeyPass","Y.KeyPass")])))
            shots.to.map2 <- shots.shiny.final2[shots.to.include,]
            values2$df <-  shots.to.map2[which(shots.to.map$season.season_name==input$season & shots.to.map$competition.competition_name==input$comp &
                                                 shots.to.map$Shot.Type %in% input$type & shots.to.map$team_name%in%input$team &
                                                 shots.to.map$Shot.Outcome %in% input$outcome & shots.to.map$xG >= min(input$xG) & shots.to.map$xG <= max(input$xG)),]
  
        }else{

            values2$df <-   shots.shiny.final2[which(shots.shiny.final$season.season_name==input$season & shots.shiny.final$competition.competition_name==input$comp &
                                                       shots.shiny.final$Shot.Type %in% input$type & shots.shiny.final$team_name%in%input$team &
                                                       shots.shiny.final$Shot.Outcome %in% input$outcome & shots.shiny.final$xG >= min(input$xG) & shots.shiny.final$xG <= max(input$xG)),]

        }  
    })
    
    
    output$assistmap <- renderPlot({hori5})
    
    output$shotplot <- renderPlot({
        shots.data <- values$df
        
        hori5 + geom_point(data=shots.data, aes(x=X.Shot,y=Y.Shot,group=team_name,color=team_name,shape=team_name,size=xG))+
            scale_size_continuous(limits=c(0,1))
        
    })
    
    

    output$mytable <- renderTable(values2$df)
    
    output$mytable2 <- renderDataTable(values2$df)
    
    output$downloadData <- downloadHandler(
        filename = function(){
            paste("nogomet-plot", "csv", sep = ".")
        },
        content = function(file){
            write.csv(output$mytable2(), file, row.names = TRUE)
            dev.off()
        }
    )
    
}

shinyApp(ui = ui, server = server)
