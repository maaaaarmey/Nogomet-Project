library(ggplot2)

###########CODE POUR TRACER LE TERRAIN DE FOOTBALL 

#On définit la taille du terrain ainsi que l'ensemble que celle des points clés
stadiumlength <- 120
stadiumwidth <- 80
linewidth <- .12
goalboxlength <- 16.5
goalboxpos <- (stadiumwidth-goalboxlength*2-7.32)/2
smallboxlength <- 5.5
smallboxpos <- goalboxpos+goalboxlength-smallboxlength
crossbarl <- 7.32
crossbarh <- 2.44
goalpos <- goalboxpos+goalboxlength
attdefzone <- 30
channel <- 4
channelsplit <- 25



##Cercle de milieu de terrain
circle_fun <- function(center=c(0,0), diameter=1, npoints=1000, start=0, end=2){
  tt <- seq(start*pi, end*pi, length.out=npoints)
  data.frame(
    x = center[1] + diameter / 2 * cos(tt),
    y = center[2] + diameter / 2 * sin(tt)
  )
}



mirror <- function(x) stadiumlength-x


#Pour utiliser ggplot2, nous devons définir tous les points qui entreront dans la forme du champ
#Les quatre paramètres sont : x (valeurs de largeur), y (valeurs de longueur), groupe (pour les deux côtés), desc (une description pour faciliter l'utilisation et la personnalisation ultérieure).
segment_coord <- function(x, y, group, desc){
  segment_df <- data.frame(x = x, y = y) 
  segment_df$group <- group
  segment_df$side <- 1
  group <- group + 1
  
  #Pareil pour l'autre coté
  segment_df2 <- data.frame(x = x, y = mirror(y))
  segment_df2$group <- group
  segment_df2$side <- 2
  group <<- group + 1
  
  #Puis on réunit les données ensemble
  segment_df <- rbind(segment_df, segment_df2)
  segment_df$desc <- desc
  
  return(segment_df)
}

#Fonction pour retourner le dessin horizontalement
rotate_pitch <- function(pitch, theta=pi/2){
  pitch_r <- pitch
  pitch_r$x <- pitch_r$x / 180 * pi
  pitch_r$y <- pitch_r$y / 180 * pi
  matrix_r <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2)
  coords_r <- apply(pitch_r[,c("x","y")], 1, function(x) x %*% matrix_r)
  pitch_r$x <- coords_r[1,] ; pitch_r$y <- coords_r[2,]
  pitch_r$x <- pitch_r$x * 180 / pi
  pitch_r$y <- pitch_r$y * 180 / pi
  return(pitch_r)
}




##Nous définissons ici les cercles dont nous avons besoin pour notre champ. Nos lignes de terrain ont une largeur de 12cm.
#Cercle central : au milieu de la ligne médiane, avec un rayon de 9,15 m
centercircle_outer <- circle_fun(center=c(stadiumwidth/2,stadiumlength/2),diameter = 9.15 * 2)
centercircle_inner <- circle_fun(center=c(stadiumwidth/2,stadiumlength/2),diameter = (9.15-linewidth) * 2)

#Penalty Arc -- arc à partir du point de pénalité (11 mètres) avec un rayon de 9,15 m 
penaltyarc_outer <- circle_fun(center=c(stadiumwidth/2,11),diameter = 9.15 * 2)
penaltyarc_inner <- circle_fun(center=c(stadiumwidth/2,11),diameter = (9.15-linewidth) * 2)

#Arcs des corners
cornerarc_outer_r <- circle_fun(center = c(stadiumwidth,0),diameter = 1*2)
cornerarc_inner_r <- circle_fun(center = c(stadiumwidth,0),diameter = (1-linewidth)*2)

cornerarc_outer_l <- circle_fun(center = c(0,0),diameter = 1*2)
cornerarc_inner_l <- circle_fun(center = c(0,0),diameter = (1-linewidth)*2)

#Point de penalty
penaltyspot <- circle_fun(center=c(stadiumwidth/2,11),diameter=.5*2)


#Dataframe avec tous les points clés
group <- 1

pitch <- segment_coord(x=c(0-linewidth,0-linewidth,stadiumwidth+linewidth,stadiumwidth+linewidth)
                       ,y=c(0-linewidth,0,0,0-linewidth),group=group,desc = "goal line") 

pitch <- rbind(pitch,segment_coord(x=c(0-linewidth,0-linewidth,0,0)
                                   ,y=c(0,stadiumlength/2-linewidth/2,stadiumlength/2-linewidth/2,0),group=group,desc = "left touch line"))

pitch <- rbind(pitch,segment_coord(x=c(stadiumwidth,stadiumwidth,stadiumwidth+linewidth,stadiumwidth+linewidth)
                                   ,y=c(0,stadiumlength/2-linewidth/2,stadiumlength/2-linewidth/2,0),group=group,desc = "right touch line"))

pitch <- rbind(pitch,segment_coord(x=c(goalboxpos-linewidth,goalboxpos-linewidth,goalboxpos,goalboxpos)
                                   ,y=c(0,goalboxlength-linewidth,goalboxlength-linewidth,0),group=group,desc = "left 18 yard box"))

pitch <- rbind(pitch,segment_coord(x=c(stadiumwidth-goalboxpos-linewidth,stadiumwidth-goalboxpos-linewidth,stadiumwidth-goalboxpos,stadiumwidth-goalboxpos)
                                   ,y=c(0,goalboxlength-linewidth,goalboxlength-linewidth,0),group=group,desc = "right 18 yard box"))

pitch <- rbind(pitch,segment_coord(x=c(goalboxpos-linewidth,goalboxpos-linewidth,stadiumwidth-goalboxpos,stadiumwidth-goalboxpos)
                                   ,y=c(goalboxlength-linewidth,goalboxlength,goalboxlength,goalboxlength-linewidth),group=group,desc = "18 yard box line"))

pitch <- rbind(pitch, segment_coord(x=c(smallboxpos-linewidth,smallboxpos-linewidth,smallboxpos,smallboxpos)
                                    ,y=c(0,smallboxlength-linewidth,smallboxlength-linewidth,0),group=group,desc = "left 6 yard box"))

pitch <- rbind(pitch, segment_coord(x=c(stadiumwidth-smallboxpos-linewidth,stadiumwidth-smallboxpos-linewidth,stadiumwidth-smallboxpos,stadiumwidth-smallboxpos)
                                    ,y=c(0,smallboxlength-linewidth,smallboxlength-linewidth,0),group=group,desc = "right 6 yard box"))

pitch <- rbind(pitch, segment_coord(x=c(smallboxpos-linewidth,smallboxpos-linewidth,stadiumwidth-smallboxpos,stadiumwidth-smallboxpos)
                                    ,y=c(smallboxlength-linewidth,smallboxlength,smallboxlength,smallboxlength-linewidth),group=group,desc = "6 yard box line"))

pitch <- rbind(pitch, segment_coord(x=c(goalpos-linewidth,goalpos-linewidth,goalpos,goalpos)
                                    ,y=c(0-crossbarh,0,0,0-crossbarh),group=group,desc = "left goal post"))

pitch <- rbind(pitch, segment_coord(x=c(stadiumwidth-goalpos-linewidth,stadiumwidth-goalpos-linewidth,stadiumwidth-goalpos,stadiumwidth-goalpos)
                                    ,y=c(0-crossbarh,0,0,0-crossbarh),group=group,desc = "right goal post"))

pitch <- rbind(pitch, segment_coord(x=c(goalpos-linewidth,goalpos-linewidth,stadiumwidth-goalpos,stadiumwidth-goalpos)
                                    ,y=c(0-crossbarh,0-crossbarh,0-crossbarh,0-crossbarh),group=group,desc = "crossbar"))

pitch <- rbind(pitch, segment_coord(x=penaltyspot[,"x"]
                                    ,y=penaltyspot[,"y"],group=group,desc = "penalty spot"))

pitch <- rbind(pitch, segment_coord(x=c(centercircle_outer[centercircle_outer$y<=stadiumlength/2,"x"],rev(centercircle_inner[centercircle_inner$y<=stadiumlength/2,"x"]))
                                    ,y=c(centercircle_outer[centercircle_outer$y<=stadiumlength/2,"y"],rev(centercircle_inner[centercircle_inner$y<=stadiumlength/2,"y"])),group=group,desc = "center circle"))


pitch <- rbind(pitch, segment_coord(x=c(0-linewidth,0-linewidth,stadiumwidth+linewidth,stadiumwidth+linewidth)
                                    ,y=c(stadiumlength/2-linewidth/2,stadiumlength/2+linewidth/2,stadiumlength/2+linewidth/2,stadiumlength/2-linewidth/2),group=group,desc = "halfway line"))

pitch <- rbind(pitch, segment_coord(x=c(penaltyarc_outer[penaltyarc_outer$y>=goalboxlength,"x"],rev(penaltyarc_inner[penaltyarc_inner$y>=goalboxlength,"x"]))
                                    ,y=c(penaltyarc_outer[penaltyarc_outer$y>=goalboxlength,"y"],rev(penaltyarc_inner[penaltyarc_inner$y>=goalboxlength,"y"])),group=group,desc = "penalty arc"))

pitch <- rbind(pitch, segment_coord(x=c(cornerarc_outer_l[cornerarc_outer_l$x>=0 & cornerarc_outer_l$y>=0,"x"],rev(cornerarc_inner_l[cornerarc_inner_l$x>=0 & cornerarc_inner_l$y>=0,"x"]))
                                    ,y=c(cornerarc_outer_l[cornerarc_outer_l$x>=0 & cornerarc_outer_l$y>=0,"y"],rev(cornerarc_inner_l[cornerarc_inner_l$x>=0 & cornerarc_inner_l$y>=0,"y"])),group=group,desc = "left corner"))

pitch <- rbind(pitch, segment_coord(x=c(cornerarc_outer_r[cornerarc_outer_r$x>=0&cornerarc_outer_r$x<=stadiumwidth & cornerarc_outer_r$y>=0,"x"],rev(cornerarc_inner_r[cornerarc_inner_r$x>=0&cornerarc_inner_r$x<=stadiumwidth& cornerarc_inner_r$y>=0,"x"]))
                                    ,y=c(cornerarc_outer_r[cornerarc_outer_r$x>=0&cornerarc_outer_r$x<=stadiumwidth & cornerarc_outer_r$y>=0,"y"],rev(cornerarc_inner_r[cornerarc_inner_r$x>=0&cornerarc_inner_r$x<=stadiumwidth & cornerarc_inner_r$y>=0,"y"])),group=group,desc = "right corner"))


pitch <- rbind(pitch, segment_coord(x=c(0-linewidth,0-linewidth,stadiumwidth+linewidth,stadiumwidth+linewidth)
                                    ,y=c(attdefzone-linewidth/2,attdefzone+linewidth/2,attdefzone+linewidth/2,attdefzone-linewidth/2),group=group,desc = "attacking zone"))

pitch <- rbind(pitch,segment_coord(x=c(goalboxpos+channel-linewidth,goalboxpos+channel-linewidth,goalboxpos+channel,goalboxpos+channel)
                                   ,y=c(0,stadiumlength/2-linewidth/2,stadiumlength/2-linewidth/2,0),group=group,desc = "left channel"))

pitch <- rbind(pitch,segment_coord(x=c(stadiumwidth-goalboxpos-channel-linewidth,stadiumwidth-goalboxpos-channel-linewidth,stadiumwidth-goalboxpos-channel,stadiumwidth-goalboxpos-channel)
                                   ,y=c(0,stadiumlength/2-linewidth/2,stadiumlength/2-linewidth/2,0),group=group,desc = "right channel"))

pitch <- pitch <- rbind(pitch,segment_coord(x=c(stadiumwidth/2-linewidth/2,stadiumwidth/2-linewidth/2,stadiumwidth/2+linewidth/2,stadiumwidth/2+linewidth/2)
                                            ,y=c(0,attdefzone,attdefzone,0),group=group,desc = "middle channel"))

pitch <- rbind(pitch,segment_coord(x=seq(0,goalboxpos,length.out = 20)
                                   ,y=seq(channelsplit,goalboxlength,length.out = 20),group=group,desc = "center left channel"))

pitch <- rbind(pitch,segment_coord(x=seq(stadiumwidth,stadiumwidth-goalboxpos,length.out = 20)
                                   ,y=seq(channelsplit,goalboxlength,length.out = 20),group=group,desc = "center right channel"))


vert3 <- ggplot() + geom_polygon(data = pitch[pitch$group %in% seq(1:36),], aes(x = x, y = y, group = group), col = "#FFFFFFB3") +
  geom_segment(aes(x=-5,y=100,xend=-5,yend=90),arrow=arrow(length = unit(0.03, "npc")))+
  coord_equal() +
  ylim(-20,125) +
  xlim(-20,88) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), axis.title = element_blank(),
        panel.background = element_rect(fill = "#77BD77",
                                        colour = "#77BD77",
                                        size = 0.5, linetype = "solid")
  )



#Rotation
horipitch <- rotate_pitch(pitch, theta = pi/2)
horipitch2 <- horipitch
horipitch2$y <- horipitch2$y * -1

hori5 <- ggplot()+ geom_polygon(data = horipitch2[horipitch2$group %in% seq(1:36),], aes(x = x, y = y, group = group), col = "#FFFFFFB3") +
  geom_segment(aes(x=90,y=-5,xend=100,yend=-5),arrow=arrow(length = unit(0.03, "npc")))+
  coord_equal() +
  theme(panel.background = element_rect(fill = '#77BD77', colour = '#77BD77')) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), axis.title = element_blank(),
        panel.background = element_rect(fill = "#77BD77",
                                        colour = "#77BD77")
  )