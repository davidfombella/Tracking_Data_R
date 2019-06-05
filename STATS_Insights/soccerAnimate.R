soccerAnimate <- function(data, sequence, pitch_color="#78c679", 
                          team1_color="#2b8cbe", team2_color= "#dd3497", 
                          method=c("clean", "convexhull", "voronoi"), provider="stats"){ 
    require(dplyr)
    require(tidyr)
    require(stringr)
    require(soccermatics)
    require(ggsoccer)
    require(ggplot2)
    require(gganimate)
    require(ggforce)
    require(gifski)
    require(transformr)
        
    ## Subsetting
    s <- sequence
    w <- data %>%
         filter(sequ == s) %>%
         mutate(sample=1:n())
        
    ## Transform data structure for 1 sequence
    if (provider=="stats"){
        
        # columns number of x,y coordenates for both teams
        idx1 <- seq(2, 22, 2)
        idy1 <- seq(3, 23, 2)
        idx2 <- seq(24, 44, 2)
        idy2 <- seq(25, 45, 2)
        
        names(w)[idx1] <- paste0("def", 1:11, "x")
        names(w)[idy1] <- paste0("def", 1:11, "y")
        names(w)[idx2] <- paste0("att", 1:11, "x")
        names(w)[idy2] <- paste0("att", 1:11, "y")
        
        # reshaping players info
        wtx <- gather(w, key, value, -1, -idy1, -idy2, -46, -47, -48) %>%
                select("sequ", "sample", key, value) %>%
                mutate(key2= substr(key, 1, (nchar(key) -1)))
        
        wty <- gather(w, key, value, -1, -idx1, -idx2, -46, -47, -48) %>%
                select("sequ", "sample", key, value) %>%
                mutate(key2= substr(key, 1, (nchar(key) -1)))
        
        wt <- wtx %>% inner_join(wty, by=c("sample", "key2")) %>%
                select(sequ.x, sample, key2, value.x, value.y) %>%
                rename(sequ=sequ.x, key=key2, x=value.x, y=value.y)
        
        wtt <- soccerTransform(wt, xMin=-52.5, xMax=52.5, yMin=-34, yMax=34, 
                               method="manual", lengthPitch = 120, widthPitch = 80)
        
        
        # reshaping ball info
        btx <- gather(w, key, value, -1, -idx1, -idx2, -idy1, -idy2, -47, -48) %>%
                select("sequ", "sample", key, value)
        
        bty <- gather(w, key, value, -1, -idx1, -idx2, -idy1, -idy2, -46, -48) %>%
                select("sequ", "sample", key, value)
        
        bt <- btx %>% inner_join(bty, by="sample") %>%
                select(sequ.x, sample, value.x, value.y) %>%
                rename(sequ=sequ.x, x=value.x, y=value.y)
        
        btt <- soccerTransform(bt, xMin=-52.5, xMax=52.5, yMin=-34, yMax=34, 
                               method="manual", lengthPitch = 120, widthPitch = 80)
        
        btt$key <- "ball"
        
        pos_data <- wtt %>% bind_rows(btt) %>%
                    mutate(team_name=substr(key, 1, 3), type="pos") #%>%
                    #filter(sample<10)
    }
    
    
    ## Viz + Animation
    if (method=="convexhull"){
            
        hull_data <- wtt %>%
                filter(key!="def1" & key!="att1") %>%  #sin arqueros
                mutate(team_name=substr(key, 1, 3)) %>%
                group_by(sample, team_name) %>%
                slice(chull(x, y)) %>%
                mutate(type="chull") 
        
        anim <- ggplot() +
                annotate_pitch(colour = "white",
                               fill   = pitch_color,
                               dimensions = pitch_statsbomb) +
                theme_pitch() +
                geom_polygon(data=hull_data, aes(x=x, y=y, fill=factor(team_name)), alpha=0.3) +
                geom_point(data=pos_data, aes(x=x, y=y, fill=factor(team_name), 
                                              size=factor(team_name)), 
                           col="black", 
                           shape=21) +
                transition_time(sample) +
                theme(plot.background = element_rect(fill = pitch_color),
                      title = element_text(colour = "white"),
                      legend.position = "none") +
                scale_size_manual(values=c(5,3,5)) +
                scale_fill_manual(values=c(team1_color, "yellow", team2_color), 
                                  aesthetics = "fill") +
                ggtitle(label=paste("STATS insights dataset - ", sequence, "/ @bigdatasport")) 
        
        a <- animate(anim,
                     width = 1024, height = 768,
                     nframes=nrow(btt), fps = 10)
        
        print(a)
    }
    
    if (method=="voronoi"){
             
            vor_data <- wtt %>%
                        mutate(team_name=substr(key, 1, 3), type="voronoi")
            
            anim <- ggplot() +
                    annotate_pitch(colour = "white",
                                   fill   = pitch_color,
                                   dimensions = pitch_statsbomb) +
                    theme_pitch() +
                    geom_voronoi_tile(data=vor_data, aes(x=x, y=y, fill=team_name, group = -1L), 
                                      colour = 'black', alpha=0.3, bound=c(0, 120, 0, 80)) + # statsbomb pitch dimensions
                    geom_point(data=pos_data, aes(x=x, y=y, fill=factor(team_name), 
                                                  size=factor(team_name)), 
                               col="black", 
                               shape=21) +
                    transition_time(sample) +
                    theme(plot.background = element_rect(fill = pitch_color),
                          title = element_text(colour = "white"),
                          legend.position = "none") +
                    scale_size_manual(values=c(5,3,5)) +
                    scale_fill_manual(values=c(team1_color, "yellow", team2_color), 
                                      aesthetics = "fill") +
                    ggtitle(label=paste("STATS insights dataset - ", sequence, "/ @bigdatasport")) 
            
            a <- animate(anim,
                         width = 1024, height = 768,
                         nframes=nrow(btt), fps = 10)
            
            print(a)
    }
    
    if (method=="clean"){
            
        anim <- ggplot() +
            annotate_pitch(colour = "white",
                           fill   = pitch_color,
                           dimensions = pitch_statsbomb) +
            theme_pitch() +
            geom_point(data=pos_data, aes(x=x, y=y, fill=factor(team_name), 
                                          size=factor(team_name)), 
                       col="black", 
                       shape=21) +
            transition_time(sample) +
            theme(plot.background = element_rect(fill = pitch_color),
                  title = element_text(colour = "white"),
                  legend.position = "none") +
            scale_size_manual(values=c(5,3,5)) +
            scale_fill_manual(values=c(team1_color, "yellow", team2_color), 
                              aesthetics = "fill") +
            ggtitle(label=paste("STATS insights dataset - ", sequence, "/ @bigdatasport")) 
            
        a <- animate(anim,
                     width = 1024, height = 768,
                     nframes=nrow(btt), fps = 10)
            
        print(a)
    }     
}
