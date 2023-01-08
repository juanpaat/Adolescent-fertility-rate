library(ggplot2)
library(dplyr)
library(ggthemes)
library(hrbrthemes)
library(ggsci)
library(extrafont)
library(showtext)
library(systemfonts)


data <- read.csv('API_SP.ADO.TFRT_DS2_en_csv_v2_4772546.csv',skip = 4)
metadata <- read.csv('Metadata_Country_API_SP.ADO.TFRT_DS2_en_csv_v2_4772546.csv')
tileURL <- "https://gist.githubusercontent.com/maartenzam/787498bbc07ae06b637447dbd430ea0a/raw/9a9dafafb44d8990f85243a9c7ca349acd3a0d07/worldtilegrid.csv"
tile <- read.csv(tileURL)

data <- data[c(1,2,3,63,64,65)]
metadata <- metadata[c(1,2,3,5)]

tile<-merge(x=tile, y=metadata,
            all.x=TRUE,
            all.y = FALSE ,
            by.x = 'alpha.3',
            by.y ='Country.Code')




  
data <- data[complete.cases(data),]
metadata <- metadata[complete.cases(metadata),]
###### left join in R using merge() function 
df = merge(x=data, y=metadata,
           all.x=TRUE,
           all.y = FALSE ,
           by.x = 'Country.Code' ,
           by.y ='Country.Code')

df<- df %>%
  group_by(Region) %>% 
  mutate(X2020_ratio = mean(X2020, na.rm = FALSE))%>%
  ungroup()

df <-df[df$Region!="",]

#plot
ggplot(data = df, aes(x=Region, y=X2020))+
    geom_boxplot()

#sort the data
ggplot(df, aes(x = reorder(Region,-X2020), y = X2020)) +
  geom_boxplot()


#flip axis
ggplot(df, aes(x = reorder(Region,-X2020), y = X2020)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(limits = c(0, 180))
  

#change the theme
theme_set(theme_light())

g <-
  ggplot(df, aes(x = reorder(Region,-X2020), y = X2020, color = Region)) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 190)) +
  scale_color_uchicago() +
  labs(x = NULL, y = "Adolescent fertility rate")+
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = NULL, size = 12),
    panel.grid = element_blank()
  )

g + geom_boxplot()
g + geom_violin()
g + geom_line(size=1)
g + geom_point(size=1)

g +   geom_point(size = 3, alpha = 0.15)

g +   geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_point(size = 3, alpha = 0.15)


set.seed(9613)
g + geom_jitter(size = 2, alpha = 0.25, width = 0.2)


g + geom_jitter(position = position_jitter(seed = 2019, width = 0.2), size = 2, alpha = 0.25)


g +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 5)


#Line with world average
world_avg <-
  df %>%
  summarize(avg = median(X2020, na.rm = TRUE)) %>%
  pull(avg)
world_avg

g +
  geom_hline(aes(yintercept = world_avg), color = "gray70", size = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2)


# lines from region mean to the world's mean 
g +
  geom_segment(
    aes(x = reorder(Region,-X2020), xend = reorder(Region,-X2020),
        y = world_avg, yend =X2020_ratio ),
    size = 0.8
  ) +
  geom_hline(aes(yintercept = world_avg), color = "gray70", size = 0.6) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 5)




#adding the text
(g_text <-
    g +
    geom_segment(
      aes(x = reorder(Region,-X2020), xend = reorder(Region,-X2020),
          y = world_avg, yend =X2020_ratio ),
      size = 0.8
    ) +
    geom_hline(aes(yintercept = world_avg), color = "gray70", size = 0.6) +
    geom_jitter(position = position_jitter(seed = 1996, width = 0.2), size = 2, alpha = 0.25) +
    stat_summary(fun = mean, geom = "point", size = 5)+
    annotate(
      "text", x = 7, y = 85, size = 2.8, color = "gray20", lineheight = .9,
      label = glue::glue("Worldwide average:\n{round(world_avg, 1)} births per 1,000 women ages 15-19")
    ) +
    annotate(
      "text", x = 3.5, y = 120, size = 2.8, color = "gray20",
      label = "Countries per Region"
    ) +
    annotate(
      "text", x = 1.6, y = 140, size = 2.8, color = "gray20", lineheight = .9,
      label = "Sub-Saharan Africa has by far\nthe most births per adolescent")
)



#adding the arrows
arrows <-
  tibble(
    x1 = c(6.8, 3.5, 3.5, 1.5),
    x2 = c(6.4, 3.8, 3.1, 1),
    y1 = c(50, 99, 99, 169),
    y2 = c(world_avg, 81, 83, 177.464)
  )


(g_arrows <- g_text +
      geom_curve(
        data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
        arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
        color = "gray20", curvature = -0.3
  )
)



#tile map 

(map_regions <-
    tile %>%
    ggplot(aes(x = x, y = y, fill = Region, color = Region)) +
    geom_tile(color = "white") +
    scale_y_reverse() +
    ggsci::scale_fill_uchicago(guide = "none") +
    coord_equal() +
    theme(line = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = "transparent"),
          panel.border = element_rect(color = "transparent"),
          strip.background = element_rect(color = "gray20"),
          axis.text = element_blank(),
          plot.margin = margin(0, 0, 0, 0)) +
    labs(x = NULL, y = NULL)
)



#adding the source
(g_final <-
    g_arrows +
    scale_y_continuous(
      limits = c(1, NA), expand = c(0.02, 0.02),
      breaks = c(1, seq(20, 190, by = 20))
    ) +
    labs(caption = "Data: The World Bank\nVisualisation by JuanBioData") +
    theme(plot.caption = element_text(size = 9, color = "gray50"))
)




#Final map
g_final +
  annotation_custom(ggplotGrob(map_regions), xmin = 4, xmax = 7, ymin = 120, ymax = 180)





