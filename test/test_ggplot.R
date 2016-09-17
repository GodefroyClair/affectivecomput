ibrary(dplyr);library(tidyr);library(ggplot2)
#make some data
data = mtcars[c(27,19,16),]
data$model=row.names(data)

#connvert data to long format and also rescale it into 0-1 scales
data1 <- data %>% gather(measure,value,-model) %>% group_by(measure) %>% mutate(value1=(value-min(value))/(max(value)-min(value)))

is.linear.polar <- function(coord) TRUE
ggplot(data1,aes(x=measure,y=value1,color=model,group=model))+geom_line()+coord_polar()


library(ggplot2)

coord_radar <- function(...) {
  structure(coord_polar(...), class = c("radar", "polar", "coord"))
}
is.linear.radar <- function(coord) TRUE

scaled <- as.data.frame(lapply(mtcars, ggplot2:::rescale01))
scaled$model <- rownames(mtcars)

mtcarsm <- reshape2::melt(scaled)
qplot(variable, value, data = mtcarsm, geom = "line", group = model) + 
  coord_radar()






example <- data.frame(c(5,4,3),c(0.9,1.1,0.6))

colnames(example) <- c("r", "theta")

myplot <- ggplot(example, aes(r, theta)) + geom_point(size=3.5) +
  coord_polar(theta="y", start = 3/2*pi, direction=-1) + 
  scale_x_continuous(breaks=seq(0,max(example$r)), lim=c(0, max(example$r))) + 
  scale_y_continuous(breaks=round(seq(0, 2*pi, by=pi/4),2), expand=c(0,0), lim=c(0,2*pi)) +
  geom_text(aes(label=rownames(example)), size=4.4, hjust=0.5, vjust=-1) + 
  geom_path(); myplot










df <- structure(list(ri = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240, 250, 260, 270, 280, 290, 300, 310, 320, 330, 340, 350, 360),
                     n = c(329L, 315L, 399L, 700L, 919L, 757L, 656L, 918L, 1117L, 976L, 878L, 803L, 811L, 1072L, 1455L, 1642L, 1891L, 1688L, 1553L, 1841L, 2061L, 2321L, 2498L, 2080L, 1595L, 1080L, 1002L, 953L, 729L, 604L, 538L, 489L, 535L, 455L, 328L, 351L, 329L),
                     d = c(0.008581340149717, 0.00821617673909074, 0.0104071572028483, 0.0182581705313128, 0.0239703695975378, 0.0197449072745768, 0.017110514097916, 0.0239442864967787, 0.0291348235478234, 0.0254571063408018, 0.022900962466418, 0.0209447299094916, 0.0211533947155638, 0.0279610840136675, 0.0379509116043715, 0.0428284514463079, 0.0493231435353035, 0.0440282740812228, 0.0405070554787553, 0.0480189884973526, 0.0537572706643366, 0.0605388768616813, 0.0651555856960275, 0.0542528495787579, 0.0416025457106341, 0.0281697488197397, 0.0261352669605363, 0.0248571950233444, 0.0190145804533243, 0.015754192858447, 0.0140327082083518, 0.0127546362711599, 0.0139544589060748, 0.0118678108453533, 0.00855525704895798, 0.0091551683664154, 0.008581340149717)),
                .Names = c("ri", "n", "d"), row.names = c(NA, 37L), class = c("tbl_df", "tbl", "data.frame"))
df <- df[df$ri!=360,]
ggplot(df, aes(x=ri, y=d)) +
  geom_line() +
  scale_x_continuous(breaks=seq(0,360,10)) +
  coord_polar() +
  theme_bw() +
  theme(panel.grid.minor=element_blank())

ggplot(df, aes(x=ri, y=d)) +
  geom_line() +
  #scale_x_continuous(breaks=seq(0,360,10)) +
  scale_y_continuous(limits=c(0,0.07), breaks=seq(0,0.06,0.01)) +
  coord_polar() 
  
  
 ggplot(df, aes(x=factor(ri), y=d, group=1)) +
    geom_polygon(fill=NA, color="black")  +
    scale_y_continuous(limits=c(0,0.07), breaks=seq(0,0.06,0.01))   +
    coord_polar(start=-pi*1/36) 
  
  +
    theme_bw() +
    theme(panel.grid.minor=element_blank())
  
