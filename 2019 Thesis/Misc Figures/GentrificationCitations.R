library(pacman)
p_load(ggplot2, tidyverse)

raw_data<- read.delim("GentrificationScholarCounts.txt")

colnames(raw_data) <- c("Year", "Records", "PCT")

n<-dim(raw_data)[1]
df<-raw_data[1:(n-2),]

df$Year <- as.numeric(as.character(df$Year))

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=8),
    axis.text = element_text(size=8),
    axis.title.x = element_text(hjust=1),
    axis.title.y = element_text(hjust=1),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"))
}

palette_1_colors <- c("#0DA3A0")

ggplot(df, aes(x = Year, y = Records)) + geom_bar(stat="identity", fill="dodgerblue4") + plotTheme()
