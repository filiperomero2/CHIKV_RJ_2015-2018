library(ggplot2)
library(lubridate)


df <- read.table(file = "rtt_data.noOutliers.tsv",
                 sep="\t",
                 header=T,
                 as.is = T)
head(df)

meta <- read.table(file = "metadata.noOutliers.tsv",
                   sep="\t",
                   header=T,
                   as.is = T)
head(meta)

region <- vector()
for(i in 1:length(df$tip)){
  region[i] <- meta$region[which(meta$name==df$tip[i])]
}
table(region)
df$region <- region
df$region[grep(pattern = "^\\d+",x = df$tip)] <- "Original"





m <- lm(formula = distance ~ date,data = df)
summary(m)

head(df)

colors <- c("#cc5252ff",
            "#8fcc52ff",
            "#52ccccff",
            "#8f52ccff",
            "#000080ff")

color_names <- c("Centre-West",
                 "North",
                 "Northeast",
                 "RJ",
                 "Original")

names(colors) <- color_names


ggplot()+
  geom_smooth(data = df,aes(x=date,y=distance),color="grey70",method="lm",se = FALSE) +
  geom_point(data = df,aes(x=date,y=distance,fill=region,color=region),size=2) +
  scale_color_manual(values = colors) +
  ylab("Root-to-tip distance") +
  xlab("Collection date") + 
  theme_classic() +
  theme(legend.position = "none")






