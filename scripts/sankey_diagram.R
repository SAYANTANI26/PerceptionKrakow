rm(list=ls())

# list the necessary packages and dependencies
pkgs <- c("tidyverse", "networkD3")
lapply(pkgs, require, character.only = TRUE)

theme_set <- ggplot2::theme_bw() +
  ggplot2::theme(text = element_text(size = 16, hjust = 0.5, vjust = 0.5),
                 panel.border=element_blank(),
                 panel.grid=element_blank(),
                 legend.key=element_blank(),
                 legend.text.align=0,
                 legend.text=element_text(size = 10, hjust = 0.5, vjust = 0.5),
                 legend.position="top",
                 strip.text=element_text(face="bold", size = 16),
                 axis.line=element_blank(),
                 axis.line.x=element_blank(),
                 axis.line.y=element_blank(),
                 panel.background=element_rect(fill="transparent", colour="black", size = 2),
                 plot.background=element_rect(fill="transparent", colour=NA),
                 strip.background=element_rect(fill="transparent", colour=NA),
                 strip.placement="outside")
# Set path
setwd("~/Desktop/Perception/Data_analysis/")
df.an <- read.delim2("./data/sankey_annotation.txt", header = T, as.is = T, sep = "\t")
mat.df <- read.delim2("./data/dataset_sankey.txt", header = T, as.is = T, sep = "\t")

# df <- matrix(ncol = length(unique(mat.df$animals)), nrow = nrow(mat.df))
# colnames(df) <- unique(mat.df$animals)
# row.names(df) <- (mat.df$animals)
# 
# for(i in colnames(df)){
#   
#   df[which(row.names(df) == i), i] <- 1
#   
# }
# 
# df <- apply(df, 2, function(x) replace_na(x, 0))
# colnames(df) <- df.an$sk_id[match(colnames(df), df.an$description)]
# 
# mat.df <- cbind.data.frame(mat.df, df)
mat.df <- apply(mat.df, 2, function(x) replace_na(x, 0))
data.frame(sum=(colSums(mat.df))) %>% write.table("./output/sum_sankey.txt", sep = "\t", quote = FALSE, row.names = TRUE)

mat.df <- as.matrix(mat.df) %>% 
  crossprod(.) %>% 
  data.frame(.)

# Making source to sink data format for network
cormat.df <- mat.df %>% add_column(source = row.names(mat.df), .before = 1) %>%
  gather(key = "target", value = "value", convert = FALSE, -source) %>%
  filter(source != target) %>%
  data.frame(., stringsAsFactors = FALSE)

# Feed in the annotations and categories
df.an$id <- paste(df.an$key, df.an$description, sep = "_")
idx <- match(cormat.df$source, df.an$sk_id)
idy <- match(cormat.df$target, df.an$sk_id)

# temp <- cormat.df %>% cbind(df.an[idx, ], df.an[idy, ])

# Preparing links by string mining
links <- cormat.df %>% 
  mutate(source = df.an$id[idx],
         target = df.an$id[idy]) %>% 
  filter(source != target)

id1 <- which(str_detect(links$source, "^Behaviour") & (str_detect(links$target, "^Animal")))
id2 <- which(str_detect(links$source, "^Animal") &
               !str_detect(links$target, "^Behaviour") &
               !str_detect(links$target, "^Animal")
             )
# id3 <- which(str_detect(links$source, "^Spring") &
#                str_detect(links$target, "^Summer")
# )
# id3 <- which(str_detect(links$source, "^Summer") &
#                str_detect(links$target, "^Autumn")
# )
# id4 <- which(str_detect(links$source, "^Autumn") &
#                str_detect(links$target, "^Winter")
# )

# idx <- c(id1, id2, id3, id4)
idx <- c(id1, id2)
links <- links[idx,]

idx <- match(links$source, df.an$id)
idy <- match(links$target, df.an$id)

# Making links dataset
links <- links %>% mutate(source = df.an[idx, "description"],
                          target = df.an[idy, "description"]) %>%
  filter(source != target) %>% 
  group_by(source, target) %>%
  summarise(value = sum(value)) %>%
  filter(value != 0) %>%
  data.frame(., stringsAsFactors=FALSE)

# Making links as per SankeyNetwork functions
links$IDsource <- match(links$source, df.an$description)-1
links$IDtarget <- match(links$target, df.an$description)-1
links$group <- df.an$description[match(links$source, df.an$description)]
# links$group <- as.factor(links$group)
df.an$group <- as.factor(c("uniques"))

# my_color <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "my_unique_group"]) .range(["#69b3a2", "steelblue", "grey"])'

# Prepare network diagram
p5 <- networkD3::sankeyNetwork(Links = links, 
                               Nodes = df.an, 
                               Source = "IDsource",
                               Target = "IDtarget", 
                               Value = "value", 
                               NodeGroup = "group",
                               LinkGroup = "group",
                               sinksRight=TRUE,
                               NodeID = "description", 
                               fontSize = 20, 
                               nodeWidth = 30, 
                               iteration = 0, fontFamily = "Arial")

#range(["orangered","orangered","orangered","orangered","seagreen","seagreen","seagreen","seagreen","dodgerblue","dodgerblue","dodgerblue","dodgerblue","dodgerblue","dodgerblue","dodgerblue","dodgerblue","grey"])'
htmlwidgets::saveWidget(p5, file = "./figures/sankey_new.html")
# END OF SCRIPT
sessionInfo()