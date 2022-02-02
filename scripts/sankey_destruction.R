rm(list = ls())

# list the necessary packages and dependencies
pkgs <- c("tidyverse")
lapply(pkgs, require, character.only = TRUE)

# Set path
setwd("~/Desktop/Perception/Data_analysis/")



# Read data table
df <- read.table("./data/destruction.txt", sep = "\t", header = TRUE, as.is = TRUE, row.names = 1)
df.an <- read.table("./data/destruction_an.txt", sep = "\t", header = TRUE, as.is = TRUE)


mat.df <- apply(df, 2, function(x) replace_na(x, 0)) %>% 
  crossprod(.) %>% 
  data.frame(.)

# Making source to sink data format for network
mat.df <- mat.df %>% add_column(source = row.names(mat.df), .before = 1) %>%
  gather(key = "target", value = "value", convert = FALSE, -source) %>%
  filter(source != target) %>%
  data.frame(., stringsAsFactors = FALSE)

# Feed in the annotations and categories
df.an$id <- paste(df.an$key, df.an$description, sep = "_")
idx <- match(mat.df$source, df.an$Relationship)
idy <- match(mat.df$target, df.an$Relationship)

temp <- mat.df %>% cbind(df.an[idx, ], df.an[idy, ])

links <- mat.df %>% mutate(source = df.an[idx, "id"], 
                           target = df.an[idy, "id"])

# Preparing links by string mining
idx <- which(str_detect(links$source, "^animals_") & str_detect(links$target, "^problems_"))

links <- links[idx,]

idx <- match(links$source, df.an$id)
idy <- match(links$target, df.an$id)

# Making links dataset
links <- links %>% mutate(source = df.an[idx, "description"], 
                          target = df.an[idy, "description"]) %>%
  group_by(source, target) %>% 
  summarise(value = sum(value)) %>%
  filter(value != 0) %>%
  data.frame(., stringsAsFactors=FALSE)

# Making links as per SankeyNetwork functions
links$IDsource <- match(links$source, df.an$description)-1
links$IDtarget <- match(links$target, df.an$description)-1
links$group <- df.an$description[match(links$source, df.an$description)]
links$group <- as.factor(links$group)
df.an$group <- as.factor(df.an$description)

# Prepare network diagram
p5 <- networkD3::sankeyNetwork(Links = links, Nodes = df.an, 
                               Source = "IDsource",
                               Target = "IDtarget", 
                               Value = "value", 
                               NodeGroup = "group",
                               LinkGroup = "group",
                               sinksRight=TRUE,
                               NodeID = "description", 
                               fontSize = 20, 
                               nodeWidth = 30, 
                               iteration = 0)

#range(["orangered","orangered","orangered","orangered","seagreen","seagreen","seagreen","seagreen","dodgerblue","dodgerblue","dodgerblue","dodgerblue","dodgerblue","dodgerblue","dodgerblue","dodgerblue","grey"])'
saveNetwork(p5, file = "./sankey_desctruction.html", selfcontained = FALSE)
# END OF SCRIPT
sessionInfo()

# END
sessionInfo()
  