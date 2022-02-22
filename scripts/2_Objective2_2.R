# arpankbasak@icloud.com

rm(list=ls())

# list the necessary packages and dependencies
pkgs <- c("tidyverse")
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
path <- as.character(getwd())
setwd(path)

# Read data table
fdata <- read.delim2("./data/year_comparison_date_annotation.txt", sep = "\t", header = TRUE, as.is = TRUE)
# metadata <- read.table("./data/metadata.txt", sep = "\t", header = TRUE, as.is = TRUE)
mat <- read.delim2("./data/year_comparison_dataset.txt", sep = "\t", header = TRUE, as.is = TRUE)
# meta <- read.delim2("./data/metadatafeatures.txt", sep = "\t", header = FALSE, as.is = TRUE)
fdata$ID <- paste0('X', fdata$ID)

# Animals that were observed most
head(fdata)
# head(metadata)
head(mat)

# Data carpenting
fdata <- fdata %>% 
  mutate(statement = as.factor(statement),
         group = as.factor(group)
         )

mat_df <- mat %>% 
  gather(key = "groupID", value = "count", -ID, -YEAR) %>% 
  cbind.data.frame(., fdata[match(.$groupID, fdata$ID), c("group", "statement")]) %>% 
  filter(!is.na(count)) %>% 
  mutate(groupID = as.factor(groupID),
         YEAR = factor(YEAR, levels = c('2010', '2020'))) %>% 
  data.frame(., stringsAsFactors = FALSE)

# Conduct pairwise comparison for each statement
stat_df <- lapply(unique(mat_df$groupID), function(x){
  
  set.seed(1)
  temp <- mat_df %>% filter(groupID == x) %>% 
    select(-group, -statement)
  
  mod <- broom::tidy(lm(count ~ YEAR, data = temp)) %>% 
    filter(term == "YEAR2020") %>% 
    mutate(term = str_replace_all(term, "YEAR", ""),
           groupID = x) %>% 
    data.frame(., stringsAsFactors = FALSE)
  
  return(mod)
  
}) %>% do.call(rbind.data.frame, .) %>% 
  cbind.data.frame(., fdata[match(.$groupID, fdata$ID), c("group", "statement")]) %>% 
  mutate(FDR = p.adjust(p.value, method = 'fdr'),
         significance = FDR <= 0.05
         )

write.table(stat_df, "./output/year_wise_comparison_stats.txt", sep = "\t", 
            row.names = FALSE, quote = FALSE)


# END SCRIPT
sessionInfo()

