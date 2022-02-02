# Script by Arpan Kumar Basak
# arpankbasak@gmail.com

# TODO::To show the relationship between animals observed and their behaviour
# TODO::To show the relationship between problemed animals and the nature of problem

rm(list=ls())

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
                 axis.line.x=element_line(colour = "black", size = 1),
                 axis.line.y=element_line(colour = "black", size = 1),
                 panel.background=element_rect(fill="transparent", colour=NA),
                 plot.background=element_rect(fill="transparent", colour=NA),
                 strip.background=element_rect(fill="transparent", colour=NA),
                 strip.placement="outside")
# Set path
setwd("~/Desktop/Perception/Data_analysis/")

# list the necessary packages and dependencies
pkgs <- c("tidyverse")
lapply(pkgs, require, character.only = TRUE)

# Read data table
fdata <- read.delim2("./data/featuredata.txt", sep = "\t", header = TRUE, as.is = TRUE)
metadata <- read.table("./data/metadata.txt", sep = "\t", header = TRUE, as.is = TRUE)
mat <- read.delim2("./data/matrix.txt", sep = "\t", header = TRUE, as.is = TRUE)
meta <- read.delim2("./data/metadatafeatures.txt", sep = "\t", header = FALSE, as.is = TRUE)

# Animals that were observed most
head(fdata)
head(metadata)
head(mat)

idx <- fdata$id[fdata$id %in% c(paste0("F", 187:190))]
idz <- fdata$id[grepl(fdata$objective, pattern = "reverse")]

mat_sub <- mat[,c("SampleID", idx)]
head(mat_sub)
head(meta)

mat <- mat_sub[,-which(colnames(mat_sub) == "SampleID")]
idx <- !is.na(rowSums(mat))
row.names(mat) <- mat_sub$SampleID
mat <- mat[idx, ]
mat[,colnames(mat) %in% idz] <- -1 * mat[,colnames(mat) %in% idz] 

# Calculate Cronbach alpha
cba <- cocron::cronbach.alpha(mat, standardized = FALSE)

# mod <- ' f =~ F187 + F188 +F189 + F190'
# cfa_mod <- lavaan::cfa(model = mod, data = mat, std.lv=TRUE)

# d <- 1-cor(t(mat))
# d <- vegan::vegdist(mat, "manhattan")
# mds_obj <- cmdscale(as.dist(d), eig=T, k=3)
# 
# variance <- round(100*mds_obj$eig/sum(mds_obj$eig), 2)
# 
# mds_df <- metadata %>% 
#   cbind.data.frame(., MDS1 = mds_obj$points[match(.$SampleID, row.names(mds_obj$points)),1],
#                            MDS2 = mds_obj$points[match(.$SampleID, row.names(mds_obj$points)),2],
#                            MDS3 = mds_obj$points[match(.$SampleID, row.names(mds_obj$points)),3]) %>%
#   mutate(key = as.factor(P0))

# mds_df %>% ggplot(aes(x=MDS1, y=MDS2, colour = key)) +
#   # facet_wrap(.~location, scale = "free") +
#   geom_point(size = 3) +
#   # stat_ellipse(type = "norm", linetype = "dashed", show.legend = FALSE,  aes(colour = key)) +
#   scale_colour_brewer(palette = "Set1") +
#   theme_dark() +
#   labs(x = paste0("MDS1: ", variance[1], "%"),
#        y = paste0("MDS2: ", variance[2], "%"))

# Summarise each features
df_mat <- mat_sub %>% gather(key = "key", value = "vals", -SampleID) %>% 
  filter(vals != "NA") %>% 
  mutate(key = fdata$level[match(.$key, fdata$id)],
         question = fdata$question[match(.$key, fdata$id)]
  ) %>% 
  data.frame(.)


df_meta <- metadata %>% gather(key = "key", value = "vals", -SampleID) %>% 
  filter(vals!="NA") %>% 
  cbind.data.frame(., meta[match(.$key, meta$V1),]) %>%
  select(-V1) %>% 
  filter(V2 == "DISTRICTS")

df_summary <- 
  df_mat %>% 
  mutate(location = df_meta[match(.$SampleID, df_meta$SampleID), "V3"]) %>%  
  filter(!key %in% c("comment", "comments", "other")) %>% 
  # group_by(location, F23, F32, F41, key, question) %>% 
  # summarise(n = n()) %>% 
  data.frame(.)

# Unstratified
# df_reason <- df_summary %>% select(SampleID, location, question, key) %>% 
#   # filter(animal != "") %>% 
#   # group_by(SampleID, location, key) %>% 
#   # summarise(n = n()) %>% 
#   spread(key = key, value = vals, fill=0) %>% 
#   mutate(new_id = paste(SampleID, key, location, sep = "_")) %>% 
#   data.frame(., row.names = .$new_id)


# Making input matrix
# mat <- as.matrix(df_animal %>% select(-SampleID, -location, -key, -new_id))
# mat <- apply(mat, 2, function(x) (x)/sum(x))
# dat <- df_animal %>% select(SampleID, location, key, new_id) %>% 
#   mutate(key = as.factor(key),
#          location = as.factor(replace_na(location, "Unknown")))
# 
# d <- 1-cor(t(mat))
# d <- vegan::vegdist(mat, "euclidean")
# mds_obj <- cmdscale(as.dist(d), eig=T, k=3)
# 
# variance <- round(100*mds_obj$eig/sum(mds_obj$eig), 2)
# 
# mds_df <- cbind.data.frame(MDS1 = mds_obj$points[,1], 
#                            MDS2 = mds_obj$points[,2], 
#                            MDS3 = mds_obj$points[,3], 
#                            dat) %>% 
#   mutate(key = as.factor(key),
#          location = factor(replace_na(location, "Unknown"), levels = c("Unknown", as.character(1:18))))
# 
# mds_df %>% ggplot(aes(x=MDS1, y=MDS2, colour = key)) +
#   # facet_wrap(.~location, scale = "free") +
#   geom_point(size = 3) +
#   stat_ellipse(type = "norm", linetype = "dashed", show.legend = FALSE,  aes(colour = key)) +
#   scale_colour_brewer(palette = "Set1") +
#   theme_dark() +
#   labs(x = paste0("MDS1: ", variance[1], "%"),
#        y = paste0("MDS2: ", variance[2], "%"))


df_summary %>%
  mutate(key = as.factor(key)) %>% 
  ggplot(aes(y = vals, x = key)) +
  # geom_point(colour = "black", size = 0.1, alpha = 0.5,
  #            position = "jitter") +
  geom_hline(yintercept = 0, colour = "darkgrey", lty = "solid", lwd = 0.5) +
  geom_violin(colour = "black", trim = TRUE, scale = "count", fill="black", alpha = 0.8) +
  geom_boxplot(colour = "black", outlier.alpha = 0, fill = "white", alpha = 0.9, width = 0.1) +
  labs(x = "",
       y = "",
       colour = "") +
  theme_set +
  theme(axis.text = element_text(size = 8)) +
  coord_flip() +
  ggsave(device = "png", 
         file = "./figures/32_violin_boxplot.png", 
         units = "in", 
         width = 5, 
         height = 3, 
         dpi = 600,
         bg = "white", limitsize = FALSE)

df_summary %>%
  mutate(key = as.factor(key)) %>% 
  ggplot(aes(y = vals, x = key)) +
  # geom_point(colour = "black", size = 0.1, alpha = 0.5,
  #            position = "jitter") +
  geom_hline(yintercept = 0, colour = "darkgrey", lty = "solid", lwd = 0.5) +
  # geom_violin(colour = "black", trim = TRUE, scale = "count", fill="black", alpha = 0.8) +
  geom_boxplot(colour = "black", outlier.alpha = 0, fill = NA) +
  labs(x = "",
       y = "",
       colour = "") +
  theme_set +
  theme(axis.text = element_text(size = 8)) +
  coord_flip() +
  ggsave(device = "png", 
         file = "./figures/32_boxplot.png", 
         units = "in", 
         width = 5, 
         height = 3, 
         dpi = 600,
         bg = "white", limitsize = FALSE)
