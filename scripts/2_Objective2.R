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
                 axis.line.x=element_blank(),
                 axis.line.y=element_blank(),
                 panel.background=element_rect(fill="transparent", colour="black", size = 2),
                 plot.background=element_rect(fill="transparent", colour=NA),
                 strip.background=element_rect(fill="transparent", colour=NA),
                 strip.placement="outside")


# Set path
setwd("~/Desktop/Perception/Data_analysis/")

# list the necessary packages and dependencies
pkgs <- c("tidyverse")
lapply(pkgs, require, character.only = TRUE)

# Read data table
fdata <- read.table("./data/featuredata.txt", sep = "\t", header = TRUE, as.is = TRUE)
metadata <- read.table("./data/metadata.txt", sep = "\t", header = TRUE, as.is = TRUE)
mat <- read.delim2("./data/matrix.txt", sep = "\t", header = TRUE, as.is = TRUE)
meta <- read.delim2("./data/metadatafeatures.txt", sep = "\t", header = FALSE, as.is = TRUE)

# Animals that were observed most
head(fdata)
head(metadata)
head(mat)

idx <- fdata$id[grepl(fdata$question, pattern = "Describe your problem")]

mat_sub <- mat[,c("SampleID", idx)]

df_mat <- mat_sub %>% 
  gather(key = "key", value = "vals", -SampleID, -F127, -F141, -F155) %>% 
  filter(vals != "NA") %>% 
  mutate(key = fdata$level[match(.$key, fdata$id)],
         question = fdata$question[match(.$key, fdata$id)]) %>% 
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

# idy <- str_detect(fdata$question, "Sp 1") | str_detect(fdata$level, "Speci 1")

df_summary$animal <- ""
df_summary$animal[str_detect(df_summary$question, "Sp1")] <- df_summary$F127[str_detect(df_summary$question, "Sp1")]
df_summary$animal[str_detect(df_summary$question, "Sp2")] <- df_summary$F141[str_detect(df_summary$question, "Sp2")]
df_summary$animal[str_detect(df_summary$question, "Sp3")] <- df_summary$F155[str_detect(df_summary$question, "Sp3")]
unique(df_summary$animal)

#Stratified to location
# df_animal <- df_summary %>% select(location, question, animal, key) %>% 
#   filter(animal != "") %>% 
#   group_by(location, animal, key) %>% 
#   summarise(n = n()) %>% 
#   spread(key = key, value = n,fill=0) %>% 
#   mutate(new_id = paste(animal, location, sep = "_")) %>% 
#   data.frame(., row.names = .$new_id)

# Unstratified
df_animal <- df_summary %>% select(SampleID, location, question, animal, key) %>% 
  filter(animal != "") %>% 
  group_by(SampleID, location, animal, key) %>% 
  summarise(n = n()) %>% 
  spread(key = animal, value = n,fill=0) %>% 
  mutate(new_id = paste(SampleID, key, location, sep = "_")) %>% 
  data.frame(., row.names = .$new_id)


# Making input matrix
mat <- as.matrix(df_animal %>% select(-SampleID, -location, -key, -new_id))
mat <- apply(mat, 2, function(x) (x)/sum(x))
dat <- df_animal %>% select(SampleID, location, key, new_id) %>% 
  mutate(key = as.factor(key),
         location = as.factor(replace_na(location, "Unknown")))

d <- 1-cor(t(mat))
# d <- vegan::vegdist(mat, "euclidean")
mds_obj <- cmdscale(as.dist(d), eig=T, k=3)

variance <- round(100*mds_obj$eig/sum(mds_obj$eig), 2)

mds_df <- cbind.data.frame(MDS1 = mds_obj$points[,1], 
                           MDS2 = mds_obj$points[,2], 
                           MDS3 = mds_obj$points[,3], 
                           dat) %>% 
  mutate(key = as.factor(key),
         location = factor(replace_na(location, "Unknown"), levels = c("Unknown", as.character(1:18))))


mds_df %>% ggplot(aes(x=MDS1, y=MDS2, colour = key)) +
  # facet_wrap(.~location, scale = "free") +
  geom_point(size = 3) +
  # stat_ellipse(type = "norm", linetype = "dashed", show.legend = FALSE,  aes(colour = key)) +
  scale_colour_brewer(palette = "Set1") +
  theme_dark() +
  labs(x = paste0("MDS1: ", variance[1], "%"),
       y = paste0("MDS2: ", variance[2], "%"))

cca_obj <- vegan::cca((mat)~key+Condition(location), data = dat, na.action = na.exclude, method = "pearson")
set.seed(1)
anova_obj <- vegan::anova.cca(cca_obj, permutations = 999, parallel = TRUE)
variance <- round(100*cca_obj$CCA$eig/sum(cca_obj$CCA$eig), 2)
pval <- anova_obj$`Pr(>F)`[1]
con_var <- round(100*cca_obj$CCA$tot.chi/cca_obj$tot.chi, 2)
chis <- c(cca_obj$tot.chi, cca_obj$CCA$tot.chi, cca_obj$CA$tot.chi)
variable <- data.frame(inertia = chis, proportion = chis/chis[1], 
                       row.names = c("total", "constrianed", "unconstrained"))

ti <- paste0("(Animals) ~ Problems + Condition(Location), Variance explained = ", con_var, " % ; p.value = ", round(pval, 3), "*")

bi_df <- as.data.frame(cca_obj$CCA$biplot) %>% 
  add_column(key = str_replace_all(row.names(.), "key", ""), .before=1)

v_df <- as.data.frame(cca_obj$CCA$v) %>% 
  add_column(animal = (row.names(.)), .before=1)


cbind.data.frame(dat,
                 cca1 = cca_obj$CCA$wa[,1],
                 cca2 = cca_obj$CCA$wa[,2]) %>%
  mutate(key = as.factor(key),
         location = as.factor(replace_na(location, "Unknown"))) %>%
  ggplot(aes(x = cca1, y = cca2)) +
  ggtitle(ti) +
  geom_hline(yintercept = 0.0, colour = "darkgrey",
             alpha = 0.5,
             lty = "solid", size = 0.8) +
  geom_vline(xintercept = 0.0, colour = "darkgrey",
             alpha = 0.5,
             lty = "solid", size = 0.8) +
  geom_point(alpha = 0.1, colour = "darkgrey", fill = "black", size = 0.2, shape = 22) +
  geom_segment(data = bi_df, aes(xend = CCA1*2.1, yend=CCA2*2.1, group = key), 
               x = 0, y = 0, arrow = arrow(length = unit(0.07, "inches"), type = "closed"), colour = "darkblue", 
               lwd = 0.5, alpha = .4,
               linetype = "solid", inherit.aes = FALSE) +
  geom_text(data = bi_df, aes(x = CCA1*2.3,
                              y = CCA2*2.3, label = as.character(key)), 
            size = 2, alpha = 0.6,
            colour = "darkblue", 
            fontface = "bold", 
            inherit.aes = FALSE) +
  geom_text(data = v_df, aes(x = CCA1*1.2,
                             y = CCA2*1.2, label = as.character(animal)), alpha = 0.85, 
            size = 1.5, colour = "darkred", inherit.aes = FALSE) +
  labs(x = paste0("CCA1 - ", variance[1], " %"),
       y = paste0("CCA2 - ", variance[2], " %"),
       colour = "",
       shape = "") +
  theme_set +
  theme(title = element_text(size = 4),
        legend.text = element_text(size = 4),
        axis.text = element_text(size = 12),
        axis.title = element_text(hjust = 0.5, vjust = 0.5, size = 14)) +
  ggsave(device = "png", 
         file = "./figures/PQ_Obj2.png", 
         units = "in", 
         width = 5, 
         height = 5, 
         dpi = 600,
         bg = "white", limitsize = FALSE)

# Save the straified forms as outputs
# plot(cca_obj, 
#      xlim = c(-4,4),
#      ylim = c(-2,2),
#      type = "n")
# points(cca_obj, pch=21, col="red", bg="yellow", cex=1.2)
# text(cca_obj, dis="cn")
# text(cca_obj, "species", col="blue", cex=0.5)
# cca_obj$CCA$biplot

# END SCRIPT
sessionInfo()

