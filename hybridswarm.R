## Setup -----

setwd("~/documents/Mandeville_Lab/Hybrid_Swarm/R/Data/")

library(tidyverse)
#library(wordcloud2)
library(RColorBrewer)
library(scales)
library(patchwork)
# install.packages("devtools")
# devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)


## Load Data -----

# Main dataset
df_raw <- read_csv("hybridswarm - Cleaned.csv")
#view(df_raw)

# remove papers that aren't relevant
df_relevant <- df_raw %>% filter(Relevant != "N")
df_hybridswarm <- df_relevant %>% filter(ActiveHybSwarm != "N")

# Author Wordcloud Datasheet
#df_authors <- read_csv("hybridswarm - AuthorFrequency.csv")

## ----- Frequency of Usage Plots -----

#Shows how many papers used the term 'hybrid swarm' each year
(freq_raw <- ggplot(df_relevant, aes(Year)) +
  geom_bar(fill = "#0073C2FF") +
  #theme_pubclean() +
  #geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25) +
  xlab("Year") + ylab("Number of Publications")+ 
   scale_y_continuous(breaks = seq(0, 9, 2), limits = c(0, 9)) )

(freq <- ggplot(df_hybridswarm, aes(Year)) +
    geom_bar(fill = "#0073C2FF") +
    #theme_pubclean() +
    #geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25) +
    xlab("Year") + ylab("Number of Publications")+ 
    scale_y_continuous(breaks = seq(0, 9, 2), limits = c(0, 9)) )


pdf("freq_of_term_usage.pdf", height = 5, width = 10)
  freq_raw + freq + 
  plot_annotation(tag_levels = 'A') 
dev.off()

# COMBINED VERSION

cols <- brewer.pal(8, "Set2")

(freq_combined <- ggplot(df_relevant, aes(x = Year, fill = ActiveHybSwarm)) +
    geom_bar(position="stack", stat="count")+
    xlab("Year") + ylab("Number of Publications")+ 
    scale_fill_manual(values = c("#FC8D62", "#E5C494"), labels = c("No", "Yes"), name = "Active Hybrid\nSwarm?")+
    scale_y_continuous(breaks = seq(0, 9, 2), limits = c(0, 9)) )

pdf("freq_of_term_usage_combined.pdf", height = 7, width = 6)
freq_combined 
dev.off()

## ----- Summary Stats Plots -----

#Shows how many papers used the term 'hybrid swarm' to describe each type of organism
(organism <- ggplot(data = subset(df_hybridswarm, !is.na(OrganismType)), aes(fct_infreq(OrganismType))) +
  geom_bar(fill = "#0073C2FF") +
  #theme_pubclean() +
  geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25) +
  xlab("Organism Type") + ylab("Number of Publications")+ 
    scale_y_continuous(breaks = breaks_pretty()))

#Shows how many papers used the term 'hybrid swarm' to describe each genus
(genus <- ggplot(data = subset(df_hybridswarm, !is.na(Genus)), aes(fct_infreq(Genus))) +
    geom_bar(fill = "#0073C2FF") +
    #theme_pubclean() +
    theme(axis.text.x = element_text(angle = 90)) +
    #geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25)+
    xlab("Genus") + ylab("Number of Publications")+ 
    scale_y_continuous(breaks = breaks_pretty()))



# number of species used
(species <- ggplot(data = df_hybridswarm, aes(x=Nspecies)) + 
    geom_bar(fill = "#0073C2FF") +
    scale_x_continuous(breaks = round(seq(min(df_hybridswarm$Nspecies), max(df_hybridswarm$Nspecies), by = 2),1)) +
    xlab("Number of Species") + ylab("Number of Publications"))


# Number of inds used, one is NA
(inds <- ggplot(data = df_hybridswarm, aes(x=Nind)) + 
    geom_histogram(fill = "#0073C2FF") +
    ylim(0,15) +
    xlab("Number of Individuals") + ylab("Number of Publications"))

# look only at entries w fewer than 2000 inds
subset_hybridswarm2 <- subset(df_hybridswarm, df_hybridswarm$Nind < 2000) # 45 of the original 48 inds
(inds2 <- ggplot(data = subset_hybridswarm2, aes(x=Nind)) + 
    geom_histogram(fill = "#0073C2FF") +
    ylim(0,15) +
    xlab("Number of Individuals (only fewer than 2000)") + ylab("Number of Publications"))


# Number of sampling sites used, 4 are NA
(sites <- ggplot(data = df_hybridswarm, aes(x=Locations)) + 
    geom_histogram(fill = "#0073C2FF") +
    ylim(0,15) +
    xlab("Number of Sampling Sites") + ylab("Number of Publications"))

# look only at entries w fewer than sites
subset_hybridswarm2 <- subset(df_hybridswarm, df_hybridswarm$Locations < 100) # 42 of the original 48 inds
(sites2 <- ggplot(data = subset_hybridswarm2, aes(x=Locations)) + 
    geom_histogram(fill = "#0073C2FF") +
    ylim(0,15) +
    xlab("Number of Sampling Sites (only fewer than 100)") + ylab("Number of Publications"))



# distribution of loci, only 40 used loci
df_hybridswarm$Nloci <- as.numeric(df_hybridswarm$Nloci)
(loci <- ggplot(data = df_hybridswarm, aes(x=Nloci)) + 
    geom_histogram(fill = "#0073C2FF") +
    xlab("Number of loci") + ylab("Number of Publications"))

#get position of the outlier and remove it
a <- max(which(df_hybridswarm$Nloci > 100000))
subset_hybridswarm <- df_hybridswarm[-a,]

# distrib of loci w outlier removed
(loci2 <- ggplot(data = subset_hybridswarm, aes(x=Nloci)) + 
    geom_histogram(fill = "#0073C2FF") +
    xlab("Number of loci w outlier removed") + ylab("Number of Publications"))

# look only at entries w fewer than 500 loci
subset_hybridswarm2 <- subset(df_hybridswarm, df_hybridswarm$Nloci < 500) # 28 of the original 40 inds, so only 12 have more than 5000
(loci3 <- ggplot(data = subset_hybridswarm2, aes(x=Nloci)) + 
    geom_histogram(fill = "#0073C2FF") +
    ylim(0,30) +
    xlab("Number of loci (only fewer than 500)") + ylab("Number of Publications"))


# #Shows the relationship between Nloci and Nind
# (locivsinds <- ggplot(data = subset(df_hybridswarm, !is.na(Nind), !is.na(Nloci)), aes(Nind,Nloci)) +
#   geom_point()+
#   xlab("Number of loci") + ylab("Number of inds"))
# cor.test(df_hybridswarm$Nloci,df_hybridswarm$Nind)

# Plot of SNP usage over time
# proportion of studies
(SNP_prop <- ggplot(data = subset(df_hybridswarm, !is.na(UsedSNPs)), aes(fill=UsedSNPs,x=Year)) + 
  geom_bar(position="fill", stat="count")+
  xlab("Year") + ylab("Proportion of Publications"))

# number of studies
(SNP_count <- ggplot(data = subset(df_hybridswarm, !is.na(UsedSNPs)), aes(fill=UsedSNPs,x=Year)) + 
  geom_bar(position="stack", stat="count")+ 
  scale_y_continuous(breaks = breaks_pretty())+
  xlab("Year") + ylab("Number of Publications")+
  scale_fill_discrete(name = "Used \nSNPs?", labels = c("No", "Yes")))


# Plots of microsat usage over time
# proportion of studies
(micro_prop <- ggplot(data = subset(df_hybridswarm, !is.na(UsedMicrosatellites)), aes(fill=UsedMicrosatellites,x=Year)) + 
  geom_bar(position="fill", stat="count")+
  xlab("Year") + ylab("Proportion of Publications"))

# number of studies
(micro_count <- ggplot(data = subset(df_hybridswarm, !is.na(UsedMicrosatellites)), aes(fill=UsedMicrosatellites,x=Year)) + 
  geom_bar(position="stack", stat="count")+ 
  scale_y_continuous(breaks = breaks_pretty())+
  xlab("Year") + ylab("Number of Publications")+
  scale_fill_discrete(name = "Used \nMicrostats?", labels = c("No", "Yes")))


# Plots of phenotypic data usage over time
# proportion of studies
(pheno_prop <- ggplot(data = subset(df_hybridswarm, !is.na(PhenotypicEvidence)), aes(fill=PhenotypicEvidence, x=Year)) + 
  geom_bar(position="fill", stat="count")+
  xlab("Year") + ylab("Proportion of Publications"))

# number of studies
(pheno_count <- ggplot(data = subset(df_hybridswarm, !is.na(PhenotypicEvidence)), aes(fill=PhenotypicEvidence, x=Year)) + 
  geom_bar(position="stack", stat="count")+ 
  scale_y_continuous(breaks = breaks_pretty())+
  xlab("Year") + ylab("Number of Publications")+
  scale_fill_discrete(name = "Used \nPhenptypic \nEvidence?", labels = c("No", "Yes")))


# Plots of genetic data usage over time
# proportion of studies
(geno_prop <- ggplot(data = subset(df_hybridswarm, !is.na(GeneticEvidence)), aes(fill=GeneticEvidence, x=Year)) + 
  geom_bar(position="fill", stat="count")+
  xlab("Year") + ylab("Proportion of Publications"))

# number of studies
(geno_count <- ggplot(data = subset(df_hybridswarm, !is.na(GeneticEvidence)), aes(fill=GeneticEvidence, x=Year)) + 
  geom_bar(position="stack", stat="count")+ 
  scale_y_continuous(breaks = breaks_pretty())+
  xlab("Year") + ylab("Number of Publications")+
  scale_fill_discrete(name = "Used \nGenetic \nEvidence?", labels = c("No", "Yes")))


# Plots of both phenotypic and genetic data usage over time
# proportion of studies
(both_prop <- ggplot(data = subset(df_hybridswarm, !is.na(GenoAndPhenoEvidence)), aes(fill=GenoAndPhenoEvidence, x=Year)) + 
  geom_bar(position="fill", stat="count") +
  scale_fill_manual(values = c("mediumpurple1", "brown2", "grey", "steelblue1"))+
    xlab("Year") + ylab("Proportion of Publications"))

# number of studies
(both_count <- ggplot(data = subset(df_hybridswarm, !is.na(GenoAndPhenoEvidence)), aes(fill=GenoAndPhenoEvidence, x=Year)) + 
  geom_bar(position="stack", stat="count")+
  scale_fill_manual(values = c("mediumpurple1", "brown2", "grey", "steelblue1"))+
    xlab("Year") + ylab("Number of Publications"))


# Data availability
# proportion of studies
(data_prop <- ggplot(data = subset(df_hybridswarm, !is.na(DataAvailable)), aes(fill=DataAvailable,x=Year)) + 
  geom_bar(position="fill", stat="count")+
  xlab("Year") + ylab("Proportion of Publications")+
  scale_fill_discrete(name = "Data \nAvailable?", labels = c("No", "Yes")))

# number of studies
(data_count <- ggplot(data = subset(df_hybridswarm, !is.na(DataAvailable)), aes(fill=DataAvailable, x=Year)) + 
  geom_bar(position="stack") +
  xlab("Year") + ylab("Number of Publications")+
  scale_fill_discrete(name = "Data \nAvailable?", labels = c("No", "Yes")))
  # geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25)


# Disturbance per taxa
df_disturbance <- df_hybridswarm[!is.na(df_hybridswarm$Disturbance),]
(disturbance_taxa <- ggplot(data=subset(df_disturbance, !is.na(OrganismType)), aes(x=OrganismType, fill=Disturbance)) + 
  geom_bar(stat="count", position=position_dodge(preserve = "single")) +
  labs(y = "Number of Publications", x = "Organism Type")+
  scale_fill_discrete(name = "Disturbance \nPresent?", labels = c("No", "Yes")))
  #geom_text(aes(label=..count..), stat = "count", position=position_dodge(width=0.9), vjust=-0.25))

# disturbance over years
(disturbance_years <- ggplot(data=subset(df_disturbance, !is.na(OrganismType)), aes(x=Year, fill=Disturbance)) + 
  geom_bar(position="stack") +
  labs(y = "Number of Publications", x = "Year")+
  scale_fill_discrete(name = "Disturbance \nPresent?", labels = c("No", "Yes")))


## Author frequency -----

# #More simplistic model
# wordcloud(words = df_authors$Author, freq = df_authors$Frequency, min.freq = 1,
#           max.words=500, random.order=FALSE, rot.per=0.35,
#           colors=brewer.pal(8, "Dark2"))
# 
# #Newer package with more options
# wordcloud2(data = df_authors, size=0.5, color='random-dark')


## ----- Output as a pdf -----

pdf("summary_plots_all.pdf", height = 48, width = 14)
  freq + organism + 
  genus + species +
  inds + inds2 + 
  sites + sites2 +
  loci + loci2 + loci3 +
  SNP_prop + SNP_count + 
  micro_prop + micro_count +
  geno_prop + geno_count +
  pheno_prop + pheno_count +
  both_prop + both_count +
  data_prop + data_count +
  disturbance_taxa + disturbance_years + 
  plot_layout(ncol = 2)+ 
  plot_annotation(tag_levels = 'A')
dev.off()

pdf("summary_plots_numeric.pdf", height = 26, width = 14)
  organism + species +
  inds + inds2 + 
  sites + sites2 +
  loci + loci3 + 
  plot_layout(ncol = 2)+ 
  plot_annotation(tag_levels = 'A') &   
  theme(axis.title = element_text(size = 18), 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14))
dev.off()
  
pdf("summary_plots_YorN.pdf", height = 22, width = 14)  
  SNP_count + micro_count +
  geno_count + pheno_count +
  data_count + disturbance_taxa + 
  plot_layout(ncol = 2)+ 
  plot_annotation(tag_levels = 'A') &   
    theme(axis.title = element_text(size = 18), 
          axis.text.x = element_text(size = 14), 
          axis.text.y = element_text(size = 14))
dev.off()


## ----- Hybrid outcomes flow chart -----

# example 
# df <- mtcars
# df <- mtcars %>%
#   make_long(cyl, vs, am, gear, carb)
# 
# ggplot(df, aes(x = x, 
#                next_x = next_x, 
#                node = node, 
#                next_node = next_node,
#                fill = factor(node))) +
#   geom_sankey() +
#   scale_fill_discrete(drop=FALSE)

# all 67 relevant papers


# # changing genetic and phenotypic to only genetic and only phenotypic to make it explicit
# df_relevant_filled$GenoAndPhenoEvidence <- replace(df_relevant_filled$GenoAndPhenoEvidence,df_relevant_filled$GenoAndPhenoEvidence=="Genetic","Only Genetic")
# df_relevant_filled$GenoAndPhenoEvidence <- replace(df_relevant_filled$GenoAndPhenoEvidence,df_relevant_filled$GenoAndPhenoEvidence=="Phenotypic","Only Phenotypic")
# df_long <- df_relevant_filled %>% make_long(GenoAndPhenoEvidence, ActiveHybSwarm, F1Present, F2orLaterPresent, BackcrossPresent)

# look at SNPs, microsats, and other types of genetic analysis
df_relevant_filled <- df_relevant %>% mutate_at(c(21:30), ~replace_na(.,"N"))
df_relevant_filled$GeneticEvType = NA

for (i in 1:nrow(df_relevant_filled)){
  if (str_detect(df_relevant_filled[i,8],'Genetic') == TRUE | str_detect(df_relevant_filled[i,8],'Both') == TRUE) {
    
    if(str_detect(df_relevant_filled[i,21],'Y') == TRUE){
      df_relevant_filled$GeneticEvType[i] <- "MicroSats" #used microsats
    }
    else if (str_detect(df_relevant_filled[i,22],'Y') == TRUE){
      df_relevant_filled$GeneticEvType[i] <- "SNPs" #uses SNPs
    }
    else {
      df_relevant_filled$GeneticEvType[i] <- "OtherGenetic" #used neither
    }
  }
  else if (str_detect(df_relevant_filled[i,8],'Phenotypic') == TRUE){
    df_relevant_filled$GeneticEvType[i] <- "OnlyPhenotypic" #uses phenotypic
  }
  else {
    df_relevant_filled$GeneticEvType[i] <- "Simulations" #used neither geno nor pheno, only one case & they used simulations
  }
}


df_long <- df_relevant_filled %>% make_long(GeneticEvType, ActiveHybSwarm, F1Present, F2orLaterPresent, BackcrossPresent)


## actual plot
p <- ggplot(df_long, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_alluvial(flow.alpha = .6) +
  #geom_alluvial_label(aes( x = as.numeric(x) + .05, label = after_stat(paste0(node, "\nn = ", freq))), size = 3, color = "black") +
    geom_alluvial_label(size = 3, color = "black") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = NULL, y = "Number of Publications") +
  theme(legend.position = "none", axis.text.y= element_text(size = 10), axis.text.x= element_text(size = 9, color = "black"), axis.title.y =  element_text(size = 10))+
    scale_x_discrete(labels = c("Evidence \nType", "Active Hybrid \nSwarm?", "F1s \nPresent?", "F2 or Later \nGenerations Present?", "Backcrosses \nPresent?"))

# adding labels to flows (https://github.com/davidsjoberg/ggsankey/issues/9)
# get flow size for each flow 
flow_labels <- df_long %>% group_by(x, node, next_x, next_node) %>% tally() %>% drop_na()
# get corresponding positions of flows from the sankey plot
flow_info <- layer_data(p) %>% select(xmax, flow_start_ymax, flow_start_ymin) %>% distinct() # get flow related key positions related from the plot
flow_info <- flow_info[with(flow_info, order(xmax, flow_start_ymax)), ] # order the positions to match the order of flow_labels
rownames(flow_info) <- NULL # drop the row indexes
flow_info <- cbind(as.data.frame(flow_info), as.data.frame(flow_labels)) # bind the flow positions and the corresponding labels
# add labels to the flows
for (i in 1:nrow(flow_info)){
  if (flow_info[i,5] == "Neither"){ # custom adjust hjust  
    p <- p + annotate("text", x = flow_info$xmax[i],
                      y = (flow_info$flow_start_ymin[i] + flow_info$flow_start_ymax[i])/2,
                      label = sprintf("%d", flow_info$n[i]), hjust = -3, size = 3)
    next
  }
  else if (flow_info[i,5] == "Simulations" & flow_info[i,8] == 1 ){ # custom adjust vjust  
    p <- p + annotate("text", x = flow_info$xmax[i],
                      y = (flow_info$flow_start_ymin[i] + flow_info$flow_start_ymax[i])/2,
                      label = sprintf("%d", flow_info$n[i]), hjust = -5, vjust = 0.25, size = 3)
    next
  }
  else if (flow_info[i,5] == "OtherGenetic" & flow_info[i,8] == 8 ){ # custom adjust vjust  
    p <- p + annotate("text", x = flow_info$xmax[i],
                      y = (flow_info$flow_start_ymin[i] + flow_info$flow_start_ymax[i])/2,
                      label = sprintf("%d", flow_info$n[i]), hjust = -1, vjust = -0.5, size = 3)
    next
  }
  else if (flow_info[i,5] == "OnlyPhenotypic" & flow_info[i,8] == 6 ){ # custom adjust vjust  
    p <- p + annotate("text", x = flow_info$xmax[i],
                      y = (flow_info$flow_start_ymin[i] + flow_info$flow_start_ymax[i])/2,
                      label = sprintf("%d", flow_info$n[i]), hjust = -1, vjust = -0.5, size = 3)
    next
  }
  else if (flow_info[i,5] == "MicroSats" & flow_info[i,8] == 20 ){ # custom adjust vjust  
    p <- p + annotate("text", x = flow_info$xmax[i],
                      y = (flow_info$flow_start_ymin[i] + flow_info$flow_start_ymax[i])/2,
                      label = sprintf("%d", flow_info$n[i]), hjust = -1, vjust = -1, size = 3)
    next
  }
  p <- p + annotate("text", x = flow_info$xmax[i],
                    y = (flow_info$flow_start_ymin[i] + flow_info$flow_start_ymax[i])/2,
                    label = sprintf("%d", flow_info$n[i]), hjust = -1, size = 3)
}

p

# pdf("sankey_plot.pdf")  
# p
# dev.off()



#------------------------------------------
#OR!!! only 49 active hybrid swarm papers

df_hybridswarm_filled <- df_relevant_filled %>% filter(ActiveHybSwarm != "N")

df_long <- df_hybridswarm_filled %>% make_long(GeneticEvType, F1Present, F2orLaterPresent, BackcrossPresent)

cols <- brewer.pal(8, "Set2")
cols <- cols[-5]

## actual plot
p2 <- ggplot(df_long, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_alluvial(flow.alpha = .6) +
  #geom_alluvial_label(aes( x = as.numeric(x) + .05, label = after_stat(paste0(node, "\nn = ", freq))), size = 3, color = "black") +
  geom_alluvial_label(size = 3, color = "black") +
  scale_fill_manual(values = cols) +
  labs(x = NULL, y = "Number of Publications") +
  theme(legend.position = "none", axis.text.y= element_text(size = 10), axis.text.x= element_text(size = 9, color = "black"), axis.title.y =  element_text(size = 10))+
  scale_x_discrete(labels = c("Evidence \nType", "F1s \nPresent?", "F2 or Later \nGenerations Present?", "Backcrosses \nPresent?"))

# adding labels to flows (https://github.com/davidsjoberg/ggsankey/issues/9)
# get flow size for each flow 
flow_labels <- df_long %>% group_by(x, node, next_x, next_node) %>% tally() %>% drop_na()
# get corresponding positions of flows from the sankey plot
flow_info <- layer_data(p2) %>% select(xmax, flow_start_ymax, flow_start_ymin) %>% distinct() # get flow related key positions related from the plot
flow_info <- flow_info[with(flow_info, order(xmax, flow_start_ymax)), ] # order the positions to match the order of flow_labels
rownames(flow_info) <- NULL # drop the row indexes
flow_info <- cbind(as.data.frame(flow_info), as.data.frame(flow_labels)) # bind the flow positions and the corresponding labels

# add labels to the flows
for (i in 1:nrow(flow_info)){
  if (flow_info[i,5] == "Simulations" & flow_info[i,8] == 1 ){ # custom adjust vjust  
    p2 <- p2 + annotate("text", x = flow_info$xmax[i],
                      y = (flow_info$flow_start_ymin[i] + flow_info$flow_start_ymax[i])/2,
                      label = sprintf("%d", flow_info$n[i]), hjust = -5, vjust = 0.25, size = 3)
    next
  }
  else if (flow_info[i,5] == "OtherGenetic" & flow_info[i,8] == 8 ){ # custom adjust vjust  
    p2 <- p2 + annotate("text", x = flow_info$xmax[i],
                        y = (flow_info$flow_start_ymin[i] + flow_info$flow_start_ymax[i])/2,
                        label = sprintf("%d", flow_info$n[i]), hjust = -6, vjust = 0, size = 3)
    next
  }
  else if (flow_info[i,5] == "OnlyPhenotypic" & flow_info[i,8] == 5 ){ # custom adjust vjust  
    p2 <- p2 + annotate("text", x = flow_info$xmax[i],
                        y = (flow_info$flow_start_ymin[i] + flow_info$flow_start_ymax[i])/2,
                        label = sprintf("%d", flow_info$n[i]), hjust = -2, vjust = -0.75, size = 3)
    next
  }
  p2 <- p2 + annotate("text", x = flow_info$xmax[i],
                    y = (flow_info$flow_start_ymin[i] + flow_info$flow_start_ymax[i])/2,
                    label = sprintf("%d", flow_info$n[i]), hjust = -1, size = 3)
}

p2



# pdf("sankey_plot_hybswrmonly.pdf")  
# p2
# dev.off()

# save both
pdf("sankey_plot_both_new.pdf", height = 10)  
  p + p2 + 
  plot_layout(ncol = 1)+ 
  plot_annotation(tag_levels = 'A')
dev.off()


## Testing out example plots
# ggplot(df_long, aes(x = x, 
#                next_x = next_x, 
#                node = node, 
#                next_node = next_node,
#                fill = factor(node))) +
#   geom_sankey() +
#   scale_fill_discrete(drop=FALSE)
# 
# ggplot(df_long, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
#   geom_sankey(flow.alpha = .6,
#               node.color = "gray30") +
#   geom_sankey_label(size = 3, color = "white", fill = "gray40") +
#   scale_fill_viridis_d(drop = FALSE) +
#   theme_sankey(base_size = 18) +
#   labs(x = NULL) +
#   theme(legend.position = "none",
#         plot.title = element_text(hjust = .5)) +
#   ggtitle("Hybrid swarm outcomes")


