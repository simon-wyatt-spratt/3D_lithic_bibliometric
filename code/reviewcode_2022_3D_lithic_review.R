### ARTICLE TITLE: After the revolution: a review of 3D modelling as a tool for stone artefact analysis ###
### AUTHOR: ### ###
### JOURNAL: Journal of Computer Applications in Archaeology
### SCRIPT AUTHOR: ### ###
### SCRIPT CONTACT: ### ###
### LAST EDITED: 31/08/2022 ###

### ABSTRACT ###
### With over 200 peer-reviewed papers published over the last 20 years, 3D modelling is no longer a gimmick but an 
### established and increasingly common analytical tool for stone artefact analysis. Laser and structured light 
### scanning, photogrammetry, and CT scanning have all been used to model stone artefacts. These have been combined 
### with a variety of different analytical approaches, from geometric morphometrics to custom reduction indices to 
### digital elevation maps. 3D lithic analyses are increasingly global in scope and studies aim to address an ever-
### broadening breadth of research topics ranging from testing the functional efficiency of artefacts to assessing the
### cognitive capabilities of hominid populations. While the impact of the computational revolution on lithic analysis
### has been reviewed, the impact of 3D modelling on lithic analysis has yet to be comprehensively assessed. This 
### paper presents a review of how 3D modelling in particular has impacted the field of stone artefact analysis. It 
### combines a quantitative bibliometric analysis with a qualitative review to assess just how “revolutionary” 3D 
### modelling has been for lithic analysis. It explores trends in the use of 3D modelling in stone artefact analysis,
### its impact  on the wider lithic analysis field, and methodological, regional and theoretical gaps which future 
### research projects could explore.

### SYSTEM INFORMATION ###
### R version 4.2.0 (2022-04-22)
### Platform: x86_64-w64-mingw32/x64 (64-bit)
### Running under: Windows 10 x64 (build 19042.928)

### ATTACHED BASE PACKAGES:
### [1] stats     graphics  grDevices utils     datasets 
### [6] methods   base  

### SCRIPT ###

### KNOWN ISSUES ###
# None.

### INSTALL AND ACTIVIATE PACKAGES ###
if(!require("bibliometrix")) install.packages("bibliometrix", repos = "http://cran.us.r-project.org") # bibliometrix 4.0.0
if(!require("cowplot")) install.packages('cowplot', repos='http://cran.us.r-project.org') # cowplot 1.1.1
if(!require("igraph")) install.packages('igraph', repos='http://cran.us.r-project.org') # igraph 1.3.1
if(!require("qgraph")) install.packages('qgraph', repos='http://cran.us.r-project.org') # qgraph 1.9.2
if(!require("tidyverse")) install.packages("tidyverse", repos = "http://cran.us.r-project.org") # tidyverse 1.3.2
if(!require("viridis")) install.packages("viridis", repos = "http://cran.us.r-project.org") # viridis 0.6.2

library(bibliometrix) # load bibliometrix package
library(cowplot)      # load cowplot package
library(igraph)       # load igraph package
library(qgraph)       # load qgraph package
library(tidyverse)    # load tidyverse package
library(viridis)      # load viridis package

### SET WORKING DIRECTORY ### - update to github for the final code
getwd() # Show working directory

### IMPORT DATA ###

### WEB OF SCIENCE ORIGINAL PLAINTEXT ####
dataset.wos <- convert2df('data/wos_3D_lithic_export_unedited.txt', dbsource = 'wos', format = "plaintext") # will need to change file name
head(dataset.wos["TI"])

### MODIFIED AND ADDITIONAL *.CSV DATA ###
dataset.csv <- read.csv("data/3D-lithics-bibliometrix-edited-final.csv", na.strings = "")
head(dataset.csv["TI"])

### EXTRACT MANUALLY ADDED COLUMNS FROM *.CSV
dataset.add <- select(dataset.csv,
                      "TI",      # Article title - to be used to match with other dataset.wos
                      "FAAC",    # Manually added the country of the first author's affiliation
                      "AAAC",	   # Manually added the countries of all authors' affiliations 
                      "MUS",	   # Model use i.e. reduction strategy, functional, techno-morphological etc.
                      "AOI",     # Model use simplified - illustrative or analytical
                      "ROS",	   # Region of study
                      "COS",	   # Country of study
                      "STS",     # Site of study
                      "AGE",	   # Age(s) of assemblage(s) analysed
                      "IND",	   # Assemblage industry e.g. Acheulean, Oldowan, Levallois
                      "MME",     # Modelling methodology i.e. CT scanning, photogrammetry, laser/light etc.
                      "ASO",     # Software used for analysing the 3D model
                      "NOM",	   # Number of 3D models used in the study
                      "ART",     # Artefact type e.g. core, flake, retouched flake, biface, point etc.
                      "CA1",     # Category 1 - includes all categories of manually keywords 
                      "CA2",     # Category 2 - keyword categories excluding country, site, and age
                      "CA3")     # Category 3 - keyword categories excluding region, country, site and age
head(dataset.add["CA1"])

dataset.com1 <- full_join(dataset.wos, dataset.add, by = "TI") # combines the Web of Science dataset to the manually modified dataset using their titles. 

dataset.csv$BP <- as.character(dataset.csv$BP) # Converts BP column to the correct format for merging
dataset.csv$IS <- as.character(dataset.csv$IS) # Converts IS column to the correct format for merging
dataset.csv$NR <- as.character(dataset.csv$NR) # Converts NR column to the correct format for merging
dataset.csv$PG <- as.character(dataset.csv$PG) # Converts PG column to the correct format for merging
dataset.csv$PM <- as.character(dataset.csv$PM) # Converts PM column to the correct format for merging
dataset.csv$U1 <- as.character(dataset.csv$U1) # Converts U1 column to the correct format for merging
dataset.csv$U2 <- as.character(dataset.csv$U2) # Converts U2 column to the correct format for merging
dataset.csv$VL <- as.character(dataset.csv$VL) # Converts VL column to the correct format for merging
dataset.csv$Z9 <- as.character(dataset.csv$Z9) # Converts Z9 column to the correct format for merging

dataset.com2 <- right_join(dataset.com1, dataset.csv, copy = TRUE, keep = FALSE) # adds manually added and cleaned  bibliographic data

### CLEANED DATA
# "AU",      # Authors - cleaned data to remove inaccuracies or incorrect duplicates e.g. PORTER S = PORTER ST
# "DE",      # Authors' keywords - cleaned data to combine relevant keywords e.g. ACHEULIAN = ACHEULEAN
# "ID",      # Web of Science keywords - cleaned data to combine relevant keywords e.g. ACHEULIAN = ACHEULEAN
# "PY",      # Published year - cleaned data - added missing publication years
# "TC",      # Times Cited - adds info for papers not found on Web of Science - data sourced from Google Scholar correct as of 26/05/2022

dataset.com3 <- filter(dataset.com2, AU != "") # removes 3 x blank rows created in the merging process
head(dataset.com2)

dataset.ana <- filter(dataset.com3, AOI != "ILLUSTRATIVE") # creates a dataset that exclusively has analytical papers
dataset.ill <- filter(dataset.com3, AOI != "ANALYTICAL") # creates a dataset that exclusively has illustrative papers

### CREATE A BIBLIOMETRIX DATAFRAME
biblio.com <- biblioAnalysis(dataset.com3, sep = ";") # creates a Bibliometrix dataframe for the entire dataset
biblio.ana <- biblioAnalysis(dataset.ana, sep = ";")  # creates a Bibliometrix dataframe for the analytical dataset
biblio.ill <- biblioAnalysis(dataset.ill, sep = ";")  # creates a Bibliometrix dataframe for the illustrative dataset

### ANALYSIS ###

### GENERIC BIBLIOMETRIX DATA ANALYSIS

### SUMMARY OF DATASET
# Provides written summary of the bibliometric datasets.

options(width=100)
summary(object = biblio.com, k = 100, pause = FALSE)  # bibliometric summary of the full dataset
summary(object = biblio.ill, k = 100, pause = FALSE)  # bibliometric summary of the illustrative dataset
summary(object = biblio.ana, k = 100, pause = FALSE) # bibliometric summary of the analytical dataset

### PLOTS OF SUMMARIES OF DATASETS ###

plot(x = biblio.com, k = 10, pause = FALSE) # plots of summary of combined dataset - NOTE PRODUCES MULTIPLE PLOTS
plot(x = biblio.ill, k = 10, pause = FALSE) # plots of summary of illustrative dataset - NOTE PRODUCES MULTIPLE PLOTS
plot(x = biblio.ana, k = 10, pause = FALSE) # plots of summary of analytical dataset - NOTE PRODUCES MULTIPLE PLOTS

### FIGURE 1. ###
# Figure 1. Annual number of published peer-reviewed papers where 3D modelling is used as either an analytical, 
# archival or pedagogical tool or illustrative technique for lithic analysis. Papers published in 2022 have been 
# excluded.

published.data1         <- select(dataset.com3, "PY", "AOI")
published.count1        <- published.data1 %>% count(PY, AOI)
published.count2        <- rename(published.count1, AOI_count = n)
published.count3        <- published.count2 %>% group_by(PY)
published.count4        <- filter(published.count3, PY != "2022") # removes 2022 from the count as the year is still in progress
total.published.count1  <- dataset.com3 %>% count(PY) # count overall number of publications per yer
total.published.count2  <- rename(total.published.count1, PY_count = n)
total.published.count3  <- filter(total.published.count2, PY != "2022") # removes 2022 from the count as the year is still in progress
tp.missing              <- data.frame (PY  = c(2003, 2004, 2005, 2006, 2007, 2009)) # creates a dataframe with the missing years
total.published.count4  <- full_join(total.published.count3, tp.missing, by = NULL)

figure_1a <- ggplot()
figure_1b <- figure_1a + geom_col(data = published.count4, 
             aes(x = PY, y = AOI_count, fill = AOI), 
             width = 0.8, 
             position = position_dodge(preserve = "single"))
figure_1  <- figure_1b + geom_line(data = total.published.count4, 
             aes(x = PY, y = PY_count),
             na.rm = FALSE,
             show.legend = TRUE,
             size = 0.25) +
             guides(fill = guide_legend(override.aes = list(linetype = 0))) +
             theme_minimal() +
             labs(x = "YEAR PUBLISHED", y = "NO. PUBLICATIONS", fill = "MODEL USE", size = 6) +
             scale_x_continuous(limits = c(2001, 2022),
             breaks = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 
                        2017, 2018, 2019, 2020, 2021)) +
             scale_color_manual(values = c("PY_count" = "black"), 
                                labels = c("PY_count" = "TOTAL PUB. PER YEAR")) +
             scale_fill_grey() +
             theme(axis.title.x = element_text(size = 6),
                   axis.title.y = element_text(size = 6),
                   axis.text.x = element_text(angle = 90, size = 5, vjust=0.8),
                   axis.text.y = element_text(size = 5),
                   panel.grid.minor = element_blank())
figure_1

ggsave2("results/Wyatt-Spratt_2022_Figure_1.tiff", 
        plot = figure_1, scale = 1.0, width = 168.275, height = 84.1375, units = "mm", dpi = 300)

### FIGURE 2. ###
# Figure 2. Top 10 journals by number of publications. Only core and zone 2 journals are present in the top 10 
# journals.

bradford.ana <- bradford(dataset.ana) # Creates table and figure of publication sources per Bradford's Law.
bradford.ana

sources.table1 <-  as.data.frame(bradford.ana$table)
sources.table2 <-  arrange(sources.table1, desc(-Rank))
sources.table3 <-  slice_head(sources.table2, n = 10)

sources.table3

figure_2 <- ggplot(
  data = sources.table3,
  aes(x = Freq, y = reorder(SO, Freq), fill = Zone)) +
  geom_col(width = 0.9) +
  theme_minimal() +
  labs(x = "NO. PUBLICATIONS", y = "ARTICLES", fill = "ZONE", size = 12) +
  scale_fill_viridis(discrete = TRUE, labels=c("CORE", "ZONE 2", "ZONE 3")) +
  theme(axis.text.x = element_text(angle = 0, size = 7),
        panel.grid.minor = element_blank())
figure_2

ggsave2("results/Wyatt-Spratt_2022_Figure_2.tiff",
        plot = figure_2, scale = 1.2, width = 168.275, height = 84.1375, units = "mm", dpi = 300)

### FIGURE 3. ###
# Figure 3. Collaboration network of all authors who have published ≥3 articles (n = 57). A Fruchterman-Reingold 
# layout and the Louvain clustering algorithm were used to produce the network. The community repulsion force was 0.5
# and the minimum number of edges was 1. Isolated nodes were kept.

# NOTE: Cluster analysis is iterative. This means that there are variations when the code is run in how authors
# cluster. These are generally minor. To run different iterations remove "set.seed(1)", and you can view different 
# iterations of the cluster.

set.seed(1)

netmatrix.auth.coll <- biblioNetwork(dataset.ana, 
                                     analysis = "collaboration", 
                                     network = "authors", 
                                     n = 57, # the number of authors who have published ≥3 articles
                                     sep = ";")

auth.coll <- networkPlot(netmatrix.auth.coll, 
                       normalize = "association",
                       Title = "Co-Citation Network", 
                       type = "auto", # network map layout
                       size = T, 
                       cluster = "louvain", # clustering algorithm
                       remove.isolates = FALSE, # isolated vertices are removed from the plot
                       remove.multiple = FALSE,
                       community.repulsion = 0.5,
                       edgesize = 5,
                       edges.min = 1, # indicates the min frequency of edges between two vertices.
                       label.cex = TRUE, # the label size of each vertex is proportional to its degree
                       label.n = 120, # number of labels
                       labelsize = 0.2)
au.coll.plot   <- auth.coll$graph
au.cluster.res <- auth.coll$cluster_res

au.cluster.res # 12 clusters identified in the analysis

ac_colrs    <- turbo(12) # creates turbo palette for the 13 clusters
ac_edgelist <- get.edgelist(au.coll.plot, names = FALSE)
ac_layout   <- qgraph.layout.fruchtermanreingold(ac_edgelist,
                                       vcount = vcount(au.coll.plot),
                                       area = 8*(vcount(au.coll.plot)^2),
                                       repulse.rad = (vcount(au.coll.plot)^3.1))
V(au.coll.plot)$color <- ac_colrs[V(au.coll.plot)$community]

tiff(file = "results/Wyatt-Spratt_2022_Figure_3.tiff", width = 168.275, height = 168.275, units = "mm", res = 300)

plot(au.coll.plot,
     vertex.color = ac_colrs[V(au.coll.plot)$community], # vertex colour turbo (n=17) palette
     vertex.frame.color = "white", # vertex frame colour
     vertex.label.font = 1, # vertex label style
     vertex.size = 4.5, # vertex size
     vertex.label.cex = 0.7, # vertex label size
     vertex.label.family = "sans", # vertex font type
     edge.color = "gray80", # edge colour
     edge.lty = 1, # edge line type
     layout = ac_layout) 
legend(x = -1.3, y = -0.5,
       c("CLUSTER 1","CLUSTER 2", "CLUSTER 3", "CLUSTER 4", "CLUSTER 5", "CLUSTER 6", "CLUSTER 7", "CLUSTER 8",
         "CLUSTER 9", "CLUSTER 10", "CLUSTER 11", "CLUSTER 12"), 
       pch = 21,
       col = "#777777", 
       pt.bg = ac_colrs,
       pt.cex = 1.6, 
       cex = .65, 
       bty = "n", 
       ncol = 1)

dev.off()

### FIGURE 4. ###
# Figure 4. Top 10 Authors’ Production over time. The number of articles published in a year is indicated by the size 
# of the bubble. The colour intensity is proportional to the number of times articles published in that year have been
# cited. The line represents an author's publication timeline.

au.top.10 <- authorProdOverTime(dataset.ana, k = 10, graph = TRUE)
figure_4  <- au.top.10$graph

ggsave2("results/Wyatt-Spratt_2022_Figure_4.tiff",
        plot = figure_4, scale = 1.2, width = 168.275, height = 84.1375, units = "mm", dpi = 300)

### FIGURE 5. ###
# Figure 5. Historical Direct Citation Network of the 20 most influential papers by number of local citations, i.e. 
# papers that are cited by papers within the dataset.

histResults <- histNetwork(dataset.ana, min.citations = 1, sep = ";", network = TRUE)

histplot.ana1 <- histPlot(histResults, 
                          n = 23, 
                          size = 2,
                          labelsize = 2
                          )
figure_5 <- histplot.ana1$g

ggsave2("results/Wyatt-Spratt_2022_Figure_5.tiff", 
        plot = figure_5, scale = 1, width = 168.275, height = 125, units = "mm", dpi = 300)

### FIGURE 6. ###
# Figure 6. Papers published per year by modelling methodology. 3D scanning includes both laser and hard light 
# scanning. Papers where the modelling method was ambiguous or not recorded were excluded. Figure produced from the 
# analytical dataset.

modelling.data1         <- select(dataset.ana, "PY", "MME")
modelling.data2         <- separate_rows(modelling.data1,MME,sep=";\\s+")
modelling.string        <- paste(modelling.data2$MME) # paste cells into one string
modelling.data3         <- filter(modelling.data2, modelling.string != "UNREPORTED") # filters out "UNREPORTED"
modelling.count1        <- modelling.data3 %>% count(PY, MME)
modelling.count2        <- filter(modelling.count1, PY != "2022") # removes 2022 from the count as the year is still in progress
modelling.count3        <- rename(modelling.count2, mod_count = n)

figure_6 <- ggplot(
  data = modelling.count3,
  aes(x = PY, y = mod_count, fill = MME)) +
  geom_col(width = 0.9, position = position_dodge(preserve = "single")) +
  theme_minimal() +
  labs(x = "YEAR PUBLISHED", y = "NO. PUBLICATIONS", fill = "MODELLING METHOD", size = 12) +
  scale_fill_viridis(discrete = TRUE, labels=c("CT SCANNING", "3D SCANNING", "PHOTOGRAMMETRY")) +
  scale_x_continuous(limits = c(2001, 2022), 
                     breaks = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 
                                2016, 2017, 2018, 2019, 2020, 2021)) +
  theme(axis.title.x = element_text(size = 8),
        axis.text.x = element_text(angle = 90, size = 6, vjust = 0.5 ),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 7),
        panel.grid.minor = element_blank())
figure_6

ggsave2("results/Wyatt-Spratt_2022_Figure_6.tiff", 
        plot = figure_6, scale = 1.2, width = 168.275, height = 105, units = "mm", dpi = 300)

### FIGURE 7. ###
# Figure 7. Trends in analytical software over time. Word minimum frequency was set to 5, number of words per year 
# was set to 5. The category “n/a” - which  refers to studies where the 3D models were used for 3D printing, archival,
# diagnostic or pedagogical purposes - was excluded.

software.data   <- filter(dataset.ana, ASO != "N/A")
software.trend  <- fieldByYear(software.data, 
                               field = "ASO", 
                               min.freq = 5, 
                               n.items = 5, 
                               graph = TRUE)
soft.trend.plot <- software.trend$graph
figure_7        <- soft.trend.plot + 
                   ggtitle("Trends in Analytical Software")
figure_7

ggsave2("results/Wyatt-Spratt_2022_Figure_7.tiff", 
        plot = figure_7, scale = 1.2, width = 168.275, height = 125, units = "mm", dpi = 300)

### FIGURE 8. ###
# Figure 8. Region of Study by number of publications. Note that studies could be multi-regional and could include 
# both an experimental and archaeological component.

regions.string        <- paste(dataset.ana$ROS, collapse = ";") # paste cells into one string, use ";" as separator
regions.vector        <- strsplit(regions.string, ";")[[1]] # split string at ";"
regions.vector.clean1 <- gsub(" " ,"", regions.vector) # get rid of space between words to prevent errors
regions.vector.clean2 <- gsub("WESTASIA","WEST ASIA",
                         gsub("EASTAFRICA", "EAST AFRICA",
                         gsub("SOUTHERNAFRICA", "SOUTHERN AFRICA",
                         gsub("NORTHAMERICA", "NORTH AMERICA",
                         gsub("NORTHAFRICA", "NORTH AFRICA",
                         gsub("SOUTHASIA", "SOUTH ASIA",
                         gsub("SOUTH-EASTASIA", "SOUTH-EAST ASIA",
                         gsub("EASTASIA", "EAST ASIA",
                         gsub("SOUTHAMERICA", "SOUTH AMERICA",
                         gsub("WESTAFRICA", "WEST AFRICA",
                         gsub("CENTRALASIA", "CENTRAL ASIA", regions.vector.clean1))))))))))) # adds spaces back in 
regions.table         <- tibble(regions.vector.clean2)
regions.count1        <- count(regions.table, regions.vector.clean2, wt = NULL, sort = FALSE, name = NULL)
regions.count2        <- rename(regions.count1, reg_count = n)

figure_8 <- ggplot(
  data = regions.count2,
  aes(x = fct_reorder(regions.vector.clean2, reg_count), y = reg_count)) +
  geom_col(width = 0.5) +
  coord_flip() + # makes bar plot horizontal
  geom_text(aes(label = reg_count), hjust = 1.6, vjust = 0.45, color = "white", size = 3.4) + #labels need adjusting
  theme_minimal() +
  labs(x = "REGION OF STUDY", y = "NO. PUBLICATIONS", size = 12)
figure_8

ggsave2("results/Wyatt-Spratt_2022_Figure_8.tiff", 
        plot = figure_8, scale = 1.2, width = 168.275, height = 84.137, units = "mm", dpi = 300)

### FIGURE 9. ###
# Figure 9. Co-occurrence Network Map of the dataset. The layout algorithm was generated with the “layout_nicely” 
# function in igraph and a Louvain clustering algorithm was used to produce the network. The community repulsion 
# force was 0.5 and the minimum number of edges was 2. Isolated nodes were removed. The size of the node reflects how
# many times the keyword occurs.

# NOTE: Bibliometrix doesn't allow you to specify which column you wish to network, it just provides a series of 
# preset choices. As such, a new dataframe was created from the analytical dataset, where the WOS keywords were
# replaced by the manually added categories. 

# Cluster analysis is iterative. This means that there are variations when the code is run in how keywords
# cluster. These are generally minor, with the only significant difference between different iterations is that
# occasionally "experimental" and "functional" are sometimes clustered together. To run different iterations remove
# "set.seed(6)", and you can view different iterations of the cluster.

dataset.ca3       <- dataset.ana         # creates a dataset called "dataset.ca3" from dataset.ana
dataset.ca3['ID'] <- dataset.ca3['CA3']  # replaces the WOS' keywords with the manually added keywords 
head(dataset.ca3["ID"])

biblio.ca3 <- biblioAnalysis(dataset.ca3, sep = ";")  # creates a Bibliometrix dataframe for dataset.ca3

# The keyword co-occurence can now be networked as per normal 

set.seed(6) 
netmatrix.keyword <- biblioNetwork(dataset.ca3, analysis = "co-occurrences", network = "keywords", sep = ";")
netmatrix.keyword
key.cooc <- networkPlot(netmatrix.keyword, 
                        n = 150, # number of nodes. All keywords were included
                        normalize = "association",
                        Title = "", 
                        type = "auto", # network map layout
                        size = T, 
                        cluster = "louvain", # clustering algorithm
                        remove.isolates = TRUE, # isolated vertices are removed from the plot
                        remove.multiple = FALSE,
                        community.repulsion = 0.5,
                        edgesize = 5,
                        edges.min = 2, # indicates the min frequency of edges between two vertices.
                        label.cex = TRUE, # the label size of each vertex is proportional to its degree
                        label.n = 200, # number of labels
                        labelsize = 0.2)
keyword.plot    <- key.cooc$graph
key.cluster.res <- key.cooc$cluster_res

key.cluster.res # 6 clusters identified in the analysis

kw_colrs  <- viridis(6, direction = -1, alpha = 0.75) # creates viridis palette for the 6 clusters
kw_e      <- get.edgelist(key.cooc$graph, names = FALSE)
kw_layout <- layout_nicely(key.cooc$graph)
V(key.cooc$graph)$color <- kw_colrs[V(key.cooc$graph)$community]

tiff(file = "results/Wyatt-Spratt_2022_Figure_9.tiff", width = 250, height = 150, units = "mm", res = 300)
par(mar=c(0,0,0,0)+.2) # reduces the margins of the plot

plot(key.cooc$graph,
     vertex.color = kw_colrs[V(key.cooc$graph)$community], # vertex colour viridis (n=6) palette
     vertex.frame.color = "white", # vertex frame colour
     vertex.label.font = 1, # vertex label style
     vertex.label.family = "sans", # vertex font type
     vertex.label.dist = 0,
     vertex.label.cex = 0.4,
     edge.color = "gray80", # edge colour
     edge.lty = 1, # edge line type
     layout = kw_layout) 
title("Keyword Co-occurence Network", line = -2)
legend(x = 0.9, y = -0.7,
       c("CLUSTER 1","CLUSTER 2", "CLUSTER 3", "CLUSTER 4", "CLUSTER 5", "CLUSTER 6"), 
       pch = 21,
       col = "#777777", 
       pt.bg = kw_colrs,
       pt.cex = 2, 
       cex = .8, 
       bty = "n", 
       ncol = 1)
dev.off()

### TABLE 1. ###
# Table 1. The top 10 keywords in the four keyword categories. Note, the numbers in the artefact category do not 
# indicate how prominent or how many artefacts were included in a given study, merely whether they had been modelled
# and analysed.

# NOTE: The summary is of the new dataframe that was created from the analytical dataset, where the WOS keywords
# were replaced by the manually added categories. See Supplementary Information for full list of keywords and
# categories.

kw.ca3 <- summary(object = biblio.ca3, k = 200, pause = FALSE) # Data for Table 1. was extracted from this summary data. 
write.csv(kw.ca3$MostRelKeywords, "results/Wyatt-Spratt_2022_Table_1.csv", row.names = TRUE) 

### TABLE 2. ###
# Table 2. List of the 5 most common keywords in each cluster. Cluster 6 has been excluded as it only contained three
# keywords.

key.cluster.res # Data for Table 2. was produced from this summary.
write.csv(key.cluster.res, "results/Wyatt-Spratt_2022_Table_2.csv", row.names = TRUE)

###########################
### OTHER DATA ANALYSIS ###
###########################

### MOST PRODUCTIVE COUNTRIES ###
kw.ca3$MostProdCountries # most productive countries based on number of articles
kw.ca3$TCperCountries    # most productive countries based on total number of citations

### STUDY COUNTRIES ###
# Top 10 Countries of Study by number of publications. Note that studies could have multiple sites from 
# different countries.

countries.string        <- paste(dataset.ana$COS, collapse = ";") # paste cells into one string, use ";" as separator
countries.vector        <- strsplit(countries.string, ";")[[1]] # split string at ";"
countries.vector.clean1 <- gsub(" " ,"", countries.vector) # get rid of space between words to prevent errors
countries.vector.clean2 <- gsub("COTED'IVOIRE","COTE D'VOIRE",
                           gsub("NEWZEALAND", "NEW ZEALAND",
                           gsub("SAUDIARABIA", "SAUDI ARABIA",
                           gsub("SOUTHAFRICA", "SOUTH AFRICA",
                           gsub("SOUTH-EASTASIA", "SOUTH-EAST ASIA",
                           gsub("UNITEDKINGDOM", "UNITED KINGDOM", countries.vector.clean1)))))) # add spaces back in
countries.table1        <- tibble(countries.vector.clean2)
countries.table2        <- filter(countries.table1, countries.vector.clean2 != "EXPERIMENTAL") # filters out "EXPERIMENTAL"
countries.count1        <- count(countries.table2, countries.vector.clean2, wt = NULL, sort = FALSE, name = NULL)
countries.count2        <- rename(countries.count1, cou_count = n)
countries10.count       <- slice_max(countries.count2, cou_count, n = 10)

figure_study_countries <- ggplot(
  data = countries10.count,
  aes(x = fct_reorder(countries.vector.clean2, cou_count), y = cou_count)) +
  geom_col(width = 0.5) +
  coord_flip() + # makes bar plot horizontal
  geom_text(aes(label = cou_count), hjust = 1.6, vjust = 0.35, color = "white", size = 3.5) + ### text needs adjusting
  theme_minimal() +
  labs(x = "COUNTRY OF STUDY", y = "NO. PUBLICATIONS", size = 12)
figure_study_countries

### STUDY SITES ###
# 10 highest sites by number of published studies. N/A and unreported have been excluded.

sites.string        <- paste(dataset.ana$STS, collapse = ";") # paste cells into one string, use ";" as separator
sites.vector        <- strsplit(sites.string, ";")[[1]] # split string at ";"
sites.vector.clean1 <- gsub(" " ,"", sites.vector) # get rid of space between words to prevent errors
sites.vector.clean2 <- gsub("GROTTADIFUMANE","GROTTA DI FUMANE",
                       gsub("GESHERBENOTYA'AQOV", "GESHER BENOT YA'AQOV",
                       gsub("CAVEOFHEARTHS", "CAVE OF HEARTHS",
                       gsub("REITPUTS15", "REITPUTS 15",
                       gsub("SWANSCOMBEMIDDLEGRAVELS", "SWANSCOMBE MIDDLE GRAVELS",
                       gsub("NAHALZIHOR", "NAHAL ZIHOR",
                       gsub("EINGEV", "EIN GEV",
                       gsub("AMANZISPRINGS", "AMANZI SPRINGS",
                       gsub("OLDUVAI GORGE", "OLDUVAI GORGE",
                       gsub("LUDANICE-MYTNANOVAVES", "LUDANICE-MYTNA NOVA VES",
                       gsub("HOLESOVZDRAZILOVKSA", "HOLESOV ZDRAZILOVKSA",
                       gsub("UNITEDKINGDOM", "UNITED KINGDOM", sites.vector.clean1)))))))))))) # add spaces back in
sites.table1        <- tibble(sites.vector.clean2)
sites.table2        <- filter(sites.table1, sites.vector.clean2 != "UNREPORTED")
sites.table3        <- filter(sites.table2, sites.vector.clean2 != "N/A")
sites.count1        <- count(sites.table3, sites.vector.clean2, wt = NULL, sort = FALSE, name = NULL)
sites.count2        <- rename(sites.count1, cou_count = n)
sites10.count       <- slice_max(sites.count2, cou_count, n = 10)

figure_study_sites <- ggplot(
  data = sites10.count,
  aes(x = fct_reorder(sites.vector.clean2, cou_count), y = cou_count)) +
  geom_col(width = 0.5) +
  coord_flip() + # makes bar plot horizontal
  geom_text(aes(label = cou_count), hjust = 1.6, vjust = 0.35, color = "white", size = 3.5) + ### text needs adjusting
  theme_minimal() +
  labs(x = "STUDY SITE", y = "NO. PUBLICATIONS", size = 12)
figure_study_sites

### NUMBER OF MODELS PER STUDY ###
# Boxplots of number of models per study, per year.

models.data1         <- select(dataset.ana, "TI", "PY", "NOM")
models.string        <- paste(models.data1$NOM) # paste cells into one string
models.data2         <- filter(models.data1, models.string != "UNREPORTED") # filters out "UNREPORTED"
models.data3         <- transform(models.data2, NOM = as.numeric(NOM)) # change to NoM values to numeric
models.count1        <- models.data1 %>% count(NOM)

models.count1 # count of values in NoM - including number of papers where the number of models is unreported

figure_no_models <- ggplot(
  models.data3, aes(x = PY, y = NOM, group = PY)) + # needs fixing
  geom_boxplot(fill = '#1B9E77', color = "black") +
  ylim(0, 750) + # NOTE - cuts off two outliers where 1000s of models were made
  scale_x_continuous(limits = c(2007, 2023), 
                     breaks = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 
                                2022)) +
  labs(x = "YEAR PUBLISHED", y = "NO. OF MODELS", size = 12) +
  theme_minimal() + 
  scale_fill_viridis(discrete = TRUE) +
  theme(panel.grid.minor = element_blank())
figure_no_models

### LIST OF KEYWORDS ###

keywords.string        <- paste(dataset.ana$CA3, collapse = ";") # paste cells into one string, use ";" as separator
keywords.vector        <- strsplit(keywords.string, ";")[[1]] # split string at ";"
keywords.vector.clean1 <- gsub(" " ,"", keywords.vector) # get rid of space between words to prevent errors
keywords.table         <- tibble(keywords.vector.clean1)
keywords.count1        <- count(keywords.table, keywords.vector.clean1, wt = NULL, sort = FALSE, name = NULL)
keywords.count2        <- rename(keywords.count1, reg_count = n)

print(keywords.count2, n = 200)
