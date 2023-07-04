meta <- read.csv2('../mounim/Projects/SARS-CoV-2 2023/ncov/data/ma2022.tsv', sep = '\t' )
ma <- meta[meta$country == 'Morocco',]
x <- gsub("hCoV-19/", "", ma$strain)
x
ma$seqName <- x

nextclade <- read.csv2('../mounim/Projects/SARS-CoV-2 2023/ncov/data/nextclade_qc.tsv', sep = '\t' )
nextclade$seqName

selected <- grep('22', nextclade$clade)
selected1 <- grep('21K', nextclade$clade)
selected2 <- grep('21L', nextclade$clade)

Omicron <- nextclade[c(selected, selected1, selected2),]


ma <- meta[meta$country == 'Morocco',]

ma$division <- gsub("casablanca", "Casablanca", ma$division)
ma$division <- gsub("Inezgane", "Inezegane", ma$division)
ma$division <- gsub("sidi", "Sidi", ma$division)
ma$division <- gsub("Mohammadia", "Mohammedia", ma$division)
ma$division <- gsub("Sale", "Salé", ma$division)
ma$division <- gsub("My Youssef", "Rabat", ma$division)
ma$division <- gsub("Tangier", "Tanger", ma$division)

ma$seqName <- x
MA <- merge(ma, nextclade, by = 'seqName')


##
T_T <- as.matrix(table(MA['division']) )
city_table <- as.data.frame(table(MA['division'])  )
colnames(city_table) <- c('City','N')
colnames(MA)

clade_table <- table(MA['division','c']) 
df_clades <- data.frame(table(MA[c('division','clade')]))
colnames(df_clades) <- c('City','clade','Count')

df_clades <- df_clades[df_clades$City != '',]



df_clades$City <- gsub("casablanca", "Casablanca", df_clades$City)
df_clades$City <- gsub("Inezgane", "Inezegane", df_clades$City)
df_clades$City <- gsub("sidi", "Sidi", df_clades$City)
df_clades$City <- gsub("Mohammadia", "Mohammedia", df_clades$City)
df_clades$City <- gsub("Sale", "Salé", df_clades$City)
df_clades$City <- gsub("My Youssef", "Rabat", df_clades$City)
df_clades$City <- gsub("Tangier", "Tanger", df_clades$City)


clades.prop <- merge(df_clades, city_table, by = 'City')
clades.prop$Freq <- clades.prop$Count / clades.prop$N



library(reshape2)
clades.Mat <- acast(clades.prop, City ~ clade, value.var = 'Freq')

library(pheatmap)
pheatmap(clades.Mat, cluster_rows = TRUE)



#####################
selected <- grep('22', nextclade$clade)
selected1 <- grep('21K', nextclade$clade)
selected2 <- grep('21L', nextclade$clade)

Omicron <- nextclade[c(selected, selected1, selected2),]


ma <- meta[meta$country == 'Morocco',]

ma$division <- gsub("casablanca", "Casablanca", ma$division)
ma$division <- gsub("Inezgane", "Inezegane", ma$division)
ma$division <- gsub("sidi", "Sidi", ma$division)
ma$division <- gsub("Mohammadia", "Mohammedia", ma$division)
ma$division <- gsub("Sale", "Salé", ma$division)
ma$division <- gsub("My Youssef", "Rabat", ma$division)
ma$division <- gsub("Tangier", "Tanger", ma$division)

ma$seqName <- x
MA <- merge(ma, Omicron, by = 'seqName')


##
T_T <- as.matrix(table(MA['division']) )
city_table <- as.data.frame(table(MA['division'])  )
colnames(city_table) <- c('City','N')
colnames(MA)

clade_table <- table(MA['division','c']) 
df_clades <- data.frame(table(MA[c('division','clade')]))
colnames(df_clades) <- c('City','clade','Count')

df_clades <- df_clades[df_clades$City != '',]



df_clades$City <- gsub("casablanca", "Casablanca", df_clades$City)
df_clades$City <- gsub("Inezgane", "Inezegane", df_clades$City)
df_clades$City <- gsub("sidi", "Sidi", df_clades$City)
df_clades$City <- gsub("Mohammadia", "Mohammedia", df_clades$City)
df_clades$City <- gsub("Sale", "Salé", df_clades$City)
df_clades$City <- gsub("My Youssef", "Rabat", df_clades$City)
df_clades$City <- gsub("Tangier", "Tanger", df_clades$City)


clades.prop <- merge(df_clades, city_table, by = 'City')
clades.prop$Freq <- clades.prop$Count / clades.prop$N



library(reshape2)
clades.Mat <- acast(clades.prop, City ~ clade, value.var = 'Freq')

library(pheatmap)
pheatmap(clades.Mat, cluster_rows = TRUE)
shape2)
