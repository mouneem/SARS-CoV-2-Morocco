meta <- read.csv2('./SARS-CoV-2-Morocco/results/ma2022.tsv', sep = '\t' )
ma <- meta[meta$country == 'Morocco',]
x <- gsub("hCoV-19/", "", ma$strain)
x
ma$seqName <- x

nextclade <- read.csv2('./SARS-CoV-2-Morocco/results/world.tsv', sep = '\t' )
nextclade$seqName

selected <- grep('22', nextclade$Nextstrain_clade)
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
MA <- merge(ma, nextclade, by = 'gisaid_epi_isl')

table(ma$gisaid_epi_isl %in% nextclade$gisaid_epi_isl)
##
T_T <- as.matrix(table(MA['division.x']) )
city_table <- as.data.frame(table(MA['division.x'])  )
colnames(city_table) <- c('City','N')
colnames(MA)
MA$Nextstrain_clade.x
clade_table <- table(MA['division.x','c']) 
df_clades <- data.frame(table(MA[c('division.x','Nextstrain_clade.y')]))
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
p <- pheatmap(clades.Mat, cluster_rows = TRUE)
ggsave(file= "./SARS-CoV-2-Morocco/clades.Mat.eps", plot=p, width=15, height=8)



#####################
selected <- grep('22', nextclade$Nextstrain_clade)
selected1 <- grep('21K', nextclade$Nextstrain_clade)
selected2 <- grep('21L', nextclade$Nextstrain_clade)

Omicron <- nextclade[c(selected, selected1, selected2),]


ma <- meta[meta$country == 'Morocco',]

ma$division <- gsub("casablanca", "Casablanca", ma$division)
ma$division <- gsub("Inezgane", "Inezegane", ma$division)
ma$division <- gsub("sidi", "Sidi", ma$division)
ma$division <- gsub("Mohammadia", "Mohammedia", ma$division)
ma$division <- gsub("Sale", "Salé", ma$division)
ma$division <- gsub("My Youssef", "Rabat", ma$division)
ma$division <- gsub("Tangier", "Tanger", ma$division)

ma$gisaid_epi_isl
MA <- merge(ma, Omicron, by = 'gisaid_epi_isl')


##
T_T <- as.matrix(table(MA['division']) )
city_table <- as.data.frame(table(MA['division'])  )
colnames(city_table) <- c('City','N')
colnames(MA)
MA$Nextstrain_clade.y

clade_table <- table(MA['division.x','c']) 
df_clades <- data.frame(table(MA[c('division.x','Nextstrain_clade.y')]))
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
p <- pheatmap(clades.Mat2, cluster_rows = TRUE)
p
ggsave(file= "./SARS-CoV-2-Morocco/clades.Mat2.eps", plot=p, width=10, height=12)

##############
CC <- read.csv2('./SARS-CoV-2-Morocco/results/country_clade.csv',sep = ',',header = FALSE)
library(reshape2)
muta <- melt(CC, id = 'V1')
mut <- muta[c(1,3)]
m <- unique(mut)
m <- m[m$value!='',]

muts_ma <- mut[mut$V1 == 'Morocco',]
muts_ma_u <- unique(muts_ma)
muts_world <- mut[mut$V1 != 'Morocco',]
muts_world_u <- unique(muts_world)

specific_muts_ma <- muts_ma_u[! muts_ma_u$value %in% muts_world_u$value, ]

ma_cnt <- muta[muta$value %in% specific_muts_ma$value, ]
ma_cnt <- as.data.frame(table(ma_cnt))
ma_cnt <- ma_cnt[ma_cnt$Freq > 0 , ]

write.csv(ma_cnt, './results/unique_mutations.csv')
