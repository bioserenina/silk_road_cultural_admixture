### DAPC (BIC EVALUATION, DAPC COMPUTATION)

library(ggplot2)
library(reshape)
library(adegenet)
library(knitr)
library(kableExtra)
library(gridExtra)
library(grid)
library(lattice)
library(cowplot)
library(pheatmap)
library(RColorBrewer)
library(viridis)

all_food = c('LIVER', 'DARK_CHOCOLATE', 'MILK_CHOCOLATE', 'COFFEE', 'ORANGE_JUICE','LEMONS', 'SWEET_BREAD', 'RED_WINE', 'WHITE_WINE', 'BEER', 'VODKA',
             'PORK_CHOPS', 'HAM', 'WILD_MUSHROOMS', 'ASPARAGUS', 'SPINACH',
             'CABBAGE', 'GARLIC', 'ONION', 'KILKA', 'OLIVES',
             'ICE_CREAM', 'COOKED_SALAMI', 'HOT_TEA', 'SARDINES', 'CHILLI_PEPPER',
             'CAKE', 'LAMB', 'BRANDY', 'PEAS', 'BEANS', 'FAVA_BEANS', 'TOMATOES',
             'TURNIP', 'WALNUTS', 'POMEGRANATE', 'CUCUMBER', 'WHOLE_MILK', 'YOGURT',
             'SHEEP_CHEESE', 'MELTED_CHEESE', 'COTTAGE_CHEESE', 'KURUT',
             'SMOKED_CHEESE', 'SULGUNI', 'SWEET_RICOTTA', 'SALTY_RICOTTA',
             'BUCKWHEAT', 'DRY_BISCUITS', 'CREAM_BISCUITS', 'EGGPLANT',
             'COOKED_CARROTS', 'RAW_CARROTS', 'VINEGAR', 'MUTTON', 'ALMONDS',
             'SOUR_CHERRIES', 'PLUM', 'APPLES', 'HONEY', 'JAM', 'MULBERRY',
             'DRIED_FRUIT', 'STRAWBERRIES', 'PRUNE', 'PEPPERS', 'ADGIKA', 'DILL',
             'BARLEY', 'BUTTER', 'WATERMELON', 'BLUEBERRY', 'BLACKBERRY','BANANA',
             'RICE', 'BEET', 'WHIPPED_CREAM', 'MELON')

### read clusters of food

raw_alcohol = read.table('data/cluster_1_foods_final.tsv')[,1] 
raw_pork = read.table('data/cluster_2_foods_final.tsv')[,1]
raw_sweets = read.table('data/cluster_3_foods_final.tsv')[,1]
raw_veggies = read.table('data/cluster_4_foods_final.tsv')[,1]
raw_dairy = read.table('data/cluster_5_foods_final.tsv')[,1]
raw_fruits = read.table('data/cluster_6_foods_final.tsv')[,1]

layer = 1
dataset_type = 'raw'
cat_to_remove='group_and_single'


### BIC EVALUATION

tab_init = read.table(pref_file, sep="\t", hea=T, row.names=1)
# tab_init contains the individual food preferences. We are not allowed to publish the data, but they can be obtained from Prof. Paolo Gasparini.
dataset = tab_init

find_cl <- find.clusters(tab_init, max.n.clust=20, 
                         pca.select = "percVar", perc.pca = 100,
                         scale = FALSE, center = FALSE, choose.n.clust=FALSE, 
                         criterion='diffNgroup')

df_bic = as.data.frame(find_cl$Kstat)
df_bic$K = rownames(df_bic)
names(df_bic) = c('bic', 'K')
df_bic$K = factor(df_bic$K, levels=c(df_bic$K))
selected_bic_df = df_bic[which(df_bic$K == names(find_cl$stat)),]
#selected_bic_df = df_bic[which(df_bic$K == 'K=5'),]

bic_plot = ggplot(data=df_bic, aes(x=K, y=bic, group=1)) +
  geom_line()+
  geom_point() + theme_classic() +
  geom_point(data=selected_bic_df,
             aes(x=K,y=bic),size=3, color='red') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = round(bic, digits=2)), hjust = 0, nudge_x = 0.07, size=3)

### DAPC RESULTS 

for (k in c(4,5,6,7)){
  food_list = all_food
  
  grp_fc <- find.clusters(tab_init, max.n.clust=20, pca.select = "percVar", perc.pca = 100,
                          n.clust = k, scale = FALSE, center = FALSE)
  
  dapc_cluster <- dapc(tab_init, grp = grp_fc$grp, var.contrib = TRUE, scale = FALSE, center = FALSE, pca.select = "percVar", perc.pca = 100, n.da = length(grp_fc$grp)- 1)
    
  tab = read.table(pref_file, hea=T, row.names=1)
  target = rownames(tab_init)
  tab_sorted = tab[match(target, rownames(tab)),]
  dataset$paese = tab_sorted$paese
  dataset$id = rownames(tab_init)
  dataset$cl =  grp_fc$assign
  dataset_with_probs = cbind(dataset, dapc_cluster$posterior)
  
  # you can find the final results for 4, 5, 6 and 7 clusters within the folder "data"
