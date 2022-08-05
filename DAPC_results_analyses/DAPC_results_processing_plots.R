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
library(ramify)
library(tidyverse)
library(rstatix)

layer = 1
dataset_type = 'raw'
cat_to_remove='group_and_single'
food_list = all_food
cbPalette = c('cornflowerblue', 'gold', 'firebrick3', 'dodgerblue4', 'forestgreen', 'darkorchid1', 'grey')

## dapc results processing -------------------------------

### props plot ----- 
k_value=5
df = read.table('DATA_GIT_HUB/K5.txt', hea=T, sep="\t")
db_with_village = unique(df[,c('country', 'village_name')])

cl_ratio = aggregate(df[, paste0('X', as.character(c(1:k_value)))], list(df$village_name), mean)
rownames(cl_ratio) = cl_ratio$Group.1
cl_ratio$Group.1=NULL
names(cl_ratio) = 1:k_value

cl_ratio$village = rownames(cl_ratio)
cl_m = melt(cl_ratio, id='village')

target = cl_m$village
cl_m$country = db_with_village[match(target, db_with_village$village_name),'country']
cl_m$country = factor(cl_m$country, levels=c('ARMENIA', 'GEORGIA', 'AZERBAIJAN', 'KAZAKISTAN', 'TAJIKISTAN', 'UZBEKISTAN'))

props_plot = ggplot(cl_m, aes(fill=variable, y=value, x=village)) + 
  geom_bar(position="stack", stat="identity", color='black', size=.4)+
  scale_fill_manual(values=cbPalette) + theme_bw() +
  ylab('Cluster proportions') +
  theme(legend.position = "None") + labs(fill = "Clusters:") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=11),
        axis.title.y=element_text(size=11)) +
  facet_grid(~country, scales="free", space='free') + xlab('')

props_plot_color <- ggplot_gtable(ggplot_build(props_plot))
strip_right <- which(grepl('strip-t', props_plot_color$layout$name))
fills <- c("#104e8b2b", "#ff7f2a2b", "#b222222b", "#556b2f2b", "#2a7fff2b", '#cd10762b')
k <- 1
for (i in strip_right) {
  j <- which(grepl('rect', props_plot_color$grobs[[i]]$grobs[[1]]$childrenOrder))
  props_plot_color$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}

grid.draw(props_plot_color)

### pref_plot ------

tab_assign = read.table(pref_file)
# pref_file contains the individual food preferences. We are not allowed to publish the data, but they can be obtained from Prof. Paolo Gasparini.
k_columns = tail(names(tab_assign), n=k_value)

tab_assign_mean = melt(tab_assign, id.vars =c('country', 'cl', 'id', k_columns)) %>% group_by(cl, variable) %>% summarise(value = mean(value, na.rm = T))
tab_assign_mean = as.data.frame(tab_assign_mean)

tab_assign_mean$cl = as.factor(tab_assign_mean$cl)
# melt for kruskal-wallis
tab_assign_melt = melt(tab_assign, id.vars =c('country', 'cl', 'id', k_columns))

wilcox_food = c()

for (food in food_list){
  if (k_value == 2){
    if (str_contains(path, 'DAPC')){
      p = unname(wilcox.test(tab_assign[tab_assign$cl == 1, food],tab_assign[tab_assign$cl == 2, food])[3])
    }
  } else {
    df = tab_assign_melt[which(tab_assign_melt$variable == food),]
    p = unname(kruskal.test(value ~ cl, data = df)[3])
  }
  if (p[[1]]*length(food_list) < 0.05){
    wilcox_food = c(wilcox_food, food)
  }
}

tab_assign_mean_sign = tab_assign_mean[which(tab_assign_mean$variable %in% wilcox_food),]
tab_assign_mean_sign$cat = NA
tab_assign_mean_sign$variable = as.character(tab_assign_mean_sign$variable)


if (dataset_type == 'raw'){
  tab_assign_mean_sign$cat[which(tab_assign_mean_sign$variable %in% raw_pork)] = 'pork'
  tab_assign_mean_sign$cat[which(tab_assign_mean_sign$variable %in% raw_alcohol)] = 'alcohol'
  tab_assign_mean_sign$cat[which(tab_assign_mean_sign$variable %in% raw_sweets)] = 'sweets'
  tab_assign_mean_sign$cat[which(tab_assign_mean_sign$variable %in% raw_fruits)] = 'fruits'
  tab_assign_mean_sign$cat[which(tab_assign_mean_sign$variable %in% raw_dairy)] = 'dairy'
  tab_assign_mean_sign$cat[which(tab_assign_mean_sign$variable %in% raw_veggies)] = 'veggies'
  tab_assign_mean_sign$variable = factor(tab_assign_mean_sign$variable, levels=c(raw_pork, 
                                                                                 raw_alcohol,
                                                                                 raw_sweets,
                                                                                 raw_fruits,
                                                                                 raw_dairy,
                                                                                 raw_veggies))
  tab_assign_mean_sign$variable = droplevels(tab_assign_mean_sign$variable)
  #pch = c(21, 22, 23, 24, 25)
  #names(pch) = c('pork', 'sweets', 'veggies', 'fruits', 'diary')
  
  tab_assign_mean_sign$cat = factor(tab_assign_mean_sign$cat, levels=c('pork', 'alcohol','sweets', 'fruits', 'dairy','veggies'))
  tab_assign_mean_sign$cat = droplevels(tab_assign_mean_sign$cat)
  
  food_cl_colors = c('#1f77b4',
                     '#ff7f0e',
                     '#2ca02c',
                     '#d62728',
                     '#9467bd',
                     '#e377c2')
  names(food_cl_colors) = c('dairy', 'pork', 'veggies','fruits','alcohol','sweets')
  
} 

tab_assign_mean_sign$variable_lc = tolower(tab_assign_mean_sign$variable)
tab_assign_mean_sign$variable_lc = gsub("_", " ", tab_assign_mean_sign$variable_lc)

tab_assign_mean_sign = tab_assign_mean_sign[order(tab_assign_mean_sign$variable),]

p_sign <- tab_assign_mean_sign %>%
  ggplot(aes(variable, value)) +
  geom_line(aes(group = cl, color = cl), size = 1.25) + 
  geom_point(aes(fill=cat), size = 2, shape=21) +
  labs(x = NULL, y = "Mean cluster preferences") +
  theme_bw(base_size = 14) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual(values=food_cl_colors) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=11), legend.position = "bottom", 
        axis.title.y=element_text(size=11)) +
  scale_x_discrete(labels = tab_assign_mean_sign$variable_lc[which(tab_assign_mean_sign$cl == 1)])

legend_plot = get_legend(p_sign)

p_sign <- tab_assign_mean_sign %>%
  ggplot(aes(variable, value)) +
  geom_line(aes(group = cl, color = cl), size = 1.25) + 
  geom_point(aes(fill=cat), size = 2, shape=21) +
  labs(x = NULL, y = "Mean cluster preferences") +
  theme_bw(base_size = 14) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual(values=food_cl_colors) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10), legend.position = "None", 
        axis.title.y=element_text(size=11)) +
  scale_x_discrete(labels = tab_assign_mean_sign$variable_lc[which(tab_assign_mean_sign$cl == 1)]) +
  theme(panel.grid.major.x = element_line(linetype = 'dashed', size=0.5),
        panel.grid.minor.y = element_line(linetype = 'solid', size=0.001),
        panel.grid.major.y = element_line(linetype = 'solid', size=0.001)) #+


### heatmap ----

# mean by ind
df = as.data.frame(cbind(rowMeans(tab_assign[,raw_pork]),
                         rowMeans(tab_assign[,raw_alcohol]),
                         rowMeans(tab_assign[,raw_sweets]),
                         rowMeans(tab_assign[,raw_fruits]),
                         rowMeans(tab_assign[,raw_dairy]),
                         rowMeans(tab_assign[,raw_veggies])))
names(df) = c('pork', 'alcohol', 'sweets', 'fruits', 'dairy', 'veggies')

df = as.data.frame(cbind(df, tab_assign[,c('cl', paste0('X', as.character(c(1:k_value))),'country')]))
rownames(df) = tab_assign$id

# add villages, sex and age

tab = read.table(pref_file)
target = rownames(df)
tab_sorted = tab[match(target, rownames(tab)),]

all(df$country == tab_sorted$country)
df$village = tab_sorted$village
df$age = tab_sorted$age
df$sex = tab_sorted$sex

# add ancestry 

# file SR_HO_anc_sources_Pagani_K6.txt contains the supervised admixture results
adm = read.table("data/SR_HO_anc_sources_Pagani_K6.txt")
int_cols = c("V1","V2","V7", "V8", "V9", "V10", "V11", "V12")
new_adm = adm[,int_cols]
names(new_adm) = c('pop', 'id', 'k1', 'k2', 'k3', 'k4', 'k5', 'k6')
new_adm$pop = factor(new_adm$pop, levels=c('HG', 'EarlyFarmers', 'YamnayaLike', 'IranN/CHG', 
                                           'CHB.SG', 'YRI.SG', 'ARMENIA', 'AZERBAIJAN', 'GEORGIA', 
                                           'KAZAKISTAN', 'TAJIKISTAN', 'UZBEKISTAN'))

new_adm_ = subset(new_adm, pop %in% c('ARMENIA', 'AZERBAIJAN', 'GEORGIA', 
                                      'KAZAKISTAN', 'TAJIKISTAN', 'UZBEKISTAN'))

vec_name = c() 
for (i in strsplit(new_adm_$id, ':')){
  vec_name = c(vec_name, i[2])
}

rownames(new_adm_) = vec_name
target = rownames(df)
adm_sorted = new_adm_[match(target, rownames(new_adm_)),]
names(adm_sorted) = c('pop', 'id', 'CHG', 'EF', 'Yamnaya', 'HG', 'YRI', 'CHB')

df = as.data.frame(cbind(df, adm_sorted[,c('CHG', 'EF', 'Yamnaya', 'HG', 'YRI', 'CHB')]))

# correlations

cl_vars = paste0('X', as.character(c(1:k_value)))
cl_expl = c('pork', 'alcohol', 'sweets', 'fruits', 'dairy', 'veggies',
            'CHG', 'EF', 'Yamnaya', 'HG', 'YRI', 'CHB')
cl_expl_cat = c('country', 'sex')

cor_mat = cor(df[,c(cl_vars, cl_expl)], method = c("spearman"), use = "complete.obs")

cor_mat_ok = cor_mat[cl_expl,cl_vars]

ann_color = list("cat"=c("pork"="#ff7f0e","alcohol"="#9467bd", 'sweets' = '#e377c2', 'fruits' = '#d62728', 'dairy' = '#1f77b4', 'veggies' = '#2ca02c',
                         'CHG'='#DC9200', 'EF'='#FFDF00', 'YAM'='#E2A4C6', 'HG'='#B668A1', 'YRI'='#743282', 'CHB'='#441C55'), 
                 "cluster" = c('X1'="cornflowerblue", 'X2'="gold", 'X3'="firebrick3", 'X4'="dodgerblue4", 'X5'="forestgreen", 'X6'='darkorchid1', 'X7'='grey'))

mat1 = as.data.frame(ann_color$cat)
mat1$cat = rownames(mat1)
mat1$'ann_color$cat' = NULL

mat2 = as.data.frame(ann_color$cluster)
mat2$cluster = rownames(mat2)
mat2$'ann_color$cluster' = NULL

paletteLength <- 200
myColor <- colorRampPalette(c("cornflowerblue", "white", "firebrick1"))(paletteLength)
myBreaks = c(seq(-1, 1, 0.01))

pheatmap_v = pheatmap(cor_mat_ok, cluster_rows=F, cluster_cols=F,  annotation_row = mat1, annotation_col=mat2, annotation_colors=ann_color, border_color ='black', gaps_row = c(6), annotation_legend=FALSE, annotation_names_row=TRUE, annotation_names_col=TRUE, breaks=myBreaks, color=myColor, cellheight=30, cellwidth = 30, display_numbers = T)

## Age boxplot and sex barplot

tab = read.table(pref_file, hea=T, row.names=1)
target = tab_assign$id
tab_sorted = tab[match(target, rownames(tab)),]
tab_assign$age = tab_sorted$age
tab_assign$STATUS = tab_sorted$STATUS
tab_assign$sex = tab_sorted$sex
#dapc$village = tab_sorted$village
food_pref = tab_assign

food_pref$age = as.numeric(food_pref$age)
food_pref$STATUS = factor(food_pref$STATUS,levels = c("NT", "MT", "ST"))
food_pref$cl = as.character(food_pref$cl)

food_pref$cl = factor(food_pref$cl, levels= c(as.character(1:k_value)))

stat_pvalue <- food_pref %>% 
  rstatix::wilcox_test(age ~ cl) %>%
  filter(p.adj < 0.05) %>% 
  rstatix::add_significance("p") %>% 
  rstatix::add_y_position() %>% 
  mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))

boxplot_age = ggboxplot(food_pref, x = "cl", y = "age",
                        color = "cl", palette = cbPalette, add = "jitter", notch=TRUE)+
  ggpubr::stat_pvalue_manual(stat_pvalue, label = "p.adj", size=3) +
  ylab('Age') +
  xlab('Clusters') + theme(legend.position = "None", axis.title.x=element_blank())


df_sex_cl = as.data.frame.matrix(table(food_pref$cl, food_pref$sex))
df_sex_cl_ratio = as.data.frame(t(apply(df_sex_cl,1,function(x){x/sum(x)})))
df_sex_cl_ratio$cl = rownames(df_sex_cl_ratio)
df_sex_cl_ratio = melt(df_sex_cl_ratio, id='cl')

df_sex_cl$cl = rownames(df_sex_cl)
df_sex_cl_ratio = melt(df_sex_cl, id='cl')
df_sex_cl$cl = NULL

stat_pvalue_chi_ok <- 
  rstatix::pairwise_prop_test(df_sex_cl) %>%
  filter(p.adj < 0.05) %>% 
  rstatix::add_significance("p") 

sex_barplot = ggbarplot(df_sex_cl_ratio, "cl", "value",
                        fill = "variable", color = "variable", palette = c('#FC4E07', '#00AFBB'),
                        label = TRUE,
                        position = position_dodge(0.8), lab.pos = "in", lab.col = "white") + 
  ggpubr::stat_pvalue_manual(stat_pvalue_chi_ok, label = "p.adj", size=3, y.position = 80, step.increase = 0.1) +
  theme(legend.position = 'None', axis.title.x=element_blank()) 

f = plot_grid(props_plot_color, p_sign, labels=c('A', 'B'), ncol = 1, rel_heights = c(1, 1.1))
g = plot_grid(boxplot_age, sex_barplot, labels=c('D', 'E'), ncol = 1, rel_heights = c(1, 0.7))
h = plot_grid(pheatmap_v$gtable, g, labels=c('C', ''), ncol = 2, scale=c(0.2, 1))
j = plot_grid(f, h, ncol = 1, rel_heights = c(1, 0.9))

pdf(paste0('k', k_value, 'correct.pdf'), height = 13, width = 11)
print(j)
dev.off()

### nnls -----------------
library(nnls)

k_df = list()
ks = c(4, 5, 6, 7)

for (k_number in 1:length(ks)){
  k_value = ks[k_number]
  df = read.table(paste0('DATA_GIT_HUB/K', k_value, '.txt'), hea=T, sep="\t")
  #df = read.table(paste0(path_abs, 'final_analyses_2022/',dataset_type,'/distruct/aligned.files/DAPC_norm_1_', k_value, 'clusters_assign_and_probs.tsv_for_distruct.txt.converted'), hea=F)

  df_ = df[,c(paste0('X', as.character(c(1:k_value))), 'id', 'country')]
  rownames(df_) = df_$id
  names(df_) = paste0(k_value, '_', names(df_))
  k_df[[k_number]] = df_
}

final_df = do.call("cbind", k_df)

### 4-->5
tab = data.frame(matrix(ncol = 4, nrow = 5))
for (i in 1:5){
  col = paste0("5_X", i)
  mod <- nnls(as.matrix(final_df[,c("4_X1", "4_X2", "4_X3", "4_X4")]), final_df[,col])
  tab[i,] =  mod$x/sum(mod$x)
}
tab1 = tab
rownames(tab1) = as.character(rownames(tab1))

### 5-->6
tab = data.frame(matrix(ncol = 5, nrow = 6))
for (i in 1:6){
  col = paste0("6_X", i)
  mod <- nnls(as.matrix(final_df[,c("5_X1", "5_X2", "5_X3", "5_X4", "5_X5")]), final_df[,col])
  tab[i,] =  mod$x/sum(mod$x)
  #tab[,i] =  mod$x/sum(mod$x)
}
tab2 = tab
rownames(tab2) = as.character(rownames(tab2))

### 6-->7
tab = data.frame(matrix(ncol = 6, nrow = 7))
for (i in 1:7){
  col = paste0("7_X", i)
  mod <- nnls(as.matrix(final_df[,c("6_X1", "6_X2", "6_X3", "6_X4", "6_X5", "6_X6")]), final_df[,col])
  tab[i,] =  mod$x/sum(mod$x)
  #tab[,i] =  mod$x/sum(mod$x)
}
tab3 = tab
rownames(tab3) = as.character(rownames(tab3))

ann_color = list("cat" = c('1'="cornflowerblue", '2'="gold", '3'="firebrick3", '4'="dodgerblue4", '5'="forestgreen", '6'='darkorchid1', '7'='grey'),
                 "cluster" = c('X1'="cornflowerblue", 'X2'="gold", 'X3'="firebrick3", 'X4'="dodgerblue4", 'X5'="forestgreen", 'X6'='darkorchid1', 'X7'='grey'))

mat1 = as.data.frame(ann_color$cat)
mat1$cat = rownames(mat1)
mat1$'ann_color$cat' = NULL

mat2 = as.data.frame(ann_color$cluster)
mat2$cluster = rownames(mat2)
mat2$'ann_color$cluster' = NULL

myColor <- brewer.pal(9, 'Purples')
myColor = c(myColor)
myColor = colorRampPalette(c("lavenderblush", "orchid4"))(10)
myBreaks = c(seq(0, 1, 0.1))

hm1 = pheatmap(tab1, cluster_rows=F, cluster_cols=F, 
               color=myColor, main="From 4 to 5K",
               cellheight=30, cellwidth = 30,
               annotation_row = mat1, annotation_col=mat2, annotation_colors=ann_color, 
               annotation_legend=FALSE, display_numbers = T, number_color = "black", breaks = myBreaks)
hm2 = pheatmap(tab2, cluster_rows=F, cluster_cols=F, 
               color=myColor, main="From 5 to 6K", 
               cellheight=30, cellwidth = 30,
               annotation_row = mat1, annotation_col=mat2, annotation_colors=ann_color, 
               annotation_legend=FALSE, display_numbers = T, number_color = "black", breaks = myBreaks)
hm3 = pheatmap(tab3, cluster_rows=F, cluster_cols=F, 
               color=myColor, main="From 6 to 7K",
               cellheight=30, cellwidth = 30,
               annotation_row = mat1, annotation_col=mat2, annotation_colors=ann_color, 
               annotation_legend=FALSE, display_numbers = T, number_color = "black", breaks = myBreaks)



j = plot_grid(hm1$gtable, hm2$gtable, hm3$gtable, ncol=3, nrow=1, labels = c('A', 'B', 'C'))

pdf('NNLS_runs.pdf', width = 12, height = 7)
print(j)
dev.off()

