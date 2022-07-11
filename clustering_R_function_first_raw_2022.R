library(reshape)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(plotly)
#library(heatmaply)
library(sjmisc)
library(ggpubr)
### preferences plot -----

raw_alcohol = read.table('../correlations_clustering/final_datasets/raw/cluster_1_foods_final.tsv')[,1] 
raw_pork = read.table('../correlations_clustering/final_datasets/raw/cluster_2_foods_final.tsv')[,1]
raw_sweets = read.table('../correlations_clustering/final_datasets/raw/cluster_3_foods_final.tsv')[,1]
raw_veggies = read.table('../correlations_clustering/final_datasets/raw/cluster_4_foods_final.tsv')[,1]
raw_dairy = read.table('../correlations_clustering/final_datasets/raw/cluster_5_foods_final.tsv')[,1]
raw_fruits = read.table('../correlations_clustering/final_datasets/raw/cluster_6_foods_final.tsv')[,1]


preferences_plot_final = function(path, k, food_list, dataset_type, cat_to_remove){
  
  # preferences plot
  cbPalette = c('cornflowerblue', 'gold', 'firebrick3', 'dodgerblue4', 'forestgreen', 'darkorchid1', 'grey')
  names(cbPalette) = c('1', '2', '3', '4', '5', '6', '7')
  # assignation ---
  new_path = paste0(path, k, 'clusters_assign_and_probs.tsv')
  
  df = read.table(new_path, hea=T)
  
  cl = as.data.frame.matrix(table(df$paese, df$cl))
  cl_ratio = as.data.frame(t(apply(cl,1,function(x){x/sum(x)})))
  
  cl_ratio$paese = rownames(cl_ratio)
  
  cl_m = melt(cl_ratio, id='paese')
  
  props_plot = ggplot(cl_m, aes(fill=variable, y=value, x=paese)) + 
    geom_bar(position="stack", stat="identity")+
    scale_fill_manual(values=cbPalette) + theme_classic() +
    xlab('country') + ylab('Clusters proportions') +
    theme(legend.position = "bottom") + labs(fill = "Clusters:")+ 
    theme(axis.text.x = element_text(angle = 45, hjust=1), 
          axis.title.x = element_blank(), axis.title.y=element_text(size=9))

  ### foods paese	id	cl	1	2
  # preferences ---
 
  tab_assign = read.table(new_path, hea=T, sep="\t")
  
  k_columns = tail(names(tab_assign), n=k)
  
  tab_assign_mean = melt(tab_assign, id.vars =c('paese', 'cl', 'id', k_columns)) %>% group_by(cl, variable) %>% summarise(value = mean(value, na.rm = T))
  tab_assign_mean = as.data.frame(tab_assign_mean)
  
  if (str_contains(path, 'GMM')){
    tab_assign_mean$cl = tab_assign_mean$cl + 1
  }
  
  tab_assign_mean$cl = as.factor(tab_assign_mean$cl)
  # melt for kruskal-wallis
  tab_assign_melt = melt(tab_assign, id.vars =c('paese', 'cl', 'id', k_columns))
  
  wilcox_food = c()
  
  for (food in food_list){
    if (k == 2){
      if (str_contains(path, 'GMM')){
        p = unname(wilcox.test(tab_assign[tab_assign$cl == 0, food],tab_assign[tab_assign$cl == 1, food])[3])
      } else if (str_contains(path, 'DAPC')){
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
  
  
  if (dataset_type == 'first'){
  tab_assign_mean_sign$cat[which(tab_assign_mean_sign$variable %in% first_muslim)] = 'alcohol and pork'
  tab_assign_mean_sign$cat[which(tab_assign_mean_sign$variable %in% first_sweets)] = 'sweets'
  tab_assign_mean_sign$cat[which(tab_assign_mean_sign$variable %in% first_veggies)] = 'veggies'
  tab_assign_mean_sign$cat[which(tab_assign_mean_sign$variable %in% first_fruits)] = 'fruits'
  tab_assign_mean_sign$cat[which(tab_assign_mean_sign$variable %in% first_dairy)] = 'dairy'
  tab_assign_mean_sign$variable = factor(tab_assign_mean_sign$variable, levels=c(first_muslim, 
                                                                                 first_sweets,
                                                                                 first_veggies,
                                                                                 first_fruits,
                                                                                 first_dairy))
  tab_assign_mean_sign$variable = droplevels(tab_assign_mean_sign$variable)
  
  tab_assign_mean_sign$cat = factor(tab_assign_mean_sign$cat, levels=c('alcohol and pork', 'sweets', 'veggies', 'fruits', 'dairy'))
  tab_assign_mean_sign$cat = droplevels(tab_assign_mean_sign$cat)
  #pch = c(21, 22, 23, 24, 25)
  #names(pch) = c('alcohol and pork', 'sweets', 'veggies', 'fruits', 'dairy')
  
  food_cl_colors = c('#1f77b4',
                     '#ff7f0e',
                     '#2ca02c',
                     '#d62728',
                     '#9467bd')
  names(food_cl_colors) = c('alcohol and pork', 'sweets', 'veggies', 'fruits', 'dairy')
  } else if (dataset_type == 'raw' | dataset_type == 'correlations'){
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
    #annotate("rect", xmin=0, xmax=3.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill="#ff7f0e") +
    #annotate("rect", xmin=3.5, xmax=8.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill="#9467bd") +
    
    geom_line(aes(group = cl, color = cl), size = 1.25) + 
    geom_point(aes(fill=cat), size = 2, shape=21) +
    labs(x = NULL, y = "Mean preferences") +
    theme_bw(base_size = 14) +
    scale_color_manual(values=cbPalette) +
    #scale_shape_manual(values=pch) + 
    scale_fill_manual(values=food_cl_colors) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8), legend.position = "bottom", 
          axis.title.y=element_text(size=9)) +
    #theme(panel.grid.minor = element_line(colour = "gray52",linetype="dashed",size=0.01), 
    #      panel.grid.major = element_line(colour = "gray52",linetype="dashed",size=0.01)) +
    scale_x_discrete(labels = tab_assign_mean_sign$variable_lc[which(tab_assign_mean_sign$cl == 1)])
  
  gg_sign = ggplotly(p_sign, tooltip = c("variable", "value")) %>%
    layout(legend = list(orientation = "h", y = 1.2))

  p_sign_legend <- tab_assign_mean_sign %>%
    ggplot(aes(variable, value)) +
    #annotate("rect", xmin=0, xmax=3.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill="#ff7f0e") +
    #annotate("rect", xmin=3.5, xmax=8.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill="#9467bd") +
    
    geom_line(aes(group = cl, color = cl), size = 1.25) + 
    geom_point(aes(fill=cat), size = 3, shape=21) +
    labs(x = NULL, y = "Mean preferences") +
    theme_bw(base_size = 14) +
    scale_color_manual(values=cbPalette) +
    #scale_shape_manual(values=pch) + 
    scale_fill_manual(values=food_cl_colors) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8), legend.position = "bottom", 
          axis.title.y=element_text(size=9)) +
    #theme(panel.grid.minor = element_line(colour = "gray52",linetype="dashed",size=0.01), 
    #      panel.grid.major = element_line(colour = "gray52",linetype="dashed",size=0.01)) +
    scale_x_discrete(labels = tab_assign_mean_sign$variable_lc[which(tab_assign_mean_sign$cl == 1)])
  
  ### means
  
  cl_means = c()
  if (dataset_type == 'first'){
    for (cl_food in c(list(first_muslim), 
                      list(first_sweets),
                      list(first_veggies),
                      list(first_fruits),
                      list(first_dairy))){
      if (length(intersect(cl_food,food_list)) == 0){
        cl_means = c(cl_means, 0)
        next
      }
      tab = as.data.frame(group_by(tab_assign, cl) %>% summarise_at(vars(cl_food),mean))
      cl_means = c(cl_means, rowMeans(abs(tab[1,-1]-tab[2,-1])))
    }
    names(cl_means) = c("first_muslim", 
                        "first_sweets",
                        "first_veggies",
                        "first_fruits",
                        "first_dairy")
    
  } else if (dataset_type == 'raw' | dataset_type == 'correlations'){
    for (cl_food in c(list(raw_pork), 
                      list(raw_alcohol),
                      list(raw_sweets),
                      list(raw_fruits),
                      list(raw_dairy),
                      list(raw_veggies))){
      if (length(intersect(cl_food,food_list)) == 0){
        cl_means = c(cl_means, 0)
        next
      }
      else if (length(intersect(cl_food,food_list)) < length(cl_food)){
        cl_food = intersect(cl_food,food_list)
      }
      tab = as.data.frame(group_by(tab_assign, cl) %>% summarise_at(vars(cl_food),mean, na.rm=TRUE))
      cl_means = c(cl_means, rowMeans(abs(tab[1,-1]-tab[2,-1])))
    }
    names(cl_means) = c("raw_pork", 
                        "raw_alcohol",
                        "raw_sweets",
                        "raw_fruits",
                        "raw_dairy",
                        "raw_veggies")
    
  }

  #max = names(cl_means[max(cl_means)]) perche' questo non funziona??
  max = names(which(cl_means == max(cl_means)))
  
  all_means_tab = as.data.frame(group_by(tab_assign, cl) %>% summarise_at(food_list,mean))
  all_means_diff = abs(all_means_tab[1,-1]-all_means_tab[2,-1])
  
  cl_means_df = as.data.frame(cl_means)
  names(cl_means_df) = 'value'
  all_means_df = as.data.frame(t(all_means_diff))
  names(all_means_df) = 'value'
  
  distr = all_means_df[-which(rownames(all_means_df) %in% eval(parse(text=max))),,drop=F]
  add_food = distr[distr > quantile(distr$value, 0.99, na.rm=TRUE),,drop=F]
  
  all_diff_df = rbind(all_means_df,cl_means_df)
  all_diff_df$variable = rownames(all_diff_df)
  all_diff_sorted = all_diff_df[order(-all_diff_df$value),]
  all_diff_sorted_30 = all_diff_sorted[1:30,,drop=F] 
  all_diff_sorted_30$cat = NA
  all_diff_sorted_30$cat[which(all_diff_sorted_30$variable %in% raw_pork)] = 'pork'
  all_diff_sorted_30$cat[which(all_diff_sorted_30$variable %in% raw_alcohol)] = 'alcohol'
  all_diff_sorted_30$cat[which(all_diff_sorted_30$variable %in% raw_sweets)] = 'sweets'
  all_diff_sorted_30$cat[which(all_diff_sorted_30$variable %in% raw_fruits)] = 'fruits'
  all_diff_sorted_30$cat[which(all_diff_sorted_30$variable %in% raw_dairy)] = 'dairy'
  all_diff_sorted_30$cat[which(all_diff_sorted_30$variable %in% raw_veggies)] = 'veggies'
  
  # rename "raw"
  rownames(all_diff_sorted_30) = gsub('raw_', 'group: ', rownames(all_diff_sorted_30))
  all_diff_sorted_30$variable = gsub('raw_', 'group: ', all_diff_sorted_30$variable)
  all_diff_sorted_30$cat[is.na(all_diff_sorted_30$cat)] <- 'food groups'
  
  distr_plot = ggplot(all_diff_sorted_30, aes(reorder(variable, value), value)) +
    geom_line(size = 1.25) + geom_point(aes(fill=cat),size = 3, shape=21) + theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8)) +
    geom_hline(yintercept=quantile(distr$value, 0.99, na.rm=TRUE), linetype='dashed', color='gray47', size=1) +
    geom_hline(yintercept=quantile(distr$value, 0.95, na.rm=TRUE), linetype='dotted', color='gray76', size=1) +
    scale_fill_manual(values=food_cl_colors) + 
    ylab(expression(paste(Delta, mu))) + 
    xlab(expression(paste('First 30 foods sorted by their ', Delta, mu))) +
    theme(legend.title = element_blank())
  
  if (cat_to_remove == 'group_and_single'){
    final_food_to_remove = c(eval(parse(text=max)), rownames(add_food))
  } else if (cat_to_remove == 'only_group'){
    final_food_to_remove = eval(parse(text=max))
  }
    
  
  # heatmap ---
  df_heatmap = cast(tab_assign_mean_sign[,-c(4,5)], id='cl')
  rownames(df_heatmap) = df_heatmap$cl
  df_heatmap$cl = NULL
  

  p_dendrogram = '...'
  
  return(list(props_plot, gg_sign, p_dendrogram, p_sign, cl_means, final_food_to_remove, distr_plot, p_sign_legend))
  
}


correlations = function(path, k, food_list, dataset_type){
  
  ### ANCESTRY CORRELATION
  cbPalette = c('cornflowerblue', 'gold', 'firebrick3', 'dodgerblue4', 'forestgreen', 'darkorchid1', 'grey')
  names(cbPalette) = c('1', '2', '3', '4', '5', '6', '7')
  
  
  tab = read.table("G:/Il mio Drive/assegno_padova/clustering//Silk_Road_food_clustering/risultati admixture/SR_HO_anc_sources_Pagani_K6.txt")
  int_cols = c("V1","V2","V7", "V8", "V9", "V10", "V11", "V12")
  new_tab = tab[,int_cols]
  names(new_tab) = c('pop', 'id', 'k1', 'k2', 'k3', 'k4', 'k5', 'k6')
  new_tab$pop = factor(new_tab$pop, levels=c('HG', 'EarlyFarmers', 'YamnayaLike', 'IranN/CHG', 
                                             'CHB.SG', 'YRI.SG', 'ARMENIA', 'AZERBAIJAN', 'GEORGIA', 
                                             'KAZAKISTAN', 'TAJIKISTAN', 'UZBEKISTAN'))
  
  Q=new_tab[,c(5,3,4,6,7,8)]
  fam=new_tab[,c(1,2)]
  
  new_path = paste0(path, k, 'clusters_assign_and_probs.tsv')
  dapc = read.table(new_path, hea=T, sep="\t")
  
  com = new_tab[which(new_tab$id %in% paste0(dapc$paese, ':', dapc$id)),'id']
  
  adm_com = subset(new_tab, id %in% com)
  
  dapc_com = subset(dapc, paste0(dapc$paese, ':', dapc$id) %in% com, c('paese', 'id', 'cl',paste0('X', 1:k)))
  
  target = adm_com$id
  dapc_com_sorted = dapc_com[match(target, paste0(dapc_com$paese, ':', dapc_com$id)),]
  identical(adm_com$id, paste0(dapc_com_sorted$paese, ':', dapc_com_sorted$id))
  
  adm_com = cbind(adm_com, dapc_com_sorted[,c(paste0('X', 1:k), 'cl')])
  
  adm_com$cl = as.character(adm_com$cl)
  
  my_comparisons <- list()
  my_comparisons_df <- combn(1:k, 2)
  for (col in 1:dim(my_comparisons_df)[2]){
    my_comparisons[[col]] = my_comparisons_df[,col]
  }
  
  yamnaya = ggboxplot(adm_com, x = "cl", y = "k3",
            color = "cl", palette = cbPalette, add = "jitter")+
    stat_compare_means(comparisons = my_comparisons, hide.ns = TRUE) +
    stat_summary(fun.y=mean, geom="point", shape=23, size=4, fill="grey") + theme_classic() +
    ylab('Yamnaya-like component') +
    xlab('Clusters') + theme(legend.position = "None")
 
  chb = ggboxplot(adm_com, x = "cl", y = "k6",
                      color = "cl", palette = cbPalette, add = "jitter")+
    stat_compare_means(comparisons = my_comparisons, hide.ns = TRUE) +
    stat_summary(fun.y=mean, geom="point", shape=23, size=4, fill="grey") + theme_classic() +
    ylab('CHB component') +
    xlab('Clusters') + theme(legend.position = "None")
  
  iran = ggboxplot(adm_com, x = "cl", y = "k1",
                      color = "cl", palette = cbPalette, add = "jitter")+
    stat_compare_means(comparisons = my_comparisons, hide.ns = TRUE) +
    stat_summary(fun.y=mean, geom="point", shape=23, size=4, fill="grey") + theme_classic() +
    ylab('IranN/CHG component') +
    xlab('Clusters') + theme(legend.position = "None")
  
  EarlyFarmers = ggboxplot(adm_com, x = "cl", y = "k2",
                      color = "cl", palette = cbPalette, add = "jitter")+
    stat_compare_means(comparisons = my_comparisons, hide.ns = TRUE) +
    stat_summary(fun.y=mean, geom="point", shape=23, size=4, fill="grey") + theme_classic() +
    ylab('EarlyFarmers component') +
    xlab('Clusters') + theme(legend.position = "None")

  
  g = plot_grid(yamnaya, chb, iran, EarlyFarmers, ncol = 4)
  png(paste0(path, k, '.ancestry_correlation.png'), height = 2500, width = 3200, res=300)
  #png('prova.png', height = 1000, width = 3200, res=300)
  print(g)
  dev.off()
  
  ### AGE AND STATUS
  tab = read.table('G:/Il mio Drive/assegno_padova/clustering/Domestic Environments/DATA_FP_ERC_SILK.txt', hea=T, row.names=1)
  #tab = read.table('/Volumes/GoogleDrive-112172391809321147879/Il mio Drive/assegno_padova/clustering/Domestic Environments/DATA_FP_ERC_SILK.txt', hea=T, row.names=1)
  target = dapc$id
  tab_sorted = tab[match(target, rownames(tab)),]
  dapc$age = tab_sorted$age
  dapc$STATUS = tab_sorted$STATUS
  dapc$sex = tab_sorted$sex
  #dapc$village = tab_sorted$village
  food_pref = dapc
  
  food_pref$age = as.numeric(food_pref$age)
  food_pref$STATUS = factor(food_pref$STATUS,levels = c("NT", "MT", "ST"))
  food_pref$cl = as.character(food_pref$cl)
  

  boxplot_age = ggboxplot(food_pref, x = "cl", y = "age",
                           color = "cl", palette = cbPalette, add = "jitter")+
    stat_compare_means(comparisons = my_comparisons, hide.ns = TRUE) +
    stat_summary(fun.y=mean, geom="point", shape=23, size=4, fill="grey") + theme_classic() +
    ylab('Age') +
    xlab('Clusters') + theme(legend.position = "None")
  
  
  #+ ggtitle(paste0('p-value = ', signif(age_p_value[[1]], digits=3)))
  
  
  df_status_cl = as.data.frame.matrix(table(food_pref$cl, food_pref$STATUS))
  df_status_cl_ratio = as.data.frame(t(apply(df_status_cl,1,function(x){x/sum(x)})))
  
  df_status_cl_ratio$cl = rownames(df_status_cl_ratio)
  df_status_cl_ratio_m = melt(df_status_cl_ratio, id='cl')
  
  status_p_value = unname(chisq.test(df_status_cl)[3])
  status_cl_barplot = ggplot(df_status_cl_ratio_m, aes(fill=variable, y=value, x=cl)) + 
    geom_bar(position="stack", stat="identity", color='black')+
    theme_minimal() +
    xlab('Clusters') + ylab('Status proportions') +
    theme(legend.position = "bottom") + labs(fill = "Bitter status:") +
    ggtitle(paste0('chisq p-value = ', signif(status_p_value[[1]], digits=3)))
  
  
  ### SEX
  
  df_sex_cl = as.data.frame.matrix(table(food_pref$cl, food_pref$sex))
  df_sex_cl_ratio = as.data.frame(t(apply(df_sex_cl,1,function(x){x/sum(x)})))
  
  df_sex_cl_ratio$cl = rownames(df_sex_cl_ratio)
  df_sex_cl_ratio = melt(df_sex_cl_ratio, id='cl')
  
  sex_p_value = unname(chisq.test(df_sex_cl)[3])
  df_sex_cl_ratio$variable = gsub(1, 'males', df_sex_cl_ratio$variable)
  df_sex_cl_ratio$variable = gsub(0, 'females', df_sex_cl_ratio$variable)
  sex_cl_barplot = ggplot(df_sex_cl_ratio, aes(fill=variable, y=value, x=cl)) + 
    geom_bar(position="stack", stat="identity", color='black')+
    scale_fill_manual(values=c("males"= 'mediumpurple', 'females'= 'orange1')) + theme_classic() +
    xlab('Clusters') + ylab('Sex') +
    theme(legend.position = "bottom") + labs(fill = "Sex:") +
    ggtitle(paste0('p-value = ', signif(sex_p_value[[1]], digits=3))) 
  
  
  discrete_cl = plot_grid(boxplot_age, status_cl_barplot, sex_cl_barplot,nrow=1, ncol=3)  
  
  #j = plot_grid(scatter_and_boxplot, discrete_cl, ncol = 1, rel_heights = c(1.5, 0.8))
  png(paste0(path, k, '.age_status_correlation.png'), height = 2500, width = 3200, res=300)
  print(discrete_cl)
  dev.off()
  
  return(list(g, boxplot_age, sex_cl_barplot))
  
  }
