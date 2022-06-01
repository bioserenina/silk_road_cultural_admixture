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
library(forcats)
library(ggthemes)
library(patchwork)
library(ggh4x)

### preferences plot -----

all_food = c('LIVER','DARK_CHOCOLATE',
             'MILK_CHOCOLATE', 'CAFFE', 'ORANGE_JUICE', 'LEMONS', 'PANE_DOLCE',
             'RED_WINE', 'WHITE_WINE', 'BIRRA', 'VODKA', 'PORK_CHOPS', 'HAM',
             'WILD_MUSHROOMS', 'ASPARAGUS', 'SPINACH', 'CABBAGE', 'GARLIC', 'ONION',
             'KILKA', 'OLIVE', 'ARTICHOKES', 'ICE_CREAM', 'SALAME_COTTO', 'HOT_TEA',
             'SARDINES', 'CHILLI_PEPPER', 'TORTA', 'LAMB', 'BRANDY', 'PISELLI',
             'FAGIOLI', 'FAVE', 'TOMATOES', 'RAPA', 'NOCI', 'MELOGRANO', 'CETRIOLI',
             'WHOLE_MILK', 'YOGURT', 'SHEEP_CHEESE', 'FORM_FUSO', 'FIOCCHI_LATTE',
             'KURUT', 'FORM_AFFUMICATO', 'SULGUNI', 'RICOTTA_DOLCE',
             'RICOTTA_SALATA', 'GRANO_SARACENO', 'BISCOTTI_SECCHI', 'BISCOTTI_CREME',
             'EGGPLANT', 'CAROTE_COTTE', 'CAROTE_CRUDE', 'ACETO', 'MONTONE',
             'MANDORLE', 'AMARENE', 'SUSINE', 'MELE', 'MIELE', 'MARMELLATA', 'GELSO',
             'FRUTTA_SECCA', 'FRAGOLE', 'PRUGNE', 'PEPERONI', 'ADGIKA', 'ANETO',
             'ORZO', 'BURRO', 'ANGURIA', 'MIRTILLI', 'MIRTILLO_NERO', 'BANANA',
             'RISO', 'BARBABIETOLA', 'WHIPPED_CREAM', 'MELONE')


no_alcohol_pork = c('LIVER','DARK_CHOCOLATE',
                    'MILK_CHOCOLATE', 'CAFFE', 'ORANGE_JUICE', 'LEMONS', 'PANE_DOLCE',
                    'WILD_MUSHROOMS', 'ASPARAGUS', 'SPINACH', 'CABBAGE', 'GARLIC', 'ONION',
                    'KILKA', 'OLIVE', 'ARTICHOKES', 'ICE_CREAM', 'HOT_TEA',
                    'SARDINES', 'CHILLI_PEPPER', 'TORTA', 'LAMB','PISELLI',
                    'FAGIOLI', 'FAVE', 'TOMATOES', 'RAPA', 'NOCI', 'MELOGRANO', 'CETRIOLI',
                    'WHOLE_MILK', 'YOGURT', 'SHEEP_CHEESE', 'FORM_FUSO', 'FIOCCHI_LATTE',
                    'KURUT', 'FORM_AFFUMICATO', 'SULGUNI', 'RICOTTA_DOLCE',
                    'RICOTTA_SALATA', 'GRANO_SARACENO', 'BISCOTTI_SECCHI', 'BISCOTTI_CREME',
                    'EGGPLANT', 'CAROTE_COTTE', 'CAROTE_CRUDE', 'ACETO', 'MONTONE',
                    'MANDORLE', 'AMARENE', 'SUSINE', 'MELE', 'MIELE', 'MARMELLATA', 'GELSO',
                    'FRUTTA_SECCA', 'FRAGOLE', 'PRUGNE', 'PEPERONI', 'ADGIKA', 'ANETO',
                    'ORZO', 'BURRO', 'ANGURIA', 'MIRTILLI', 'MIRTILLO_NERO', 'BANANA',
                    'RISO', 'BARBABIETOLA', 'WHIPPED_CREAM', 'MELONE')

preferences_plot_village = function(path, k, food_list){
  cbPalette = c('cornflowerblue', 'gold', 'firebrick3', 'dodgerblue4', 'forestgreen', 'darkorchid1')
  # assignation ---
  new_path = paste0(path, k, 'clusters_assign_and_probs.tsv')
  df = read.table(new_path, hea=T)
  
  db_with_village = read.table('../food_db_miss_first_norm_village.txt', hea=T, sep="\t")
  rownames(db_with_village) = db_with_village$id
  db_with_village$id = NULL
  target = df$id
  db_with_village$village2 = as.character(db_with_village$village2)
  db_with_village$village = as.character(db_with_village$village)
  df$village = db_with_village[match(target, rownames(db_with_village)),'village2']
  df$village_name = db_with_village[match(target, rownames(db_with_village)),'village']
  df = df[complete_cases(df),]
  
  cl = as.data.frame.matrix(table(df$village_name, df$cl))
  cl_ratio = as.data.frame(t(apply(cl,1,function(x){x/sum(x)})))
  
  if (str_contains(path, 'GMM')){
    colnames(cl_ratio) = as.integer(colnames(cl_ratio)) + 1
  }
  
  cl_ratio$village = rownames(cl_ratio)
  
  cl_m = melt(cl_ratio, id='village')
  
  target = cl_m$village
  cl_m$country = db_with_village[match(target, db_with_village$village),'paese']
  
  props_plot = ggplot(cl_m, aes(fill=variable, y=value, x=village)) + 
    geom_bar(position="stack", stat="identity")+
    scale_fill_manual(values=cbPalette) + theme_classic() +
    ylab('assignment proportions') +
    theme(legend.position = "None") + labs(fill = "Clusters:") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15)) +
    facet_grid(~country, scales="free", space='free') + xlab('Villages')
  
  ###assignments per individual
  
  cl_m_ind = melt(df[,c('paese', 'id', 'cl', 'X1', 'X2', 'X3', 'X4', 'X5', 'village_name')], id=c('id', 'paese', 'village_name', 'cl'))
  
  cbPalette_ind = c('cornflowerblue', 'gold', 'firebrick3', 'dodgerblue4', 'forestgreen')
  #cbPalette_ind = c("cornflowerblue", "gold")
  names(cbPalette_ind) = c('X1', 'X2', 'X3', 'X4', 'X5')
  
  cl_m_ind$village_name = gsub('Mtskheta_Mtianeti', 'Mtskheta Mtianeti', cl_m_ind$village_name)
  
  label_wrap_gen3 <- function(width = 100) {
    function(variable, value) {
      inter <- lapply(strwrap(as.character(value), width=width, simplify=FALSE), 
                      paste, collapse="\n")
      inter <- gsub(paste0("(.{",width,"})"), "\\1\n",inter)
    }
  }
  props_plot_ind <-
    ggplot(cl_m_ind, aes(factor(id), value, fill = factor(variable))) +
    #geom_col(color = "gray", size = 0.1) +
    geom_bar(position="stack", stat="identity")+
    scale_fill_manual(values=cbPalette_ind) + theme_classic() +
    #facet_grid(. ~ paese + village_name,scales = "free", space = "free") +
    facet_nested(. ~ paese + village_name,scales = "free", space = "free", 
                 labeller=label_wrap_gen(width=5), nest_line = TRUE) +
    labs(x = "Individuals",  y = 'assignment proportions') +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(), 
          legend.position = "None") +
    theme(strip.text.x = element_text(size = 10), 
          strip.background = element_blank(),
          ggh4x.facet.nestline = element_line(colour = "black"))
  

  
  

  
  
      
  # preferences ---
  tab_assign = read.table(new_path, hea=T, sep="\t")
  k_columns = tail(names(tab_assign), n=k)
  
  db_with_village = read.table('../food_db_miss_first_norm_village.txt', hea=T, sep="\t")
  rownames(db_with_village) = db_with_village$id
  db_with_village$id = NULL
  target = tab_assign$id
  db_with_village$village2 = as.character(db_with_village$village2)
  tab_assign$village = db_with_village[match(target, rownames(db_with_village)),'village2']
  tab_assign = tab_assign[complete_cases(tab_assign),]
  
  tab_assign_mean = melt(tab_assign, id.vars =c('village', 'paese', 'cl', 'id', k_columns)) %>% group_by(cl, variable) %>% summarise(value = mean(value))
  tab_assign_mean = as.data.frame(tab_assign_mean)
  
  if (str_contains(path, 'GMM')){
    tab_assign_mean$cl = tab_assign_mean$cl + 1
  }
  
  tab_assign_mean$cl = as.factor(tab_assign_mean$cl)
  # melt for kruskal-wallis
  tab_assign_melt = melt(tab_assign, id.vars =c('village', 'paese','cl', 'id', k_columns))
  
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
  
  p_sign <- tab_assign_mean_sign %>%
    ggplot(aes(variable, value, group = cl, color = cl)) +
    geom_point(size = 2.25) +
    geom_line(size = 1.25) + 
    labs(x = NULL, y = "Mean preferences") +
    theme_bw(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=6), legend.position = "top")
  
  gg_sign = ggplotly(p_sign, tooltip = c("variable", "value")) %>%
    layout(legend = list(orientation = "h", y = 1.2))
  
  # heatmap ---
  df_heatmap = cast(tab_assign_mean_sign, id='cl')
  rownames(df_heatmap) = df_heatmap$cl
  df_heatmap$cl = NULL
  

  p_dendrogram = '...'

  return(list(props_plot, gg_sign, p_dendrogram, props_plot_ind))
  
}


### admixture plot ---

### individuals ---

admixture_means_village = function(path_2K, path_distruct, output_path){
  #tab = read.table('GMM/all_food/gaussian_mixture_2clusters_probs.tsv', hea=T)
  tab = read.table(path_2K, hea=T)
  fam = tab[,c('id', 'village')]
  clst = tab[,c('id', 'village')]
  names(clst) <- c("V2", "V3")
  #file_a="GMM/1602061710/aligned.files/gaussian_mixture_"
  file_a=path_distruct
  
  Q0=read.table(paste0(file_a, 2, "clusters_probs.tsv.converted"))
  Q0=Q0[,-(1:5)]
  Q1=Q0
  for (i in 3:6){
    comp = i
    #Q=read.table(paste0(file, ".K",comp, "CLUMPP.out"))
    Q=read.table(paste0(file_a, comp, "clusters_probs.tsv.converted"))
    Q=Q[,-(1:5)]
    Q1=cbind(Q1,Q)
  }  
  
  Q1$sample <- fam$id
  Q1$pop <- clst$V3
  Q1=droplevels(Q1)
  Qmat=as.data.frame(Q1)
  or <- levels(Q1$pop)
  or_label <- levels(Q1$pop)
  
  clst_unord=clst$V3[match(fam$id,clst$V2)]
  k = order(match(clst$V3,or)) #, -(Qmat[,5]))
  QmatO=Qmat[k,]
  # Compute where we will place the population labels in the barplot
  n=length(k)
  clst_ord=clst_unord[k]
  breaks=c(0,which(clst_ord[1:(n-1)]!=clst_ord[2:n]),n)
  nbrks=length(breaks)
  midpts=(breaks[1:(nbrks-1)]+breaks[2:nbrks])/2
  
  QmatO <- QmatO[,c(1:(dim(QmatO)[2]-2))]
  
  cbPalette = c('cornflowerblue', 'gold', 'firebrick3', 'dodgerblue4', 'forestgreen', 'darkorchid1')
  cbPalette <-  c(cbPalette[1:2], cbPalette[1:3], cbPalette[1:4], cbPalette[1:5], cbPalette[1:6], cbPalette[1:7], cbPalette[1:8], cbPalette[1:9], cbPalette[1:10], cbPalette[1:11], cbPalette[1:12], cbPalette[1:13], cbPalette[1:14], cbPalette[1:15], cbPalette[1:16], cbPalette[1:17], cbPalette[1:18], cbPalette[1:19], cbPalette[1:20])
  
  pdf(paste0(output_path, 'individuals.pdf'), width=7, height=4)
  barplot(t(QmatO),col=cbPalette,border=NA,space=0,inside=TRUE, xaxt='n', cex.axis = 0.8, yaxt='n', ann=FALSE, main='Food preferences clusters')
  abline(v=breaks,lwd=1)
  horiz = c(0, 1, 2,3, 4, 5, 6)
  horiz_at = c(0.5, 1.5, 2.5, 3.5, 4.5)
  horiz_label = c('K=2', 'K=3', 'K=4', 'K=5', 'K=6')
  abline(h=horiz,lwd=1)
  mtext(or_label, side=1, at=midpts, las=2, cex=0.8, font=2)
  mtext(horiz_label, side = 2, at=horiz_at, las=1)
  dev.off()  
  
  ### means  ----
  Qmat=as.data.frame(Q1)
  names(Qmat)[1:20] = 1:20
  Qmat_mean <- aggregate(Qmat[, c(1:(dim(QmatO)[2]))], list(Qmat$pop), mean)
  Qmat_mean <- Qmat_mean[match(or, Qmat_mean$Group.1),]
  
  or_label <- or
  
  Qmat_mean_ok <- Qmat_mean[,c(2:21)]
  Qmat_mean_ok <- as.data.frame(Qmat_mean_ok)
  Qmat_mean_ok_t <- t(Qmat_mean_ok)
  colnames(Qmat_mean_ok_t) <- Qmat_mean$Group.1
  
  cbPalette = c('cornflowerblue', 'gold', 'firebrick3', 'dodgerblue4', 'forestgreen', 'darkorchid1')
  cbPalette_1 <-  c(cbPalette[1:2], cbPalette[1:3], cbPalette[1:4], cbPalette[1:5], cbPalette[1:6], cbPalette[1:7], cbPalette[1:8], cbPalette[1:9], cbPalette[1:10], cbPalette[1:11], cbPalette[1:12], cbPalette[1:13], cbPalette[1:14], cbPalette[1:15], cbPalette[1:16], cbPalette[1:17], cbPalette[1:18], cbPalette[1:19], cbPalette[1:20])
  
  #png(paste0(output_path, 'means_horiz.png'), width=250, height=500) # colonna 17
  pdf(paste0(output_path, 'means_horiz.pdf'), width=5, height=10) # colonna 17
  barplot(Qmat_mean_ok_t, col=cbPalette_1,border="black",space=0,inside=TRUE, cex.axis = 0.8, yaxt='n', ann=FALSE, las=2, cex=0.7)
  mtext(paste0("K=",c(2:6)), side=2, at=c(0.5:5), adj=0, las=0, cex=0.8, font=2)
  dev.off()  
  
  pdf(paste0(output_path, 'means.pdf'), width=10, height=5) # colonna 17
  barplot(Qmat_mean_ok_t, col=cbPalette_1,border="black",space=0,inside=TRUE, cex.axis = 0.8, yaxt='n', ann=FALSE, las=2, cex=0.7)
  mtext(paste0("K=",c(2:6)), side=2, at=c(0.5:5), adj=0, las=0, cex=0.8, font=2)
  dev.off() 
}
