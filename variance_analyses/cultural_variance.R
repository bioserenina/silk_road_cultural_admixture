### variance analysis (fig 2)

library(ggplot2)
library(reshape)
library(ramify)
library(tidyverse)
library(rstatix)

cbPalette = c('cornflowerblue', 'gold', 'firebrick3', 'dodgerblue4', 'forestgreen', 'darkorchid1', 'grey')

### variance 
df = read.table('data/K5.txt', hea=T, sep="\t")
  
### lmer ---------
library(lme4)
var_X1 = as.data.frame(VarCorr(lmer(X1 ~  (1|village_name/country) + (1|country), data=df)))
var_X1$prop = var_X1$vcov/sum(var_X1$vcov)
var_X1$var1 = "cl1"

var_X2 = as.data.frame(VarCorr(lmer(X2 ~  (1|village_name/country) + (1|country), data=df)))
var_X2$prop = var_X2$vcov/sum(var_X2$vcov)
var_X2$var1 = "cl2"

var_X3 = as.data.frame(VarCorr(lmer(X3 ~  (1|village_name/country) + (1|country), data=df)))
var_X3$prop = var_X3$vcov/sum(var_X3$vcov)
var_X3$var1 = "cl3"

var_X4 = as.data.frame(VarCorr(lmer(X4 ~  (1|village_name/country) + (1|country), data=df)))
var_X4$prop = var_X4$vcov/sum(var_X4$vcov)
var_X4$var1 = "cl4"

var_X5 = as.data.frame(VarCorr(lmer(X5 ~  (1|village_name/country) + (1|country), data=df)))
var_X5$prop = var_X5$vcov/sum(var_X5$vcov)
var_X5$var1 = "cl5"

var_all = rbind(var_X1, var_X2, var_X3, var_X4, var_X5)
var_all$grp = gsub("country:village_name", "Within cities", var_all$grp)
var_all$grp = gsub("village_name", "Between cities", var_all$grp)
var_all$grp = gsub("country", "Between countries", var_all$grp)
var_all$grp = gsub("Residual", "Among individuals", var_all$grp)
var_all$grp = factor(var_all$grp, levels = c("Between countries", "Between cities", "Within cities", "Among individuals"))
var_all$var2 = 'culture'

gene_var = read.table('genetic_variance.txt', hea=T, sep="\t")
var_all_genes = rbind(var_all, gene_var)

palette = c("#8C2B0E", "#C5692D", "#FEB359", "#132F5B", "#435F90", "#68434E", "#B47E83")

var_plot = ggplot(var_all_genes, aes(fill=grp, y=prop, x=var1)) + 
  geom_bar(position="stack", stat="identity", color='black', size=.2)+
  scale_fill_manual(values=palette[c(7,3,2,5)]) + theme_bw() +
  ylab('Variance partitions') +
  theme(legend.position = "bottom") + labs(fill = "Clusters:") + 
  theme(axis.title.y=element_text(size=11), panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), legend.title=element_blank())+
  facet_grid(~var2, scales="free", space='free') + xlab('')

pdf('var_plot.pdf', width = 6, height = 4)
print(var_plot)
dev.off()
