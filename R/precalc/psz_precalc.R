pkgs = c('plyr', 'dplyr','stringr','reshape2','tibble')
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

rm(list = ls())
setwd("/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Alzheimer_Disease_Study/R/")

fns = list.files("../chromatogram", full.names=T)
curves = list()

for(fn in fns){ # read the curves
    curve = read.delim(fn, header = FALSE, sep="\t", skip = 3)[,1:2]
    colnames(curve) = c("vol", "int")
    curve = curve[!is.na(curve$vol),]
    curves[[gsub("\\.asc","",gsub("\\.\\.\\/chromatogram\\/",'',fn))]] = curve
}
fracs = list()
for(fn in fns){ # read the fractions
    frac = read.delim(fn, header = FALSE, sep="\t", skip = 3, nrow=11)[,3:4]
    colnames(frac) = c("vol", "fraction")
    fracs[[gsub("\\.asc","",gsub("\\.\\.\\/chromatogram\\/",'',fn))]] = frac
}

auc = data.frame(matrix(nrow = 15, ncol = 8))
rownames(auc) = names(curves)
colnames(auc) = 1:8

for(i in 1:nrow(auc)){
    curve = curves[[rownames(auc)[i]]]
    frac = fracs[[rownames(auc)[i]]]
    for(j in 1:ncol(auc)){
        frac_curve = curve[curve$vol >= frac$vol[j+1] & curve$vol <frac$vol[j+2],]
        area = 0
        for(k in 1:(nrow(frac_curve)-1)){
            int_left = frac_curve$int[k]
            int_right = frac_curve$int[k+1]
            width = frac_curve$vol[k+1]-frac_curve$vol[k]
            area = area + (int_left + int_right) * width / 2
        }
        auc[i,j] = area
    }
}

auc %>%
    rownames_to_column(var = "sample_id") %>%
    write.csv(file = "alzheimer_pilot_sample_auc.csv",row.names=FALSE)

save.image("precalc.Rdata")