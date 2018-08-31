pkgs = c('plyr', 'dplyr','stringr','reshape2','tibble', "Metabase", "MatCorR")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

## -------- load data ----------------------------------------------------------
rm(list=ls())
setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Alzheimer_Disease_Study/analysis/R/precalc")
load("../../data/hdl.rda")

## -------- limma --------------------------------------------------------------
design = model.matrix(
    data = as(glc$peptide$sample_table, "data.frame"),
    ~ Group
)
glc_lm = lapply(glc, function(mset){
    li = lapply(c("AD   E3/E3", "AD   E3/E4"), function(group){
        mset = subset_samples(
            mset, mset$sample_table$Group %in% c("CTRL E3/E3", group)
        )
        mset$sample_table$Group = factor(mset$sample_table$Group,
                                         levels = c("CTRL E3/E3", group))
        design = model.matrix(data = as(mset$sample_table, "data.frame"), 
                              ~ Group)
        mSet_limma(mset, design, coef = 2, p.value = 2)
    })
    names(li) = c("AD   E3/E3", "AD   E3/E4")
    return(li)
})

fct_lm = lapply(c("AD   E3/E3", "AD   E3/E4"), function(group){
    mset = subset_samples(
        fct, fct$sample_table$Group %in% c("CTRL E3/E3", group)
    )
    mset$sample_table$Group = factor(mset$sample_table$Group,
                                     levels = c("CTRL E3/E3", group))
    design = model.matrix(data = as(mset$sample_table, "data.frame"), 
                          ~ Group)
    mSet_limma(mset, design, coef = 2, p.value = 2)
})
names(fct_lm) = c("AD   E3/E3", "AD   E3/E4")

## -------- correlations -------------------------------------------------------
corr_fct = lapply(glc, function(mset)  
    MatCorPack(fct$conc_table, mset$conc_table, 
    method = c("pearson", "spearman", "kendall")))

## ------------------------ save -------------------------------
data = list(
    data = list(
        glc = glc,
        fct = fct
    ),
    lm = list(
        glc = glc_lm,
        fct = fct_lm
    ),
    corr = list(
        glc = list(
            fct = corr_fct
        )
    )
)
save(data, file="data.rda")


