setwd(dirname(parent.frame(2)$ofile))

pkgs=c("dplyr", "reshape2", "stringr", "HTSet", "clusterProfiler",
       "org.Mm.eg.db", "limma", "edgeR")
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only=TRUE))
}

counts = read.delim("../../../data-raw/BAC3_KO/counts.tsv", sep = "\t", row.names = 1)
rownames(counts) = gsub("\\..+$", "", rownames(counts))
pdata = data.frame(
    rowname = colnames(counts),
    treatment = str_split_fixed(colnames(counts), "_", 2)[,1],
    region = ifelse(grepl("C$", colnames(counts)), "cortex", "hippocampus")
) %>%
    mutate(treatment = factor(treatment, levels = c("WT", "BAC3", "KO"))) %>%
    tibble::column_to_rownames("rowname")
entrezids = bitr(rownames(counts), fromType = "ENSEMBL", toType = "ENTREZID", OrgDb = org.Mm.eg.db)
gene_names = bitr(rownames(counts), fromType = "ENSEMBL", toType = "SYMBOL", OrgDb = org.Mm.eg.db)
fdata = data.frame(
    row.names = rownames(counts),
    ensembl_id = rownames(counts)
) %>%
    mutate(
        entrez_id = entrezids$ENTREZID[match(rownames(counts), entrezids$ENSEMBL)],
        gene_name = gene_names$SYMBOL[match(rownames(counts), gene_names$ENSEMBL)]
    )
rownames(fdata) = fdata$ensembl_id

rna = HTSet(
    edata = as.matrix(counts),
    pdata = pdata,
    fdata = fdata
)

# differnetial expression -------------------------------------------------

d = DGEList(counts)
d = calcNormFactors(d)
d = d[apply(cpm(d), 1, max) > 3,]
group = paste(pdata$treatment, pdata$region, sep = ".")
design = model.matrix(~ group + 0)
y = voom(d, design, plot = T)
rna = subset_features(rna, rownames(d))
cpm = cpm(d)
fit = lmFit(y, design)
contrasts = list()
group_levels = levels(pdata$treatment)
for(r in unique(pdata$region)){
    for(i in 1:2){
        for(j in (i+1):3){
            t1 = group_levels[i]
            t2 = group_levels[j]
            contr = glue::glue("group{t2}.{r} - group{t1}.{r}")
            contrasts[[glue::glue("{r}_{t1}v{t2}")]] = makeContrasts(
                contr, levels = colnames(design)
            )
        }
    }
}

tts = lapply(contrasts, function(contr){
    fit1 = contrasts.fit(fit, contr)
    fit1 = eBayes(fit1)
    topTable(fit1, sort.by = "none", number = Inf)    
})
names(tts) = names(contrasts)

# KEGG pathways -----------------------------------------------------------
# get kegg pathways for each gene
genelist = fdata$entrez_id
genelist = genelist[!is.na(genelist)]
KEGG_DATA = clusterProfiler:::prepare_KEGG("mmu")
qExtID2TermID = DOSE:::EXTID2TERMID(genelist, KEGG_DATA)
qTermID = unique(unlist(qExtID2TermID))
Description = DOSE:::TERM2NAME(qTermID, KEGG_DATA)
Description = paste(names(Description), Description, sep = ": ")
names(Description) = qTermID

rna$fdata$kegg_pathways = sapply(rna$fdata$entrez_id, function(x){
    if(!x %in% names(qExtID2TermID)) return(NA)
    paste(Description[qExtID2TermID[[x]]], collapse = " | ")
})


data = list(
    count = rna,
    cpm = cpm,
    tts = tts
)

saveRDS(data, file = "data.rds")