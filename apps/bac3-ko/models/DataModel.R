DataModel = R6Class(
    "DataModel",
    public = list(
        # attributes
        data = NULL,
        
        # initializer
        initialize = function(data_path){
            self$data = readRDS(data_path)
        },

        # getters --------------------------------------------------------------
        
        getPdata = function(){
            return(self$data$count$pdata)
        },
        
        getFdata = function(){
            return(self$data$count$fdata)
        },
        
        getAllComparisons = function(){
            str_split(names(self$data$tts), "_", simplify = TRUE)[,2]
        },
        
        getAllPathways = function(){
            str_split(self$data$count$fdata$kegg_pathways, " \\| ") %>%
                unlist() %>% 
                str_split(": ", simplify = TRUE) %>%
                `[`(,1) %>%
                `[`(!is.na(.)) %>%
                unique()
        },
        
        getDETable = function(region, comp, pathways) {
            res = self$data$tts[[paste(region, comp, sep = "_")]] %>%
                tibble::rownames_to_column("ensembl") %>%
                select(-B) %>%
                mutate(
                    gene_name = self$data$count$fdata$gene_name,
                    kegg_pathway = self$data$count$fdata$kegg_pathways
                )
            if(!is.null(pathways)){
                p = paste(gsub("mmu","",pathways), collapse = "|")
                res = res[grep(p, res$kegg_pathway),]
            }
            res
        },
        

        # enrichment -----------------------------------------------------------

        enrichKEGG = function(region, comp, cutoff, dir){
            tt = self$data$tts[[paste(region, comp, sep = "_")]]
            entriz = self$data$count$fdata$entrez_id
            if(dir == "none"){
                genelist = entriz[tt$P.Value <= cutoff]
            } else if(dir == "increase"){
                genelist = entriz[tt$P.Value <= cutoff & tt$logFC > 0]
            } else{
                genelist = entriz[tt$P.Value <= cutoff & tt$logFC < 0]
            }
            genelist = genelist[!is.na(genelist)]
            enrichKEGG(
                genelist, organism = "mmu", pvalueCutoff = 0.05,
                minGSSize = 3, maxGSSize = 1000, 
            )
        },
        
        gseaKEGG = function(region, comp){
            tt = self$data$tts[[paste(region, comp, sep = "_")]]
            genelist = tt$t[order(tt$t, decreasing = TRUE)]
            entrezids = self$data$count$fdata$entrez_id[order(tt$t, decreasing = TRUE)]
            genelist = genelist[!is.na(entrezids)]
            entrezids = entrezids[!is.na(entrezids)]
            
            names(genelist) = entrezids
            gseKEGG(genelist, minGSSize = 3, organism = "mmu", pvalueCutoff = 1)
        },
        

        # plots ----------------------------------------------------------------

        boxplot = function(x){
            pdata = self$data$count$pdata
            data.frame(
                gene = self$data$cpm[x,],
                treatment = pdata$treatment,
                region = pdata$region
            ) %>%
                ggplot(aes(x = treatment, y = gene)) +
                geom_boxplot(aes(fill = treatment), alpha = 0.5) +
                geom_point() +
                facet_grid(cols = vars(region)) +
                labs(y = "cpm", x = NULL) +
                theme_classic() +
                theme(
                    axis.text = element_text(color = "black"),
                    axis.text.y = element_text(angle = 90)
                )
        },
        
        dotplot = function(ke, pathways, n){
            df = ke@result %>% arrange(pvalue)
            if(!is.null(pathways)){
                df = df[rownames(ke@result) %in% pathways,]
            }
            if(nrow(df) > n){
                df = df[1:n,]
            }
            df = df %>% 
                mutate(
                    GeneRatio = sapply(GeneRatio, function(x) eval(parse(text = x)))
                ) %>%
                arrange(GeneRatio) %>%
                mutate(Description = factor(Description, levels = Description))
            
            ggplot(df) +
                geom_point(aes(x = GeneRatio, y = Description, color = pvalue, size = Count)) +
                scale_color_gradient(low = pal_lancet()(2)[1], high = pal_lancet()(2)[2]) +
                theme_bw()
        },
        
        gseaplot = function(gsea, geneSetID){
            title = gsea$Description[gsea@result$ID == geneSetID]
            enrichplot::gseaplot2(gsea, geneSetID = geneSetID, title = title) +
                theme(
                    plot.title = element_text(hjust = 0.5)
                )
        }
    )
)