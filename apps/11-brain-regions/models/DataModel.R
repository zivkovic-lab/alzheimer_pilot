DataModel = R6Class(
    "DataModel",
    public = list(
        # attributes
        data = NULL,
        lm = NULL,
        ea = NULL,
        params = list(
            
        ),
        
        # initializer
        initialize = function(){
            self$data = readRDS(.DATA_RDS_PATH)
        },
        
        plot_pca = function(rel_abund = FALSE, color) {
            data_slot_name = if (rel_abund) "rel_abund" else "abs_abund"
            data = self$data[[data_slot_name]]
            pca = (data$edata + 1) %>% log %>% apply(1, scale) %>% prcomp
            df = data.frame(
                PC1 = pca$x[,1],
                PC2 = pca$x[,2]
            ) %>%
                mutate(
                    region = data$pdata$region,
                    sample = sampleNames(data),
                    group  = data$pdata$group,
                    age    = data$pdata$age
                )
            sdev = (pca$sdev ^ 2) / sum(pca$sdev ^ 2)
            p = ggplot(df, aes(x = PC1, y = PC2, group = group, region = region)) +
                geom_point(aes_string(color = color)) +
                labs(x = glue("PC1 [ {round(sdev[1] * 100, 2)}% ]"),
                     y = glue("PC1 [ {round(sdev[2] * 100, 2)}% ]"))
            ggplotly(p)
        },
        
        plot_glycan_barplots = function(data_type, glycans, regions){
            if(length(glycans) == 0) return()
            if(length(regions) == 0) return()
            glycans = sapply(glycans, function(glycan) glycan[1])
            regions = sapply(regions, function(region) region[1])
            data = self$data[[data_type]]
            
            df0 = NULL
            for(i in seq_along(glycans)){
                glycan = glycans[i]
                if(is.null(df0)){
                    df0 = data.frame(x = data$edata[glycan,])
                } else {
                    df0 = mutate(df0, x = data$edata[glycan,])
                }
                if(glycan %in% colnames(df0)){
                    k = 1
                    while(glycan %in% colnames(df0)){
                        k = k + 1
                        glycan = paste(glycans[i], k)
                    }
                    glycans[i] = glycan
                }
                colnames(df0)[i] = glycan
            }
            df0 = df0 %>% mutate(
                region = data$pdata$region,
                group = data$pdata$group,
                sample_id = sampleNames(data)
            )
            
            df = NULL
            for(i in seq_along(regions)) {
                region = regions[i]
                tmp = df0 %>%
                    filter(region == regions[i]) %>%
                    melt(
                        id.vars = c("sample_id", "region", "group"),
                        variable.name = "glycan"
                    )
                if(is.null(df)) {
                    df = tmp
                } else {
                    if(region %in% df$region){
                        k = 1
                        while(region %in% df$region){
                            k = k + 1
                            region = paste(regions[i], k)
                        }
                        regions[i] = region
                        tmp$region = region
                    }
                    df = rbind(df, tmp)
                }
            }
            df = mutate(
                df,
                region = factor(region, levels = regions),
                glycan = factor(glycan, levels = glycans)
            )
            p = ggplot(df, aes(x = glycan, y = value, fill = group)) +
                stat_summary(geom = "bar", fun = "mean", position = "dodge") +
                stat_summary(geom = "errorbar", fun.data = mean_se, 
                             position = position_dodge(0.9), width = 0.3) +
                scale_fill_lancet() +
                scale_x_discrete(breaks = glycans, labels = gsub(" [0-9]+$", "", glycans)) +
                labs(x = "glycan", y = "brian region") +
                facet_grid(
                    rows = vars(region), 
                    labeller = labeller(region = function(x) gsub(" [0-9]+$", "", x))
                )
            return(ggplotly(p))
        },
        
        plot_volcano = function(data_type, region, selected){
            if(is.null(self$lm)) self$do_lm()
            self$lm[[data_type]][[region]]$results %>%
                rownames_to_column("glycan") %>%
                ggplot(aes(logFC, -log(pval))) +
                geom_point(aes(glycan = glycan, pval = pval, padj = padj), 
                           color = "grey20", alpha = 0.6) +
                geom_point(data = function(x) x[selected, , drop=FALSE],
                           aes(glycan = glycan, pval = pval, padj = padj),
                           color = "salmon") +
                geom_hline(yintercept = -log(0.05), linetype = "dashed",
                           color = "salmon")
        },
        
        plot_ma = function(data_type, region, selected){
            if(is.null(self$lm)) self$do_lm()
            self$lm[[data_type]][[region]]$results %>%
                rownames_to_column("glycan") %>%
                ggplot(aes(mean, logFC)) +
                geom_point(aes(color = pval < 0.05, glycan = glycan, 
                               pval = pval, padj = padj),
                           alpha = 0.6) +
                geom_point(data = function(x) x[selected, , drop=FALSE],
                           aes(glycan = glycan, pval = pval, padj = padj),
                           color = "salmon") +
                scale_color_manual(values = c("grey20", "salmon"))
        },
        
        plot_lm_hist = function(data_type, region){
            if(is.null(self$lm)) self$do_lm()
            self$lm[[data_type]][[region]]$results %>%
                ggplot() +
                geom_histogram(aes(pval), binwidth = 0.025, color = "white",
                               fill = "gray20", boundary = 0) +
                geom_vline(xintercept = 0.05, linetype = "dashed", color = "salmon")
        },
        
        do_lm = function(){
            self$lm = lapply(names(self$data), function(data_type){
                data = self$data[[data_type]]
                data$pdata$interaction = interaction(data$pdata$region, data$pdata$group)
                design = model.matrix(~ interaction + 0, data = data$pdata)
                contrasts = paste0(
                    "interaction", unique(data$pdata$region),
                    ".AD - interaction", unique(data$pdata$region),
                    ".NonAD"
                )
                d0 = DGEList(data$edata)
                d0 = calcNormFactors(d0)
                edata = voom(d0, design)
                fit = lmFit(edata, design)
                
                tts = lapply(contrasts, function(contr){
                    contrast = makeContrasts(contrasts = contr, levels = colnames(coef(fit)))
                    res = contrasts.fit(fit, contrast)
                    res = eBayes(res)
                    tt = topTable(res, number = Inf, sort.by = "none")
                    structure(
                        list(
                            results = data.frame(
                                logFC = tt$logFC,
                                mean  = tt$AveExpr,
                                stat  = tt$t,
                                pval  = tt$P.Value,
                                padj  = tt$adj.P.Val,
                                row.names = rownames(res)
                            ),
                            df = res$df.total,
                            distribution = "t",
                            adjust.method = "BH",
                            design = design,
                            coef = strsplit(contr, " - ")[[1]][1],
                            params = list(),
                            engine = "limma"
                        ),
                        class = "ModelFit"
                    )
                })
                names(tts) = unique(data$pdata$region)
                return(tts)
            })
            names(self$lm) = names(self$data)
        },
        
        lm_table = function(data_type, region) {
            if(is.null(self$lm)) self$do_lm()
            res = self$lm[[data_type]][[region]]$results %>%
                rownames_to_column("glycan")
            return(res)
        },
        
        enrichment_test = function(data_type, region, test_type, alternative, cutoff) {
            if(is.null(self$lm)) self$do_lm()
            htset = self$data[[data_type]]
            self$ea = enrichment_test(
                object = htset,
                fit = self$lm[[data_type]][[region]],
                group = "subgroups",
                test = test_type,
                alternative = alternative,
                p.cutoff = cutoff
            )
        },
        
        get_enrichment_table = function(){
            if(is(self$ea, "EnrichmentFET")){
                table = as.data.frame(self$ea$matrix)
                table$pval = self$ea$pval
                table$odds_ratio = self$ea$odds.ratio
                return(table)
            } else {
                table = data.frame(
                    d = self$ea$d,
                    pval = self$ea$pval
                )
                return(table)
            }
        },
        
        plot_enrichment = function(selected) {
            if(is.null(self$ea)) return()
            if(self$ea$alternative == "less"){
                labels = c("not less", "less")
            } else if(self$ea$alternative == "greater") {
                labels = c("not greater", "greater")
            } else {
                labels = c("not different", "different")   
            }
            if(is(self$ea, "EnrichmentFET")) {
                data.frame(
                    group = rownames(self$ea$matrix),
                    x = self$ea$matrix[,"x"]
                ) %>%
                    mutate(y = self$ea$matrix[,"m"] - x) %>%
                    arrange(desc(self$ea$pval)) %>%
                    mutate(group = factor(group, levels = group)) %>%
                    melt(id.vars = "group") %>%
                    mutate(variable = factor(variable, levels = c("y", "x"),
                                             labels = labels)) %>%
                    ggplot(aes(x = group, y = value)) +
                    geom_bar(aes(fill = variable), stat = "identity") +
                    scale_fill_manual(values = c("grey30", pal_lancet()(9)[2])) +
                    labs(x = NULL) +
                    coord_flip() +
                    guides(fill = guide_legend(title = NULL))
            } else {
                if(self$ea$alternative == "two.sided"){
                    pval = self$ea$fit_pvalues$raw
                } else {
                    pval = self$ea$fit_pvalues$adjusted
                }
                group = names(self$ea$pval)[selected]
                pval = pval[sapply(self$ea$group, function(x) group %in% x)]
                data.frame(
                    pval = pval[order(pval)],
                    ref = seq(from = 0, to = 1, len = length(pval))
                ) %>%
                    melt(value.name = "pval") %>%
                    mutate(variable = factor(variable, levels = c("ref", "pval"))) %>%
                    ggplot() +
                    stat_ecdf(geom="step", aes(pval, color = variable)) +
                    stat_ecdf(geom="point", aes(pval, color = variable)) +
                    scale_color_manual(values = c("grey30", pal_lancet()(9)[2])) +
                    guides(color = guide_legend(title = NULL)) +
                    labs(y = "Fn(pval)", title = "ecdf plot")
            }
        }
    )
)