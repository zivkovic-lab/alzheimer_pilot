#' This is Jennifer's study, glycomix in two AD and two control patients, 11 
#' brain ragions
setwd(dirname(parent.frame(2)$ofile))
pkgs=c("dplyr", "reshape2", "stringr", "tibble", "tidyr", "Metabase", "readxl")
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only=TRUE))
}

file = "../data-raw/All E1_20191109_NGlycomics.xlsx"

# subject codes
pdata = read_excel(file, sheet = 3, na = 'n/a') %>%
    as.data.frame %>% 
    select(-`...2`) %>%
    data.table::setnames(old = "...1", "region") %>%
    melt(id.vars = "region") %>% 
    separate(variable, into = c("group", "age"), sep = " ") %>% 
    mutate(age = as.integer(gsub('\\(([0-9]+)\\)', "\\1", age))) %>%
    filter(!is.na(value)) %>%
    column_to_rownames("value")
 
# sheet 1
abs_abund = read_excel(file, sheet = 1)
fdata = abs_abund[,1:7] %>%
    as.data.frame %>%
    mutate(name = paste(Class, Glycan, sep="_")) %>%
    column_to_rownames("name")
edata = abs_abund[,8:45] %>%
    as.matrix %>%
    `rownames<-`(rownames(fdata)) %>%
    `[`(,rownames(pdata))

abs_abund = GlycomicsSet(
    conc_table = conc_table(edata),
    feature_data = feature_data(fdata),
    sample_table =sample_table(pdata)
)

# sheet 2
rel_abund = read_excel(file, sheet = 2)
fdata = rel_abund[,1:7] %>%
    as.data.frame %>%
    mutate(name = paste(Class, Glycan, sep="_")) %>%
    column_to_rownames("name")
edata = rel_abund[,8:45] %>%
    as.matrix %>%
    `rownames<-`(rownames(fdata)) %>%
    `[`(,rownames(pdata))

rel_abund = GlycomicsSet(
    conc_table = conc_table(edata),
    feature_data = feature_data(fdata),
    sample_table =sample_table(pdata)
)

data = list(
    abs_abund = abs_abund,
    rel_abund = rel_abund
)


# subtypes ----------------------------------------------------------------
file = "../data-raw/AZD-11_brian_regions-glycan_annotations_20200519.xlsx"
subtype_map = read_excel(file, sheet = "Catagories", range = "A2:G9") %>% 
    as.data.frame() %>%
    melt(measure.vars = 1:7) %>%
    filter(!is.na(value)) %>%
    mutate(value = as.character(value), variable = as.character(variable))
subtype_keys = unique(subtype_map$value)
subtype_map = lapply(subtype_keys, function(key) 
    subtype_map$variable[subtype_map$value == key])
names(subtype_map) = subtype_keys

# fucosylated
fuc_map = read_excel(file, sheet = "Catagories",
                      range = "A13:B33", col_names = FALSE) %>%
    `colnames<-`(c("condition", "category")) %>%
    mutate(
        Fuc = gsub("^#=\\W*(\\d)+.+", "\\1", condition) %>% as.integer(),
        NeuAc = gsub("^.+A[=≥](\\d)+.*$", "\\1", condition) %>% as.integer()
    )

# sialylated
sia_map = read_excel(file, sheet = "Catagories",
                     range = "A37:B54", col_names = FALSE) %>%
    `colnames<-`(c("condition", "category")) %>%
    mutate(
        NeuAc = gsub("^#\\W*=\\W*(\\d)+.+", "\\1", condition) %>% as.integer(),
        Fuc = gsub("^.+B[=≥](\\d)+.*$", "\\1", condition) %>% as.integer()
    )

# structural_a
struct_map = read_excel(file, sheet = "Catagories",
                     range = "A59:B62", col_names = FALSE) %>%
    `colnames<-`(c("condition", "category")) %>%
    mutate(
        HexNAc = gsub("^#\\W*=\\W*(\\d)+.*$", "\\1", condition) %>% as.integer()
    )

get_glyc_subtype = function(HexNAc, Fuc, NeuAc){
    prefix_map = c("mono", "di", "tri", "tetra", "penta", "hexa", "hepta")
    subtypes = character()
    if(between(Fuc, 1, 7)) {
        fuc_type = paste0(prefix_map[Fuc], "-fuc ")
        fuc_suffix = ifelse(NeuAc == 0, " only", " (sialofuc)")
        subtypes = c(subtypes, paste0(fuc_type, c("total", fuc_suffix)))
    }
    if(between(NeuAc, 1, 6)) {
        sia_type = paste0(prefix_map[NeuAc], "-sia ")
        sia_suffix = ifelse(Fuc == 0, " only", " (sialofuc)")
        subtypes = c(subtypes, paste0(sia_type, c("total", sia_suffix)))
    }
    if(HexNAc == 4) subtypes = c(subtypes, "Bi-antennary")
    else if (HexNAc == 5)  subtypes = c(subtypes, "Tri-antennary")
    else if (HexNAc == 6) subtypes = c(subtypes, "Tetra-antennary")
    else if (HexNAc == 7) subtypes = c(subtypes, "Bisecting GlcNAc")
    
    return(subtypes)
}

for(name in names(data)){
    glc_prefix = gsub("^(.+)_[0-9]+$", "\\1", Metabase::featureNames(data[[name]]))
    data[[name]]$feature_data$subtype = lapply(
        seq_len(nfeatures(data[[name]])), function(i){
        subtypes = subtype_map[[glc_prefix[i]]]
        if(grepl("^CH", glc_prefix[i]))
            subtypes = c(subtypes, "Complex")
        else if(grepl("^C", glc_prefix[i]))
            subtypes = c(subtypes, "Complex/Hybrid")
        else if(grepl("^H", glc_prefix[i]))
            subtypes = c(subtypes, "Hybrid")
        HexNAc = data[[name]]$feature_data$HexNAc[i]
        Fuc = data[[name]]$feature_data$Fuc[i]
        NeuAc = data[[name]]$feature_data$NeuAc[i]
        subtypes = c(subtypes, get_glyc_subtype(HexNAc, Fuc, NeuAc))
        return(subtypes)
    })
}

save(data, file = "brain.rda")