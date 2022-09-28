




#################################################################
## ===================== PROCESS GWAS ===========================
#################################################################
print("Process GWA")
gwa=fread(paste0(HOME,"/results/rds/PS_weighted")) # GWA_PS.rds
gwa$INFO=NULL

gwaOut=data.frame(CHR=gwa$CHR,
                  SNP= gwa$SNP,
                  BP = gwa$BP,
                  A1=gwa$A1,
                  A2=gwa$A2,
                  FREQ = gwa$MAF,
                  BETA = gwa$BETA_sw,
                  SE = gwa$SE_sw,
                  P = gwa$P_sw,
                  N = gwa$N)


print("Select GWA significant SNPs")
clump_input=subset(gwaOut, P<5e-8)
head(clump_input)
NROW(clump_input)

write.table(clump_input,
            file= paste0(HOME, "/data/clump/weightedGWA.pvalsfile"),
            sep="\t",
            row.names = FALSE,
            col.names=T,
            quote=F)

system(paste0(HOME, "/data/clump/plink --bfile ", HOME, "/data/clump/g1000_eur --clump ", HOME, "/data/clump/weightedGWA.pvalsfile --clump-snp-field SNP --clump-field P --out ", HOME, "/data/clump/weightedGWA.pvalsfile --clump-kb 250 --clump-r2 0.1 --clump-p1 1"))
clumped=read.table(paste0(HOME, "/data/clump/weightedGWA.pvalsfile.clumped"), header = TRUE)
clump_sel=subset(clump_input, SNP %in% unique(  clumped$SNP))

nSNPsTotal=NROW(gwa); nSNPsbeforeClumping=NROW(clump_input); nSNPsafterClumping=NROW(clump_sel)


# ========= Gene mapping (positional) ================================
print("Add description")
pheno <- phenoscanner(snpquery=clump_sel$SNP, build=37, catalogue=c("GWAS"))$snps 
geneEnsembl=gconvert(query = pheno$hgnc, organism = "hsapiens",
                     target="ENSG", mthreshold = Inf,
                     filter_na = TRUE)[c("target", "name", "description")]  %>% distinct(name, .keep_all = TRUE)
geneEnsemblPheno=merge(pheno, geneEnsembl, by.x = "hgnc", by.y = "name", all.x = T, suffixes = c("", ".y"))
geneSNPs=merge(clump_sel, geneEnsemblPheno, by.x = "SNP", by.y = "snp", all.x = TRUE, suffixes = c("", "y"))
gene_sel=subset(geneSNPs, select=c(SNP, CHR, A1, A2, BETA, SE, P, N, hgnc, consequence, description))
gene_sel_ordered <-gene_sel[order(gene_sel$P),]


# ========= Phenoscanner mapping ================================
print("Phenoscanner")
pheno2=phenoscanner(gene_sel_ordered$SNP, build=37, catalogue=c("GWAS"))$results
phenoSNPs=merge(gene_sel_ordered, pheno2, by.x = "SNP", by.y = "snp", all.y = TRUE, suffixes = c("", "y"), sort = F)
pheno_sel=subset(phenoSNPs, select=c(SNP, P, N, p, n, trait, dataset), p<5e-8)


# ========= Manhattan plot ================================
ManHplotDF=dataManHplot(df=gwaOut, genes=gene_sel_ordered, topn=30)
# plot output
top=2; left=6; right=2; bottom=4
#jpeg(file=paste0(HOME,"/results/figures/ManHplot_PS.jpeg"),  width=40,height=17, units = "cm", res=1000)
#par(mar = c(bottom, left, top, right))
#dataManHplot_comOut=ManHplot(ManHplotDF, width=37, height=15, type="m", ylimit=NULL)
#dev.off()

# ========= QQ Plot ================================
#jpeg(file=paste0(HOME,"/results/figures/QQplot_PS.jpeg"),  width=18,height=18, units = "cm", res=1000)
#par(mar = c(6, 6, 6, 6))
#QQplot_comOut=ManHplot(ManHplotDF, width=40, height=30, type="q", ylimit= c(0,25))
#dev.off()


# ========= LD Score regression with external traits ================================
# read in clean labels
gwaDat <- as.data.frame(readxl::read_excel(paste0(HOME, "/data/gwaDat.xlsx")))
gwaDat=subset(gwaDat, includeLDSC=="yes")

variableLDSC=subset(variableList, includeLDSC=="yes")
gwaDatVar=rbind(data.frame(label=paste0("mrcieu_" ,variableLDSC$labelUKBB_recoded), labelClean=variableLDSC$label_clean),
                data.frame(label=paste0(gwaDat$label, "_clean"), labelClean=gwaDat$label_clean))


# TABLE: summary of GWA used to re-esimate SNP effects
mrGWAinfoList=list()
ieuID=variableLDSC$ieugwasrID
labelID=variableLDSC$label_clean

for ( i in 1:length(ieuID) ) {
  info=gwasinfo(id=ieuID[i]) 
  infoOut=data.frame(phenotype=labelID[i], ID=ieuID[i], N=info$sample_size, Publication=info$author, Link="https://gwas.mrcieu.ac.uk/")
  mrGWAinfoList[[i]]=infoOut
}
GWAinfoMRC=do.call(rbind, mrGWAinfoList)
GWAinfoExt=data.frame(phenotype=gwaDat$label_clean, ID=NA, N=gwaDat$N, Publication=gwaDat$Publication, Link=gwaDat$link)
GWAinfoSupp=rbind(GWAinfoMRC, GWAinfoExt)

ldscExternal=readRDS(paste0(HOME,"/results/rds/ldscExternalOutDF.rds"))
ldscExternal$participation=recodeParticipation(ldscExternal$trait)
ldscExternal$participation=ifelse(ldscExternal$participation=="participation", "participation", "other")

ldscExternal$uCI=ldscExternal$rg + 1.96 * ldscExternal$rg_se
ldscExternal$lCI=ldscExternal$rg - 1.96 * ldscExternal$rg_se
ldscExternal$pval=2*pnorm(-abs(ldscExternal$rg/ldscExternal$rg_se))
# add clean labels
ldscExternal=merge(ldscExternal, gwaDatVar, by.x="file", by.y="label", all.x=T, sort=F)
ldscExternalClean=ldscExternal[!duplicated(ldscExternal$trait), ]

# generate plot
ldscorePlot = ggplot(data=ldscExternalClean, aes(x=fct_reorder(as.factor(labelClean), -rg), y=rg, fill=participation)) +
  geom_bar(stat="identity") +   
  scale_fill_manual("",values = c("cornflowerblue", "darkgreen") , labels = c("Other", "Participatory behaviour")) + 
  geom_errorbar(aes(ymin=lCI, ymax=uCI), width=.2,
                position=position_dodge(.9)) +
  theme_classic() +
  theme(
    legend.position = c(0.8, 0.8),
    axis.text.x=element_text(size=10, angle=45, hjust=1), plot.margin = margin(t=0, r=0, b=0, l=2, "cm") ) +
  xlab("") +
  labs( y = expression((italic(r[g])))  )

saveFigure(fileName=paste0(HOME,"/results/figures/ldscorePlot"), plotName= ldscorePlot , w=30, h=15)

