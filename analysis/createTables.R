

################################################################
# ====================== Create output tables ==================
################################################################

# +++++++++++++++++++++++++ RUN FUNCTIOND AND DEFINE STYLES ++++++++++++++++++++++++++++++++++++++++++++++++++
ColStart=2
RowHeader=2
RowSubheaderStart=3
RowSubheaderEnds=3
RowTable=4


# Create info text
createInfo=function(dataInfoPath){
  datOut=read.csv(dataInfoPath,header=T)
  datOut$X=NULL
  datOut_merged=paste0(datOut[,1],": " ,datOut[,2])
  return(datOut_merged)
}

# Headerstyle
hs1 <- createStyle(halign = "CENTER", textDecoration = "Bold",
                   border = "Bottom", fontColour = "black", fgFill = "white")

h_info <- createStyle(halign = "left", textDecoration = "Bold",
                      border = "Bottom", fontColour = "black", fgFill = "white")

addTable=function(sheet, table){
  writeDataTable(wb, sheet, table, headerStyle=hs1, tableStyle = "TableStyleLight1",
                 startRow = RowTable, startCol = ColStart)
  setColWidths(wb, sheet, cols=2:10, widths = 15)
}

# HEADER
headerFunc=function(TITLE, sheet){
  writeData(wb, sheet = sheet, TITLE,
            colNames = FALSE, rowNames = FALSE,
            startCol = ColStart, startRow = RowHeader)
}
# INFO ROW
InfoFunc=function(TITLE, sheet){
  writeData(wb, sheet = sheet, TITLE,
            colNames = FALSE, rowNames = FALSE,
            startCol = ColStart, startRow = RowSubheaderStart)
}

# Create new workbook
setwd(paste0(HOME,"/results/tables/"))
wb <- openxlsx::createWorkbook()




# +++++++ Table 1+++++++
# Create new sheet
extGWA="Table 1"
addWorksheet(wb, extGWA)
# Add parameters
title_name="Table 1. GWA summary statistic files included in LDSC regression"
sheet=extGWA
table=GWAinfoSupp
Info_text="Summary statistic files used to test for genetic correlations between the liability to UKBB participation and other traits, accessible via consortia websites or the MRC-IEU OpenGWAS project (https://gwas.mrcieu.ac.uk)"
# Run functions
addTable(sheet, table)
headerFunc(title_name, sheet)
InfoFunc(Info_text, sheet)

# +++++++ Table +++++++
# Create new sheet
censusData="Table 2"
addWorksheet(wb, censusData)
# Add parameters
title_name="Table 2. Demographic variables in HSE, UK Census and UK Biobank"
sheet=censusData
census$Category=recodeLevel(census$Category)
table=census
Info_text="Comparison of distributions between variables present in the UK Census sample (2011 Census Microdata), the HSE (Health Survey England) and the UKBB (UK Biobank)"
# Run functions
addTable(sheet, table)
headerFunc(title_name, sheet)
InfoFunc(Info_text, sheet)


# +++++++ Table +++++++
# Create new sheet
weightedMean="Table 3"
addWorksheet(wb, weightedMean)
# Add parameters
title_name="Table 3. Weighted means and prevalences"
sheet=weightedMean
table=weightingChange
Info_text="Weighted means (for continuous variables) and prevalences (for binary traits) in the UKBB, using the UKBB sampling weights"
# Run functions
addTable(sheet, table)
headerFunc(title_name, sheet)
InfoFunc(Info_text, sheet)



# +++++++ Table +++++++
weighSNPSupp=weighSNPDiff
weighSNPSupp$Trait=recodeHarmVar2(weighSNPSupp$pheno)
weighSNPSupp=subset(weighSNPSupp, select=c(Trait, SNP, CHR, BP, A1, A2, BETA, SE, P, N, BETA_wt, SE_wt,P_wt, N_wt))

names(weighSNPSupp)[names(weighSNPSupp) == 'BETA'] <- 'BETA (standard)'
names(weighSNPSupp)[names(weighSNPSupp) == 'SE'] <- 'SE (standard)'
names(weighSNPSupp)[names(weighSNPSupp) == 'P'] <- 'P (standard)'
names(weighSNPSupp)[names(weighSNPSupp) == 'N'] <- 'N (standard)'

names(weighSNPSupp)[names(weighSNPSupp) == 'BETA_wt'] <- 'BETA (weighted)'
names(weighSNPSupp)[names(weighSNPSupp) == 'SE_wt'] <- 'SE (weighted)'
names(weighSNPSupp)[names(weighSNPSupp) == 'P_wt'] <- 'P (weighted)'
names(weighSNPSupp)[names(weighSNPSupp) == 'N_wt'] <- 'Effective N (weighted)'
weighSNPSupp <- weighSNPSupp[order(weighSNPSupp$Trait),] 

# Create new sheet
wGWAsnps="Table 4"
addWorksheet(wb, wGWAsnps)
# Add parameters
title_name=paste0("Table 4. Standard and weighted SNP effects for " , NROW(levels(as.factor(weighSNPSupp$Trait))), " UKBB traits")
sheet=wGWAsnps
table=weighSNPSupp
Info_text="Listed are all SNPs reaching genome-wide significance in either weighted GWA or standard GWA analyses "
# Run functions
addTable(sheet, table)
headerFunc(title_name, sheet)
InfoFunc(Info_text, sheet)


# +++++++ Table +++++++
sexGWASupp=subset(sexGWACheck, select=c(SNP, A1, A2, BETA_23, SE_23, P_23, BETA, SE, P, BETA_sw, SE_sw, P_sw))
names(sexGWASupp)[names(sexGWASupp) == 'BETA_23'] <- 'BETA (23andMe)'
names(sexGWASupp)[names(sexGWASupp) == 'SE_23'] <- 'SE (23andMe)'
names(sexGWASupp)[names(sexGWASupp) == 'P_23'] <- 'P (23andMe)'
names(sexGWASupp)[names(sexGWASupp) == 'BETA'] <- 'BETA (unweighted)'
names(sexGWASupp)[names(sexGWASupp) == 'SE'] <- 'SE (unweighted)'
names(sexGWASupp)[names(sexGWASupp) == 'P'] <- 'P (unweighted)'
names(sexGWASupp)[names(sexGWASupp) == 'BETA_sw'] <- 'BETA (weighted)'
names(sexGWASupp)[names(sexGWASupp) == 'SE_sw'] <- 'SE (weighted)'
names(sexGWASupp)[names(sexGWASupp) == 'P_sw'] <- 'P (weighted)'
# Create new sheet
sexGWA="Table 5"
addWorksheet(wb, sexGWA)
# Add parameters
title_name="Table 5. Autosomal variants associated with sex"
sheet=sexGWA
table=sexGWASupp
Info_text="Listed are the effect estimates for sex-associated ausosomal variants extracted from 23andMe, restricted to directly genotyped SNPs and additional quality control exclusion criteria [low allele frequency (minor allele frequency<5%), significant departure from Hardy–Weinberg equilibrium (P<1×10−6), low genotyping call rate (<98%)]. All beta estimates and standard errors are standardized"
# Run functions
addTable(sheet, table)
headerFunc(title_name, sheet)
InfoFunc(Info_text, sheet)

# +++++++ Table +++++++
# Create new sheet
gwaUKBB="Table 6"
addWorksheet(wb, gwaUKBB)
# Add parameters
title_name="Table 6. Variants associated with the liability to UK Biobank participation"
sheet=gwaUKBB
table=gene_sel_ordered
Info_text="Listed are LD-independent SNPs reaching genome-wide significance in the weighted GWA analysis on the liability to UKBB participation, using the individual participation probabilities as the outcome of interest."
# Run functions
addTable(sheet, table)
headerFunc(title_name, sheet)
InfoFunc(Info_text, sheet)


# +++++++ Table +++++++
pheno_sel=subset(phenoSNPs, select=c(SNP, P, N, trait, p, n, dataset), p<5e-8)
names(pheno_sel)[names(pheno_sel) == 'P_PS'] <- 'p-value (UKBB participation)'
names(pheno_sel)[names(pheno_sel) == 'N'] <- 'effective N (UKBB participation)'
names(pheno_sel)[names(pheno_sel) == 'p'] <- 'p-value (other trait)'
names(pheno_sel)[names(pheno_sel) == 'n'] <- 'n (other trait)'
names(pheno_sel)[names(pheno_sel) == 'n'] <- 'n (other trait)'
# Create new sheet
gwaPhenoScan="Table 7"
addWorksheet(wb, gwaPhenoScan)
# Add parameters
title_name="Table 7. Summary of PhenoScanner results for lead SNPs associated with UK Biobank participations"
sheet=gwaPhenoScan
table=pheno_sel
Info_text="Shown are the results from Phenoscanner, exploring previously identified associations of lead SNPs linked to UK Biobank participation"
# Run functions
addTable(sheet, table)
headerFunc(title_name, sheet)
InfoFunc(Info_text, sheet)


# +++++++ Table +++++++
h2Est=readRDS(paste0(HOME,"/results/rds/h2outDF.rds"))
ldscExternalh2=merge(ldscExternalClean, h2Est, by="file", all.x=T)
ldscExternalSupp=subset(ldscExternalh2, select=c(labelClean, rg, rg_se, pval, h2, h2_se, intercept))
names(ldscExternalSupp)[names(ldscExternalSupp) == 'labelClean'] <- 'Trait name'
names(ldscExternalSupp)[names(ldscExternalSupp) == 'rg_se'] <- 'SE(rg)'
names(ldscExternalSupp)[names(ldscExternalSupp) == 'h2_se'] <- 'SE(h2)'

head(ldscExternalClean)
# Create new sheet
ldscUKBBpart="Table 8"
addWorksheet(wb, ldscUKBBpart)
# Add parameters
title_name="Table 8. LDSC regression between the liability to UKBB participation and other traits"
sheet=ldscUKBBpart
table=ldscExternalSupp
Info_text="Shown are the results from LDSC regression analyses, testing for genetic correlations between the liability to UKBB participation and other traits"
# Run functions
addTable(sheet, table)
headerFunc(title_name, sheet)
InfoFunc(Info_text, sheet)



# +++++++ Table +++++++
liabH2unweighted=subset(h2CombDiff, select=c(phenoClean, h2, h2_se, I, N), scale=="liability" & scheme=="unweighted")
names(liabH2unweighted)[names(liabH2unweighted) == 'h2'] <- 'h2 (standard)'
names(liabH2unweighted)[names(liabH2unweighted) == 'h2_se'] <- 'SE(h2) (standard)'
names(liabH2unweighted)[names(liabH2unweighted) == 'I'] <- 'I (standard)'
names(liabH2unweighted)[names(liabH2unweighted) == 'N'] <- 'N (standard)'

liabH2weighted=subset(h2CombDiff, select=c(phenoClean, h2, h2_se, I, N), scale=="liability" & scheme=="weighted")
names(liabH2weighted)[names(liabH2weighted) == 'h2'] <- 'h2 (weighted)'
names(liabH2weighted)[names(liabH2weighted) == 'h2_se'] <- 'SE(h2) (weighted)'
names(liabH2weighted)[names(liabH2weighted) == 'I'] <- 'I (weighted)'
names(liabH2weighted)[names(liabH2weighted) == 'N'] <- 'N (weighted)'

liabH2Diff=subset(h2CombDiff, select=c(phenoClean, h2, h2_se, pFDRh2), scale=="liability" & scheme=="h2diff")
names(liabH2Diff)[names(liabH2Diff) == 'h2'] <- 'h2 difference (standard - weighted)'
names(liabH2Diff)[names(liabH2Diff) == 'h2_se'] <- 'SE(h2 difference)'
names(liabH2Diff)[names(liabH2Diff) == 'pFDRh2'] <- 'P (h2 difference, FDR corrected)'


liabH2supp1=merge(liabH2unweighted, liabH2weighted, by="phenoClean", all=T, sort=F)
liabH2supp=merge(liabH2supp1, liabH2Diff, by="phenoClean", all=T, sort=F)
names(liabH2supp)[names(liabH2supp) == 'phenoClean'] <- 'Trait'

# Create new sheet
h2Supp="Table 9"
addWorksheet(wb, h2Supp)
# Add parameters
title_name="Table 9. SNP heritability estimates obtained from weighted and standard GWA analyses"
sheet=h2Supp
table=liabH2supp
Info_text="Shown are the LDSC heritability (h2) estimates, comparing h2 obtained from weighted GWA to h2 obtained from standard GWA"
# Run functions
addTable(sheet, table)
headerFunc(title_name, sheet)
InfoFunc(Info_text, sheet)


# +++++++ Table +++++++
# Create new sheet
rgSupp="Table 10"
addWorksheet(wb, rgSupp)
# Add parameters
title_name="Table 10. Weighted and standard genetic correlation estimates"
sheet=rgSupp
table=suppRGtableClean
Info_text="Shown are the LDSC regression estimates (rg), comparing rg obtained from weighted GWA to rg obtained from standard GWA"
# Run functions
addTable(sheet, table)
headerFunc(title_name, sheet)
InfoFunc(Info_text, sheet)


# +++++++ Table +++++++
MRDiff$Analysis=ifelse(MRDiff$model=="unweighted", "standard GWA", "weighted GWA")
MRsuppTable=subset(MRDiff, select=c(Analysis, exposure_clean, outcome_clean, b, se, pval, ZscoreDiff_pval_fdr))
names(MRsuppTable)[names(MRsuppTable) == 'exposure_clean'] <- 'Exposure'
names(MRsuppTable)[names(MRsuppTable) == 'outcome_clean'] <- 'Outcome'
names(MRsuppTable)[names(MRsuppTable) == 'b'] <- 'Beta (IVW MR)'
names(MRsuppTable)[names(MRsuppTable) == 'se'] <- 'Se (IVW MR)'
names(MRsuppTable)[names(MRsuppTable) == 'pval'] <- 'p-value (IVW MR)'
names(MRsuppTable)[names(MRsuppTable) == 'ZscoreDiff_pval_fdr'] <- 'p-value (FDR corrected) comparing weighted and standard MR estimate'

# Create new sheet
MRSupp="Table 11"
addWorksheet(wb, MRSupp)
# Add parameters
title_name="Table 11. Weighted and standard Mendelian Randomization"
sheet=MRSupp
table=MRsuppTable
Info_text=""
# Run functions
addTable(sheet, table)
headerFunc(title_name, sheet)
InfoFunc(Info_text, sheet)











################################################################
# ====================== Export table ==================
################################################################

# Create new styles
s <- createStyle(fgFill = "#FFFFFF")
h_info <- createStyle(halign = "left",
                      border = "BOTTOM", fontColour = "black", fgFill = "white", fontSize=16, textDecoration = "Bold", numFmt="TEXT", borderColour = "black")
info_info <- createStyle(halign = "left",
                         border = NULL, fontColour = "black", fgFill = "white", fontSize=14, textDecoration = NULL, numFmt="TEXT", wrapText=TRUE)
# Run loop
for(curr_sheet in names(wb)){
  addStyle(wb,sheet = curr_sheet, s, cols=1:40, rows=1:2000, gridExpand = TRUE)
  setColWidths(wb, sheet = curr_sheet, cols=1:40, widths = 20)
  addStyle(wb,sheet = curr_sheet, h_info, cols=ColStart:20, rows=RowHeader, gridExpand = TRUE)
  addStyle(wb,sheet = curr_sheet, info_info, cols=ColStart:5, rows=RowSubheaderStart, gridExpand = TRUE)
  mergeCells(wb,sheet = curr_sheet, cols = 2:100, rows = RowSubheaderStart:RowSubheaderEnds)
}

library( openxlsx)
openxlsx::saveWorkbook(wb, paste0(HOME,"/results/tables/weightingUKBB.xlsx"), overwrite = TRUE)
# Open File
openXL(wb)







