HOME='/data/sgg3/tabea/TS2021_UKBBweighting'
UKBB='/data/sgg3/data/UKBB'
GWA='/data/sgg3/tabea/GWAssumstats'
UKBBgeno='/data/sgg3/tabea/UKBBgeno'
LOCAL='githubProjects/TabeaSchoeler/TS2021_UKBBweighting'
R_libPaths=paste0('/data/sgg3/tabea/TS2021_UKBBweighting', "/programs/R")
.libPaths(R_libPaths)
load.lib=c('tidyverse', 'plyr', 'data.table', 'devtools', 'survey', 'foreign', 'gmodels', 'rdrop2', 'TwoSampleMR')
sapply(load.lib,require,character=TRUE)
drop_auth(rdstoken = paste0('/data/sgg3/tabea/TS2021_UKBBweighting', '/token.rds'))
drop_acc()
