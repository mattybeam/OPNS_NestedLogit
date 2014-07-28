varSave<-'../variables/'
ex.mods<-'modules/'
library('fExtremes')
library('plyr')
library('reshape2')
library('dplyr')
l_ply(dir(ex.mods), function(l) source(paste(ex.mods, l, sep="")))
