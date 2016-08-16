### R code from vignette source 'gui_tutorial.rnw'

###################################################
### code chunk number 1: gui_tutorial.rnw:820-823
###################################################
require(sdcMicro)
sdc <- createSdcObj(testdata2, keyVars=c('urbrur','roof','walls','water','electcon','sex'), 
	numVars=c('expend','income','savings'), w='sampling_weight', hhId='ori_hid')


###################################################
### code chunk number 2: gui_tutorial.rnw:828-829
###################################################
slotNames(sdc) 


###################################################
### code chunk number 3: gui_tutorial.rnw:872-873
###################################################
print(sdc, "risk")	


