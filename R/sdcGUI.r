#should be in zzz.R
#require(gWidgetsRGtk2)localSupp1_tmp
#sdcGUIenv <- new.env() starts in gui function

# just for test case while not in sdcMicro package
#require(sdcMicro)





sdcGUI <- function() {
  sdcGUIenv <- new.env()
  ## utility functions
# envionment with get and set functions
# not used, cause it ignores new env ... to use, remove ...x
  sdcGUIenvx <- function() {
    pos <- match("sdcGUIenv", search())
    if(is.na(pos)) {
      sdcGUIenv <- list()
      attach(sdcGUIenv, pos=length(search())-1)
      rm(sdcGUIenv)
      pos <- match("sdcGUIenv", search())
    }
    return(pos.to.env(pos))
  }
  
  putd <- function(x, value) {
    assign(x, value, envir=sdcGUIenv) # add () to sdcGUIenv
  }
  
  getd <- function(x, mode="any") {
    get(x, envir=sdcGUIenv, mode=mode, inherits=FALSE) # add () to sdcGUIenv
  }
  
  existd <- function(x, mode="any") {
    exists(x, envir=sdcGUIenv, mode=mode, inherits=FALSE) # add () to sdcGUIenv
  }
#Tooltip main window und select variables
  tt_selVar <- "Summary of the selected variables and their assignment"
  tt_print <- "Frequency print"
  tt_summary <- "Frequency summary"
  tt_ir <- "measure_risk Output"
  tt_vc <- "Configure your key variables"
  tt_ls1 <- "Local Suppression"
  tt_ld1 <- "Compute l-Diversity"
  tt_man <- "Run additional R commands"
  tt_pir <- "Histogram and ECDF of the individual risks"
  tt_noi <- "Add noise"
  tt_ma <- "Microaggregation of numeric variables"
  tt_rr <- "Recalculate Risk"
  tt_slider1 <- "Paramter k for risk computation"
  tt_slider2 <- "Paramter k2 for risk computation"
  tt_nmr <- "Numerical method risk"
  tt_pram1 <- "PRAM is a probabilistic, perturbative method which can be applied on categorical variables"
  tt_genstrat <- "Generate a strata variable"
#
  mosaic_check <- function(formX){
    xtmp <- ActiveDataSet()
    ft <- as.data.frame(ftable(xtmp[,formX]))
    ft <- ft[ft$Freq!=0,]
    if(nrow(ft)>40){ 
      plot(1,main="Two many classes for a nice mosaic plot!")
    }else{
      mosaic(as.formula(paste("~",paste(formX,collapse="+"),sep="")),data=xtmp,shade=FALSE)
    }
  }
  data(free1)
  data(testdata)
  # 
  
  .findHelpPage <- function(topic, package=NULL) {
    l <- list(topic=topic)
    if(!is.null(package))
      l$package <- package
    out <- do.call("help", l)
    if(length(out) == 0) return(NULL)
    
    pkgname <-  basename(dirname(dirname(out)))
    
    ## thanks to Josef L for this
    help.txt <- "" ## keep R CMD check happy  
    help.con <- textConnection("help.txt", "w", local = TRUE)
    tools::Rd2txt(utils:::.getHelpFile(out), out=help.con, package=pkgname,
        width=80L)
    close(help.con)
    
    return(list(x=help.txt,topic=topic, package=pkgname))
  }
  .insertHelpPage <- function(obj, x) {
    isSlow <- obj@toolkit@toolkit == "tcltk" || obj@toolkit@toolkit == "RGtk2"
    dispose(obj)       # clear
    
    out <- c()
    for(i in x) {
      if(grepl("^_\b",i)) {
        if(isSlow)
          out <- c(out, gsub("_\b","",i))
        else
          insert(obj, gsub("_\b","",i), font.attr=c(weight="bold"))
      } else {
        if(isSlow)
          out <- c(out,i)
        else
          insert(obj, i,font.attr=c(weight="normal"))
      }
    }
    if(isSlow)
      svalue(obj) <- out
    else
      insert(obj, "", do.newline=FALSE, where="beginning")              
  }
  helpR <- function(topic){
    print(help(topic))
  }
  ActiveDataSet <- function(name) {
    if( missing(name) ) {
      getd("activeDataSet")
    } else {
      if( is.matrix(get(name)) ) {
        putd("activeDataSet", data.frame(get(name), stringsAsFactors=FALSE))
      } else {
        putd("activeDataSet", get(name))
      }
      putd("dataSetName", name)
    }
  }
  
# update ActiveDataSet
  updateActiveDataSet <- function(x, ...) {
    if( is.matrix(x) ) {
      factorizeVars = FALSE
      x <- data.frame(x, stringsAsFactors=FALSE)
      xtmp <- getd("numIndex")
      xtmp <- c(xtmp, getd("wIndex"))
      for( i in 1:length(xtmp) ) {
        try( x[,xtmp[i]] <- as.numeric(x[,xtmp[i]]), silent=TRUE )
      }
      if( factorizeVars ) {
        xtmp <- getd("keyIndex")
        for( i in 1:length(xtmp) ) {
          try( x[,xtmp[i]] <- as.factor(x[,xtmp[i]]), silent=TRUE )
        }
      }
    }
    putd("activeDataSet", x)
    if(!is.null(getd("keyIndex")))
      freqCalcIndivRisk()
  }
  
  # Script
  #
  Script <- function(name, ...) {
    if( missing(name) ) {
      getd("activeScript")
    } else { 
      putd("activeScript", name)
    }
  }
  
  Script.new <- function(...) {
    xtmp <- list(cmd=c())
    putd("activeScript", xtmp)
  }
  
  Script.add <- function(cmd, ...) {
    xtmp <- Script()
    xtmp$cmd[length(xtmp$cmd)+1] = cmd
    Script(xtmp)
  }
  
  Script.run <- function(xscr, ...) {
    if( existd("activeDataSet") ) {
      if( missing(xscr) ) {
        xcmd <- Script()
        xcmd <- xcmd$cmd
      } else {
        xcmd <- xscr
      }
      xprogress = gwindow("please wait", width=180, height=40, parent=window)
      glabel("... script running ...", container=xprogress)
      for( i in 1:length(xcmd) ) {
        ytmp <- xcmd[i]
        eval(parse(text=ytmp), envir=sdcGUIenv)
        #xtmp <- function() { eval(parse(text=ytmp)) }
        #do.call(xtmp, list(), envir=sdcGUIenv)
      }
      dispose(xprogress)
    } else {
      gmessage("Run not possible, because no active data set found.", title="Attention", icon="error", parent=window)
    }
  }
  
  parseVar <- function(x, ...) {
    s <- "c("
    for ( i in 1:length(x) ) {
      s <- paste(s, x[i])
      if (i < length(x)) {
        s <- paste(s, ",")
      }
    }
    s <- paste(s, ")")
    return(s)
  }
  
  parseVarStr <- function(x, ...) {
    s <- "c("
    for ( i in 1:length(x) ) {
      s <- paste(s, "'", x[i], "'", sep="")
      if (i < length(x)) {
        s <- paste(s, ",", sep="")
      }
    }
    s <- paste(s, ")", sep="")
    return(s)
  }
  
  # getIndex to get the col index of categorical, numerical and weight vars
  getIndex <- function(x, ...) {
    ads <- names(ActiveDataSet())
    ord <- c()
    for( i in 1:length(x) ) {
      for( j in 1:length(ads) ) {
        if( x[i]==ads[j] ) {
          ord <- c(ord, j)
        }
      }
    }
    return(ord)
  }
  
  # function for button ir_button (plotIndivRisk)
  # indivRiskGroup function
  # x ... object of class indivRisk
  # y ... object of class freqCalc
  plotIndivRisk <- function(y, x, ...) {
    method = "histogram"
    putd("method","histogram")
    mu <- 0.0025
    sd <- 0.5
    s2 <- 0.5
    mu.old <- mu
    sd.old <- sd
    s2.old <- s2
    maxsd <- 1/length(x$rk) * (sum(x$fk * x$rk)) *100
    n1 <- x$knames[1]     ## next, the plot of column names of keys
    if( length(x$knames) > 1 ){
      for(i in 2:length(x$knames)){
        n1 <- paste(n1, "x", x$knames[i])
      }
    }
    norm.refresh <- function(...) {
      method = getd("method")
      mu <- as.numeric(evalq(svalue(smu)))
      sd <- as.numeric(evalq(svalue(ssd)))
      s2 <- as.numeric(evalq(svalue(ss2)))
      if (mu != mu.old) {
        s2 <- round(length(which(x$rk > mu)))
        sd <- 1/length(x$rk) * (sum(x$fk[x$rk < mu] * x$rk[x$rk < mu]) + mu*sum(x$fk[x$rk>mu])) * 100
        try(svalue(ssd)<-sd)
        try(svalue(ss2)<-s2)
        sd.old <<- sd
        s2.old <<- s2
      }
      if (sd != sd.old) {
        sd <- as.numeric(evalq(tclvalue(s2), envir = slider.env))
        s2 <- length(which(x$rk > mu))
        try(svalue(ssd)<-sd)
        try(svalue(ss2)<-s2)
        sd.old <<- sd
        s2.old <<- s2
      }
      if (s2 != s2.old) {
        s2 <- as.numeric(evalq(tclvalue(s2), envir = slider.env))
        sd <- 1/length(x$rk) * (sum(x$fk * x$rk) + 0.02*sum(x$fk))
        try(svalue(ssd)<-sd)
        sd.old <<- sd
        s2.old <<- length(which(x$rk > mu))
      }
      if( method == "histogram" ){
        hist(x$rk, main=n1,freq=TRUE, xlab="individual risk", col="yellow")
        abline(v=mu, col="blue", lwd=2)
      }
      if( method == "ecdf" ){
        plot(ecdf(x$rk), main="ecdf of individual risk", xlab="individual risk")
        abline(v=mu, col="blue", lwd=2)
      }
    }
    plot1 <- function(method){
      if( method == "histogram" ){
        putd("method","histogram")
        hist(x$rk, main=n1,freq=TRUE, xlab="individual risk", col="yellow")
        abline(v=mu, col="blue", lwd=2)
      }
      if( method == "ecdf" ){
        putd("method","ecdf")
        plot(ecdf(x$rk), main="ecdf of individual risk", xlab="individual risk")
        abline(v=as.numeric(evalq(svalue(smu))), col="blue", lwd=2)
      }
    }
    win = gwindow("Individual Risk Adjustments", parent=window)
    mainGroup1 = ggroup(container=win, horizontal=FALSE)
    method = "histogram"
    sliderGroup = ggroup(container=mainGroup1, horizontal=FALSE)
    tmp = gframe("Individual Risk Threshold", container=sliderGroup)
    smu = gslider(from=0, to=max(x$rk), by=0.001, value=mu, handler=norm.refresh)
    add(tmp, smu, expand=TRUE)
    tmp = gframe("Re-identification Rate", container=sliderGroup)
    ssd = gslider(from=0, to=maxsd, by=0.01, value=sd, handler=norm.refresh)
    add(tmp, ssd, expand=TRUE)
    tmp = gframe("Unsafe Records", container=sliderGroup)
    ss2 = gslider(from=0, to=length(x$rk), by=1, value=s2, handler=norm.refresh)
    add(tmp, ss2, expand=TRUE)
    gbutton("Show ecdf", container=mainGroup1, handler=function(x,...) plot1("ecdf"))
    gbutton("Show histogram", container=mainGroup1, handler=function(x,...) plot1("histogram"))
    add(mainGroup1, ggraphics())
    if( method == "histogram" ){
      try(hist(x$rk, main=n1,freq=TRUE, xlab="individual risk", col="yellow"), silent=TRUE)
      try(abline(v=mu, col="blue", lwd=2), silent=TRUE)
    }
  }
  
  # FreqCalc and indivRisk calculation - freqCalc()
  #                                    - indivRisk()
  # TODO: not needed - save freqCalcIndivRisk for script/history
  freqCalcIndivRisk <- function(...) {
    xprogressFQ = gwindow("please wait", width=250, height=140, parent=window)
    glabel("... calculating ...", container=xprogressFQ)
    # freqCalc
    f1 <- freqCalc(ActiveDataSet(), keyVars=getd("keyIndex"), w=getd("wIndex"))
    #-- Start - print.freqCalc
    tmp <- capture.output(print.freqCalc(f1))
    fc_print <- getd("fc_print")
    svalue(fc_print) <- tmp[1]
    if(existd("ffc_print")){
      ffc_print <- getd("ffc_print")
      if(isExtant(ffc_print)){
        svalue(ffc_print) <- tmp[1]
        if( length(tmp)> 1 ) {
          for( i in 2:length(tmp) ) {
            insert(ffc_print, tmp[i])
          }
       }
      } 
    }
    if( length(tmp)> 1 ) {
      for( i in 2:length(tmp) ) {
        insert(fc_print, tmp[i])
      }
    }
    #-- End - print.freqCalc
    #-- Start - summary.freqCalc
    tmp <- capture.output(summary.freqCalc(f1))
    svalue(fc_summary) <- tmp[1]
    if( length(tmp)> 1 ) {
      for( i in 2:length(tmp) ) {
        if( !tmp[i] == "" ) {
          insert(fc_summary, tmp[i])
        }
      }
    }
    #-- End - summary.freqCalc
    putd("freqCalc", f1)
    # indivRisk
    i1 <- indivRisk(f1)
    #-- Start - print.indivRisk
    
    #Measure Risk Funktion
    if(length(getd("keyIndex"))>1){
      m1 <- measure_risk(ActiveDataSet(), keyVars=getd("keyIndex"), w=getd("wIndex"),hid=getd("hIndex"))
      putd("measure_risk_res",m1)
      tmp <- capture.output(print(m1))
    }else{
      tmp <- capture.output(print.indivRisk(i1))
    }
    svalue(ir_print) <- tmp[1]
    if( length(tmp)> 1 ) {
      for( i in 2:length(tmp) ) {
        insert(ir_print, tmp[i])
      }
    }
    #-- End - print.indivRisk
    putd("indivRisk", i1)
    dispose(xprogressFQ)
  }
  
  # TODO: var to factor tmp
  varToFactor_tmp <- function(var){
    Script.add(paste("varToFactor_tmp(", 
            parseVarStr(var), 
            ")", sep=""))  
    Var <- getIndex(var)
#	print(Var)
    xtmp <- ActiveDataSet()
    xtmp[,Var] <- as.factor(xtmp[,Var])
#	print(head(xtmp))
    updateActiveDataSet(xtmp)
  }
  varToNumeric_tmp <- function(var){
    Var <- getIndex(var)
#	print(Var)
    xtmp <- ActiveDataSet()
    suppressWarnings(tmpvar <- as.numeric(as.character(xtmp[,Var])))
    if(sum(is.na(tmpvar))>sum(is.na(xtmp[,Var]))){
      if(existd("rb")){
        rb <- getd("rb")
        keyname <- getd("keyVars")
        ind <- which(keyname==var)
        svalue(rb[[ind]]) <- "Factor"
        gr1_window <- getd("gr1_window")
        gmessage("Variable cannot be changed to numeric!", title="Information", icon="info", parent=gr1_window)
      }
    }else{
      xtmp[,Var] <- tmpvar
      updateActiveDataSet(xtmp)
      Script.add(paste("varToNumeric_tmp(", 
              parseVarStr(var), 
              ")", sep=""))
    }
#	print(head(xtmp))
    
  }
  pram_tmp <- function(var,strata_var=NULL){
    if(length(strata_var)>0){
      strata_var <- parseVarStr(strata_var)
      Script.add(paste("pram_tmp(", 
              parseVarStr(var),",",strata_var, 
              ")", sep="")) 
    }else 
      Script.add(paste("pram_tmp(", 
              parseVarStr(var), 
              ")", sep=""))
    xtmp <- ActiveDataSet()
    tem <- pram_strata(xtmp,variables=var,strata_variables=strata_var)
    xtmp[,var] <- tem[,paste(var,"_pram",sep="")]
    updateActiveDataSet(xtmp)
  }
  #LocalSuppression
  localSuppression_tmp <- function(k, importance, redo=FALSE) {
    if( !redo ) {
      Script.add(paste("localSuppression_tmp(", parseVar(k), ", ", parseVar(importance), ", redo=TRUE)", sep=""))
      xprogress = gwindow("please wait", width=180, height=40)
      glabel("... script running ...", container=xprogress)
    }
    x <- ActiveDataSet()
    keyVars <- getd("keyIndex")
    w <- getd("wIndex")
    k <- k
    importance <- importance
    l1 <- localSuppression(x=x[,c(keyVars,w),drop=FALSE], keyVars=1:length(keyVars), w=length(keyVars)+1, k=k, importance=importance)
    x[,keyVars] <- l1$xAnon[,1:length(keyVars)]
    updateActiveDataSet(x)
    if( !redo ) {
      dispose(xprogress)
    }
    #freqCalcIndivRisk()
  }
  
  
  # microaggregation_tmp - microaggregation()
  # TODO: done - save microaggregation for script/history
  microaggregation_tmp <- function(aggr, method, vars,strata_variables=NULL, redo=FALSE) {
    if( !redo ) {
      if(length(strata_variables)==0){
        Script.add(paste("microaggregation_tmp(", parseVar(aggr), ", ",
                parseVarStr(method), ", ", parseVarStr(vars), ",redo=TRUE)", sep=""))
        strata_variables <- NULL
      }else
        Script.add(paste("microaggregation_tmp(", parseVar(aggr), ", ",
                parseVarStr(method), ", ", parseVarStr(vars),",",parseVarStr(strata_variables), ",redo=TRUE)",
                sep=""))
      putd("oldCols", ActiveDataSet()[,vars,drop=FALSE])
    }
    xtmp <- ActiveDataSet()
    if(length(vars)==1){
      vars <- c(vars,"dummyvarforma")
      xtmp[,"dummyvarforma"] <- 1
      dummy <- TRUE
    }else
      dummy <- FALSE
    xtmp[,vars] <- microaggregation(xtmp, method=method, aggr=aggr,variables=vars,strata_variables=strata_variables)$mx
    if(dummy){
      xtmp <- xtmp[,-which(colnames(xtmp)=="dummyvarforma")]
      vars <- vars[1]
    }
    updateActiveDataSet(xtmp)
    #freqCalcIndivRisk()
    if( !redo ) {
      putd("newCols", ActiveDataSet()[,vars,drop=FALSE])
      nm_risk_print_function()
    }
  }
  ls4 <- function(...){
    nm2_window = gwindow("Local Suppression", width=230, parent=window,height=400)
    nb <- gnotebook(container=nm2_window, closebuttons=FALSE)
    #Main
    ls3_pars = ggroup(container=nb, horizontal=FALSE,label="Function")
    #Help
    t <- gtext(container=nb, label="Help", expand=TRUE)
    l <- .findHelpPage("localSuppression", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    svalue(nb) <- 1
    
    tmp = gframe("k-Anonymity parameter", container=ls3_pars)
    y_tmp <- names(ActiveDataSet())[sort(getd("keyIndex"))]
    x = gslider(2, 12, by=1)
    add(tmp, x, expand=TRUE)
    y <- list()
    for( i in 1:length(y_tmp) ) {
      fns <- eval(parse(text=paste("
      function(...){
        if(existd(\"impslider\")){      
          ii <- ",i,"
          y <- getd(\"impslider\")
          yval <- as.numeric(as.vector(lapply(y,svalue)))
          valtmp <- c(1:length(y))[-yval[ii]]
          yval[-ii][order(yval[-ii])] <- valtmp
          yval[yval[ii]<yval] <- yval[yval[ii]<yval]+1
          for(i in 1:length(yval)){
            svalue(y[[i]]) <- yval[i]
          }
        } 
      }
      ",sep="")))
      y[[i]] <- gslider(from=1, to=length(y_tmp), by=1, value=i,handler=fns)
    }
    putd("impslider",y)
    tmp = gframe("Importance of keyVars", container=ls3_pars, horizontal=FALSE)
    for( i in 1:length(y_tmp) ) {
      tmpg = ggroup(container=tmp)
      tmpt = glabel(y_tmp[i])
      add(tmpg, tmpt, expand=TRUE)
      add(tmpg, y[[i]], expand=TRUE)
    }
    gseparator(container=ls3_pars)
    ls3_parsButtonGroup = ggroup(container=ls3_pars)
    addSpring(ls3_parsButtonGroup)
    gbutton("Ok", container=ls3_parsButtonGroup,
        handler=function(h,...) {
          importance <- as.numeric(as.vector(lapply(y,svalue)))
          k <- svalue(x)
          localSuppression_tmp(k, importance)
          dispose(nm2_window)
#          }
        })
    gbutton("Cancel", container=ls3_parsButtonGroup, handler=function(h,...) { dispose(nm2_window) })
    gbutton("Help", container=ls3_parsButtonGroup, handler=function(h,...) { helpR("localSuppression") })
    
    
  }
  # function for nm_button2
  # globalRecodeGroup-numericalMethods function
nm2 <- function(...) {
  #Tooltip Microaggegation
  tt_aggr <- "aggregation level (default=3)"
  tt_method <- "mdav, rmd, pca, clustpppca, influence"
  tt_ltr <- "Add selected variable(s)"
  tt_rtl <- "Remove selected variable(s)" 
  tt_ltr1 <- "Add selected strata variable(s)"
  tt_rtl1 <- "Remove selected strata variable(s)"
  lTOr <- function(h, ...) {
    if( length(h)>0 ) {
      if( length(selTab[])==1 ) {
        if( is.na(selTab[]) ) {
          selTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
        } else {
          selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
        }
      } else {
        selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
      }
      if( length(h)==length(varTab[]) ) {
        varTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
      } else {
        xtmp <- c()
        for( i in 1:length(varTab[]) ) {
          for( j in 1:length(h) ) {
            if( varTab[][i]==h[j] ) {
              xtmp <- c(xtmp, i)
            }
          }
        }
        varTab[,] <- data.frame(vars=varTab[-xtmp], stringsAsFactors=FALSE)
      }
    }
  }
  rTOl <- function(h, ...) {
    if( length(h)>0 ) {
      if( length(varTab[])==1 ) {
        if( is.na(varTab[]) ) {
          varTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
        } else {
          varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
        }
      } else {
        varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
      }
      if( length(h)==length(selTab[]) ) {
        selTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
      } else {
        xtmp <- c()
        for( i in 1:length(selTab[]) ) {
          for( j in 1:length(h) ) {
            if( selTab[][i]==h[j] ) {
              xtmp <- c(xtmp, i)
            }
          }
        }
        selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
      }
    }
  }
  lTOr1 <- function(h, ...) {
    if( length(h)>0 ) {
      if( length(selTab1[])==1 ) {
        if( is.na(selTab1[]) ) {
          selTab1[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
        } else {
          selTab1[,] <- data.frame(vars=c(selTab1[], h), stringsAsFactors=FALSE)
        }
      } else {
        selTab1[,] <- data.frame(vars=c(selTab1[], h), stringsAsFactors=FALSE)
      }
      if( length(h)==length(sTab[]) ) {
        sTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
      } else {
        xtmp <- c()
        for( i in 1:length(sTab[]) ) {
          for( j in 1:length(h) ) {
            if( sTab[][i]==h[j] ) {
              xtmp <- c(xtmp, i)
            }
          }
        }
        sTab[,] <- data.frame(vars=sTab[-xtmp], stringsAsFactors=FALSE)
      }
    }
  }
  rTOl1 <- function(h, ...) {
    if( length(h)>0 ) {
      if( length(sTab[])==1 ) {
        if( is.na(sTab[]) ) {
          sTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
        } else {
          sTab[,] <- data.frame(vars=c(sTab[], h), stringsAsFactors=FALSE)
        }
      } else {
        sTab[,] <- data.frame(vars=c(sTab[], h), stringsAsFactors=FALSE)
      }
      if( length(h)==length(selTab1[]) ) {
        selTab1[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
      } else {
        xtmp <- c()
        for( i in 1:length(selTab1[]) ) {
          for( j in 1:length(h) ) {
            if( selTab1[][i]==h[j] ) {
              xtmp <- c(xtmp, i)
            }
          }
        }
        selTab1[,] <- data.frame(vars=selTab1[-xtmp], stringsAsFactors=FALSE)
      }
    }
  }
  
  nm2_window = gwindow("Microaggregation", width=230, parent=window,height=600)
  nb <- gnotebook(container=nm2_window, closebuttons=FALSE)
  #Main
  nm2_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
  #Help
  t <- gtext(container=nb, label="Help", expand=TRUE)
  l <- .findHelpPage("microaggregation", "sdcMicro")
  x <- l$x
  .insertHelpPage(t, x)
  svalue(nb) <- 1
  
  tmp = gframe("Aggregation level", container=nm2_windowGroup, horizontal=FALSE)
  ntmp = ggroup(container=tmp)
  aggrSel = gslider(from=2, to=20, by=1)
  tooltip(aggrSel) <- tt_aggr
  svalue(aggrSel) <- 3
  add(ntmp, aggrSel, expand=TRUE)
  tmp = gframe("Method", container=nm2_windowGroup, horizontal=FALSE)
  methodSel = gdroplist(c("mdav","rmd", "pca", "clustpppca", "influence"))
  tooltip(methodSel) <- tt_method
  add(tmp, methodSel)
  tmp = gframe("Variable selection", container=nm2_windowGroup)
  numVars <- c()
  xtmp <- ActiveDataSet()
  # just use all numerical vars
  #for( i in 1:dim(xtmp)[2] ) {
  #	if( is.numeric(xtmp[,i]) & names(xtmp)[i] != getd("wVars") ) {
  #		numVars <- c(numVars, names(xtmp)[i])
  #	}
  #}
  numVars <- getd("numVars")
  varTab = gtable(data.frame(vars=numVars, stringsAsFactors=FALSE), multiple=TRUE)
  size(varTab) <- c(120,200)
  add(tmp, varTab)
  btmp = ggroup(container=tmp, horizontal=FALSE)
  addSpring(btmp)
  b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr(svalue(varTab)) })
  b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl(svalue(selTab)) })
  tooltip(b1) <- tt_ltr
  tooltip(b2) <- tt_rtl
  addSpring(btmp)
  selTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
  size(selTab) <- c(120,200)
  add(tmp, selTab)
  
  
  tmp = gframe("Strata Variable selection", container=nm2_windowGroup)
  sVars <- c()
  xtmp <- ActiveDataSet()
  # just use all numerical vars
  #for( i in 1:dim(xtmp)[2] ) {
  #	if( is.numeric(xtmp[,i]) & names(xtmp)[i] != getd("wVars") ) {
  #		numVars <- c(numVars, names(xtmp)[i])
  #	}
  #}
  sVars <- getd("sVars")
  keyVars <- getd("keyVars")
  sTab = gtable(data.frame(vars=c(sVars,keyVars), stringsAsFactors=FALSE), multiple=TRUE)
  size(sTab) <- c(120,200)
  add(tmp, sTab)
  btmp = ggroup(container=tmp, horizontal=FALSE)
  addSpring(btmp)
  b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr1(svalue(sTab)) })
  b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl1(svalue(selTab1)) })
  tooltip(b1) <- tt_ltr1
  tooltip(b2) <- tt_rtl1
  addSpring(btmp)
  selTab1 = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
  size(selTab1) <- c(120,200)
  add(tmp, selTab1)
  
  
  gseparator(container=nm2_windowGroup)
  nm2_windowButtonGroup = ggroup(container=nm2_windowGroup)
  addSpring(nm2_windowButtonGroup)
  gbutton("Ok", container=nm2_windowButtonGroup,
      handler=function(h,...) {
        aggrVal <- as.numeric(svalue(aggrSel))
        if( length(selTab[])<1 | any(is.na(selTab[])) ) {
          gmessage("You need to select at least 1 variable!", title="Information", icon="info", parent=nm2_window)
        } else {
          microaggregation_tmp(aggrVal, svalue(methodSel), vars=selTab[],strata_variables=selTab1[])
          dispose(nm2_window)
        }
      })
  gbutton("Cancel", container=nm2_windowButtonGroup, handler=function(h,...) { dispose(nm2_window) })
  gbutton("Help", container=nm2_windowButtonGroup, handler=function(h,...) { helpR("microaggregation") })
}
ldiv1 <- function(...) {
  tt_ltr <- "Add selected variable(s)"
  tt_rtl <- "Remove selected variable(s)" 
  tt_slider1 <- "l_recurs_c Parameter"
  lTOr <- function(h, ...) {
    if( length(h)>0 ) {
      if( length(selTab[])==1 ) {
        if( is.na(selTab[]) ) {
          selTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
        } else {
          selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
        }
      } else {
        selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
      }
      if( length(h)==length(varTab[]) ) {
        varTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
      } else {
        xtmp <- c()
        for( i in 1:length(varTab[]) ) {
          for( j in 1:length(h) ) {
            if( varTab[][i]==h[j] ) {
              xtmp <- c(xtmp, i)
            }
          }
        }
        varTab[,] <- data.frame(vars=varTab[-xtmp], stringsAsFactors=FALSE)
      }
    }
  }
  rTOl <- function(h, ...) {
    if( length(h)>0 ) {
      if( length(varTab[])==1 ) {
        if( is.na(varTab[]) ) {
          varTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
        } else {
          varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
        }
      } else {
        varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
      }
      if( length(h)==length(selTab[]) ) {
        selTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
      } else {
        xtmp <- c()
        for( i in 1:length(selTab[]) ) {
          for( j in 1:length(h) ) {
            if( selTab[][i]==h[j] ) {
              xtmp <- c(xtmp, i)
            }
          }
        }
        selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
      }
    }
  }
  
  nm2_window = gwindow("l-diversity", width=230, parent=window,height=600)
  nb <- gnotebook(container=nm2_window, closebuttons=FALSE)
  #Main
  nm2_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
  #Help
  t <- gtext(container=nb, label="Help", expand=TRUE)
  l <- .findHelpPage("measure_risk", "sdcMicro")
  x <- l$x
  .insertHelpPage(t, x)
  svalue(nb) <- 1
  tmp = gframe("l Recursive Constant", container=nm2_windowGroup, horizontal=FALSE)
  recconst = gslider(from=1, to=10, by=1, value=2)
  tooltip(recconst) <- tt_slider1
  enabled(recconst) = TRUE
  add(tmp, recconst, expand=TRUE)
  
  tmp = gframe("Choose sensitive variable(s)", container=nm2_windowGroup, horizontal=FALSE)
  xtmp <- ActiveDataSet()
  # just use all numerical vars
  #for( i in 1:dim(xtmp)[2] ) {
  #	if( is.numeric(xtmp[,i]) & names(xtmp)[i] != getd("wVars") ) {
  #		numVars <- c(numVars, names(xtmp)[i])
  #	}
  #}
  numVars <- getd("numVars")
  keyVars <- getd("keyVars")
  hVars <- getd("hVars")
  wVars <- getd("wVars")
  sVars <- getd("sVars")
  posssensVars <- colnames(xtmp)[!colnames(xtmp)%in%c(numVars,keyVars,hVars,wVars,sVars)]
  
  varTab = gtable(data.frame(vars=posssensVars, stringsAsFactors=FALSE), multiple=TRUE)
  size(varTab) <- c(120,200)
  add(tmp, varTab)
  btmp = ggroup(container=tmp, horizontal=TRUE)
  addSpring(btmp)
  b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr(svalue(varTab)) })
  b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl(svalue(selTab)) })
  tooltip(b1) <- tt_ltr
  tooltip(b2) <- tt_rtl
  addSpring(btmp)
  selTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
  size(selTab) <- c(120,200)
  add(tmp, selTab)
  
  gseparator(container=nm2_windowGroup)
  nm2_windowButtonGroup = ggroup(container=nm2_windowGroup)
  addSpring(nm2_windowButtonGroup)
  gbutton("Ok", container=nm2_windowButtonGroup,
      handler=function(h,...) {
        if( length(selTab[])<1 | any(is.na(selTab[])) ) {
          gmessage("You need to select at least 1 variables!", title="Information", icon="info", parent=nm2_window)
        } else {
          mr <- ldiversity(xtmp,keyVars=keyVars,ldiv_index=selTab[],l_recurs_c=svalue(recconst))
          dispose(nm2_window)
          ldiverg_window = gwindow("l-diversity", width=520, parent=window,height=400)
          nb <- gnotebook(container=ldiverg_window, closebuttons=FALSE)
          #Main
          nm2_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
          #Help
          t <- gtext(container=nb, label="Help", expand=TRUE)
          l <- .findHelpPage("ldiversity", "sdcMicro")
          x <- l$x
          .insertHelpPage(t, x)
          svalue(nb) <- 1
          tmp = gframe("Output", container=nm2_windowGroup, horizontal=FALSE)
          gte <- gtext("", container=tmp, height=350, width=500)
          svalue(gte) <- capture.output(print(mr),append=FALSE)
          gseparator(container=nm2_windowGroup)
          nm2_windowButtonGroup = ggroup(container=nm2_windowGroup)
          addSpring(nm2_windowButtonGroup)
          gbutton("Ok", container=nm2_windowButtonGroup,handler=function(h,...)dispose(ldiverg_window))
          gbutton("Help", container=nm2_windowButtonGroup, handler=function(h,...) { helpR("ldiversity") })
        }
      })
  gbutton("Cancel", container=nm2_windowButtonGroup, handler=function(h,...) { dispose(nm2_window) })
  gbutton("Help", container=nm2_windowButtonGroup, handler=function(h,...) { helpR("ldiversity") })
}

  # addNoise_tmp - addNoise()
  # TODO: done - save addNoise for script/history
  addNoise_tmp <- function(noise, method, vars, redo=FALSE) {
    if( !redo ) {
      Script.add(paste("addNoise_tmp(", parseVar(noise), ", ",
              parseVarStr(method), ", ", parseVarStr(vars), ", redo=TRUE)", sep=""))
      putd("oldCols", ActiveDataSet()[,vars,drop=FALSE])
    }
    # with just 1 var, create fake-matrix, execute function and delete fake
    if( length(vars)==1 ) {
      xtmp <- ActiveDataSet()
      x1tmp <- cbind(0, xtmp[,vars])
      xtmp[, vars] <- addNoise(x1tmp, noise=noise, method=method)$xm[,2,drop=FALSE]
      updateActiveDataSet(xtmp)
      freqCalcIndivRisk()
    } else {
      xtmp <- ActiveDataSet()
      xtmp[, vars] <- addNoise(xtmp[,vars], noise=noise, method=method)$xm
      updateActiveDataSet(xtmp)
      #freqCalcIndivRisk()
    }
    if( !redo ) {
      putd("newCols", ActiveDataSet()[,vars])
      nm_risk_print_function()
    }
  }
  
  # function for nm_button1
  # globalRecodeGroup-numericalMethods function
  nm1 <- function(...) {
    #ToolTip Addnoise Window
    tt_noise <- "amount of noise (in percentages)"
    tt_method <- "choose between additive and correlated2"
    tt_ltr <- "Add selected variable(s)"
    tt_rtl <- "Remove selected variable(s)"
    lTOr <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab[])==1 ) {
          if( is.na(selTab[]) ) {
            selTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(varTab[]) ) {
          varTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(varTab[]) ) {
            for( j in 1:length(h) ) {
              if( varTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          varTab[,] <- data.frame(vars=varTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    rTOl <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(varTab[])==1 ) {
          if( is.na(varTab[]) ) {
            varTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab[]) ) {
          selTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab[]) ) {
            for( j in 1:length(h) ) {
              if( selTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    nm1_window = gwindow("Add noise", width=230, parent=window)
    nb <- gnotebook(container=nm1_window, closebuttons=FALSE)
    #Main
    nm1_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
    tmp = gframe("Noise", container=nm1_windowGroup, horizontal=FALSE)
    #Help
    t <- gtext(container=nb, label="Help", expand=TRUE)
    l <- .findHelpPage("addNoise", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    svalue(nb) <- 1
    ntmp = ggroup(container=tmp)
    glabel("Value between 0 and 2000", container=ntmp)
    noiseSel = gedit()
    svalue(noiseSel) <- "150"
    tooltip(noiseSel) <- tt_noise
    add(ntmp, noiseSel)
    tmp = gframe("Method", container=nm1_windowGroup, horizontal=FALSE)
    methodSel = gdroplist(c("correlated2","additive"))
    tooltip(methodSel) <- tt_method
    add(tmp, methodSel)
    tmp = gframe("Variable selection", container=nm1_windowGroup)
    numVars <- c()
    xtmp <- ActiveDataSet()
    # not all vars, just numerical vars
    #for( i in 1:dim(xtmp)[2] ) {
    #	if( class(xtmp[,i])=="numeric" & names(xtmp)[i] != getd("wVars") ) {
    #		numVars <- c(numVars, names(xtmp)[i])
    #	}
    #}
    numVars <- getd("numVars")
    varTab = gtable(data.frame(vars=numVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(varTab) <- c(120,200)
    add(tmp, varTab)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr(svalue(varTab)) })
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl(svalue(selTab)) })
    tooltip(b1) <- tt_ltr
    tooltip(b2) <- tt_rtl
    addSpring(btmp)
    selTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab) <- c(120,200)
    add(tmp, selTab)
    gseparator(container=nm1_windowGroup)
    nm1_windowButtonGroup = ggroup(container=nm1_windowGroup)
    addSpring(nm1_windowButtonGroup)
    gbutton("Ok", container=nm1_windowButtonGroup,
        handler=function(h,...) {
          noise <- as.numeric(svalue(noiseSel))
          if( !is.numeric(noise) | is.na(noise) ) {
            gmessage("Noise needs to be a numeric value!", title="Information", icon="info", parent=nm1_window)
          } else {
            if( length(selTab[])==0 | any(is.na(selTab[])) ) {
              gmessage("You need to select at least 1 variable!", title="Information", icon="info", parent=nm1_window)
            } else {
              addNoise_tmp(noise, svalue(methodSel), selTab[])
              dispose(nm1_window)
            } 
          }
        })
    gbutton("Cancel", container=nm1_windowButtonGroup, handler=function(h,...) { dispose(nm1_window) })
    gbutton("Help", container=nm1_windowButtonGroup, handler=function(h,...) { helpR("addNoise") })
  }
  
  # needed sub functions
  # TODO: done - save rename for script/history
  renameVars_tmp <- function(v, h, newName, redo=FALSE) {
    if( !redo ) {
      Script.add(paste("renameVars_tmp(", parseVarStr(v), ", ",
              parseVarStr(h), ", ", parseVarStr(newName), ", redo=TRUE)", sep=""))
    }
    xtmp <- ActiveDataSet()
    levels(xtmp[,v]) <- ifelse(levels(xtmp[,v])==h, newName, levels(xtmp[,v]))
    updateActiveDataSet(xtmp)
    #freqCalcIndivRisk()
  }
  # TODO: done - save group for script/history
  groupVars_tmp <- function(v, h, newName, redo=FALSE) {
    if( !redo ) {
      Script.add(paste("groupVars_tmp(", parseVarStr(v), ", ",
              parseVarStr(h), ", ", parseVarStr(newName), ", redo=TRUE)", sep=""))
    }
    xtmp <- ActiveDataSet()
    for( i in 1:length(h) ) {
      levels(xtmp[,v]) <- ifelse(levels(xtmp[,v])==h[i], newName, levels(xtmp[,v]))
    }
    updateActiveDataSet(xtmp)
    #freqCalcIndivRisk()
  }
  # group and rename variables
  # globalRecodeGroup function
  
  # globalRecode_tmp - globalRecode()
  # TODO: replace cut with globalRecode as soon as it is corrected
  # TODO: done - save globalRecode for script/history
  globalRecode_tmp <- function(var, breaks, labels, redo=FALSE) {
    if( !redo ) {
      Script.add(paste("globalRecode_tmp(", parseVarStr(var), ", ",
              parseVar(breaks), ", ", parseVarStr(labels), ", redo=TRUE)", sep=""))
    }
    xtmp <- ActiveDataSet()
    res <- cut(xtmp[,getIndex(var)], breaks=breaks, labels=labels)
    xtmp[,getIndex(var)] <- res
    updateActiveDataSet( xtmp )
    #freqCalcIndivRisk()
  }
  
  # globalRecodeGroup function
  vc <- function(...) {
    renameFacVar <- function(h, v, ...) {
      gr1_window <- getd("gr1_window")
      if( length(h)< 1 ) {
        gmessage("You need to select at least 1 level.", title="Information", icon="warning")
      } else {
        if( length(h)> 1 ) {
          gmessage("To rename one, you just have to select 1.", title="Information",
              icon="warning", parent=gr1_window)
        } else {
          newName <- ginput("Please enter a new level name.", parent=gr1_window)
          if( !is.na(newName) & newName!="" ) {
            renameVars_tmp(v, h, newName)
            cat("v:\n")
            print(v)
            cat("h:\n")
            print(h)
            showLevels(v)
            updateSummary(v)
          }
        }
      }
    }
    groupFacVar <- function(h, v, ...) {
      gr1_window <- getd("gr1_window")
      if( length(h)< 2 ) {
        gmessage("You need to select at least 2 levels to group.", title="Information",
            icon="warning", parent=gr1_window)
      } else {
        levName <- h[1]
        for( i in 2:length(h) ) {
          levName <- paste(levName, "_and_", h[i], sep="")
        }
        newName <- ginput("Please enter a new level name.", text=levName, parent=gr1_window)
        if( !is.na(newName) ) {
          groupVars_tmp(v, h, newName)
          showLevels(v)
          updateSummary(v)
        }
      }
    }
    updateSummary <- function(v){
      index <- which(getd("keyVars")==v)
      gr1_head <- getd("gr1_head")
      gr1_summary <- getd("gr1_summary")
      xtmp <- ActiveDataSet()
      var <- xtmp[,v]
      if(isExtant(gr1_head[[index]])){
        svalue(gr1_head[[index]]) <- capture.output(print(head(var)),append=FALSE)
        svalue(gr1_summary[[index]]) <- capture.output(print(summary(var)),append=FALSE)
      }
      dev.set(getd("gdev")[[index]])
      if(is.factor(var)){
        try(plot(var,main=v),silent=TRUE)
      }else if(is.numeric(var)){
        try(hist(var,main=v),silent=TRUE)
      }
      keyname <- getd("keyVars")
      varmoslist <- keyname[unlist(lapply(keyname,function(x)is.factor(xtmp[,x])))]
      mosdev <- getd("mosdev")
      dev.set(mosdev)
      if(length(varmoslist)>=2){
        formX <- varmoslist
        try(mosaic_check(formX),silent=TRUE)
      }else{
        try(plot(1,main="Two variables as factors needed!"),silent=TRUE)
      }
      FreqT <- getd("FreqT")
      if(isExtant(FreqT)){
        m1 <- getd("measure_risk_res")
        f1 <- getd("freqCalc")
        xtmp <- ActiveDataSet()
        tabDat <- cbind(xtmp[,keyname],risk=m1$Res[,"risk"],fk=f1$fk,Fk=f1$Fk)
        ind <- !duplicated(apply(xtmp[,keyname],1,function(x)paste(x,collapse="_")))
        tabDat <- tabDat[ind,]
        tabDat <- apply(tabDat,2,function(x)as.character(x))
        FreqT[,] <- data.frame(tabDat,stringsAsFactors=FALSE)
      }
    }
    showLevels <- function(h, ...) {
      facTab <- getd("facTab")
      i <- which(getd("keyVars")==h)
      x <- facTab[[i]]
      if(isExtant(x)){
        xtmp <- ActiveDataSet()
        x[,] <- levels(xtmp[,h])
        gr3_windowButton1 <- getd("gr3_windowButton1")
        gr3_windowButton2 <- getd("gr3_windowButton2")
        enabled(gr3_windowButton1[[i]]) <- TRUE
        enabled(gr3_windowButton2[[i]]) <- TRUE
      }
    }
    hideLevels <- function(h, ...) {
      facTab <- getd("facTab")
      i <- which(getd("keyVars")==h)
      x <- facTab[[i]] 
      x[,] <- character(0)
      gr3_windowButton1 <- getd("gr3_windowButton1")
      gr3_windowButton2 <- getd("gr3_windowButton2")
      enabled(gr3_windowButton1[[i]]) <- FALSE
      enabled(gr3_windowButton2[[i]]) <- FALSE
    }
    keyname <- getd("keyVars")
    gr1_window = gwindow("Choose parameters for globalRecode", width=1100, parent=window)
    gr1_main <-  gframe("", container=gr1_window, horizontal=FALSE)
    nb <- gnotebook(container=gr1_main, closebuttons=FALSE)
    #Main
    xtmp <- ActiveDataSet()[,keyname]
    groupFacVarFun <- renameFacVarFun <- gdev <- recFactorFun <- breaksInput <- labelsInput <- list()
    facTab <- gr3_windowButton1 <- gr3_windowButton2 <- recButton2 <- rb <- gr1_head <- gr1_summary <- rbfun <- list()
    for(i in 1:length(keyname)){
      #Main
      tmp <- ggroup(horizontal=FALSE, container=nb,label=keyname[i]) 
      glabel("Type:",container=tmp)
      rb[[i]] <- gradio(c("Numeric","Factor"), container=tmp)
      rbfun[[i]] <- eval(parse(text=paste("
        function(h,...) {
        index <- ",i,"
        name <- \"",keyname[i],"\"
        if(svalue(h$obj)==\"Factor\"){
          enabled(recButton2[[index]]) <- FALSE
          varToFactor_tmp(name)
          showLevels(name)
        }else{
          varToNumeric_tmp(name)
          enabled(recButton2[[index]]) <- TRUE
          hideLevels(name)
        }
        var <- ActiveDataSet()[,name]
        updateSummary(name)
      }",sep="")))
      
      addHandlerClicked(rb[[i]], handler=rbfun[[i]])

      glabel("Head:",container=tmp)
      gr1_head[[i]] <- gtext("", container=tmp, height=50, width=250)
      glabel("Summary:",container=tmp)
      gr1_summary[[i]] <- gtext("", container=tmp, height=50, width=250)
      svalue(gr1_head[[i]]) <- capture.output(print(head(xtmp[,keyname[i]])),append=FALSE)
      svalue(gr1_summary[[i]]) <- capture.output(print(summary(xtmp[,keyname[i]])),append=FALSE)
      tmp2 <- gframe("", container=tmp, horizontal=TRUE)
      #####Recode to Factor
      tmpRecFac <-  gframe("Recode to factor", container=tmp2, horizontal=FALSE)
      lab <- "BREAKS: Example input: 1,3,5,9 splits var in 3 groups"
      lab <- paste(lab, "\n(1,3],(3,5] and (5,9]. If you just supply")
      lab <- paste(lab, "\n1 number, like 3, the var will be split in")
      lab <- paste(lab, "\n3 equal sized groups.")
      glabel(lab, container=tmpRecFac)
      breaksInput[[i]] = gedit(width=40)
      add(tmpRecFac, breaksInput[[i]], expand=TRUE)
      lab <- "LABELS: Labels are depending on your breaks-input."
      lab <- paste(lab, "\nExample inupt with breaks=1,3,5,9 or breaks=3:")
      lab <- paste(lab, "\n- leave it blank: auto numbering from 1 to 3")
      lab <- paste(lab, "\n- a,b,c: the 3 groups are named a, b and c")
      glabel(lab, container=tmpRecFac)
      labelsInput[[i]] = gedit()
      add(tmpRecFac, labelsInput[[i]] , expand=TRUE)
      recFactorFun[[i]] <- eval(parse(text=paste(
        "function(...){
        index <- ",i,"
        name <- \"",keyname[i],'"
        breaksInput <- getd("breaksInput")
        labelsInput <- getd("labelsInput")
        breaks=svalue(breaksInput[[index]])
        labels=svalue(labelsInput[[index]])
        breaks <- strsplit(breaks, ",")[[1]]
          labels <- strsplit(labels, ",")[[1]]
          allNumeric <- TRUE
          labelsNumeric <- TRUE
          gr_do <- TRUE
          if( length(breaks)==0 ) {
            allNumeric <- FALSE
          } else {
            try(breaks <- as.numeric(breaks), silent=TRUE)
            for( i in 1:length(breaks) ) {
              if( is.na(breaks[i]) ) {
                allNumeric <- FALSE
              }
            }
          }
          if( allNumeric==FALSE ) {
            gmessage("Breaks argument is not valid", title="Information", icon="info", parent=gr1_window)
            gr_do <- FALSE
          }
          if( allNumeric ) {
            if( length(labels)>0 ) {
              if( length(breaks)==1 ) {
                if( length(labels)!=breaks) {
                  gmessage(paste("Too many or few labels supplied. ",breaks," labels should be supplied.",sep=""), title="Information", icon="info", parent=gr1_window)
                  gr_do <- FALSE
                }
              }
              if( length(breaks)>1 ) {
                if( length(labels)!=(length(breaks)-1) ) {
                  gmessage(paste("Too many or few labels supplied. ",(length(breaks)-1)," labels should be supplied.",sep=""), title="Information", icon="info", parent=gr1_window)
                  gr_do <- FALSE
                }
              }
              if( gr_do ) {
                try(tmp_labels <- as.numeric(labels), silent=TRUE)
                for( i in 1:length(tmp_labels) ) {
                  if( is.na(tmp_labels[i]) ) {
                    labelsNumeric <- FALSE
                  }
                }
                if( labelsNumeric ) {
                  labels <- as.numeric(labels)
                }
                if( !labelsNumeric ) {
                  gr_do <- gconfirm("Variable will be of typ factor afterwards", title="Information",
                      icon="warning", parent=gr1_window)
                }
              }
            } else {
              labels <- FALSE
            }
          }        
        if( gr_do ) {
          globalRecode_tmp (name, breaks, labels)     
          var <- ActiveDataSet()[,name]
          rb <- getd("rb")
          svalue(rb[[index]]) <- "Factor"
          updateSummary(name)
        }
      }',sep="")))
        recButton2[[i]] <- gbutton("Recode to factor", container=tmpRecFac, handler=recFactorFun[[i]])

      gseparator(container=tmp)
      ##Group/Rename Factor
      tmpGroupFac <-  gframe("Group a factor", container=tmp2, horizontal=FALSE)
      tmpGroupFac2 = gframe("Levels", container=tmpGroupFac)
      facTab[[i]] <-  gtable(data.frame(levels=character(0), stringsAsFactors=FALSE),
          multiple=TRUE)
      size(facTab[[i]]) <- c(120,150)
      add(tmpGroupFac2, facTab[[i]])
      btmp = ggroup(container=tmpGroupFac2, horizontal=FALSE, expand=TRUE)
      renameFacVarFun[[i]] <- eval(parse(text=paste('
      function(h,...){
        facTab <- getd("facTab")
        renameFacVar(svalue(facTab[[',i,']]), "',keyname[i],'")
      }
      ',sep="")))
      
      
      gr3_windowButton1[[i]] <- gbutton("rename",
          handler= renameFacVarFun[[i]])
      enabled(gr3_windowButton1[[i]]) <- FALSE
      groupFacVarFun[[i]] <- eval(parse(text=paste('
      function(h,...) {
        facTab <- getd("facTab")
        groupFacVar(svalue(facTab[[',i,']]), "',keyname[i],'") 
      }
      ',sep="")))
          
      gr3_windowButton2[[i]] <-  gbutton("group",
          handler=groupFacVarFun[[i]])
      enabled(gr3_windowButton2[[i]]) <- FALSE
      add(btmp, gr3_windowButton1[[i]])
      add(btmp, gr3_windowButton2[[i]])
      gseparator(container=tmpGroupFac)
      gr3_windowButtonGroup = ggroup(container=tmpGroupFac)
      addSpring(gr3_windowButtonGroup)
      #Graphics Fenser
      tmpGraph <-  gframe("Plot", container=tmp2, horizontal=FALSE)
       ggraphics(container=tmpGraph)
       gdev[[i]] <- dev.cur()
      
      ##Main
      if(is.factor(xtmp[,keyname[i]])){
        svalue(rb[[i]]) <- "Factor"
      }else{
        svalue(rb[[i]]) <- "Numeric"
      }
    }
    #Save Input-Fields to the env
    putd("breaksInput",breaksInput)
    putd("labelsInput",labelsInput)
    putd("rb",rb)
    putd("gr1_window",gr1_window)
    putd("facTab",facTab)    
    putd("gr3_windowButton1",gr3_windowButton1)
    putd("gr3_windowButton2",gr3_windowButton2)
    putd("gr1_head",gr1_head)
    putd("gr1_summary",gr1_summary)
    putd("gdev",gdev)

    #Insert Levels in List for Factor variables
    for(i in 1:length(keyname)){
      if(is.factor(xtmp[,keyname[i]])){
        showLevels(keyname[i])
      }
    }
    
    ##Mosaic Plot
    t <- ggraphics(container=nb, label="Mosaic Plot")
    mosdev <- dev.cur()
    putd("mosdev",mosdev)
    varmoslist <- keyname[unlist(lapply(keyname,function(x)is.factor(xtmp[,x])))]
    if(length(varmoslist)>=2){
      formX <- varmoslist
      try(mosaic_check(formX),silent=TRUE)
    }else{
      try(plot(1,main="Two variables as factors needed!"),silent=TRUE)
    }
    #Frequencies Tab
    FreqTT <- ggroup(horizontal=FALSE, container=nb,label="Frequencies")
    ffc_print = gtext(container=FreqTT,text="", width=240, height=80)
    putd("ffc_print",ffc_print)
    svalue(ffc_print) <- svalue(fc_print)
    m1 <- getd("measure_risk_res")
    f1 <- getd("freqCalc")
    xtmp <- ActiveDataSet()
    tabDat <- cbind(xtmp[,keyname],risk=m1$Res[,"risk"],fk=f1$fk,Fk=f1$Fk)
    ind <- !duplicated(apply(xtmp[,keyname],1,function(x)paste(x,collapse="_")))
    tabDat <- tabDat[ind,]
    
    FreqT <- gtable(data.frame(apply(tabDat,2,function(x)as.character(x)),stringsAsFactors=FALSE))
    size(FreqT) <- c(800,800)
    add(FreqTT, FreqT)
    putd("FreqT",FreqT)
    #Help
    t <- gtext(container=nb, label="Help", expand=TRUE)
    l <- .findHelpPage("globalRecode", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    # First Keyvar-Tab
    svalue(nb) <- 1
    gseparator(container=gr1_main)
    okCancelGroup = ggroup(container=gr1_main)
    addSpring(okCancelGroup)
    gbutton("Ok", container=okCancelGroup,
        handler=function(h,...) {dispose(gr1_window) } )
    #gbutton("Cancel", container=okCancelGroup, handler=function(h,...) dispose(gr1_window) )
    gbutton("Help", container=okCancelGroup, handler=function(h,...) helpR("globalRecode") )
    
    #Plot ausfuehren
    for(i in 1:length(keyname)){
      dev.set(gdev[[i]])
      var <- xtmp[,keyname[i]]
      if(is.factor(var)){
        plot(var,main=keyname[i])
      }else if(is.numeric(var)){
        hist(var,main=keyname[i])
      }
    }
  }
  pram1 <- function(...) {
    #ToolTip Pram Window
    tt_var <- "choose categorical variables for Pram"
    tt_strat <- "choose variables for stratification"
    tt_ltr <- "Add selected variable(s)"
    tt_rtl <- "Remove selected variable(s)"
    lTOr <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab[])==1 ) {
          if( is.na(selTab[]) ) {
            selTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(varTab[]) ) {
          varTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(varTab[]) ) {
            for( j in 1:length(h) ) {
              if( varTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          varTab[,] <- data.frame(vars=varTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    rTOl <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(varTab[])==1 ) {
          if( is.na(varTab[]) ) {
            varTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab[]) ) {
          selTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab[]) ) {
            for( j in 1:length(h) ) {
              if( selTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    lTOr1 <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab1[])==1 ) {
          if( is.na(selTab1[]) ) {
            selTab1[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab1[,] <- data.frame(vars=c(selTab1[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab1[,] <- data.frame(vars=c(selTab1[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(sTab[]) ) {
          sTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(sTab[]) ) {
            for( j in 1:length(h) ) {
              if( sTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          sTab[,] <- data.frame(vars=sTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    rTOl1 <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(sTab[])==1 ) {
          if( is.na(sTab[]) ) {
            sTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            sTab[,] <- data.frame(vars=c(sTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          sTab[,] <- data.frame(vars=c(sTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab1[]) ) {
          selTab1[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab1[]) ) {
            for( j in 1:length(h) ) {
              if( selTab1[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    p1_window = gwindow("Pram", width=230, parent=window)
    nb <- gnotebook(container=p1_window, closebuttons=FALSE)
    #Main
    p1_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
    tmp = gframe("Pram", container=p1_windowGroup, horizontal=FALSE)
    #Help
    t <- gtext(container=nb, label="Help", expand=TRUE)
    l <- .findHelpPage("pram_strata", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    svalue(nb) <- 1
    
    
    tmp = gframe("Variable Selection", container=p1_windowGroup)
    keyVars <- c()
    xtmp <- ActiveDataSet()
    # not all vars, just numerical vars
    #for( i in 1:dim(xtmp)[2] ) {
    #	if( class(xtmp[,i])=="numeric" & names(xtmp)[i] != getd("wVars") ) {
    #		numVars <- c(numVars, names(xtmp)[i])
    #	}
    #}
    ###Select categorical variables
    keyVars <- getd("keyVars")
    varTab = gtable(data.frame(vars=keyVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(varTab) <- c(120,200)
    add(tmp, varTab)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr(svalue(varTab)) })
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl(svalue(selTab)) })
    tooltip(b1) <- tt_ltr
    tooltip(b2) <- tt_rtl
    addSpring(btmp)
    selTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab) <- c(120,200)
    add(tmp, selTab)
    gseparator(container=p1_windowGroup)
    
    
    #Select strata_variables
    tmp = gframe("Strata Variable Selection", container=p1_windowGroup)
    sVars <- getd("sVars")
    sTab = gtable(data.frame(vars=sVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(sTab) <- c(120,200)
    add(tmp, sTab)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr1(svalue(sTab)) })
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl1(svalue(selTab1)) })
    tooltip(b1) <- tt_ltr
    tooltip(b2) <- tt_rtl
    addSpring(btmp)
    selTab1 = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab1) <- c(120,200)
    add(tmp, selTab1)
    gseparator(container=p1_windowGroup)
    
    p1_windowButtonGroup = ggroup(container=p1_windowGroup)
    addSpring(p1_windowButtonGroup)
    gbutton("Ok", container=p1_windowButtonGroup,
        handler=function(h,...) {
          if( length(selTab[])==0 ) {
            gmessage("You need to select at least 1 variable!", title="Information", icon="info", parent=p1_window)
          } else {
            var <- selTab[]
            svar <- sTab[]
            if(length(svar)==0) svar <- NULL
            pram_tmp(var, svar)
            
            dispose(p1_window)
          } 
        })
    gbutton("Cancel", container=p1_windowButtonGroup, handler=function(h,...) { dispose(p1_window) })
    gbutton("Help", container=p1_windowButtonGroup, handler=function(h,...) { helpR("pram_strata") })
  }  
  # function for gr_button2
  # opens script window to execute R commands directly
  # globalRecodeGroup function
  scriptWindow <- function(...) {
    # TODO: auto scroll down needs to be implemented
    scriptEnv = new.env()
    assign("cmdhist", c(), envir=scriptEnv)
    sendCommand <- function(gin, gout, ...) {
      cat("start\n")
      insert(gout, paste(">", svalue(gin)), font.attr=c(color="red", family="monospace"))
      cat("1\n")
      res <- capture.output(err <- try(eval(parse(text=svalue(gin)), envir=scriptEnv), silent=TRUE), append=FALSE)
      cat("2\n")
      if( class(err)=="try-error" ) {
        insert(gout, strsplit(err, " : ")[[1]][2], font.attr=c(family="monospace"))
        cat("3\n")
      } else {
        cat("4\n")
        if( length(err)>0 ){
          err <- capture.output(print(err))
          cat("5\n")
          insert(gout, err[1], font.attr=c(family="monospace"))
          cat("6\n")
          if( length(err)>1 ) {
            for( i in 2:length(err) ) {
              cat("7\n")
              insert(gout, err[i], font.attr=c(family="monospace"))
            }
          }
        }
        cat("8\n")
        if( length(strsplit(svalue(gin), "<-")[[1]])>1 || length(strsplit(svalue(gin), "=")[[1]])>1 ) {
          cat("9\n")
          cmdhist <- get("cmdhist", envir=scriptEnv)
          cat("10\n")
          cmdhist <- c(cmdhist, svalue(gin))
          cat("11\n")
          assign("cmdhist", cmdhist, envir=scriptEnv)
          cat("12\n")
        }
      }
      svalue(gin) <- ""
cat("xxx");
    }
    saveAds <- function(...) {
      updateActiveDataSet(get("ads", envir=scriptEnv))
      #freqCalcIndivRisk()
      #assign("x", get("ads", envir=scriptEnv), envir=.GlobalEnv)
      # TODO: done? - save commands for script/history
      cmdhist <- get("cmdhist", envir=scriptEnv)
      if( length(cmdhist) > 0 ) {
        Script.add("ads <- ActiveDataSet()")
        for( i in 1:length(cmdhist) ) {
          Script.add(cmdhist[i])
        }
        Script.add("updateActiveDataSet(ads)")
        Script.add("freqCalcIndivRisk()")
      }
      # end save
      quitScriptWindow()
    }
    removeWs <- function(...) {
      if( exists("scriptEnv", envir=.GlobalEnv) ) {
        try(rm(scriptEnv, envir=.GlobalEnv), silent=TRUE)
      }
    }
    sureQuit <- function(...) {
      gconfirm("You want to close the window without saving?", icon="question", parent=scriptWindow,
          handler=function(h,...) quitScriptWindow() )
    }
    quitScriptWindow <- function(...) {
      removeWs()
      dispose(scriptWindow)
    }
    loadAds <- function(...) {
      assign("ads", ActiveDataSet(), envir=scriptEnv)
      #assign("ads", francdat, envir=scriptEnv)
    }
    scriptWindow = gwindow("Script window", parent=window)
    scriptWidget = ggroup(horizontal=FALSE)
    scriptInfoGroup = ggroup(container=scriptWidget)
    addSpring(scriptInfoGroup)
    glabel("ActiveDataSet available for modifications as variable: ads",
        container=scriptInfoGroup)
    gbutton("Reload active data set to ads", container=scriptInfoGroup,
        handler=function(h,...) loadAds() )
    addSpring(scriptInfoGroup)
    loadAds()
    xout = gtext(text="", width=700, height=400)
    add(scriptWidget, xout)
    scriptSubmit = ggroup(container=scriptWidget)
    glabel(" >", container=scriptSubmit)
    xcom = gedit("", container=scriptSubmit, expand=TRUE)#, handler=function(h, ...) sendCommand(xcom, xout))
    gbutton("submit", container=scriptSubmit, handler=function(h, ...) sendCommand(xcom, xout))
    gseparator(container=scriptWidget)
    saveCancelGroup = ggroup(container=scriptWidget)
    addSpring(saveCancelGroup)
    gbutton("Save", container=saveCancelGroup, handler=function(h,...) saveAds() )
    gbutton("Cancel", container=saveCancelGroup, handler=function(h,...) sureQuit() )
    
    add(scriptWindow, scriptWidget)
    focus(xcom)
  }
  
  # TODO: nm_risk_print_function
  # nm_risk_print output function
  nm_risk_print_function <- function(...) {
    if(existd("oldCols")){
      xprogress = gwindow("please wait", width=180, height=40, parent=window)
      
      drisk <- dRiskRMD(getd("oldCols"), getd("newCols"), k=svalue(nm_risk_slider1), k2=svalue(nm_risk_slider2))
      dutil1 <- dUtility(getd("oldCols"), getd("newCols"),method="IL1")
      dutil2 <- dUtility(getd("oldCols"), getd("newCols"),method="eigen")
      dutil3 <- dUtility(getd("oldCols"), getd("newCols"),method="robeigen")
      svalue(nm_risk_print) <- paste("Sensitive obs.: ", round(drisk$risk2,2), "%\n ------------------------------ \nUtility:\n", 
          "IL1: ", round(dutil1,2),"\nEigen: ",round(dutil2,2),"\nR-Eigen: ",round(dutil3,2),sep="")
      
      dispose(xprogress)
    }
  }
  generateStrata_tmp <- function(stratavars,name){
    putd("sLen", 1)
    putd("sVars",name)
    
    xtmp <- ActiveDataSet()
    strata <- rep("",nrow(xtmp))
    for(i in 1:length(stratavars)){
      strata <- paste(strata,xtmp[,stratavars[i]],sep="")
      if(length(stratavars)>i)
        strata <- paste(strata,"-",sep="")
    }
    xtmp <- cbind(xtmp,strata)
    colnames(xtmp)[length(colnames(xtmp))] <- name
    putd("activeDataSet", xtmp)
    putd("sIndex", getIndex(name))
  }
  selVar <- function(...) {
    putd("keyLen", 0)
    putd("numLen", 0)
    putd("wLen", 0)
    putd("hLen", 0)
    putd("sLen", 0)
    ft <- function(f, t, h, var, pm, ...) {
      # pm: 1 for +, 0 for -
      count = getd(var)
      if( pm == 1 ) {
        count <- count + length(h);
      } else {
        count <- count - length(h);
      }
      putd(var, count)
      if( length(h)>0 ) {
        if( length(f[])==1 ) {
          if( is.na(f[]) ) {
            f[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            f[,] <- data.frame(vars=c(f[], h), stringsAsFactors=FALSE)
          }
        } else {
          f[,] <- data.frame(vars=c(f[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(t[]) ) {
          t[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(t[]) ) {
            for( j in 1:length(h) ) {
              if( t[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          t[,] <- data.frame(vars=t[-xtmp], stringsAsFactors=FALSE)
        }
        f[,] <- names(ActiveDataSet())[names(ActiveDataSet())%in%f[,]]
        t[,] <- names(ActiveDataSet())[names(ActiveDataSet())%in%t[,]]
      }
    }
    selVar_window = gwindow("Select variables", width=230, parent=window,height=700)
    selVar_windowGroup = ggroup(container=selVar_window, horizontal=FALSE)
    selVar_main = ggroup(container=selVar_windowGroup)
    mtmp = ggroup(container=selVar_main)
    
    allVars <-names(ActiveDataSet())
    
    
    
    
    
    # If it is not the first call to selVar, the previous selection is read
    if(existd("numVars")){
      numVars <- getd("numVars")
      putd("numLen", length(numVars))
    }else{
      numVars <- character(0)
    }
    if(existd("keyVars")){
      keyVars <- getd("keyVars")
      putd("keyLen", length(keyVars))
    }else{
      keyVars <- character(0)
    }  
    if(existd("hVars")){
      hVars <- getd("hVars")
      putd("hLen", length(hVars))
    }else{
      hVars <- character(0)
    }
    if(existd("wVars")){
      wVars <- getd("wVars")
      putd("wLen", length(wVars))
    }else{
      wVars <- character(0)
    }  
    if(existd("sVars")){
      sVars <- getd("sVars")
      putd("sLen", length(sVars))
    }else{
      sVars <- character(0)
    }  
    #Not selected variables
    nsVars <- allVars[!allVars%in%c(numVars,keyVars,hVars,wVars,sVars)]
    #numVars <- c()
    #xtmp <- ActiveDataSet()
    #for( i in 1:dim(xtmp)[2] ) {
    #	numVars <- c(numVars, names(xtmp)[i])
    #}
    varTab = gtable(data.frame(vars=nsVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(varTab) <- c(120,400)
    add(mtmp, varTab)
    
    rtmp = ggroup(container=mtmp, horizontal=FALSE)
    
    tmp = gframe("categorical", container=rtmp)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    gbutton(">>", container=btmp, handler=function(h,...) { ft(catTab, varTab, svalue(varTab), "keyLen", 1) })
    gbutton("<<", container=btmp, handler=function(h,...) { ft(varTab, catTab, svalue(catTab), "keyLen", 0) })
    addSpring(btmp)
    catTab = gtable(data.frame(vars=keyVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(catTab) <- c(120,150)
    add(tmp, catTab)
    
    tmp = gframe("numerical", container=rtmp)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    gbutton(">>", container=btmp, handler=function(h,...) { ft(numTab, varTab, svalue(varTab), "numLen", 1) })
    gbutton("<<", container=btmp, handler=function(h,...) { ft(varTab, numTab, svalue(numTab), "numLen", 0) })
    addSpring(btmp)
    numTab = gtable(data.frame(vars=numVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(numTab) <- c(120,150)
    add(tmp, numTab)
    
    tmp = gframe("weight", container=rtmp)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    gbutton(">>", container=btmp, handler=function(h,...) { ft(wTab, varTab, svalue(varTab), "wLen", 1) })
    gbutton("<<", container=btmp, handler=function(h,...) { ft(varTab, wTab, svalue(wTab), "wLen", 0) })
    addSpring(btmp)
    wTab = gtable(data.frame(vars=wVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(wTab) <- c(120,50)
    add(tmp, wTab)
    ##Household Selection
    tmp = gframe("household id", container=rtmp)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    gbutton(">>", container=btmp, handler=function(h,...) { ft(hTab, varTab, svalue(varTab), "hLen", 1) })
    gbutton("<<", container=btmp, handler=function(h,...) { ft(varTab, hTab, svalue(hTab), "hLen", 0) })
    addSpring(btmp)
    hTab = gtable(data.frame(vars=hVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(hTab) <- c(120,50)
    add(tmp, hTab)
    
    tmp = gframe("strata", container=rtmp)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    gbutton(">>", container=btmp, handler=function(h,...) { ft(sTab, varTab, svalue(varTab), "sLen", 1) })
    gbutton("<<", container=btmp, handler=function(h,...) { ft(varTab, sTab, svalue(sTab), "sLen", 0) })
    addSpring(btmp)
    sTab = gtable(data.frame(vars=sVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(sTab) <- c(120,100)
    add(tmp, sTab)
    
    
    
    gseparator(container=selVar_windowGroup)
    selVar_windowButtonGroup = ggroup(container=selVar_windowGroup)
    addSpring(selVar_windowButtonGroup)
    b1 <- gbutton("Generate Strata Variable", container=selVar_windowButtonGroup, handler=function(h,...) {
          #confirmSelection_tmp(catTab[], numTab[], wTab[],hTab[],sTab[])
          confirmSelection_tmp(catTab[], numTab[], wTab[],hTab[],sTab[])    
          dispose(selVar_window)
          stVar_window = gwindow("Generate a strata variable", width=230, parent=window)
          stVar_windowGroup = ggroup(container=stVar_window, horizontal=FALSE)
          stVar_main = ggroup(container=stVar_windowGroup)
          mtmp = ggroup(container=stVar_main)
          allVars <-colnames(ActiveDataSet())
          rmIndex <- c()
          nro <- nrow(ActiveDataSet())
          for(i in 1:length(allVars)){
            if(nrow(unique(ActiveDataSet()[,allVars[i],drop=FALSE]))>nro*.2)
              rmIndex <- c(rmIndex,i)
          }
          allVars <- allVars[-rmIndex]
          varTab = gtable(data.frame(vars=allVars, stringsAsFactors=FALSE), multiple=TRUE)
          size(varTab) <- c(120,400)
          add(mtmp, varTab)
          rtmp = ggroup(container=mtmp, horizontal=FALSE)
          tmp = gframe("strata", container=rtmp)
          btmp = ggroup(container=tmp, horizontal=FALSE)
          addSpring(btmp)
          gbutton(">>", container=btmp, handler=function(h,...) { ft(sTab, varTab, svalue(varTab), "sLen", 1) })
          gbutton("<<", container=btmp, handler=function(h,...) { ft(varTab, sTab, svalue(sTab), "sLen", 0) })
          addSpring(btmp)
          sTab = gtable(data.frame(vars=sVars, stringsAsFactors=FALSE), multiple=TRUE)
          size(sTab) <- c(120,400)
          add(tmp, sTab)
          gseparator(container=stVar_windowGroup)
          stVar_windowButtonGroup = ggroup(container=stVar_windowGroup)
          addSpring(stVar_windowButtonGroup)
          gbutton("Ok", container=stVar_windowButtonGroup,
              handler=function(h,...) {
                name <- "sdcMicroStrataVariable"
                sVars <- sTab[]
                if(length(sVars)==0)
                  gmessage("You have to select at least  one categoric variable to generate a strata variable.",
                      title="Information", icon="warning", parent=window)
                else{
                  name <- paste(paste(sVars,collapse="_"),"_stratavar",sep="")
                  t1 <- paste("c(",paste("\"",sVars,"\"",sep="",collapse=","),")",sep="")
                  Script.add(paste("generateStrata_tmp(", t1, ", \"",name,"\")",sep=""))
                  generateStrata_tmp(sVars,name)
                  dispose(stVar_window)
                  selVar()
                }
                
              })
          gbutton("Cancel", container=stVar_windowButtonGroup, handler=function(h,...) { dispose(stVar_window) })
          
        })
    tooltip(b1) <- tt_genstrat 
    gbutton("Ok", container=selVar_windowButtonGroup,
        handler=function(h,...) {
          # check if firstrun - if not reset script and dataset to original one
          #cat(paste(getd("keyLen"), getd("numLen"), getd("wLen"), getd("hLen"), "\n"))
          fr_do <- TRUE
          if( !getd("firstRun") ) {
            fr_do <- gconfirm("If you reselect vars, script and dataset will reset.\nAre you sure?", title="Attention",
                icon="warning", parent=window)
            if( fr_do ) {
              Script.new()
              if( existd("oldDataSet") ) {
                putd("activeDataSet", getd("oldDataSet"))
                #xtmp <- getd("oldDataSet")
                #updateActiveDataSet(xtmp)
              }
            }
          } else {
            putd("firstRun", FALSE)
          }
          # check if enough is selected
          if( fr_do ) {
            # min selection must be 1 in each category
            if( ((getd("keyLen")>=1  || getd("numLen")>=1))&&getd("wLen")%in%c(0,1)&&getd("hLen")%in%c(0,1)) {
              confirmSelection_tmp(catTab[], numTab[], wTab[],hTab[],sTab[])
              dispose(selVar_window)
              if(getd("keyLen")>=1){
                keyV <- getd("keyVars")
                keynofac <- keyV[!as.vector(sapply(keyV,function(x)is.factor(ActiveDataSet()[,keyV])))]
                if(length(keynofac)>0){
                  keynofac <- paste(keynofac,collapse=",")
                  gmessage(paste("The variables ",keynofac," are selected as categoric but are not of type factor.
 In the next window you can change this, you can reopen this window by clicking \"Key Variable Configuration\"",sep=""),
                    title="Information", parent=window)
                  vc()
                }
              }
            } else {
              gmessage("You have to select at least  one numeric or categoric variable and optional one weight variable, one household ID variable and several strata variables.",
                  title="Information", icon="warning", parent=window)
            }
          }
        })
    gbutton("Cancel", container=selVar_windowButtonGroup, handler=function(h,...) { dispose(selVar_window) })
  }
  
  
  # function for gb1 (confirm selection)
  # needed sub functions
  # TODO: done - save selection for script/history
  confirmSelection_tmp <- function(t1=character(0), t2=character(0), t3=character(0),t4=character(0),t5=character(0), redo=FALSE) {
    selvar <- vector()
    if(length(t1)>0)
      selvar[length(selvar)+1] <- paste("t1=",parseVarStr(t1),sep="")
    if(length(t2)>0)
      selvar[length(selvar)+1] <- paste("t2=",parseVarStr(t2),sep="")
    if(length(t3)>0)
      selvar[length(selvar)+1] <- paste("t3=",parseVarStr(t3),sep="")
    if(length(t4)>0)
      selvar[length(selvar)+1] <- paste("t4=",parseVarStr(t4),sep="")
    if(length(t5)>0)
      selvar[length(selvar)+1] <- paste("t5=",parseVarStr(t5),sep="")
    selvar <- paste(selvar,collapse=",")
    if( !redo ) {
      Script.add(paste("confirmSelection_tmp(", selvar, ", redo=TRUE)", sep=""))
    }
    putd("keyVars", t1)
    if(length(t1)>0)
      putd("keyIndex", getIndex(t1))
    else
      putd("keyIndex", NULL)
    putd("numVars", t2)
    if(length(t2)>0)
      putd("numIndex", getIndex(t2))
    else
      putd("numIndex", NULL)
    putd("wVars", t3)
    if(length(t3)>0)
      putd("wIndex", getIndex(t3))
    else
      putd("wIndex", NULL)
    
    putd("hVars", t4)
    if(length(t4)>0)
      putd("hIndex", getIndex(t4))
    else
      putd("hIndex", NULL)
    
    putd("sVars", t5)
    if(length(t5)>0)
      putd("sIndex", getIndex(t5))
    else
      putd("sIndex", NULL)
    
    # TODO: Experimental - all key vars as factors, done in updateActiveDataSet()!!!
    updateActiveDataSet(ActiveDataSet())
    # End - Experimental
    # TODO: update main window variable display
    
    vtmp <- getd("keyVars") #Categorical variables names
    if(length(vtmp)>0){
      stmp <- "Categorical:"
      for( i in 1:length(vtmp) ) {
        stmp <- paste(stmp, vtmp[i])
        if( i < length(vtmp) )
          stmp <- paste(stmp, ",", sep="")
      } 
      svalue(tab1) <- stmp
      enabled(ir_button) <- TRUE
      enabled(ls_button1) <- TRUE
      enabled(ld_button1) <- TRUE
      enabled(pram_button1) <- TRUE
      #enabled(ls_button2) <- TRUE
      enabled(vc_button1) <- TRUE
      enabled(gr_button2) <- TRUE
    }else{
      svalue(tab1) <- ""
      enabled(ir_button) <- FALSE
      enabled(ls_button1) <- FALSE
      enabled(ld_button1) <- FALSE
      enabled(pram_button1) <- FALSE
      #enabled(ls_button2) <- TRUE
      enabled(vc_button1) <- FALSE
      enabled(gr_button2) <- FALSE
    }
    vtmp <- getd("numVars") #Numerical variables names
    if(length(vtmp)>0){
      stmp <- "Numerical:"
      for( i in 1:length(vtmp) ) {
        stmp <- paste(stmp, vtmp[i])
        if( i < length(vtmp) )
          stmp <- paste(stmp, ",", sep="")
      }
      svalue(tab2) <- stmp
      enabled(nm_button1) <- TRUE
      enabled(nm_button2) <- TRUE
      enabled(nm_button3) <- TRUE
      enabled(nm_risk_slider1) <- TRUE
      enabled(nm_risk_slider2) <- TRUE
    }else{
      svalue(tab2) <- ""
      enabled(nm_button1) <- FALSE
      enabled(nm_button2) <- FALSE
      enabled(nm_button3) <- FALSE
      enabled(nm_risk_slider1) <- FALSE
      enabled(nm_risk_slider2) <- FALSE
    }
    vtmp <- getd("wVars") #Weight variable names
    if(length(vtmp)>0){
      stmp <- "Weight:"
      for( i in 1:length(vtmp) ) {
        stmp <- paste(stmp, vtmp[i])
      }
      svalue(tab3) <- stmp
    }else
      svalue(tab3) <- ""
    vtmp <- getd("hVars") #Household ID variable
    if(length(vtmp)>0){
      stmp <- "Household ID:"
      
      for( i in 1:length(vtmp) ) {
        stmp <- paste(stmp, vtmp[i])
      }
      svalue(tab4) <- stmp
    }else 
      svalue(tab4) <- ""
    vtmp <- getd("sVars") #strata variable
    if(length(vtmp)>0){
      stmp <- "Strata:"
      
      for( i in 1:length(vtmp) ) {
        stmp <- paste(stmp, vtmp[i])
      }
      svalue(tab5) <- stmp
    }else 
      svalue(tab5) <- ""
    
    # enable plot indivRisk button
    
  }
  # variableSelectionGroup function
  #       if re-clicked, prompt and ask if you want to reset all work and script done
  # 			this is to be used to set dataset to start format as well as reset script,
  #				because it is not needed to reselect the vars during the work process
  confirmSelection <- function(...) {
    # open selection window
    selVar()
  }
  
  ## Menubar Functions
  vign <- function(...) print(vignette("gWidgets"))
  vign2 <- function(...) print(vignette("sdcMicroPaper"))
  paind <- function(...)print(help(package="sdcMicro"))
  
  # Data - Load Dataset
  loadDataSet <- function(...) {
    xname <- gfile("Select file to load", parent=window, type="open" )
    if( xname != '' ) {
      load(xname, envir=.GlobalEnv)
    }
    setDataSet()
  }
  # Data - Choose Dataset
  setDataSet <- function(...) {
    vardt <- ls(envir = .GlobalEnv, all.names=TRUE)
    vards <- names(which(sapply(vardt, function(.x) is.data.frame(get(.x)))))
    vards <- c(vards,names(which(sapply(vardt, function(.x) is.matrix(get(.x))))))
    if( length(vards)==0 ) {
      gmessage("No datasets loaded.", title="Information", icon="warning",
          parent=window)
    } else {
      gbasicdialog(title="Choose Dataset",
          x<-gdroplist(vards), parent=window,
          handler=function(x, ...) { ActiveDataSet(svalue(x$obj))
            putd("oldDataSet", ActiveDataSet()) })
      if( existd("activeDataSet") ) {
        if( dim(ActiveDataSet())[1] > 4000 ) {
          gmessage("Operations in this dataset may require some time, so please be patient.", title="Information",
              icon="info", parent=window)
        }
        svalue(dslab) <- getd("dataSetName")
        enabled(gb1) <- TRUE
      }
      putd("numLen", 0)
      putd("numVars", character(0))
      putd("keyLen", 0)
      putd("keyVars", character(0))
      putd("hLen", 0)
      putd("hVars", character(0))
      putd("wLen", 0)
      putd("wVars", character(0))
      putd("sLen", 0)
      putd("sVars", character(0))
      selVar()
    }
  }
  # Data - Save Dataset To - File
  saveToFile <- function(...) {
    saveVar <- function(fileName, ...) {
      xtmp <- ActiveDataSet()
      save(xtmp, file=paste(fileName,".RData", sep=""))
    }
    if( existd("activeDataSet") ) {
      xname <- gfile("Choose a file to save the Dataset", type="save", parent=window)
      if( xname != "" ) {
        saveVar(xname)
      }
    } else {
      gmessage("No active Dataset found.", title="Information", icon="warning",
          parent=window)
    }
  }
  # Data - Save Dataset To - Variable
  saveToVariable <- function(...) {
    checkAndSave <- function(parent, varName, ...) {
      saveVar <- function(varName, ...) {
        assign(varName, ActiveDataSet(), envir=.GlobalEnv)
      }
      if( exists(varName, envir=.GlobalEnv) ) {
        gconfirm("Variable already exists, do you want to replace it?",
            title="Information", parent=parent,
            handler=function(h, ...) { saveVar(varName) } )
      } else {
        saveVar(varName)
      }
    }
    if( existd("activeDataSet") ) {
      xname = ginput("Please enter a Variable name",
          title="Choose Variable name", icon="question", parent=window,
          handler=function(h, ...) checkAndSave(h$obj, h$input) )
    } else {
      gmessage("No active Dataset found.", title="Information", icon="warning",
          parent=window)
    }
  }
  
  # Script - New Script
  newScript <- function(...) {
    ns_do <- gconfirm("A new script will be started.\nAre you sure?", title="Information",
        icon="warning", parent=window)
    if( ns_do ) {
      Script.new()
    }
  }
  
  # Script - Save Script
  saveScript <- function(...) {
    saveScriptToFile <- function(fileName, ...) {
      cmdtmp <- Script()
      save(cmdtmp, file=paste(fileName,".sdcMicroScript", sep=""))
    }
    if( existd("activeScript") ) {
      xname <- gfile("Select file to save Script", type="save", parent=window)
      if( xname != "" ) {
        saveScriptToFile(xname)
      }
    } else {
      gmessage("No active Script found.", title="Information", icon="warning",
          parent=window)
    }
  }
  
  # Script - Load Script
  loadScript <- function(...) {
    # open file browser and load the needed script
    xname <- gfile("Select script file to open.", parent=window, type="open", 
        filter=list("Script files" = list(patterns = c("*.sdcMicroScript"))) )
    if( xname != '' ) {
      load(xname, envir=sdcGUIenv)
      Script.new()
      putd("activeScript", get("cmdtmp", envir=sdcGUIenv))
      rm(cmdtmp, envir=sdcGUIenv)
    }
  }
  
  # Script - View Script
  # TODO: implement view script
  viewScript <- function(...) {
    cmdhist <- Script()$cmd
    if( is.null(cmdhist) ) {
      gmessage("No script present at the moment.", title="Attention", icon="warning", parent=window)
    } else {
      sureQuit <- function(...) {
        gconfirm("Do you want to close the window without saving?", icon="question", parent=scriptEditWindow,
            handler=function(h,...) quitEditScriptWindow() )
      }
      quitEditScriptWindow <- function(...) {
        xtmp <- list(cmd=c(xscript[]))
        Script(xtmp)
        dispose(scriptEditWindow)
      }
      runCMDhist <- function(...) {
        rto <- as.numeric(svalue(runTo))
        cmdhist <- xscript[]
        if( is.numeric(rto) & !is.na(rto) ) {
          if( rto>0 & rto<(length(cmdhist)+1) ) {
            cmdhisttmp <- cmdhist[c(1:rto)]
            Script.run(cmdhisttmp)
            quitEditScriptWindow()
          }
        } else {
          gmessage("Script step not valid.", title="Input not valid", icon="info", parent=scriptEditWindow)
        }
      }
      delCMDhist <- function(...) {
        dto <- as.numeric(svalue(delRow))
        cmdhist <- xscript[]
        if( is.numeric(dto) & !is.na(dto) ) {
          if( dto>0 & dto<(length(cmdhist)+1) ) {
            cmdhisttmp <- cmdhist[-dto]
            xscript[] <- cmdhisttmp
            svalue(delRow) <- ""
          }
        } else {
          gmessage("Script step not valid.", title="Input not valid", icon="info", parent=scriptEditWindow)
        }
      }
      scriptEditWindow = gwindow("View script", parent=window, width=700, height=400)
      scriptWidget = ggroup(horizontal=FALSE)
      xscript = gdf(cmdhist, expand=TRUE)
      # TODO: find replacement, cause in linux it wouldnt display anything.
      #enabled(xscript) <- FALSE
      add(scriptWidget, xscript, expand=TRUE)
      gseparator(container=scriptWidget)
      saveCancelGroup = ggroup(container=scriptWidget)
      addSpring(saveCancelGroup)
      tmp = ggroup(container=saveCancelGroup)
      glabel("Delete script step: ", container=tmp)
      delRow = gedit(text="", width=3, container=tmp)
      gbutton("Delete", container=tmp, handler=function(h,...) delCMDhist() )
      addSpring(saveCancelGroup)
      tmp = ggroup(container=saveCancelGroup)
      glabel("Run script to row: ", container=tmp)
      runTo = gedit(text=length(cmdhist), width=3, container=tmp)
      gbutton("Run", container=tmp, handler=function(h,...) runCMDhist() )
      addSpring(saveCancelGroup)
      gbutton("Close", container=saveCancelGroup, handler=function(h,...) quitEditScriptWindow() )
      
      add(scriptEditWindow, scriptWidget)
    }
  }
  
  # Script - Run Script
  runScript <- function(...) {
    # dialog and ask if you want to run the whole script on this dataset
    Script.run()
  }
  
  # GUI - Quit
  quitGUI <- function(...) {
    val <- gconfirm("Do you really want to close the window?", parent=window)
    if( as.logical(val) ) {
      dispose(window)
    }
  }
  restartGUI <- function(...) {
    val <- gconfirm("Do you really want to close the window?", parent=window)
    if( as.logical(val) ) {
      dispose(window)
    }
    sdcGUI()
  }
  
  ## initialize
  # set first run
  putd("firstRun", TRUE)
  # set up new script
  Script.new()
  # get values of internal vars if they exist
  activeDataSet <- if( existd("activeDataSet") ) getd("activeDataSet") else ""
  dataSetName <- if( existd("dataSetName") ) getd("dataSetName") else ""
  # save intitial values in env
  if( !dataSetName=="" ) {
    ActiveDataSet(dataSetName)
  }
  putd("dataSetName", dataSetName)
  
  ## create window
  window = gwindow("sdcMicro GUI")
  addHandlerUnrealize(window, handler = function(h,...) {
        val <- gconfirm("Do you really want to close the window?", parent=h$obj)
        if(as.logical(val))
          return(FALSE)             # destroy
        else
          return(TRUE)              # don't destroy
      })
  
  ## Menubar
  mbar = list()
  mbar$GUI$Quit$handler = quitGUI
  mbar$GUI$Restart$handler = restartGUI
  mbar$Data$"Load Dataset"$handler = loadDataSet
  mbar$Data$"Choose Dataset"$handler = setDataSet
  mbar$Data$"Save Dataset to"$File$handler = saveToFile
  mbar$Data$"Save Dataset to"$Variable$handler = saveToVariable
  mbar$Script$"New"$handler = newScript
  mbar$Script$"Save to file"$handler = saveScript
  mbar$Script$"Load from file"$handler = loadScript
  mbar$Script$"View"$handler = viewScript
  mbar$Script$"Run"$handler = runScript
  mbar$Help$"Guidelines"$handler = vign
  mbar$Help$"sdcMicro Paper"$handler = vign2
  mbar$Help$"Package Index"$handler = paind
  
  ## layout
  mainGroup = ggroup(container=window, horizontal=FALSE)
  # Start - add menu
  add(mainGroup, gmenu(mbar))
  # End - add menu
  # Start - variable Selection Container
  varSelGroup = ggroup(container=mainGroup)
  mtmp = ggroup(container=varSelGroup, horizontal=FALSE, expand=TRUE)
  tmp = gframe("Selected variables", container=mtmp, horizontal=FALSE)
  
  rtmp = ggroup(container=tmp)
  tab1 = glabel("")#categorical info
  tooltip(tab1) <- tt_selVar
  add(rtmp, tab1, expand=TRUE)
  addSpring(rtmp)
  rtmp = ggroup(container=tmp)
  
  tab2 <- glabel("")#numerical info
  tooltip(tab2) <- tt_selVar
  add(rtmp, tab2, expand=TRUE)
  addSpring(rtmp)
  rtmp = ggroup(container=tmp)
  
  tab3 <- glabel("")#weight info
  tooltip(tab3) <- tt_selVar
  add(rtmp, tab3, expand=TRUE)
  
  
  tab4 <- glabel("")# household info
  tooltip(tab4) <- tt_selVar
  add(rtmp, tab4, expand=TRUE)
  addSpring(rtmp)
  
  rtmp = ggroup(container=tmp)
  tab5 <- glabel("")#strata info
  tooltip(tab5) <- tt_selVar
  add(rtmp, tab5, expand=TRUE)
  addSpring(rtmp)
  
  varSelGroupButton = ggroup(container=varSelGroup, horizontal=FALSE)
  glabel("Loaded data set:", container=varSelGroupButton)
  if( existd("dataSetName") ) {
    dslab = glabel(getd("dataSetName"),container=varSelGroupButton)
  } else {
    dslab = glabel("none",container=varSelGroupButton)
  }
  gb1 = gbutton(text="Select variables", container=varSelGroupButton,
      handler=function(h,...) confirmSelection())
  tooltip(gb1) <- "(Re)-identify categorical, numerical variables (and the weight variable, the household ID variable and the strata variables)"
  enabled(gb1) <- FALSE
  # End - variable Selection Container
  # Start - freqCalc Container
  freqCalcGroup = ggroup(container=mainGroup)
  fc_tmp = gframe("Frequency calculations",
      container=freqCalcGroup, expand=TRUE)
  tmp = gframe("(Individual) risk computation", container=fc_tmp)
  fc_print = gtext(text="", width=240, height=80)
  tooltip(fc_print)<- tt_print
  putd("fc_print",fc_print)
  add(tmp, fc_print)
  addSpring(fc_tmp)
  tmp = gframe("Suppressions", container=fc_tmp)
  fc_summary = gtext(text="", width=240, height=80)
  tooltip(fc_summary)<- tt_summary
  add(tmp, fc_summary)
  # End - freqCalc Container
  # indivRisk and globalRecode Container
  indivRiskGlobalRecodeGroup = ggroup(container=mainGroup)
  # Start - indivRisk Container
  indivRiskGroup = ggroup(container=indivRiskGlobalRecodeGroup)
  ir_tmp = gframe("Risk for categorical key variables", container=indivRiskGroup, horizontal=FALSE)
  ir_print = gtext(text="", width=240, height=250)
  tooltip(ir_print)<- tt_ir
  add(ir_tmp, ir_print)
  indivRiskGroupButton = ggroup(container=ir_tmp)
  addSpring(indivRiskGroupButton)
  ir_button = gbutton("plot individual Risk", container=indivRiskGroupButton,
      handler=function(h, ...) plotIndivRisk(getd("freqCalc"),getd("indivRisk")))
  tooltip(ir_button) <- tt_pir
  addSpring(indivRiskGroupButton)
  enabled(ir_button) <- FALSE
  # End - indivRisk Container
  # Start - globalRecode Container
  globalRecodeGroup = ggroup(container=indivRiskGlobalRecodeGroup, horizontal=FALSE)
  tmp = gframe("Recode", container=globalRecodeGroup)
  globalRecodeGroupLeft = ggroup(container=tmp, horizontal=FALSE)
  vc_button1 = gbutton("Key Variable Configuration", handler=function(h, ...) vc() )
  tooltip(vc_button1) <- tt_vc
  enabled(vc_button1) <- FALSE
  add(globalRecodeGroupLeft, vc_button1)
  #globalRecodeGroupRight = ggroup(container=tmp, horizontal=FALSE)
  #tmp = gframe("Experts only", container=globalRecodeGroupRight)
  #gr_button2 = gbutton("Manual R commands", handler=function(h, ...) scriptWindow() )
  #enabled(gr_button2) <- FALSE
  #add(tmp, gr_button2)
  # End - globalRecode Container
  # Start - pram Container
  pramGroup = ggroup(container=globalRecodeGroup)
  tmp = gframe("Pram", container=pramGroup, horizontal=FALSE)
  pram_button1 = gbutton("pram",
      handler=function(h,...) pram1() )
  tooltip(pram_button1) <- tt_pram1
  add(tmp, pram_button1)
  enabled(pram_button1) <- FALSE
  
  # Start - localSupp Container
  localSuppGroup = ggroup(container=globalRecodeGroup)
  tmp = gframe("Local suppression", container=localSuppGroup, horizontal=FALSE)
  ls_button1 = gbutton("Local suppression",
      handler=function(h,...) ls4() )
  tooltip(ls_button1) <- tt_ls1
  add(tmp, ls_button1)
  addSpring(tmp)
  enabled(ls_button1) <- FALSE

  # Start - l-Diversity Container
  ldivGroup = ggroup(container=globalRecodeGroup)
  tmp = gframe("l-Diversity", container=localSuppGroup, horizontal=FALSE)
  ld_button1 = gbutton("l-Diversity",
      handler=function(h,...) ldiv1() )
  tooltip(ld_button1) <- tt_ld1
  add(tmp, ld_button1)
  addSpring(tmp)
  enabled(ld_button1) <- FALSE
  
  globalRecodeGroupRight = ggroup(container=globalRecodeGroup, horizontal=FALSE)
  tmp = gframe("Experts only", container=globalRecodeGroupRight)
  gr_button2 = gbutton("Manual R commands", handler=function(h, ...) scriptWindow() )
  tooltip(gr_button2) <- tt_man
  enabled(gr_button2) <- FALSE
  add(tmp, gr_button2)
  addSpring(tmp)
  
  
  # End - localSupp Container
  # Start - numericalMethod Container
  tmp = gframe("Methods for Continuous Key Variables", container=mainGroup)
  tmp1 = ggroup(container=tmp, horizontal=FALSE)
  addSpring(tmp1)
  nm_button1 = gbutton("Adding noise", handler=function(h,...) nm1() )
  tooltip(nm_button1) <- tt_noi
  add(tmp1, nm_button1)
  enabled(nm_button1) <- FALSE
  nm_button2 = gbutton("Microaggregation", handler=function(h,...) nm2() )
  tooltip(nm_button2) <- tt_ma
  add(tmp1, nm_button2)
  enabled(nm_button2) <- FALSE
  addSpring(tmp1)
  nm_button3 = gbutton("Recalculate risk", handler=function(h,...) nm_risk_print_function() )
  tooltip(nm_button3) <- tt_rr
  add(tmp1, nm_button3)
  enabled(nm_button3) <- FALSE
  addSpring(tmp1)
  tmp1 = ggroup(container=tmp, horizontal=FALSE, expand=TRUE)
  tmp2 = gframe("Parameters for risk est.", container=tmp1, horizontal=FALSE)
  tmp3 = ggroup(container=tmp2)
  glabel("k ", container=tmp3)
  nm_risk_slider1 = gslider(from=0, to=0.1, by=0.01, value=0.01)
  tooltip(nm_risk_slider1) <- tt_slider1
  enabled(nm_risk_slider1) = FALSE
  add(tmp3, nm_risk_slider1, expand=TRUE)
  tmp3 = ggroup(container=tmp2)
  glabel("k2", container=tmp3)
  nm_risk_slider2 = gslider(from=0, to=0.05, by=0.01, value=0.05)
  tooltip(nm_risk_slider2) <- tt_slider2
  enabled(nm_risk_slider2) = FALSE
  add(tmp3, nm_risk_slider2, expand=TRUE)
  tmp1 = ggroup(container=tmp)
  tmp2 = gframe("Risk/Utility for continuous key variables", container=tmp1)
  nm_risk_print = gtext(text="", width=240, height=140)
  tooltip(nm_risk_print) <- tt_nmr
  add(tmp2, nm_risk_print)
  # End - numericalMethod Container
}
# TODO: remove for final version
#sdcGUI()
