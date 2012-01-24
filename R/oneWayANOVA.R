# last modified: 27 May 2012 by Daniela Vicente
# Some Rcmdr dialogs for the StatisticalURV package

oneWayANOVA <- function(){

#Required libraries
	require(multcomp)
	require(agricolae)
	
#Principal Window
	ttANOVA<-tktoplevel()
	tkfocus(ttANOVA)
	tkwm.title(ttANOVA,"ANOVA de un Factor")
# Window s frames
	frameOver <-tkframe(ttANOVA,relief="groove",borderwidth=1)
	frameVar<-tkframe(frameOver,borderwidth=10)
	frameDependent <- tkframe(frameVar)
	frameFactor <- tkframe(frameVar)
	frameOp <- tkframe(frameOver,borderwidth=10)
	frameOCH <- tkframe(ttANOVA,borderwidth=5)
	

#creation new environment
	envANOVA <- new.env(parent=baseenv())

#variables
	dependentsList <- Numeric()  #Numeric = Dependiente
	factorList <- Factors()		#Factor=Factor

##global variables
#Post hoc...
	#Equality of variances
	assign("okBonferroni",0,envir=envANOVA)
	assign("okTukey",0, envir=envANOVA)
	assign("okLSD",0,envir=envANOVA)
	#Conficende Level
	assign("okConfLvl",0.95, envir=envANOVA)
#Options
	assign("okMean",1, envir=envANOVA)
	assign("okDevSt",1, envir=envANOVA)
	assign("okQuant",0, envir=envANOVA)
#graphics
	assign("okErrGraph","null", envir=envANOVA)
	assign("okGraphConfLvl",0.95, envir=envANOVA)
#fi global variables

#Select box with Scrollbar for Numeric variables
	srcDependents <- tkscrollbar(frameDependent, repeatinterval=5,command=function(...)tkyview(tlDependents,...))
	tlDependents<-tklistbox(frameDependent,height=4,selectmode="single",yscrollcommand=function(...)tkset(srcDependents,...),background="white")
	titleListBoxDep<-tklabel(frameDependent,text="Dependientes(escoja 1 o mas):")
#Numeric Selected box with Scrollbar	
	srcDependents2 <- tkscrollbar(frameDependent, repeatinterval=5,command=function(...)tkyview(tlDependents2,...))
	tlDependents2<-tklistbox(frameDependent,height=4,selectmode="single",yscrollcommand=function(...)tkset(srcDependents2,...),background="white")
	titleListBoxDep2<-tklabel(frameDependent,text="   Dependientes:")
	
#Dependent ScrollBox Actions
	for (i in dependentsList) 
	{
		tkinsert(tlDependents, "end", i)
	}
	OnOkSelectDependent <- function()
	{
		elecDependent <- tkget(tlDependents,tkcurselection(tlDependents))
		tkdelete(tlDependents,tkcurselection(tlDependents))
		tkinsert(tlDependents2,"end",elecDependent)
	}
	OKSelectDependent.but <-tkbutton(frameDependent,text="->",width="5",command=OnOkSelectDependent)
	OnOkUnselectDependent <- function()
	{
		elecDependent <- tkget(tlDependents2,tkcurselection(tlDependents2))
		tkdelete(tlDependents2,tkcurselection(tlDependents2))
		tkinsert(tlDependents,"end",elecDependent)
	}
	OKUnselectDependent.but <-tkbutton(frameDependent,text="<-",width="5",command=OnOkUnselectDependent)

#Select box with Scrollbar for Factor variables
	srcFactor <- tkscrollbar(frameFactor, repeatinterval=5,command=function(...)tkyview(tlFactor,...))
	tlFactor<-tklistbox(frameFactor,height=4,selectmode="single",yscrollcommand=function(...)tkset(srcFactor,...),background="white")
	titleListBoxFac<-tklabel(frameFactor,text="Factores(escoja 1 o mas):")
#Factor Selected box with Scrollbar
	srcFactor2 <- tkscrollbar(frameFactor, repeatinterval=5,command=function(...)tkyview(tlFactor2,...))
	tlFactor2<-tklistbox(frameFactor,height=4,selectmode="single",yscrollcommand=function(...)tkset(srcFactor2,...),background="white")
	titleListBoxFac2<-tklabel(frameFactor,text="   Factores:")

#Factor ScrollBox Actions
	for (i in factorList) 
	{
		tkinsert(tlFactor, "end", i)
	}
	OnOkSelectFactor <- function()
	{
		elecFactor <- tkget(tlFactor,tkcurselection(tlFactor))
		tkdelete(tlFactor,tkcurselection(tlFactor))
		tkinsert(tlFactor2,"end",elecFactor)
	}
	OKSelectFactor.but <-tkbutton(frameFactor,text="->",width="5",command=OnOkSelectFactor)
	OnOkUnselectFactor <- function()
	{
		elecFactor <- tkget(tlFactor2,tkcurselection(tlFactor2))
		tkdelete(tlFactor2,tkcurselection(tlFactor2))
		tkinsert(tlFactor,"end",elecFactor)
	}
	OKUnselectFactor.but <-tkbutton(frameFactor,text="<-",width="5",command=OnOkUnselectFactor)

#Statistical Model ANOVA One Factor	
	OnOKANOVA <- function()
	{
		tkfocus(CommanderWindow())
		
		sizeDependent<-tksize(tlDependents2)
		sizeFactor<-tksize(tlFactor2)

		if (as.numeric(sizeDependent)==0) #Not selected dependent variable by User
		{
			errorCondition(ttANOVA,recall=oneWayANOVA, message=gettextRcmdr("Selecciona Una variable Dependiente"))
			tkmessageBox(message="Seleciona Una variable Dependiente   ",icon="error",type="ok")
			return()
		}
		if (as.numeric(sizeFactor) == 0) #Not selected factor variable by User
		{
			tkmessageBox(message="Seleciona un Factor   ",icon="error",type="ok")
			errorCondition(ttANOVA,recall=oneWayANOVA, message=gettextRcmdr("Selecciona un Factor   "))
			return()
		}
		okBonferroni <- get("okBonferroni",envir=envANOVA)
		okTukey	<- get("okTukey", envir=envANOVA)
		okLSD<- get("okLSD",envir=envANOVA)
		#Conficende Level
		okConfLvl<- get("okConfLvl", envir=envANOVA)
		#Options
		okMean<- get("okMean", envir=envANOVA)
		okDevSt<- get("okDevSt", envir=envANOVA)
		okQuant<- get("okQuant", envir=envANOVA)
		#graphics
		okErrGraph<- get("okErrGraph", envir=envANOVA)
		okGraphConfLvl<- get("okGraphConfLvl",envir=envANOVA)

		
		#Anova model for all selected denpendent variables
		for (i in 0:(as.numeric(sizeDependent)-1))
		{
			dependent<-tkget(tlDependents2,i)
			for(j in 0:(as.numeric(sizeFactor)-1)) #Anova model for all selected Factor
			{
				factor<-tkget(tlFactor2,j)
				#Stadistic data
				.activeDataSet <- ActiveDataSet()
				#ANOVA, model statistic
				modelValue <- ("Anova")
				command <- paste(modelValue, " <- aov(", dependent, " ~ ", factor, ", data=", .activeDataSet, ")", sep="")
				justDoIt(command)
				logger(command)
				doItAndPrint(paste("summary(", modelValue, ")", sep=""))

				#command shows means, st. dev. and quantiles
				if (okMean==1 && okDevSt==1 && okQuant==1){  
					doItAndPrint(paste("numSummary(", .activeDataSet, "$", dependent, " , groups=", .activeDataSet, "$", factor,")", sep=""))
				#command shows means and st. dev.
				}else if (okMean==1 && okDevSt==1){
					doItAndPrint(paste("numSummary(", .activeDataSet, "$", dependent, " , groups=", .activeDataSet, "$", factor,
									   ', statistics=c("mean", "sd"))', sep=""))
				#command shows means and quantiles
				}else if (okMean==1 && okQuant==1){
					doItAndPrint(paste("numSummary(", .activeDataSet, "$", dependent, " , groups=", .activeDataSet, "$", factor,
									   ', statistics=c("mean", "quantiles"))', sep=""))
				#command shows st. dev. and quantiles
				}else if (okDevSt==1 && okQuant==1){
					doItAndPrint(paste("numSummary(", .activeDataSet, "$", dependent, " , groups=", .activeDataSet, "$", factor,
									   ', statistics=c("sd", "quantiles"))', sep=""))
				#command shows means
				}else if (okMean==1){
					doItAndPrint(paste("numSummary(", .activeDataSet, "$", dependent, " , groups=", .activeDataSet, "$", factor,
									   ', statistics=c("mean"))', sep=""))
				#command shows st. dev.
				}else if (okDevSt==1){
					doItAndPrint(paste("numSummary(", .activeDataSet, "$", dependent, " , groups=", .activeDataSet, "$", factor,
									   ', statistics=c("sd"))', sep=""))
				#command shows quantiles
				}else if (okQuant==1){
					doItAndPrint(paste("numSummary(", .activeDataSet, "$", dependent, " , groups=", .activeDataSet, "$", factor,
									   ', statistics=c("quantiles"))', sep=""))
				}
				activeModel(modelValue)
				#Bonferroni
				if (okBonferroni==1){
					command <- paste("pairwise.t.test(", .activeDataSet, "$", dependent, ", ",.activeDataSet, "$", factor, ',p.adj = "bonferroni", conf.level = ',okConfLvl,")", sep="")
					doItAndPrint(command)
				}
				#Tukey
				if (okTukey == 1)
				{
					modelValue <- ("Anova")
					tukey <-("dataTukey")
					command <- paste(tukey," <- glht(", modelValue, ", linfct = mcp(", factor, ' = "Tukey"))', sep="")
					justDoIt(command)
					logger(command)
					invLevel=okConfLvl
					command <- paste("showDataTukey <- confint(",tukey,", level=",invLevel,")", sep="")
					doItAndPrint(command)
					doItAndPrint("showDataTukey")
					justDoIt("x11()")
					justDoIt("oldConfigPlot <- par(oma=c(0,5,0,0))")
					command<-paste("plot(showDataTukey)", sep="")
					justDoIt(command)
					logger(command)
					justDoIt("par(oldConfigPlot)")
				}
				#LSD method
				if (okLSD == 1){
					#valueDMerror <- ("omAnova") 
					errorLvl=(1 - okConfLvl)
					command <- paste("omAnova <- anova(lm(",dependent," ~ ",factor,",", .activeDataSet,"))",sep="")
					doItAndPrint(command)
					command <- paste("LSD.test(" ,.activeDataSet, "$", dependent,",",.activeDataSet, "$", factor,", omAnova$'Sum Sq'[2], omAnova$'Mean Sq'[2], alpha=",errorLvl,")",sep="")
					doItAndPrint(command)
				}
				#Draw graphics
				if (okErrGraph != "null"){
					if (okErrGraph== "conf.int") {
						justDoIt("x11()")
						doItAndPrint(paste("plotMeans(", .activeDataSet, "$", dependent,
										   ", ", .activeDataSet, "$", factor,
										   ', error.bars="', okErrGraph, '", level=', okGraphConfLvl, ')', sep=""))
					}else{
						justDoIt("x11()")
						doItAndPrint(paste("plotMeans(", .activeDataSet, "$", dependent,
										   ", ", .activeDataSet, "$", factor,
										   ', error.bars="', okErrGraph, '")', sep=""))
						
					}
				}
			}
		}
		tkdestroy(ttANOVA)
	}

#ANOVA s Window buttons Ok, Cancel, Help, Posthoc, Options, Graphics 
	OKANOVA.but <-tkbutton(frameOCH,text="Aceptar",width="12",command=OnOKANOVA)
	CancelANOVA.but <-tkbutton(frameOCH,text="Cancel",width="12",command=function()OnCancel(ttANOVA))
	HelpANOVA.but <-tkbutton(frameOCH,text="Ayuda",width="12",command=function()OnHelp(ttANOVA,"oneWayANOVAURV"))
	posthoc.but <-tkbutton(frameOp,text= "Post hoc...",width="12",command=function()OnPosthoc(envANOVA))
	options.but <-tkbutton(frameOp,text="Opciones",width="12",command=function()OnOptions(envANOVA))
	graphics.but <-tkbutton(frameOp,text= "Graficas",width="12",command=function()OnGraphics(envANOVA))

#Organizing frameDependent objects at frameDependent
	tkgrid(titleListBoxDep,titleListBoxDep2,columnspan=5)
	tkgrid.configure(titleListBoxDep,sticky="w")
	tkgrid.configure(titleListBoxDep2,sticky="w")
	tkgrid(tlDependents,srcDependents,tklabel(frameDependent,text="  "),OKSelectDependent.but,OKUnselectDependent.but,tklabel(frameDependent,text="  "),tlDependents2,srcDependents2)
	tkgrid.configure(srcDependents,rowspan=4,sticky="nsw")
	tkgrid.configure(srcDependents2,rowspan=4,sticky="nsw")
	tkgrid.configure(OKSelectDependent.but,column=3,sticky="new")
	tkgrid.configure(OKUnselectDependent.but,column=3,sticky="sew")	

#Organizing frameFactor objects at frameFactor
	tkgrid(titleListBoxFac,titleListBoxFac2,columnspan=5)
	tkgrid.configure(titleListBoxFac,sticky="w")
	tkgrid.configure(titleListBoxFac2,sticky="w")
	tkgrid(tlFactor,srcFactor,tklabel(frameFactor,text="  "),OKSelectFactor.but,OKUnselectFactor.but,tklabel(frameFactor,text="  "),tlFactor2,srcFactor2)
	tkgrid.configure(srcFactor,rowspan=4,sticky="nsw")
	tkgrid.configure(srcFactor2,rowspan=4,sticky="nsw")
	tkgrid.configure(OKSelectFactor.but,column=3,sticky="new")
	tkgrid.configure(OKUnselectFactor.but,column=3,sticky="sew")	

#Organizing frameOp objects at frameOp
	tkgrid(tklabel(frameOp,text="    "))
	tkgrid(posthoc.but,columnspan=1,sticky="nw")
	tkgrid(options.but,columnspan=1,sticky="nw")
	tkgrid(graphics.but,columnspan=1,sticky="nw")

#Organizing frameOCH  objects at frameOCH
	tkgrid(OKANOVA.but, CancelANOVA.but,HelpANOVA.but)

#Organinzing frameOver
	tkgrid(frameDependent)
	tkgrid(tklabel(frameVar,text="    "))
	tkgrid(frameFactor)
	tkgrid(frameVar,frameOp,sticky="n")

#Include frameOver the ttANOVA
	tkgrid(frameOver)
	tkgrid(frameOCH)
}

#########################################################################
######################## POST HOC WINDOW ################################
#########################################################################
OnPosthoc <- function(env)
{
#Principal Window
	ttPH <- tktoplevel()
	tkwm.title(ttPH,"ANOVA de un Factor: Comparaciones Multiples post hoc")
#Window s Frames
	frameOverallPH <- tkframe(ttPH, relief="groove", padx=37, borderwidth=1)
	frameWorkPH <- tkframe(frameOverallPH,borderwidth=10)
	frameEqualVar <- tkframe(frameWorkPH,relief="groove",borderwidth=2)
	frameConfidenceLvl <- tkframe(frameWorkPH)
	frameOCHPH <- tkframe(ttPH,borderwidth=5)

	
#Equality of variances		
#Bonferroni
	cbBonferroni <- tkcheckbutton(frameEqualVar)
	cbValueBon <- tclVar("0")
	tkconfigure(cbBonferroni,variable=cbValueBon)
	textBonferroni<-tklabel(frameEqualVar,text="Bonferroni  ")
		
#Tukey
	cbTukey <- tkcheckbutton(frameEqualVar)
	cbValueTuk <- tclVar("0")
	tkconfigure(cbTukey,variable=cbValueTuk)
	textTukey<-tklabel(frameEqualVar,text="Tukey  ")
		
#LSD
	cbLSD <- tkcheckbutton(frameEqualVar)
	cbValueLSD <- tclVar("0")
	tkconfigure(cbLSD,variable=cbValueLSD)
	textLSD<-tklabel(frameEqualVar,text="LSD  ")
		
#Confidence level
	confidenceLvl <- tclVar("0.95")
	entry.confidenceLvl <-tkentry(ttPH,width="10",textvariable=confidenceLvl)
	textConfidenceLvl<-tklabel(frameConfidenceLvl,text="Nivel de Confianza")
#TODO 

#Post hoc s Window buttons Ok, Cancel, Help			
#Acept PostHoc
	OnAceptPH <- function()
	{
		tkdestroy(ttPH)
#Equality of variances
		assign("okBonferroni",as.numeric(tclvalue(cbValueBon)), envir=env)
		assign("okTukey",as.numeric(tclvalue(cbValueTuk)), envir=env)
		assign("okLSD",as.numeric(tclvalue(cbValueLSD)), envir=env)
#Conficende Level
		assign("okConfLvl",as.numeric(tclvalue(confidenceLvl)), envir=env)
	}

#Post hoc s Window buttons Ok, Cancel, Help
	AceptPH.but <-tkbutton(frameOCHPH,text="Aceptar",width="12",command=OnAceptPH)
	CancelPH.but <-tkbutton(frameOCHPH,text="Cancelar",width="12",command=function()OnCancel(ttPH))
	HelpPH.but <-tkbutton(frameOCHPH,text="Ayuda",width="12",command=function()OnHelp(ttPH,"oneWayANOVAURV"))

#Organizing frameEqualVar s objects at frameEqualVar
	tkgrid(cbBonferroni,textBonferroni,cbTukey,textTukey,cbLSD,textLSD)

#Organizing frameConfidendeLvl s objects at frameConfidenceLvl
	tkgrid(textConfidenceLvl,entry.confidenceLvl)

#Organizing frameOCHPH s objects at frameOCHPH
	tkgrid(AceptPH.but,CancelPH.but,HelpPH.but)

#Organizing framework s objects at frameOCHPH	
	tkgrid(tklabel(frameWorkPH,text="Asumiendo variables iguales:"))
	tkgrid(frameEqualVar)
	tkgrid(tklabel(frameWorkPH,text="    "))
	tkgrid(frameConfidenceLvl)

#Organinzing frameOverallPH
	tkgrid(frameWorkPH)

#Include frameOverallPH of ttPH
	tkgrid(frameOverallPH)
	tkgrid(frameOCHPH)
#tkgrid(frameHPH,sticky="e")
}

#########################################################################
######################## OPTIONS WINDOW #################################
#########################################################################
OnOptions <- function(env)
{
#Principal Window
	ttO <- tktoplevel()
	tkwm.title(ttO,"ANOVA de un Factor: Opciones")
#Window s Frames
	frameOverallO <- tkframe(ttO, relief="groove",padx=108, borderwidth=1)
	frameWorkO <- tkframe(frameOverallO, borderwidth=10)
	frameEstatistic <- tkframe(frameWorkO,relief="groove",borderwidth=2)
	frameOCHO <- tkframe(ttO,borderwidth=5)
		
#Mean #Mitjanes
	cbMean <- tkcheckbutton(frameEstatistic)
	cbMeanValue <- tclVar("1")
	tkconfigure(cbMean,variable=cbMeanValue)
	textMean<-tklabel(frameEstatistic,text="Medias")
		
#st. dev.
	cbStDev <- tkcheckbutton(frameEstatistic)
	cbStDevValue <- tclVar("1")
	tkconfigure(cbStDev,variable=cbStDevValue)
	textStDev<-tklabel(frameEstatistic,text="Desv. st")
		
#Quantile
	cbQuant <- tkcheckbutton(frameEstatistic)
	cbQuantValue <- tclVar("0")
	tkconfigure(cbQuant,variable=cbQuantValue)
	textQuant<-tklabel(frameEstatistic,text="Cuantiles  ")
		
##Options Window buttons Ok, Cancel, Help			
#Acept Options
	OnAceptO <- function()
	{
		tkdestroy(ttO)
		assign("okMean",as.numeric(tclvalue(cbMeanValue)), envir=env)
		assign("okDevSt",as.numeric(tclvalue(cbStDevValue)), envir=env)
		assign("okQuant",as.numeric(tclvalue(cbQuantValue)), envir=env)
	}


#Options Window buttons Ok, Cancel, Help
	AceptO.but <-tkbutton(frameOCHO,text="Aceptar",width="12",command=OnAceptO)
	CancelO.but <-tkbutton(frameOCHO,text="Cancelar",width="12",command=function()OnCancel(ttO))
	HelpO.but <-tkbutton(frameOCHO,text="Ayuda",width="12",command=function()OnHelp(ttO,"oneWayANOVAURV"))
		
#Organizing frameStatistic s objects at frameStatistic
	tkgrid(cbMean,textMean,sticky="w")
	tkgrid(cbStDev,textStDev,sticky="w")
	tkgrid(cbQuant,textQuant,sticky="w")

#Organizing frameOCHO s objects at frameOCHO
	tkgrid(AceptO.but,CancelO.but,HelpO.but)


#Organizing frameWorkO s objects at frameWorkO
	tkgrid(tklabel(frameWorkO,text="Estadisticos"))
	tkgrid(frameEstatistic)

#Organinzing frameOverallO
	tkgrid(frameWorkO)

#Include frameOverallO of ttPH
	tkgrid(frameOverallO)
	tkgrid(frameOCHO)
}

#########################################################################
######################## GRAPHIC S WINDOWS ##############################
#########################################################################
OnGraphics	<- function(env)
{
#Principal Window
	ttGraph <- tktoplevel()
	tkwm.title(ttGraph,"ANOVA de un Factor: Graficas")
# Window s frames
	frameOverallGraph <- tkframe(ttGraph,relief="groove",borderwidth=1)
	frameWorkGraph <- tkframe(frameOverallGraph,padx=46, borderwidth=10)
	frameGraph <-tkframe(frameWorkGraph,relief="groove",borderwidth=2)
	frameOCHGraph <- tkframe(ttGraph,borderwidth=5)

		
	valueErrGraph <- tclVar("null")
		
#Tipical Errors
	rbTipErr <- tkradiobutton(frameGraph, variable=valueErrGraph, value="se")
	textTipErr<-tklabel(frameGraph,text="Errores Tipicos  ")
		
#Tipical dev.
	rbTipDev <- tkradiobutton(frameGraph, variable=valueErrGraph, value="sd")
	textTipDev<-tklabel(frameGraph,text="Desviaciones Tipicas  ")
		
#Confidence Intervals
	rbConfInt <- tkradiobutton(frameGraph, variable=valueErrGraph, value="conf.int")
	textConfInt<-tklabel(frameGraph,text="Intervalos de Confianza  ")
		
#Confidence Level graph
	graphConfidenceLvl <- tclVar("0.95")
	entry.graphConfidenceLvl <-tkentry(frameGraph,width="6",textvariable=graphConfidenceLvl)
	textGraphConfidenceLvl<-tklabel(frameGraph,text="Nivel de Confianza")
		
#Without bar if errors
	rbNonBarErr <- tkradiobutton(frameGraph, variable=valueErrGraph, value="none")
	textNonBarErr<-tklabel(frameGraph,text="Sin Barra de Errores  ")
		
		
#Graph Window action Acept, Cancel, Help button
	OnAceptGraph <- function()
	{
		tkdestroy(ttGraph)
		tkfocus(CommanderWindow())
		print(tclvalue(valueErrGraph))
		assign("okErrGraph",tclvalue(valueErrGraph), envir=env)
		assign("okGraphConfLvl",as.numeric(tclvalue(graphConfidenceLvl)), envir=env)
	}

#Graph Window buttons Ok, Cancel, Help
	AceptGraph.but <-tkbutton(frameOCHGraph,text="Aceptar",width="12",command=OnAceptGraph)
	CancelGraph.but <-tkbutton(frameOCHGraph,text="Cancelar",width="12",command=function()OnCancel(ttGraph))
	HelpGraph.but <-tkbutton(frameOCHGraph,text="Ayuda",width="12",command=function()OnHelp(ttGraph,"oneWayANOVAURV"))

#Organizing frameGraph s objects at frameGraph
	tkgrid(textTipErr,rbTipErr,sticky="w")
	tkgrid(textTipDev,rbTipDev,sticky="w")
	tkgrid(textConfInt,rbConfInt,sticky="w")
	tkgrid(textGraphConfidenceLvl,entry.graphConfidenceLvl ,sticky="e")
	tkgrid(tklabel(frameGraph,text="    "))
	tkgrid(textNonBarErr,rbNonBarErr ,sticky="w")

#Organizing frameWorkGraph s objects at frameWorkGRaph
	tkgrid(tklabel(frameWorkGraph,text="Barras de Error"))
	tkgrid(frameGraph)

#Organinzing frameOverallGraph
	tkgrid(frameWorkGraph)

#Organizing frameOCHPH s objects at frameOCHPH
	tkgrid(AceptGraph.but,CancelGraph.but,HelpGraph.but)

#Include frameOverallGraph of ttGraph
	tkgrid(frameOverallGraph)
	tkgrid(frameOCHGraph)
}

