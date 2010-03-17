# last modified: 30 May 2012 by Daniela Vicente
# Two Way ANOVA dialog for the StatisticalURV package-plugin 
  
twoWayANOVA <- function(){

#Required libraries
	require(multcomp)
	require(car)

#Principal Window
	ttTwoANOVA<-tktoplevel()
	tkfocus(ttTwoANOVA)
	tkwm.title(ttTwoANOVA,"ANOVA de Multiples Factores")
# Window s frames
	frameOverall <-tkframe(ttTwoANOVA,relief="groove",borderwidth=1)
	frameWork<-tkframe(frameOverall,borderwidth=10)
	frameDependent <- tkframe(frameWork)
	frameFactor <- tkframe(frameWork)
	frameCovariable <- tkframe(frameWork)
	frameOp <- tkframe(frameOverall,borderwidth=10)
	frameOCH <- tkframe(ttTwoANOVA,borderwidth=5)	
	
#creation new environment
	envTwoWayANOVA <- new.env(parent=baseenv())
	
#variables
	dependentsList <- Numeric()  #Numeric = Dependiente
	factorList <- Factors()		#Factor=Factor
	
##global variables
#Models and type
	assign("typeMeanSqValue","Tipo III", envir=envTwoWayANOVA )
	assign("okCMod",0, envir=envTwoWayANOVA)
	assign("okMainFac",0, envir=envTwoWayANOVA)
	assign("okIntOrd",1, envir=envTwoWayANOVA)
#Graphics
	assign("okErrGraph","null", envir=envTwoWayANOVA)
	assign("okGraphConfLvl",0.95, envir=envTwoWayANOVA)
	
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
		iSelection <- tkcurselection(tlDependents)
		elecDependent <- tkget(tlDependents,iSelection)
		tkdelete(tlDependents,iSelection)
		tkdelete(tlCovariable,iSelection)
		tkinsert(tlDependents2,"end",elecDependent)
	}
	OKSelectDependent.but <-tkbutton(frameDependent,text="->",width="5",command=OnOkSelectDependent)
	OnOkUnselectDependent <- function()
	{
		elecDependent <- tkget(tlDependents2,tkcurselection(tlDependents2))
		tkdelete(tlDependents2,tkcurselection(tlDependents2))
		tkinsert(tlDependents,"end",elecDependent)
		tkinsert(tlCovariable,"end",elecDependent)
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
	
#Select box with Scrollbar for Covariable variables
	srcCovariable <- tkscrollbar(frameCovariable, repeatinterval=5,command=function(...)tkyview(tlCovariable,...))
	tlCovariable<-tklistbox(frameCovariable,height=4,selectmode="single",yscrollcommand=function(...)tkset(srcCovariable,...),background="white")
	titleListBoxCov<-tklabel(frameCovariable,text="Covariables(escoja 0 o mas):")
#Covariable Selected box with Scrollbar
	srcCovariable2 <- tkscrollbar(frameCovariable, repeatinterval=5,command=function(...)tkyview(tlCovariable2,...))
	tlCovariable2<-tklistbox(frameCovariable,height=4,selectmode="single",yscrollcommand=function(...)tkset(srcCovariable2,...),background="white")
	titleListBoxCov2<-tklabel(frameCovariable,text="   Covariable:")
	
#Covariable ScrollBox Actions
	for (i in dependentsList) 
	{
		tkinsert(tlCovariable, "end", i)
	}
	OnOkSelectCovariable <- function()
	{
		iSelection <- tkcurselection(tlCovariable)
		elecCovariable <- tkget(tlCovariable,iSelection)
		tkdelete(tlCovariable,iSelection)
		tkdelete(tlDependents,iSelection)
		tkinsert(tlCovariable2,"end",elecCovariable)
	}
	OKSelectCovariable.but <-tkbutton(frameCovariable,text="->",width="5",command=OnOkSelectCovariable)
	OnOkUnselectCovariable <- function()
	{
		elecCovariable <- tkget(tlCovariable2,tkcurselection(tlCovariable2))
		tkdelete(tlCovariable2,tkcurselection(tlCovariable2))
		tkinsert(tlCovariable,"end",elecCovariable)
		tkinsert(tlDependents,"end",elecCovariable)
	}
	OKUnselectCovariable.but <-tkbutton(frameCovariable,text="<-",width="5",command=OnOkUnselectCovariable)
	
	#Statistical Model ANOVA Multiples Factor
	OnOKTwoWayANOVA <- function()
	{
		tkfocus(CommanderWindow())
		sizeDependent<-as.numeric(tksize(tlDependents2))
		sizeFactor<-as.numeric(tksize(tlFactor2))
		sizeCovariable<-as.numeric(tksize(tlCovariable2))
	  
		if (sizeDependent==0) #Not selected dependent variable by User
		{
			errorCondition(ttTwoANOVA,recall=twoWayANOVA, message=gettextRcmdr("Selecciona Una variable Dependiente"))
			tkmessageBox(message="Seleciona Una variable Dependiente   ",icon="error",type="ok")
			return()
		}
		if (sizeFactor == 0) #Not selected factor variable by User
		{
			tkmessageBox(message="Seleciona un Factor   ",icon="error",type="ok")
			errorCondition(ttTwoANOVA,recall=twoWayANOVA, message=gettextRcmdr("Selecciona un Factor   "))
			return()
		}
  
		typeMeanSqValue <- get("typeMeanSqValue", envir=envTwoWayANOVA )
		okCMod <- get("okCMod", envir=envTwoWayANOVA)
		okMainFac <- get("okMainFac", envir=envTwoWayANOVA)
		okIntOrd <- get("okIntOrd", envir=envTwoWayANOVA)
		okErrGraph <- get("okErrGraph", envir=envTwoWayANOVA)
		okGraphConfLvl <- get("okGraphConfLvl", envir=envTwoWayANOVA)
		
		#Anova model for all selected denpendent variables
		for (i in 0:(sizeDependent - 1))
		{
			dependent <- tkget(tlDependents2,i)
			#Stadistic data
			.activeDataSet <- ActiveDataSet()
			
			if (okCMod == 1){
				#ANOVA, model statistic
				modelValueCMod <- ("twoWayAnovaCMod")
				modelValueCMod2 <- ("twoWayAnovaCMod2")
				factor<-tkget(tlFactor2,0)
				command <- paste(modelValueCMod, " <- lm(", dependent, " ~ ", factor, sep="")				
				command2 <- paste(modelValueCMod2, " <- lm(", dependent, " ~ ", factor, sep="")
				
				if (sizeFactor > 1){
					for(j in 1:(sizeFactor-1)) #Anova model for all selected Factor
					{
						factor<-tkget(tlFactor2,j)
						command <- paste(command, "*", factor, sep="")
						command2 <- paste(command2, "*", factor, sep="")
					}
				}
				if (sizeCovariable > 0){
					for(k in 0:(sizeCovariable-1))
					{
						covariable<-tkget(tlCovariable2,k)
						command2 <- paste(command2, "*", covariable, sep="")
					}
					
				}
				if (typeMeanSqValue=="Tipo I"){
					command <- paste(command, ", data=", .activeDataSet,")", sep="")
					command2 <- paste(command2, ", data=", .activeDataSet,")", sep="")
					doItAndPrint(command)
					doItAndPrint(paste("anova(",modelValueCMod,")", sep=""))
				}else if (typeMeanSqValue=="Tipo II"){
					command <- paste(command, ", data=", .activeDataSet,")", sep="")
					command2 <- paste(command2, ", data=", .activeDataSet,")", sep="")
					doItAndPrint(command)
					doItAndPrint(paste("Anova(",modelValueCMod,")", sep=""))
				}else{
					factor<-tkget(tlFactor2,0)
					command <- paste(command,", data=", .activeDataSet, ", contrast=list(",factor,"=contr.sum", sep="")
					command2 <- paste(command2,", data=", .activeDataSet, ", contrast=list(",factor,"=contr.sum", sep="")

					if (sizeFactor > 1){
						for(j in 1:(sizeFactor-1)) #Anova model for all selected Factor
						{
							factor<-tkget(tlFactor2,j)
							command <- paste(command, ",", factor,"=contr.sum", sep="")		
							command2 <- paste(command2, ",", factor,"=contr.sum", sep="")				

						}
					}
					command <- paste(command, "))", sep="")
					command2 <- paste(command2, "))", sep="")

					doItAndPrint(command)
					doItAndPrint(paste("Anova(",modelValueCMod,",type=3)"))
				}
				if (sizeCovariable > 0){
					doItAndPrint(command2)
					doItAndPrint(paste("summary(",modelValueCMod2,")", sep=""))
				}
			}
			if (okMainFac == 1){
				#ANOVA, model statistic
				modelValueMainFac <- ("twoWayAnovaMainFac")
				modelValueMainFac2 <- ("twoWayAnovaMainFac2")
				factor<-tkget(tlFactor2,0)
				command <- paste(modelValueMainFac, " <- lm(", dependent, " ~ ", factor, sep="")				
				command2 <- paste(modelValueMainFac2, " <- lm(", dependent, " ~ ", factor, sep="")				

				if (sizeFactor > 1){
					for(j in 1:(sizeFactor-1)) #Anova model for all selected Factor
					{
						factor<-tkget(tlFactor2,j)
						command <- paste(command, "+", factor, sep="")		
						command2 <- paste(command2, "+", factor, sep="")				

					}
				}
				if (sizeCovariable > 0){
					for(k in 0:(sizeCovariable-1))
					{
						covariable<-tkget(tlCovariable2,k)
						command2 <- paste(command2, "+", covariable, sep="")
					}
					
				}
				if (typeMeanSqValue=="Tipo I"){
					command <- paste(command, ", data=", .activeDataSet,")", sep="")
					command2 <- paste(command2, ", data=", .activeDataSet,")", sep="")

					doItAndPrint(command)
					doItAndPrint(paste("anova(",modelValueMainFac,")", sep=""))
				}else if (typeMeanSqValue=="Tipo II"){
					command <- paste(command, ", data=", .activeDataSet,")", sep="")
					command2 <- paste(command2, ", data=", .activeDataSet,")", sep="")

					doItAndPrint(command)
					doItAndPrint(paste("Anova(",modelValueMainFac,")", sep=""))
				}else{
					factor<-tkget(tlFactor2,0)
					command <- paste(command,", data=", .activeDataSet, ", contrast=list(",factor,"=contr.sum", sep="")
					command2 <- paste(command2,", data=", .activeDataSet, ", contrast=list(",factor,"=contr.sum", sep="")

					if (sizeFactor > 1){
						for(j in 1:(sizeFactor-1)) #Anova model for all selected Factor
						{
							factor<-tkget(tlFactor2,j)
							command <- paste(command, ",", factor,"=contr.sum", sep="")		
							command2 <- paste(command2, ",", factor,"=contr.sum", sep="")				

						}
					}
					command <- paste(command, "))", sep="")
					command2 <- paste(command2, "))", sep="")

					doItAndPrint(command)
					doItAndPrint(paste("Anova(",modelValueMainFac,",type=3)", sep=""))
				}
				if (sizeCovariable > 0){
					doItAndPrint(command2)
					doItAndPrint(paste("summary(",modelValueMainFac,")", sep=""))
				}
			}
			if (okIntOrd == 1){
				
				#ANOVA, model statistic
				modelValueOrderTwo <- ("twoWayAnovaOrderTwo")
				modelValueOrderTwo2 <- ("twoWayAnovaOrderTwo2")
				
				factor<-tkget(tlFactor2,0)
				command <- paste(modelValueOrderTwo, " <- lm(", dependent, " ~ ", factor, sep="")
				command2 <- paste(modelValueOrderTwo2, " <- lm(", dependent, " ~ ", factor, sep="")				

				if (sizeFactor > 1){
					for(j in 1:(sizeFactor-1)) #Anova model for all selected Factor
					{
						factor1<-tkget(tlFactor2,j)
						command <- paste(command, " + ", factor1, sep="")	
						command2 <- paste(command2, " + ", factor1, sep="")				

					}
				}
				if (sizeCovariable > 0){
					for(k in 0:(sizeCovariable-1))
					{
						covariable<-tkget(tlCovariable2,k)
						command2 <- paste(command2, " + ", covariable, sep="")
					}
					
				}
				if (sizeFactor > 1){
					for(j in 0:(sizeFactor-1)) #Anova model for all selected Factor
					{
						factor1 <- tkget(tlFactor2, j)
						if ((j + 1) <= (sizeFactor-1)){
							for (l in (j+1):(sizeFactor -1)){
								factor2 <- tkget(tlFactor2, l)
								command <- paste(command, " + ",factor1,"*",factor2, sep="")
								command2 <- paste(command2, " + ",factor1,"*",factor2, sep="")

							}
						}
						if (sizeCovariable > 0){
							for (k in 0:(sizeCovariable-1)){
								covariable <- tkget(tlCovariable2,k)
								command2 <- paste(command2, " + ",factor1,"*",covariable, sep="")
							}
						}
					}
				}
				if (sizeCovariable > 1){
					for(j in 0:(sizeCovariable-2)) #Anova model for all selected Factor
					{
						covariable1 <- tkget(tlCovariable2, j)
						if ((j + 1) <= (sizeCovariable-1)){
							for (l in (j+1):(sizeCovariable -1)){
								covariable2 <- tkget(tlCovariable2, l)
								command2 <- paste(command2, " + ",covariable1,"*",covariable2, sep="")
							}
						}
					}
				}
				if (typeMeanSqValue=="Tipo I"){
					command <- paste(command, ", data=", .activeDataSet,")", sep="")
					command2 <- paste(command2, ", data=", .activeDataSet,")", sep="")

					doItAndPrint(command)
					doItAndPrint(paste("anova(",modelValueOrderTwo,")", sep=""))
				}else if (typeMeanSqValue=="Tipo II"){
					command <- paste(command, ", data=", .activeDataSet,")", sep="")
					command2 <- paste(command2, ", data=", .activeDataSet,")", sep="")

					doItAndPrint(command)
					doItAndPrint(paste("Anova(",modelValueOrderTwo,")", sep=""))
				}else{
					factor<-tkget(tlFactor2,0)
					command <- paste(command,", data=", .activeDataSet, ", contrast=list(",factor,"=contr.sum", sep="")
					command2 <- paste(command2,", data=", .activeDataSet, ", contrast=list(",factor,"=contr.sum", sep="")

					if (sizeFactor > 1){
						for(j in 1:(sizeFactor-1)) #Anova model for all selected Factor
						{
							factor<-tkget(tlFactor2,j)
							command <- paste(command, ",", factor,"=contr.sum", sep="")	
							command2 <- paste(command2, ",", factor,"=contr.sum", sep="")				

						}
					}
					command <- paste(command, "))", sep="")
					command2 <- paste(command2, "))", sep="")

					doItAndPrint(command)
					doItAndPrint(paste("Anova(",modelValueOrderTwo,",type=3)", sep=""))
					
				}
				if (sizeCovariable > 0){
					doItAndPrint(command2)
					doItAndPrint(paste("summary(",modelValueOrderTwo2,")", sep=""))
				}
			}
		}
		
		#Draw graphics
		if (okErrGraph != "null"){
			factor<-tkget(tlFactor2,0)
			
			if (okErrGraph== "conf.int") {
				if(sizeFactor > 1){
					for(j in 0:(sizeFactor-2)){
						factor1<-tkget(tlFactor2,j)
						if ((j+1) <= (sizeFactor-1)){
							for (k in (j+1):(sizeFactor-1) ){
								factor2<-tkget(tlFactor2,k)
								justDoIt("x11()")
								doItAndPrint(command <- paste("plotMeans(", .activeDataSet, "$", dependent,", ", .activeDataSet, "$", factor1,", ", .activeDataSet, "$", factor2,', error.bars="', okErrGraph, '", level=', okGraphConfLvl, ')', sep=""))
							}
						}
					}
				}else{
					justDoIt("x11()")
					doItAndPrint(paste("plotMeans(", .activeDataSet, "$", dependent,", ", .activeDataSet, "$", factor,', error.bars="', okErrGraph, '", level=', okGraphConfLvl, ')', sep=""))
				}
			}else{
				if(sizeFactor > 1){
					for(j in 0:(sizeFactor-2)){
						factor1<-tkget(tlFactor2,j)
						if ((j+1) <= (sizeFactor-1)){
							for (k in (j+1):(sizeFactor-1) ){
								factor2<-tkget(tlFactor2,k)
								justDoIt("x11()")
								doItAndPrint(paste("plotMeans(", .activeDataSet, "$", dependent,", ", .activeDataSet, "$", factor1,", ", .activeDataSet, "$", factor2,', error.bars="', okErrGraph, '")', sep=""))
							}
						}
					}
				}else{
					justDoIt("x11()")
					doItAndPrint(paste("plotMeans(", .activeDataSet, "$", dependent,", ", .activeDataSet, "$", factor,', error.bars="', okErrGraph, '")', sep=""))
				}
			}
		}
		tkdestroy(ttTwoANOVA)
	}
	
#ANOVA s Window buttons Ok, Cancel, Help, Posthoc, Models, Graphics 
	OKTwoWayANOVA.but <-tkbutton(frameOCH,text="Aceptar",width="12",command=OnOKTwoWayANOVA)
	CancelTwoWayANOVA.but <-tkbutton(frameOCH,text="Cancel",width="12",command=function()OnCancel(ttTwoANOVA))
	HelpTwoWayANOVA.but <-tkbutton(frameOCH,text="Ayuda",width="12",command=function()OnHelp(ttTwoANOVA,"twoWayANOVAURV"))
	models.but <-tkbutton(frameOp,text="Modelos",width="12",command=function()OnModelsTwoWayANOVA(envTwoWayANOVA))
	graphics.but <-tkbutton(frameOp,text= "Graficas",width="12",command=function()OnGraphicsTwoWayANOVA(envTwoWayANOVA))
	
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
	
#Organizing frameDependent objects at frameDependent
	tkgrid(titleListBoxCov,titleListBoxCov2,columnspan=5)
	tkgrid.configure(titleListBoxCov,sticky="w")
	tkgrid.configure(titleListBoxCov2,sticky="w")
	tkgrid(tlCovariable,srcCovariable,tklabel(frameCovariable,text="  "),OKSelectCovariable.but,OKUnselectCovariable.but,tklabel(frameCovariable,text="  "),tlCovariable2,srcCovariable2)
	tkgrid.configure(srcCovariable,rowspan=4,sticky="nsw")
	tkgrid.configure(srcCovariable2,rowspan=4,sticky="nsw")
	tkgrid.configure(OKSelectCovariable.but,column=3,sticky="new")
	tkgrid.configure(OKUnselectCovariable.but,column=3,sticky="sew")

  
#Organizing frameOp objects at frameOp
	tkgrid(tklabel(frameOp,text="    "))
	tkgrid(models.but,columnspan=1,sticky="nw")
	tkgrid(graphics.but,columnspan=1,sticky="nw")
	
#Organizing frameOCH  objects at frameOCH
	tkgrid(OKTwoWayANOVA.but, CancelTwoWayANOVA.but,HelpTwoWayANOVA.but)
	tkgrid.configure(OKTwoWayANOVA.but,column=1)
	tkgrid.configure(CancelTwoWayANOVA.but,column=2)
	tkgrid.configure(HelpTwoWayANOVA.but,column=5)
	
#Organinzing frameOverall
	tkgrid(frameDependent)
	tkgrid(tklabel(frameWork,text="    "))
	tkgrid(frameFactor)
	tkgrid(tklabel(frameWork,text="    "))
	tkgrid(frameCovariable)
	tkgrid(frameWork,frameOp,sticky="n")
	
#Include frameOver the ttTwoANOVA
	tkgrid(frameOverall)
	tkgrid(frameOCH)
}


#########################################################################
######################## MODELS WINDOW #################################
#########################################################################
OnModelsTwoWayANOVA <- function(env)
{
#Principal Window
	ttModels <- tktoplevel()
	tkwm.title(ttModels,"ANOVA Multiples Factores: Modelos")
#Window s Frames
	frameOverallModels <- tkframe(ttModels, relief="groove",padx=108, borderwidth=1)
	frameWorkModels <- tkframe(frameOverallModels, borderwidth=10)
	frameModels <- tkframe(frameWorkModels,relief="groove",borderwidth=2)
	frameOCHModels <- tkframe(ttModels,borderwidth=5)
	
#Complete Model
	cbCMod <- tkcheckbutton(frameModels)
	cbCModValue <- tclVar("0")
	tkconfigure(cbCMod,variable=cbCModValue)
	textCMod<-tklabel(frameModels,text="Completo")
	
#main factors
	cbMainFac <- tkcheckbutton(frameModels)
	cbMainFacValue <- tclVar("0")
	tkconfigure(cbMainFac,variable=cbMainFacValue)
	textMainFac<-tklabel(frameModels,text="Factores principales")
	
#Interaction of order two
	cbIntOrd <- tkcheckbutton(frameModels)
	cbIntOrdValue <- tclVar("1")
	tkconfigure(cbIntOrd,variable=cbIntOrdValue)
	textIntOrd<-tklabel(frameModels,text="Interaccion de orden 2")
	
#Mean Sq.
	textMeanSq<-tklabel(frameWorkModels,text="Suma de cuadrados Tipo:")
	typeMeanSqValue <- tclVar("Tipo III")
	rbMeanSqI <- tkradiobutton(frameWorkModels, variable=typeMeanSqValue, value="Tipo I")
	textMeanSqI<-tklabel(frameWorkModels,text="I  ")
	rbMeanSqII <- tkradiobutton(frameWorkModels, variable=typeMeanSqValue, value="Tipo II")
	textMeanSqII<-tklabel(frameWorkModels,text="II  ")
	rbMeanSqIII <- tkradiobutton(frameWorkModels, variable=typeMeanSqValue, value="Tipo III")
	textMeanSqIII<-tklabel(frameWorkModels,text="III  ")
	
	
##Models Window buttons Ok, Cancel, Help			
#Acept Models
	OnAceptO <- function()
	{
		tkdestroy(ttModels)
		assign("typeMeanSqValue",tclvalue(typeMeanSqValue), envir=env)
		assign("okCMod",as.numeric(tclvalue(cbCModValue)), envir=env)
		assign("okMainFac",as.numeric(tclvalue(cbMainFacValue)), envir=env)
		assign("okIntOrd",as.numeric(tclvalue(cbIntOrdValue)), envir=env)
	}
	
	
#Models Window buttons Ok, Cancel, Help
	AceptO.but <-tkbutton(frameOCHModels,text="Aceptar",width="12",command=OnAceptO)
	CancelO.but <-tkbutton(frameOCHModels,text="Cancelar",width="12",command=function()OnCancel(ttModels))
	HelpO.but <-tkbutton(frameOCHModels,text="Ayuda",width="12",command=function()OnHelp(ttModels,"twoWayANOVAURV"))
	
#Organizing frameStatistic s objects at frameStatistic
	tkgrid(cbCMod,textCMod,sticky="w")
	tkgrid(cbMainFac,textMainFac,sticky="w")
	tkgrid(cbIntOrd,textIntOrd,sticky="w")
	
#Organizing frameOCHModels s objects at frameOCHModels
	tkgrid(AceptO.but,CancelO.but,HelpO.but)
	
	
#Organizing frameWorkModels s objects at frameWorkModels
	tkgrid(tklabel(frameWorkModels,text="Estadisticos"))
	tkgrid(frameModels)
	tkgrid(textMeanSq,rbMeanSqI,textMeanSqI,rbMeanSqII,textMeanSqII,rbMeanSqIII,textMeanSqIII)
	
#Organinzing frameOverallModels
	tkgrid(frameWorkModels)
	
#Include frameOverallModels of ttPH
	tkgrid(frameOverallModels)
	tkgrid(frameOCHModels)
}

#########################################################################
######################## GRAPHIC S WINDOWS ##############################
#########################################################################
OnGraphicsTwoWayANOVA <- function(env)
{
#Principal Window
	ttGraph <- tktoplevel()
	tkwm.title(ttGraph,"ANOVA de Multiples Factores: Graficas")
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
		assign("okErrGraph",tclvalue(valueErrGraph), envir=env)
		assign("okGraphConfLvl",as.numeric(tclvalue(graphConfidenceLvl)), envir=env)
	}
	
#Graph Window buttons Ok, Cancel, Help
	AceptGraph.but <-tkbutton(frameOCHGraph,text="Aceptar",width="12",command=OnAceptGraph)
	CancelGraph.but <-tkbutton(frameOCHGraph,text="Cancelar",width="12",command=function()OnCancel(ttGraph))
	HelpGraph.but <-tkbutton(frameOCHGraph,text="Ayuda",width="12",command=function()OnHelp(ttGraph,"twoWayANOVAURV"))
	
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

