# last modified: 30 May 2012 by Daniela Vicente
# Cancel and Help functions for the StatisticalURV package

#Cancel
OnCancel <- function(frame)
{
	tkdestroy(frame)
	tkfocus(CommanderWindow())
	return()
}	
#Help
OnHelp <- function(frame,helpLink)
{
	tkfocus(CommanderWindow())
	tkdestroy(frame)
	print(help(helpLink))
	return()
}

