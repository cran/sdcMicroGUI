.onLoad <- function(lib,pkg)
	{
		#cat("\n--------\n\n")
		#print(citation("sdcMicroGUI"))
		#print(citEntry(entry="Article",
		#				title = "A Graphical User Interface for Microdata Protection Which Provides Reproducibility and Interactions: the sdcMicro GUI",
		#				author = personList(as.person("Matthias Templ, Thomas Petelin")),
		#			    journal = "Transactions on Data Privacy",
		#				year = "2009",
		#				volume = "2",
		#				number = "3",
		#				pages = "207--224",						
		#				textVersion =
		#						paste("Matthias Templ",
		#								" (2009). A Graphical User Interface for Microdata Protection Which Provides Reproducibility and Interactions: the sdcMicro GUI. Transactions on Data Privacy, 2(3),207-224 ",
		#								".", sep="")))
  packageStartupMessage("\n--------\n")
  packageStartupMessage("you may start the graphical user interface by running 'sdcGUI()'\n")
  #sdcGUI()
}

