\name{twoWayANOVAURV}
\alias{twoWayANOVAURV}
\title{ 
	Two Way ANOVA Dialog to the Rcmdr Package
}

\description{
This dialog is used to show two way ANOVA results, and plot means}

\details{
	The upper left model-formula box specifies the dependent variable, the middle left model-formula box specifies the factor variable, the bottom left model-formula box specifies the factor variable. The right boxes specify the selected variables of both.If you choose more than a dependent variable the result will be repeated for each of them.\cr
	If you introduce a covariate, the results showed uses the  \code{\link[base]{summary}} function.
	
	There are two option buttons: Models and Plots.\cr

Model's Window. You can choose between: \cr
- Complete Model: There are interaction between factors. \cr 
- Main Factors: There are not interaction between factors. \cr
- Interactions of order two. This one is pre-selected.\cr
You can choose the type of sums of squares:\cr
- Type I sums of Squares: This one uses: \code{\link[stats]{lm}}, \code{\link[stats]{anova}} functions \cr
- Type II sums of Squares: This one uses: \code{\link[stats]{lm}}, \code{\link[car]{Anova}} functions\cr
- Type III sums of Squares: This one is pre-selected. It uses: \code{\link[stats]{lm}}, \code{\link[car]{Anova}} functions\cr

Graphics Window: Plots means with error bars for standard error, standard deviation, confidence interval or without errors bars. It uses \code{\link[Rcmdr]{plotMeans}} function.\cr
}


\author{ Daniela Vicente <daniela.vicente@estudiants.urv.cat> }

\seealso{
	\code{\link{Commander}}, \code{\link[stats]{lm}}, \code{\link[stats]{anova}}, \code{\link[base]{summary}}, \code{\link[car]{Anova}},  \code{\link[Rcmdr]{plotMeans}}.
	}

\keyword{models}
