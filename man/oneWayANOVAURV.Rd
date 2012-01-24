\name{oneWayANOVAURV}
\alias{oneWayANOVAURV}
\title{ 
	One Way ANOVA Dialog to the Rcmdr Package
}

\description{
This dialog is used to show ANOVA results, and plot means}

\details{
	The upper left model-formula box specifies the dependent variable, the bottom left model-formula box specifies the factor variable. The right boxes specify the selected variables of both. If you choose more than a dependent variable the result will be repeated for each of them. \cr
	
There are three option buttons: Post Hoc, Options and Plots.\cr

Post Hoc's Window. You can choose between: \cr
- Bonferroni: This one uses \code{\link[stats]{pairwise.t.test}} function.\cr
- Tukey: This one uses: \code{\link[stats]{aov}},\code{\link[multcomp]{glht}} and \code{\link[stats]{confint}} functions. The  \code{\link[graphics]{par}} and \code{\link[graphics]{plot}} functions are used to draw plots\cr
- LSD: This one uses: The \code{\link[stats]{lm}} and \code{\link[stats]{anova}} functions to calculate DFerror, MSerror for the \code{\link[agricolae]{LSD.test}} function.\cr\cr
Options Window: It shows the results of means, standard deviations and quantiles. It uses the \code{\link[Rcmdr]{numSummary}} function. Results of means and standard deviations are pre-selected \cr\cr
Graphics Window: Plots means with error bars for standard error, standard deviation, confidence interval or without errors bars. It uses \code{\link[Rcmdr]{plotMeans}} function.\cr
}


\author{ Daniela Vicente <daniela.vicente@estudiants.urv.cat> }

\seealso{
	\code{\link{Commander}}, \code{\link[stats]{pairwise.t.test}}, \code{\link[stats]{aov}}, \code{\link[stats]{confint}}, \code{\link[stats]{lm}}, \code{\link[stats]{anova}}, \code{\link[multcomp]{glht}}, \code{\link[graphics]{par}}, \code{\link[graphics]{plot}}, \code{\link[Rcmdr]{numSummary}}, \code{\link[Rcmdr]{plotMeans}}.
	}

\keyword{models}
