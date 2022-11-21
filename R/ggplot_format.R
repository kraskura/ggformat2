


#' ""
#'
#' @param plot saved ggplot, variable name, must provide
#' @param title the title of the plot; default= no title
#' @param y_title the title of y axis, must provide
#' @param x_title the title of x axis, must provide
#' @param print logical, default = TRUE
#' @param size_text font size for text, default = 15
#'
#' @return formatted ggplot. Formatting themes include: axis, text size, label sizes, no gridlines, panel border around the plot
#' @export
#' @import ggplot2
#' @examples
#' plotToformat<-ggplot()+geom_point(mapping = aes(x = seq(0,10,0.1), y = sin(seq(0,10,0.1))))
#' ggformat(plotToformat, y = "Sine Wave", x = "Time", size_text = 15)
ggformat<-function(plot, y_title, x_title, title="", print=TRUE, size_text = 15){

	plot_name<-deparse(substitute(plot))

	plot<- plot +
	theme_classic()+
	ggtitle(title)+
	ylab(y_title)+
	xlab(x_title)+
	theme(axis.text.y=element_text(size=size_text, colour= 'black'),
		axis.text.x=element_text(size=size_text, colour= 'black'),
		axis.line.y=element_line(colour = 'black',size=0.5),
		axis.line.x=element_line(colour = 'black',size=0.5),
		axis.ticks.y=element_line(size=0.5),
		# axis.ticks.x=element_line(size=0.5),
		axis.ticks.x.bottom = element_line(size=0.5, colour = "black"),
		axis.title.y=element_text(size=size_text),
		axis.title.x=element_text(size=size_text),
		panel.border = element_rect(linetype = "solid",fill=NA, colour = "black"))

	if (print==TRUE){
		print(plot)
	}

	return(assign(plot_name, plot, envir=parent.frame()))

}


