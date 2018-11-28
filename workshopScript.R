
#title: "GGplot in R"
#author: "Cindy Xiong & Caitlyn McColeman"
#date: "11/28/2018"


require(ggplot2)

# ===== Load Data ===== #

# move to this file's directory, and use it as a reference point to find the data folder
this.dir <- dirname(parent.frame(2)$ofile)   # this finds where the current folder is saved
setwd(this.dir)                              # this sets the working directory, where R will first look to find things such as files and scripts

# load in the .csv file and save its contents to a variable entitled "results". 
results <- read.csv("ExampleDataShare.csv", header=T) # header = T means that the first line of the .csv will be read as titles for the columns we're loading in

# ===== Response Error Overview ===== #

# start with a histogram of errors, the column called 'estimation_error'
overallHist<-ggplot(data=results, aes(x=estimation_error)) + geom_histogram()

# how can we further break this down? 
# Are bar's error different from line's error? 
# Check by splitting the histogram by having one histogram for where participants judge "bar" and one where they judge "line"
byTaskHist<-ggplot(data=results, aes(x=estimation_error, col=judge)) + geom_histogram()

# Let's seperate them in space so they're clearer to see
byTaskHist1x2<-ggplot(data=results, aes(x=estimation_error, col=judge)) + geom_histogram() + facet_wrap(~judge)

# at this point, it seems like the pattern of error is different for line and bars. Generally it looks like people overestimate bars and underestimate lines.
# we can break it down further. 

# ===== Response Error Given Different Values ===== #

# does the error change based on the value of the actual position of mean bar/line?
byActualValueBar<-ggplot(data=results, aes(x=correct_location_bar, y=estimation_error, col=judge)) + geom_line() + facet_wrap(~judge)

byActualValueLine<-ggplot(data=results, aes(x=correct_location_line, y=estimation_error, col=judge)) + geom_line() + facet_wrap(~judge)

# it looks like there may be a change as a function of the actual value of the lines and bars, and that the task may matter too. 
# I want to see them all together to better understand.
# We'll need to do a little processing so that we can have all of the error in column and a marker 
require('reshape2')

# keep everything in its own row (via id.vars) except for the correct_location_line and correct_location_bar columns which will be combined
verticallyCombinedActualValue <- melt(results, id.vars = c('id', 'display_figure', 'overlap', 'estimated_location', 'estimation_error', 'item', 'rt', 'judge', 'drew_from'))

# now we can see both the task (color) and the correct location of both the bar and the line
byTaskAndRelevantPosition <- ggplot(data=verticallyCombinedActualValue, aes(x=value, y = estimation_error, col=judge)) + geom_line() + facet_wrap(~variable)

# clean up graph; exclude the bar-only and line-only presentations
byTaskAndRelevantPosition <- ggplot(data=verticallyCombinedActualValue[verticallyCombinedActualValue$value>5,], aes(x=value, y = estimation_error, col=judge)) + geom_line() + facet_wrap(~variable)

# some basic formatting things to help with readability
byTaskAndRelevantPosition <- byTaskAndRelevantPosition + theme_bw() + labs(x = 'actual height', y = 'error')

# I still hate those facet labels. Let's change the data so that the labels are more human readable
levels(verticallyCombinedActualValue$variable) <- c("Line", "Bar")

# notice that the plot hasn't changed. We need to run everything again. At this point in development, I'd move the line above to the start of the chunk of code that makes this plot. 
# once we have meaningful facet titles, we can nix the legend. Legends are something to avoid if possible. 
byTaskAndRelevantPosition <- byTaskAndRelevantPosition + theme(legend.position="none")

# given (Bartram & Stone, 2010) I know grid lines aren't great, so I can either eliminate or reduce them. I'll go with reduce to the major grid lines.
byTaskAndRelevantPosition <- byTaskAndRelevantPosition + theme(panel.grid.major = element_blank())

# if we know our data well enough, at this point we can smooth it out using various interpolation methods to make it a little more pleasing to look at
byTaskAndRelevantPosition <- byTaskAndRelevantPosition + geom_smooth()

# actually, I want the fit to be more salient than the line plot, so I'm going to play with how things look. This is also an example of how to streamline code, rather than having everything in seperate lines.
errorSmooth <- ggplot(data=verticallyCombinedActualValue[verticallyCombinedActualValue$value>5,], aes(x=value, y = estimation_error, col=judge)) +
  geom_line(alpha = .25) +
  facet_wrap(~variable) + 
  theme_bw() + 
  theme(legend.position="none") + 
  labs(x = 'actual height', y = 'error') + 
  geom_smooth() 

# I also know that reading vertical labels is annoying, so I'll make things easier for my audience
errorSmooth <- errorSmooth + theme(axis.title.y = element_text(angle=0))

# ===== Summarize error patterns by graph types ===== #

verticallyCombinedActualValue$display_figure <- factor(verticallyCombinedActualValue$display_figure, 
                                       levels=c("BH", "BM", "BL", "RH", "RM", "RL", "RHBM", "RLBM", "RMBM", "RMBL","RMBH"))

overviewForCindyResults <- ggplot(verticallyCombinedActualValue, aes(x=display_figure, y=estimation_error, col=judge)) +
  stat_summary(fun.y="mean", geom="bar")