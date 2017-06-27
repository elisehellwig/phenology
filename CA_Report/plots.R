drivepath <- '/Users/echellwig/Drive/Phenology'
library(ggplot2)


w <- read.csv(file.path(drivepath, 'data/walnutdata/walnutclean.csv'))
p <- read.csv(file.path(drivepath, 'data/clean/processed/pruneclean.csv'))

vars <- c('Chandler','Franquette','Payne')
w <- w[w$cultivar %in% vars, ]

w$cultivar <- droplevels(w$cultivar)

walnutcol <- 'green4' 
prunecol <- 'purple4'
almondcol <- 'darkgoldenrod3'


##############################################
#prune Maturation Time
#geom ribbon


pmplot <- ggplot(data=p) #+ geom_ribbon(aes(x=year, ymin=147, ymax=163),
                                      # fill='grey70')
pmplot <- pmplot + geom_point(aes(x=year, y=length1), 
                                      size=0.5, 
                                      color=prunecol) 
pmplot <- pmplot + geom_hline(yintercept=155, color='grey50', linetype='dashed')
pmplot <- pmplot + geom_smooth(aes(x=year, y=length1), 
                               size = 0.4,
                               method = "lm", 
                               se=FALSE, 
                               color="black", 
                               formula = y ~ x)
pmplot <- pmplot + labs(x='Year', y='Maturation Time (Days)')

pmplot <- pmplot + theme_bw(7) #+ theme(panel.border = element_blank(),
#                                        panel.grid.major = element_blank(),
#                                        panel.grid.minor = element_blank(),
#                                        axis.line = element_line(colour = "black"))


tiff(file.path(drivepath, 'FollowUp/plots/PruneMatureTime.tiff'), width=640,
    res=300)
pmplot
dev.off()


##############################################
#walnut Maturation Time
MTmeans <- data.frame(cultivar=c('Chandler','Franquette','Payne'),
                      average=c(183.6, 181, 184.4))


wmplot <- ggplot(data=w) + geom_point(aes(x=year, y=length1), 
                                        size=0.5, 
                                        color=walnutcol) 
wmplot <- wmplot + facet_wrap(~cultivar)
wmplot <- wmplot + theme_bw(6) + labs(x='Year', y='Maturation Time (Days)')
wmplot <- wmplot + geom_hline(aes(yintercept=average), 
                              data=MTmeans,
                              color='grey50', 
                              linetype='dashed')
wmplot <- wmplot + geom_smooth(aes(x=year, y=length1), 
                               size = 0.4,
                               method = "lm", 
                               se=FALSE, 
                               color="black", 
                               formula = y ~ x)

tiff(file.path(drivepath, 'FollowUp/plots/WalnutMatureTime.tiff'), height = 400,
    width=810,
    res=300)
wmplot
dev.off()







