library(quickpsy)
library(stringr)
library(readr)
library(plyr)

## FUNCTIONS ###
get_weber <- function(qp, intensity) {
  t25 <- ddply(qp$par, qp$groups, one_threshold, 0.25, qp$log, qp$groups,
               qp$funname, qp$guess, qp$lapses, qp$curves)
  t75 <- ddply(qp$par, qp$groups, one_threshold, 0.75, qp$log, qp$groups,
               qp$funname, qp$guess, qp$lapses, qp$curves)
  m <- merge(t25, t75, by = qp$groups)
  m <- merge(qp$thresholds, m, by = qp$groups)
  m$intensity <- intensity
  w <- ddply(m, qp$groups, transform, weber = ((thre.y - thre.x)/intensity)/2)
  w <- w[,c(qp$groups,'weber')] #subset(w, select=-c(thre.x,prob.x,prob.y))
  names(w) <- c(qp$groups,'par') #[names(w) == 'thre.y'] <- 'thre'
  w$parn <- 'p3'
  wpar <- merge(qp$par, w, all=TRUE)
  list(wpar=wpar)
}


## READ IN THE DATA ##

data.folder <- 'Experiments/'
data.files <- dir(data.folder, 'data')
pain.data <- c()
for (thisFile in data.files) {
  fname.info <- str_split(thisFile, '_')[[1]]
  rawdata <- read_csv(paste0(data.folder,thisFile))
  exptNo <- fname.info[4]
  rawdata$experiment <- exptNo
  rawdata$participant <- fname.info[5]
  rawdata$sex <- fname.info[6]
  rawdata$age <- fname.info[7]
  rawdata$site <- fname.info[8]
  if (exptNo == 2) {
    rawdata$unit.threshold <- fname.info[9]
    rawdata$unit.type <- fname.info[10]
  } else {
    rawdata$unit.threshold <- NA
    rawdata$unit.type <- NA
  }
  pain.data <- rbind(pain.data,rawdata)
}

## FITTING PSYCHOMETRIC FUNCTIONS ##
pain.qp.1 <- quickpsy(d = subset(pain.data, experiment == 1), 
                      x = comparison,
                      k = comparison.more.painful,
                      # minimum needed to get all separate thresholds
                      # is .(participant,site)
                      grouping = .(participant,site),
                      log = TRUE)

pain.qp.2 <- quickpsy(d = subset(pain.data, experiment == 2), 
                      x = comparison,
                      k = comparison.more.painful,
                      # minimum needed to get all separate thresholds
                      # is .(participant,unit.type)
                      grouping = .(participant,unit.type,unit.threshold),
                      log = TRUE)

## CALCULATE THE WEBER FRACTIONS ##
pain.qp.1.weber <- get_weber(pain.qp.1, 60)
pain.qp.2.weber <- get_weber(pain.qp.2, 60)


## PLOT ##
theme_set(theme_classic(base_size = 14))

# psychometric functions

quartz()

t25.data.1 <- ddply(pain.qp.1$par, pain.qp.1$groups, one_threshold, 0.25, pain.qp.1$log, pain.qp.1$groups,
                    pain.qp.1$funname, pain.qp.1$guess, pain.qp.1$lapses, pain.qp.1$curves)
t75.data.1 <- ddply(pain.qp.1$par, pain.qp.1$groups, one_threshold, 0.75, pain.qp.1$log, pain.qp.1$groups,
                    pain.qp.1$funname, pain.qp.1$guess, pain.qp.1$lapses, pain.qp.1$curves)

ggplot(data=pain.qp.1$curves, aes(x=x, y=y)) +
  facet_wrap( ~ paste(participant,site), scales='free') +
  geom_segment(data = t25.data.1, aes(y = 0.25, yend = -Inf, x = thre, xend = thre), lty =2, size = 0.2) + #vertical 25%
  geom_segment(data = t75.data.1, aes(y = 0.75, yend = -Inf, x = thre, xend = thre), lty =2, size = 0.2) + #vertical 75%
  geom_segment(data = t25.data.1, aes(y = 0.25, yend = 0.25, x = 0, xend = thre), lty = 2, size = 0.2) + #horizontal 25%
  geom_segment(data = t75.data.1, aes(y = 0.75, yend = 0.75, x = 0, xend = thre), lty = 2, size = 0.2) + #horizontal 75%
  geom_line() +
  geom_point(data = pain.qp.1$averages, aes(x = comparison, y=prob)) +
  scale_x_log10() +
  labs(title = 'Experiment 1', 
       x ='Comparison intensity', 
       y ='Proportion comparison called more painful')

ggsave('Plots/Expt1_PsychFuns.pdf')

quartz()
t25.data.2 <- ddply(pain.qp.2$par, pain.qp.2$groups, one_threshold, 0.25, pain.qp.2$log, pain.qp.2$groups,
                    pain.qp.2$funname, pain.qp.2$guess, pain.qp.2$lapses, pain.qp.2$curves)
t75.data.2 <- ddply(pain.qp.2$par, pain.qp.2$groups, one_threshold, 0.75, pain.qp.2$log, pain.qp.2$groups,
                    pain.qp.2$funname, pain.qp.2$guess, pain.qp.2$lapses, pain.qp.2$curves)

ggplot(data=pain.qp.2$curves, aes(x=x, y=y)) +
  facet_wrap( ~ paste(participant,unit.type), scales='free') +
  geom_segment(data = t25.data.2, aes(y = 0.25, yend = -Inf, x = thre, xend = thre), lty =2, size = 0.2) + #vertical 25%
  geom_segment(data = t75.data.2, aes(y = 0.75, yend = -Inf, x = thre, xend = thre), lty =2, size = 0.2) + #vertical 75%
  geom_segment(data = t25.data.2, aes(y = 0.25, yend = 0.25, x = 0, xend = thre), lty = 2, size = 0.2) + #horizontal 25%
  geom_segment(data = t75.data.2, aes(y = 0.75, yend = 0.75, x = 0, xend = thre), lty = 2, size = 0.2) + #horizontal 75%
  geom_line() +
  geom_point(data = pain.qp.2$averages, aes(x = comparison, y=prob)) +
  scale_x_log10() +
  labs(title = 'Experiment 2', 
       x ='Comparison intensity', 
       y ='Proportion comparison called more painful')

ggsave('Plots/Expt2_PsychFuns.pdf')

# weber fractions
quartz()
ggplot() +
  geom_point(data=subset(pain.qp.1.weber$wpar, parn=='p3'), 
             aes(x=participant, y=par, colour=site, shape=site), size=2.5) +
  scale_y_continuous(limits = c(0,2)) +
  scale_color_brewer(palette = 'Set1') +
  labs(title = 'Experiment 1', x='\nParticipant', y='Weber fraction\n') 
ggsave('Plots/Experiment1_Weber_Individual.pdf')

quartz()
ggplot() +
  geom_point(data=subset(pain.qp.2.weber$wpar, parn=='p3'), 
             aes(x=unit.type, y=par, 
                 colour=unit.threshold), 
             size=2.5) +
  scale_y_continuous(limits = c(0.1,0.85), labels = scales::percent) +
  scale_color_brewer(palette = 'Set1') +
  labs(title = 'Experiment 2', x='\nUnit Type', y='Weber Fraction (%) \n') 
ggsave('Plots/Experiment2_Weber_Individual.pdf')

# save summary data files
expt1.summary <- subset(pain.qp.1.weber$wpar, parn == 'p3', select = -c(3,5,6))
names(expt1.summary)[3] <- 'weber'
write_csv(expt1.summary, 'expt1.summary.csv')

expt2.summary <- subset(pain.qp.2.weber$wpar, parn == 'p3', select = -c(4,6,7))
names(expt2.summary)[4] <- 'weber'
write_csv(expt2.summary, 'expt2.summary.csv')
