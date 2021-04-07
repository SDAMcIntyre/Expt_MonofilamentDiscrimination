library(quickpsy)
library(readr)
library(purrr)
library(readxl)
library(Hmisc)
library(patchwork)
library(svglite)

## READ IN THE DATA ##

touch.data.nav17 <- read_excel('Nav 1.7 Data.xlsx', range = 'A1:F71') %>% 
  mutate(experiment = 'touch',
         group = 'Nav1.7',
         ID = '0')  

touch.folder <- 'data_controls/Touch - random spots/'
touch.data.control <- touch.folder %>% 
  list.files('foot_data', recursive = TRUE, full.names = TRUE) %>% 
  map_dfr(read_csv, col_types = cols(), .id = "ID") %>% 
  mutate(experiment = 'touch',
         group = 'control')

touch.data <- bind_rows(touch.data.nav17, touch.data.control) %>% 
  mutate(comparison.mN = 10 * comparison) # mutate(comparison.mN = 9.80665 * comparison)


## FITTING PSYCHOMETRIC FUNCTIONS ##
touch.qp <- quickpsy(d = touch.data, 
                     x = comparison.mN,
                     k = comparison.more.painful,
                     grouping = .(ID,group),
                     log = TRUE,
                     fun = logistic_fun,
                     B = 1000)

## PLOT ##
theme_set(theme_classic(base_size = 14))

# psychometric functions

textsignif <- function(x, digits) {
  formatC(signif(x,digits), digits,format="fg")
}

textround <- function(x, digits) {
  formatC(round(x,digits), digits,format="f")
}


plot_psyfun <- function(qp) {
  
  # my.colour.scale <- c('#1b9e77', '#d95f02')
  
  my.colour.scale <- c(
    rgb(202, 202, 202, maxColorValue=255), 
    rgb(69, 139, 250, maxColorValue=255)
  )
  
  ggplot() +
    stat_summary(data = filter(qp$curves, group == 'control'),
                 aes(x=x, y=y), fill = my.colour.scale[1], alpha = 0.4,
                 geom = 'ribbon', fun.data = 'mean_cl_boot') +
    geom_point(data = qp$averages,
               aes(x = comparison.mN, y=prob, colour = group, fill = group),
               position = position_jitter(width = 0.03, height = 0),
               shape = 21, alpha = 0.4, size = 3) +
    stat_summary(data = qp$curves,
                 aes(x=x, y=y, colour = group),
                 geom = 'line', fun = 'mean') +
    scale_colour_manual(values = my.colour.scale) +
    scale_fill_manual(values = my.colour.scale)
}

touch.breaks <- unique(touch.data$comparison.mN)
touch.labels <- textsignif(touch.breaks, digits = 2)

quartz(width = 5.9, height = 5.1); plot(1:10)
plot_psyfun(touch.qp) +
  scale_x_log10(breaks = touch.breaks, labels = touch.labels) +
  labs(title = 'Force discrimination', 
       x ='Comparison force (mN)', 
       y ='Proportion called greater than 20 mN') 

ggsave('Nav17-vs-control_low-force-discrimination.svg')

# get slope CI for whole control group
touch.qp.grouped <- quickpsy(d = touch.data, 
                             x = comparison.mN,
                             k = comparison.more.painful,
                             grouping = .(group),
                             log = TRUE,
                             fun = logistic_fun,
                             B = 1000)

touch.qp.grouped$par %>% 
  group_by(group, parn) %>% 
  summarise(across(
    .cols = where(is.numeric), .fns = exp 
  ))
