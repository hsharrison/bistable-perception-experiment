library(magrittr)

e1 <- read.table('e1.csv', sep=',', header=TRUE)
e2 <- read.table('e2.csv', sep=',', header=TRUE)
e1$image_type <- 'ascii'
e2$image_type <- 'jpeg'
e2$participant <- e2$participant + e2$participant %>% unique %>% length
data <- rbind(e1, e2)
data$image <- data$image + 1
data$design <- as.factor(data$design)
data$response_type <- as.factor(data$response_type)
data$image_type <- as.factor(data$image_type)
data$participant <- as.factor(data$participant)
data$block <- as.factor(data$block)


library(ggplot2)
library(ggthemes)
library(extrafont)

sem <- function(x) {
  sd(x) / sqrt(length(x))
}

theme <- theme_few(base_family = 'Ubuntu', base_size = 15)
image_x <- scale_x_continuous(name='Image')
response_y <- scale_y_continuous(name = 'Response', breaks = c(-1, 0, 1), labels = c('Woman', 'Neutral', 'Man'))
response_type_linetype <- scale_linetype_discrete(name = 'Response type', breaks = c('continuous', 'discrete'))
image_type_linetype <- scale_linetype_discrete(name = 'Image type', breaks = c('ascii', 'jpeg'))
design_color <- scale_color_few(name = 'Order', breaks = c('ascending', 'descending', 'random'))

ggplot(data, aes(
  x = image,
  y = response,
  color = design,
  fill = design,
  linetype = response_type,
  group = interaction(response_type, design)
)) +
  #stat_summary(geom = 'ribbon', fun.ymax = . %>% {mean(.) + sem(.)}, fun.ymin = . %>% {mean(.) - sem(.)}, show_guide = FALSE, alpha = 0.5, fill = 'light gray') +
  geom_hline(aes(intercept = 0, linetype = 'dotted')) +
  stat_summary(geom = 'line', size = 1.5, fun.y=mean) +
  facet_wrap(~image_type) +
  image_x + response_y + response_type_linetype + design_color +
  theme

ggsave('ambiguous-figure-judgments.png', width=14, height=8)
