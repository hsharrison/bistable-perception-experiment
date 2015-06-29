## Load data.

library(magrittr)

e1 <- read.table('e1.csv', sep=',', header=TRUE)
e2 <- read.table('e2.csv', sep=',', header=TRUE)
e1$image_type <- 'ascii'
e2$image_type <- 'jpeg'
e2$participant <- e2$participant + e1$participant %>% unique %>% length
data <- rbind(e1, e2)
data$image <- data$image + 1
data$design <- as.factor(data$design)
data$response_type <- as.factor(data$response_type)
data$image_type <- as.factor(data$image_type)
data$participant <- as.factor(data$participant)
data$block <- as.factor(data$block)


## Plot data.

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

ggplot(data, aes(
  x = image,
  y = response,
  color = design,
  fill = design,
)) +
  geom_hline(aes(intercept = 0, linetype = 'dotted')) +
  stat_summary(geom = 'line', size = 1.5, fun.y=mean) +
  facet_wrap(~ participant + image_type + response_type) +
  image_x + response_y + design_color +
  theme

ggsave('all-participants.png', width=20, height=16)


# Fitting the cusp model
library(cusp)

fit.all <- cusp(y ~ response, alpha ~ image + image_type, beta ~ image + image_type, subset(data, data$response_type == 'continuous'))
summary(fit.all, logist = TRUE)
plot(fit.all)
png('fit-all.png', height = 800, width = 800)
cusp3d(fit.all, theta = 45)
dev.off()

fit.jpeg <- cusp(y ~ response, alpha ~ image, beta ~ image, subset(data, data$response_type == 'continuous' & data$image_type == 'jpeg'))
summary(fit.jpeg, logist = TRUE)
plot(fit.jpeg)
png('fit-jpeg.png', height = 800, width = 800)
cusp3d(fit.jpeg, theta = 45)
dev.off()

fit.1 <- cusp(y ~ response, alpha ~ image, beta ~ image, subset(data, data$participant == 18))
summary(fit.1, logist = TRUE)
plot(fit.1)
png('fit-p18.png', height = 800, width = 800)
cusp3d(fit.1, theta = 45)
dev.off()

n <- nrow(fit.1$data)
combined <- rbind(fit.1$data, fit.1$data)
combined$source <- as.factor(c(rep('observed', n), rep('model', n)))


m <- fit.1$coefficients[['w[response]']]
b <- fit.1$coefficients[['w[(Intercept)]']]

rows <- combined$source == 'model'
combined[rows, 'response'] <- (fit.1$fitted - b) / m

source_linetype <- scale_linetype_discrete(name = 'Data', breaks = c('observed', 'model'))

ggplot(combined, aes(x = image, y = response, color = design, linetype = source)) +
  geom_line(size = 1.5) +
  image_x + response_y + design_color + source_linetype +
  theme +
  coord_cartesian(ylim = c(-1, 1), xlim = c(1, 15))
ggsave('p18-model.png', width=14, height=8)
