## Load data.
library(magrittr)
library(dplyr)

data <- read.table('e2.csv', sep=',', header=TRUE)
data$image %<>% add(1)
data$response_type %<>% plyr::revalue(., c(discrete = 'categorical'))
n_per_exp <- data$participant %>% unique %>% length %>% divide_by_int(2)
new_ps <- c(1:n_per_exp, 1:n_per_exp)
old_ps <- c(
  data %>% filter(response_type == 'continuous') %>% extract2('participant') %>% unique,
  data %>% filter(response_type == 'categorical') %>% extract2('participant') %>% unique
)
data %<>% mutate(
  experiment = plyr::revalue(response_type, c(continuous = 1, categorical = 2)),
  p_exp = plyr::mapvalues(participant, from = old_ps, to = new_ps)
)
data %<>% mutate(
  exp_f = plyr::revalue(response_type, c(
    continuous = 'E1: Continuous',
    categorical = 'E2: Categorical'
  )),
  p_exp_f = paste('P', p_exp, sep = '') %>% as.factor
)

data_w_means <- data %>% group_by(exp_f, image, design) %>% summarise(response = mean(response))
data_w_means$p_exp_f <- 'Mean'
data_w_means %<>% rbind(., data[, c('exp_f', 'image', 'response', 'p_exp_f', 'design')])

## Plot data by participant.
library(ggplot2)
library(ggthemes)
library(showtext)
library(grid)

showtext.auto()
font.add('Arial', regular = 'arial.ttf')

cairo_ps('all_participants.eps', height = 11, width = 9.5)
ggplot(data_w_means, aes(
  x = image,
  y = response,
  color = design,
)) +
  facet_grid(p_exp_f ~ exp_f) +
  geom_hline(yintercept = 0, linetype = 'dotted', size = 0.5) +
  geom_line(size = 1, position = position_jitter(width = 0, height = 0.05)) +
  scale_y_continuous(breaks = c(-1, 1), labels = c('Woman', 'Man')) +
  scale_color_few(name = 'order', palette = 'dark') +
  theme_tufte(base_size = 22, base_family = 'Arial') +
  geom_rangeframe(color = 'black') +
  theme(
    axis.title.y = element_text(angle = 0),
    strip.text.y = element_text(angle = 0),
    panel.margin = unit(2, 'lines')
  )
dev.off()


# Fitting cusp model.
library(cusp)

predict.cusp <- function(mdl) {
  n <- nrow(mdl$data)
  combined <- rbind(mdl$data, mdl$data)
  combined$source <- c(rep('observed', n), rep('model', n)) %>% as.factor

  m <- mdl$coefficients[['w[y]']]
  b <- mdl$coefficients[['w[(Intercept)]']]

  rows <- combined$source == 'model'
  combined[rows, 'y'] <- (mdl$fitted - b) / m

  return(combined)
}

p_data <- data %>% filter(p_exp_f == 'P6' | p_exp_f == 'P1', experiment == 1, design != 'random') %>% mutate(
  order = design,
  design = NULL,
  y = response,
  response = NULL,
  participant = NULL,
  response_type = NULL,
  experiment = NULL,
  p_exp = NULL,
  exp_f = NULL
)
cusp.mdl <- cusp(y ~ y, alpha ~ image, beta ~ 1, filter(p_data, p_exp_f == 'P1'))
cusp.fit <- predict.cusp(mdl)

approx.zero <- . %>% plyr::aaply(., 1, . %>% all.equal(., 0) %>% equals(TRUE))
is.real <- . %>% Im %>% approx.zero
real.roots <- function(coefs) {
  roots <- polyroot(coefs)
  return(roots %>% Re %>% extract(roots %>% is.real))
}

simulate.cusp <- function(alpha, beta) {
  result <- data.frame(alpha, beta, y = NA)
  for (row in 1:nrow(result)) {
    roots <- real.roots(c(-result[row, 'alpha'], result[row, 'beta'], 0, -1))

    if (length(roots) == 1) {
      result[row, 'y'] = roots

    } else if (row > 1) {
      last.y <- result[row - 1, 'y']
      result[row, 'y'] <- roots[roots %>% subtract(last.y) %>% abs %>% which.min]

    } else {
      result[row, 'y'] <- roots[roots %>% abs %>% which.min]
    }
  }
  return(result)
}

simulate.cusp.both.ways <- function(alpha, beta) {
  ascending <- simulate.cusp(alpha, beta)
  ascending$order <- 'ascending'

  descending <- simulate.cusp(rev(alpha), rev(beta))
  descending$order <- 'descending'

  return(rbind(ascending, descending))
}

simulate.cusp.data <- function(df, alpha, beta, y = NULL) {
  model <- simulate.cusp(
    eval(substitute(alpha), df),
    eval(substitute(beta), df)
  )
  if (!is.null(substitute(y))) {
    model$y <- eval(substitute(y), model)
  }

  result <- rbind(df, df)
  n <- nrow(df)
  result$source <- c(rep('observed', n), rep('model', n))
  result[(n+1):(2*n), 'y'] <- model$y
  return(result)
}

manual.fit <- simulate.cusp.data(
  filter(p_data, p_exp_f == 'P6'),
  alpha = (image - 6)/7,
  beta = 0.2,
  y = y + .1
)

both.fits <- rbind(cusp.fit, manual.fit)

cairo_ps('cusp_fits.eps', height = 8, width = 9.5)
ggplot(both.fits, aes(
  x = image,
  y = y,
  color = order,
  linetype = source
)) +
  facet_grid(~p_exp_f) +
  geom_hline(yintercept = 0, size = 0.5, linetype = 'dotted') +
  geom_line(size = 1, position = position_jitter(width = 0, height = 0.02)) +
  scale_color_few(palette = 'dark') +
  scale_y_continuous(breaks = c(-1, 1), labels = c('Woman', 'Man'), name = 'response') +
  scale_linetype_manual(breaks = c('observed', 'model'), values = c(2, 1)) +
  theme_tufte(base_size = 22, base_family = 'Arial') +
  geom_rangeframe(color = 'black') +
  theme(
    axis.title.y = element_text(angle = 0)
  )
dev.off()
