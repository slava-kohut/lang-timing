library(purrr)
library(ggplot2)

set.seed(123)

time_fun <- function(code, n_times){

  time_run <- c()

  for (i in 1:n_times){
   time_start <- Sys.time()
   eval(parse(text = code))
   time_end <- Sys.time()
   time_run <- c(time_run, as.numeric(time_end - time_start))
   }

   mean(time_run)
  }

code_1 <- 'sort(rnorm(n_elem), method = \'quick\')'
code_2 <- 'matrix(rnorm(n_elem*n_elem), n_elem, n_elem) %*% matrix(rnorm(n_elem*n_elem), n_elem, n_elem)'

time_res_1 <- c()
for (n_elem in c(1.e5, 1.e6, 1.e7, 1.e8, 2.e8)){
  n_elem <- n_elem
  time_res_1 <- c(time_res_1, time_fun(code_1, 5))
}

time_res_2 <- c()
for (n_elem in c(500, 1000, 2000, 3000, 4000)){
  n_elem <- n_elem
  time_res_2 <- c(time_res_2, time_fun(code_2, 5))
}

data_py <- read.csv('./python.csv')[, c(2,3)]
data_julia <- read.csv('./julia.csv')

time_sort <- data.frame(
  size = c(1.e5, 1.e6, 1.e7, 1.e8, 2.e8),
  time_r= time_res_1,
  time_py = data_py$sort,
  time_julia = data_julia$sort
)

time_mm <- data.frame(
  size = c(500, 1000, 2000, 3000, 4000),
  time_r= time_res_2,
  time_py = data_py$mm,
  time_julia = data_julia$mm
)

ggplot(data = time_sort) +
  geom_point(aes(size, time_r, color = 'R'), size = 4) +
  geom_line(aes(size, time_r), color = 'red', size = 1.4) +
  geom_point(aes(size, time_py, color = 'Python'), size = 4) +
  geom_line(aes(size, time_py), color = 'blue', size = 1.4) +
  geom_point(aes(size, time_julia, color = 'Julia'), size = 4) +
  geom_line(aes(size, time_julia), color = 'green', size = 1.4) +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = 'array size (elements)', y = 'time (s)', title = 'Array sorting (quicksort)') +
  scale_x_continuous(trans = 'log10', breaks = c(1e6, 1e7, 1e8)) +
  scale_color_manual(name = 'language', values = c('R' = 'red',
                                                   'Python' = 'blue',
                                                   'Julia' = 'green'))

ggplot(data = time_mm) +
  geom_point(aes(size, time_r, color = 'R'), size = 4) +
  geom_line(aes(size, time_r), color = 'red', size = 1.4) +
  geom_point(aes(size, time_py, color = 'Python'), size = 4) +
  geom_line(aes(size, time_py), color = 'blue', size = 1.4) +
  geom_point(aes(size, time_julia, color = 'Julia'), size = 4) +
  geom_line(aes(size, time_julia), color = 'green', size = 1.4) +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = 'matrix dimension (elements)', y = 'time (s)', title = 'Matrix multiplication') +
  scale_x_continuous(trans = 'log2', breaks = c(1000, 2000, 4000)) +
  scale_color_manual(name = 'language', values = c('R' = 'red',
                                                   'Python' = 'blue',
                                                   'Julia' = 'green'))
