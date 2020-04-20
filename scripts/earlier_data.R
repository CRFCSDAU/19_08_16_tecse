
  library(readxl)
  library(tidyverse)
  library(viridis)
  library(forecast)

  data <- read_excel("data/StudydataOSF.xlsx")

  names(data) <- make.names(tolower(names(data)))

  names(data)[names(data) == "totaltecs_erc.score.animation.task"] <- "total_tecse"
  names(data)[names(data) == "totaltrcscore.mutiple.choice"] <- "total_trc"


  totaltrcscore.mutiple.choice
  n_ <- length(data$total_tecse[!is.na(data$total_tecse)])

  ggplot(data, aes(x = total_tecse)) +
    geom_histogram(bins = 40) +
    ggtitle("n = ", n_)

  select(data, total_tecse, total_trc) %>%
    mutate(log_total_tecse = log(total_tecse)) %>%
    mutate(sqrt_total_tecse = sqrt(total_tecse)) %>%
    mutate(bxcx_total_tecse = BoxCox(data$total_tecse, lambda = "auto")) %>%
    gather(version, value, contains("tecse")) %>%
    ggplot(aes(x = value, y = total_trc, color = version)) +
    geom_point(alpha = 0.5) +
    geom_smooth(se = FALSE, method = "lm") +
    scale_color_viridis(discrete = TRUE, end = 0.8) +
    facet_wrap(~version, scales = "free") +
    theme_minimal()


  data <- mutate(data, log_total_tecse = log(total_tecse)) %>%
    mutate(sqrt_total_tecse = sqrt(total_tecse)) %>%
    mutate(bxcx_total_tecse = BoxCox(data$total_tecse, lambda = "auto"))

  plot(lm(bxcx_total_tecse ~ total_trc, data = data))
  plot(lm(     total_tecse ~ total_trc, data = data))

  with(data, cor(bxcx_total_tecse, total_trc, method = "pearson"))
  with(data, cor(     total_tecse, total_trc, method = "pearson"))

