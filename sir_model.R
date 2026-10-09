# SIR epidemic model: how R0 shapes the epidemic curve
# install.packages(c("deSolve", "ggplot2", "dplyr", "scales"))
library(deSolve); library(ggplot2); library(dplyr)

dir.create("01-sir-epidemic-model/figures", showWarnings = FALSE, recursive = TRUE)

sir <- function(t, state, parms) {
  with(as.list(c(state, parms)), {
    dS <- -beta * S * I / N
    dI <-  beta * S * I / N - gamma * I
    dR <-  gamma * I
    list(c(dS, dI, dR))
  })
}

N <- 1e6; gamma <- 1 / 7   # 7-day infectious period

run <- function(R0) {
  out <- ode(y = c(S = N - 10, I = 10, R = 0), times = 0:300, func = sir,
             parms = c(beta = R0 * gamma, gamma = gamma, N = N))
  as.data.frame(out) |> mutate(scenario = paste("R0 =", R0))
}
res <- bind_rows(lapply(c(1.5, 2.5, 4), run))

summary_tbl <- res |> group_by(scenario) |>
  summarise(peak_infected = round(max(I)), peak_day = time[which.max(I)],
            final_size_pct = round(100 * max(R) / N, 1))
print(summary_tbl)

p <- ggplot(res, aes(time, I, colour = scenario)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "SIR model: infections over time by R0",
       x = "Day", y = "Currently infected", colour = NULL) +
  theme_minimal(base_size = 13)
ggsave("01-sir-epidemic-model/figures/sir_curves.png", p, width = 8, height = 5, dpi = 300)
