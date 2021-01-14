library(tidyverse)
library(readxl)
library(ggplot2)
library(binom)
detaineedeaths_analysis <- read_excel("detaineedeaths_analysis.xlsx")

get_mmr <- function(x,y) x / y * 10^5
get_se_mmr <- function(x,y) sqrt(get_mmr(x,y)^2/x)
get_se <- function(x,y) sqrt(get_se_mmr(x,y)^2 / get_mmr(x,y)^2)

detaineedeaths_analysis <- detaineedeaths_analysis %>%
  mutate_at(.vars = vars(`Total Deaths`, `COVID deaths`, `Non-COVID Deaths`, `Suicide`),
            .funs = list(lower = ~ qchisq(0.025, 2 * .) / 2 / .data$`Average Daily Population`*10^5,
                         upper = ~ qchisq(0.975, 2 * (. + 1)) / 2 / .data$`Average Daily Population`*10^5,
                         mmr = ~ get_mmr(.,.data$`Average Daily Population`),
                         se_mmr = ~ get_se_mmr(.,.data$`Average Daily Population`),
                         log_mmr = ~ log(get_mmr(.,.data$`Average Daily Population`)),
                         log_se = ~ get_se(.,.data$`Average Daily Population`)))

dodge_val <- 0.15
gsize = 6
lsize = 1.5
deaths_past_decade <- detaineedeaths_analysis %>%
  filter(between(`Fiscal Year`, 2010, 2019)) %>%
  select(`Total Deaths_mmr`, `Suicide_mmr`) %>%
  summarise_all(.funs = list(mean = ~ mean(.),
                             sd = ~ sd(.),
                             n = ~ length(.))) %>%
  mutate(`Total Deaths_int` = 1.96*`Total Deaths_mmr_sd`/sqrt(`Total Deaths_mmr_n`),
         `Suicide_int` = 1.96*`Suicide_mmr_sd`/sqrt(`Suicide_mmr_n`))

# Figure 1
g_suicides <- ggplot(detaineedeaths_analysis %>% filter(`Fiscal Year` > 2009)) + 
  geom_hline(yintercept=deaths_past_decade$`Suicide_mmr_mean`, linetype = "dashed", size = lsize, alpha = 0.8) + 
  geom_line(size = lsize, aes(y = `Suicide_mmr`, x = `Fiscal Year`))+ 
  geom_point(size = gsize, shape = 19, stroke = lsize, aes(y = `Suicide_mmr`, x = `Fiscal Year`)) + 
  geom_errorbar(width = .25, size = lsize, aes(ymin = `Suicide_lower`, ymax = `Suicide_upper`, x = `Fiscal Year`)) +
  scale_y_continuous(limits = c(0,40)) + 
  scale_x_continuous(breaks = seq(2010, 2020, 2)) + 
  labs(y = "Suicides (per 100,000 person-years)", x = "Fiscal year") +
  theme_minimal(base_size = 24, base_family = "Helvetica-Narrow") +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank())

ggsave(g_suicides, filename = "suicides.pdf",
       width = 12, height = 8)

# Appendix Figure
# Total Deaths. Non-Covid deaths in with solid point estimates. Total deaths for 2020 FFY with an empty point estimate
g_deaths <- ggplot(detaineedeaths_analysis %>% filter(`Fiscal Year` > 2009)) + 
  geom_hline(yintercept=deaths_past_decade$`Total Deaths_mmr_mean`, linetype = "dashed", size = lsize, alpha = 0.8) + 
  geom_line(size = lsize, aes(y = `Non-COVID Deaths_mmr`, x = `Fiscal Year`))+ 
  geom_point(size = gsize, shape = 19, stroke = lsize, aes(y = `Non-COVID Deaths_mmr`, x = `Fiscal Year`)) + 
  geom_errorbar(width = .25, size = lsize, aes(ymin = `Non-COVID Deaths_lower`, ymax = `Non-COVID Deaths_upper`, x = `Fiscal Year`)) +
  geom_errorbar(data = detaineedeaths_analysis %>% filter(`Fiscal Year` == 2020),
                width = .25, size = lsize, aes(ymin = `Total Deaths_lower`, ymax = `Total Deaths_upper`, x = `Fiscal Year` + 2 * dodge_val)) +
  geom_point(data = detaineedeaths_analysis %>% filter(`Fiscal Year` == 2020),
             size = gsize, stroke = lsize, shape = 21, aes(y = `Total Deaths_mmr`, x = `Fiscal Year` + 2* dodge_val)) + 
  scale_y_continuous(limits = c(0,100)) + 
  scale_x_continuous(breaks = seq(2010, 2020, 2)) + 
  labs(y = "Deaths (per 100,000 person-years)", x = "Fiscal year") +
  theme_minimal(base_size = 24, base_family = "Helvetica-Narrow") +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank())

ggsave(g_deaths, filename = "deaths.pdf",
       width = 10, height = 8)
