#-----------------------------------------------------------------------------#
#                    7. Generation of figures and tables                      #
#-----------------------------------------------------------------------------#


# 0. Function for computing trends ----

linear_trend <- function(df, outcome, spunit){
  mod <- lm(as.formula(paste0(outcome, "~year")), data = df)
  data.frame(spunit = as.character(df[1,spunit]),
             int = coef(mod)[1],
             coef = coef(mod)[2],
             lower = confint(mod)[2,1],
             upper = confint(mod)[2,2],
             pval = summary(mod)$coefficients[2,4])
}


# 1. Trends by European regions ----

FWI_region <- read_csv("data/processed/FWI_region.csv")
pm25_region <- read_csv("data/processed/pm25_region.csv")
att_region <- read_csv("data/processed/attributable_region.csv") %>% 
  right_join(read_csv("data/processed/population_region.csv"), by = c("region"="region", "year"="year")) %>% 
  mutate(attr_stand = (attr/population*100000))
FWI_trend <- split(FWI_region, f = FWI_region$region) |>
  map_df(linear_trend, outcome = "FWI_pop", spunit = "region")
pm25_trend <- split(pm25_region, f = pm25_region$region) |>
  map_df(linear_trend, outcome = "pm25_pop", spunit = "region")
att_trend <- split(att_region, f = att_region$region) |>
  map_df(linear_trend, outcome = "attr_stand", spunit = "region")

p1 <- ggplot() +
  geom_abline(data = FWI_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = F, lwd = 0.9) +
  geom_line(data = FWI_region, aes(x=year, y=FWI_pop, col=region, group=region),
            alpha = 0.7, lwd = 1.2) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_x_continuous(breaks = seq(2003, 2023, 4), minor_breaks = seq(2003, 2023, 4)) +
  xlab("") +
  ylab("Annual average FWI") +
  labs(colour = "") +
  theme_classic() +
  theme(legend.position = "none")
 
p2 <- ggplot() +
  geom_abline(data = pm25_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = F, lwd = 0.9) +
  geom_line(data = pm25_region, aes(x=year, y=pm25_pop, col=region, group=region),
            alpha = 0.7, lwd = 1.2) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_x_continuous(breaks = seq(2003, 2023, 4), minor_breaks = seq(2003, 2023, 1)) +
  theme_bw() +
  xlab("") +
  ylab(expression(Annual~average~`wildfire-PM`[2.5]~(mu*g/m^3))) +
  labs(colour = "") +
  theme_classic() +
  theme()

p3 <- ggplot() + 
  geom_abline(data = att_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = F, lwd = 0.9) +
  geom_line(data = att_region, aes(x = year, y = attr_stand, col = region, group = region),
            alpha = 0.7, lwd = 1.2) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_x_continuous(breaks = seq(2003, 2023, 4), minor_breaks = seq(2003, 2023, 1)) +
  xlab("") +
  ylab(expression(Annual~attributable~deaths~to~`wildfire-PM`[2.5]~"/100K")) +
  labs(colour = "") + 
  theme_classic() +
  theme(legend.position = "none")
 
p4 <- as_ggplot(get_legend(p2))
p2 <- p2 + theme(legend.position = "none")

pall <- ggpubr::ggarrange(ggpubr::ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)) +
  ggpubr::bgcolor("white") +
  ggpubr::border("white")

ggsave("figures/regiontrend_FWI_pm2.5_attrdeaths.png", pall, width = 8, height = 8, dpi = 600)

# Export trends
trendtab <- rbind(mutate(FWI_trend, metric = "FWI"),
                  mutate(pm25_trend, metric = "PM2.5"),
                  mutate(att_trend, metric = "attributable deaths/100K")) |>
  rename(region = spunit, pvalue = pval) |>
  mutate(trend = paste0(round(coef, 3), " (", round(lower, 3), ", ", round(upper,3), ")"),
         pvalue = round(pvalue, 4)) |>
  select(metric, region, trend, pvalue) |>
  arrange(desc(metric), region)

write_csv(trendtab, "figures/figures_regiontrend.csv")
rm("FWI_region", "FWI_trend", "att_region", "att_trend",
   "p1", "p2", "pall", "trendtab")




# Exposure percentiles: # days above certain thresholds

# data import
pm25_perc <- read_csv("data/processed/pm25_city.csv")

# calculate the population weight by city population or region population, respectvely
num_days_percentile <- pm25_perc %>%
  group_by(region, year) %>%
  mutate(across(starts_with("perc"), ~(. * popw_URAU_region), .names = "{.col}_pop")) %>% 
  summarise(across(starts_with("perc"), ~ sum(., na.rm = T)),
            across(ends_with("_pop"), ~ sum(., na.rm = T)),
            .groups = "drop")

# population weighted exposure
# trends of peak days by region and year above percentiles
days50_region_trend <- split(num_days_percentile, f = num_days_percentile$region) |>
  map_df(linear_trend, outcome = c("perc50_pop"), spunit = "region")
days75_region_trend <- split(num_days_percentile, f = num_days_percentile$region) |>
  map_df(linear_trend, outcome = c("perc75_pop"), spunit = "region")
days90_region_trend <- split(num_days_percentile, f = num_days_percentile$region) |>
  map_df(linear_trend, outcome = c("perc90_pop"), spunit = "region")
days95_region_trend <- split(num_days_percentile, f = num_days_percentile$region) |>
  map_df(linear_trend, outcome = c("perc95_pop"), spunit = "region")
days99_region_trend <- split(num_days_percentile, f = num_days_percentile$region) |>
  map_df(linear_trend, outcome = c("perc99_pop"), spunit = "region")
days99.9_region_trend <- split(num_days_percentile, f = num_days_percentile$region) |>
  map_df(linear_trend, outcome = c("perc99.9_pop"), spunit = "region")


p50 <- ggplot() +
  geom_abline(data = days50_region_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = F, lwd = 0.9) +
  geom_line(data = num_days_percentile, aes(x=year, y=perc50_pop, col=region, group=region),
            alpha = 0.7, lwd = 1.2) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_x_continuous(breaks = seq(2003, 2023, 2), minor_breaks = seq(2003, 2023, 1)) +
  theme_bw() +
  xlab("") +
  ylab("Peak PM2.5 above 50th percentile") +
  labs(colour = "") +
  theme_classic() +
  theme()

p75 <- ggplot() +
  geom_abline(data = days75_region_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = F, lwd = 0.9) +
  geom_line(data = num_days_percentile, aes(x=year, y=perc75_pop, col=region, group=region),
            alpha = 0.7, lwd = 1.2) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_x_continuous(breaks = seq(2003, 2023, 2), minor_breaks = seq(2003, 2023, 1)) +
  theme_bw() +
  xlab("") +
  ylab("Peak PM2.5 above 75th percentile") +
  labs(colour = "") +
  theme_classic() +
  theme(legend.position = "none")

p90 <- ggplot() +
  geom_abline(data = days90_region_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = F, lwd = 0.9) +
  geom_line(data = num_days_percentile, aes(x=year, y=perc90_pop, col=region, group=region),
            alpha = 0.7, lwd = 1.2) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_x_continuous(breaks = seq(2003, 2023, 2), minor_breaks = seq(2003, 2023, 1)) +
  theme_bw() +
  xlab("") +
  ylab("Peak PM2.5 above 90th percentile") +
  labs(colour = "") +
  theme_classic() +
  theme(legend.position = "none")

p95 <- ggplot() +
  geom_abline(data = days95_region_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = F, lwd = 0.9) +
  geom_line(data = num_days_percentile, aes(x=year, y=perc95_pop, col=region, group=region),
            alpha = 0.7, lwd = 1.2) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_x_continuous(breaks = seq(2003, 2023, 2), minor_breaks = seq(2003, 2023, 1)) +
  theme_bw() +
  xlab("") +
  ylab("Peak PM2.5 above 95th percentile") +
  labs(colour = "") +
  theme_classic() +
  theme(legend.position = "none")

p99 <- ggplot() +
  geom_abline(data = days99_region_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = F, lwd = 0.9) +
  geom_line(data = num_days_percentile, aes(x=year, y=perc99_pop, col=region, group=region),
            alpha = 0.7, lwd = 1.2) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_x_continuous(breaks = seq(2003, 2023, 2), minor_breaks = seq(2003, 2023, 1)) +
  theme_bw() +
  xlab("") +
  ylab("Peak PM2.5 above 99th percentile") +
  labs(colour = "") +
  theme_classic() +
  theme(legend.position = "none")

p99.9 <- ggplot() +
  geom_abline(data = days99.9_region_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = F, lwd = 0.9) +
  geom_line(data = num_days_percentile, aes(x=year, y=perc99.9_pop, col=region, group=region),
            alpha = 0.7, lwd = 1.2) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_x_continuous(breaks = seq(2003, 2023, 2), minor_breaks = seq(2003, 2023, 1)) +
  theme_bw() +
  xlab("") +
  ylab("Peak PM2.5 above 99.9th percentile") +
  labs(colour = "") +
  theme_classic() +
  theme(legend.position = "none")

plegend <- as_ggplot(get_legend(p50))
p50 <- p50 + theme(legend.position = "none")

pall <- ggpubr::ggarrange(ggpubr::ggarrange(p50, p75, p90, p95, p99, p99.9, plegend, nrow = 4, ncol = 2)) +
  ggpubr::bgcolor("white") +
  ggpubr::border("white") 

ggsave("figures/regiontrend_days_above_percentile_pm25_pop_weighted_country_adjusted.png", pall, width = 8, height = 10, dpi = 600)

# single plots
p99 <- ggplot() +
  geom_abline(data = days99_region_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = F, lwd = 0.9) +
  geom_line(data = num_days_percentile, aes(x=year, y=perc99_pop, col=region, group=region),
            alpha = 0.7, lwd = 1.2) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_x_continuous(breaks = seq(2003, 2023, 2), minor_breaks = seq(2003, 2023, 1)) +
  theme_bw() +
  xlab("") +
  ylab("Peak PM2.5 above 99th percentile") +
  labs(colour = "") +
  theme_classic() 

ggsave("figures/regiontrend_days_above_99percentile_pm25_pop_weighted_country_adjusted.png", p99, width = 6, height = 3, dpi = 600)


p99.9 <- ggplot() +
  geom_abline(data = days99.9_region_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = F, lwd = 0.9) +
  geom_line(data = num_days_percentile, aes(x=year, y=perc99.9_pop, col=region, group=region),
            alpha = 0.7, lwd = 1.2) +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_x_continuous(breaks = seq(2003, 2023, 2), minor_breaks = seq(2003, 2023, 1)) +
  theme_bw() +
  xlab("") +
  ylab("Peak PM2.5 above 99.9th percentile") +
  labs(colour = "") +
  theme_classic() 

ggsave("figures/regiontrend_days_above_99.9percentile_pm25_pop_weighted_country_adjusted.png", p99.9, width = 6, height = 3, dpi = 600)


# Export trends
trendtab <- rbind(mutate(days50_region_trend, metric = "perc50_pop"),
                  mutate(days75_region_trend, metric = "perc75_pop"),
                  mutate(days90_region_trend, metric = "perc90_pop"),
                  mutate(days95_region_trend, metric = "perc95_pop"),
                  mutate(days99_region_trend, metric = "perc99_pop"),
                  mutate(days99.9_region_trend, metric = "perc99.9_pop")) |>
  rename(region = spunit, pvalue = pval) |>
  mutate(trend = paste0(round(coef, 3), " (", round(lower, 3), ", ", round(upper,3), ")"),
         pvalue = round(pvalue, 4)) |>
  select(metric, region, trend, pvalue) |>
  arrange(metric, region)

write_csv(trendtab, "figures/figures_regiontrend_days_percentile_pm25_pop_weighted_country_adjusted.csv")
rm("p50", "p75", "p90", "p95", "p99", "p99.9", "plegend", "pall", "trendtab")



# 2. Trends by country ----

# Trends of FWI, average PM2.5 and attr deaths 
# Compute trends
FWI_country <- read_csv("data/processed/FWI_country.csv")
pm25_country <- read_csv("data/processed/pm25_country.csv")
attr_country <- read_csv("data/processed/attributable_country.csv")%>% 
  right_join(read_csv("data/processed/population_country.csv"), by = c("CNTR_CODE"="CNTR_CODE", "year"="year")) %>% 
  mutate(attr_stand = (attr/population*100000))
FWI_trend <- split(FWI_country, f = FWI_country$CNTR_CODE) |>
  map_df(linear_trend, outcome = "FWI_pop", spunit = "CNTR_CODE")
pm25_trend <- split(pm25_country, f = pm25_country$CNTR_CODE) |>
  map_df(linear_trend, outcome = "pm25_pop", spunit = "CNTR_CODE")
attr_trend <- attr_country %>%
  filter(!is.na(attr_stand)) %>% # Remove rows with NA in 'attr_stand'
  split(.$CNTR_CODE) %>% 
  map_df(linear_trend, outcome = "attr_stand", spunit = "CNTR_CODE")

# Add complete names
euroregions <- import("data/raw/cities/List_cities.xlsx") %>%
  dplyr::select(1,2,6)
euroregions <- euroregions[!duplicated(euroregions),]
names(euroregions) <- c("country_name", "CNTR_CODE", "region")

FWI_trend <- left_join(FWI_trend, euroregions, by = c("spunit" = "CNTR_CODE"))
pm25_trend <- left_join(pm25_trend, euroregions, by = c("spunit" = "CNTR_CODE"))
attr_trend <- left_join(attr_trend, euroregions, by = c("spunit" = "CNTR_CODE"))

# Prepare for plotting
FWI_trend$Pvalue <- case_when(
  FWI_trend$pval > 0.2 ~ "> 0.2",
  FWI_trend$pval > 0.05 ~ "0.05 to 0.2",
  FWI_trend$pval <= 0.05 ~ "< 0.05")
FWI_trend$Pvalue <- fct_relevel(FWI_trend$Pvalue,
                                   c("> 0.2","0.05 to 0.2","< 0.05"))

pm25_trend$Pvalue <- case_when(
  pm25_trend$pval > 0.2 ~ "> 0.2",
  pm25_trend$pval > 0.05 ~ "0.05 to 0.2",
  pm25_trend$pval <= 0.05 ~ "< 0.05")
pm25_trend$Pvalue <- fct_relevel(pm25_trend$Pvalue,
                                    c("> 0.2","0.05 to 0.2","< 0.05"))

attr_trend$Pvalue <- case_when(
  attr_trend$pval > 0.2 ~ "> 0.2",
  attr_trend$pval > 0.05 ~ "0.05 to 0.2",
  attr_trend$pval <= 0.05 ~ "< 0.05")
attr_trend$Pvalue <- fct_relevel(attr_trend$Pvalue,
                                    c("> 0.2","0.05 to 0.2","< 0.05"))

# Plotting
p1 <- ggpubr::ggdotchart(FWI_trend, x = "country_name", y = "coef",
                         color = "region",
                         dot.size = "Pvalue",
                         sorting = "descending",
                         add = "segments",
                         add.params = list(color = "lightgray", size = 2),
                         font.label = list(color = "white", size = 9,
                                           vjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey20") +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_size_manual(values = c(1,3,5)) +
  labs(color = "Region") +
  xlab("") +
  ylab("Trend in Fire Weather Index") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box="vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 10),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.margin=margin(-10,0.1,0.1,0.1),
        legend.box.margin=margin(-10,0.1,0.1,0.1))

p2 <- ggpubr::ggdotchart(pm25_trend, x = "country_name", y = "coef",
                         color = "region",
                         dot.size = "Pvalue",
                         sorting = "descending",
                         add = "segments",
                         add.params = list(color = "lightgray", size = 2),
                         font.label = list(color = "white", size = 9,
                                           vjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey20") +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_size_manual(values = c(1,3,5)) +
  labs(color = "Region") +
  xlab("") +
  ylab(expression(Trend~"in"~`wildfire-PM`[2.5])) +
  theme_bw() +
  theme(legend.position = "bottom", legend.box="vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 10),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.margin=margin(-10,0.1,0.1,0.1),
        legend.box.margin=margin(-10,0.1,0.1,0.1))

p3 <- ggpubr::ggdotchart(attr_trend, x = "country_name", y = "coef",
                         color = "region",
                         dot.size = "Pvalue",
                         sorting = "descending",
                         add = "segments",
                         add.params = list(color = "lightgray", size = 2),
                         font.label = list(color = "white", size = 9,
                                           vjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey20") +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_size_manual(values = c(1,3,5)) +
  labs(color = "Region") +
  xlab("") +
  ylab(expression(Trend~"in"~attributable~deaths~to~`wildfire-PM`[2.5]~"/100K")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.box="vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 10),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.margin=margin(-10,0.1,0.1,0.1),
        legend.box.margin=margin(-10,0.1,0.1,0.1))

pall <- ggpubr::ggarrange(p1, p2, p3,
                          nrow = 3, common.legend = T, legend = "bottom")

ggsave("figures/countrytrend_popw.png", pall,  width = 8, height = 11.5)
rm("FWI_country", "FWI_trendpop", "attr_country", "attr_trendpop", "p1", "p2", "p3", "pall",
   "pm25_country", "pm25_trendpop", "euroregions")


# peak days above percentiles PM2.5 by country

# population-weighted PM2.5
pm25_perc <- read_csv("data/processed/pm25_city.csv")

# calculate the population weight by city population or region population, respectively
pm25_country <- pm25_perc %>%
  group_by(CNTR_CODE, year) %>%
  mutate(across(starts_with("perc"), ~(. * popw_URAU_CNTR), .names = "{.col}_pop")) %>% 
  summarise(across(starts_with("perc"), ~ sum(., na.rm = T)),
            across(ends_with("_pop"), ~ sum(., na.rm = T)),
            .groups = "drop")

# Compute trends
days50_country_trend <- split(pm25_country, f = pm25_country$CNTR_CODE) |>
  map_df(linear_trend, outcome = "perc50_pop", spunit = "CNTR_CODE")
days75_country_trend <- split(pm25_country, f = pm25_country$CNTR_CODE) |>
  map_df(linear_trend, outcome = "perc75_pop", spunit = "CNTR_CODE")
days90_country_trend <- split(pm25_country, f = pm25_country$CNTR_CODE) |>
  map_df(linear_trend, outcome = "perc90_pop", spunit = "CNTR_CODE")
days95_country_trend <- split(pm25_country, f = pm25_country$CNTR_CODE) |>
  map_df(linear_trend, outcome = "perc95_pop", spunit = "CNTR_CODE")
days99_country_trend <- split(pm25_country, f = pm25_country$CNTR_CODE) |>
  map_df(linear_trend, outcome = "perc99_pop", spunit = "CNTR_CODE")
days99.9_country_trend <- split(pm25_country, f = pm25_country$CNTR_CODE) |>
  map_df(linear_trend, outcome = "perc99.9_pop", spunit = "CNTR_CODE")

# Add complete names
euroregions <- import("data/raw/cities/List_cities.xlsx") %>%
  dplyr::select(1,2,3,4,6,7)
euroregions <- euroregions[!duplicated(euroregions),]
names(euroregions) <- c("country_name", "CNTR_CODE", "URAU_CODE", "URAU_NAME", "region", "citysize")
# merge XXL and Global city city size categories from UN geoscheme into one category (≥XXL)
euroregions <- euroregions %>%
  mutate(citysize = ifelse(citysize %in% c("XXL", "Global city"), "≥XXL", citysize))

days50_country_trend <- left_join(days50_country_trend, euroregions, by = c("spunit" = "CNTR_CODE"))
days75_country_trend <- left_join(days75_country_trend, euroregions, by = c("spunit" = "CNTR_CODE"))
days90_country_trend <- left_join(days90_country_trend, euroregions, by = c("spunit" = "CNTR_CODE"))
days95_country_trend <- left_join(days95_country_trend, euroregions, by = c("spunit" = "CNTR_CODE"))
days99_country_trend <- left_join(days99_country_trend, euroregions, by = c("spunit" = "CNTR_CODE"))
days99.9_country_trend <- left_join(days99.9_country_trend, euroregions, by = c("spunit" = "CNTR_CODE"))

# Prepare for plotting
days50_country_trend$Pvalue <- case_when(
  days50_country_trend$pval > 0.2 ~ "> 0.2",
  days50_country_trend$pval > 0.05 ~ "0.05 to 0.2",
  days50_country_trend$pval <= 0.05 ~ "< 0.05")
days50_country_trend$Pvalue <- fct_relevel(days50_country_trend$Pvalue,
                                    c("> 0.2","0.05 to 0.2","< 0.05"))

days75_country_trend$Pvalue <- case_when(
  days75_country_trend$pval > 0.2 ~ "> 0.2",
  days75_country_trend$pval > 0.05 ~ "0.05 to 0.2",
  days75_country_trend$pval <= 0.05 ~ "< 0.05")
days75_country_trend$Pvalue <- fct_relevel(days75_country_trend$Pvalue,
                                           c("> 0.2","0.05 to 0.2","< 0.05"))

days90_country_trend$Pvalue <- case_when(
  days90_country_trend$pval > 0.2 ~ "> 0.2",
  days90_country_trend$pval > 0.05 ~ "0.05 to 0.2",
  days90_country_trend$pval <= 0.05 ~ "< 0.05")
days90_country_trend$Pvalue <- fct_relevel(days90_country_trend$Pvalue,
                                           c("> 0.2","0.05 to 0.2","< 0.05"))

days95_country_trend$Pvalue <- case_when(
  days95_country_trend$pval > 0.2 ~ "> 0.2",
  days95_country_trend$pval > 0.05 ~ "0.05 to 0.2",
  days95_country_trend$pval <= 0.05 ~ "< 0.05")
days95_country_trend$Pvalue <- fct_relevel(days95_country_trend$Pvalue,
                                           c("> 0.2","0.05 to 0.2","< 0.05"))

days99_country_trend$Pvalue <- case_when(
  days99_country_trend$pval > 0.2 ~ "> 0.2",
  days99_country_trend$pval > 0.05 ~ "0.05 to 0.2",
  days99_country_trend$pval <= 0.05 ~ "< 0.05")
days99_country_trend$Pvalue <- fct_relevel(days99_country_trend$Pvalue,
                                           c("> 0.2","0.05 to 0.2","< 0.05"))

days99.9_country_trend$Pvalue <- case_when(
  days99.9_country_trend$pval > 0.2 ~ "> 0.2",
  days99.9_country_trend$pval > 0.05 ~ "0.05 to 0.2",
  days99.9_country_trend$pval <= 0.05 ~ "< 0.05")
days99.9_country_trend$Pvalue <- fct_relevel(days99.9_country_trend$Pvalue,
                                           c("> 0.2","0.05 to 0.2","< 0.05"))

# Plotting
p1 <- ggpubr::ggdotchart(days50_country_trend, x = "country_name", y = "coef",
                         color = "region",
                         dot.size = "Pvalue",
                         sorting = "descending",
                         add = "segments",
                         add.params = list(color = "lightgray", size = 2),
                         font.label = list(color = "white", size = 9,
                                           vjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey20") +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_size_manual(values = c(1,3,5)) +
  labs(color = "Region") +
  xlab("") +
  ylab("Trend peak PM2.5 above 50th percentile") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box="vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 10),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.margin=margin(-10,0.1,0.1,0.1),
        legend.box.margin=margin(-10,0.1,0.1,0.1))

p2 <- ggpubr::ggdotchart(days75_country_trend, x = "country_name", y = "coef",
                            color = "region",
                            dot.size = "Pvalue",
                            sorting = "descending",
                            add = "segments",
                            add.params = list(color = "lightgray", size = 2),
                            font.label = list(color = "white", size = 9,
                                              vjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey20") +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_size_manual(values = c(1,3,5)) +
  labs(color = "Region") +
  xlab("") +
  ylab("Trend peak PM2.5 above 75th percentile") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box="vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 10),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.margin=margin(-10,0.1,0.1,0.1),
        legend.box.margin=margin(-10,0.1,0.1,0.1))


p3 <- ggpubr::ggdotchart(days90_country_trend, x = "country_name", y = "coef",
                            color = "region",
                            dot.size = "Pvalue",
                            sorting = "descending",
                            add = "segments",
                            add.params = list(color = "lightgray", size = 2),
                            font.label = list(color = "white", size = 9,
                                              vjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey20") +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_size_manual(values = c(1,3,5)) +
  labs(color = "Region") +
  xlab("") +
  ylab("Trend peak PM2.5 above 90th percentile") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box="vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 10),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.margin=margin(-10,0.1,0.1,0.1),
        legend.box.margin=margin(-10,0.1,0.1,0.1))

p4 <- ggpubr::ggdotchart(days95_country_trend, x = "country_name", y = "coef",
                         color = "region",
                         dot.size = "Pvalue",
                         sorting = "descending",
                         add = "segments",
                         add.params = list(color = "lightgray", size = 2),
                         font.label = list(color = "white", size = 9,
                                           vjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey20") +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_size_manual(values = c(1,3,5)) +
  labs(color = "Region") +
  xlab("") +
  ylab("Trend peak PM2.5 above 95th percentile") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box="vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 10),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.margin=margin(-10,0.1,0.1,0.1),
        legend.box.margin=margin(-10,0.1,0.1,0.1))

p5 <- ggpubr::ggdotchart(days99_country_trend, x = "country_name", y = "coef",
                         color = "region",
                         dot.size = "Pvalue",
                         sorting = "descending",
                         add = "segments",
                         add.params = list(color = "lightgray", size = 2),
                         font.label = list(color = "white", size = 9,
                                           vjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey20") +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_size_manual(values = c(1,3,5)) +
  labs(color = "Region") +
  xlab("") +
  ylab("Trend peak PM2.5 above 99th percentile") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box="vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 10),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.margin=margin(-10,0.1,0.1,0.1),
        legend.box.margin=margin(-10,0.1,0.1,0.1))

p6 <- ggpubr::ggdotchart(days99.9_country_trend, x = "country_name", y = "coef",
                         color = "region",
                         dot.size = "Pvalue",
                         sorting = "descending",
                         add = "segments",
                         add.params = list(color = "lightgray", size = 2),
                         font.label = list(color = "white", size = 9,
                                           vjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey20") +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_size_manual(values = c(1,3,5)) +
  labs(color = "Region") +
  xlab("") +
  ylab("Trend peak PM2.5 above 99.9th percentile") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box="vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 10),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.margin=margin(-10,0.1,0.1,0.1),
        legend.box.margin=margin(-10,0.1,0.1,0.1))

pall <- ggpubr::ggarrange(p1, p2, p3, p4, p5, p6,
                  nrow = 6, common.legend = T, legend = "bottom")

ggsave("figures/countrytrend_days_percentiles_popw_countryw.png", pall,  width = 8, height = 14, , dpi = 600)



# single plots
p5 <- ggpubr::ggdotchart(days99_country_trend, x = "country_name", y = "coef",
                         color = "region",
                         dot.size = "Pvalue",
                         sorting = "descending",
                         add = "segments",
                         add.params = list(color = "lightgray", size = 2),
                         font.label = list(color = "white", size = 9,
                                           vjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey20") +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_size_manual(values = c(1,3,5)) +
  labs(color = "Region") +
  xlab("") +
  ylab("Trend peak PM2.5 above 99th percentile") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box="vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 10),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.margin=margin(-10,0.1,0.1,0.1),
        legend.box.margin=margin(-10,0.1,0.1,0.1))

ggsave("figures/countrytrend_days_99percentiles_popw_countryw.png", p5,  width = 8, height = 5, , dpi = 600)

p6 <- ggpubr::ggdotchart(days99.9_country_trend, x = "country_name", y = "coef",
                         color = "region",
                         dot.size = "Pvalue",
                         sorting = "descending",
                         add = "segments",
                         add.params = list(color = "lightgray", size = 2),
                         font.label = list(color = "white", size = 9,
                                           vjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey20") +
  scale_colour_manual(values = c("#EE3377", "#33BBEE", "#EE7733", "#CC3311", "#009988")) +
  scale_size_manual(values = c(1,3,5)) +
  labs(color = "Region") +
  xlab("") +
  ylab("Trend peak PM2.5 above 99.9th percentile") +
  theme_bw() +
  theme(legend.position = "bottom", legend.box="vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 10),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.margin=margin(-10,0.1,0.1,0.1),
        legend.box.margin=margin(-10,0.1,0.1,0.1))

ggsave("figures/countrytrend_days_99.9percentiles_popw_countryw.png", p6,  width = 8, height = 5, , dpi = 600)


rm("p1", "p2", "p3", "p4", "p5", "p6", "pall",
   "pm25_country", "euroregions", "days50_country_trend", "days75_country_trend", 
   "days90_country_trend", "days95_country_trend", "days99_country_trend", "days99.9_country_trend")



# 3. Trends by city size ----

# Trends of FWI, average PM2.5 and attr deaths by city size
# Compute trends
FWI_citysize <- read_csv("data/processed/FWI_citysize.csv") %>% 
  mutate(citysize = case_when(
    citysize=="S"~"1. S",
    citysize=="M"~"2. M",
    citysize=="L"~"3. L",
    citysize=="XL"~"4. XL",
    citysize=="≥XXL"~"5. ≥XXL"))
pm25_citysize <- read_csv("data/processed/pm25_citysize.csv")%>% 
  mutate(citysize = case_when(
    citysize=="S"~"1. S",
    citysize=="M"~"2. M",
    citysize=="L"~"3. L",
    citysize=="XL"~"4. XL",
    citysize=="≥XXL"~"5. ≥XXL"))
attr_citysize <- read_csv("data/processed/attributable_citysize.csv") %>% 
  right_join(read_csv("data/processed/population_citysize.csv"), by = c("citysize"="citysize", "year"="year")) %>% 
  mutate(attr_stand = (attr/population*100000)) %>% 
  mutate(citysize = case_when(
    citysize=="S"~"1. S",
    citysize=="M"~"2. M",
    citysize=="L"~"3. L",
    citysize=="XL"~"4. XL",
    citysize=="≥XXL"~"5. ≥XXL"))
FWI_trend <- split(FWI_citysize, f = FWI_citysize$citysize) |>
  map_df(linear_trend, outcome = "FWI_pop", spunit = "citysize")
pm25_trend <- split(pm25_citysize, f = pm25_citysize$citysize) |>
  map_df(linear_trend, outcome = "pm25_pop", spunit = "citysize")
attr_trend <- split(attr_citysize, f = attr_citysize$citysize) %>% 
  map_df(linear_trend, outcome = "attr_stand", spunit = "citysize")

# Prepare for plotting
FWI_trend$Pvalue <- case_when(
  FWI_trend$pval > 0.2 ~ "> 0.2",
  FWI_trend$pval > 0.05 ~ "0.05 to 0.2",
  FWI_trend$pval <= 0.05 ~ "< 0.05")
FWI_trend$Pvalue <- fct_relevel(FWI_trend$Pvalue,
                                c("> 0.2","0.05 to 0.2","< 0.05"))

pm25_trend$Pvalue <- case_when(
  pm25_trend$pval > 0.2 ~ "> 0.2",
  pm25_trend$pval > 0.05 ~ "0.05 to 0.2",
  pm25_trend$pval <= 0.05 ~ "< 0.05")
pm25_trend$Pvalue <- fct_relevel(pm25_trend$Pvalue,
                                 c("> 0.2","0.05 to 0.2","< 0.05"))

attr_trend$Pvalue <- case_when(
  attr_trend$pval > 0.2 ~ "> 0.2",
  attr_trend$pval > 0.05 ~ "0.05 to 0.2",
  attr_trend$pval <= 0.05 ~ "< 0.05")
attr_trend$Pvalue <- fct_relevel(attr_trend$Pvalue,
                                 c("> 0.2","0.05 to 0.2","< 0.05"))

# Plotting
p1 <- ggplot() +
  geom_abline(data = FWI_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = F, lwd = 0.9) +
  geom_line(data = FWI_citysize, aes(x=year, y=FWI_pop, col=citysize, group=citysize),
            alpha = 0.7, lwd = 1.2) +
  scale_colour_manual(values = c("#983de6", "#d73dd1", "#df4464", "#ee7848", "#eebf0d")) +
  scale_x_continuous(breaks = seq(2003, 2023, 4), minor_breaks = seq(2003, 2023, 4)) +
  xlab("") +
  ylab("Annual average FWI") +
  labs(colour = "") +
  theme_classic() +
  theme(legend.position = "none")

p2 <- ggplot() +
  geom_abline(data = pm25_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = F, lwd = 0.9) +
  geom_line(data = pm25_citysize, aes(x=year, y=pm25_pop, col=citysize, group=citysize),
            alpha = 0.7, lwd = 1.2) +
  scale_colour_manual(values = c("#983de6", "#d73dd1", "#df4464", "#ee7848", "#eebf0d")) +
  scale_x_continuous(breaks = seq(2003, 2023, 4), minor_breaks = seq(2003, 2023, 1)) +
  theme_bw() +
  xlab("") +
  ylab(expression(Annual~average~`wildfire-PM`[2.5]~(mu*g/m^3))) +
  labs(colour = "City size") +
  theme_classic() +
  theme()

p3 <- ggplot() + 
  geom_abline(data = attr_trend, aes(intercept = int, slope = coef, col=spunit),
              lty = 2, show.legend = F, lwd = 0.9) +
  geom_line(data = attr_citysize, aes(x = year, y = attr_stand, col = citysize, group = citysize),
            alpha = 0.7, lwd = 1.2) +
  scale_colour_manual(values = c("#983de6", "#d73dd1", "#df4464", "#ee7848", "#eebf0d")) +
  scale_x_continuous(breaks = seq(2003, 2023, 4), minor_breaks = seq(2003, 2023, 1)) +
  xlab("") +
  ylab(expression(Annual~attributable~deaths~to~`wildfire-PM`[2.5]~"/100K")) +
  labs(colour = "") + 
  theme_classic() +
  theme(legend.position = "none")

p4 <- as_ggplot(get_legend(p2))
p2 <- p2 + theme(legend.position = "none")

pall <- ggpubr::ggarrange(ggpubr::ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)) +
  ggpubr::bgcolor("white") +
  ggpubr::border("white")

ggsave("figures/citysizetrend_FWI_pm2.5_attrdeaths.png", pall, width = 8, height = 8, dpi = 600)


# Export trends
trendtab <- rbind(mutate(FWI_trend, metric = "FWI"),
                  mutate(pm25_trend, metric = "PM2.5"),
                  mutate(attr_trend, metric = "attributable deaths/100K")) |>
  rename(region = spunit, pvalue = pval) |>
  mutate(trend = paste0(round(coef, 3), " (", round(lower, 3), ", ", round(upper,3), ")"),
         pvalue = round(pvalue, 4)) |>
  select(metric, region, trend, pvalue) |>
  arrange(desc(metric), region)

write_csv(trendtab, "figures/figures_citysizetrend.csv")
rm("FWI_region", "FWI_trend", "att_region", "attr_trend",
   "p1", "p2", "pall", "trendtab")




# 4. Trends by cities ----
# Top 20 death counts 2003-2023 by city 
att_city <- read_csv("data/processed/attributable_city.csv")
att_city2 <- att_city %>% 
  right_join(read_csv("data/processed/population_city.csv"), by = c("URAU_CODE", "year")) %>% 
  mutate(attr_stand = (attr/population*100000),
         attrlower_stand = (attrlower/population*100000),
         attrupper_stand = (attrupper/population*100000)) %>% 
  group_by(URAU_CODE) %>% 
  mutate(mean_att = mean(attr),
         mean_attrlower = mean(attrlower), 
         mean_attrupper = mean(attrupper)) %>% 
  ungroup() %>% 
  mutate(attr_main = paste0(round(attr,2), " (", round(attrlower,2),
                            ", ", round(attrupper,2), ")"),
         attr_main_stand = paste0(round(attr_stand,2), " (", round(attrlower_stand,2),
                                  ", ", round(attrupper_stand,2), ")"),
         mean_attr_main = paste0(round(mean_att,2), " (", round(mean_attrlower,2),
                                  ", ", round(mean_attrupper,2), ")")) 

# calculate trends of standardized attributable mortality by city
att_city3 <- att_city2 %>% 
  left_join(split(att_city2, f = att_city$URAU_CODE) %>% 
              map_df(linear_trend, outcome = "attr_stand", spunit = "URAU_CODE") %>% 
              select(spunit, coef, pval) %>% 
              rename(URAU_CODE = spunit,
                     trend_city = coef,
                     Pvalue = pval), by = c("URAU_CODE"))

# merge the country-level attributable mortality based on nuts2 and calculate contribution
att_country_copy <- read_csv("data/processed/attributable_country_full_report_copy.csv") %>% 
  rename(att_copy = attr, 
         attrlower_copy = attrlower, 
         attrupper_copy = attrupper) %>%
  group_by(NUTS_0) %>% #average across period
  mutate(mean_attr_copy = mean(att_copy),
         mean_attrlower_copy = mean(attrlower_copy), 
         mean_attrupper_copy = mean(attrupper_copy)) %>% 
  ungroup() %>% 
  mutate(attr_main_copy = paste0(round(att_copy,2), " (", round(attrlower_copy,2),
                               ", ", round(attrupper_copy,2), ")"),
         mean_attr_main_copy = paste0(round(mean_attr_copy,2), " (", round(mean_attrlower_copy,2),
                               ", ", round(mean_attrupper_copy,2), ")")) %>% 
  select(year, NUTS_0, att_copy, attrlower_copy, attrupper_copy, attr_main_copy, mean_attr_copy, mean_attr_main_copy)
att_city4 <-  att_city3 %>% 
  left_join(att_country_copy, 
            by = c("year" = "year", "CNTR_CODE" = "NUTS_0"))
att_city5 <- att_city4 %>% 
  group_by(CNTR_CODE, year) %>% 
  mutate(mean_attr_perc_city = ((100/mean_attr_copy)*mean_att)) %>% 
  ungroup()
# add city and country names
euroregions <- import("data/raw/cities/List_cities.xlsx") %>%
  dplyr::select(1,2,3,4,6,7) %>% distinct()
names(euroregions) <- c("CNTR_name", "CNTR_CODE", "URAU_CODE", "URAU_NAME", "region", "citysize")
att_city6 <- att_city5 %>% 
  left_join(euroregions, by = c("URAU_CODE"))

# sort by highest attributable death counts per 100000 inhabitants
attr <- att_city6 %>% 
  arrange(desc(attr_stand)) %>% 
  slice(1:20) %>% 
  select(URAU_NAME, CNTR_CODE.x, citysize.x, year, attr_main, attr_main_stand, trend_city, Pvalue)
names(attr) <- c("City", "Country", "Size", "Year", "Absolute attributable deaths (95% CI)", "Attributable deaths/100K (95% CI)",
                  "City trend", "Pvalue")

write_csv(attr, "figures/tab_attrper100k_cities_20.csv")

# sort by average city absolute deaths -> city-level death contribution to country's total 
attr1 <- att_city6 %>% 
  group_by(URAU_CODE) %>% 
  slice(1) %>%
  ungroup() %>% 
  arrange(desc(mean_att)) %>% 
  select(URAU_NAME, CNTR_CODE.x, citysize.x, year, mean_attr_main, mean_attr_main_copy, mean_attr_perc_city, trend_city, Pvalue)
names(attr1) <- c("City", "Country", "Size", "Year", "Avg city-level absolute attr deaths (95% CI)", 
                  "Avg country-level absolute attr deaths (95% CI)", "Avg city contribution to national attr deaths (%)", 
                  "City trend", "Pvalue")

write_csv(attr1, "figures/tab_avg_citycontribution_tonationaldeaths_sortedbyavgcitydeaths_all.csv")

# sort by strongest good trend (decrease in deaths) of attributable death counts per 100000 inhabitants
attr2 <- att_city6 %>% 
group_by(URAU_CODE) %>% 
  summarise(sum_attrper100k = sum(attr_stand),
            sum_attr = sum(attr),
            CNTR_CODE = first(CNTR_CODE.x),
            citysize = first(citysize.x),
            trend_city = first(trend_city),
            Pvalue = first(Pvalue),
            URAU_NAME = first(URAU_NAME)) %>% 
  ungroup() %>% 
  arrange(trend_city) %>% 
  slice(1:20) %>% 
  select(URAU_NAME, CNTR_CODE, citysize, trend_city, Pvalue, sum_attr, sum_attrper100k)
names(attr2) <- c("City", "Country", "Size", "City trend", "Pvalue", 
                  "Sum of absolute attributable deaths", "Sum of attributable deaths/100K")

write_csv(attr2, "figures/tab_city_goodtrends_attrper100k_20.csv")

# sort by strongest bad trend (increase in deaths) of attributable death counts (absolute and per 100000 inhabitants)
attr3 <- att_city6 %>% 
  group_by(URAU_CODE) %>% 
  summarise(sum_attrper100k = sum(attr_stand),
            sum_attr = sum(attr),
            CNTR_CODE = first(CNTR_CODE.x),
            citysize = first(citysize.x),
            trend_city = first(trend_city),
            Pvalue = first(Pvalue),
            URAU_NAME = first(URAU_NAME)) %>% 
  ungroup() %>% 
  arrange(desc(trend_city)) %>%
  slice(1:20) %>%
  select(URAU_NAME, CNTR_CODE, citysize, trend_city, Pvalue, sum_attr, sum_attrper100k)
names(attr3) <- c("City", "Country", "Size", "City trend", "Pvalue", 
                  "Sum of absolute attributable deaths", "Sum of attributable deaths/100K")

write_csv(attr3, "figures/tab_city_badtrends_attrper100k_20.csv")

# all trend of attributable death counts (absolute and per 100000 inhabitants)
attr4 <- att_city6 %>% 
  group_by(URAU_CODE) %>% 
  summarise(sum_attrper100k = sum(attr_stand),
            sum_attr = sum(attr),
            CNTR_CODE = first(CNTR_CODE.x),
            citysize = first(citysize.x),
            trend_city = first(trend_city),
            Pvalue = first(Pvalue),
            URAU_NAME = first(URAU_NAME)) %>% 
  ungroup() %>% 
  arrange(desc(trend_city)) %>%
  select(URAU_NAME, CNTR_CODE, citysize, trend_city, Pvalue, sum_attr, sum_attrper100k)
names(attr4) <- c("City", "Country", "Size", "City trend", "Pvalue", 
                  "Sum of absolute attributable deaths", "Sum of attributable deaths/100K")

write_csv(attr4, "figures/tab_city_trends_attrper100k_all.csv")

# significant city trends 
attr5 <- att_city6 %>% 
  group_by(URAU_CODE) %>% 
  summarise(sum_attrper100k = sum(attr_stand),
            sum_attr = sum(attr),
            CNTR_CODE = first(CNTR_CODE.x),
            citysize = first(citysize.x),
            region = first(region.x),
            trend_city = first(trend_city),
            Pvalue = first(Pvalue),
            URAU_NAME = first(URAU_NAME)) %>% 
  ungroup() %>% 
  arrange(desc(trend_city)) %>%
  select(URAU_NAME, CNTR_CODE, citysize, region, trend_city, Pvalue, sum_attr, sum_attrper100k)
attr6 <- attr5 %>% 
  filter(Pvalue<0.05)

########### Where are the cities with trends coming from?
attr7 <- attr6 %>% 
  filter(trend_city>0) 
attr7 %>% mutate(region = region %>% as.factor()) %>% summary()

attr8 <- attr6 %>% 
  filter(trend_city<0) 
attr8 %>% mutate(region = region %>% as.factor()) %>% summary()
############

names(attr6) <- c("City", "Country", "Size", "Region", "City trend", "Pvalue", 
                  "Sum of absolute attributable deaths", "Sum of attributable deaths/100K")

write_csv(attr6, "figures/tab_sign_city_trends_attrper100k.csv")
  

# sensitivity analysis
att_euro <- read_csv("data/processed/attributable_euro.csv")
names(att_euro) <- c("year", "N_countries_main", "attr", "lower", "upper")
att_euro_season <- read_csv("data/processed/attributable_euro_sens_seasonmort.csv") 
names(att_euro_season) <- c("year", "N_countries_sens", "attr_season", "lower_season", "upper_season")
att_euro_noseason <- read_csv("data/processed/attributable_euro_sens_noseasonmort.csv")
names(att_euro_season) <- c("year", "N_countries_sens2", "attr_noseason", "lower_noseason", "upper_moseason")

sens_att <- att_euro %>% 
  bind_cols(att_euro_noseason %>% select(2,3,4,5), 
            att_euro_season %>% select(3,4,5)) 
sens_att <- sens_att %>% 
  mutate(attr_main = paste0(round(.[[3]], 2), " (", round(.[[4]], 2), ", ", round(.[[5]], 2), ")"),
         attr_season = paste0(round(.[[7]], 2), " (", round(.[[8]], 2), ", ", round(.[[9]], 2), ")"),
         attr_noseason = paste0(round(.[[10]], 2), " (", round(.[[11]], 2), ", ", round(.[[12]], 2), ")")) %>% 
  select(1,2,13,6,14,15) 

write_csv(sens_att, "figures/tab_sens_attr_euro.csv")


# clean
rm(list = ls())
