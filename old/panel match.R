############ panel match----------
library(PanelMatch)
# duplicate_check <- table(g$ADM1_EN, g$ADM2_EN, g$time_id)
# duplicates <- which(duplicate_check > 1, arr.ind = TRUE)
# g_deduplicated <- g[!duplicated(g[, c("ADM2_EN", "time_id")]), ]
# g_deduplicated <- g_deduplicated %>% 
#   mutate(adm3_id_new = as.integer(adm3_id)) %>% 
#   as.data.frame()
# 

g <- g %>%
  group_by(ADM1_EN, ADM2_EN) %>%
  mutate(ADM2_id = as.integer(cur_group_id())) %>%
  ungroup() %>% 
  as.data.frame()


dem.panel <- PanelData(panel.data = g_adm1,
                       unit.id = "ADM1_id",
                       time.id = "time_id",
                       treatment = "is_ban",
                       outcome = "n_fires_modis"
)


PM.maha <- PanelMatch(panel.data = dem.panel,
                      lag = 30,
                      refinement.method = "mahalanobis",
                      match.missing = FALSE,
                      covs.formula = ~ I(lag(is_ban, 0:30)) +
                        I(lag(n_fires_modis, 1:30)),
                      size.match = 5,
                      qoi = "att",
                      lead = 0:30,
                      use.diagonal.variance.matrix = TRUE,
                      forbid.treatment.reversal = FALSE)
PM.maha

dem.panel$moderator <- ifelse(dem.panel$ADM1_EN %in% northern_regions_en, 1, 0)

est <- PanelEstimate(sets = PM.maha,
                     panel.data = dem.panel,
                     se.method = "bootstrap",
                     moderator = "moderator")

plot(est$`1`)

pm.obj <- PanelMatch(lead = 0:30, lag = 30, refinement.method = "mahalanobis",
                     panel.data = dem.panel, match.missing = TRUE,
                     covs.formula = ~ is_ban + Total_pop + I(lag(is_ban, 1:30)) + 
                       I(lag(n_fires_modis, 1:30)),
                     size.match = 5, qoi = "att", placebo.test = TRUE)

placebo_test(
  pm.obj,
  dem.panel,
  lag.in = NULL,
  number.iterations = 100,
  confidence.level = 0.95,
  plot = FALSE,
  se.method = "bootstrap",
  parallel = FALSE,
  num.cores = 1
)

DisplestDisplayTreatment(
  dem.panel,
  color.of.treated = "red",
  color.of.untreated = "blue",
  title = "Treatment Distribution \n Across Units and Time",
  xlab = "Time",
  ylab = "Unit",
  x.size = NULL
)

# Find combinations that appear more than once
#DisplayTreatment(panel.data = dem.panel, legend.position = "none",
#                 xlab = "year", ylab = "Country Code",
#                 hide.x.tick.label = TRUE, hide.y.tick.label = TRUE)


PM.maha <- PanelMatch(panel.data = dem.panel,
                      lag = 4,
                      refinement.method = "mahalanobis",
                      match.missing = FALSE,
                      covs.formula = ~ I(lag(tradewb, 0:4)) +
                        I(lag(y, 1:4)),
                      size.match = 5,
                      qoi = "att",
                      lead = 0:2,
                      use.diagonal.variance.matrix = TRUE,
                      forbid.treatment.reversal = FALSE)
