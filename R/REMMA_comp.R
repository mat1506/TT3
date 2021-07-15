
path_x0 <- here::here("analysis/data/raw_data/","2021-06-16_TT3_data_H01.csv")
X0 <- read_csv(path_x0, col_types =cols())

path_tte <- here::here("analysis/data/raw_data/","TTG.csv")
X <- read_csv(path_tte, col_types =cols(),col_names=T) %>%
  mutate_at(vars(depth), factor) %>%
  column_to_rownames(var = "depth") %>%
  as.matrix()

##phi <- convert.units(mu = mu)
phi <- as.numeric(colnames(X))
## get l
l <- get.l(X = X, max = 0.95, n = 20)
## get q
q <- get.q(X = X, l = l)
l <- as.numeric(rownames(q))
## model EMs
em.pot <- model.EM(X = X, q = q, l = l, plot = F)
## set limits
limits <- get.limits(loadings = em.pot)
## get robust EMs
library(RColorBrewer)
em.rob <- robust.EM(em = em.pot,classunits = phi,limits = limits,
                    plot = F,
                    legend = "topright",
                    cex = 1.2,
                    cex.axis = 2,
                    colour = brewer.pal(length(limits)/2, 'Set1'),
                    median = TRUE,
                    mc_n = 1000)

X1 <-as_tibble(em.rob$scores$mean) %>%
  rename(EM1=V1, EM2=V2,EM3=V3, EM4=V4,EM5=V5)
X2 <-as_tibble(em.rob$scores$sd) %>%
  rename(EM1sd=V1, EM2sd=V2,EM3sd=V3, EM4sd=V4,EM5sd=V5)

datageo <- bind_cols(X1, X2)
  path_out <- here::here("analysis/data/derived_data/","datageo.csv")
  write_csv(datageo,path_out)

rm(list = ls())
