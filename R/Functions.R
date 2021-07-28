
# 1. Setup create geodata function --------------------------------------------------


read_write_geodata <- function(path_x0,path_tte) {

# Load data ---------------------------------------------------------------

  X <- read_csv(path_x0, col_types =cols(),col_names=T) %>%
    mutate_at(vars(depth), factor) %>%
    column_to_rownames(var = "depth") %>%
    as.matrix()

  X0 <- read_csv(path_tte, col_types =cols())

# Krumbein phi scale ---------------------------------------------------------

  phi <- as.numeric(colnames(X))

# get l and q -------------------------------------------------------------------

  l <- get.l(X = X, max = 0.95, n = 20)
  q <- get.q(X = X, l = l)
  l <- as.numeric(rownames(q))

# model EMs and set limits ---------------------------------------------------------------

  em.pot <- model.EM(X = X, q = q, l = l, plot = F)

  limits <- get.limits(loadings = em.pot)

# get robust EMs ----------------------------------------------------------

  em.rob <- robust.EM(em = em.pot,classunits = phi,limits = limits,
                      plot = F,
                      legend = "topright",
                      cex = 1.2,
                      cex.axis = 2,
                      colour = brewer.pal(length(limits)/2, 'Set1'),
                      median = TRUE,
                      mc_n = 1000)


# write datageo.csv file ------------------------------------------------------


  X1 <-as_tibble(matrix(em.rob$scores$mean,ncol = 5, dimnames = list(NULL, c("V1", "V2", "V3", "V4", "V5"))), .name_repair = "unique") %>%
    rename(EM1=V1, EM2=V2,EM3=V3, EM4=V4,EM5=V5)
  X2 <-as_tibble(matrix(em.rob$scores$sd,ncol = 5, dimnames = list(NULL, c("V1", "V2", "V3", "V4", "V5"))), .name_repair = "unique") %>%
    rename(EM1sd=V1, EM2sd=V2,EM3sd=V3, EM4sd=V4,EM5sd=V5)


  if(!file.exists("analysis/data/derived_data/datageo.csv")){
    datageo <- bind_cols(X0,X1, X2)
    path_out <- here::here("analysis/data/derived_data/","datageo.csv")
    write_csv(datageo,path_out)
  }


}


# 2. Create Figure 2 stratigraphic plot -----------------------------------------------------

plot_stratplot <- function(path) {

  # Load data ---------------------------------------------------------------
  geodata <- read_csv(path, col_types =cols())

  # lengthens data --------------------------------------------------------

  geodata_longer <- geodata %>%
    mutate(silt= silt_coarse + silt_fine,
           TOC.TN = (TOC/12)/(TN/14)) %>%
    dplyr::select(depth,clay,silt,sand,
                  TOC,TIC,TC,TN,TOC.TN,
                  d13C, d18O) %>%
    rename(Depth = depth,`Clay`=clay,
           `Silt`=silt,
           `Sand`=sand,
           `TOC/TN`=TOC.TN,
           `d13C`=d13C,
           `d18O`=d18O) %>%
    pivot_longer(cols =Clay:d18O,names_to = "values", values_to = "count") %>%
    filter(values %in% c("Sand", "Silt","Clay",
                         "TOC","TIC","TC","TN","TOC/TN","d13C", "d18O")) %>%
    mutate(values = fct_relevel(values,"Sand", "Silt",
                                "Clay","TC","TIC","TOC","TN","TOC/TN","d13C", "d18O"))


  # get zone  ---------------------------------------------------------------

  zone_data <- tibble(ymin = c(38,140,240),
                      ymax = c(50,170,268),
                      xmin = -Inf, xmax = Inf)  ###### Stratiplot

  # Cluster plot ------------------------------------------------------------

  coniss <- geodata_longer %>% ###### Cluster plot
    nested_data(qualifiers = Depth, key = values, value = count, trans = scale) %>%
    nested_chclust_coniss()

  # Geochemical stratigraphic plots  -------------------------------------------------------

  alta_plot <- ggplot(geodata_longer,aes(x = count, y = Depth)) +
    geom_lineh() +
    geom_point() +
    geom_rect(mapping = aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax),
              data = zone_data,
              alpha = 0.4,
              fill = "blue",inherit.aes = FALSE ) +
    scale_y_reverse(breaks = c(40,60,80,100,120,140,
                               160,180,200,220,240,260,280)) +
    facet_geochem_gridh(
      vars(values),
      units = c("d13C" = "‰", "d18O" = "‰"),
      default_units = "%") +
    labs(x = NULL, y = "Depth (cm)") +
    scale_x_continuous(breaks = scales::breaks_extended(n = 3)) +
    theme_set(theme_paleo(8)) +
    theme(
      text = element_text(size=20),
      axis.ticks = element_line(colour = "grey70", size = 0.3),
      panel.grid.major =element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = "grey50")
    )

  wrap_plots(
    alta_plot +
      theme(strip.background = element_blank(), strip.text.y = element_blank()),
    ggplot() +
      layer_zone_boundaries(coniss, aes(y = Depth)) +
      layer_dendrogram(coniss, aes(y = Depth), param = "CONISS") +
      scale_x_continuous(breaks = scales::breaks_extended(n = 3)) +
      theme(axis.text.y.left = element_blank(),
            axis.ticks.y.left = element_blank(),
            text = element_text(size=15),
            panel.background = element_rect(fill = "white", colour = "grey50"))+
      labs(x = "coniss", y = NULL),
    nrow = 1,
    widths = c(8, 0.8)
  )


# Export plot -------------------------------------------------------------

  #  pdf_out <- here::here("analysis/figures","Fig2.pdf")
  #  png_out <- here::here("analysis/figures","Fig2.png")

  #  ggsave(pdf_out, width = 25,height = 12, units = 'cm',device = cairo_pdf)
  #  ggsave(png_out,  width = 25,height = 12, units = 'cm',device = "png")

}


# Table age ---------------------------------------------------------------

table_age <- function(path) {

  dt <- tibble(
    Items = c("D-AMS 039542", "D-AMS 039543", "D-AMS 039544", "D-AMS 039545","D-AMS 039546", "D-AMS 039547","D-AMS 039548","D-AMS 039549","D-AMS 039550","D-AMS 039551","D-AMS 039552","D-AMS 039553","D-AMS 039554"),
    Text_1 = c("39", "44", "67","106", "150","161","162","162","168","199","223","254","266"),
    Text_2 = c("5345", "5806", "7259","8657","9736","10211","10936","10747","10590","11117","9977","17353","16558"),
    Text_3 = c("28", "33", "32", "45","35", "63","41", "49","39", "51","45", "84","69"),
    Text_4 = c("51.41","48.54","40.51","34.04","29.76","28.05","25.63","26.24","26.76","25.06","28.88","11.53","12.73"),
    Text_5 = c("0.18", "0.20", "0.16", "0.19","0.13", "0.22","0.13", "0.16","0.13", "0.16","0.16", "0.12","0.11"),
    Text_6 = c("6086","6571","8021","9591","11134","11817","12813","12709","12549" ,"13015","11377", "20890","19959"),
    Text_7 = c("69","56","62","57","111","144","46", "38","64" ,"61","122", "124","130"),
    Text_8 = c("Charcoal ", "Charcoal", "Charcoal","Charcoal","Bulk sediment","Bulk sediment","Charcoal","Bulk sediment","Charcoal","Bulk sediment","Bulk sediment","Bulk sediment","Bulk sediment")
  )
  kbl(dt, caption = "mtcars Data Summary\n",align = c('l','c','c','c','c','c','c','c','r'),format = "latex", booktabs = T, row.names = FALSE, linesep = "", escape = F,
      col.names = c("Lab code","Depth","$^{14}$C Age","$1\\sigma$ error","pMC","$1\\sigma$ error","Median yr cal BP","$1\\sigma$ error","Material")) %>%
    kable_classic(full_width = F) %>%
    kable_styling(position = "left",font_size = 7)


}


# age model ---------------------------------------------------------------

agemodel_bacon <- function(path) {

  rbacon::Bacon(core = "TT3", coredir = "cores_bacon",
        ask = FALSE, plot.pdf = T,
        thick = 2,ssize=8000,
        hiatus.depths=c(176),
        acc.mean=c(50,80),
        acc.shape=c(1.4,1),
        slump=c(176,180)
        )
}
