## R/plot_diatom_pb.R

#' Plot Diatom P/B Index
#'
#' @param diatom_df tibble from load_and_prep_data()$diatom
#' @param zone_data tibble from load_and_prep_data()$zone_data
#' @return A ggplot2 object
#' @import ggplot2
#' @export
plot_diatom_pb <- function(diatom_df, zone_data) {
  taxon0 <- diatom_df %>%
    dplyr::select(taxon, depth, abn) %>%
    tidyr::pivot_wider(names_from = taxon, values_from = abn, values_fill = 0)

  taxon_sel <- taxon0 %>%
    dplyr::mutate(
      Planktonic = Melosira_aff_varians + Stephanocyclus_planus + Discostella_aff_stelligera,
      Benthic    = Fragilaria_sp1 + Cymbella_aff_subturgidula + Ulnaria_sp2_thin +
        Nitzschia_sp1_square + Nitzschia_sp2_aff_frustulum + Nitzschia_sp3_long_without_striae +
        Eunotia_sp1_large + Eunotia_sp2_aff_bilunaris + Epithimia_turgida + Epithemia_gibba +
        Anomoeoneis_sphaerophora + Anomoeoneis_sp2_smaller + Cymbella_sp1_aff_subturgidula +
        Cymbella_sp2_aff_fontinalis + Cymbella_sp4_thin + Enyonema_sp1 + Encyonema_sp2 +
        Gomphonema_sp1_aff_parvalum + Gomphonema_sp2_aff_celatum_with_stigma + Gomphonema_sp3_aff_spatiosum +
        Gomphonema_sp4_aff_gracile + Gomphonema_sp5_aff_spatiosum + Gomphonema_aff_anglicum +
        Gomphonema_lagenula + Diploneis_sp1 + Pinnularia_nov_sp18 + Pinnularia_sp1_large +
        Pinnularia_aff_gigas + Pinnularia_acrosphaeria + Pinnularia_inconstans + Cocconeis_sp1_aff_placentula +
        Sellahora_sp1_aff_nigri + Sellaphora_sp2_rostrado + Sellaphora_sp3_classic +
        Planothidium_sp1_aff_lancelatum + Planothidium_sp2_aff_cross + Planothidium_sp3 +
        Stauroneis_aff_heinii + Amphora_sp1 + Amphora_copulata + Craticula_sp1 + Craticula_sp2_smaller +
        Cymbopleura_sp1 + Hanzschia_sp1 + Navicula_sp1 + Sellaphora_pupula + Sellaphora_laevis +
        Caloneis_sp1 + Campylodiscus + Adlafia_sp1 + Achnanthidium_macrocephalum,
      Aulacoseira = Aulacoseira_granulata + Aulacoseira_sp2_aff_liucoensis + Aulacoseira_sp3_grande,
      Small_Fragilarioids = small_fragilarioids
    ) %>%
    dplyr::transmute(
      depth,
      `Aulacoseira sp.`       = Aulacoseira,
      Plankton                = Planktonic,
      Benthic                 = Benthic,
      `Small Fragilarioids`   = Small_Fragilarioids,
      `P/B index`             = Planktonic / (Planktonic + Benthic)
    ) %>%
    tidyr::pivot_longer(-depth, names_to = "taxon", values_to = "value")

  ggplot2::ggplot(taxon_sel, aes(x = value, y = depth)) +
    geom_col_segsh(aes(colour = taxon), size = 1.5, show.legend = FALSE) +
    scale_color_grey(start = 0.7, end = 0.1) +
    scale_y_reverse(breaks = seq(0, 260, by = 20)) +
    geom_rect(data = zone_data,
              aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax),
              fill = "red", alpha = 0.2, inherit.aes = FALSE) +
    facet_abundanceh(vars(taxon), scales = "free", space = "fixed", shrink = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_paleo(8) +
    ggtitle("Diatom P/B index")
}

