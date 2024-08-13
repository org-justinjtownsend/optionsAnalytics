# Need a good way of incorporating the powerful message of the stacked VOL visualisations below.

vols <- c(10, 20, 30)
vols.methods <- c("close", "garman.klass", "parkinson")
vols.real.all <- idx.vols.real(vols, vols.methods, SPX.QMOD)

vols.real.all <- as.data.frame(vols.real.all)
vols.real.all.cols <- colnames(vols.real.all)

# Packages
library(DT)
library(ggridges)

# xts method to combine chart series for vol, see: ...com

vols.real.all <- tidyr::pivot_longer(vols.real.all, cols = vols.real.all.cols, names_to = "vols", values_to = "vals")

vols.real.all %>%
  dplyr::group_by(vols) %>%
  dplyr::mutate(mean_vols = mean(vals)) %>%
  ggplot2::ggplot(ggplot2::aes(x = vals, y = reorder(vols, mean_vols), color = vols, fill = vols)) +
  ggplot2::geom_boxplot(alpha = 0.3) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none",
                 axis.title.y = ggplot2::element_blank()) +
  ggplot2::ggtitle("Realized Volatility Distributions")

vols.real.all %>%
  dplyr::group_by(vols) %>%
  ggplot2::ggplot(ggplot2::aes(x = vals, color = vols, fill = vols)) +
  ggplot2::geom_density(alpha = 0.15) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::ggtitle("Realized Volatility Distributions") +
  ggplot2::facet_wrap(~vols)

vols.real.all %>%
  dplyr::group_by(vols) %>%
  dplyr::mutate(mean_vols = mean(vals)) %>%
  ggplot2::ggplot(ggplot2::aes(x = vals, y = reorder(vols, mean_vols), color = vols, fill = vols)) +
  ggridges::geom_density_ridges(alpha = 0.15) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::ggtitle("Realized Volatility Distributions")