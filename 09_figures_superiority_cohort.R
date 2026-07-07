# =====================================================================
# 07_figures_superiority_only.R
#
# Regenerates manuscript Figures 2 and 3 using the superiority-only cohort.
#
# Figure 1 PRISMA is intentionally NOT regenerated.
#
# Inputs:
#   outputs/fi_set_superiority_only.rds
#   outputs/nb_fit_superiority_only.rds
#
# Outputs:
#   figures/Figure2_Radar_FI_superiority.png/.tiff
#   figures/Figure3_ForestPlot_superiority.png/.tiff
#   outputs/Figure3_caption_note_superiority.txt
# =====================================================================

source("00_utils.R")

require_pkgs(c(
  "dplyr", "tidyr", "ggplot2", "scales", "broom", "cowplot",
  "forcats", "tibble", "stringr", "readr", "patchwork"
))

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(broom)
library(cowplot)
library(forcats)
library(tibble)
library(stringr)
library(readr)
library(patchwork)

DIR_OUTPUTS <- if (exists("DIR_OUTPUTS")) DIR_OUTPUTS else "outputs"
DIR_FIGURES <- if (exists("DIR_FIGURES")) DIR_FIGURES else "figures"

ensure_dir(DIR_OUTPUTS)
ensure_dir(DIR_FIGURES)

FI_SET_RDS <- file.path(DIR_OUTPUTS, "fi_set_superiority_only.rds")
MODEL_RDS  <- file.path(DIR_OUTPUTS, "nb_fit_superiority_only.rds")

if (!file.exists(FI_SET_RDS)) {
  stop("Missing outputs/fi_set_superiority_only.rds.", call. = FALSE)
}

if (!file.exists(MODEL_RDS)) {
  stop("Missing outputs/nb_fit_superiority_only.rds.", call. = FALSE)
}

fi_set <- readRDS(FI_SET_RDS)
final_model <- readRDS(MODEL_RDS)

message("Loaded superiority-only cohort: ", dplyr::n_distinct(fi_set$record_id), " trials")
print(table(fi_set$rct_type.factor, useNA = "ifany"))

# =====================================================================
# FIGURE 2 — Radar plot using superiority-only cohort
# =====================================================================

use_fixed_threshold <- TRUE
fi_cut <- 4
ylim_max <- 60
radial_breaks <- c(0, 20, 40, 60)
wrap_labels <- TRUE

fi_set2 <- fi_set %>%
  dplyr::mutate(
    FI_group = if (use_fixed_threshold) {
      ifelse(
        fi_each > fi_cut,
        paste0("FI > ", fi_cut),
        paste0("FI ≤ ", fi_cut)
      )
    } else {
      ifelse(
        fi_each > median(fi_each, na.rm = TRUE),
        "FI > median",
        "FI ≤ median"
      )
    },
    
    rct_blind_d.factor = dplyr::case_when(
      rct_blind.factor %in% c("Single blind", "Double blind", "Triple blind") ~ "Yes",
      rct_blind.factor %in% c("Unblinded") ~ "No",
      TRUE ~ NA_character_
    ),
    
    rct_conceal_d.factor = dplyr::case_when(
      rct_conceal.factor %in% c(
        "Sequentially numbered sealed/opaque envelopes",
        "Sequentially numbered containers",
        "Pharmacy controlled allocation",
        "Central allocation (site remote from trial location)",
        "Other"
      ) ~ "Yes",
      rct_conceal.factor %in% c("Unclear") ~ "Unclear",
      TRUE ~ NA_character_
    )
  )

risk_by_FI <- fi_set2 %>%
  dplyr::group_by(FI_group) %>%
  dplyr::summarise(
    `No blinding`                = mean(rct_blind_d.factor == "No", na.rm = TRUE) * 100,
    `No concealment`             = mean(rct_conceal_d.factor == "Unclear", na.rm = TRUE) * 100,
    `No sample size calculation` = mean(sample_calc.factor == "No", na.rm = TRUE) * 100,
    `No data sharing`            = mean(d_share.factor == "No", na.rm = TRUE) * 100,
    `Funding not reported`       = mean(funding.factor == "Not reported", na.rm = TRUE) * 100,
    .groups = "drop"
  )

axes <- c(
  "No blinding",
  "No concealment",
  "No sample size calculation",
  "No data sharing",
  "Funding not reported"
)

dat_long <- risk_by_FI %>%
  tidyr::pivot_longer(
    -FI_group,
    names_to = "axis",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    axis = factor(axis, levels = axes),
    value = ifelse(is.na(value) | is.nan(value), 0, value)
  ) %>%
  dplyr::arrange(FI_group, axis)

k <- length(axes)

theta <- seq(-pi / 2, 3 * pi / 2, length.out = k + 1)[1:k]

angle_df <- tibble::tibble(
  axis = factor(axes, levels = axes),
  theta = theta
)

dat_cart <- dat_long %>%
  dplyr::left_join(angle_df, by = "axis") %>%
  dplyr::mutate(
    r = pmin(value, ylim_max),
    x = r * cos(theta),
    y = r * sin(theta)
  )

dat_poly <- dat_cart %>%
  dplyr::group_by(FI_group) %>%
  dplyr::bind_rows(dplyr::slice_head(., n = 1)) %>%
  dplyr::ungroup()

grid_rings <- do.call(
  rbind,
  lapply(radial_breaks, function(br) {
    df <- data.frame(axis = factor(axes, levels = axes))
    df <- merge(df, angle_df, by = "axis", sort = FALSE)
    df$r <- br
    df$x <- df$r * cos(df$theta)
    df$y <- df$r * sin(df$theta)
    df$ring <- br
    rbind(df, df[1, ])
  })
)

spokes <- angle_df %>%
  dplyr::mutate(
    x1 = ylim_max * cos(theta),
    y1 = ylim_max * sin(theta)
  )

label_radius <- ylim_max * 1.17

labels <- angle_df %>%
  dplyr::mutate(
    lab = as.character(axis),
    lab = if (wrap_labels) {
      dplyr::case_when(
        lab == "No sample size calculation" ~ "No sample size\ncalculation",
        lab == "Funding not reported" ~ "Funding not\nreported",
        TRUE ~ lab
      )
    } else {
      lab
    },
    x = label_radius * cos(theta),
    y = label_radius * sin(theta),
    hjust = dplyr::case_when(
      cos(theta) > 0.15 ~ 0,
      cos(theta) < -0.15 ~ 1,
      TRUE ~ 0.5
    ),
    vjust = dplyr::case_when(
      sin(theta) > 0.15 ~ 0,
      sin(theta) < -0.15 ~ 1,
      TRUE ~ 0.5
    )
  )

xlim <- range(c(-ylim_max, ylim_max, labels$x)) * 1.18
ylim <- range(c(-ylim_max, ylim_max, labels$y)) * 1.18

p_radar <- ggplot() +
  geom_polygon(
    data = grid_rings,
    aes(x = x, y = y, group = ring),
    fill = NA,
    color = "grey80",
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  geom_segment(
    data = spokes,
    aes(x = 0, y = 0, xend = x1, yend = y1),
    color = "grey75",
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  geom_polygon(
    data = dat_poly,
    aes(x = x, y = y, group = FI_group, fill = FI_group),
    alpha = 0.22,
    color = NA
  ) +
  geom_polygon(
    data = dat_poly,
    aes(x = x, y = y, group = FI_group, color = FI_group),
    fill = NA,
    linewidth = 0.9
  ) +
  geom_point(
    data = dat_cart,
    aes(x = x, y = y, color = FI_group),
    size = 2
  ) +
  geom_text(
    data = labels,
    aes(x = x, y = y, label = lab, hjust = hjust, vjust = vjust),
    size = 4.1,
    color = "grey35",
    lineheight = 1.05
  ) +
  scale_x_continuous(
    limits = xlim,
    expand = expansion(mult = 0.04)
  ) +
  scale_y_continuous(
    limits = ylim,
    expand = expansion(mult = 0.04)
  ) +
  coord_equal(clip = "off") +
  theme_void(base_size = 13) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    plot.margin = unit(c(35, 70, 35, 70), "pt")
  )

ggsave(
  file.path(DIR_FIGURES, "Figure2_Radar_FI_superiority.png"),
  p_radar,
  width = 8.5,
  height = 6.2,
  dpi = 600,
  bg = "white"
)

ggsave(
  file.path(DIR_FIGURES, "Figure2_Radar_FI_superiority.tiff"),
  p_radar,
  width = 8.5,
  height = 6.2,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

readr::write_csv(
  risk_by_FI,
  file.path(DIR_OUTPUTS, "Figure2_Radar_FI_superiority_data.csv")
)

# =====================================================================
# FIGURE 3 — Forest plot from final reduced superiority-only model
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(broom)
  library(patchwork)
  library(readr)
})

DIR_OUTPUTS <- if (exists("DIR_OUTPUTS")) DIR_OUTPUTS else "outputs"
DIR_FIGURES <- if (exists("DIR_FIGURES")) DIR_FIGURES else "figures"

dir.create(DIR_OUTPUTS, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_FIGURES, showWarnings = FALSE, recursive = TRUE)

# Save final model
saveRDS(
  nb_superiority,
  file.path(DIR_OUTPUTS, "nb_fit_superiority_only_reduced.rds")
)

# Term labels
term_labels <- c(
  "c_participants.factorCardiovascular" = "Population: Cardiovascular",
  "c_participants.factorObstetric"      = "Population: Obstetric",
  "c_participants.factorPediatric"      = "Population: Pediatric",
  "c_participants.factorRegional"       = "Population: Regional",
  
  "c_design_collapsed.factorTwo-by-two factorial trial" = "Design: 2×2 factorial",
  "c_design_collapsed.factorOther" = "Design: Other",
  
  "i_type.factorNon-drug related" = "Intervention: Non-drug related",
  
  "rct_blind_d.factorYes" = "Blinding: Yes",
  "rct_conceal_d.factorYes" = "Allocation concealment: Yes",
  
  "rct_centers.factorSingle-center" = "Centers: Single-center",
  "rct_centers.factorUnclear" = "Centers: Unreported",
  
  "funding.factorNon-industry funded" = "Funding: Non-industry funded",
  "funding.factorNot reported" = "Funding: Not reported",
  
  "d_share.factorUnclear" = "Data sharing: Unreported",
  "d_share.factorYes" = "Data sharing: Yes",
  
  "sample_size" = "Sample size",
  "ltfu" = "Lost to follow-up"
)

label_term <- function(term) {
  if (term %in% names(term_labels)) {
    term_labels[[term]]
  } else {
    term
  }
}

# Build forest dataset
forest_data <- broom::tidy(
  nb_superiority,
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::transmute(
    term,
    label   = vapply(term, label_term, character(1)),
    IRR     = estimate,
    lowerCI = conf.low,
    upperCI = conf.high,
    p.value = p.value
  ) %>%
  dplyr::mutate(
    direction = dplyr::case_when(
      IRR > 1 ~ "IRR > 1",
      IRR < 1 ~ "IRR < 1",
      TRUE ~ "IRR = 1"
    ),
    IRR_txt = sprintf("%.2f", IRR),
    CI_txt = paste0(
      "[",
      sprintf("%.2f", lowerCI),
      ", ",
      sprintf("%.2f", upperCI),
      "]"
    ),
    p_txt = dplyr::case_when(
      is.na(p.value) ~ "",
      p.value < 0.001 ~ "<0.001",
      TRUE ~ formatC(p.value, format = "f", digits = 3)
    )
  ) %>%
  dplyr::arrange(desc(IRR)) %>%
  dplyr::mutate(
    label_factor = factor(label, levels = rev(label), ordered = TRUE)
  )

# Export data behind figure
readr::write_csv(
  forest_data,
  file.path(DIR_OUTPUTS, "Figure3_ForestPlot_superiority_reduced_data.csv")
)

# Color map
col_map <- c(
  "IRR > 1" = "#2C7BE5",
  "IRR < 1" = "#D94841",
  "IRR = 1" = "grey30"
)

x_max <- max(forest_data$upperCI, na.rm = TRUE)

# Left forest plot
p_forest_left <- ggplot(
  forest_data,
  aes(x = IRR, y = label_factor)
) +
  geom_errorbar(
    aes(xmin = lowerCI, xmax = upperCI, colour = direction),
    linewidth = 0.9,
    width = 0.20
  ) +
  geom_point(
    aes(colour = direction),
    size = 3
  ) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    linewidth = 0.8,
    colour = "grey35"
  ) +
  scale_colour_manual(values = col_map, guide = "none") +
  scale_x_continuous(
    limits = c(0, x_max * 1.05),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    x = "Incidence Rate Ratio (IRR) with 95% CI",
    y = "Trial characteristics"
  ) +
  theme_minimal(base_size = 12.5) +
  theme(
    axis.text.y = element_text(size = 11),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 6, 10, 10)
  )

# Right table
p_table_right <- ggplot(
  forest_data,
  aes(y = label_factor)
) +
  geom_text(aes(x = 1, label = IRR_txt), hjust = 0, size = 3.6) +
  geom_text(aes(x = 2, label = CI_txt), hjust = 0, size = 3.6) +
  geom_text(aes(x = 3, label = p_txt), hjust = 0, size = 3.6) +
  annotate("text", x = 1, y = Inf, label = "IRR",
           vjust = 1.2, hjust = 0, fontface = "bold", size = 3.8) +
  annotate("text", x = 2, y = Inf, label = "95% CI",
           vjust = 1.2, hjust = 0, fontface = "bold", size = 3.8) +
  annotate("text", x = 3, y = Inf, label = "p-value",
           vjust = 1.2, hjust = 0, fontface = "bold", size = 3.8) +
  scale_x_continuous(
    limits = c(0.9, 3.9),
    expand = c(0, 0)
  ) +
  theme_void(base_size = 12.5) +
  theme(
    plot.margin = margin(10, 25, 10, 0)
  ) +
  coord_cartesian(clip = "off")

# Combine
p_forest <- p_forest_left + p_table_right +
  patchwork::plot_layout(widths = c(2.5, 1.7))

# Export
ggsave(
  file.path(DIR_FIGURES, "Figure3_ForestPlot_superiority_reduced.png"),
  p_forest,
  width = 13,
  height = 8,
  dpi = 600,
  bg = "white"
)

ggsave(
  file.path(DIR_FIGURES, "Figure3_ForestPlot_superiority_reduced.tiff"),
  p_forest,
  width = 13,
  height = 8,
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

writeLines(
  "Note: The vertical dashed line indicates IRR = 1 (no association).",
  con = file.path(DIR_OUTPUTS, "Figure3_caption_note_superiority_reduced.txt")
)

message("Figure 3 updated using the final reduced superiority-only model.")

# =====================================================================
# FIGURE 3 — Alternative version, grouped by domain
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(broom)
  library(patchwork)
  library(readr)
  library(MASS)
})

DIR_OUTPUTS <- if (exists("DIR_OUTPUTS")) DIR_OUTPUTS else "outputs"
DIR_FIGURES <- if (exists("DIR_FIGURES")) DIR_FIGURES else "figures"

dir.create(DIR_OUTPUTS, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_FIGURES, showWarnings = FALSE, recursive = TRUE)

# Rescale sample size for interpretability
model_superiority <- model_superiority %>%
  dplyr::mutate(sample_size100 = sample_size / 100)

# Refit same final model, using sample size per 100 participants
nb_superiority_alt <- MASS::glm.nb(
  fi_each ~
    c_participants.factor +
    c_design_collapsed.factor +
    i_type.factor +
    rct_blind_d.factor +
    rct_conceal_d.factor +
    rct_centers.factor +
    funding.factor +
    d_share.factor +
    sample_size100 +
    ltfu,
  data = model_superiority
)

saveRDS(
  nb_superiority_alt,
  file.path(DIR_OUTPUTS, "nb_fit_superiority_only_reduced_sample100.rds")
)

# Labels
term_labels <- c(
  "c_participants.factorCardiovascular" = "Population: Cardiovascular",
  "c_participants.factorObstetric"      = "Population: Obstetric",
  "c_participants.factorPediatric"      = "Population: Pediatric",
  "c_participants.factorRegional"       = "Population: Regional",
  
  "c_design_collapsed.factorTwo-by-two factorial trial" = "Design: 2×2 factorial",
  "c_design_collapsed.factorOther" = "Design: Other",
  
  "i_type.factorNon-drug related" = "Intervention: Non-drug related",
  
  "rct_blind_d.factorYes" = "Blinding: Yes",
  "rct_conceal_d.factorYes" = "Concealment: Yes",
  
  "rct_centers.factorSingle-center" = "Single-center",
  "rct_centers.factorUnclear" = "Centers: Unreported",
  
  "funding.factorNon-industry funded" = "Funding: Non-industry funded",
  "funding.factorNot reported" = "Funding: Not reported",
  
  "d_share.factorUnclear" = "No data-sharing statement",
  "d_share.factorYes" = "Data-sharing statement: Yes",
  
  "sample_size100" = "Sample size, per 100 participants",
  "ltfu" = "Lost to follow-up"
)

label_term <- function(term) {
  if (term %in% names(term_labels)) term_labels[[term]] else term
}

# Desired display order
display_order <- c(
  "Population: Cardiovascular",
  "Population: Obstetric",
  "Population: Pediatric",
  "Population: Regional",
  "Design: 2×2 factorial",
  "Design: Other",
  "Intervention: Non-drug related",
  "Blinding: Yes",
  "Concealment: Yes",
  "Single-center",
  "Funding: Non-industry funded",
  "Funding: Not reported",
  "Data-sharing statement: Yes",
  "No data-sharing statement",
  "Sample size, per 100 participants",
  "Lost to follow-up"
)

forest_data_alt <- broom::tidy(
  nb_superiority_alt,
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(
    label = vapply(term, label_term, character(1))
  ) %>%
  # Omit sparse category from the figure only
  dplyr::filter(label != "Centers: Unreported") %>%
  dplyr::transmute(
    term,
    label,
    IRR = estimate,
    lowerCI = conf.low,
    upperCI = conf.high,
    p.value = p.value
  ) %>%
  dplyr::mutate(
    direction = dplyr::case_when(
      IRR > 1 ~ "IRR > 1",
      IRR < 1 ~ "IRR < 1",
      TRUE ~ "IRR = 1"
    ),
    IRR_txt = sprintf("%.2f", IRR),
    CI_txt = paste0("[", sprintf("%.2f", lowerCI), ", ", sprintf("%.2f", upperCI), "]"),
    p_txt = dplyr::case_when(
      is.na(p.value) ~ "",
      p.value < 0.001 ~ "<0.001",
      TRUE ~ formatC(p.value, format = "f", digits = 3)
    ),
    label_factor = factor(label, levels = rev(display_order), ordered = TRUE)
  ) %>%
  dplyr::arrange(label_factor)

readr::write_csv(
  forest_data_alt,
  file.path(DIR_OUTPUTS, "Figure3_ForestPlot_superiority_grouped_data.csv")
)

col_map <- c(
  "IRR > 1" = "#2C7BE5",
  "IRR < 1" = "#D94841",
  "IRR = 1" = "grey30"
)

x_max <- max(forest_data_alt$upperCI, na.rm = TRUE)

p_forest_left_alt <- ggplot(
  forest_data_alt,
  aes(x = IRR, y = label_factor)
) +
  geom_errorbar(
    aes(xmin = lowerCI, xmax = upperCI, colour = direction),
    linewidth = 0.9,
    width = 0.20
  ) +
  geom_point(
    aes(colour = direction),
    size = 3
  ) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    linewidth = 0.8,
    colour = "grey35"
  ) +
  scale_colour_manual(values = col_map, guide = "none") +
  scale_x_continuous(
    limits = c(0, x_max * 1.05),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    x = "Incidence Rate Ratio (IRR) with 95% CI",
    y = "Trial characteristics"
  ) +
  theme_minimal(base_size = 12.5) +
  theme(
    axis.text.y = element_text(size = 11),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 6, 10, 10)
  )

p_table_right_alt <- ggplot(
  forest_data_alt,
  aes(y = label_factor)
) +
  geom_text(aes(x = 1, label = IRR_txt), hjust = 0, size = 3.6) +
  geom_text(aes(x = 2, label = CI_txt), hjust = 0, size = 3.6) +
  geom_text(aes(x = 3, label = p_txt), hjust = 0, size = 3.6) +
  annotate("text", x = 1, y = Inf, label = "IRR",
           vjust = 1.2, hjust = 0, fontface = "bold", size = 3.8) +
  annotate("text", x = 2, y = Inf, label = "95% CI",
           vjust = 1.2, hjust = 0, fontface = "bold", size = 3.8) +
  annotate("text", x = 3, y = Inf, label = "p-value",
           vjust = 1.2, hjust = 0, fontface = "bold", size = 3.8) +
  scale_x_continuous(
    limits = c(0.9, 3.9),
    expand = c(0, 0)
  ) +
  theme_void(base_size = 12.5) +
  theme(
    plot.margin = margin(10, 25, 10, 0)
  ) +
  coord_cartesian(clip = "off")

p_forest_alt <- p_forest_left_alt + p_table_right_alt +
  patchwork::plot_layout(widths = c(2.5, 1.7))

ggsave(
  file.path(DIR_FIGURES, "Figure3_ForestPlot_superiority_grouped.png"),
  p_forest_alt,
  width = 13,
  height = 8,
  dpi = 600,
  bg = "white"
)

ggsave(
  file.path(DIR_FIGURES, "Figure3_ForestPlot_superiority_grouped.tiff"),
  p_forest_alt,
  width = 13,
  height = 8,
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

message("Alternative grouped Figure 3 saved.")

# =====================================================================
# FIGURE 3 — Final forest plot, reduced superiority-only model
# Ordered by IRR, with improved labels
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(broom)
  library(patchwork)
  library(readr)
  library(MASS)
})

DIR_OUTPUTS <- if (exists("DIR_OUTPUTS")) DIR_OUTPUTS else "outputs"
DIR_FIGURES <- if (exists("DIR_FIGURES")) DIR_FIGURES else "figures"

dir.create(DIR_OUTPUTS, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_FIGURES, showWarnings = FALSE, recursive = TRUE)

# Rescale sample size
model_superiority <- model_superiority %>%
  dplyr::mutate(sample_size100 = sample_size / 100)

# Refit final reduced model
nb_superiority_final <- MASS::glm.nb(
  fi_each ~
    c_participants.factor +
    c_design_collapsed.factor +
    i_type.factor +
    rct_blind_d.factor +
    rct_conceal_d.factor +
    rct_centers.factor +
    funding.factor +
    d_share.factor +
    sample_size100 +
    ltfu,
  data = model_superiority
)

saveRDS(
  nb_superiority_final,
  file.path(DIR_OUTPUTS, "nb_fit_superiority_only_final.rds")
)

# Labels
term_labels <- c(
  "c_participants.factorCardiovascular" = "Population: Cardiovascular",
  "c_participants.factorObstetric"      = "Population: Obstetric",
  "c_participants.factorPediatric"      = "Population: Pediatric",
  "c_participants.factorRegional"       = "Population: Regional",
  
  "c_design_collapsed.factorTwo-by-two factorial trial" = "Design: 2×2 factorial",
  "c_design_collapsed.factorOther" = "Design: Other",
  
  "i_type.factorNon-drug related" = "Intervention: Non-drug related",
  
  "rct_blind_d.factorYes" = "Blinding: Yes",
  "rct_conceal_d.factorYes" = "Allocation Concealment: Yes",
  
  "rct_centers.factorSingle-center" = "Single-center",
  "rct_centers.factorUnclear" = "Centers: Unreported",
  
  "funding.factorNon-industry funded" = "Funding: Non-industry funded",
  "funding.factorNot reported" = "Funding: Not reported",
  
  "d_share.factorUnclear" = "No data-sharing statement",
  "d_share.factorYes" = "Data-sharing statement",
  
  "sample_size100" = "Sample size, per 100 participants",
  "ltfu" = "Lost to follow-up"
)

label_term <- function(term) {
  if (term %in% names(term_labels)) term_labels[[term]] else term
}

# Build forest data
forest_data_final <- broom::tidy(
  nb_superiority_final,
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(
    label = vapply(term, label_term, character(1))
  ) %>%
  # Omit sparse category from figure only
  dplyr::filter(label != "Centers: Unreported") %>%
  dplyr::transmute(
    term,
    label,
    IRR = estimate,
    lowerCI = conf.low,
    upperCI = conf.high,
    p.value = p.value
  ) %>%
  dplyr::mutate(
    direction = dplyr::case_when(
      IRR > 1 ~ "IRR > 1",
      IRR < 1 ~ "IRR < 1",
      TRUE ~ "IRR = 1"
    ),
    IRR_txt = sprintf("%.2f", IRR),
    CI_txt = paste0(
      "[",
      sprintf("%.2f", lowerCI),
      ", ",
      sprintf("%.2f", upperCI),
      "]"
    ),
    p_txt = dplyr::case_when(
      is.na(p.value) ~ "",
      p.value < 0.001 ~ "<0.001",
      TRUE ~ formatC(p.value, format = "f", digits = 3)
    )
  ) %>%
  dplyr::arrange(desc(IRR)) %>%
  dplyr::mutate(
    label_factor = factor(label, levels = rev(label), ordered = TRUE)
  )

readr::write_csv(
  forest_data_final,
  file.path(DIR_OUTPUTS, "Figure3_ForestPlot_superiority_final_data.csv")
)

# Color map
col_map <- c(
  "IRR > 1" = "#2C7BE5",
  "IRR < 1" = "#D94841",
  "IRR = 1" = "grey30"
)

x_max <- max(forest_data_final$upperCI, na.rm = TRUE)

# Forest plot
p_forest_left_final <- ggplot(
  forest_data_final,
  aes(x = IRR, y = label_factor)
) +
  geom_errorbar(
    aes(xmin = lowerCI, xmax = upperCI, colour = direction),
    linewidth = 0.9,
    width = 0.20
  ) +
  geom_point(
    aes(colour = direction),
    size = 3
  ) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    linewidth = 0.8,
    colour = "grey35"
  ) +
  scale_colour_manual(values = col_map, guide = "none") +
  scale_x_continuous(
    limits = c(0, x_max * 1.05),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    x = "Incidence Rate Ratio (IRR)",
    y = "Trial characteristics"
  ) +
  theme_minimal(base_size = 12.5) +
  theme(
    axis.text.y = element_text(size = 11),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 6, 10, 10)
  )

# Right-side table
p_table_right_final <- ggplot(
  forest_data_final,
  aes(y = label_factor)
) +
  geom_text(aes(x = 1, label = IRR_txt), hjust = 0, size = 3.6) +
  geom_text(aes(x = 2, label = CI_txt), hjust = 0, size = 3.6) +
  geom_text(aes(x = 3, label = p_txt), hjust = 0, size = 3.6) +
  annotate("text", x = 1, y = Inf, label = "IRR",
           vjust = 1.2, hjust = 0, fontface = "bold", size = 3.8) +
  annotate("text", x = 2, y = Inf, label = "95% CI",
           vjust = 1.2, hjust = 0, fontface = "bold", size = 3.8) +
  annotate("text", x = 3, y = Inf, label = "p-value",
           vjust = 1.2, hjust = 0, fontface = "bold", size = 3.8) +
  scale_x_continuous(
    limits = c(0.9, 3.9),
    expand = c(0, 0)
  ) +
  theme_void(base_size = 12.5) +
  theme(
    plot.margin = margin(10, 25, 10, 0)
  ) +
  coord_cartesian(clip = "off")

# Combine
p_forest_final <- p_forest_left_final + p_table_right_final +
  patchwork::plot_layout(widths = c(2.5, 1.7))

# Export
ggsave(
  file.path(DIR_FIGURES, "Figure3_ForestPlot_superiority_final.png"),
  p_forest_final,
  width = 13,
  height = 8,
  dpi = 600,
  bg = "white"
)

ggsave(
  file.path(DIR_FIGURES, "Figure3_ForestPlot_superiority_final.tiff"),
  p_forest_final,
  width = 13,
  height = 8,
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

writeLines(
  "Note: The vertical dashed line indicates IRR = 1 (no association).",
  con = file.path(DIR_OUTPUTS, "Figure3_caption_note_superiority_final.txt")
)

message("Final Figure 3 saved.")