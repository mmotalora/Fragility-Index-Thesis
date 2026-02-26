# =====================================================================
# 06_figures.R  (REVISED FINAL)
# Figures for manuscript + optional presentation plots
#
# Figure 1: PRISMA2020 diagram (preferred style) + reviewer wording fix
#   - automatically replaces "new studies" -> "clinical guidelines"
#
# Figure 3: Forest plot (IRR) + aligned table
#   - colored points/CI by IRR direction
#   - robust labels from factor levels in no_na_data.rds
#   - dashed line explained (IRR=1)
#
# Requires:
#  - outputs/fi_set_2026-02.rds
#  - outputs/final_nb_model_2026-02.rds
#  - outputs/no_na_data.rds   (for correct factor labels)
#  - PRISMA.csv  (in project root or adjust path)
# =====================================================================

source("00_utils.R")
require_pkgs(c(
  "dplyr","tidyr","ggplot2","scales","broom","cowplot","gridExtra","forcats",
  "tibble","stringr","readr","PRISMA2020","DiagrammeRsvg","magick"
))

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(broom)
library(cowplot)
library(gridExtra)
library(forcats)
library(tibble)
library(stringr)
library(readr)
library(PRISMA2020)
library(DiagrammeRsvg)
library(magick)

DIR_OUTPUTS <- if (exists("DIR_OUTPUTS")) DIR_OUTPUTS else "outputs"
DIR_FIGURES <- if (exists("DIR_FIGURES")) DIR_FIGURES else "figures"
ensure_dir(DIR_OUTPUTS)
ensure_dir(DIR_FIGURES)

FI_SET_RDS <- file.path(DIR_OUTPUTS, "fi_set_2026-02.rds")
MODEL_RDS  <- file.path(DIR_OUTPUTS, "final_nb_model_2026-02.rds")
NONA_RDS   <- file.path(DIR_OUTPUTS, "no_na_data.rds")

if (!file.exists(FI_SET_RDS)) stop("Missing outputs/fi_set_2026-02.rds. Run 04_compute_fragility.R first.", call. = FALSE)
if (!file.exists(MODEL_RDS))  stop("Missing outputs/final_nb_model_2026-02.rds. Run 05_analysis.R first.", call. = FALSE)

fi_set <- readRDS(FI_SET_RDS)
final_model <- readRDS(MODEL_RDS)

no_na_data <- NULL
if (file.exists(NONA_RDS)) {
  no_na_data <- readRDS(NONA_RDS)
}

# =====================================================================
# FIGURE 1 — PRISMA2020 flow diagram
# Fix: replace template wording in SVG + rasterize with rsvg (keeps side labels)
# =====================================================================

PRISMA_CSV <- "PRISMA.csv"

if (!file.exists(PRISMA_CSV)) {
  warning("PRISMA.csv not found in working directory. Figure 1 will be skipped.")
} else {
  
  data1 <- read.csv(PRISMA_CSV, sep = ";", stringsAsFactors = FALSE)
  
  # Optional: replace any CSV text fields too
  data1 <- data1 %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character),
        ~ stringr::str_replace_all(
          .,
          stringr::regex("new\\s+studies", ignore_case = TRUE),
          "clinical guidelines"
        )
      )
    )
  
  data1 <- PRISMA2020::PRISMA_data(data1)
  
  prisma <- PRISMA2020::PRISMA_flowdiagram(
    data = data1,
    interactive = FALSE,
    previous = FALSE,
    other = TRUE,
    detail_databases = TRUE,
    detail_registers = FALSE,
    fontsize = 10,
    font = "Helvetica",
    title_colour = "Goldenrod1",
    greybox_colour = "Gainsboro",
    main_colour = "Black",
    arrow_colour = "Black",
    arrow_head = "normal",
    arrow_tail = "none",
    side_boxes = TRUE
  )
  
  # Export SVG text
  svg_txt <- DiagrammeRsvg::export_svg(prisma)
  
  # ✅ Replace template wording inside SVG (this is what actually changes the labels)
  svg_txt <- stringr::str_replace_all(
    svg_txt,
    stringr::regex("new\\s+studies", ignore_case = TRUE),
    "clinical guidelines"
  )
  
  # Save SVG (useful for debugging + lets rsvg rasterize from file)
  svg_file <- file.path(DIR_FIGURES, "Figure1_PRISMA.svg")
  writeLines(svg_txt, con = svg_file, useBytes = TRUE)
  
  # Rasterize with rsvg to PNG (preserves rotated side labels)
  png_file <- file.path(DIR_FIGURES, "Figure1_PRISMA.png")
  rsvg::rsvg_png(svg_file, file = png_file, width = 3600, height = 2400)
  
  # Convert PNG -> TIFF using magick
  tiff_file <- file.path(DIR_FIGURES, "Figure1_PRISMA.tiff")
  img <- magick::image_read(png_file)
  magick::image_write(img, path = tiff_file, format = "tiff",
                      compression = "lzw")
}

# =====================================================================
# Helper: robust term labeling from model terms + factor levels
# =====================================================================

pretty_var <- function(v) {
  # Friendly variable prefixes
  d <- c(
    "c_participants.factor" = "Population",
    "c_design_reg.factor"   = "Design",
    "rct_type.factor"       = "Trial type",
    "i_type.factor"         = "Intervention",
    "rct_blind_d.factor"    = "Blinding",
    "rct_conceal_d.factor"  = "Allocation concealment",
    "rct_centers.factor"    = "Centers",
    "ethic.factor"          = "Ethics statement",
    "funding.factor"        = "Funding",
    "d_share.factor"        = "Data sharing",
    "pval_significance"     = "Statistical significance",
    "sample_size"           = "Sample size",
    "ltfu"                  = "Lost to follow-up",
    "moved"                 = "Protocol deviations"
  )
  if (!is.null(d[[v]])) d[[v]] else v
}

get_level_label <- function(var, lvl, data_for_levels = NULL) {
  # If we have the analysis dataset, match to actual factor levels exactly
  if (!is.null(data_for_levels) && var %in% names(data_for_levels) && is.factor(data_for_levels[[var]])) {
    # lvl comes from model term encoding; it should match the factor level text
    return(lvl)
  }
  # Fallback: just return lvl
  lvl
}

label_term <- function(term, data_for_levels = NULL) {
  # numeric terms
  if (term %in% c("sample_size","ltfu","moved")) return(pretty_var(term))
  
  # factors: detect which variable the term belongs to
  vars <- c(
    "c_participants.factor","c_design_reg.factor","rct_type.factor","i_type.factor",
    "rct_blind_d.factor","rct_conceal_d.factor","rct_centers.factor","ethic.factor",
    "funding.factor","d_share.factor","pval_significance"
  )
  
  hit <- vars[startsWith(term, vars)]
  if (length(hit) == 0) return(term)
  
  var <- hit[1]
  lvl <- sub(paste0("^", var), "", term)
  
  # Clean up any odd encodings
  lvl <- gsub("\\.", " ", lvl)
  
  # Special wording tweaks (match your manuscript language)
  if (var == "c_design_reg.factor" && lvl == "Other") {
    lvl <- "Other (incl. crossover)"
  }
  if (var == "rct_centers.factor" && tolower(lvl) == "unclear") {
    lvl <- "Unreported"
  }
  if (var == "d_share.factor" && tolower(lvl) == "unclear") {
    lvl <- "Unreported"
  }
  if (var == "rct_conceal_d.factor" && tolower(lvl) == "unclear") {
    lvl <- "Unclear/No"
  }
  if (var == "pval_significance" && lvl == "Significant") {
    lvl <- "Yes"
  }
  
  paste0(pretty_var(var), ": ", get_level_label(var, lvl, data_for_levels))
}


# =====================================================================
# FIGURE 3 — Forest plot (IRR) + aligned table, COLOURED
# Reviewer: x-axis label + dashed line meaning (handled via caption note)
# =====================================================================

# ---------- Term → human label mapping ----------
# Use the exact coefficients your model prints (term names from broom::tidy)
term_labels <- c(
  # Participants (reference category is whatever glm chose; labels only for non-ref)
  "c_participants.factorCardiovascular" = "Population: Cardiovascular",
  "c_participants.factorObstetric"      = "Population: Obstetric",
  "c_participants.factorPediatric"      = "Population: Pediatric",
  "c_participants.factorRegional"       = "Population: Regional",
  
  # Design (regression-only collapsed design)
  "c_design_reg.factorParallel-arm trial" = "Design: Parallel",
  "c_design_reg.factorTwo-by-two factorial trial" = "Design: 2×2 factorial",
  "c_design_reg.factorOther"                      = "Design: Other (incl. crossover)",
  
  # Trial type
  "rct_type.factorSuperiority trial"  = "Trial type: Superiority trial",
  "rct_type.factorEquivalence trial"  = "Trial type: Equivalence trial",
  "rct_type.factorNon-inferiority trial" = "Trial type: Non-inferiority trial",
  
  # Intervention type
  "i_type.factorNon-drug related" = "Intervention: Non-drug related",
  
  # Blinding / concealment (dichotomous)
  "rct_blind_d.factorNo"            = "Blinding: Open-label",
  "rct_conceal_d.factorUnclear"     = "Allocation concealment: Unclear/No",
  
  # Centers
  "rct_centers.factorSingle-center" = "Centers: Single-center",
  "rct_centers.factorMulticenter"   = "Centers: Multicenter",
  "rct_centers.factorUnclear"       = "Centers: Unreported",
  
  # Ethics
  "ethic.factorYes" = "Ethics statement: Yes",
  "ethic.factorNo"  = "Ethics statement: No/Unreported",
  
  # Funding
  "funding.factorNon-industry funded" = "Funding: Non-industry funded",
  "funding.factorIndustry-funded"     = "Funding: Industry-funded",
  "funding.factorNot reported"        = "Funding: Not reported",
  
  # Data sharing
  "d_share.factorYes"     = "Data sharing: Yes",
  "d_share.factorNo"      = "Data sharing: No",
  "d_share.factorUnclear" = "Data sharing: Unreported",
  
  # P value significance
  "pval_significanceSignificant" = "Statistical significance: Yes",
  
  # Continuous
  "sample_size" = "Sample size",
  "ltfu"        = "Lost to follow-up",
  "moved"       = "Protocol deviations"
)

label_term <- function(term) {
  if (term %in% names(term_labels)) return(term_labels[[term]])
  return(term) # fallback (never crashes)
}

# ---------- Build forest dataset ----------
coef_summary <- broom::tidy(final_model, conf.int = TRUE, exponentiate = TRUE)

forest_data <- coef_summary %>%
  filter(term != "(Intercept)") %>%
  transmute(
    term,
    label   = vapply(term, label_term, character(1)),
    IRR     = estimate,
    lowerCI = conf.low,
    upperCI = conf.high,
    p.value = p.value
  ) %>%
  mutate(
    direction = case_when(
      IRR > 1 ~ "IRR > 1",
      IRR < 1 ~ "IRR < 1",
      TRUE    ~ "IRR = 1"
    ),
    IRR_txt = sprintf("%.2f", IRR),
    CI_txt  = paste0("[", sprintf("%.2f", lowerCI), ", ", sprintf("%.2f", upperCI), "]"),
    p_txt   = ifelse(is.na(p.value), "", formatC(p.value, format = "f", digits = 3))
  ) %>%
  arrange(desc(IRR)) %>%
  mutate(label_factor = factor(label, levels = rev(label), ordered = TRUE))

# Two-tone color map
col_map <- c("IRR > 1" = "#2C7BE5", "IRR < 1" = "#D94841", "IRR = 1" = "grey30")

# --- LEFT: forest only (tight x-scale so it doesn't get squished)
x_min <- min(forest_data$lowerCI, na.rm = TRUE)
x_max <- max(forest_data$upperCI, na.rm = TRUE)

p_forest_left <- ggplot(forest_data, aes(x = IRR, y = label_factor)) +
  geom_errorbar(aes(xmin = lowerCI, xmax = upperCI, colour = direction),
                linewidth = 0.9, width = 0.20) +
  geom_point(aes(colour = direction), size = 3) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.8, colour = "grey35") +
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
    axis.text.y  = element_text(size = 11),
    plot.margin  = margin(10, 6, 10, 10)
  )

# --- RIGHT: “table” only (own x-scale, so it doesn't distort the forest)
# Use a simple 3-column coordinate system: 1, 2, 3
x_IRR <- 1
x_CI  <- 2
x_p   <- 3

p_table_right <- ggplot(forest_data, aes(y = label_factor)) +
  geom_text(aes(x = x_IRR, label = IRR_txt), hjust = 0, size = 3.6) +
  geom_text(aes(x = x_CI,  label = CI_txt),  hjust = 0, size = 3.6) +
  geom_text(aes(x = x_p,   label = p_txt),   hjust = 0, size = 3.6) +
  annotate("text", x = x_IRR, y = Inf, label = "IRR",     vjust = 1.2, hjust = 0,
           fontface = "bold", size = 3.8) +
  annotate("text", x = x_CI,  y = Inf, label = "95% CI",  vjust = 1.2, hjust = 0,
           fontface = "bold", size = 3.8) +
  annotate("text", x = x_p,   y = Inf, label = "p-value", vjust = 1.2, hjust = 0,
           fontface = "bold", size = 3.8) +
  scale_x_continuous(limits = c(0.9, 3.9), expand = c(0, 0)) +
  theme_void(base_size = 12.5) +
  theme(
    plot.margin = margin(10, 25, 10, 0)
  ) +
  coord_cartesian(clip = "off")

# --- Combine with fixed widths (controls forest vs table size)
# install.packages("patchwork") if needed
p_forest <- p_forest_left + p_table_right +
  patchwork::plot_layout(widths = c(2.5, 1.7))

ggsave(file.path(DIR_FIGURES, "Figure3_ForestPlot.png"),
       p_forest, width = 13, height = 8, dpi = 600)
ggsave(file.path(DIR_FIGURES, "Figure3_ForestPlot.tiff"),
       p_forest, width = 13, height = 8, dpi = 600)

writeLines(
  "Note: The vertical dashed line indicates IRR = 1 (no association).",
  con = file.path(DIR_OUTPUTS, "Figure3_caption_note.txt")
)

# =====================================================================
# Radar plot (optional manuscript / slides) — unchanged logic, cleaned labels
# =====================================================================

use_fixed_threshold <- TRUE
fi_cut <- 4
ylim_max <- 60
radial_breaks <- c(0, 20, 40, 60)
wrap_labels <- TRUE

fi_set2 <- fi_set %>%
  mutate(
    FI_group = if (use_fixed_threshold) {
      ifelse(fi_each > fi_cut, paste0("FI > ", fi_cut), paste0("FI ≤ ", fi_cut))
    } else {
      ifelse(fi_each > median(fi_each, na.rm = TRUE), "FI > median", "FI ≤ median")
    },
    rct_blind_d.factor = case_when(
      rct_blind.factor %in% c("Single blind","Double blind","Triple blind") ~ "Yes",
      rct_blind.factor %in% c("Unblinded") ~ "No",
      TRUE ~ NA_character_
    ),
    rct_conceal_d.factor = case_when(
      rct_conceal.factor %in% c(
        "Sequentially numbered sealed/opaque envelopes",
        "Sequentially numbered containers",
        "Pharmacy controlled allocation",
        "Central allocation (site remote from trial location)",
        "Other"
      ) ~ "Yes",
      rct_conceal.factor %in% "Unclear" ~ "Unclear",
      TRUE ~ NA_character_
    )
  )

risk_by_FI <- fi_set2 %>%
  group_by(FI_group) %>%
  summarise(
    `No blinding`                = mean(rct_blind_d.factor == "No",        na.rm = TRUE) * 100,
    `No concealment`             = mean(rct_conceal_d.factor == "Unclear", na.rm = TRUE) * 100,
    `No sample size calculation` = mean(sample_calc.factor == "No",        na.rm = TRUE) * 100,
    `No data sharing`            = mean(d_share.factor == "No",            na.rm = TRUE) * 100,
    `Funding not reported`       = mean(funding.factor == "Not reported",  na.rm = TRUE) * 100,
    .groups = "drop"
  )

axes <- c("No blinding","No concealment","No sample size calculation","No data sharing","Funding not reported")

dat_long <- risk_by_FI %>%
  pivot_longer(-FI_group, names_to = "axis", values_to = "value") %>%
  mutate(
    axis = factor(axis, levels = axes),
    value = ifelse(is.na(value) | is.nan(value), 0, value)
  ) %>%
  arrange(FI_group, axis)

k <- length(axes)
theta <- seq(-pi/2, 3*pi/2, length.out = k + 1)[1:k]
angle_df <- tibble(axis = factor(axes, levels = axes), theta = theta)

dat_cart <- dat_long %>%
  left_join(angle_df, by = "axis") %>%
  mutate(
    r = pmin(value, ylim_max),
    x = r * cos(theta),
    y = r * sin(theta)
  )

dat_poly <- dat_cart %>%
  group_by(FI_group) %>%
  bind_rows(slice_head(., n = 1)) %>%
  ungroup()

grid_rings <- do.call(rbind, lapply(radial_breaks, function(br) {
  df <- data.frame(axis = factor(axes, levels = axes))
  df <- merge(df, angle_df, by = "axis", sort = FALSE)
  df$r <- br
  df$x <- df$r * cos(df$theta)
  df$y <- df$r * sin(df$theta)
  df$ring <- br
  rbind(df, df[1,])
}))

spokes <- angle_df %>%
  mutate(x1 = ylim_max * cos(theta),
         y1 = ylim_max * sin(theta))

label_radius <- ylim_max * 1.12
labels <- angle_df %>%
  mutate(
    lab = as.character(axis),
    lab = if (wrap_labels) case_when(
      lab == "No sample size calculation" ~ "No sample size\ncalculation",
      lab == "Funding not reported"       ~ "Funding not\nreported",
      TRUE ~ lab
    ) else lab,
    x = label_radius * cos(theta),
    y = label_radius * sin(theta),
    hjust = case_when(cos(theta) >  0.15 ~ 0, cos(theta) < -0.15 ~ 1, TRUE ~ 0.5),
    vjust = case_when(sin(theta) >  0.15 ~ 0, sin(theta) < -0.15 ~ 1, TRUE ~ 0.5)
  )

xlim <- range(c(-ylim_max, ylim_max, labels$x)) * 1.05
ylim <- range(c(-ylim_max, ylim_max, labels$y)) * 1.05

p_radar <- ggplot() +
  geom_polygon(data = grid_rings,
               aes(x = x, y = y, group = ring),
               fill = NA, color = "grey80", linewidth = 0.4, linetype = "dashed") +
  geom_segment(data = spokes, aes(x = 0, y = 0, xend = x1, yend = y1),
               color = "grey75", linewidth = 0.4, linetype = "dashed") +
  geom_polygon(data = dat_poly,
               aes(x = x, y = y, group = FI_group, fill = FI_group),
               alpha = 0.22, color = NA) +
  geom_polygon(data = dat_poly,
               aes(x = x, y = y, group = FI_group, color = FI_group),
               fill = NA, linewidth = 0.9) +
  geom_point(data = dat_cart, aes(x = x, y = y, color = FI_group), size = 2) +
  geom_text(data = labels,
            aes(x = x, y = y, label = lab, hjust = hjust, vjust = vjust),
            size = 4.1, color = "grey35", lineheight = 1.05) +
  scale_x_continuous(limits = xlim, expand = expansion(mult = 0.02)) +
  scale_y_continuous(limits = ylim, expand = expansion(mult = 0.02)) +
  coord_equal(clip = "off") +
  theme_void(base_size = 13) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    plot.margin = unit(c(20, 40, 20, 40), "pt")
  )

ggsave(file.path(DIR_FIGURES, "Radar_FI.png"),  p_radar, width = 7.5, height = 5.2, dpi = 300)
ggsave(file.path(DIR_FIGURES, "Radar_FI.tiff"), p_radar, width = 6,   height = 4, units = "in", dpi = 600, compression = "lzw")

message("06_figures.R complete: PRISMA + Forest plot + Radar saved to /figures; caption note in /outputs.")