library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(broom)
library(cowplot)
library(gridExtra)
###############################################################################
#PRISMA 
###############################################################################
library(PRISMA2020)

data1 <- read.csv("PRISMA.csv", sep = ";") 
data1 <- PRISMA_data(data1)

PRISMA_flowdiagram(
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
  side_boxes = TRUE)

###############################################################################
# FOREST PLOT 
###############################################################################
coef_summary2 <- tidy(nb12, conf.int = TRUE, exp = TRUE)

forest_data2 <- coef_summary2 %>%
  filter(term != "(Intercept)") %>%
  rename(
    Variable = term,
    IRR      = estimate,
    lowerCI  = conf.low,
    upperCI  = conf.high
  )

forest_data2 <- forest_data2 %>%
  arrange(desc(IRR)) %>%
  mutate(
    VarFactor = factor(
      Variable,
      levels = rev(Variable),  # reversed factor levels
      ordered = TRUE
    )
  )

forest_data2


#labels 

variable_labels <- c(
  "c_design.factorTwo-by-two factorial trial" = "Design: 2x2 factorial",
  "c_design.factorCrossover"                 = "Design: Crossover",
  "rct_centers.factorMulticenter"            = "Number of centers: Multicenter",
  "pval_significanceSignificant"             = "Statistical significance: Yes",
  "d_share.factorYes"                        = "DSA: yes",
  "rct_conceal_d.factorUnclear"              = "Allocation concealment: Unclear",
  "rct_centers.factorUnclear"                = "Number of centers: Unreported",
  "funding.factorNon-industry funded"        = "Funding: Non-industry",
  "d_share.factorUnclear"                    = "DSA: Unreported",
  "moved"                                    = "Protocol Deviations",
  "ltfu"                                     = "Lost to Follow-up",
  "sample_size"                              = "Sample Size",
  "year_group2008 or Later"                  = "Year: 2008 or later",
  "i_type.factorNon-drug related"            = "Intervention: Non-drug related",
  "rct_blind_d.factorNo"                     = "Open-label",
  "c_participants.factorCardiovascular"      = "Population: Cardiovascular",
  "funding.factorIndustry-funded"            = "Funding: Industry",
  "rct_type.factorEquivalence trial"         = "Equivalence trial",
  "c_participants.factorAdults"              = "Population: General",
  "c_participants.factorRegional"            = "Population: Regional",
  "c_participants.factorPediatric"           = "Population: Pediatric",
  "ethic.factorNo"                           = "Ethics statement: Unreported"
)


#Forest plot referencing VarFactor

forest_plot2 <- ggplot(forest_data2, aes(x = IRR, y = VarFactor)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = lowerCI, xmax = upperCI), 
                 height = 0.2, color = "black") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(
    x     = "Incidence Rate Ratio (95% CI)",
    y     = "Trial Characteristics",
    title = " 
      
    "
  ) +
  scale_y_discrete(labels = variable_labels) +
  theme_minimal(base_size = 12.5) +
  theme(
    plot.title.position = "plot",
    axis.text.y = element_text(size = 11)
  )

forest_plot2

# Table with IRR, 95% CI, p-value 
table_data2 <- forest_data2 %>%
  mutate(
    IRR    = round(IRR, 2),
    CI     = paste0("[", round(lowerCI, 2), ", ", round(upperCI, 2), "]"),
    p_val  = sprintf("%.2f", p.value)  
  ) %>%
  select(IRR, CI, p_val)

colnames(table_data2) <- c("IRR", "95% CI", "p-value")

# ttheme with smaller font & minimal row padding
my_theme <- ttheme_minimal(
  base_size   = 11,               
  base_family = "Arial",
  padding = unit(c(4, 3.2), "mm"),  
  core = list(
    fg_params = list(hjust = 0, x = 0.1, fontsize = 10)
  )
)

table_plot2 <- tableGrob(table_data2, rows = NULL, theme = my_theme)

# Combined forest plot and table
combined_plot2 <- plot_grid(
  forest_plot2,
  table_plot2,
  align      = "h",
  axis       = "tb",
  rel_widths = c(2.3, 1.0)  
)

combined_plot2

ggsave("final_forest_plot_with_table.png", 
       combined_plot, width = 12, height = 8, dpi = 300)

###############################################################################
# Radar plot
###############################################################################
# ============================
# Straight-edge Radar (Cartesian) with your labels
# ============================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# ----------------------------
# 0) OPTIONS
# ----------------------------
use_fixed_threshold <- TRUE   # TRUE: FI > fi_cut vs ≤ fi_cut; FALSE: median split
fi_cut <- 4                   # cutoff when use_fixed_threshold = TRUE
ylim_max <- 60                # outer ring (like your slide)
radial_breaks <- c(0, 20, 40, 60)
wrap_labels <- TRUE           # wrap the two long labels onto 2 lines

# ----------------------------
# 1) Grouping + dichotomies (starts from fi_set in your environment)
# ----------------------------
fi_set2 <- fi_set %>%
  mutate(
    FI_group = if (use_fixed_threshold) {
      ifelse(fi_each > fi_cut, "FI > 4", "FI ≤ 4")
    } else {
      ifelse(fi_each > median(fi_each, na.rm = TRUE), "FI > median", "FI ≤ median")
    },
    # Blinding (Yes/No)
    rct_blind_d.factor = dplyr::case_when(
      rct_blind.factor %in% c("Single blind","Double blind","Triple blind") ~ "Yes",
      rct_blind.factor %in% c("Unblinded") ~ "No",
      TRUE ~ NA_character_
    ),
    # Concealment (Yes vs Unclear)
    rct_conceal_d.factor = dplyr::case_when(
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

# ----------------------------
# 2) Summaries (percent) for each axis
# ----------------------------
risk_by_FI <- fi_set2 %>%
  group_by(FI_group) %>%
  summarise(
    `No blinding`                 = mean(rct_blind_d.factor == "No",        na.rm = TRUE) * 100,
    `No concealment`              = mean(rct_conceal_d.factor == "Unclear", na.rm = TRUE) * 100,
    `No sample size calculation`  = mean(sample_calc.factor == "No",        na.rm = TRUE) * 100,
    `No DSA`                      = mean(d_share.factor == "No",            na.rm = TRUE) * 100,
    `Not-reported Funding`        = mean(funding.factor == "Not reported",  na.rm = TRUE) * 100,
    .groups = "drop"
  )

# ----------------------------
# 3) Long format + axis order (your exact labels)
# ----------------------------
axes <- c("No blinding",
          "No concealment",
          "No sample size calculation",
          "No DSA",
          "Not-reported Funding")

dat_long <- risk_by_FI %>%
  pivot_longer(-FI_group, names_to = "axis", values_to = "value") %>%
  mutate(
    axis  = factor(axis, levels = axes),
    value = ifelse(is.na(value) | is.nan(value), 0, value)
  ) %>%
  arrange(FI_group, axis)

# ----------------------------
# 4) Convert to Cartesian (straight edges)
# ----------------------------
k <- length(axes)
theta <- seq(-pi/2, 3*pi/2, length.out = k + 1)[1:k]   # start at top, clockwise
angle_df <- tibble(axis = factor(axes, levels = axes), theta = theta)

dat_cart <- dat_long %>%
  left_join(angle_df, by = "axis") %>%
  mutate(
    r = pmin(value, ylim_max),
    x = r * cos(theta),
    y = r * sin(theta)
  )

# Close polygons by repeating first row per group
dat_poly <- dat_cart %>%
  group_by(FI_group) %>%
  bind_rows(slice_head(., n = 1)) %>%
  ungroup()

# Grid rings and spokes
grid_rings <- do.call(rbind, lapply(radial_breaks, function(br) {
  df <- data.frame(axis = factor(axes, levels = axes))
  df <- merge(df, angle_df, by = "axis", sort = FALSE)
  df$r <- br
  df$x <- df$r * cos(df$theta)
  df$y <- df$r * sin(df$theta)
  df$ring <- br
  rbind(df, df[1,])  # close
}))

spokes <- angle_df %>%
  mutate(x1 = ylim_max * cos(theta),
         y1 = ylim_max * sin(theta))

# ----------------------------
# 5) Labels: push outward + smart alignment
# ----------------------------
label_radius <- ylim_max * 1.12
labels <- angle_df %>%
  mutate(
    lab = as.character(axis),
    lab = if (wrap_labels) dplyr::case_when(
      lab == "No sample size calculation" ~ "No sample size\ncalculation",
      lab == "Not-reported Funding"       ~ "Not-reported\nFunding",
      TRUE ~ lab
    ) else lab,
    x = label_radius * cos(theta),
    y = label_radius * sin(theta),
    hjust = dplyr::case_when(
      cos(theta) >  0.15 ~ 0,
      cos(theta) < -0.15 ~ 1,
      TRUE ~ 0.5
    ),
    vjust = dplyr::case_when(
      sin(theta) >  0.15 ~ 0,
      sin(theta) < -0.15 ~ 1,
      TRUE ~ 0.5
    )
  )

# Limits so labels are fully visible
xlim <- range(c(-ylim_max, ylim_max, labels$x)) * 1.05
ylim <- range(c(-ylim_max, ylim_max, labels$y)) * 1.05

# ----------------------------
# 6) Colors + Plot
# ----------------------------
col_dark  <- "#0B3C5D"   # FI > 4
col_light <- "#8FB0FF"   # FI ≤ 4

p <- ggplot() +
  # rings
  geom_polygon(data = grid_rings,
               aes(x = x, y = y, group = ring),
               fill = NA, color = "grey80", linewidth = 0.4, linetype = "dashed") +
  # spokes
  geom_segment(data = spokes, aes(x = 0, y = 0, xend = x1, yend = y1),
               color = "grey75", linewidth = 0.4, linetype = "dashed") +
  # polygons (fill + border)
  geom_polygon(data = dat_poly,
               aes(x = x, y = y, group = FI_group, fill = FI_group),
               alpha = 0.25, color = NA) +
  geom_polygon(data = dat_poly,
               aes(x = x, y = y, group = FI_group, color = FI_group),
               fill = NA, linewidth = 0.9) +
  geom_point(data = dat_cart, aes(x = x, y = y, color = FI_group), size = 2) +
  # labels
  geom_text(data = labels,
            aes(x = x, y = y, label = lab, hjust = hjust, vjust = vjust),
            size = 4.2, color = "grey35", lineheight = 1.05) +
  # keep everything in view
  scale_x_continuous(limits = xlim, expand = expansion(mult = 0.02)) +
  scale_y_continuous(limits = ylim, expand = expansion(mult = 0.02)) +
  coord_equal(clip = "off") +
  scale_color_manual(NULL, values = c("FI > 4" = col_dark, "FI ≤ 4" = col_light)) +
  scale_fill_manual(NULL,  values = c("FI > 4" = col_dark, "FI ≤ 4" = col_light)) +
  theme_void(base_size = 13) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    plot.margin = grid::unit(c(20, 40, 20, 40), "pt")
  )

print(p)

ggsave("radar_FI_straight_labels_fixed.png", p, width = 7.5, height = 5.2, dpi = 300)

###############################################################################
# Plots for oral presentation 
###############################################################################

# data by statistical significance 
df <- fi_set %>%
  mutate(
    group = ifelse(!is.na(pval) & pval < 0.05, "Significant", "Non-significant"),
    fi_each = as.numeric(fi_each),
    fq = as.numeric(fq)
  ) %>%
  filter(!is.na(fi_each), !is.na(fq)) %>%
  mutate(
    fi_each = ifelse(fi_each < 0, NA, fi_each),
    fq = ifelse(fq < 0, NA, fq)
  ) %>%
  filter(!is.na(fi_each), !is.na(fq))

df <- df %>% mutate(fq_pct = fq * 100)

# Quick summaries to confirm medians/IQRs 
summary_table <- df %>%
  group_by(group) %>%
  summarise(
    n = n(),
    FI_median = median(fi_each),
    FI_IQR_low = quantile(fi_each, 0.25),
    FI_IQR_high = quantile(fi_each, 0.75),
    FI_min = min(fi_each),
    FI_max = max(fi_each),
    FQ_median = median(fq,
    FQ_IQR_low = quantile(fq 0.25),
    FQ_IQR_high = quantile(fq, 0.75),
    FQ_min = min(fq),
    FQ_max = max(fq),
    .groups = "drop"
  ))
print(summary_table)

# Faceted plots (FI + FQ) ---
long_df <- df %>%
  select(group, FI = fi_each, FQ = fq) %>%
  pivot_longer(cols = c(FI, FQ), names_to = "Metric", values_to = "Value")

# Boxplot (faceted FI & FQ) ---
p_box <- ggplot(long_df, aes(x = group, y = Value, fill = group)) +
  geom_boxplot(width = 0.6, alpha = 0.7, outlier.shape = 16, outlier.size = 1.5) +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(
    title = "Fragility Index (FI) and Fragility Quotient (FQ)",
    x = "Study group",
    y = "Value"
  ) +
  scale_fill_manual(values = c("Significant" = "#2C7BE5", "Non-significant" = "#6C757D")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10))
  )
print(p_box)

ggsave("boxplot_FI_FQ.png", p_box, width = 9, height = 5.5, dpi = 300)

# Violin + embedded boxplot (faceted FI & FQ) ---
p_violin <- ggplot(long_df, aes(x = group, y = Value, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.12, fill = "white", outlier.shape = 16) +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(
    title = "Distribution of FI and FQ by Significance",
    x = "Study group",
    y = "Value"
  ) +
  scale_fill_manual(values = c("Significant" = "#2C7BE5", "Non-significant" = "#6C757D")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10))
  )
print(p_violin)

ggsave("violin_FI_FQ.png", p_violin, width = 9, height = 5.5, dpi = 300)


df_base <- fi_set %>%
  dplyr::mutate(
    n_total = as.numeric(i_total) + as.numeric(c_total),
    fi_each = as.numeric(fi_each),
    fq      = as.numeric(fq)
  ) %>%
  dplyr::filter(!is.na(n_total), n_total > 0, !is.na(fi_each), !is.na(fq)) %>%
  # If fq is a proportion (0–1), convert to %
  dplyr::mutate(fq_pct = pmin(pmax(fq * 100, 0), 100))

# Robust quartiles of N 
# Use quantiles to define breaks; 'type = 2' is robust for discrete N
q_breaks <- quantile(df_base$n_total, probs = c(0, .25, .5, .75, 1),
                     na.rm = TRUE, type = 2)

# ensure strictly increasing breaks (ties can repeat values)
q_breaks <- unique(q_breaks)
if (length(q_breaks) < 5) {
  # if too many ties, jitter slightly to make cut() work
  q_breaks <- sort(unique(c(q_breaks, q_breaks + 1e-6)))
}

df_q <- df_base %>%
  dplyr::mutate(
    N_quartile = cut(
      n_total,
      breaks = q_breaks,
      include.lowest = TRUE,
      labels = c("Q1 (smallest N)", "Q2", "Q3", "Q4 (largest N)")[seq_len(length(q_breaks)-1)]
    )
  )

# Counts per quartile
print(table(df_q$N_quartile, useNA = "ifany"))

# Quartile summary table ----
quart_tbl <- df_q %>%
  dplyr::group_by(N_quartile) %>%
  dplyr::summarise(
    N_min     = min(n_total),
    N_max     = max(n_total),
    FI_median = median(fi_each),
    FI_IQR    = IQR(fi_each),
    FQ_median = median(fq_pct),
    FQ_IQR    = IQR(fq_pct),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    N_range = paste0(scales::comma(N_min), "–", scales::comma(N_max))
  ) %>%
  dplyr::select(N_quartile, N_range, FI_median, FI_IQR, FQ_median, FQ_IQR)

print(quart_tbl)

# Boxplot: FQ by N-quartile (optional)
p_box_FQ_by_quart <- ggplot(df_q, aes(x = N_quartile, y = fq_pct, fill = N_quartile)) +
  geom_boxplot(alpha = 0.85, outlier.shape = 16) +
  labs(title = "Fragility Quotient (FQ) by Sample Size Quartile",
       x = "Sample size quartile", y = "FQ (%)") +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
ggsave("FQ_by_N_quartile_boxplot.png", p_box_FQ_by_quart, width = 8, height = 5, dpi = 300)

# Correlations 
cor_FI <- cor.test(log10(df_base$n_total), df_base$fi_each, method = "spearman", exact = FALSE)
cor_FQ <- cor.test(log10(df_base$n_total), df_base$fq_pct,  method = "spearman", exact = FALSE)

ann_FI <- sprintf("Spearman \u03C1 = %.2f (p = %.1e)", unname(cor_FI$estimate), cor_FI$p.value)
ann_FQ <- sprintf("Spearman \u03C1 = %.2f (p = %.1e)", unname(cor_FQ$estimate), cor_FQ$p.value)

# Panel A: FI vs N ----
p_FI <- ggplot(df_base, aes(x = n_total, y = fi_each)) +
  geom_point(alpha = 0.50, color = "#2C7BE5", size = 2) +
  geom_smooth(se = TRUE, method = "loess", span = 0.8,
              color = "#495057", linewidth = 1.1) +
  scale_x_log10(labels = comma_format(accuracy = 1)) +
  labs(title = "A) Fragility Index (FI) vs Total Sample Size",
       x = "Total sample size (log scale)", y = "Fragility Index (FI)") +
  annotate("text", x = quantile(df_base$n_total, 0.9), 
           y = max(df_base$fi_each, na.rm = TRUE),
           label = ann_FI, hjust = 1, vjust = 1, size = 4.0) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

p_FI

# Panel B: FQ vs N ----
p_FQ <- ggplot(df_base, aes(x = n_total, y = fq_pct)) +
  geom_point(alpha = 0.50, color = "#6C757D", size = 2) +
  geom_smooth(se = TRUE, method = "loess", span = 0.8,
              color = "#C92A2A", linewidth = 1.1) +
  scale_x_log10(labels = comma_format(accuracy = 1)) +
  labs(title = "B) Fragility Quotient (FQ) vs Total Sample Size",
       x = "Total sample size (log scale)", y = "Fragility Quotient (FQ), %") +
  annotate("text", x = quantile(df_base$n_total, 0.9),
           y = max(df_base$fq_pct, na.rm = TRUE),
           label = ann_FQ, hjust = 1, vjust = 1, size = 4.0) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

p_FQ 

gridExtra::grid.arrange(p_FI, p_FQ, ncol = 1)  # renders to the current device

ggsave("FI_and_FQ_vs_SampleSize_two_panel.png",
       plot = gridExtra::grid.arrange(p_FI, p_FQ, ncol = 1),
       width = 9.5, height = 9, dpi = 300)


