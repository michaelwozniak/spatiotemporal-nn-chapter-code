suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(patchwork)
})

fig_dir <- here::here("output", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

theme_didactic <- ggplot2::theme_void(base_size = 12) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(hjust = 0.5, size = 13, face = "bold"),
    plot.subtitle    = ggplot2::element_text(hjust = 0.5, size = 10, colour = "grey30"),
    plot.margin      = ggplot2::margin(8, 8, 8, 8),
    legend.position  = "none"
  )

box_node <- function(xmin, xmax, ymin, ymax, fill, label) {
  list(
    ggplot2::annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
             fill = fill, colour = "grey20", linewidth = 0.4),
    ggplot2::annotate("text", x = (xmin + xmax) / 2, y = (ymin + ymax) / 2,
             label = label, size = 4.4, parse = TRUE)
  )
}

op_circle <- function(x, y, label, fill = "white", label_size = 5.2) {
  list(
    ggplot2::annotate("point", x = x, y = y, shape = 21, fill = fill,
             colour = "grey20", stroke = 0.6, size = 11),
    ggplot2::annotate("text", x = x, y = y, label = label, size = label_size)
  )
}

arrow_seg <- function(x, xend, y, yend, lty = "solid", lwd = 0.55) {
  ggplot2::annotate("segment", x = x, xend = xend, y = y, yend = yend,
           arrow = grid::arrow(length = grid::unit(0.10, "cm"), type = "closed"),
           linewidth = lwd, linetype = lty, colour = "grey20")
}

plain_seg <- function(x, xend, y, yend, lty = "solid", lwd = 0.55) {
  ggplot2::annotate("segment", x = x, xend = xend, y = y, yend = yend,
           linewidth = lwd, linetype = lty, colour = "grey20")
}

col_state <- "#FCE4A6"
col_gate  <- "#CFE8FF"
col_cand  <- "#D9F0D3"
col_op    <- "white"

y_top <- 5.5
y_bot <- 1.3

gx <- c(f = 2.7, i = 4.7, g = 6.7, o = 8.7)
gw <- 0.85

mul_f     <- gx["f"]
mul_ig    <- (gx["i"] + gx["g"]) / 2
plus_node <- mul_ig
mul_o     <- 10.3
tanh_node <- 10.3

p_lstm <- ggplot2::ggplot() +
  ggplot2::annotate("rect", xmin = 0.5, xmax = 11.0, ymin = 0.4, ymax = 6.4,
           fill = "grey97", colour = "grey60", linewidth = 0.3,
           linetype = "dashed") +
  ggplot2::annotate("text", x = 10.9, y = 6.25, label = "LSTM cell",
           hjust = 1, vjust = 1, size = 3.2,
           colour = "grey45", fontface = "italic") +

  plain_seg(0.6, mul_f - 0.18, y_top, y_top, lwd = 1.0) +
  op_circle(mul_f, y_top, "×", fill = col_op) +
  plain_seg(mul_f + 0.18, plus_node - 0.18, y_top, y_top, lwd = 1.0) +
  op_circle(plus_node, y_top, "+", fill = col_op) +
  plain_seg(plus_node + 0.18, 10.9, y_top, y_top, lwd = 1.0) +
  ggplot2::annotate("text", x = 0.55, y = y_top + 0.35, label = "c[t-1]",
           parse = TRUE, hjust = 0, size = 4.4) +
  ggplot2::annotate("text", x = 10.95, y = y_top + 0.35, label = "c[t]",
           parse = TRUE, hjust = 1, size = 4.4) +
  ggplot2::annotate("text", x = (mul_f + plus_node) / 2, y = y_top + 0.55,
           label = "additive update (CEC)",
           size = 3.0, colour = "#B8860B", fontface = "italic") +

  plain_seg(0.6, mul_o + 0.18, y_bot, y_bot, lwd = 0.5, lty = "dotted") +
  plain_seg(mul_o, 10.9, y_bot, y_bot, lwd = 1.0) +
  ggplot2::annotate("text", x = 0.55, y = y_bot + 0.35, label = "h[t-1]",
           parse = TRUE, hjust = 0, size = 4.4) +
  ggplot2::annotate("text", x = 10.95, y = y_bot + 0.35, label = "h[t]",
           parse = TRUE, hjust = 1, size = 4.4) +

  plain_seg(0.6, gx["o"] + 0.0, 0.7, 0.7, lwd = 0.5, lty = "dotted") +
  ggplot2::annotate("text", x = 0.55, y = 0.7, label = "x[t]",
           parse = TRUE, hjust = 1.1, size = 4.4) +

  box_node(gx["f"] - gw, gx["f"] + gw, 2.5, 3.5, col_gate, "f[t]") +
  box_node(gx["i"] - gw, gx["i"] + gw, 2.5, 3.5, col_gate, "i[t]") +
  box_node(gx["g"] - gw, gx["g"] + gw, 2.5, 3.5, col_cand, "g[t]") +
  box_node(gx["o"] - gw, gx["o"] + gw, 2.5, 3.5, col_gate, "o[t]") +

  arrow_seg(gx["f"], gx["f"], y_bot, 2.5) +
  arrow_seg(gx["i"], gx["i"], y_bot, 2.5) +
  arrow_seg(gx["g"], gx["g"], y_bot, 2.5) +
  arrow_seg(gx["o"], gx["o"], y_bot, 2.5) +

  arrow_seg(gx["f"], gx["f"], 3.5, y_top - 0.18) +
  plain_seg(gx["i"], gx["i"], 3.5, 4.4, lwd = 0.5) +
  plain_seg(gx["g"], gx["g"], 3.5, 4.4, lwd = 0.5) +
  plain_seg(gx["i"], mul_ig - 0.16, 4.4, 4.4, lwd = 0.5) +
  plain_seg(mul_ig + 0.16, gx["g"], 4.4, 4.4, lwd = 0.5) +
  op_circle(mul_ig, 4.4, "×", fill = col_op, label_size = 4.4) +
  arrow_seg(mul_ig, plus_node, 4.4, y_top - 0.18) +

  op_circle(10.3, 4.4, "tanh", fill = col_state, label_size = 3.4) +
  plain_seg(10.3, 10.3, y_top - 0.18, 4.6, lwd = 0.5) +
  plain_seg(10.3, 10.3, 4.2, 3.6, lwd = 0.5) +
  op_circle(10.3, 3.4, "×", fill = col_op, label_size = 4.4) +
  plain_seg(gx["o"] + gw, gx["o"] + gw, 3.0, 3.4, lwd = 0.5) +
  plain_seg(gx["o"] + gw, 10.15, 3.4, 3.4, lwd = 0.5) +
  arrow_seg(10.3, 10.3, 3.22, y_bot + 0.05) +

  ggplot2::coord_fixed(xlim = c(-0.2, 11.4), ylim = c(0.0, 6.6), expand = FALSE) +
  ggplot2::labs(
    title    = "The LSTM cell: an additive cell-state highway with multiplicative gates",
    subtitle = expression("Cell state " * c[t] == f[t] %.% c[t-1] + i[t] %.% g[t] *
                          ".  The '+' on the highway lets gradients flow back through time unimpeded.")
  ) +
  theme_didactic

gate_eqs_top <- expression(
  paste(f[t] == sigma(W["xf"] * x[t] + W["hf"] * h[t-1] + b[f]),
        "          ",
        i[t] == sigma(W["xi"] * x[t] + W["hi"] * h[t-1] + b[i]))
)
gate_eqs_bot <- expression(
  paste(g[t] == tanh(W["xg"] * x[t] + W["hg"] * h[t-1] + b[g]),
        "          ",
        o[t] == sigma(W["xo"] * x[t] + W["ho"] * h[t-1] + b[o]))
)

caption_panel <- ggplot2::ggplot() +
  ggplot2::annotate("text", x = 0, y = 0.85, label = gate_eqs_top,
           size = 3.4, hjust = 0.5, parse = TRUE) +
  ggplot2::annotate("text", x = 0, y = 0.50, label = gate_eqs_bot,
           size = 3.4, hjust = 0.5, parse = TRUE) +
  ggplot2::annotate("text", x = 0, y = 0.0,
           label = expression(paste(c[t] == f[t] %.% c[t-1] + i[t] %.% g[t],
                                    "        ",
                                    h[t] == o[t] %.% tanh(c[t]))),
           size = 3.8, hjust = 0.5, parse = TRUE, fontface = "italic") +
  ggplot2::coord_cartesian(xlim = c(-1, 1), ylim = c(-0.3, 1.1)) +
  ggplot2::theme_void()

p_lstm_full <- p_lstm / caption_panel + patchwork::plot_layout(heights = c(7, 1.5))

ggsave_quiet <- function(...) {
  withCallingHandlers(
    ggplot2::ggsave(...),
    warning = function(w) {
      if (grepl("applied to non-.*'expression'", conditionMessage(w))) invokeRestart("muffleWarning")
    }
  )
}

ggsave_quiet(file.path(fig_dir, "lstm_cell_diagram.png"),
       p_lstm_full, width = 11, height = 6.4, dpi = 200, bg = "white")

set.seed(7)
input_mat  <- matrix(c(
  3, 1, 0, 2, 1,
  2, 4, 1, 0, 2,
  1, 2, 3, 1, 0,
  0, 1, 2, 3, 1,
  2, 0, 1, 2, 3
), nrow = 5, byrow = TRUE)

kernel_mat <- matrix(c(
  1,  0, -1,
  1,  0, -1,
  1,  0, -1
), nrow = 3, byrow = TRUE)

out_row1 <- sapply(1:3, function(s) sum(input_mat[1:3, s:(s + 2)] * kernel_mat))

grid_to_df <- function(M, x_off = 0, y_off = 0) {
  nr <- nrow(M); nc <- ncol(M)
  tidyr::expand_grid(row = seq_len(nr), col = seq_len(nc)) |>
    dplyr::mutate(
      x     = col + x_off,
      y     = (nr - row + 1) + y_off,
      value = as.vector(t(M))
    )
}

INP_X <- 0
KER_X <- 6
OUT_X <- 10
Y_OFF <- 1

conv_panel <- function(slide) {
  patch <- input_mat[1:3, slide:(slide + 2)]
  out_val <- out_row1[slide]

  inp_df <- grid_to_df(input_mat, x_off = INP_X) |>
    dplyr::mutate(highlight = (col >= slide & col <= slide + 2 & row <= 3))

  ker_df <- grid_to_df(kernel_mat, x_off = KER_X, y_off = Y_OFF)

  out_grid <- matrix(NA_real_, nrow = 3, ncol = 3)
  for (s in seq_len(slide)) out_grid[1, s] <- out_row1[s]
  out_df <- grid_to_df(out_grid, x_off = OUT_X, y_off = Y_OFF) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(is_current = col == slide)

  out_frame <- tidyr::expand_grid(row = 1:3, col = 1:3) |>
    dplyr::mutate(x = col + OUT_X, y = (3 - row + 1) + Y_OFF)

  prods <- sprintf("%d·%d",
                   as.vector(t(kernel_mat)),
                   as.vector(t(patch)))
  eq_rhs <- paste(prods, collapse = " + ")

  ggplot2::ggplot() +
    ggplot2::geom_tile(data = inp_df, ggplot2::aes(x = x, y = y, fill = highlight),
              colour = "grey30", linewidth = 0.3) +
    ggplot2::geom_text(data = inp_df, ggplot2::aes(x = x, y = y, label = value),
              size = 3.6) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "#FFE9B0", `FALSE` = "white")) +
    ggplot2::annotate("text", x = INP_X + 3, y = 6.6, label = "input  U  (5×5)",
             size = 3.6) +

    ggplot2::geom_tile(data = ker_df, ggplot2::aes(x = x, y = y),
              fill = "#CFE8FF", colour = "grey30", linewidth = 0.3) +
    ggplot2::geom_text(data = ker_df, ggplot2::aes(x = x, y = y, label = value),
              size = 3.6) +
    ggplot2::annotate("text", x = KER_X + 2, y = 6.6, label = "kernel  K  (3×3)",
             size = 3.6) +

    ggplot2::geom_tile(data = out_frame, ggplot2::aes(x = x, y = y),
              fill = "white", colour = "grey80", linewidth = 0.3) +
    ggplot2::geom_tile(data = out_df,
              ggplot2::aes(x = x, y = y,
                  fill = is_current),
              colour = "grey30", linewidth = 0.4, show.legend = FALSE) +
    ggplot2::geom_text(data = out_df, ggplot2::aes(x = x, y = y, label = value),
              size = 4.2, fontface = "bold") +
    ggplot2::annotate("text", x = OUT_X + 2, y = 6.6, label = "output  V  (3×3)",
             size = 3.6) +

    ggplot2::annotate("text", x = (INP_X + OUT_X + 3) / 2, y = -0.2,
             label = sprintf("V[1,%d] = %s = %d", slide, eq_rhs, out_val),
             size = 3.1, colour = "grey15") +

    ggplot2::coord_fixed(xlim = c(-0.4, OUT_X + 4.5), ylim = c(-0.7, 7),
                expand = FALSE) +
    theme_didactic
}

p_conv <- (conv_panel(1) / conv_panel(2) / conv_panel(3)) +
  patchwork::plot_annotation(
    title    = "A 3×3 kernel walks across a 5×5 input",
    subtitle = "The same nine kernel weights are applied at every position; each output cell is the sum of the elementwise product",
    theme    = ggplot2::theme(
      plot.title    = ggplot2::element_text(hjust = 0.5, size = 13, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10, colour = "grey30")
    )
  )

ggsave_quiet(file.path(fig_dir, "conv2d_in_action.png"),
       p_conv, width = 11, height = 9.2, dpi = 200, bg = "white")

rf_panel <- function(L, rf_size, grid_size = 15) {
  centre <- (grid_size + 1) / 2
  half   <- (rf_size - 1) / 2
  df <- tidyr::expand_grid(row = seq_len(grid_size), col = seq_len(grid_size)) |>
    dplyr::mutate(
      in_rf  = abs(row - centre) <= half & abs(col - centre) <= half,
      is_ctr = row == centre & col == centre
    )

  ggplot2::ggplot(df, ggplot2::aes(x = col, y = grid_size - row + 1)) +
    ggplot2::geom_tile(ggplot2::aes(fill = in_rf), colour = "grey80", linewidth = 0.2) +
    ggplot2::geom_tile(data = dplyr::filter(df, is_ctr), fill = "#E63946",
              colour = "grey20", linewidth = 0.4) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "#FFD27A", `FALSE` = "grey97")) +
    ggplot2::coord_fixed(expand = FALSE) +
    ggplot2::labs(
      title    = sprintf("After %d stride-2 conv layer%s", L, if (L == 1) "" else "s"),
      subtitle = sprintf("receptive field ≈ %d×%d input cells",
                         rf_size, rf_size)
    ) +
    theme_didactic +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(hjust = 0.5, size = 11, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 9.5, colour = "grey30")
    )
}

p_rf <- rf_panel(1, 3) + rf_panel(2, 7) + rf_panel(3, 15) +
  patchwork::plot_annotation(
    title    = "Stacking convolutions widens the receptive field",
    subtitle = "Red = the output cell. Yellow = the input cells it depends on. Each stride-2 layer roughly doubles the field (3 → 7 → 15 cells).",
    theme    = ggplot2::theme(
      plot.title    = ggplot2::element_text(hjust = 0.5, size = 13, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10, colour = "grey30")
    )
  )

ggsave_quiet(file.path(fig_dir, "receptive_field_growth.png"),
       p_rf, width = 10, height = 4.4, dpi = 200, bg = "white")

message("Wrote three didactic figures to ", fig_dir)
