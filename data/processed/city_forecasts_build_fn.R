build_city_plot <- 
function (city_df, cn) 
{
    levs <- names(model_colors)
    levs <- c("Actual", setdiff(levs[levs %in% unique(city_df$model)], 
        "Actual"))
    p <- plotly::plot_ly(height = 560)
    for (lv in levs) {
        d <- city_df %>% dplyr::filter(model == lv) %>% dplyr::arrange(date)
        p <- p %>% plotly::add_trace(x = d$date, y = d$pm25, 
            name = lv, type = "scatter", mode = "lines", line = list(color = unname(model_colors[lv]), 
                width = if (lv == "Actual") 2.2 else 1.4, dash = if (lv == 
                  "Actual") "solid" else "dot"), hovertemplate = paste0("<b>", 
                lv, "</b><br>%{x|%Y-%m-%d}: %{y:.1f} µg/m³<extra></extra>"))
    }
    p %>% plotly::layout(autosize = TRUE, title = list(text = paste0("<b>", 
        cn, "</b>"), x = 0.02, y = 0.97), xaxis = list(title = NULL, 
        rangeslider = list(visible = TRUE, thickness = 0.08)), 
        yaxis = list(title = "PM₂.₅ (µg/m³)"), hovermode = "x unified", 
        margin = list(t = 80, b = 60, l = 60, r = 20), legend = list(orientation = "h", 
            x = 0, xanchor = "left", y = 1.08, yanchor = "bottom", 
            bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#ccc", 
            borderwidth = 1, font = list(size = 11)), shapes = list(list(type = "line", 
            x0 = min(city_df$date), x1 = max(city_df$date), y0 = 15, 
            y1 = 15, line = list(color = "red", width = 1, dash = "dash"))), 
        annotations = list(list(x = min(city_df$date), y = 15, 
            xref = "x", yref = "y", text = "WHO 2021 AQG 15 µg/m³", 
            showarrow = FALSE, xanchor = "left", yanchor = "bottom", 
            font = list(color = "red", size = 10)))) %>% plotly::config(responsive = TRUE)
}
