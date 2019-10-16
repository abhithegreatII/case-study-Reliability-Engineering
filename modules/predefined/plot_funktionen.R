
# Diese Funktion erzeugt den Wahrscheinlichkeitsplot für mehrere Merkmale (Merkmale sind
# hier z.B. Material oder Biegewinkel).
#
# Sie benötigt folgende Argumente:
#
# x1(, x2, x3): Vektor, der die Biegungsanzahlen für das 1.(, 2., 3.) Merkmal enthält
# y1(, y2, y3): Vektor, der die Ausfallwahrscheinlichkeiten für das 1.(, 2., 3.) Merkmals enthält
# event1(, event2, event3): Vektor, der die Status für das 1.(, 2., 3.) Merkmals enthält
# merkmal: Character-Vektor, der die Merkmale in der entsprechenden Reihenfolge enthält 
#          (z.B. c("Al", "Cu") oder c("45", "90", "180"))
# id1(, id2, id3): Vektor, der die IDs für das 1.(, 2., 3.) Merkmals enthält
# distribution: angenommene Verteilung der Zufallsvariable. Muss hier "weibull" sein.
# title_main: Titel des Plots
# title_x: Titel der x-Achse
# title_y: Titel der y-Achse
#
# Anmerkung: Für x, y, event und id können die entsprechenden Spalten des Dataframes übernommen werden, 
#            der bspw. von den Funktionen "johnson_method" oder "mr_method" erzeugt wird.


plot_wahrsch <- function(x1, x2 = NULL, x3 = NULL, y1, y2 = NULL, y3 = NULL, event1, event2 = NULL, event3 = NULL, 
  merkmal, id1, id2 = NULL, id3 = NULL,
  distribution = c("weibull", "lognormal", "loglogistic"),
  title_main = "Probability Plot",
  title_x = "Characteristic",
  title_y = "Unreliability") {
  
  
  x1_s <- x1[event1 == 1]
  y1_s <- y1[event1 == 1]
  x1_s <- x1_s[order(x1_s)]
  y1_s <- y1_s[order(x1_s)]
  mm <- c(rep(merkmal[1], length(x1_s)))
  id <- id1
  event <- event1
  
  if(!is.null(x2)) {
    x2_s <- x2[event2 ==1]
    y2_s <- y2[event2 == 1]
    x2_s <- x2_s[order(x2_s)]
    y2_s <- y2_s[order(x2_s)]
    
    x_s <- c(x1_s, x2_s)
    y_s <- c(y1_s, y2_s)
    mm <- c(mm, rep(merkmal[2], length(x2_s)))
    id <- c(id, id2)
    event <- c(event, event2)
    
    if(!is.null(x3)) {
      x3_s <- x3[event3 ==1]
      y3_s <- y3[event3 == 1]
      x3_s <- x3_s[order(x3_s)]
      y3_s <- y3_s[order(x3_s)]
      
      x_s <- c(x_s, x3_s)
      y_s <- c(y_s, y3_s)
      mm <- c(mm, rep(merkmal[3], length(x3_s)))
      id <- c(id, id3)
      event <- c(event, event3)
    }
  } else {
    x_s <- x1_s
    y_s <- y2_s
  }
  
  p <- weibulltools::plot_layout(x = x_s, distribution = distribution, 
    title_main = title_main,
    title_x = title_x,
    title_y = title_y)
  
  mark_x <- unlist(strsplit(title_x, " "))[1]
  
  if (distribution == "weibull") {
    q = SPREDA::qsev(y_s)
  } else if (distribution == "lognormal") {
    q = stats::qnorm(y_s)
  } else if (distribution == "loglogistic") {
    q = stats::qlogis(y_s)
  }
  
  df <- data.frame(x_s, q, merkmal = as.factor(mm))
  u <- as.character(unique(df$merkmal))
  
  
  plot <- p %>% plotly::add_trace(x = ~x_s, y = ~q, type = "scatter",
    mode = "markers", hoverinfo = "text",
    color = ~df$merkmal,
    colors = c("45" = "blue", "90" = "orange", "180" = "limegreen",
               "45°" = "blue", "90°" = "orange", "180°" = "limegreen",
      "Al" = "blue", "Cu" = "orange","5% bis 95% Quantile" = "red", "Wöhlerlinie" = "red",
      "Al-ML" = "blue4", "Cu-ML" = "orange4", "45°-ML" = "blue4", "90°-ML" = "orange4", "180°-ML" = "green4"),
    text = ~paste("ID:", id[event == 1],
      paste("<br>", paste0(mark_x, ":")), x_s,
      paste("<br>", paste0(title_y, ":")), round(y_s*100, digits = 5))
  )
  return(plot)
}


#_____________________________________________________________________________________________________________

# Diese Funktion fügt dem Wahrscheinlichkeitsplot die Weibullgeraden hinzu.
# Sie benötigt folgende Argumente:
# p_obj: Plotly-Objekt, das den Wahrscheinlichkeitsplot enthält
# x_1(, x_2, x_3): Vektor, der die Biegungsanzahlen des 1.(, 2., 3.) Merkmals enthält.
#                  Kann z.B. die entsprechende Spalte des Dataframes sein, der von  
#                  den Funktionen "johnson_method" oder "mr_method" erzeugt wird.
# y: Vektor, der die y-Koordinaten der Regressionslinie enthält. Der Default-Wert ist NULL
#    und sollte in diesem Fall nicht geändert werden, da die y-Koordinaten so aus den
#    Verteilungsparametern in mrr1(, mrr2, mrr3) berechnet werden
# mrr1(, mrr2, mrr3): Output, der von der Funktion "rank_regression" erzeugt wird
#                     und die Werte beinhaltet, die zur Schätzung der Weibullgerade des
#                     1.(, 2., 3.) Merkmals benötigt werden
# distribution: angenommene Verteilung der Zufallsvariable. Muss hier "weibull" sein.
# title_trace: Character-Vektor, der für die korrekte Beschriftung in der Legende sorgt.
#              Sollte die Merkmale in der gleichen Reihenfolge enthalten, wie die Argumente
#              "x" und "mrr" angegeben werden. (Bsp.: c("Cu", "Al") oder c("45", "90", "180"))


plot_reg <- function(p_obj, x1, x2 = NULL, x3 = NULL, y = NULL, 
  mrr1, mrr2 = NULL, mrr3 = NULL,
  distribution = c("weibull", "lognormal", "loglogistic"),
  title_trace = c("Merkmal")) {
  
  
  
  plot_mod_groups <- function(p_obj, x, y. = y, loc_sc_params, color, title_trace.) {
    
    if (is.null(y.)) {
      x_min <- min(x, na.rm = TRUE)
      x_max <- max(x, na.rm = TRUE)
      x_low <- x_min - 10 ^ floor(log10(x_min)) * .5
      x_high <- x_max + 10 ^ floor(log10(x_max)) * .25
      
      x_p <- seq(x_low, x_high, length.out = 200)
      y_p <- predict_prob(q = x_p, loc_sc_params = loc_sc_params,
        distribution = distribution)
    } else {
      x_p <- x
      y_p <- y.
    }
    
    y_p <- round(y_p, digits = 5)
    x_p <- x_p[!duplicated(y_p) & y_p != 1]
    y_p <- y_p[!duplicated(y_p) & y_p != 1]
    
    df_p <- data.frame(x_p = x_p, y_p = y_p)
    
    x_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$xaxis$title,
      " "))[1]
    y_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$yaxis$title,
      " "))[1]
    
    if (distribution == "weibull") {
      q = SPREDA::qsev(y_p)
    } else if (distribution == "lognormal") {
      q = stats::qnorm(y_p)
    } else if (distribution == "loglogistic") {
      q = stats::qlogis(y_p)
    }
    
    p_mod <- plotly::add_lines(
      p = p_obj, data = df_p, x = ~x_p, y = ~q,
      type = "scatter", mode = "lines", hoverinfo = "text", line = list(color = color),
      name = title_trace.,
      text = ~paste(paste0(x_mark, ":"), round(x_p, digits = 2),
        paste("<br>", paste0(y_mark, " in %:")), round(100*y_p, digits = 5),
        "<br>T:", round(exp(loc_sc_params[[1]]), digits = 2),
        "<br>b:", round(1 / loc_sc_params[[2]], digits = 2)))
    
    return(p_mod)
  }
  
  col_df <- data.frame(tt = title_trace, col = vector(length = length(title_trace)))
  col_df$col[col_df$tt == "45°"  | col_df$tt == "45" | col_df$tt == "Al"] <- "blue"
  col_df$col[col_df$tt == "90°"  | col_df$tt == "90" | col_df$tt == "Cu"] <- "orange"
  col_df$col[col_df$tt == "180°" | col_df$tt == "180"] <- "limegreen"
  
  if(!is.null(x2)) {
    
    plot <- p_obj %>% plot_mod_groups(x = x1,loc_sc_params = mrr1$loc_sc_coefficients, color = col_df$col[1], title_trace = paste(title_trace[1])) %>%
      plot_mod_groups(x = x2, loc_sc_params = mrr2$loc_sc_coefficients, color = col_df$col[2], title_trace. = paste(title_trace[2]))
    
    if(!is.null(x3)) {
      
      plot <- plot_mod_groups(p_obj = plot, x = x3, loc_sc_params = mrr3$loc_sc_coefficients, color = col_df$col[3], title_trace. = paste(title_trace[3]))
    }
  } else {
    plot <- p_obj %>% plot_mod_groups(x = x1,loc_sc_params = mrr1$loc_sc_coefficients, color = col_df$col[1], title_trace. = title_trace[1])
  }
  return(plot)
}

#_____________________________________________________________________________________________________________

# Mit dieser Funktion wird die Basis des Wöhlerplots erzeugt. Sie enthält die korrekt formatierten Achsen
# und alle durch die Biegeversuche aufgenommenen Datenpunkte.
# Die Funktion benötigt folgende Argumente:
# x: Vektor, der alle Biegungsanzahlen enthält
# y: Vektor, der die Winkel für jede Biegungsanzahl enthält
# title_main, title_x und title_y müssen nur genutzt werden, wenn Sie den Plottitel und die Achsenbeschriftung
# ändern wollen.

plot_woehler_base <- function(x, y,
  title_main = "Wöhlerlinie",
  title_x = "Biegungsanzahl",
  title_y = "Biegewinkel") {
  
  x_s <- x[!is.na(x) == T]
  y_s <- y[!is.na(x) == T]
  
  # layout dependent on data x
  # define x-ticks of logarithm to the base of 10
  x_base <- function(xb) floor(log10(xb))
  xlog10_range <- (x_base(min(x_s)) - 1):x_base(max(x_s))
  
  # x-ticks and x-labels
  x_ticks <- sapply(xlog10_range, function(z) seq(10 ^ z, 10 ^ (z + 1), 10 ^ z),
    simplify = TRUE)
  x_ticks <- round(as.numeric(x_ticks), digits = 10)
  x_ticks <- x_ticks[!duplicated(x_ticks)]
  x_labels <- x_ticks
  x_labels[c(rep(F, 3), rep(T, 6))] <- ''
  
  # y-ticks and y-labels
  # hard coded but it's okay since range is always between 0 and 1.
  y_ticks <- c(45, 90, 180)
  
  
  # configuration x axis
  x_config <- list(
    color = "#000000",
    title = list(
      text = title_x,
      font = list(
        family = "Arial",
        size = 12,
        color = "#A3A3A3"
      )
    ),
    type = "log",
    autorange = TRUE,
    rangemode = "nonnegative",
    tickvals = x_ticks,
    ticktext = x_labels,
    ticks = "inside",
    tickcolor = "#a0a0a0",
    tickwidth = 1,
    tickfont = list(family = 'Arial', size = 10, color = "#a0a0a0"),
    showticklabels = TRUE,
    zeroline = FALSE,
    showgrid = TRUE,
    gridwidth = 1,
    exponentformat = "none",
    showline = TRUE,
    linecolor = "#a0a0a0"
  )
  
  # configuration y axis
  y_config <- list(
    color = "#000000",
    title = list(
      text = paste(title_y),
      font = list(
        family = "Arial",
        size = 12,
        color = "#A3A3A3"
      )
    ),
    autorange = TRUE,
    tickvals = y_ticks,
    ticktext = y_ticks,
    ticks = "inside",
    tickcolor = "#a0a0a0",
    tickwidth = 1,
    tickfont = list(family = 'Arial', size = 10, color = "#a0a0a0"),
    showticklabels = TRUE,
    zeroline = FALSE,
    showgrid = TRUE,
    gridwidth = 1,
    exponentformat = "none",
    showline = TRUE,
    linecolor = "#a0a0a0"
  )
  
  # configuration legend
  l <- list(
    title = list(
      font = list(
        family = "Arial",
        size = 10,
        color = "#000000"
      )
    )
  )
  
  # margins layout
  m <- list(
    l = 55,
    r = 10,
    b = 55,
    t = 25,
    pad = 4
  )
  
  # create grid
  
  #type = "scatter", mode = "none", colors = colors
  
  p <- plotly::plotly_empty() %>%
    plotly::layout(title = list(text = title_main, font = list(family = "Arial",
      size = 16, color = "#000000")), separators = ".",
      legend = l, xaxis = x_config, yaxis = y_config, margin = m)
  
  plot <- p %>% plotly::add_trace(x = ~x_s, y = ~y_s, type = "scatter",
    mode = "markers", hoverinfo = "text",
    color = ~as.factor(y_s),
    colors = c("45" = "blue", "90" = "orange", "180" = "limegreen"),
    showlegend = FALSE,
    text = ~paste(paste("<br>", paste0(title_x, ":")), x_s,
      paste("<br>", paste0(title_y, ":")), round(y_s, digits = 2)))
  return(plot)
}

#___________________________________________________________________________________________________

plot_woehler_komplett <- function(mrr1, mrr2, mrr3, title_main = "Wöhlergerade", title_x = "Biegungsanzahl", title_y = "Biegewinkel"){
  
  
  t_45 <- c()
  t_90 <- c()
  t_180 <- c()
  
  for(i in c(0.05, 0.5, 0.95)){
    t_45 <- c(t_45, predict_quantile(p = i, loc_sc_params = mrr1$loc_sc_coefficients))
    t_90 <- c(t_90, predict_quantile(p = i, loc_sc_params = mrr2$loc_sc_coefficients))
    t_180 <- c(t_180, predict_quantile(p = i, loc_sc_params = mrr3$loc_sc_coefficients))
  }
    
    x <- x_s <- c(t_45, t_90, t_180)
    y <- y_s <- c(45,45,45,90,90, 90,180,180, 180)
  
  # layout dependent on data x
  # define x-ticks of logarithm to the base of 10
  x_base <- function(xb) floor(log10(xb))
  xlog10_range <- (x_base(min(x_s)) - 1):x_base(max(x_s))
  
  # x-ticks and x-labels
  x_ticks <- sapply(xlog10_range, function(z) seq(10 ^ z, 10 ^ (z + 1), 10 ^ z),
                    simplify = TRUE)
  x_ticks <- round(as.numeric(x_ticks), digits = 10)
  x_ticks <- x_ticks[!duplicated(x_ticks)]
  x_labels <- x_ticks
  x_labels[c(rep(F, 3), rep(T, 6))] <- ''
  
  # y-ticks and y-labels
  # hard coded but it's okay since range is always between 0 and 1.
  y_ticks <- c(45, 90, 180)
  
  
  # configuration x axis
  x_config <- list(
    color = "#000000",
    title = list(
      text = title_x,
      font = list(
        family = "Arial",
        size = 12,
        color = "#A3A3A3"
      )
    ),
    type = "log",
    autorange = TRUE,
    rangemode = "nonnegative",
    tickvals = x_ticks,
    ticktext = x_labels,
    ticks = "inside",
    tickcolor = "#a0a0a0",
    tickwidth = 1,
    tickfont = list(family = 'Arial', size = 10, color = "#a0a0a0"),
    showticklabels = TRUE,
    zeroline = FALSE,
    showgrid = TRUE,
    gridwidth = 1,
    exponentformat = "none",
    showline = TRUE,
    linecolor = "#a0a0a0"
  )
  
  # configuration y axis
  y_config <- list(
    color = "#000000",
    title = list(
      text = paste(title_y),
      font = list(
        family = "Arial",
        size = 12,
        color = "#A3A3A3"
      )
    ),
    autorange = TRUE,
    tickvals = y_ticks,
    ticktext = y_ticks,
    ticks = "inside",
    tickcolor = "#a0a0a0",
    tickwidth = 1,
    tickfont = list(family = 'Arial', size = 10, color = "#a0a0a0"),
    showticklabels = TRUE,
    zeroline = FALSE,
    showgrid = TRUE,
    gridwidth = 1,
    exponentformat = "none",
    showline = TRUE,
    linecolor = "#a0a0a0"
  )
  
  # configuration legend
  l <- list(
    font = list(
        family = "Arial",
        size = 10,
        color = "#000000"
      )
    )
  
  
  # margins layout
  m <- list(
    l = 55,
    r = 10,
    b = 55,
    t = 25,
    pad = 4
  )
  
  # create grid
  
  #type = "scatter", mode = "none", colors = colors
  
  p <- plotly::plotly_empty() %>%
    plotly::layout(title = list(text = title_main, 
                                font = list(family = "Arial",
                                            size = 16, 
                                            color = "#000000")), 
                   separators = ".",
                   legend = l, xaxis = x_config, yaxis = y_config, margin = m)
  
  plot <- p %>% plotly::add_trace(x = ~x_s, y = ~y_s, type = "scatter",
                                  mode = "markers", hoverinfo = "text",
                                  color = c("45°","45°","45°", "90°","90°","90°", "180°", "180°", "180°"),
                                  colors = c("45°" = "blue", "90°" = "orange", "180°" = "limegreen", "Wöhlerlinie" = "red"),
                                  showlegend = TRUE,
                                  text = ~paste(paste("<br>", paste0(title_x, ":")), round(x_s, digits = 4),
                                                paste("<br>", paste0(title_y, ":")), round(y_s, digits = 2)))%>%
    add_lines(x = c(t_45[2], t_90[2], t_180[2]), y = c(45, 90, 180), hoverinfo = "text",
              color = as.factor("Wöhlerlinie"),
              colors = c("45°" = "blue", "90°" = "orange", "180°" = "limegreen", "Wöhlerlinie" = "red"),
              text = ~paste("Woehler-Gerade der 50% Quantile"))
  return(plot)
  }

