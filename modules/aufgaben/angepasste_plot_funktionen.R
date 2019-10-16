
###############################################################################
#
# file:         "angepasste_plot_funktionen.R"
#
# date:         2019-09-13
#
# authors:      Group 3
#
# brief:        This script contains the customized version of predefined 
#               "plot_funktionen.R"
#
###############################################################################

plot_wahrsch_angepasst <- function(x1, x2 = NULL, x3 = NULL, y1, y2 = NULL, y3 = NULL, event1, event2 = NULL, event3 = NULL, 
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

plot_reg_angepasst <- function(p_obj, x1, x2 = NULL, x3 = NULL, y = NULL, 
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

Woehlerlinie <- function(x, y, mrr1, mrr2, mrr3,
                         title_main = "Wöhlerlinie",
                         title_x = "Biegungsanzahl",
                         title_y = "Biegewinkel") {
  
  
  ################################
  t_45 <- c()
  t_90 <- c()
  t_180 <- c()
  
  for(i in c(0.05, 0.5, 0.95)){
    t_45 <- c(t_45, predict_quantile(p = i, loc_sc_params = mrr1$loc_sc_coefficients))
    t_90 <- c(t_90, predict_quantile(p = i, loc_sc_params = mrr2$loc_sc_coefficients))
    t_180 <- c(t_180, predict_quantile(p = i, loc_sc_params = mrr3$loc_sc_coefficients))
  }
  
  # x <- x_s <- c(t_45, t_90, t_180)
  # y <- y_s <- c(45,45,45,90,90, 90,180,180, 180)
  ################################
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
                   legend = l, xaxis = x_config, yaxis = y_config, margin = m, height = 650)
  
  plot <- p %>% plotly::add_trace(x = ~x_s, y = ~y_s, type = "scatter",
                                  mode = "markers", hoverinfo = "text",
                                  color = ~as.factor(y_s),
                                  colors = c("45" = "blue", "90" = "orange", "180" = "limegreen", "Wöhlerlinie" = "red"),
                                  showlegend = FALSE,
                                  text = ~paste(paste("<br>", paste0(title_x, ":")), x_s,
                                                paste("<br>", paste0(title_y, ":")), round(y_s, digits = 2))) %>%
    
    add_trace(type = "scatter", mode = "lines", x = c(t_45[1], t_90[1], t_180[1]), y =c(45, 90, 180), 
              name = "Wöhlerlinie 5 %", line = list(color = 'transparent') , showlegend = FALSE,
              hoverinfo = "text", text = "Blabla") %>%
    add_trace(type = "scatter", mode = "lines", x = c(t_45[3], t_90[3], t_180[3]), y =c(45, 90, 180), 
              name = "Wöhlerlinie 95 %", line = list(color = 'transparent') ,fill = 'tonextx', fillcolor = 'rgba(255, 199, 186, 0.5)',
              showlegend = FALSE,
              hoverinfo = "text", text = "Blablub") %>%
    add_trace(type = "scatter", mode = "markers", x = c(t_45[1], t_90[1], t_180[1]), y =c(45, 90, 180), 
              name = "Ausfallwahrscheinlichkeit 5 %",  marker = list( symbol="triangle-down",
                color = '#ff8469'),
              hoverinfo = "text", text = ~paste(paste("Ausfallwahrscheinlichkeit 5 %:","<br>", paste0(title_x, ":")), round(c(t_45[1], t_90[1], t_180[1]), digits = 0),
                                                paste("<br>", paste0(title_y, ":")), round(c(45, 90, 180), digits = 0)))%>%
    add_trace(type = "scatter", mode = "markers", x = c(t_45[3], t_90[3], t_180[3]), y =c(45, 90, 180), 
              name = "Ausfallwahrscheinlichkeit 95 %",  marker = list( symbol="triangle-down",
                color = '#9c1c00'),
              hoverinfo = "text", text = ~paste(paste("Ausfallwahrscheinlichkeit 95 %:","<br>", paste0(title_x, ":")), round(c(t_45[3], t_90[3], t_180[3]), digits = 0),
                                                paste("<br>", paste0(title_y, ":")), round(c(45, 90, 180), digits = 0))) %>%
    add_trace(type = "scatter", mode = "lines+markers", x = c(t_45[2], t_90[2], t_180[2]), y =c(45, 90, 180), 
              name = "Wöhlerlinie 50 %", color = I("#ff2e00"), marker = list( symbol="triangle-down"),
              hoverinfo = "text", text = ~paste(paste("Wöhlerlinie 50 %:","<br>", paste0(title_x, ":")), round(c(t_45[2], t_90[2], t_180[2]), digits = 0),
                                                paste("<br>", paste0(title_y, ":")), round(c(45, 90, 180), digits = 0))) %>% 
    config(displayModeBar = F) 
    
  
  return(plot)
}
