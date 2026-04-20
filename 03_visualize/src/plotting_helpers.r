library(ggplot2)
library(dplyr)

#' Create a Custom Boxplot with Optional Faceting
#'
#' This function generates a standalone boxplot or a single-row facet grid
#' of boxplots to easily compare distributions across different scenarios.
#' It automatically removes x-axis text and ticks to prevent confusion, 
#' and allows detailed control over font sizes for titles and axes.
#'
#' @param data A data frame containing the variables to plot.
#' @param y_var A string specifying the column name for the y-axis variable.
#' @param facet_var A string specifying the column name for the faceting variable (optional). Default is NULL.
#' @param fill_color A string specifying the fill color for the boxplots. Default is "skyblue".
#' @param plot_title A string for the main plot title.
#' @param y_label A string for the y-axis label.
#' @param title_size Numeric. Font size for the main plot title. Default is 14.
#' @param y_title_size Numeric. Font size for the y-axis title. Default is 12.
#' @param y_text_size Numeric. Font size for the y-axis text (numbers). Default is 10.
#' @param facet_text_size Numeric. Font size for the facet strip labels (subtitles). Default is 12.
#'
#' @return A ggplot object.
#' @export
plot_boxplots <- function(data,
                                   y_var,
                                   facet_var=NULL,
                                   fill_color,
                                   plot_title,
                                   y_label,
                                   title_size,
                                   y_title_size,
                                   y_text_size,
                                   facet_text_size,
                                   plot_subtitle=NULL) {
  
  # Initialize the base plot using .data[[]] to evaluate the string column names safely
  p <- ggplot(data, aes(y = .data[[y_var]])) +
    geom_boxplot(fill = fill_color, outlier.color = "black") +
    labs(title = plot_title, y = y_label, subtitle = plot_subtitle) +
    theme_bw() +
    theme(
      # Apply custom font sizes
      plot.title = element_text(size = title_size, face = "bold"),
      axis.title.y = element_text(size = y_title_size),
      axis.text.y = element_text(size = y_text_size),
      
      # Remove x-axis text, ticks, and titles to prevent confusion
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    )
  
  # Add faceting if a facet variable was provided
  if (!is.null(facet_var)) {
    p <- p + 
      # nrow = 1 forces them onto the same row; y-axis scales are fixed by default
      facet_wrap(vars(.data[[facet_var]]), nrow = 1) + 
      theme(
        strip.text = element_text(size = facet_text_size),
        strip.background = element_rect(fill = "grey95") # Clean background for subtitles
      )
  }
  
  return(p)
}


#' Create a Custom Horizontal Bar Plot with Overflow Control
#'
#' @param right_padding Numeric. Multiplier for the empty space on the right. 
#' Increase this (e.g., to 0.6 or 0.8) if text is getting cut off.
plot_bar_graph <- function(data,
                           num_var,
                           category_var,
                           fill_color,
                           plot_title,
                           title_size,
                           bar_label_size,
                           right_padding = 0.6) { # Default padding
  
  ggplot(data, aes(x = .data[[num_var]], 
                   y = reorder(.data[[category_var]], .data[[num_var]]))) +
    geom_col(fill = fill_color, color = "black") +
    
    # Combined labels
    geom_text(aes(label = paste0(.data[[category_var]], ": ", round(.data[[num_var]], 1), "%")), 
              hjust = -0.1, 
              size = bar_label_size) +
    
    labs(title = plot_title) +
    theme_bw() +
    theme(
      plot.title = element_text(size = title_size, face = "bold"),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line.y = element_line(color = "black"),
      # Added: Increase the plot margin on the right to accommodate 'clip = "off"'
      plot.margin = margin(5, 50, 5, 5) 
    ) +
    # Use the parameter here to control the 'white space' on the right
    scale_x_continuous(expand = expansion(mult = c(0, right_padding))) +
    
    # CRITICAL: Allows labels to be drawn even if they technically exit the plot panel
    coord_cartesian(clip = "off")
}