case_analysis <- function(model, type, term = NULL, id = TRUE) {
  # John's case analysis function updated by KW 10/4/24
  # type can be hat_values, residuals, cooks_d, df_betas, influence_plot, cov_ratio
  # term can be used to pass in a single parameter for the df_betas plot
  # Influence plot will label points with row id by default. Can turn off by setting id to FALSE
  
  model_formula <- as.character(model$call$formula)
  
  plot_hist <- function(data) {
    data |> 
      ggplot(aes(x = x)) +
      geom_histogram(fill = "light grey", color = "black", bins = 10) +
      geom_rug(color = "blue") +
      labs(x = str_to_sentence(str_replace(type, "_", " ")),
           y = "Frequency",
           title = str_c("Model:", model_formula[2], model_formula[1], model_formula[3],
                         sep = " "))
  }
  
  if(type == "hat_values") {
    p <- tibble(x = hatvalues(model)) |> 
      plot_hist() +
      geom_vline(xintercept = 2*mean(hatvalues(model)), color = "red", linewidth = .75) +
      labs(subtitle = "Cut point: 2 * mean(hat)")
    print(p)
  }
  
  if(type == "residuals") {
    t_cut_point <- qt(p = .025/nrow(data), 
                      df = nrow(data) - length(coef(model)) - 1 - 2, 
                      lower.tail = FALSE)
    
    p <- tibble(x = rstudent(model)) |> 
      plot_hist() +
      geom_vline(xintercept = c(-1, 1) * t_cut_point, color = "red", linewidth = .75) +
      labs(subtitle = "Bonferroni corrected p-value < 0.05 in red")
    print(p)
  }
  
  if(type == "cooks_d") {
    p <- tibble(x = cooks.distance(model)) |> 
      plot_hist() +
      geom_vline(xintercept = 4/(nrow(data) - length(coef(model)) - 1 - 1),
                 color = c("red"), linewidth = .75) +
      labs(subtitle = "4/(N-P) cut-off in red")
    print(p)
  }
  
  if(type == "df_betas") {
    m_1_coefs <- if(!is.null(term))term else names(as_tibble(dfbetas(model)))
    i <- 0
    for (term in m_1_coefs) {
      i <- i + 1
      p <- dfbetas(model) |>
        as_tibble() |>
        select(x = !!sym(term)) |>
        plot_hist() +
        labs(x = str_c("DFBETAS: ", term),
             subtitle = str_c("B = ", round(coef(model)[i], 6)))
      print(p)
    }
  }
  
  if(type == "cov_ratio") {
    p <- tibble(x = covratio(model)) |>
      plot_hist() +
      geom_vline(xintercept = abs((3 * (length(coef(model)))/nrow(data)) - 1), 
                 color = "red", linewidth = .75) +
      labs(subtitle = "3*(P/N) - 1 cut-off in red")
    print(p)
  }
  
  if(type == "influence_plot") {
    t_cut_point <- qt(p = .025/nrow(data), 
                      df = nrow(data) - length(coef(model)) - 1 - 2, 
                      lower.tail = FALSE)
    p <- tibble(residuals = rstudent(model),
             hat_values = hatvalues(model)) |>
      ggplot(aes(x = hat_values, y = residuals)) +
      geom_point(size = 10 * sqrt(cooks.distance(model))/max(cooks.distance(model)),
                 shape = 1) +
      geom_hline(yintercept = c(-1, 0, 1) * t_cut_point, color = "red", 
                 linetype = c("solid", "dashed", "solid"), linewidth = .75) +
      geom_vline(xintercept = c(1, 2, 3) * mean(hatvalues(model)), color = "red", 
                 linetype = c("dashed", "solid", "solid"), linewidth = .75) +
      labs(title = "Influence Bubble Plot",
           subtitle = str_c("Model:", model_formula[2], model_formula[1], model_formula[3],
                            sep = " "),
           x = "Hat Values",
           y = "Studentized Residuals")
    
    if(id) {
      p <- p +
        geom_text(aes(label = names(cooks.distance(model))), vjust = 0, hjust = 1)
    }
    print(p)
  }
  
}
