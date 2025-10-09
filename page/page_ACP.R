# ---- page/page_acp.R ----
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(FactoMineR)
library(factoextra)
library(missMDA)

# ---- UI ----
pageACPUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Principal Component Analysis (PCA)"),
    
    wellPanel(
      fluidRow(
        column(6,
               numericInput(ns("min_variance"), "Minimum variance threshold:", 
                            value = 0.1, min = 0, max = 10, step = 0.1)
        ),
        column(6,
               numericInput(ns("max_correlation"), "Maximum correlation threshold:", 
                            value = 0.95, min = 0.5, max = 1, step = 0.05)
        )
      )
    ),
    
    fluidRow(
      column(6,
             h4("Individuals Plot"),
             fluidRow(
               column(6, selectInput(ns("dim_x_ind"), "X axis:", 
                                     choices = paste0("Dim.", 1:5), selected = "Dim.1")),
               column(6, selectInput(ns("dim_y_ind"), "Y axis:", 
                                     choices = paste0("Dim.", 1:5), selected = "Dim.2"))
             ),
             plotlyOutput(ns("plot_individuals"), height = "600px")
      ),
      column(6,
             h4("Variables Plot"),
             fluidRow(
               column(6, selectInput(ns("dim_x_var"), "X axis:", 
                                     choices = paste0("Dim.", 1:5), selected = "Dim.1")),
               column(6, selectInput(ns("dim_y_var"), "Y axis:", 
                                     choices = paste0("Dim.", 1:5), selected = "Dim.2"))
             ),
             plotlyOutput(ns("plot_variables"), height = "600px")
      )
    )
  )
}

# ---- SERVER ----
pageACPServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Variable selection and data preparation
    data_prepared <- reactive({
      
      # Aggregate across ALL years
      df <- dataset |>
        filter(SEX_LABEL %in% c("Male", "Female")) |>
        group_by(REF_AREA_LABEL, SEX_LABEL, INDICATOR_LABEL) |>
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
        mutate(
          Individual = paste(REF_AREA_LABEL, SEX_LABEL, sep = "_")
        ) |>
        select(Individual, INDICATOR_LABEL, value) |>
        pivot_wider(names_from = INDICATOR_LABEL, values_from = value)
      
      # Convert to data.frame with rownames
      df_final <- as.data.frame(df)
      rownames(df_final) <- df_final$Individual
      df_final <- df_final |> select(-Individual)
      
      # Remove columns with too many NAs (>70%)
      na_prop <- colSums(is.na(df_final)) / nrow(df_final)
      df_final <- df_final[, na_prop < 0.7]
      
      # Remove rows with too many NAs (>70%)
      na_prop_rows <- rowSums(is.na(df_final)) / ncol(df_final)
      df_final <- df_final[na_prop_rows < 0.7, ]
      
      req(nrow(df_final) >= 10, ncol(df_final) >= 2)
      
      # Variable selection based on variance and correlation
      
      # 1. Remove low variance variables
      variances <- apply(df_final, 2, var, na.rm = TRUE)
      df_filtered <- df_final[, variances > input$min_variance]
      
      # 2. Impute missing values
      if (any(is.na(df_filtered))) {
        nb_comp <- min(3, ncol(df_filtered) - 1, nrow(df_filtered) - 1)
        res_impute <- imputePCA(df_filtered, ncp = nb_comp)
        df_filtered <- as.data.frame(res_impute$completeObs)
      }
      
      # 3. Remove highly correlated variables (keep the one with higher variance)
      cor_matrix <- cor(df_filtered, use = "complete.obs")
      to_remove <- c()
      
      for (i in 1:(ncol(cor_matrix) - 1)) {
        for (j in (i + 1):ncol(cor_matrix)) {
          if (abs(cor_matrix[i, j]) > input$max_correlation) {
            # Keep the variable with higher variance
            var_i <- var(df_filtered[, i])
            var_j <- var(df_filtered[, j])
            if (var_i > var_j) {
              to_remove <- c(to_remove, j)
            } else {
              to_remove <- c(to_remove, i)
            }
          }
        }
      }
      
      if (length(to_remove) > 0) {
        to_remove <- unique(to_remove)
        df_filtered <- df_filtered[, -to_remove]
      }
      
      req(ncol(df_filtered) >= 2)
      
      df_filtered
    })
    
    # Run PCA
    pca_results <- reactive({
      req(data_prepared())
      
      df <- data_prepared()
      
      # Determine number of components
      n_comp <- min(5, ncol(df), nrow(df) - 1)
      
      # Run PCA
      res_pca <- PCA(df, scale.unit = TRUE, ncp = n_comp, graph = FALSE)
      
      # Extract metadata
      metadata <- data.frame(
        Individual = rownames(df),
        Country = gsub("_Male$|_Female$", "", rownames(df)),
        Sex = ifelse(grepl("_Male$", rownames(df)), "Male", "Female")
      )
      
      list(
        res = res_pca,
        metadata = metadata,
        n_comp = n_comp
      )
    })
    
    # Individuals plot
    output$plot_individuals <- renderPlotly({
      req(pca_results())
      
      pca <- pca_results()$res
      metadata <- pca_results()$metadata
      
      dim_x_num <- as.numeric(gsub("Dim.", "", input$dim_x_ind))
      dim_y_num <- as.numeric(gsub("Dim.", "", input$dim_y_ind))
      
      df_plot <- as.data.frame(pca$ind$coord[, c(dim_x_num, dim_y_num)])
      colnames(df_plot) <- c("X", "Y")
      df_plot <- cbind(df_plot, metadata)
      df_plot$contrib <- pca$ind$contrib[, dim_x_num] + pca$ind$contrib[, dim_y_num]
      
      # Show top 15 contributors
      top_ind <- df_plot |>
        arrange(desc(contrib)) |>
        head(15) |>
        pull(Individual)
      
      df_plot$label <- ifelse(df_plot$Individual %in% top_ind, df_plot$Country, "")
      
      p <- ggplot(df_plot, aes(x = X, y = Y, color = Sex, 
                               text = paste0(Individual, "\nContrib: ", round(contrib, 2)))) +
        geom_point(size = 3, alpha = 0.7) +
        geom_text(aes(label = label), vjust = -0.7, size = 3, show.legend = FALSE) +
        scale_color_manual(values = c("Male" = palette_6[1], "Female" = palette_6[5])) +
        labs(
          x = paste0(input$dim_x_ind, " (", round(pca$eig[dim_x_num, 2], 1), "%)"),
          y = paste0(input$dim_y_ind, " (", round(pca$eig[dim_y_num, 2], 1), "%)"),
          title = "Individuals"
        ) +
        theme_minimal()
      
      ggplotly(p, tooltip = "text")
    })
    
    # Variables plot
    output$plot_variables <- renderPlotly({
      req(pca_results())
      
      pca <- pca_results()$res
      
      dim_x_num <- as.numeric(gsub("Dim.", "", input$dim_x_var))
      dim_y_num <- as.numeric(gsub("Dim.", "", input$dim_y_var))
      
      df_var <- as.data.frame(pca$var$coord[, c(dim_x_num, dim_y_num)])
      colnames(df_var) <- c("X", "Y")
      df_var$Variable <- rownames(df_var)
      
      p <- ggplot(df_var, aes(x = X, y = Y, label = Variable)) +
        geom_segment(aes(x = 0, y = 0, xend = X, yend = Y),
                     arrow = arrow(length = unit(0.3, "cm")),
                     color = palette_6[2], linewidth = 1) +
        geom_text(vjust = -0.5, size = 3) +
        geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
        geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
        coord_fixed() +
        labs(
          x = paste0(input$dim_x_var, " (", round(pca$eig[dim_x_num, 2], 1), "%)"),
          y = paste0(input$dim_y_var, " (", round(pca$eig[dim_y_num, 2], 1), "%)"),
          title = "Variables correlation plot"
        ) +
        theme_minimal()
      
      ggplotly(p)
    })
  })
}