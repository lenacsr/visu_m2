library(shiny)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(VIM)

server <- function(input, output, session) {
  
  # ONGLET 1 :
  observeEvent(input$country, {
    available_indicators <- data_clean_glo %>%
      filter(REF_AREA_LABEL %in% input$country, !is.na(value)) %>%
      pull(INDICATOR_LABEL) %>% unique() %>% sort()
    updateSelectInput(session, "indicator", choices = available_indicators, selected = available_indicators[1])
  })
  
  observeEvent(input$indicator, {
    available_ages <- data_clean_glo %>%
      filter(INDICATOR_LABEL %in% input$indicator) %>%
      pull(AGE_LABEL) %>% unique() %>% sort()
    updateSelectInput(session, "age", choices = available_ages, selected = available_ages[1])
  })
  
  data_filtered <- reactive({
    req(input$age, input$sex)
    data_clean_glo %>%
      filter(REF_AREA_LABEL %in% input$country,
             INDICATOR_LABEL %in% input$indicator,
             AGE_LABEL == input$age,
             SEX_LABEL %in% input$sex) %>%
      na.omit() %>%
      mutate(year = as.factor(year))
  })
  
  output$graph <- renderPlot({
    df <- data_filtered()
    if(nrow(df)==0) return(NULL)
    unit_label <- unique(df$UNIT_MEASURE_LABEL)
    y_label <- ifelse(any(grepl("Percentage|Ratio", unit_label, ignore.case=TRUE)), "Valeur (%)", "Valeur")
    
    ggplot(df, aes(x=year, y=value, group=SEX_LABEL, color=SEX_LABEL)) +
      geom_point() + geom_line() +
      labs(title=input$indicator,
           subtitle=paste("Pays :", input$country, "\nÂge :", input$age),
           x="Année", y=y_label, color="Sexe") +
      theme_minimal(base_size=13)
  })
  
  observeEvent(list(input$country, input$indicator, input$age, input$sex), {
    available_years <- data_clean_glo %>%
      filter(REF_AREA_LABEL %in% input$country,
             INDICATOR_LABEL %in% input$indicator,
             AGE_LABEL %in% input$age,
             !is.na(value)) %>%
      pull(year) %>% unique() %>% sort()
    updateSelectInput(session, "year_bar", choices = available_years, selected = tail(available_years,1))
  })
  
  data_bar <- reactive({
    req(input$country, input$indicator, input$age, input$sex, input$year_bar)
    data_clean_glo %>%
      filter(REF_AREA_LABEL %in% input$country,
             INDICATOR_LABEL %in% input$indicator,
             AGE_LABEL %in% input$age,
             SEX_LABEL %in% input$sex,
             year == input$year_bar) %>%
      na.omit()
  })
  
  output$graph_bar <- renderPlot({
    df <- data_bar()
    if(nrow(df)==0) return(NULL)
    min_val <- min(df$value, na.rm=TRUE)
    max_val <- max(df$value, na.rm=TRUE)
    range_val <- max_val - min_val
    ylim_inf <- min_val - 0.1*range_val
    ylim_sup <- max_val + 0.1*range_val
    
    ggplot(df, aes(x=SEX_LABEL, y=value, fill=SEX_LABEL)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=round(value,2)), vjust=-0.5) +
      coord_cartesian(ylim=c(ylim_inf, ylim_sup)) +
      labs(title=input$indicator,
           subtitle=paste(input$country,"-",input$year_bar,"\nÂge :", input$age),
           x="Sexe", y="Valeur (%)", fill="Sexe") +
      theme_minimal(base_size=14)
  })
  
  # ONGLET 2 :
  observeEvent(input$country_compare, {
    indicator_lists <- lapply(input$country_compare, function(p) {
      data_clean_glo %>% filter(REF_AREA_LABEL==p, !is.na(value)) %>% pull(INDICATOR_LABEL) %>% unique()
    })
    common_indicators <- sort(Reduce(intersect, indicator_lists))
    updateSelectInput(session, "indicator_compare", choices=common_indicators)
  })
  
  observeEvent(list(input$country_compare, input$indicator_compare), {
    age_country_map <- data_clean_glo %>%
      filter(REF_AREA_LABEL %in% input$country_compare,
             INDICATOR_LABEL %in% input$indicator_compare) %>%
      select(REF_AREA_LABEL, AGE_LABEL) %>% distinct()
    age_grouped <- age_country_map %>%
      group_by(AGE_LABEL) %>%
      summarise(pays = paste(sort(unique(REF_AREA_LABEL)), collapse=", ")) %>%
      mutate(age_country = paste0(AGE_LABEL," (",pays,")")) %>%
      pull(age_country) %>% sort()
    updateSelectInput(session, "age_compare", choices=age_grouped)
  })
  
  data_compare_filtered <- reactive({
    req(input$country_compare, input$indicator_compare, input$age_compare, input$sex_compare)
    selected_ages <- gsub(" \\(.*\\)$","",input$age_compare)
    data_clean_glo %>%
      filter(REF_AREA_LABEL %in% input$country_compare,
             INDICATOR_LABEL %in% input$indicator_compare,
             AGE_LABEL %in% selected_ages,
             SEX_LABEL %in% input$sex_compare) %>%
      na.omit() %>% mutate(year = as.factor(year))
  })
  
  output$graph_compare <- renderPlot({
    df <- data_compare_filtered()
    if(nrow(df)==0) return(NULL)
    country_palette <- RColorBrewer::brewer.pal(min(8,length(unique(df$REF_AREA_LABEL))), "Set2")
    names(country_palette) <- unique(df$REF_AREA_LABEL)
    sex_linetypes <- c("Female"="solid","Male"="longdash","Total"="twodash")
    sex_shapes <- c("Female"=16,"Male"=17,"Total"=15)
    y_label <- ifelse(any(grepl("Percentage|Ratio", unique(df$UNIT_MEASURE_LABEL))), "Valeur (%)", "Valeur")
    
    ggplot(df, aes(x=year, y=value, group=interaction(REF_AREA_LABEL, SEX_LABEL),
                   color=REF_AREA_LABEL, linetype=SEX_LABEL, shape=SEX_LABEL)) +
      geom_line(linewidth=1) + geom_point(size=3) +
      scale_color_manual(values=country_palette, name="Pays") +
      scale_linetype_manual(values=sex_linetypes, name="Sexe") +
      scale_shape_manual(values=sex_shapes, name="Sexe") +
      labs(title=input$indicator_compare,
           subtitle=paste("Âge(s):",paste(input$age_compare,collapse=","),
                          "\nComparaison entre pays:",paste(input$country_compare,collapse=",")),
           x="Année", y=y_label) +
      theme_minimal(base_size=13) +
      theme(plot.title=element_text(face="bold"),
            plot.subtitle=element_text(size=11,color="gray30"),
            axis.text.x=element_text(angle=45,hjust=1),
            legend.position="bottom", legend.box="horizontal", legend.text=element_text(size=10))
  })
  
  # ONGLET 3 : 
  output$total_obs <- renderText({ format(nrow(data_clean_glo), big.mark=" ") })
  output$nb_pays <- renderText({ length(unique(data_clean_glo$REF_AREA_LABEL)) })
  output$nb_indicateurs <- renderText({ length(unique(data_clean_glo$INDICATOR_LABEL)) })
  output$nb_annees <- renderText({ length(unique(data_clean_glo$year)) })
  
  output$sex_distribution_plot <- renderPlot({
    df <- data_clean_glo %>%
      group_by(SEX_LABEL) %>% summarise(Nombre=n()) %>%
      arrange(desc(Nombre))
    ggplot(df, aes(x=reorder(SEX_LABEL, Nombre), y=Nombre, fill=SEX_LABEL)) +
      geom_bar(stat="identity", width=0.7) +
      geom_text(aes(label=scales::comma(Nombre)), vjust=-0.5) +
      theme_minimal(base_size=13) +
      theme(legend.position="none") +
      labs(x=NULL, y="Nombre d’observations")
  })
  
  output$age_distribution_plot <- renderPlot({
    df <- data_clean_glo %>%
      group_by(AGE_LABEL) %>% summarise(Nombre=n()) %>%
      arrange(desc(Nombre)) %>% head(15)
    ggplot(df, aes(x=reorder(AGE_LABEL, Nombre), y=Nombre, fill=Nombre)) +
      geom_bar(stat="identity") +
      coord_flip() +
      scale_fill_viridis_c(option="plasma") +
      theme_minimal(base_size=12) +
      theme(legend.position="none") +
      labs(x=NULL, y="Nombre d’observations")
  })
  
  output$unit_distribution_plot <- renderPlot({
    df <- data_clean_glo %>%
      group_by(UNIT_MEASURE_LABEL) %>% summarise(Nombre=n()) %>%
      arrange(desc(Nombre))
    ggplot(df, aes(x=reorder(UNIT_MEASURE_LABEL, Nombre), y=Nombre, fill=UNIT_MEASURE_LABEL)) +
      geom_col(width=0.7) +
      coord_flip() +
      theme_minimal(base_size=12) +
      theme(legend.position="none") +
      labs(x=NULL, y="Nombre d’observations")
  })
  
  # ONGLET 3 : 
  output$na_plot <- renderPlot({
    aggr(
      data_clean_glo,
      col = c("skyblue", "orange"),
      numbers = TRUE,
      sortVars = TRUE,
      cex.axis = .7,
      gap = 2,
      ylab = c("Données présentes", "Valeurs manquantes")
    )
  })
  
  output$na_summary <- renderPrint({
    sapply(data_clean_glo, function(x) sum(is.na(x)))
  })
}
