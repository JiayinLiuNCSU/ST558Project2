library(shiny)
library(bslib)
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(scales)
library(DT)
library(shinycssloaders)
library(ggridges)
library(ggExtra)
library(rsconnect)

# Data preparation
dt <- readxl::read_excel("US_Superstore_data.xls")
dt_date <- dt %>%
  clean_names() %>%
  mutate(sales = as.numeric(sales),
         profit = as.numeric(profit),
         discount = as.numeric(discount),
         quantity = as.numeric(quantity),
         postal_code= as.character(postal_code)) %>% 
  mutate(order_date = as.Date(order_date),
         ship_date = as.Date(ship_date),
         order_year = year(order_date),
         order_month = month(order_date,label = TRUE,abbr = TRUE,locale = "C"),
         order_year_month = floor_date(order_date, unit = "month"),
         shipping_days = as.numeric(ship_date - order_date),
         profit_status = case_when(profit > 0 ~ "Profit",
                                   profit == 0 ~ "Break Even",
                                   profit < 0 ~ "Loss",
                                   TRUE ~ NA))

categorical_choices <- c("Product Category" = "category",
                         "Product Sub-Category" = "sub_category",
                         "Customer Segment" = "segment",
                         "Region" = "region",
                         "Ship Mode" = "ship_mode",
                         "Profit Status" = "profit_status",
                         "Order Year" = "order_year")

numeric_choices <- c("Sales" = "sales",
                     "Quantity" = "quantity",
                     "Discount" = "discount",
                     "Profit" = "profit",
                     "Shipping Days" = "shipping_days")

image_folder <- normalizePath(file.path(getwd(), "www"),winslash = "/",mustWork = TRUE)

addResourcePath(prefix = "images",directoryPath = image_folder)


ui <- page_sidebar(
  title = "Superstore Sales and Profit Explorer",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    width = 330,
    h4("Subset the data"),
    p("Choose filters, then press Apply Filters. The app will not update until the button is pressed.",
      class = "text-muted"),
    
    checkboxGroupInput(inputId = "category_filter",
                       label = "Product Category",
                       choices = sort(unique(dt_date$category)),
                       selected = sort(unique(dt_date$category))),
    
    checkboxGroupInput(inputId = "segment_filter",
                       label = "Customer Segment",
                       choices = sort(unique(dt_date$segment)),
                       selected = sort(unique(dt_date$segment))),
    
    selectInput(inputId = "numeric_filter1",
                label = "First numeric filter",
                choices = numeric_choices,
                selected = "sales"),
    
    uiOutput("numeric_slider1_ui"),
    
    selectInput(inputId = "numeric_filter2",
                label = "Second numeric filter",
                choices = numeric_choices,
                selected = "profit"),
    
    uiOutput("numeric_slider2_ui"),
    
    actionButton(inputId = "apply_filters",
                 label = "Apply Filters",
                 icon = icon("filter"),
                 class = "btn-primary w-100"),
    
    hr(),
    textOutput("filter_status")),
  
  navset_card_tab(id = "main_tabs",
                  nav_panel("About",
                            card(card_header("Purpose of the app"),
                                 p("This app investigates the factors related to sales and profitability in the U.S. Superstore data. "),
                                 tags$ul(tags$li("Compare sales and profit across product categories, customer segments, and regions."),
                                         tags$li("Investigate whether larger discounts are associated with lower profit."),
                                         tags$li("Examine how sales and profit change over time."),
                                         tags$li("Create categorical and numerical summaries from a user-selected subset of the data."))),
                            
                            layout_columns(
                              card(card_header("Data source"),
                                   p("The data contain U.S. Superstore order-item records, including order dates, customer segments, "),
                                   p("product categories, sales, discounts, quantities, and profit."),
                                   tags$a(href = "https://www.kaggle.com/datasets/juhi1994/superstore",
                                          "View the dataset information on Kaggle",
                                          target = "_blank")),
                              
                              card(
                                card_header("How to use the app"),
                                tags$ol(
                                  tags$li("Use the sidebar to select categories, customer segments, and two numeric ranges."),
                                  tags$li("Press Apply Filters to update the data used throughout the app."),
                                  tags$li("Use Data Download to review and save the filtered observations."),
                                  tags$li("Use Data Exploration to create tables, numerical summaries, and plots."))),
                              col_widths = c(6, 6)),
                            
                            card(card_header("Superstore data"),
                                 tags$figure(
                                   style = "text-align: center;",
                                   tags$img(src = "images/Stopnshop.png",
                                            alt = "Superstore illustration",
                                            style = "width: 650px; max-width: 100%; height: auto;"),
                                   
                                   tags$figcaption("Image source: ",
                                                   tags$a(
                                                     href = "https://family-guy-the-quest-for-stuff.fandom.com/wiki/Super_Cowboy_USA_Cleaners",
                                                     "Family Guy: The Quest for Stuff Wiki",
                                                     target = "_blank"))))),
                  
                  nav_panel("Data Download",
                            card(card_header("Filtered Superstore data"),
                                 p("The table below uses the subset created by the sidebar filters."),
                                 downloadButton(outputId = "download_data",
                                                label = "Download Filtered Data",
                                                class = "btn-success"),
                                 br(),
                                 br(),
                                 shinycssloaders::withSpinner(DTOutput("data_table"),type = 6))),
                  
                  nav_panel("Featured Plots",
                            layout_columns(card(
                              card_header("Choose a featured plot"),
                              selectInput(inputId = "featured_plot_type",
                                          label = "Plot",
                                          choices = c("Discount and Profit by Product Category" = "discount_profit",
                                                      "Monthly Sales Trends by Product Category" = "monthly_sales",
                                                      "Profit Distributions Across Product Sub-Categories" = "ridgeline",
                                                      "Sales and Profit with Marginal Distributions" = "marginal"),
                                          selected = "discount_profit"),
                              uiOutput("featured_plot_description")),
                              
                              card(card_header("Featured visualization"),
                                   shinycssloaders::withSpinner(
                                     plotOutput("featured_plot", height = "650px"),type = 6)),col_widths = c(4, 8))),
                  
                  nav_panel("Data Exploration",
                            
                            card(card_header("Choose the type of exploration"),
                                 radioButtons(inputId = "summary_type",
                                              label = NULL,
                                              choices = c("Categorical summaries" = "categorical",
                                                          "Numerical summaries" = "numerical"),
                                              selected = "categorical",
                                              inline = TRUE)),
                            
                            conditionalPanel(
                              condition = "input.summary_type == 'categorical'",
                              
                              layout_columns(
                                card(card_header("Categorical controls"),
                                     selectInput(
                                       inputId = "cat_var1",
                                       label = "Primary categorical variable",
                                       choices = categorical_choices,
                                       selected = "category"),
                                     selectInput(
                                       inputId = "cat_var2",
                                       label = "Second categorical variable",
                                       choices = c("None" = "none", categorical_choices),
                                       selected = "segment"),
                                     radioButtons(
                                       inputId = "cat_display",
                                       label = "Display",
                                       choices = c("Counts" = "count", "Percentages" = "percent"),
                                       selected = "count",inline = TRUE)),
                                
                                card(
                                  card_header("Categorical summary table"),
                                  shinycssloaders::withSpinner(tableOutput("categorical_table"),type = 6)),col_widths = c(4, 8)),
                              
                              card(
                                card_header("Categorical graph"),
                                shinycssloaders::withSpinner(plotOutput("categorical_plot", height = "500px"),type = 6))),
                            
                            conditionalPanel(
                              condition = "input.summary_type == 'numerical'",
                              
                              layout_columns(
                                card(card_header("Numerical controls"),
                                     selectInput(
                                       inputId = "summary_numeric_var",
                                       label = "Numeric variable to summarize",
                                       choices = numeric_choices,
                                       selected = "sales"
                                     ),
                                     selectInput(
                                       inputId = "summary_group_var",
                                       label = "Summarize across levels of",
                                       choices = c("No grouping" = "none", categorical_choices),
                                       selected = "category"
                                     ),
                                     selectInput(
                                       inputId = "numeric_plot_type",
                                       label = "Graph type",
                                       choices = c(
                                         "Histogram" = "histogram",
                                         "Box plot" = "boxplot",
                                         "Scatterplot" = "scatterplot"
                                       ),
                                       selected = "boxplot"
                                     ),
                                     conditionalPanel(
                                       condition = "input.numeric_plot_type == 'scatterplot'",
                                       selectInput(
                                         inputId = "scatter_y_var",
                                         label = "Second numeric variable",
                                         choices = numeric_choices,
                                         selected = "profit"
                                       )
                                     ),
                                     selectInput(
                                       inputId = "plot_color_var",
                                       label = "Color/group variable",
                                       choices = c("None" = "none", categorical_choices),
                                       selected = "segment"
                                     ),
                                     selectInput(
                                       inputId = "plot_facet_var",
                                       label = "Facet variable",
                                       choices = c("None" = "none", categorical_choices),
                                       selected = "none"
                                     )
                                ),
                                
                                card(
                                  card_header("Numerical summary table"),
                                  shinycssloaders::withSpinner(
                                    tableOutput("numerical_table"),
                                    type = 6
                                  )
                                ),
                                col_widths = c(4, 8)
                              ),
                              
                              card(
                                card_header("Numerical graph"),
                                shinycssloaders::withSpinner(
                                  plotOutput("numerical_plot", height = "520px"),
                                  type = 6))
                            )
                  )
  )
)


server <- function(input, output, session) {
  
  output$numeric_slider1_ui <- renderUI({
    req(input$numeric_filter1)
    x <- dt_date[[input$numeric_filter1]]
    x <- x[is.finite(x)]
    
    sliderInput(
      inputId = "numeric_range1",
      label = paste("Range for", names(numeric_choices)[numeric_choices == input$numeric_filter1]),
      min = floor(min(x)),
      max = ceiling(max(x)),
      value = c(floor(min(x)), ceiling(max(x))))
  })
  
  output$numeric_slider2_ui <- renderUI({
    req(input$numeric_filter2)
    x <- dt_date[[input$numeric_filter2]]
    x <- x[is.finite(x)]
    
    sliderInput(
      inputId = "numeric_range2",
      label = paste("Range for", names(numeric_choices)[numeric_choices == input$numeric_filter2]),
      min = floor(min(x)),
      max = ceiling(max(x)),
      value = c(floor(min(x)), ceiling(max(x))))
  })
  
  filtered_data <- reactiveVal(dt_date)
  
  observeEvent(input$apply_filters, {
    req(input$category_filter,
        input$segment_filter,
        input$numeric_filter1,
        input$numeric_filter2,
        input$numeric_range1,
        input$numeric_range2)
    
    dat <- dt_date %>%
      filter(category %in% input$category_filter,
             segment %in% input$segment_filter)
    
    dat <- dat %>%
      filter(
        .data[[input$numeric_filter1]] >= input$numeric_range1[1],
        .data[[input$numeric_filter1]] <= input$numeric_range1[2],
        .data[[input$numeric_filter2]] >= input$numeric_range2[1],
        .data[[input$numeric_filter2]] <= input$numeric_range2[2]
      )
    
    if (nrow(dat) == 0) {
      showNotification(
        "No rows remain after filtering. Please use wider filter selections.",
        type = "error")
      return()
    }
    
    filtered_data(dat)
  })
  
  output$filter_status <- renderText({
    paste(
      format(nrow(filtered_data()), big.mark = ","),
      "rows are currently available."
    )
  })
  
  output$data_table <- renderDT({
    datatable(filtered_data(),
              filter = "top",
              rownames = FALSE,
              options = list(pageLength = 15,scrollX = TRUE))
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("filtered_superstore_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  output$featured_plot_description <- renderUI({
    req(input$featured_plot_type)
    
    descriptions <- list(
      discount_profit = tagList(
        p(strong("Purpose:"), " Examine whether larger discounts are associated with lower profit and whether this relationship differs across product categories and customer segments."),
        p(strong("How to read it:"), " Points below the dashed horizontal line represent losses. The smooth curves summarize the general discount-profit pattern within each segment."),
        p(class = "text-muted", "This plot uses discount, profit, customer segment, and product category, and it satisfies the faceting requirement.")
      ),
      monthly_sales = tagList(
        p(strong("Purpose:"), " Examine how total monthly sales change over time for each product category."),
        p(strong("How to read it:"), " Compare the lines to identify growth, seasonal peaks, unusually high or low months, and differences among categories."),
        p(class = "text-muted", "The data are aggregated by month after the sidebar filters are applied.")
      ),
      ridgeline = tagList(
        p(strong("Purpose:"), " Compare the full profit distributions across product sub-categories."),
        p(strong("How to read it:"), " Density to the right of zero represents profitable transactions; density to the left represents losses. Wider curves indicate more variability."),
        p(class = "text-muted", "Sub-categories are ordered by median profit. The visible range is limited to the 1st through 99th percentiles for readability.")
      ),
      marginal = tagList(
        p(strong("Purpose:"), " Examine the relationship between sales and profit while also viewing the separate distributions of both variables."),
        p(strong("How to read it:"), " The center scatterplot shows the sales-profit relationship. The marginal density curves compare the sales and profit distributions across product categories."),
        p(class = "text-muted", "The sales axis uses log10 values, so a one-unit increase represents approximately ten times as much sales.")))
    
    descriptions[[input$featured_plot_type]]
  })
  
  output$featured_plot <- renderPlot({
    dat <- filtered_data()
    req(input$featured_plot_type)
    
    validate(
      need(nrow(dat) > 1, "The current subset does not contain enough rows to create this plot.")
    )
    
    profit_limits <- quantile(dat$profit,
                              probs = c(0.01, 0.99),
                              na.rm = TRUE,
                              names = FALSE)
    
    if (input$featured_plot_type == "discount_profit") {
      plot_dat <- dat %>%
        drop_na(discount, profit, category, segment)
      
      validate(
        need(nrow(plot_dat) > 10, "The current subset does not contain enough complete observations for this plot.")
      )
      
      ggplot(
        plot_dat,
        aes(x = discount,
            y = profit,
            color = segment)) +
        geom_point(alpha = 0.3) +
        geom_smooth(method = "loess",se = FALSE,linewidth = 0.8) +
        geom_hline(yintercept = 0,
                   linetype = "dashed") +
        facet_wrap(~ category) +
        coord_cartesian(ylim = profit_limits) +
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::dollar) +
        labs(title = "Relationship Between Discount and Profit by Product Category",
             subtitle = "Smooth curves show the general pattern for each customer segment",
             x = "Discount",
             y = "Profit",
             color = "Customer Segment") +
        theme_minimal() +
        theme(legend.position = "bottom")
    }
    
    else if (input$featured_plot_type == "monthly_sales") {
      plot_dat <- dat %>%
        drop_na(order_year_month, category, sales) %>%
        group_by(order_year_month, category) %>%
        summarize(total_sales = sum(sales, na.rm = TRUE),.groups = "drop")
      
      validate(
        need(nrow(plot_dat) > 1, "The current subset does not contain enough months to create a trend plot.")
      )
      
      ggplot(plot_dat,
             aes(x = order_year_month,
                 y = total_sales,
                 color = category)) +
        geom_line(linewidth = 0.9) +
        geom_point(size = 1.2) +
        scale_x_date(date_breaks = "6 months",
                     labels = scales::label_date(format = "%b %Y",locale = "C")) +
        scale_y_continuous(labels = scales::dollar) +
        labs(
          title = "Monthly Sales Trends by Product Category",
          subtitle = "Total sales are aggregated by month",
          x = "Order Month",
          y = "Total Monthly Sales",
          color = "Product Category"
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    else if (input$featured_plot_type == "ridgeline") {
      plot_dat <- dat %>%
        drop_na(sub_category, profit) %>%
        dplyr::filter(profit >= profit_limits[1],
                      profit <= profit_limits[2])
      
      validate(
        need(nrow(plot_dat) > 10, "The current subset does not contain enough observations for the ridgeline plot."),
        need(dplyr::n_distinct(plot_dat$sub_category) > 1, "Select data containing at least two product sub-categories.")
      )
      
      ggplot(plot_dat,
             aes(x = profit,
                 y = forcats::fct_reorder(
                   sub_category,
                   profit,
                   median),
                 fill = after_stat(x))) +
        ggridges::geom_density_ridges_gradient(scale = 2.5,rel_min_height = 0.01) +
        geom_vline(xintercept = 0,linetype = "dashed") +
        scale_x_continuous(labels = scales::dollar) +
        scale_fill_viridis_c(labels = scales::dollar) +
        labs(
          title = "Profit Distributions Across Product Sub-Categories",
          subtitle = "Sub-categories are ordered by median profit",
          x = "Profit",
          y = "Product Sub-Category",
          fill = "Profit"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
    }
    
    else if (input$featured_plot_type == "marginal") {
      plot_dat <- dat %>%
        dplyr::filter(sales > 0,!is.na(profit),!is.na(category))
      
      validate(
        need(nrow(plot_dat) > 10, "The current subset does not contain enough observations for the marginal plot."),
        need(dplyr::n_distinct(plot_dat$category) > 1, "Select data containing at least two product categories.")
      )
      
      base_plot <- ggplot(
        plot_dat,
        aes(x = log10(sales),
            y = profit,
            color = category)) +
        geom_point(alpha = 0.35) +
        geom_hline(yintercept = 0,linetype = "dashed") +
        coord_cartesian(ylim = profit_limits) +
        labs(
          title = "Sales and Profit with Marginal Distributions",
          subtitle = "Marginal density curves show the distributions of sales and profit",
          x = "Log10 Sales",
          y = "Profit",
          color = "Product Category"
        ) +
        theme_minimal() +
        theme(legend.position = "right")
      
      print(
        ggExtra::ggMarginal(
          base_plot,
          type = "density",
          groupColour = TRUE,
          groupFill = TRUE,
          alpha = 0.3
        )
      )
    }
  })
  
  
  categorical_summary <- reactive({
    dat <- filtered_data()
    req(input$cat_var1, input$cat_var2, input$cat_display)
    
    if (input$cat_var2 == "none" || input$cat_var2 == input$cat_var1) {
      out <- dat %>%
        drop_na(all_of(input$cat_var1)) %>%
        group_by(.data[[input$cat_var1]]) %>%
        summarize(count = n(), .groups = "drop")
      
      if (input$cat_display == "percent") {
        out <- out %>%
          mutate(percent = count / sum(count)) %>%
          select(-count)
      }
    } else {
      out <- dat %>%
        drop_na(all_of(c(input$cat_var1, input$cat_var2))) %>%
        group_by(
          .data[[input$cat_var1]],
          .data[[input$cat_var2]]
        ) %>%
        summarize(count = n(), .groups = "drop")
      
      if (input$cat_display == "percent") {
        out <- out %>%
          group_by(.data[[input$cat_var1]]) %>%
          mutate(value = count / sum(count)) %>%
          ungroup() %>%
          select(-count)
      } else {
        out <- out %>%
          rename(value = count)
      }
      
      out <- out %>%
        pivot_wider(
          names_from = all_of(input$cat_var2),
          values_from = value,
          values_fill = 0
        )
    }
    
    out
  })
  
  output$categorical_table <- renderTable({
    out <- categorical_summary()
    
    if (input$cat_display == "percent") {
      out %>%
        mutate(across(where(is.numeric),~ scales::percent(.x, accuracy = 0.1)))
    } else {
      out
    }
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$categorical_plot <- renderPlot({
    dat <- filtered_data()
    req(input$cat_var1, input$cat_var2, input$cat_display)
    
    if (input$cat_var2 == "none" || input$cat_var2 == input$cat_var1) {
      plot_dat <- dat %>%
        drop_na(all_of(input$cat_var1)) %>%
        count(.data[[input$cat_var1]], name = "count")
      
      if (input$cat_display == "percent") {
        plot_dat <- plot_dat %>%
          mutate(value = count / sum(count))
        y_label <- "Percent of Order Items"
      } else {
        plot_dat <- plot_dat %>%
          mutate(value = count)
        y_label <- "Number of Order Items"
      }
      
      p <- ggplot(
        plot_dat,
        aes(
          x = .data[[input$cat_var1]],
          y = value
        )
      ) +
        geom_col() +
        labs(
          title = paste("Distribution of", names(categorical_choices)[categorical_choices == input$cat_var1]),
          x = names(categorical_choices)[categorical_choices == input$cat_var1],
          y = y_label
        )
    } else {
      plot_dat <- dat %>%
        drop_na(all_of(c(input$cat_var1, input$cat_var2))) %>%
        count(
          .data[[input$cat_var1]],
          .data[[input$cat_var2]],
          name = "count"
        )
      
      if (input$cat_display == "percent") {
        plot_dat <- plot_dat %>%
          group_by(.data[[input$cat_var1]]) %>%
          mutate(value = count / sum(count)) %>%
          ungroup()
        y_label <- "Percent Within Primary Variable"
      } else {
        plot_dat <- plot_dat %>%
          mutate(value = count)
        y_label <- "Number of Order Items"
      }
      
      p <- ggplot(
        plot_dat,
        aes(
          x = .data[[input$cat_var1]],
          y = value,
          fill = .data[[input$cat_var2]]
        )
      ) +
        geom_col(position = "dodge") +
        labs(
          title = paste(
            names(categorical_choices)[categorical_choices == input$cat_var1],
            "by",
            names(categorical_choices)[categorical_choices == input$cat_var2]
          ),
          x = names(categorical_choices)[categorical_choices == input$cat_var1],
          y = y_label,
          fill = names(categorical_choices)[categorical_choices == input$cat_var2]
        )
    }
    
    if (input$cat_display == "percent") {
      p <- p + scale_y_continuous(labels = scales::percent)
    }
    
    p +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "bottom"
      )
  })
  
  numerical_summary <- reactive({
    dat <- filtered_data()
    req(input$summary_numeric_var, input$summary_group_var)
    
    if (input$summary_group_var == "none") {
      dat %>%
        summarize(
          variable = names(numeric_choices)[numeric_choices == input$summary_numeric_var],
          mean = mean(.data[[input$summary_numeric_var]], na.rm = TRUE),
          median = median(.data[[input$summary_numeric_var]], na.rm = TRUE),
          sd = sd(.data[[input$summary_numeric_var]], na.rm = TRUE),
          IQR = IQR(.data[[input$summary_numeric_var]], na.rm = TRUE)
        )
    } else {
      dat %>%
        drop_na(all_of(input$summary_group_var)) %>%
        group_by(.data[[input$summary_group_var]]) %>%
        summarize(
          mean = mean(.data[[input$summary_numeric_var]], na.rm = TRUE),
          median = median(.data[[input$summary_numeric_var]], na.rm = TRUE),
          sd = sd(.data[[input$summary_numeric_var]], na.rm = TRUE),
          IQR = IQR(.data[[input$summary_numeric_var]], na.rm = TRUE),
          .groups = "drop"
        )
    }
  })
  
  output$numerical_table <- renderTable({
    numerical_summary() %>%
      mutate(
        across(
          where(is.numeric),
          ~ round(.x, 2)
        )
      )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$numerical_plot <- renderPlot({
    dat <- filtered_data()
    req(input$summary_numeric_var, input$numeric_plot_type)
    
    color_mapping <- if (input$plot_color_var == "none") {
      aes()
    } else {
      aes(color = .data[[input$plot_color_var]])
    }
    
    if (input$numeric_plot_type == "histogram") {
      p <- ggplot(
        dat,
        aes(x = .data[[input$summary_numeric_var]])
      ) +
        geom_histogram(bins = 35, color = "white") +
        labs(
          title = paste("Distribution of", names(numeric_choices)[numeric_choices == input$summary_numeric_var]),
          x = names(numeric_choices)[numeric_choices == input$summary_numeric_var],
          y = "Number of Order Items"
        )
    }
    
    if (input$numeric_plot_type == "boxplot") {
      validate(
        need(input$summary_group_var != "none", "Choose a grouping variable to create a box plot.")
      )
      
      p <- ggplot(
        dat,
        aes(
          x = .data[[input$summary_group_var]],
          y = .data[[input$summary_numeric_var]]
        )
      ) +
        geom_boxplot(
          aes(fill = .data[[input$summary_group_var]]),
          show.legend = FALSE,
          outlier.alpha = 0.25
        ) +
        labs(
          title = paste(
            names(numeric_choices)[numeric_choices == input$summary_numeric_var],
            "by",
            names(categorical_choices)[categorical_choices == input$summary_group_var]
          ),
          x = names(categorical_choices)[categorical_choices == input$summary_group_var],
          y = names(numeric_choices)[numeric_choices == input$summary_numeric_var]
        )
    }
    
    if (input$numeric_plot_type == "scatterplot") {
      req(input$scatter_y_var)
      validate(
        need(
          input$scatter_y_var != input$summary_numeric_var,
          "Choose two different numeric variables for the scatterplot."
        )
      )
      
      p <- ggplot(
        dat,
        aes(x = .data[[input$summary_numeric_var]],
            y = .data[[input$scatter_y_var]])) +
        geom_point(mapping = color_mapping,
                   alpha = 0.45) +
        geom_smooth(method = "loess",
                    se = FALSE,
                    linewidth = 0.8) +
        labs(
          title = paste(names(numeric_choices)[numeric_choices == input$summary_numeric_var],
                        "and",
                        names(numeric_choices)[numeric_choices == input$scatter_y_var]),
          x = names(numeric_choices)[numeric_choices == input$summary_numeric_var],
          y = names(numeric_choices)[numeric_choices == input$scatter_y_var],
          color = if (input$plot_color_var == "none") NULL else
            names(categorical_choices)[categorical_choices == input$plot_color_var])
    }
    
    if (input$plot_facet_var != "none") {
      p <- p + facet_wrap(
        vars(!!rlang::sym(input$plot_facet_var)),
        scales = "free"
      )
    }
    
    p +theme_minimal() +
      theme(axis.text.x = element_text(angle = 35, hjust = 1),
            legend.position = "bottom")
  })
}

shinyApp(ui = ui, server = server)


