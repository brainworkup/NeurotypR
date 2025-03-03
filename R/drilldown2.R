drilldown2 <- function(data, patient, neuro_domain = c(
                         "Neuropsychological Test Scores",
                         "Behavioral Rating Scales",
                         "Effort/Validity Test Scores"
                       ), theme) {
  # Create 3 levels of dataframes for drilldown ----------------------------------
  ## Level 1 -------------------------------------------------------
  ## PASS scores
  # 1. create mean z-scores for domain
  df1 <- data |>
    dplyr::group_by(pass) |>
    dplyr::summarize(
      zMean = mean(z, na.rm = TRUE),
      zPct = mean(percentile, na.rm = TRUE)
    ) |>
    dplyr::mutate(range = NA) |>
    ungroup() # NOTE this is new


  df1$zMean <- round(df1$zMean, 2L)
  df1$zPct <- round(df1$zPct, 0L)
  df1 <-
    df1 |>
    dplyr::mutate(
      range = dplyr::case_when(
        zPct >= 98 ~ "Exceptionally High",
        zPct %in% 91:97 ~ "Above Average",
        zPct %in% 75:90 ~ "High Average",
        zPct %in% 25:74 ~ "Average",
        zPct %in% 9:24 ~ "Low Average",
        zPct %in% 2:8 ~ "Below Average",
        zPct < 2 ~ "Exceptionally Low",
        TRUE ~ as.character(range)
      )
    )

  # 2. sort hi to lo
  df1 <- dplyr::arrange(df1, desc(zPct)) # NOTE this is new

  # 3. create tibble with new column with domain name lowercase
  df_level5_status <- tibble(
    name = df1$pass,
    y = df1$zMean,
    y2 = df1$zPct,
    range = df1$range,
    drilldown = tolower(name)
  )

  ## Level 2 -------------------------------------------------------
  ## Verbal scores
  df_level6_drill <-
    lapply(unique(data$pass), function(x_level) {
      df2 <- subset(data, data$pass %in% x_level)

      # same as above
      df2 <-
        df2 |>
        dplyr::group_by(verbal) |>
        dplyr::summarize(
          zMean = mean(z, na.rm = TRUE),
          zPct = mean(percentile, na.rm = TRUE)
        ) |>
        dplyr::mutate(range = NA) |>
        dplyr::ungroup() # NOTE this is new

      # round z-score to 1 decimal
      df2$zMean <- round(df2$zMean, 2L)
      df2$zPct <- round(df2$zPct, 0L)
      df2 <-
        df2 |>
        dplyr::mutate(
          range = dplyr::case_when(
            zPct >= 98 ~ "Exceptionally High",
            zPct %in% 91:97 ~ "Above Average",
            zPct %in% 75:90 ~ "High Average",
            zPct %in% 25:74 ~ "Average",
            zPct %in% 9:24 ~ "Low Average",
            zPct %in% 2:8 ~ "Below Average",
            zPct < 2 ~ "Exceptionally Low",
            TRUE ~ as.character(range)
          )
        )

      # 2. sort hi to lo
      df2 <- dplyr::arrange(df2, desc(zPct)) # NOTE this is new

      # 3. create tibble with new column with domain name lowercase
      df_level6_status <- tibble(
        name = df2$verbal,
        y = df2$zMean,
        y2 = df2$zPct,
        range = df2$range,
        drilldown = tolower(paste(x_level, name, sep = "_"))
      )

      list(
        id = tolower(x_level),
        type = "column",
        data = list_parse(df_level6_status)
      )
    })

  ## Level 3 -------------------------------------------------------
  ## Timed vs untimed
  df_level7_drill <-
    lapply(unique(data$pass), function(x_level) {
      df2 <- subset(data, data$pass %in% x_level)

      # reuse function but with y_level
      lapply(unique(df2$verbal), function(y_level) {
        # 1. create mean z-scores for subdomain
        # df3 becomes pronoun for domain
        df3 <- subset(df2, df2$verbal %in% y_level)

        df3 <- df3 |>
          dplyr::group_by(timed) |>
          dplyr::summarize(
            zMean = mean(z, na.rm = TRUE),
            zPct = mean(percentile, na.rm = TRUE)
          ) |>
          dplyr::mutate(range = NA) |>
          ungroup() # NOTE this is new

        # round z-score to 1 decimal
        df3$zMean <- round(df3$zMean, 2L)
        df3$zPct <- round(df3$zPct, 0L)
        df3 <-
          df3 |>
          dplyr::mutate(
            range = dplyr::case_when(
              zPct >= 98 ~ "Exceptionally High",
              zPct %in% 91:97 ~ "Above Average",
              zPct %in% 75:90 ~ "High Average",
              zPct %in% 25:74 ~ "Average",
              zPct %in% 9:24 ~ "Low Average",
              zPct %in% 2:8 ~ "Below Average",
              zPct < 2 ~ "Exceptionally Low",
              TRUE ~ as.character(range)
            )
          )

        df3 <- dplyr::arrange(df3, desc(zPct))

        df_level7_status <- tibble(
          name = df3$timed,
          y = df3$zMean,
          y2 = df3$zPct,
          range = df3$range,
          drilldown = tolower(paste(x_level, y_level, name, sep = "_"))
        )

        list(
          id = tolower(paste(x_level, y_level, sep = "_")),
          type = "column",
          data = list_parse(df_level7_status)
        )
      })
    }) |> unlist(recursive = FALSE)

  # Create charts ----------------------------------
  # Theme
  theme <-
    highcharter::hc_theme_merge(
      highcharter::hc_theme_monokai(),
      highcharter::hc_theme_darkunica()
    )

  # Tooltip
  x <- c("Name", "Score", "Percentile", "Range")
  y <- c("{point.name}", "{point.y}", "{point.y2}", "{point.range}")
  tt <- highcharter::tooltip_table(x, y)

  ## Create drilldown bar plot zscores
  plot <-
    highcharter::highchart() |>
    highcharter::hc_title(
      text = patient,
      style = list(fontSize = "15px")
    ) |>
    highcharter::hc_add_series(df_level5_status,
      type = "bar",
      name = neuro_domain,
      highcharter::hcaes(x = name, y = y)
    ) |>
    highcharter::hc_xAxis(
      type = "category",
      title = list(text = "Domain"),
      categories = .$name
    ) |>
    highcharter::hc_yAxis(
      title = list(text = "z-Score (Mean = 0, SD = 1)"),
      labels = list(format = "{value}")
    ) |>
    highcharter::hc_tooltip(
      pointFormat = tt,
      useHTML = TRUE,
      valueDecimals = 1
    ) |>
    highcharter::hc_plotOptions(
      series = list(
        colorByPoint = TRUE,
        allowPointSelect = TRUE,
        dataLabels = TRUE
      )
    ) |>
    highcharter::hc_drilldown(
      allowPointDrilldown = TRUE,
      series = c(
        df_level6_drill,
        df_level7_drill
      )
    ) |>
    highcharter::hc_colorAxis(
      minColor = "red",
      maxColor = "blue"
    ) |>
    highcharter::hc_add_theme(theme) |>
    highcharter::hc_chart(
      style = list(fontFamily = "Cabin"),
      backgroundColor = list("gray")
    )

  return(plot)
}


pass <- function(data, patient, neuro_domain = c(
                   "Neuropsychological Test Scores",
                   "Behavioral Rating Scales",
                   "Effort/Validity Test Scores"
                 ), theme) {
  # Create 4 levels of dataframes for drilldown ----------------------------------
  ## Level 1 -------------------------------------------------------
  ## Domain scores
  # 1. create mean z-scores for domain
  df1 <- data |>
    dplyr::group_by(pass) |>
    dplyr::summarize(
      zMean = mean(z, na.rm = TRUE),
      zPct = mean(percentile, na.rm = TRUE)
    ) |>
    dplyr::mutate(range = NA) |>
    ungroup() # NOTE this is new


  df1$zMean <- round(df1$zMean, 2L)
  df1$zPct <- round(df1$zPct, 0L)
  df1 <-
    df1 |>
    dplyr::mutate(
      range = dplyr::case_when(
        zPct >= 98 ~ "Exceptionally High",
        zPct %in% 91:97 ~ "Above Average",
        zPct %in% 75:90 ~ "High Average",
        zPct %in% 25:74 ~ "Average",
        zPct %in% 9:24 ~ "Low Average",
        zPct %in% 2:8 ~ "Below Average",
        zPct < 2 ~ "Exceptionally Low",
        TRUE ~ as.character(range)
      )
    )

  # 2. sort hi to lo
  df1 <- dplyr::arrange(df1, desc(zPct)) # NOTE this is new

  # 3. create tibble with new column with domain name lowercase
  df_pass_status <- tibble(
    name = df1$pass,
    y = df1$zMean,
    y2 = df1$zPct,
    range = df1$range,
    drilldown = tolower(name)
  )



  ## Level 2 -------------------------------------------------------
  ## Scale scores
  df_scale_drill <-
    lapply(unique(data$pass), function(x_level) {
      df2 <- subset(data, data$pass %in% x_level)

      df2 <- df2 |>
        dplyr::group_by(scale) |>
        dplyr::summarize(
          zMean = mean(z, na.rm = TRUE),
          zPct = mean(percentile, na.rm = TRUE)
        ) |>
        dplyr::mutate(range = NA) |>
        dplyr::ungroup()

      # round z-score to 1 decimal
      df2$zMean <- round(df2$zMean, 2L)
      df2$zPct <- round(df2$zPct, 0L)
      df2 <- df2 |>
        dplyr::mutate(
          range = dplyr::case_when(
            zPct >= 98 ~ "Exceptionally High",
            zPct %in% 91:97 ~ "Above Average",
            zPct %in% 75:90 ~ "High Average",
            zPct %in% 25:74 ~ "Average",
            zPct %in% 9:24 ~ "Low Average",
            zPct %in% 2:8 ~ "Below Average",
            zPct < 2 ~ "Exceptionally Low",
            TRUE ~ as.character(range)
          )
        )

      df2 <- dplyr::arrange(df2, desc(zPct))

      df_scale_status <- tibble(
        name = df2$scale,
        y = df2$zMean,
        y2 = df2$zPct,
        range = df2$range
      )

      list(
        id = tolower(x_level),
        type = "column",
        data = list_parse(df_scale_status)
      )
    })

  # Create charts ----------------------------------
  # Theme
  theme <-
    highcharter::hc_theme_merge(
      highcharter::hc_theme_monokai(),
      highcharter::hc_theme_darkunica()
    )

  # Tooltip
  x <- c("Name", "Score", "Percentile", "Range")
  y <- c("{point.name}", "{point.y}", "{point.y2}", "{point.range}")
  tt <- highcharter::tooltip_table(x, y)

  ## Create drilldown bar plot zscores
  plot <-
    highcharter::highchart() |>
    highcharter::hc_title(
      text = patient,
      style = list(fontSize = "15px")
    ) |>
    highcharter::hc_add_series(df_pass_status,
      type = "bar",
      name = neuro_domain,
      highcharter::hcaes(x = name, y = y)
    ) |>
    highcharter::hc_xAxis(
      type = "category",
      title = list(text = "Domain"),
      categories = df_pass_status$name
    ) |>
    highcharter::hc_yAxis(
      title = list(text = "z-Score (Mean = 0, SD = 1)"),
      labels = list(format = "{value}")
    ) |>
    highcharter::hc_tooltip(
      pointFormat = tt,
      useHTML = TRUE,
      valueDecimals = 1
    ) |>
    highcharter::hc_plotOptions(
      series = list(
        colorByPoint = TRUE,
        allowPointSelect = TRUE,
        dataLabels = TRUE
      )
    ) |>
    highcharter::hc_drilldown(
      allowPointDrilldown = TRUE,
      series = c(
        df_scale_drill
      )
    ) |>
    highcharter::hc_colorAxis(
      minColor = "red",
      maxColor = "blue"
    ) |>
    highcharter::hc_add_theme(theme) |>
    highcharter::hc_chart(
      style = list(fontFamily = "Cabin"),
      backgroundColor = list("gray")
    )

  return(plot)
}



drilldown3 <- function(data, patient, neuro_domain = c(
                         "Neuropsychological Test Scores",
                         "Behavioral Rating Scales",
                         "Effort/Validity Test Scores"
                       ), theme) {
  # Create 4 levels of dataframes for drilldown ----------------------------------
  ## Level 1 -------------------------------------------------------
  ## Domain scores
  # 1. create mean z-scores for domain
  df1 <- data |>
    dplyr::group_by(domain) |>
    dplyr::summarize(
      zMean = mean(z, na.rm = TRUE),
      zPct = mean(percentile, na.rm = TRUE)
    ) |>
    dplyr::mutate(range = NA) |>
    ungroup() # NOTE this is new


  df1$zMean <- round(df1$zMean, 2L)
  df1$zPct <- round(df1$zPct, 0L)
  df1 <-
    df1 |>
    dplyr::mutate(
      range = dplyr::case_when(
        zPct >= 98 ~ "Exceptionally High",
        zPct %in% 91:97 ~ "Above Average",
        zPct %in% 75:90 ~ "High Average",
        zPct %in% 25:74 ~ "Average",
        zPct %in% 9:24 ~ "Low Average",
        zPct %in% 2:8 ~ "Below Average",
        zPct < 2 ~ "Exceptionally Low",
        TRUE ~ as.character(range)
      )
    )

  # 2. sort hi to lo
  df1 <- dplyr::arrange(df1, desc(zPct)) # NOTE this is new

  # 3. create tibble with new column with domain name lowercase
  df_level1_status <- tibble(
    name = df1$domain,
    y = df1$zMean,
    y2 = df1$zPct,
    range = df1$range,
    drilldown = tolower(name)
  )

  ## Level 2 -------------------------------------------------------
  ## Subdomain scores
  ## function to create second level of drilldown (subdomain scores)
  df_level2_drill <-
    lapply(unique(data$domain), function(x_level) {
      df2 <- subset(data, data$domain %in% x_level)

      # same as above
      df2 <-
        df2 |>
        dplyr::group_by(subdomain) |>
        dplyr::summarize(
          zMean = mean(z, na.rm = TRUE),
          zPct = mean(percentile, na.rm = TRUE)
        ) |>
        dplyr::mutate(range = NA) |>
        dplyr::ungroup() # NOTE this is new

      # round z-score to 1 decimal
      df2$zMean <- round(df2$zMean, 2L)
      df2$zPct <- round(df2$zPct, 0L)
      df2 <-
        df2 |>
        dplyr::mutate(
          range = dplyr::case_when(
            zPct >= 98 ~ "Exceptionally High",
            zPct %in% 91:97 ~ "Above Average",
            zPct %in% 75:90 ~ "High Average",
            zPct %in% 25:74 ~ "Average",
            zPct %in% 9:24 ~ "Low Average",
            zPct %in% 2:8 ~ "Below Average",
            zPct < 2 ~ "Exceptionally Low",
            TRUE ~ as.character(range)
          )
        )

      # 2. sort hi to lo
      df2 <- dplyr::arrange(df2, desc(zPct)) # NOTE this is new

      # 3. create tibble with new column with domain name lowercase
      df_level2_status <- tibble(
        name = df2$subdomain,
        y = df2$zMean,
        y2 = df2$zPct,
        range = df2$range,
        drilldown = tolower(paste(x_level, name, sep = "_"))
      )

      list(
        id = tolower(x_level),
        type = "column",
        data = list_parse(df_level2_status)
      )
    })

  ## Level 3 -------------------------------------------------------
  ## Narrow subdomains
  ## reuse function
  df_level3_drill <-
    lapply(unique(data$domain), function(x_level) {
      df2 <- subset(data, data$domain %in% x_level)

      # reuse function but with y_level
      lapply(unique(df2$subdomain), function(y_level) {
        # 1. create mean z-scores for subdomain
        # df3 becomes pronoun for domain
        df3 <- subset(df2, df2$subdomain %in% y_level)

        df3 <- df3 |>
          dplyr::group_by(narrow) |>
          dplyr::summarize(
            zMean = mean(z, na.rm = TRUE),
            zPct = mean(percentile, na.rm = TRUE)
          ) |>
          dplyr::mutate(range = NA) |>
          ungroup() # NOTE this is new

        # round z-score to 1 decimal
        df3$zMean <- round(df3$zMean, 2L)
        df3$zPct <- round(df3$zPct, 0L)
        df3 <-
          df3 |>
          dplyr::mutate(
            range = dplyr::case_when(
              zPct >= 98 ~ "Exceptionally High",
              zPct %in% 91:97 ~ "Above Average",
              zPct %in% 75:90 ~ "High Average",
              zPct %in% 25:74 ~ "Average",
              zPct %in% 9:24 ~ "Low Average",
              zPct %in% 2:8 ~ "Below Average",
              zPct < 2 ~ "Exceptionally Low",
              TRUE ~ as.character(range)
            )
          )

        df3 <- dplyr::arrange(df3, desc(zPct))

        df_level3_status <- tibble(
          name = df3$narrow,
          y = df3$zMean,
          y2 = df3$zPct,
          range = df3$range,
          drilldown = tolower(paste(x_level, y_level, name, sep = "_"))
        )

        list(
          id = tolower(paste(x_level, y_level, sep = "_")),
          type = "column",
          data = list_parse(df_level3_status)
        )
      })
    }) |> unlist(recursive = FALSE)

  ## Level 4 -------------------------------------------------------
  ## Scale scores
  ## reuse both functions
  df_level4_drill <-
    lapply(unique(data$domain), function(x_level) {
      df2 <- subset(data, data$domain %in% x_level)

      lapply(unique(df2$subdomain), function(y_level) {
        df3 <- subset(df2, df2$subdomain %in% y_level)

        lapply(unique(df3$narrow), function(z_level) {
          df4 <- subset(df3, df3$narrow %in% z_level)

          df4 <-
            df4 |>
            dplyr::group_by(scale) |>
            dplyr::summarize(
              zMean = mean(z, na.rm = TRUE),
              zPct = mean(percentile, na.rm = TRUE)
            ) |>
            dplyr::mutate(range = NA) |>
            dplyr::ungroup() # NOTE this is new

          # round z-score to 1 decimal
          df4$zMean <- round(df4$zMean, 2L)
          df4$zPct <- round(df4$zPct, 0L)
          df4 <-
            df4 |>
            dplyr::mutate(
              range = dplyr::case_when(
                zPct >= 98 ~ "Exceptionally High",
                zPct %in% 91:97 ~ "Above Average",
                zPct %in% 75:90 ~ "High Average",
                zPct %in% 25:74 ~ "Average",
                zPct %in% 9:24 ~ "Low Average",
                zPct %in% 2:8 ~ "Below Average",
                zPct < 2 ~ "Exceptionally Low",
                TRUE ~ as.character(range)
              )
            )

          df4 <- dplyr::arrange(df4, desc(zMean))

          df_level4_status <- tibble(
            name = df4$scale,
            y = df4$zMean,
            y2 = df4$zPct,
            range = df4$range
          )

          list(
            id = tolower(paste(x_level, y_level, z_level, sep = "_")),
            type = "column",
            data = list_parse(df_level4_status)
          )
        })
      }) |> unlist(recursive = FALSE)
    }) |> unlist(recursive = FALSE)

  # Create charts ----------------------------------
  # Theme
  theme <-
    highcharter::hc_theme_merge(
      highcharter::hc_theme_monokai(),
      highcharter::hc_theme_darkunica()
    )

  # Tooltip
  x <- c("Name", "Score", "Percentile", "Range")
  y <- c("{point.name}", "{point.y}", "{point.y2}", "{point.range}")
  tt <- highcharter::tooltip_table(x, y)

  ## Create drilldown bar plot zscores
  plot <-
    highcharter::highchart() |>
    highcharter::hc_title(
      text = patient,
      style = list(fontSize = "15px")
    ) |>
    highcharter::hc_add_series(df_level1_status,
      type = "bar",
      name = neuro_domain,
      highcharter::hcaes(x = name, y = y)
    ) |>
    highcharter::hc_xAxis(
      type = "category",
      title = list(text = "PASS Domain"),
      categories = df_level1_status$name
    ) |>
    highcharter::hc_yAxis(
      title = list(text = "Standard Score (Mean = 0, SD = 1)"),
      labels = list(format = "{value}")
    ) |>
    highcharter::hc_tooltip(
      pointFormat = tt,
      useHTML = TRUE,
      valueDecimals = 1
    ) |>
    highcharter::hc_plotOptions(
      series = list(
        colorByPoint = TRUE,
        allowPointSelect = TRUE,
        dataLabels = TRUE
      )
    ) |>
    highcharter::hc_drilldown(
      allowPointDrilldown = TRUE,
      series = c(
        df_level2_drill,
        df_level3_drill,
        df_level4_drill
      )
    ) |>
    highcharter::hc_colorAxis(
      minColor = "red",
      maxColor = "blue"
    ) |>
    highcharter::hc_add_theme(theme) |>
    highcharter::hc_chart(
      style = list(fontFamily = "Cabin"),
      backgroundColor = list("gray")
    )

  return(plot)
}
