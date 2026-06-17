#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  options(shiny.timeout = 0)

  observeEvent(input$model, {
    req(input$model)
    
    all_regions_selected <- length(input$model) == 8
    
    if (all_regions_selected) {
      new_max <- 100
      new_values <- c(15, 30, 45)
    } else {
      new_max <- 20
      new_values <- c(5, 10, 15)
    }
    
    shiny::updateSliderInput(
      session,
      "min_h.levels",
      min = 0,
      max = new_max,
      value = new_values[1]
    )
    
    shiny::updateSliderInput(
      session,
      "mid_h.levels",
      min = 0,
      max = new_max,
      value = new_values[2]
    )
    
    shiny::updateSliderInput(
      session,
      "max_h.levels",
      min = 0,
      max = new_max,
      value = new_values[3]
    )
  })
  # ============================================================
  # Constants and small helpers
  # ============================================================

  region_targets <- tibble::tibble(
    Region = as.character(1:8),
    RegTar = c(0, 12, 5, 6, 10, 12, 10, 10)
  )

  selected_regions <- reactive({
    req(input$model)
    as.character(input$model)
  })

  selected_hist_regions <- reactive({
    req(input$histReg)
    as.character(input$histReg)
  })

  harvest_levels <- reactive({
    c(input$min_h.levels, input$mid_h.levels, input$max_h.levels)
  })

  q_vec <- function(x, probs = c(0.125, 0.25, 0.5, 0.75, 0.875)) {
    stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 7)
  }

  summarise_by_col <- function(mat, years) {
    mat <- as.matrix(mat)
    qs <- t(apply(mat, 2, q_vec))
    colnames(qs) <- c("q12_5", "q25", "q50", "q75", "q87_5")

    tibble::tibble(
      year  = years,
      mean  = colMeans(mat, na.rm = TRUE),
      q12_5 = qs[, "q12_5"],
      q25   = qs[, "q25"],
      q50   = qs[, "q50"],
      q75   = qs[, "q75"],
      q87_5 = qs[, "q87_5"]
    )
  }

  summarise_pred <- function(mat_pred, harvest_levels) {
    mat_pred <- as.matrix(mat_pred)
    qs <- t(apply(mat_pred, 2, q_vec))
    colnames(qs) <- c("q12_5", "q25", "q50", "q75", "q87_5")

    tibble::tibble(
      kvotealternativ = harvest_levels,
      mean  = colMeans(mat_pred, na.rm = TRUE),
      q12_5 = qs[, "q12_5"],
      q25   = qs[, "q25"],
      q50   = qs[, "q50"],
      q75   = qs[, "q75"],
      q87_5 = qs[, "q87_5"]
    )
  }

  shape_from_stats <- function(mu, sigma) {
    # Moment matching for beta distribution parameters.
    a <- (mu^2 - mu^3 - mu * sigma^2) / sigma^2
    b <- (mu - 2 * mu^2 + mu^3 - sigma^2 + mu * sigma^2) / sigma^2
    c(a, b)
  }

  prepare_model_data <- function(data, start_year, end_year, regions) {
    year_seq <- start_year:end_year

    annual <- data |>
      dplyr::mutate(Region = as.character(Region)) |>
      dplyr::filter(
        Region %in% regions,
        Aar >= start_year,
        Aar <= end_year
      ) |>
      dplyr::group_by(Aar) |>
      dplyr::summarise(
        FG = sum(FG, na.rm = TRUE),
        HV = sum(V.Hunner.belastet.kvoten, na.rm = TRUE),
        .groups = "drop"
      ) |>
      tidyr::complete(Aar = year_seq, fill = list(FG = 0, HV = 0)) |>
      dplyr::arrange(Aar)

    list(
      years = year_seq,
      FG = annual$FG,
      HV = annual$HV[annual$Aar < end_year]
    )
  }

  jags_runner <- function(use_parallel) {
    if (isTRUE(use_parallel) && "jags.parallel" %in% getNamespaceExports("R2jags")) {
      R2jags::jags.parallel
    } else {
      R2jags::jags
    }
  }

  # ============================================================
  # Model run
  # ============================================================

  dataInput <- eventReactive(input$Run.model, {

    validate(
      need(length(selected_regions()) > 0, "Velg minst én region"),
      need(input$startYear < input$endYear, "Startår må være før sluttår"),
      need(length(input$startYear:input$endYear) >= 3, "Velg minst tre år"),
      need(
        harvest_levels()[1] <= harvest_levels()[2] && harvest_levels()[2] <= harvest_levels()[3],
        "Kvotealternativene må være i stigende rekkefølge"
      )
    )

    model_data <- prepare_model_data(
      data = HarvestGolem::Lynx_monitoring_data,
      start_year = input$startYear,
      end_year = input$endYear,
      regions = selected_regions()
    )

    validate(
      need(length(model_data$FG) == length(model_data$years), "Feil i klargjøring av familiegruppedata"),
      need(length(model_data$HV) == length(model_data$years) - 1, "Feil i klargjøring av jaktuttaksdata"),
      need(any(model_data$FG > 0, na.rm = TRUE), "Ingen familiegrupper funnet for valgt region og periode")
    )

    T_len <- length(model_data$years)
    shapes <- shape_from_stats(mu = 0.44, sigma = 0.03)

    bugs.data <- list(
      y.a = shapes[1],
      y.b = shapes[2],
      y   = as.vector(model_data$FG),
      hv  = as.vector(model_data$HV),
      T   = T_len,
      h   = as.vector(harvest_levels()),
      I   = length(harvest_levels())
    )

    inits <- function() {
      list(
        sigma.proc = stats::runif(1, 0, 0.1),
        mean.lambda = 1.15,
        sigma.obs = stats::runif(1, 5, 10),
        N.est = c(model_data$FG[1], rep(NA, T_len - 1))
      )
    }

    parameters <- c("N.est", "N.pred", "X.est")

    if (identical(input$speed, "Rask")) {
      n.iter   <- 25000
      n.chains <- 3
      n.burnin <- 15000
      n.thin   <- 2
    } else {
      # Kept deliberately long so robust runs remain comparable to your current setup.
      n.iter   <- 2500000
      n.chains <- 3
      n.burnin <- 1500000
      n.thin   <- 2
    }

    use_parallel <- isTRUE(getOption("HarvestGolem.use_parallel", TRUE))
    run_jags <- jags_runner(use_parallel)

    withProgress(message = "Kjører modell", value = 0, {
      incProgress(0.05, detail = "Klargjør data")

      fit <- tryCatch(
        {
          incProgress(0.15, detail = "Starter JAGS")
          run_jags(
            data = bugs.data,
            inits = inits,
            parameters.to.save = parameters,
            model.file = system.file("JAGs", "ssm_lynx1.bug", package = "HarvestGolem"),
            n.chains = n.chains,
            n.iter = n.iter,
            n.burnin = n.burnin,
            n.thin = n.thin
          )
        },
        error = function(e) {
          showNotification(
            paste("Modellen feilet:", e$message),
            type = "error",
            duration = NULL
          )
          NULL
        }
      )

      incProgress(1, detail = "Ferdig")
      req(fit)
      fit
    })
  })

  # ============================================================
  # Cached summaries
  # ============================================================

  summaries <- reactive({
    req(dataInput())

    sims <- dataInput()$BUGSout$sims.list
    year_seq <- input$startYear:input$endYear

    sum_Nest  <- summarise_by_col(sims$N.est, years = year_seq)
    sum_Npred <- summarise_pred(sims$N.pred, harvest_levels = harvest_levels())

    sum_Xest <- NULL
    if (!is.null(sims$X.est)) {
      sum_Xest <- summarise_by_col(sims$X.est, years = year_seq)
    }

    list(
      sum_Nest  = sum_Nest,
      sum_Npred = sum_Npred,
      sum_Xest  = sum_Xest
    )
  })

  selected_target <- reactive({
    region_targets |>
      dplyr::filter(Region %in% selected_regions()) |>
      dplyr::summarise(target = sum(RegTar, na.rm = TRUE)) |>
      dplyr::pull(target)
  })

  observed_summary <- reactive({
    HarvestGolem::Lynx_monitoring_data |>
      dplyr::mutate(Region = as.character(Region)) |>
      dplyr::inner_join(region_targets, by = "Region") |>
      dplyr::filter(
        Region %in% selected_regions(),
        Aar >= input$startYear,
        Aar <= input$endYear
      ) |>
      dplyr::group_by(Aar) |>
      dplyr::summarise(
        TotalFG = sum(FG, na.rm = TRUE),
        TotalHarvest = sum(Antall.belastet.kvoten, na.rm = TRUE),
        TotalRegTar = sum(RegTar, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(Aar)
  })

  probability_less_than_target <- reactive({
    req(dataInput())
    n_years <- length(input$startYear:input$endYear)
    draws_last <- dataInput()$BUGSout$sims.list$N.est[, n_years]
    round(mean(draws_last <= selected_target(), na.rm = TRUE), 2)
  })

  prognosis_table <- reactive({
    req(summaries(), observed_summary())

    obs_last <- observed_summary() |>
      dplyr::filter(Aar == max(Aar))

    EstN_last <- summaries()$sum_Nest |>
      dplyr::filter(year == max(year))

    prognosis_fg <- paste0(
      round(EstN_last$mean, 0), " [",
      round(EstN_last$q12_5, 2), " - ", round(EstN_last$q87_5, 2), "]"
    )

    prognosis_lynx <- NA_character_
    if (!is.null(summaries()$sum_Xest)) {
      X_last <- summaries()$sum_Xest |>
        dplyr::filter(year == max(year))

      prognosis_lynx <- paste0(
        round(X_last$mean, 2), " [",
        round(X_last$q12_5, 2), " - ", round(X_last$q87_5, 2), "]"
      )
    }

    tibble::tibble(
      Bestandsmål = selected_target(),
      `Antall familiegrupper av gaupe påvist` = sum(obs_last$TotalFG, na.rm = TRUE),
      `Prognose for antall familiegrupper [75% CI]` = prognosis_fg,
      `Prognose for antall gaupe [75% CI]` = prognosis_lynx,
      `P(under bestandsmål)` = probability_less_than_target()
    )
  })

  harvest_table <- reactive({
    req(summaries())

    summaries()$sum_Npred |>
      dplyr::transmute(
        kvotealternativ,
        prognose = q50,
        lower50 = q25,
        lower75 = q12_5,
        upper50 = q75,
        upper75 = q87_5
      )
  })

  # ============================================================
  # Outputs: tables
  # ============================================================

  output$table2 <- DT::renderDataTable(
    prognosis_table(),
    extensions = "Buttons",
    options = list(
      paging = FALSE,
      searching = FALSE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = "tB",
      buttons = c("copy", "csv", "excel")
    ),
    rownames = FALSE,
    class = "display"
  )

  output$table <- DT::renderDataTable(
    {
      harvest_table() |>
        dplyr::mutate(
          prognose = round(prognose, 2),
          `75% CI` = paste0(round(lower75, 2), " - ", round(upper75, 2))
        ) |>
        dplyr::select(kvotealternativ, prognose, `75% CI`)
    },
    extensions = "Buttons",
    options = list(
      paging = FALSE,
      searching = FALSE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = "tB",
      buttons = c("copy", "csv", "excel")
    ),
    rownames = FALSE,
    class = "display"
  )

  # ============================================================
  # Outputs: plots
  # ============================================================

  output$plot1 <- plotly::renderPlotly({
    req(summaries(), observed_summary())
    
    EstN <- summaries()$sum_Nest
    obs <- observed_summary()
    target <- selected_target()
    
    final_estimate <- EstN |>
      dplyr::filter(year == max(year)) |>
      dplyr::mutate(
        forecast_year = year + 1
      )
    
    p <- ggplot2::ggplot() +
      
      # observed family groups
      ggplot2::geom_line(
        data = obs,
        ggplot2::aes(x = Aar, y = TotalFG),
        linewidth = 1.4,
        colour = "darkgoldenrod4"
      ) +
      ggplot2::geom_point(
        data = obs,
        ggplot2::aes(x = Aar, y = TotalFG),
        size = 5,
        colour = "darkgoldenrod4"
      ) +
      
      # management target / bestandsmål
      ggplot2::geom_segment(
        data = obs,
        ggplot2::aes(
          x = min(Aar) + 9,
          xend = max(Aar) - 1,
          y = target,
          yend = target
        ),
        inherit.aes = FALSE,
        linetype = 2,
        linewidth = 1.3,
        colour = "black"
      ) +
      
      # 75% credible interval for final forecast
      ggplot2::geom_segment(
        data = final_estimate,
        ggplot2::aes(
          x = forecast_year + 0.25,
          xend = forecast_year + 0.25,
          y = q12_5,
          yend = q87_5
        ),
        inherit.aes = FALSE,
        colour = "grey70",
        linewidth = 6,
        lineend = "butt"
      ) +
      
      # 50% credible interval for final forecast
      ggplot2::geom_segment(
        data = final_estimate,
        ggplot2::aes(
          x = forecast_year + 0.25,
          xend = forecast_year + 0.25,
          y = q25,
          yend = q75
        ),
        inherit.aes = FALSE,
        colour = "black",
        linewidth = 3,
        lineend = "butt"
      ) +
      
      # forecast point estimate
      ggplot2::geom_point(
        data = final_estimate,
        ggplot2::aes(x = forecast_year, y = mean),
        inherit.aes = FALSE,
        size = 5,
        shape = 15,
        colour = "darkred"
      ) +
      
      ggplot2::labs(
        x = "År",
        y = "Antall familiegrupper",
        caption = "Rød firkant = modellprognose; svart/grå stolpe = 50% og 75% intervaller; stiplet linje = bestandsmål."
      ) +
      ggplot2::coord_cartesian(
        xlim = c(min(obs$Aar) - 1, max(obs$Aar) + 4)
      ) +
      ggplot2::theme_classic(base_size = 15) +
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = 18),
        axis.text = ggplot2::element_text(size = 14),
        plot.caption = ggplot2::element_text(hjust = 0)
      )
    
    plotly::ggplotly(p, tooltip = c("x", "y"))
  })
  output$plot3 <- plotly::renderPlotly({
    req(harvest_table())
    
    p <- harvest_table() |>
      ggplot2::ggplot(ggplot2::aes(x = kvotealternativ, y = prognose)) +
      ggplot2::geom_pointrange(
        ggplot2::aes(ymin = lower75, ymax = upper75),
        fatten = 1,
        size = 1.2,
        colour = "grey55"
      ) +
      ggplot2::geom_pointrange(
        ggplot2::aes(ymin = lower50, ymax = upper50),
        fatten = 1,
        size = 2,
        colour = "darkorange"
      ) +
      ggplot2::geom_point(size = 3, colour = "black") +
      ggplot2::geom_hline(yintercept = selected_target(), linetype = 2, size = 1) +
      ggplot2::labs(
        x = "Uttak voksne hunndyr",
        y = "Prognose antall familiegrupper",
        caption = "Grå = 75% intervall; oransje = 50% intervall; stiplet linje = bestandsmål."
      ) +
      ggplot2::theme_classic(base_size = 15) +
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = 18),
        axis.text = ggplot2::element_text(size = 14),
        plot.caption = ggplot2::element_text(hjust = 0)
      )
    
    plotly::ggplotly(p, tooltip = c("x", "y", "ymin", "ymax"))
  })
  # ============================================================
  # Historical data plot
  # ============================================================

  National_data <- reactive({
    validate(
      need(length(selected_hist_regions()) > 0, "Velg minst én region")
    )

    HarvestGolem::Lynx_monitoring_data |>
      dplyr::mutate(Region = as.character(Region)) |>
      dplyr::inner_join(region_targets, by = "Region") |>
      dplyr::filter(Region %in% selected_hist_regions())
  })

  output$National <- plotly::renderPlotly({
    req(National_data())

    target <- National_data() |>
      dplyr::distinct(Region, RegTar) |>
      dplyr::summarise(target = sum(RegTar, na.rm = TRUE)) |>
      dplyr::pull(target)

    plotd <- National_data() |>
      dplyr::group_by(Aar) |>
      dplyr::summarise(
        FG = sum(FG, na.rm = TRUE),
        uttak = sum(Antall.belastet.kvoten, na.rm = TRUE),
        .groups = "drop"
      ) |>
      ggplot2::ggplot(ggplot2::aes(Aar, FG)) +
      ggplot2::geom_col(
        ggplot2::aes(y = uttak),
        fill = "darkcyan",
        colour = "black",
        size = 0.3,
        alpha = 0.35
      ) +
      ggplot2::geom_line(colour = "darkgreen", size = 1.3) +
      ggplot2::geom_point(ggplot2::aes(text = paste0("År: ", Aar, "<br>FG: ", FG)), size = 3.5, colour = "darkgreen") +
      ggplot2::geom_hline(yintercept = target, size = 1, linetype = 2) +
      ggplot2::labs(x = "År", y = "Antall familiegrupper / felte gauper") +
      ggplot2::theme_classic(base_size = 15) +
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.text = ggplot2::element_text(size = 14)
      )

    plotly::ggplotly(plotd, tooltip = "text")
  })

  output$Legend <- renderText({
    d <- HarvestGolem::Lynx_monitoring_data
    paste0(
      "Antall familiegrupper av gaupe (punkter/linje) og uttak av gauper (stolper) i perioden ",
      min(d$Aar, na.rm = TRUE), "–", max(d$Aar, na.rm = TRUE),
      ". Antall familiegrupper i 2014 og senere år er ikke direkte sammenlignbart med tidligere år, da overvåkingsmetodikken er endret i forbindelse med samordningen med Sverige."
    )
  })
}
