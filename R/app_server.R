#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  options(shiny.timeout = 0)
  
  # ============================================================
  # Helper functions (fast + memory-light)
  # ============================================================
  
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
      kvotealtternativ = harvest_levels,
      mean  = colMeans(mat_pred, na.rm = TRUE),
      q12_5 = qs[, "q12_5"],
      q25   = qs[, "q25"],
      q50   = qs[, "q50"],
      q75   = qs[, "q75"],
      q87_5 = qs[, "q87_5"]
    )
  }
  
  # ============================================================
  # Model run (eventReactive)
  # ============================================================
  
  dataInput <- eventReactive(input$Run.model, {
    
    validate(
      need(!is.null(input$model), "Velg data")
    )
    
    d <- HarvestGolem::Lynx_monitoring_data |>
      dplyr::filter(Region == input$model)
    
    # Filter by years
    data <- d[d$Aar >= input$startYear & d$Aar <= input$endYear, ]
    
    # Harvest levels
    h.levels <- c(input$min_h.levels, input$mid_h.levels, input$max_h.levels)
    h <- h.levels
    
    year_seq <- input$startYear:input$endYear
    T_len <- length(year_seq)
    
    # Build FG and HV vectors (as numeric vectors; JAGS likes vectors)
    FG <- numeric(T_len)
    HV <- numeric(T_len - 1)
    
    # Years t = start ... end-1 for HV
    for (i in input$startYear:(input$endYear - 1)) {
      temp1 <- data[data$Aar == i, , drop = FALSE]
      FG[i - input$startYear + 1] <- sum(temp1[, "FG"], na.rm = TRUE)
      HV[i - input$startYear + 1] <- sum(temp1[, "V.Hunner.belastet.kvoten"], na.rm = TRUE)
    }
    # Last year FG (end year)
    temp_last <- data[data$Aar == input$endYear, , drop = FALSE]
    FG[T_len] <- sum(temp_last[, "FG"], na.rm = TRUE)
    
    # Moment matching
    shape_from_stats <- function(mu, sigma) {
      a <- (mu^2 - mu^3 - mu * sigma^2) / sigma^2
      b <- (mu - 2 * mu^2 + mu^3 - sigma^2 + mu * sigma^2) / sigma^2
      c(a, b)
    }
    
    shapes <- shape_from_stats(.44, .03)
    
    # Bundle data
    bugs.data <- list(
      y.a = shapes[1],
      y.b = shapes[2],
      y   = as.vector(FG),
      hv  = as.vector(HV),
      T   = T_len,
      h   = as.vector(h),
      I   = length(h)
    )
    
    # Initial values
    inits <- function() {
      list(
        sigma.proc = runif(1, 0, 0.1),
        mean.lambda = 1.15,
        sigma.obs = runif(1, 5, 10),
        N.est = c(FG[1], rep(NA, T_len - 1))
      )
    }
    
    # STREAMLINED monitored parameters (only what you use downstream)
    parameters <- c("N.est", "N.pred", "X.est")
    
    # Iter settings
    if (input$speed == "Rask") {
      n.iter   <- 25000
      n.chains <- 3
      n.burnin <- 15000
      n.thin   <- 2
    } else {
      # robust (as you want, long for reproducibility)
      n.iter   <- 2500000
      n.chains <- 3
      n.burnin <- 1500000
      n.thin   <- 2
    }
    
    # IMPORTANT: Ensure the eventReactive RETURNS the model object
    out1 <- withProgress(message = "running model", value = 0, {
      incProgress(0.05)
      
      fit <- R2jags::jags(
        data = bugs.data,
        inits = inits,
        parameters.to.save = parameters,
        model.file = system.file("JAGs", "ssm_lynx1.bug", package = "HarvestGolem"),
        n.chains = n.chains,
        n.iter = n.iter,
        n.burnin = n.burnin,
        n.thin = n.thin
      )
      
      incProgress(1)
      fit
    })
    
    out1
  })
  
  # ============================================================
  # Summaries computed ONCE per run (small objects)
  # ============================================================
  
  summaries <- reactive({
    req(dataInput())
    
    year_seq <- input$startYear:input$endYear
    h.levels <- c(input$min_h.levels, input$mid_h.levels, input$max_h.levels)
    
    sims <- dataInput()$BUGSout$sims.list
    
    sum_Nest  <- summarise_by_col(sims$N.est, years = year_seq)
    sum_Npred <- summarise_pred(sims$N.pred, harvest_levels = h.levels)
    
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
  
  # ============================================================
  # Table 2 (Estimated FG etc.) — rewritten without tidybayes
  # ============================================================
  
  dataInput4 <- reactive({
    req(dataInput())
    
    Region <- c(1, 2, 3, 4, 5, 6, 7, 8)
    RegTar <- c(0, 12, 5, 6, 10, 12, 10, 10)
    RegTars <- data.frame(Region, RegTar) |>
      dplyr::filter(Region == input$model)
    
    n.years <- length(input$startYear:input$endYear)
    
    draws_last <- dataInput()$BUGSout$sims.list$N.est[, n.years]
    target <- sum(as.numeric(RegTars$RegTar))
    
    # probability below (<=) target
    p <- mean(draws_last <= target, na.rm = TRUE)
    round(p, 2)
  })
  
  dataInput1 <- reactive({
    req(summaries())
    
    Region <- c(1, 2, 3, 4, 5, 6, 10, 12, 10, 10) # (kept your original targets below; corrected next)
    Region <- c(1, 2, 3, 4, 5, 6, 7, 8)
    RegTar <- c(0, 12, 5, 6, 10, 12, 10, 10)
    RegTars <- data.frame(Region, RegTar)
    
    d <- HarvestGolem::Lynx_monitoring_data |>
      dplyr::inner_join(RegTars) |>
      dplyr::filter(Region == input$model) |>
      subset(Aar <= as.numeric(input$endYear) & Aar >= as.numeric(input$startYear)) |>
      as.data.frame()
    
    obs <- d |>
      dplyr::filter(Aar == max(Aar)) |>
      dplyr::summarise(
        Bestandsmål = sum(RegTar),
        `Antall familiegrupper av gaupe påvist` = sum(FG),
        year = Aar[1]
      )
    
    EstN_last <- summaries()$sum_Nest |>
      dplyr::filter(year == max(year))
    
    prognosis <- paste0(
      round(EstN_last$mean, 0), " [",
      round(EstN_last$q12_5, 2), " - ", round(EstN_last$q87_5, 2), "]"
    )
    
    prognosis2 <- NA_character_
    if (!is.null(summaries()$sum_Xest)) {
      X_last <- summaries()$sum_Xest |>
        dplyr::filter(year == max(year))
      
      prognosis2 <- paste0(
        round(X_last$mean, 2), " [",
        round(X_last$q12_5, 2), " - ", round(X_last$q87_5, 2), "]"
      )
    }
    
    tab <- data.frame(
      Bestandsmål = obs$Bestandsmål,
      `Antall familiegrupper av gaupe påvist` = obs$`Antall familiegrupper av gaupe påvist`,
      `Prognose for antall familiegrupper [75% CI]` = prognosis,
      `Prognose for antall gaupe [75% CI]` = prognosis2,
      P_lessThanTarget = dataInput4()
    )
    
    tab |> tidyr::drop_na()
  })
  
  # ============================================================
  # Table 1 (harvest scenarios) — rewritten to use cached summary
  # ============================================================
  
  dataInput3 <- reactive({
    req(summaries())
    summaries()$sum_Npred |>
      dplyr::transmute(
        kvotealtternativ,
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
    { dataInput1() },
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
    class = "display"
  )
  
  output$table <- DT::renderDataTable(
    {
      dataInput3() |>
        dplyr::mutate(`75% CI` = paste0(lower75, " - ", upper75)) |>
        dplyr::select(kvotealtternativ, prognose, `75% CI`)
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
    class = "display"
  )
  
  # ============================================================
  # Plot 1 — rewritten without tidybayes/gather
  # ============================================================
  
  output$plot1 <- plotly::renderPlotly({
    req(summaries())
    
    EstN <- summaries()$sum_Nest |>
      dplyr::mutate(
        CI75_low  = q12_5,
        CI75_high = q87_5,
        CI50_low  = q25,
        CI50_high = q75
      )
    
    Region <- c(1, 2, 3, 4, 5, 6, 7, 8)
    RegTar <- c(0, 12, 5, 6, 10, 12, 10, 10)
    RegTars <- data.frame(Region, RegTar)
    
    d <- HarvestGolem::Lynx_monitoring_data |>
      dplyr::inner_join(RegTars) |>
      dplyr::filter(Region == input$model) |>
      subset(Aar <= input$endYear & Aar >= input$startYear) |>
      dplyr::group_by(Aar) |>
      dplyr::summarise(TotalFG = sum(FG), TotalRegTar = sum(RegTar), .groups = "drop")
    
    EstNlast <- EstN |>
      dplyr::filter(dplyr::row_number() == dplyr::n()) |>
      dplyr::rename(Aar = year, TotalFG = mean)
    
    p <- d |>
      ggplot2::ggplot(ggplot2::aes(Aar, TotalFG)) +
      ggplot2::geom_point(size = 6, colour = "darkgoldenrod4") +
      ggplot2::geom_line(colour = "darkgoldenrod4", size = 1) +
      ggplot2::labs(y = "Antall familiegrupper") +
      ggplot2::geom_segment(
        x = 2005, xend = input$endYear - 2,
        y = d$TotalRegTar, yend = d$TotalRegTar,
        lty = 2, size = 2
      ) +
      ggplot2::geom_point(
        data = EstNlast,
        ggplot2::aes(Aar + 1, TotalFG),
        colour = "darkred", size = 6, shape = 15
      ) +
      ggplot2::geom_segment(
        data = EstNlast,
        ggplot2::aes(
          x = input$endYear + 1.2, xend = input$endYear + 1.2,
          y = CI50_low, yend = CI50_high
        ),
        size = 5, lineend = "round"
      ) +
      ggplot2::geom_segment(
        data = EstNlast,
        ggplot2::aes(
          x = input$endYear + 1.4, xend = input$endYear + 1.4,
          y = CI75_low, yend = CI75_high
        ),
        size = 5, lineend = "round", colour = "grey"
      ) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(color = "grey20", size = 15, face = "plain"),
        axis.text.y = ggplot2::element_text(color = "grey20", size = 15, face = "plain"),
        axis.title.x = ggplot2::element_text(color = "grey20", size = 20, face = "plain"),
        axis.title.y = ggplot2::element_text(color = "grey20", size = 20, face = "plain")
      )
    
    plotly::ggplotly(p, tooltip = "none")
  })
  
  # ============================================================
  # Plot 3 — unchanged except it uses dataInput3() (now cached)
  # ============================================================
  
  output$plot3 <- plotly::renderPlotly({
    RegTar <- c(0, 12, 5, 6, 10, 12, 10, 10)
    Region <- c(1, 2, 3, 4, 5, 6, 7, 8)
    RegTars <- data.frame(Region, RegTar) |>
      dplyr::filter(Region == input$model)
    
    p <- dataInput3() |>
      ggplot2::ggplot() +
      ggplot2::geom_pointrange(
        ggplot2::aes(x = kvotealtternativ, y = prognose, ymin = upper75, ymax = lower75),
        fatten = 1, size = 6, color = "dark orange"
      ) +
      ggplot2::geom_point(ggplot2::aes(kvotealtternativ, prognose), colour = "black", size = 3) +
      ggplot2::labs(x = "Uttak voksne hunndyr", y = "Prognose antall familiegrupper") +
      ggplot2::geom_hline(yintercept = sum(RegTars$RegTar), linetype = 2) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(color = "grey20", size = 15, face = "plain"),
        axis.text.y = ggplot2::element_text(color = "grey20", size = 15, face = "plain"),
        axis.title.x = ggplot2::element_text(color = "grey20", size = 20, face = "plain"),
        axis.title.y = ggplot2::element_text(color = "grey20", size = 20, face = "plain")
      )
    
    plotly::ggplotly(p, tooltip = "none")
  })
  
  # ============================================================
  # Remaining code: kept as close to your original as possible
  # ============================================================
  
  points <- reactive({
    plotdat |>
      dplyr::filter(Aar == as.numeric(input$year))
  })
  
  National_data <- reactive({
    d <- HarvestGolem::Lynx_monitoring_data
    Region <- c(1, 2, 3, 4, 5, 6, 7, 8)
    RegTar <- c(0, 12, 5, 6, 10, 12, 10, 10)
    RegTars <- data.frame(Region, RegTar)
    d <- d |>
      dplyr::inner_join(RegTars)
    
    validate(
      need(!is.null(input$histReg), "Velg data")
    )
    
    d |>
      dplyr::filter(Region %in% input$histReg)
  })
  
  output$National <- plotly::renderPlotly({
    plotd <- National_data() |>
      dplyr::group_by(Aar) |>
      dplyr::select(!kommentar) |>
      dplyr::summarise(FG = sum(FG), uttak = sum(Antall.belastet.kvoten), .groups = "drop") |>
      ggplot2::ggplot(ggplot2::aes(Aar, FG, label = FG)) +
      ggplot2::geom_line(colour = "dark green", size = 3) +
      ggplot2::geom_bar(
        ggplot2::aes(Aar, uttak),
        stat = "identity", fill = "dark cyan",
        colour = "black", size = 1, alpha = 0.4
      ) +
      ggplot2::geom_hline(
        yintercept = sum(National_data()$RegTar[which(National_data()$Aar == 2020)]),
        size = 2, lty = 2
      ) +
      ggplot2::geom_point(ggplot2::aes(Aar, FG), size = 8, colour = "dark green") +
      ggplot2::geom_text(size = 2.5, colour = "white") +
      ggplot2::labs(x = "År", y = "Antall familiegrupper / felte gauper") +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.line = ggplot2::element_line(colour = "black", size = 2),
        axis.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.text = ggplot2::element_text(size = 14)
      )
    
    plotly::ggplotly(plotd, tooltip = NULL)
  })
  
  output$Legend <- renderText({
    d <- HarvestGolem::Lynx_monitoring_data
    inyear <- min(d$Aar)
    outyear <- max(d$Aar)
    paste0(
      "Antall familiegrupper av gaupe (sirkler) og uttak av gauper (stolpediagram) i Norge i perioden ",
      "1996", "–",
      "2010",
      ". Antall familiegrupper i 2014 og senere år er ikke direkte sammenlignbart med tidligere år, da overvåkingsmetodikken
                  er endret i forbindelse med samordningen med Sverige."
    )
  })
  
  # Reactive Values (kept)
  model <- reactive({
    Region <- c(1, 2, 3, 4, 5, 6, 7, 8)
    RegTar <- c(0, 12, 5, 6, 10, 12, 10, 10)
    RegTars <- data.frame(Region, RegTar) |>
      dplyr::filter(Region == input$model)
    RegTars$Region
  })
  
  table <- reactive({ dataInput3() })
  
  plot <- reactive({
    Region <- c(1, 2, 3, 4, 5, 6, 7, 8)
    RegTar <- c(0, 12, 5, 6, 10, 12, 10, 10)
    RegTars <- data.frame(Region, RegTar) |>
      dplyr::filter(Region == input$model)
    
    dataInput3() |>
      ggplot2::ggplot() +
      ggplot2::geom_pointrange(
        ggplot2::aes(x = kvotealtternativ, y = prognose, ymin = upper75, ymax = lower75),
        fatten = 1, size = 6, color = "grey"
      ) +
      ggplot2::geom_pointrange(
        ggplot2::aes(x = kvotealtternativ, y = prognose, ymin = upper50, ymax = lower50),
        fatten = 1, size = 6, color = "dark orange"
      ) +
      ggplot2::geom_point(ggplot2::aes(kvotealtternativ, prognose), colour = "black", size = 3) +
      ggplot2::labs(x = "Uttak voksne hunndyr", y = "Prognose antall familiegrupper") +
      ggplot2::geom_hline(yintercept = sum(RegTars$RegTar), linetype = 2)
  })
  
  # NOTE: plotx was left mostly unchanged but it uses dataInput() directly (fine).
  plotx <- reactive({
    req(dataInput())
    
    d <- HarvestGolem::Lynx_monitoring_data
    year <- input$startYear:input$endYear
    n.years <- length(year)
    
    d <- d |>
      dplyr::filter(Region == input$model)
    d <- subset(d, d$Aar <= input$endYear & d$Aar >= input$startYear)
    d <- as.data.frame(d)
    
    FG <- matrix(NA, ncol = 1, nrow = length(year))
    for (i in min(year):(max(year) - 1)) {
      temp1 <- subset(d, d$Aar == i)
      FG[i - min(year) + 1] <- sum(temp1[, "FG"], na.rm = TRUE)
    }
    
    dat <- data.frame("FG" = FG, "År" = year)
    
    Pred.res <- as.matrix(stats::quantile(
      dataInput()$BUGSout$sims.list$N.est[, n.years],
      c(0.125, 0.25, 0.5, 0.75, 0.875),
      na.rm = TRUE
    ))
    
    Pred.res <- data.frame(
      lower = c(NA, Pred.res[2, 1], Pred.res[1, 1]),
      upper = c(NA, Pred.res[4, 1], Pred.res[5, 1]),
      Med   = c(Pred.res[3, 1], NA, NA),
      Group = c(NA, "50%", "75%"),
      "År"  = c(2021, 2021.3, 2021.5)
    )
    rownames(Pred.res) <- NULL
    
    dat <- dplyr::full_join(dat, Pred.res, by = "År")
    
    list(dat, Pred.res, n.years, year)
  })
  
}
