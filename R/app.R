library(shiny)
library(tidyverse)
library(lubridate)
library(ghql)
library(gt)
library(patchwork)
library(shinyWidgets)

Sys.setlocale(category = "LC_ALL", locale = "nn_NO.UTF-8")
tidssone = "Europe/Oslo"
mnd_no = month(now(tzone = tidssone))
forste_denne_mnd = floor_date(now(tzone = tidssone), "month")

ui = fluidPage(
  titlePanel("Straumprisar"),
  navlistPanel(
    "Prisar og forbruk",
    tabPanel(
      "Per time",
      plotOutput("dagens_morgondagens_pris", height = 600),
      plotOutput("prisar_denne_veka", height = 600),
      airMonthpickerInput(
        "mnd_prisar",
        "Månad:",
        value = forste_denne_mnd,
        minDate = as.Date("2022-01-01"),
        maxDate = forste_denne_mnd,
        # dateFormat = "yyyy-mm",
        inline = TRUE
      ),
      plotOutput("prisar_denne_mnd", height = 600)
    ),
    tabPanel(
      "Kostnadar",
      airYearpickerInput(
        inputId = "aar_kostnadar",
        label = "År:",
        value = now(tzone = tidssone),
        minDate = as.Date("2022-01-01"),
        maxDate = forste_denne_mnd,
        inline = TRUE
      ),
      tableOutput("del_stotte")
    ),
    tabPanel(
      "Snittprisdifferanse",
      airYearpickerInput(
        inputId = "aar_snittprisdifferanse",
        label = "År:",
        value = now(tzone = tidssone),
        minDate = as.Date("2022-01-01"),
        maxDate = forste_denne_mnd,
        inline = TRUE
      ),
      plotOutput("snittpris_differanse")
    ),
    tabPanel(
      "Timemaksar",
      gt_output("timesmaksar")
    ),
    "Fordeling",
    tabPanel(
      "Prisar",
      plotOutput("straumpris_boxplot"),
      fluidRow(
        column(
          1,
          checkboxGroupInput(
            "mnd",
            "Månad:",
            choices = month(1:month(Sys.Date()), label = TRUE),
            selected = month(1:month(Sys.Date()), label = TRUE)
          )
        ),
        column(
          5,
          plotOutput("straumpris_histogram")
        ),
        column(
          5,
          plotOutput("spotpris_histogram")
        )
      )
    ),
    tabPanel(
      "Forbruk",
      plotOutput("kostnad_boxplot")
    ),
    widths = c(2, 10)
  )
)

server = function(input, output) {
  theme_set(ggplot2::theme_light())
  update_geom_defaults("col", list(fill = "#3182bd"))

  d_forbruk = les_data_tibber() %>%
    legg_til_straumstotte() %>%
    legg_til_kostnadsinfo()

  d_forbruk_snitt = d_forbruk %>%
    group_by(aar_mnd = lubridate::floor_date(from, "month")) %>%
    summarise(
      spot = mean(total) - 0.01,
      faktisk = weighted_mean(total, w = consumption, na.rm = TRUE) - 0.01,
      differanse = faktisk - spot
    )

  d_forbruk = d_forbruk %>%
    mutate(
      sum = total + nettleige,
      consumption = replace_na(consumption, 0)
    )

  d_forbruk_prisgraf = d_forbruk %>%
    legg_til_ekstra_time()

  d_prisar_siste_veke = d_forbruk_prisgraf %>%
    filter(date(from) >= today(tzone = tidssone) - 7) %>%
    mutate(time_lab = as.POSIXct(from, format = "%H:%M"))

  d_prisar_idag_imorgon = d_prisar_siste_veke %>%
    filter(date(from) >= today(tzone = tidssone))

  output$straumpris_histogram = renderPlot({
    d_forbruk_aktuell = d_forbruk %>%
      filter(mnd %in% input$mnd)

    ggplot(d_forbruk_aktuell, aes(x = pris_faktisk)) +
      geom_histogram(aes(fill = after_stat(x < 0)), boundary = 0) +
      scale_fill_manual(values = c("red", "limegreen")) +
      scale_x_continuous(breaks = scales::breaks_pretty()) +
      theme(
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) +
      ggtitle("Faktisk pris (spotpris + nettleige - stønad)") +
      xlab("Pris (kr/kWh)") +
      ylab("Tal på timar") +
      theme(plot.title = element_text(size = 20))
  })

  output$spotpris_histogram = renderPlot({
    d_forbruk_aktuell = d_forbruk %>%
      filter(mnd %in% input$mnd)

    ggplot(d_forbruk_aktuell, aes(x = unitPrice + nettleige)) +
      geom_histogram(boundary = 0, fill = "#3182bd") +
      scale_x_continuous(breaks = scales::breaks_pretty()) +
      theme(
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) +
      ggtitle("Pris før stønad (spotpris + nettleige)") +
      xlab("Pris (kr/kWh)") +
      ylab("Tal på timar") +
      theme(plot.title = element_text(size = 20))
  })

  output$straumpris_boxplot = renderPlot({
    fargar = c(Stønad = "#32CD32", Gjennomsnitt = "#FF0000")
    ggplot(d_forbruk, aes(x = mnd, y = pris_faktisk)) +
      geom_boxplot() +
      geom_point(aes(y = stotte, colour = "Stønad"),
        size = 4,
        shape = 4
      ) +
      geom_point(aes(y = snitt_mnd, colour = "Gjennomsnitt"),
        size = 4,
        shape = 4
      ) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_colour_manual(values = fargar) +
      theme(
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) +
      ggtitle("Faktiske timeprisar (spotpris + nettleige - stønad)") +
      xlab(NULL) +
      ylab("Pris (kr/kWh)") +
      theme(plot.title = element_text(size = 20))
  })

  output$kostnad_boxplot = renderPlot({
    ggplot(d_forbruk, aes(x = mnd, y = kostnad_faktisk)) +
      geom_boxplot() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) +
      ggtitle("Faktiske kostnadar per time (spotpris + nettleige - stønad)") +
      xlab(NULL) +
      ylab("Kostnad (kr/time)") +
      theme(plot.title = element_text(size = 20))
  })

  output$del_stotte = renderTable({
    d_forbruk %>%
      filter(year(from) == year(input$aar_kostnadar)) %>%
      group_by(mnd) %>%
      summarise(
        kostnad = sum(kostnad_faktisk, na.rm = TRUE),
        stønad = sum(stotte * consumption, na.rm = TRUE),
        nettleige = sum(nettleige * consumption, na.rm = TRUE)
      ) %>%
      janitor::adorn_totals("row") %>%
      mutate(
        total = kostnad + stønad,
        del_stønad = stønad / (kostnad + stønad),
        del_stønad_eks_nettleige = stønad / (kostnad + stønad - nettleige)
      )
  })

  output$snittpris_differanse = renderPlot({
    d_snittprisdifferanse = d_forbruk_snitt %>%
      filter(year(aar_mnd) == year(input$aar_snittprisdifferanse))
    ggplot(d_snittprisdifferanse, aes(x = aar_mnd, y = differanse, group = TRUE)) +
      geom_line() +
      geom_point(aes(color = after_stat(y < 0)), size = 3) +
      scale_color_manual(values = c("red", "limegreen")) +
      scale_x_datetime(
        labels = scales::date_format("%B", tz = tidssone),
        breaks = scales::breaks_pretty(nrow(d_snittprisdifferanse))
      ) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      theme(
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) +
      ggtitle("Differanse forbruksvekta snittpris og snittspotpris",
        subtitle = "Har eg brukt meir straum billige timar enn dyre?"
      ) +
      xlab(NULL) +
      ylab("Differanse (kr)") +
      theme(plot.title = element_text(size = 20))
  })

  output$dagens_morgondagens_pris = renderPlot({
    p_dagens_morgondagens_pris = ggplot(d_prisar_idag_imorgon, aes(x = from, y = sum)) +
      geom_step() +
      geom_step(aes(x = from, y = stotte), color = "limegreen") +
      geom_step(aes(x = from, y = stotte_time), color = "purple", linetype = "dotted") +
      scale_y_continuous(limits = c(0, NA)) +
      scale_x_datetime(
        labels = scales::date_format("%H:%M", tz = tidssone),
        breaks = scales::breaks_pretty(8)
      ) +
      geom_vline(
        xintercept = now(tzone = tidssone),
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = as_datetime(paste0(today(tzone = tidssone) + 1), tz = tidssone),
        linetype = 1
      ) +
      geom_text(
        data = distinct(map_df(head(d_prisar_idag_imorgon, -1), rev), stotte, .keep_all = TRUE),
        aes(
          x = from,
          y = stotte,
          label = scales::label_dollar(
            prefix = "",
            suffix = " kr",
            decimal.mark = ","
          )(round(stotte, 2))
        ),
        color = "limegreen",
        size = 5,
        hjust = if_else(nrow(d_prisar_idag_imorgon) == 25, -2, -1)
      ) +
      ggtitle("Dagens og morgondagens prisar og forbruk", subtitle = "Inkludert moms og nettleige") +
      xlab(NULL) +
      ylab("Pris (kr)") +
      theme(plot.title = element_text(size = 20))

    p_dagens_forbruk = ggplot(
      head(d_prisar_idag_imorgon, -1),
      aes(x = from, y = consumption, fill = pris_faktisk)
    ) +
      geom_col(position = position_nudge(60 * 30)) +
      scale_x_datetime(
        labels = scales::date_format("%H:%M", tz = tidssone),
        breaks = scales::breaks_pretty(8)
      ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) +
      xlab(NULL) +
      ylab("Forbruk (kWh)") +
      labs(fill = "Faktisk pris (kr/kWh)") +
      theme(
        plot.title = element_text(size = 20),
        legend.position = c(.95, .75)
      ) +
      scale_fill_gradient2(
        low = "limegreen",
        mid = "grey98",
        high = "red",
        midpoint = 0
      )

    p_dagens_morgondagens_pris / p_dagens_forbruk
  })

  output$prisar_denne_veka = renderPlot({
    p_vekas_prisar = ggplot(d_prisar_siste_veke, aes(x = from, y = sum)) +
      geom_step() +
      geom_step(aes(x = from, y = stotte), color = "limegreen") +
      geom_step(aes(x = from, y = stotte_time), color = "purple", linetype = "dotted") +
      scale_y_continuous(limits = c(0, NA)) +
      scale_x_datetime(
        labels = scales::date_format(
          format = "%A",
          tz = tidssone,
          locale = "nn"
        ),
        breaks = scales::breaks_pretty(10)
      ) +
      geom_vline(
        xintercept = now(tzone = tidssone),
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = as_datetime(paste0(today(tzone = tidssone) + 1), tz = tidssone),
        linetype = 1
      ) +
      geom_text(
        data = distinct(map_df(d_prisar_siste_veke, rev), stotte, .keep_all = TRUE),
        aes(
          x = from,
          y = stotte,
          label = scales::label_dollar(
            prefix = "",
            suffix = " kr",
            decimal.mark = ","
          )(round(stotte, 2))
        ),
        color = "limegreen",
        size = 5,
        hjust = -.3
      ) +
      theme(panel.grid.minor.x = element_blank()) +
      ggtitle("Prisar og forbruk siste veke", subtitle = "Inkludert moms og nettleige") +
      xlab(NULL) +
      ylab("Pris (kr)") +
      theme(plot.title = element_text(size = 20))

    p_vekas_forbruk = ggplot(
      d_prisar_siste_veke,
      aes(x = from, y = consumption, fill = pris_faktisk)
    ) +
      geom_col(position = position_nudge(60 * 30)) +
      scale_x_datetime(
        labels = scales::date_format(
          format = "%A",
          tz = tidssone,
          locale = "nn"
        ),
        breaks = scales::breaks_pretty(10)
      ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) +
      xlab(NULL) +
      ylab("Forbruk (kWh)") +
      labs(fill = "Faktisk pris (kr/kWh)") +
      theme(
        plot.title = element_text(size = 20),
        legend.position = c(.95, .75)
      ) +
      scale_fill_gradient2(
        low = "limegreen",
        mid = "grey98",
        high = "red",
        midpoint = 0
      )

    p_vekas_prisar / p_vekas_forbruk
  })

  output$prisar_denne_mnd = renderPlot({
    d_forbruk_mnd = filter(
      d_forbruk_prisgraf,
      from >= input$mnd_prisar,
      from < ceiling_date(input$mnd_prisar, "month")
    )

    p_vekas_prisar = ggplot(d_forbruk_mnd, aes(x = from, y = sum)) +
      geom_step() +
      geom_step(aes(x = from, y = stotte), color = "limegreen") +
      geom_step(aes(x = from, y = stotte_time), color = "purple", linetype = "dotted") +
      geom_step(aes(x = from, y = sum - stotte_time), color = "blue", linetype = "dotted") +
      geom_line(aes(x = from, y = stotte_kum), color = "orange3", linetype = "dashed") +
      scale_y_continuous(limits = c(0, NA)) +
      scale_x_datetime(
        labels = scales::date_format(
          format = "%d/%m",
          tz = tidssone,
          locale = "nn"
        ),
        breaks = scales::breaks_pretty(10)
      ) +
      geom_vline(
        xintercept = now(tzone = tidssone),
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = as_datetime(paste0(today(tzone = tidssone) + 1), tz = tidssone),
        linetype = 1
      ) +
      geom_text(
        data = distinct(map_df(d_forbruk_mnd, rev), stotte, .keep_all = TRUE),
        aes(
          x = from,
          y = stotte,
          label = scales::label_dollar(
            prefix = "",
            suffix = " kr",
            decimal.mark = ","
          )(round(stotte, 2))
        ),
        color = "limegreen",
        size = 5,
        hjust = -.3
      ) +
      theme(panel.grid.minor.x = element_blank()) +
      ggtitle("Prisar og forbruk vald månad", subtitle = "Inkludert moms og nettleige") +
      xlab(NULL) +
      ylab("Pris (kr)") +
      theme(plot.title = element_text(size = 20))

    p_vekas_forbruk = ggplot(d_forbruk_mnd,
      mapping = aes(x = from, y = consumption, fill = pris_faktisk)
    ) +
      geom_col(position = position_nudge(60 * 30)) +
      scale_x_datetime(
        labels = scales::date_format(
          format = "%d/%m",
          tz = tidssone,
          locale = "nn"
        ),
        breaks = scales::breaks_pretty(10)
      ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) +
      xlab(NULL) +
      ylab("Forbruk (kWh)") +
      labs(fill = "Faktisk pris (kr/kWh)") +
      theme(
        plot.title = element_text(size = 20),
        legend.position = c(.95, .75)
      ) +
      scale_fill_gradient2(
        low = "limegreen",
        mid = "grey98",
        high = "red",
        midpoint = 0
      )

    p_vekas_prisar / p_vekas_forbruk
  })

  output$timesmaksar = render_gt(lag_timemaks_tabell(d_forbruk))
}

# Run the application
shinyApp(ui = ui, server = server)
