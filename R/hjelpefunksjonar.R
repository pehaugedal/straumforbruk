#' @importFrom lubridate year month
#' @importFrom dplyr group_by case_when last mutate ungroup
#' @importFrom tibble add_row
NULL
#' Straumstøtte
#'
#' @param d_forbruk
#'
#' @return
#' @export
#'
#' @examples
legg_til_straumstotte = function(d_forbruk) {
  d_forbruk %>%
    mutate(
      stotte_time = case_when(
        # 80 % stønad over 70 øre før august 2022
        year(from) == 2022 & month(from) <= 8 ~ (energy - 0.7),
        # 90 % stønad frå og med september 2022 til og med mars 2023
        year(from) == 2022 | year(from) == 2023 & month(from) <= 3 ~ (energy - 0.7) * 0.9 * 1.25,
        # 80 % stønad frå og med april 2023 til og med august 2023
        year(from) == 2023 & month(from) <= 8 ~ (energy - 0.7),
        # 90 % stønad
        year(from) >= 2023 ~ (energy - 0.7) * 0.9 * 1.25
      ),
    ) %>%
    group_by(aar_mnd = lubridate::floor_date(from, "month")) %>%
    mutate(
      stotte = case_when(
        # 80 % stønad over 70 øre før august 2022
        year(from) == 2022 & month(from) <= 8 ~ (mean(energy) - 0.7),
        # 90 % stønad frå og med september 2022 til og med mars 2023
        year(from) == 2022 | year(from) == 2023 & month(from) <= 3 ~ (mean(energy) - 0.7) * 0.9 * 1.25,
        # 80 % stønad frå og med april 2023 til og med august 2023
        year(from) == 2023 & month(from) <= 8 ~ (mean(energy) - 0.7),
        # 90 % stønad
        year(from) >= 2023 ~ (mean(energy) - 0.7) * 0.9 * 1.25
      ),
      stotte_kum = case_when(
        # 80 % stønad over 70 øre før august 2022
        year(from) == 2022 & month(from) <= 8 ~ (cummean(energy) - 0.7),
        # 90 % stønad frå og med september 2022 til og med mars 2023
        year(from) == 2022 | year(from) == 2023 & month(from) <= 3 ~ (cummean(energy) - 0.7) * 0.9 * 1.25,
        # 80 % stønad frå og med april 2023 til og med august 2023
        year(from) == 2023 & month(from) <= 8 ~ (cummean(energy) - 0.7),
        # 90 % stønad
        year(from) >= 2023 ~ (cummean(energy) - 0.7) * 0.9 * 1.25
      ),
      stotte = pmax(stotte, 0),
      stotte_time = pmax(stotte_time, 0),
    ) %>%
    ungroup()
}

#' Legg til kostnadsinfo
#'
#' @param d_forbruk
#'
#' @return
#' @export
#'
#' @examples
legg_til_kostnadsinfo = function(d_forbruk) {
  d_forbruk %>%
      group_by(aar_mnd = lubridate::floor_date(from, "month")) %>%
      mutate(
        nettleige = nettleige(from),
        pris_faktisk = total + nettleige - stotte,
        kostnad_faktisk = consumption * pris_faktisk,
        snitt_mnd = mean(pris_faktisk)
      ) %>%
      ungroup()
}

#' Nettleige
#'
#' @param tidspunkt
#'
#' @return
#' @export
#'
#' @examples
nettleige = function(tidspunkt) {
  case_when(
    # Januar - mars 2022
    year(tidspunkt) == 2022 & month(tidspunkt) %in% 1:3 ~ 0.32888,
    # April 2022
    year(tidspunkt) == 2022 & month(tidspunkt) == 4 ~ 0.41013,
    # Mai - juni 2022
    year(tidspunkt) == 2022 & month(tidspunkt) %in% 5:6 ~ 0.43,
    # Dagtid juli - desember 2022
    year(tidspunkt) == 2022 & month(tidspunkt) %in% 7:12 & dagtid(tidspunkt) ~
      0.499,
    # Kveld/helg juli - desember 2022
    year(tidspunkt) == 2022 & month(tidspunkt) %in% 7:12 & !dagtid(tidspunkt) ~
      0.399,
    # Dagtid januar - mars 2023
    year(tidspunkt) == 2023 & month(tidspunkt) %in% 1:3 & dagtid(tidspunkt) ~
      0.4209,
    # Kveld/helg januar - mars 2023
    year(tidspunkt) == 2023 & month(tidspunkt) %in% 1:3 & !dagtid(tidspunkt) ~
      0.3209,
    # Dagtid april - juni 2023
    year(tidspunkt) == 2023 & month(tidspunkt) %in% 4:6 & dagtid(tidspunkt) ~
      0.5044,
    # Kveld/helg april - juni 2023
    year(tidspunkt) == 2023 & month(tidspunkt) %in% 4:6 & !dagtid(tidspunkt) ~
      0.4044,
    # Dagtid f.o.m. juli 2023
    year(tidspunkt) == 2023 & month(tidspunkt) >= 7 & dagtid(tidspunkt) ~
      0.5573,
    # Kveld/helg f.o.m. juli 2023
    year(tidspunkt) == 2023 & month(tidspunkt) >= 7 & !dagtid(tidspunkt) ~
      0.4393
  )
}

#' Er det dagtid eller kveld/helg?
#'
#' @description
#' Funksjonen tek inn tidspunkt og sjekkar om dei er rekna som dagtid eller
#' kveld/helg i samband med nettleige.
#'
#' @param tidspunkt Tidspunkt som skal sjekkast.
#'
#' @return
#' `TRUE` eller `FALSE` for kvart element i `tidspunkt`.
#' `TRUE` dersom `tidspunkt` er ein kvardag kl. 6-22,
#' og `FALSE` elles.
#'
#' @export
#'
#' @examples
#' dagtid(Sys.time())
dagtid = function(tidspunkt) {
  lubridate::hour(tidspunkt) %in% 6:21 &
    lubridate::wday(tidspunkt, week_start = 1) %in% 1:5
}


#' Vekta snitt
#'
#' @description
#' Funksjon for å rekna ut vekta snitt som kan handtera manglande vektverdiar.
#'
#' @param x
#' An object containing the values whose weighted mean is to be computed.
#' @param w
#' A numerical vector of weights the same length as `x`
#' giving the weights to use for elements of `x`.
#' @param ...
#' Arguments to be passed to or from methods.
#' @param na.rm
#' A logical value indicating whether `NA` values in `x` should be stripped
#' before the computation proceeds.
#'
#' @return
#' @export
#'
#' @examples
weighted_mean = function(x, w, ..., na.rm = FALSE) {
  if (na.rm) {
    keep = !is.na(x) & !is.na(w)
    w = w[keep]
    x = x[keep]
  }
  weighted.mean(x, w, ..., na.rm = FALSE)
}

#' Legg til ekstra time
#'
#' @param d
#'
#' @return
#' @export
#'
#' @examples
legg_til_ekstra_time = function(d) {
  d %>%
    add_row(tail(d, 1)) %>%
    mutate(from = c(from[-n()], last(from) + 60 * 60))
}
