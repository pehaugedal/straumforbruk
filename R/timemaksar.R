#' Lag timemakstabell
#'
#' @param d_forbruk
#'
#' @return
#' @export
#'
#' @examples
lag_timemaks_tabell = function(d_forbruk) {
  mnd_dag = mday(tail(d_forbruk, 1)$from)

  nummer = rep(1:3, n_distinct(d_forbruk$aar_mnd))

  if (mnd_dag %in% c(1:2)) {
    nummer = head(nummer, -(3 - mnd_dag))
  }

  d_timesmaksar = d_forbruk %>%
    mutate(dato = lubridate::date(from)) %>%
    group_by(dato) %>%
    summarise(timesmaks = collapse::fmax(consumption, na.rm = TRUE)) %>%
    group_by(
      aar = year(dato),
      mnd = month(dato, label = TRUE, abbr = FALSE, locale = "nn_NO.utf8")
    ) %>%
    arrange(desc(timesmaks), .by_group = TRUE) %>%
    slice_head(n = 3) %>%
    ungroup() %>%
    mutate(nr = nummer) %>%
    pivot_wider(
      id_cols = c(aar, nr),
      names_from = mnd,
      values_from = timesmaks
    ) %>%
    select(-nr)


  gt_timesmaksar = gt(d_timesmaksar, rowname_col = "nr", groupname_col = "aar") %>%
    summary_rows(
      fns = list(snitt ~ mean(.)),
      fmt = ~ fmt_number(., decimals = 2)
    ) %>%
    fmt_number(decimals = 2)
}
