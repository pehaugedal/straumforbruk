#' Last ned data frå Tibber
#'
#' @return
#' @export
#'
#' @examples
les_data_tibber = function() {
  token = "Bd9NDuzYG0nWuaxRHcO4SjbLwo5X6u_v3bEKCQ9BA08"
  con = GraphqlClient$new(
    url = "https://api.tibber.com/v1-beta/gql",
    headers = list(Authorization = paste0("Bearer ", token))
  )

  tidssone = "Europe/Oslo"
  startdato = paste0(year(today(tzone = tidssone)), "-01-01")
  startdato_base64 = base64enc::base64encode(charToRaw(startdato))

  qry_forbruk = Query$new()
  qry_forbruk$query(
    "hent_forbruk",
    glue::glue(
      "{{
          viewer {{
            homes {{
              consumption(resolution: HOURLY, after: \"{startdato_base64}\", last: 50000) {{
                nodes {{
                  from
                  to
                  cost
                  unitPrice
                  unitPriceVAT
                  consumption
                  consumptionUnit
                  currency
                }}
              }}
            }}
          }}
        }}"
    )
  )

  qry_pris = Query$new()
  qry_pris$query(
    "hent_prisar",
    "{
    viewer {
      homes {
        currentSubscription {
          priceInfo {
            range(resolution: HOURLY, last: 50000) {
              nodes {
                total
                energy
                tax
                startsAt
                level
              }
            }
            today {
              total
              energy
              tax
              startsAt
              level
            }
            tomorrow {
              total
              energy
              tax
              startsAt
              level
            }
          }
          priceRating {
            hourly {
              entries {
                total
                energy
                tax
                time
                level
              }
            }
          }
        }
      }
    }
  }"
  )

  q_forbruk = con$exec(qry_forbruk$queries$hent_forbruk)

  q_prisar = con$exec(qry_pris$queries$hent_prisar)

  d_p = jsonlite::fromJSON(q_prisar)
  d_prisar_historisk = d_p$data$viewer$homes$currentSubscription$priceInfo$range$nodes[[1]] |>
    as_tibble()
  d_prisar_idag = d_p$data$viewer$homes$currentSubscription$priceInfo$today[[1]] |>
    as_tibble()
  d_prisar_imorgon = d_p$data$viewer$homes$currentSubscription$priceInfo$tomorrow[[1]] |>
    as_tibble()
  # d_prisar_timar = d_p$data$viewer$homes$currentSubscription$priceRating$hourly$entries[[1]] |>
  #   as_tibble() |>
  #   rename(startsAt = time)
  d_prisar = d_prisar_historisk |>
    full_join(d_prisar_idag)

  if (nrow(d_prisar_imorgon) != 0) {
    d_prisar = d_prisar |>
      rbind(d_prisar_imorgon)
  } else {
    # d_prisar = d_prisar |>
    #   full_join(select(d_prisar_timar, -level))
  }

  d_prisar = d_prisar |>
    rename(from = startsAt)

  jsonlite::fromJSON(q_forbruk)$data$viewer$homes$consumption$nodes[[1]] |>
    as_tibble() |>
    full_join(d_prisar, by = "from") |>
    arrange(from) |>
    mutate(
      from = as_datetime(from, tz = tidssone),
      to = as_datetime(to, tz = tidssone),
      mnd = month(from, label = TRUE)
    )
}
