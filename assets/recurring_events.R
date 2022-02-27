library(clock)
previous <- sub("\\.R", "", list.files("R"))
previous <- previous[as.Date(previous) < Sys.Date()]
n <- length(previous)
today <- date_today("Europe/Paris")
events <- date_shift(
  x = date_build(get_year(today), get_month(today) + 1:3),
  target = weekday(code = 4, encoding = "iso")
)

rcode <- sprintf(
  paste(
    "create_spotlight_night(",
    '  output = "posters/%s.png",',
    "  rmd_params = list(",
    '    title = "%s",',
    '    date = "Jeudi %s %s 2022 à 21 h 00"',
    "  )",
    ")",
    sep = "\n"
  ),
  events,
  seq(n + 1, n + length(events), 1),
  get_day(events),
  (function(x) {
    s <- as.character(x)
    paste0(
      toupper(substring(s, 1, 1)),
      substring(s, 2)
    )
  })(date_month_factor(events, labels = "fr"))
)
names(rcode) <- sprintf("R/%s.R", events)

mapply(
  FUN = writeLines,
  text = rcode,
  con = names(rcode)
)
