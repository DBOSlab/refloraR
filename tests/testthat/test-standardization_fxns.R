# Accented characters are built with intToUtf8() so these tests do not depend
# on how the session locale parses \uXXXX escapes.
.chr <- function(...) intToUtf8(c(...))
.e_acute <- function() .chr(0x00E9)
.a_acute <- function() .chr(0x00E1)
.a_tilde <- function() .chr(0x00E3)
.i_acute <- function() .chr(0x00ED)


test_that(".brazil_states() returns the 27 states with their accents", {
  st <- .brazil_states()

  expect_length(st, 27)
  expect_false(anyDuplicated(names(st)) > 0)
  expect_false(anyDuplicated(unname(st)) > 0)
  expect_equal(unname(st[paste0("S", .a_tilde(), "o Paulo")]), "SP")
  expect_equal(unname(st[paste0("Goi", .a_acute(), "s")]), "GO")
  # the "drop accents" key used to repair corrupted values must be unique
  k <- .key_drop(names(st))
  expect_false(anyDuplicated(k) > 0)
})


test_that(".norm_text() transliterates and removes brackets and remarks", {
  expect_equal(.norm_text(paste0("[Br", .e_acute(), "sil]")), "bresil")
  expect_equal(.norm_text("Chile (excl. Juan Fernandez Islands)"), "chile")
  expect_equal(.norm_text("South Africa ?"), "south africa")
  expect_equal(.norm_text("  United   States "), "united states")
  expect_true(is.na(.norm_text(NA_character_)))
})


test_that(".is_placeholder() flags empty and placeholder values", {
  x <- c("?", "s.loc.", "Indeterminado", "-", "", NA, "Bahia", "Brasil")
  expect_equal(.is_placeholder(x),
               c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
})


test_that(".std_whitespace() collapses blanks (including NBSP) and empties to NA", {
  nbsp <- .chr(0x00A0)
  df <- data.frame(a = c(paste0("Paran", .a_acute(), nbsp), "  x   y ", "", NA),
                   b = 1:4, stringsAsFactors = FALSE)

  res <- .std_whitespace(df)

  expect_equal(res$a[1], paste0("Paran", .a_acute()))
  expect_equal(res$a[2], "x y")
  expect_true(is.na(res$a[3]))
  expect_true(is.na(res$a[4]))
  expect_equal(res$b, 1:4)
})


test_that(".std_country() resolves multilingual names, typos and variants to English", {
  x <- c("Brasil", paste0("Br", .e_acute(), "sil"), paste0("[Br", .e_acute(), "sil]"),
         "brasil", "Brasi", "Barsil", "Basil", "Brasill", "Brasilien", "Brazil",
         "Paraguai", "Alemanha", paste0("M", .e_acute(), "xico"), "E.U.A.", "USA",
         paste0("Estados Unidos da Am", .e_acute(), "rica"),
         "United States (excl. Hawaii)", "Chile (excl. Juan Fernandez Islands)",
         "South Africa ?", "Guyane", "Nouv Granada")
  res <- .std_country(x, rep(NA_character_, length(x)))

  expect_equal(res$country[1:10], rep("Brazil", 10))
  expect_equal(res$countryCode[1:10], rep("BR", 10))
  expect_equal(res$country[11:21],
               c("Paraguay", "Germany", "Mexico", "United States", "United States",
                 "United States", "United States", "Chile", "South Africa",
                 "French Guiana", "Colombia"))
  expect_equal(res$countryCode[11:21],
               c("PY", "DE", "MX", "US", "US", "US", "US", "CL", "ZA", "GF", "CO"))
})


test_that(".std_country() trusts a valid countryCode and fills a missing one", {
  res <- .std_country(
    country = c("Alemanha", "Bolivia", NA, "Peru", "Colombia"),
    countryCode = c("DE", "bo", "AR", NA, "ZZ")
  )

  # the code wins and the name is replaced by the English one
  expect_equal(res$country, c("Germany", "Bolivia", "Argentina", "Peru", "Colombia"))
  # codes are upper-cased, invalid ones are replaced by the one found in the text
  expect_equal(res$countryCode, c("DE", "BO", "AR", "PE", "CO"))
})


test_that(".std_country() sets non-countries to NA and translates regional names", {
  x <- c("?", "s.loc.", "Indeterminado", "Capital", "Saladas", "Guiana", "Congo",
         strrep("long free text ", 10),
         paste0("Iugosl", .a_acute(), "via"), "Antilhas", "America do Norte")
  res <- .std_country(x, rep(NA_character_, length(x)))

  expect_true(all(is.na(res$country[1:8])))
  expect_true(all(is.na(res$countryCode)))
  expect_equal(res$country[9:11], c("Yugoslavia", "Antilles", "North America"))
})


test_that(".std_country() recognizes a Brazilian state typed in the country field", {
  res <- .std_country(c("Rio de Janeiro", "Alagoas", "Brasilia"), rep(NA_character_, 3))

  expect_equal(res$country, rep("Brazil", 3))
  expect_equal(res$countryCode, rep("BR", 3))
  expect_equal(res$state, c("Rio de Janeiro", "Alagoas", NA))
})


test_that(".match_brazil_state() normalizes case, accents, acronyms and typos", {
  x <- c("Goias", "goias", "GOIAS", paste0("Goi", .a_acute(), "s"), "PR", "Goyaz",
         "Matto Grosso", "Minas Gerias", "Minas Geraes", "Pernanbuco",
         "Tocamtins", "Rio Garnde do Sul", "Estado de Amazonas",
         "Sta Catharina", "Prov. S. Catharina", "Brasilia", "Bahaia")
  res <- .match_brazil_state(x)

  expected <- c(paste0("Goi", .a_acute(), "s"), paste0("Goi", .a_acute(), "s"),
                paste0("Goi", .a_acute(), "s"), paste0("Goi", .a_acute(), "s"),
                paste0("Paran", .a_acute()), paste0("Goi", .a_acute(), "s"),
                "Mato Grosso", "Minas Gerais", "Minas Gerais", "Pernambuco",
                "Tocantins", "Rio Grande do Sul", "Amazonas", "Santa Catarina",
                "Santa Catarina", "Distrito Federal", "Bahia")
  expect_equal(res, expected)
})


test_that(".match_brazil_state() repairs values corrupted by the source herbarium", {
  # accented letters exported as blanks, or the value truncated at the accent
  x <- c("Goi  s", "S  o Paulo", "Par", "Paran", "Cear", "Para")
  res <- .match_brazil_state(x)

  expect_equal(res, c(paste0("Goi", .a_acute(), "s"),
                      paste0("S", .a_tilde(), "o Paulo"),
                      paste0("Par", .a_acute()),
                      paste0("Paran", .a_acute()),
                      paste0("Cear", .a_acute()),
                      paste0("Par", .a_acute())))
})


test_that(".match_brazil_state() does not turn other places into states", {
  x <- c("Lima", "Cuzco", "Caracas", "Montevideo", "Santa Cruz", "Texas",
         "Buenos Aires", "Rio Negro", NA)
  expect_true(all(is.na(.match_brazil_state(x))))
})


test_that(".std_brazil_state() only changes Brazilian records and drops the country name", {
  res <- .std_brazil_state(
    state = c("Goias", "Brazil", "Brasil", "Goias", "Sao Luis", NA),
    is_brazil = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
  )

  expect_equal(res[1], paste0("Goi", .a_acute(), "s"))
  expect_true(is.na(res[2]))
  expect_true(is.na(res[3]))
  expect_equal(res[4], "Goias")           # not a Brazilian record: untouched
  expect_equal(res[5], "Sao Luis")        # not a state: kept as is
  expect_true(is.na(res[6]))
})


test_that(".std_type_status() gives English upper-case types", {
  x <- c("TYPUS", "ISOTYPUS", "Holotypus", "Holotipo", "Isotipo", "Paratipo",
         "Paratypus", "SYNTYPUS", "ISOSYNTYPUS", "Sintipo", "LECTOTYPUS",
         "ISONEOTYPUS", "EPITYPUS", "Syntype", NA)
  expect_equal(.std_type_status(x),
               c("TYPE", "ISOTYPE", "HOLOTYPE", "HOLOTYPE", "ISOTYPE", "PARATYPE",
                 "PARATYPE", "SYNTYPE", "ISOSYNTYPE", "SYNTYPE", "LECTOTYPE",
                 "ISONEOTYPE", "EPITYPE", "SYNTYPE", NA))
})


test_that(".std_id_qualifier() unifies cf. and aff.", {
  expect_equal(.std_id_qualifier(c("CF", "cf.", "cf", "AFF", "aff.", "aff", NA, "nr.")),
               c("cf.", "cf.", "cf.", "aff.", "aff.", "aff.", NA, "nr."))
})


test_that(".std_family_synonyms() maps conserved names and refreshes taxonName", {
  df <- data.frame(
    taxonRank = c("FAMILY", "SPECIES", "FAMILY", "FAMILY"),
    family = c("Labiatae", "Umbelliferae", "Fabaceae", "Compositeae"),
    genus = NA_character_, specificEpithet = NA_character_,
    infraspecificEpithet = NA_character_,
    taxonName = c("Labiatae", NA, "Fabaceae", "Compositeae"),
    stringsAsFactors = FALSE
  )

  res <- .std_family_synonyms(df)

  expect_equal(res$family, c("Lamiaceae", "Apiaceae", "Fabaceae", "Asteraceae"))
  expect_equal(res$taxonName[c(1, 3, 4)], c("Lamiaceae", "Fabaceae", "Asteraceae"))
})


test_that(".std_dates_coordinates() blanks impossible dates, coordinates and elevations", {
  df <- data.frame(
    year = c("2020", "0", "1774", "3000", "86"),
    month = c("5", "0", "1774", "12", "13"),
    day = c("31", "32", "0", "1", "15"),
    decimalLatitude = c(-12.9, 123456, 10, NA, 95),
    decimalLongitude = c(-38.5, 5, 200, -40, 10),
    minimumElevationInMeters = c("100", "-9999", "8001200", "-2", NA),
    maximumElevationInMeters = c(100, 100, 100, 100, 100),
    stringsAsFactors = FALSE
  )

  res <- .std_dates_coordinates(df)

  # 1774 is a plausible collecting year; 0, 86 and 3000 are not
  expect_equal(res$year, c(2020L, NA, 1774L, NA, NA))
  # a year-like value in the month column is blanked
  expect_equal(res$month, c(5L, NA, NA, 12L, NA))
  expect_equal(res$day, c(31L, NA, NA, 1L, 15L))
  # both coordinates are blanked when either is impossible
  expect_equal(res$decimalLatitude, c(-12.9, NA, NA, NA, NA))
  expect_equal(res$decimalLongitude, c(-38.5, NA, NA, -40, NA))
  expect_equal(res$minimumElevationInMeters, c(100, NA, NA, -2, NA))
  expect_type(res$maximumElevationInMeters, "double")
})


test_that(".coerce_parsed_types() gives each column a fixed class", {
  df <- data.frame(occurrenceID = 1:2, recordNumber = c(NA, NA), year = c("2020", NA),
                   decimalLatitude = c("1.5", NA), family = c("A", "B"),
                   stringsAsFactors = FALSE)

  res <- .coerce_parsed_types(df)

  expect_type(res$occurrenceID, "character")
  expect_type(res$recordNumber, "character")
  expect_type(res$year, "integer")
  expect_type(res$decimalLatitude, "double")
  expect_type(res$family, "character")
})


test_that(".std_parsed_columns() standardizes a complete record table", {
  df <- data.frame(
    country = c("Brasil", paste0("Br", .e_acute(), "sil"), "Alemanha", "?", NA),
    countryCode = c(NA, NA, NA, NA, NA),
    stateProvince = c("Goias", "Minas Gerias", "Bayern", "s.loc.", "Goias"),
    municipality = c("  Rio de Janeiro ", "-", NA, NA, NA),
    typeStatus = c("Holotipo", NA, NA, NA, NA),
    identificationQualifier = c("CF", NA, NA, NA, NA),
    family = c("Labiatae", "Fabaceae", "Fabaceae", "Fabaceae", "Fabaceae"),
    taxonRank = "FAMILY", genus = NA_character_, specificEpithet = NA_character_,
    infraspecificEpithet = NA_character_, taxonName = NA_character_,
    year = c("2020", "0", "2019", NA, NA),
    stringsAsFactors = FALSE
  )

  res <- .std_parsed_columns(df)

  expect_equal(res$country, c("Brazil", "Brazil", "Germany", NA, NA))
  expect_equal(res$countryCode, c("BR", "BR", "DE", NA, NA))
  # only Brazilian records get their state names standardized
  expect_equal(res$stateProvince,
               c(paste0("Goi", .a_acute(), "s"), "Minas Gerais", "Bayern", NA, "Goias"))
  expect_equal(res$municipality, c("Rio de Janeiro", NA, NA, NA, NA))
  expect_equal(res$typeStatus[1], "HOLOTYPE")
  expect_equal(res$identificationQualifier[1], "cf.")
  expect_equal(res$family[1], "Lamiaceae")
  expect_equal(res$year, c(2020L, NA, 2019L, NA, NA))
})


test_that(".std_parsed_columns() works on tables that lack some columns", {
  df <- data.frame(family = "Labiatae", stringsAsFactors = FALSE)
  res <- .std_parsed_columns(df)
  expect_equal(res$family, "Lamiaceae")
})
