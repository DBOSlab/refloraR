# Helpers that standardize the (very heterogeneous) metadata supplied by the
# Reflora-associated herbaria, applied by reflora_parse() after the taxonomic
# cleaning done in .std_inside_columns().
# Author: Domingos Cardoso
#
# This file is kept ASCII-only on purpose: country names are matched on a
# transliterated key and accented Brazilian state names are built at runtime
# (see .brazil_states()).


#_______________________________________________________________________________
# Brazilian states: canonical name (with diacritics) -> two-letter acronym ####

.brazil_states <- function() {

  # Accented state names are built at *runtime* with intToUtf8(), not typed
  # as \uXXXX escapes: under a non-UTF-8 session locale (e.g. LC_CTYPE=C,
  # common in CI), package sourcing has been observed to parse \uXXXX
  # escapes into the literal 8-character text "<U+00E3>" instead of the
  # intended single accented character, which no amount of post-hoc
  # Encoding<- can recover. intToUtf8() sidesteps that parse-time escape
  # resolution entirely.
  a_acute <- intToUtf8(0x00E1)
  i_acute <- intToUtf8(0x00ED)
  a_tilde <- intToUtf8(0x00E3)
  o_circumflex <- intToUtf8(0x00F4)

  stats::setNames(
    c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS",
      "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC",
      "SP", "SE", "TO"),
    c("Acre", "Alagoas", paste0("Amap", a_acute), "Amazonas", "Bahia",
      paste0("Cear", a_acute), "Distrito Federal",
      paste0("Esp", i_acute, "rito Santo"), paste0("Goi", a_acute, "s"),
      paste0("Maranh", a_tilde, "o"), "Mato Grosso", "Mato Grosso do Sul",
      "Minas Gerais", paste0("Par", a_acute), paste0("Para", i_acute, "ba"),
      paste0("Paran", a_acute), "Pernambuco", paste0("Piau", i_acute),
      "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul",
      paste0("Rond", o_circumflex, "nia"), "Roraima", "Santa Catarina",
      paste0("S", a_tilde, "o Paulo"), "Sergipe", "Tocantins")
  )
}


#_______________________________________________________________________________
# Generic text helpers ####

# Lower-case ASCII transliteration without brackets, question marks or
# parenthetical remarks, and with squished whitespace.
.norm_text <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- stringi::stri_trans_tolower(x)
  x <- gsub("\\[|\\]|\\?", "", x)
  x <- gsub("\\([^)]*\\)?", "", x)
  x <- stringi::stri_replace_all_regex(x, "[\\s\\p{Z}]+", " ")
  trimws(x)
}

# Letters-only key after transliterating accents (e.g. "São Paulo" ->
# "saopaulo")
.key_ascii <- function(x) {
  x <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  gsub("[^a-z]", "", stringi::stri_trans_tolower(x))
}

# Letters-only key that *drops* accented letters instead of transliterating
# them (e.g. "Goiás" -> "gois"). Some herbaria export accented
# characters as blanks or truncate the value at the accent ("Goi  s", "Par",
# "Paran"), and this key is what those corrupted values have in common with
# the correct name.
.key_drop <- function(x) {
  gsub("[^a-z]", "", stringi::stri_trans_tolower(as.character(x)))
}

.is_placeholder <- function(x) {
  placeholders <- c("", "-", "--", "s.loc.", "s. loc.", "s.loc", "s/loc",
                    "s.l.", "indeterminado", "indeterminada", "n/a", "na",
                    "null", "unknown", "desconhecido", "desconhecida",
                    "sem informacao", "sem localidade", "not available")
  is.na(x) | .norm_text(x) %in% placeholders
}

# Collapse any kind of blank (including non-breaking spaces) to a single
# regular space, trim, and turn empty strings into NA.
.std_whitespace <- function(df) {
  chr <- names(df)[vapply(df, is.character, logical(1))]
  for (cn in chr) {
    x <- df[[cn]]
    has <- !is.na(x) &
      stringi::stri_detect_regex(x, "^[\\s\\p{Z}]|[\\s\\p{Z}]$|[\\s\\p{Z}]{2,}|\\p{Z}")
    if (any(has)) {
      x[has] <- stringi::stri_replace_all_regex(x[has], "[\\s\\p{Z}]+", " ")
      x[has] <- trimws(x[has])
    }
    x[!is.na(x) & !nzchar(x)] <- NA_character_
    df[[cn]] <- x
  }
  df
}


#_______________________________________________________________________________
# Countries ####

# Country names that the multilingual lookup cannot resolve by itself:
# historical names, regional spellings, abbreviations and typos found in the
# Reflora collections. Keys are in the format returned by .norm_text().
.country_overrides <- function() {
  c("brasil" = "BR", "brazil" = "BR", "bresil" = "BR", "brasilien" = "BR",
    "brasilia" = "BR", "brasil meridional" = "BR", "bresil austral" = "BR",
    "usa" = "US", "u.s.a." = "US", "e.u.a." = "US", "eua" = "US",
    "united states" = "US", "united states o" = "US",
    "united states of america" = "US", "estados unidos" = "US",
    "estados unidos da america" = "US",
    "inglaterra" = "GB", "england" = "GB", "great britain" = "GB",
    "reino unido" = "GB",
    "nouv granada" = "CO", "nova granada" = "CO", "nueva granada" = "CO",
    "nova hollandia" = "AU", "nova holanda" = "AU",
    "peru oriental" = "PE",
    "guiane hollandaise suriname" = "SR", "surinam" = "SR",
    "guyane" = "GF", "guyane francaise" = "GF", "french guiana" = "GF",
    "guiana francesa" = "GF",
    "guiana inglesa" = "GY", "guiana british" = "GY", "british guiana" = "GY",
    "guyana" = "GY",
    "porto rico" = "PR", "puerto rico" = "PR",
    "trinidad e tobago" = "TT", "trinidad and tobago" = "TT")
}

# Historical or regional names without an ISO 3166-1 code that are translated
# to English instead (the country code stays NA). Keys as in .norm_text().
.country_regions <- function() {
  c("iugoslavia" = "Yugoslavia", "yugoslavia" = "Yugoslavia",
    "antilhas" = "Antilles", "antilles" = "Antilles",
    "lesser antilles" = "Lesser Antilles",
    "america do norte" = "North America", "north america" = "North America",
    "america central" = "Central America",
    "central america, s. loc." = "Central America",
    "central america" = "Central America",
    "caribbean, s. loc." = "Caribbean", "caribbean" = "Caribbean",
    "caribe" = "Caribbean")
}

# Names that look like a country but are ambiguous and must not be resolved.
.country_ambiguous <- function() {
  c("guiana", "guayana", "guiane", "congo", "america", "antilhas")
}

# Named vector (normalized name -> ISO 3166-1 alpha-2) built from the English
# and CLDR (pt, es, fr, de, it) country names shipped with 'countrycode'.
.country_lookup <- function() {
  cl <- countrycode::codelist
  cols <- intersect(c("country.name.en", "cldr.name.en", "cldr.name.pt",
                      "cldr.name.es", "cldr.name.fr", "cldr.name.de",
                      "cldr.name.it"), names(cl))
  keys <- unlist(lapply(cols, function(cn) .norm_text(cl[[cn]])))
  iso <- rep(cl$iso2c, times = length(cols))
  ok <- !is.na(keys) & nzchar(keys) & !is.na(iso)
  keys <- keys[ok]
  iso <- iso[ok]
  keep <- !duplicated(keys)
  stats::setNames(iso[keep], keys[keep])
}

# Resolve free-text country names (any of the languages above, with typos and
# bracketed/parenthetical remarks) to ISO alpha-2 codes. When the text is the
# name of a Brazilian state, the country is Brazil and the state is returned
# as well so it can be used to fill a missing stateProvince.
.country_text_to_iso2 <- function(x) {
  key <- .norm_text(x)
  iso <- stats::setNames(rep(NA_character_, length(x)), x)
  state <- stats::setNames(rep(NA_character_, length(x)), x)

  ambiguous <- key %in% .country_ambiguous()

  ov <- .country_overrides()
  hit <- match(key, names(ov))
  iso[!is.na(hit)] <- ov[hit[!is.na(hit)]]

  todo <- is.na(iso) & !ambiguous & nzchar(key)
  if (any(todo)) {
    lk <- .country_lookup()
    hit <- match(key[todo], names(lk))
    iso[which(todo)[!is.na(hit)]] <- lk[hit[!is.na(hit)]]
  }

  # Typos of "Brasil" (Brasi, Brasl, Barsil, Basil, Brasill, ...)
  todo <- is.na(iso) & !ambiguous & nchar(key) >= 4 & nchar(key) <= 9 &
    substr(key, 1, 1) == "b"
  if (any(todo)) {
    d <- utils::adist(key[todo], c("brasil", "brazil", "bresil"))
    iso[which(todo)[apply(d, 1, min) <= 2]] <- "BR"
  }

  # Brazilian state written in the country field
  todo <- is.na(iso) & !ambiguous
  if (any(todo)) {
    st <- .match_brazil_state(x[todo], allow_acronym = FALSE, allow_drop = FALSE)
    found <- !is.na(st)
    iso[which(todo)[found]] <- "BR"
    state[which(todo)[found]] <- st[found]
  }

  list(iso2 = unname(iso), state = unname(state))
}

# Standardize the country columns: country names in English (as returned by
# 'countrycode') and ISO alpha-2 codes. A valid countryCode always wins; the
# text is used to fill in a missing code. Historical or regional names
# (e.g. "Iugoslavia", "Antilhas") are translated to English, and anything
# else that cannot be resolved to a country (placeholders such as "?" or
# "Indeterminado", localities typed into the country field, free text)
# becomes NA, so that the column only holds English country names.
.std_country <- function(country, countryCode) {

  country <- as.character(country)
  code <- toupper(trimws(as.character(countryCode)))
  cl <- countrycode::codelist

  code[!code %in% cl$iso2c] <- NA_character_
  state_fill <- rep(NA_character_, length(country))

  need <- is.na(code) & !is.na(country)
  if (any(need)) {
    u <- unique(country[need])
    res <- .country_text_to_iso2(u)
    m <- match(country[need], u)
    code[need] <- res$iso2[m]
    state_fill[need] <- res$state[m]
  }

  out <- country
  has_code <- !is.na(code)
  out[has_code] <- cl$country.name.en[match(code[has_code], cl$iso2c)]

  unresolved <- !has_code & !is.na(country)
  if (any(unresolved)) {
    regions <- .country_regions()
    key <- tolower(trimws(stringi::stri_trans_general(country[unresolved], "Latin-ASCII")))
    key <- gsub("\\s+", " ", key)
    hit <- match(key, names(regions))
    key2 <- .norm_text(country[unresolved])
    hit2 <- match(key2, names(regions))
    hit[is.na(hit)] <- hit2[is.na(hit)]
    out[unresolved] <- unname(regions[hit])
  }

  list(country = out, countryCode = code, state = state_fill)
}


#_______________________________________________________________________________
# Brazilian states ####

# Match state values against the canonical Brazilian state names. Handles
# case, accents, trailing/inner blanks, two-letter acronyms, a few historical
# spellings and values corrupted by the source herbarium (accented characters
# exported as blanks, or truncated at the accent).
.match_brazil_state <- function(x, allow_acronym = TRUE, allow_drop = TRUE,
                                allow_fuzzy = TRUE) {

  canon <- .brazil_states()
  canon_names <- names(canon)
  k_ascii <- .key_ascii(canon_names)
  k_drop <- .key_drop(canon_names)

  xx <- as.character(x)
  out <- rep(NA_character_, length(xx))

  # "Estado de Amazonas", "Prov. S. Catharina", ...
  cleaned <- gsub("^(estado|provincia|prov|province|state)\\.?\\s*(do|da|de|of)?\\s+",
                  "", stringi::stri_trans_tolower(
                    stringi::stri_trans_general(xx, "Latin-ASCII")))
  key <- gsub("[^a-z]", "", cleaned)

  # historical or abbreviated spellings
  key <- gsub("catharina", "catarina", key, fixed = TRUE)
  key <- gsub("^(sta|st|s)(?=catarina)", "santa", key, perl = TRUE)
  key <- gsub("^matto", "mato", key)
  key <- gsub("geraes$", "gerais", key)
  key <- gsub("^goyaz$", "goias", key)
  key <- gsub("^piauhy$", "piaui", key)
  key <- gsub("^(brasilia|brasiliadistritofederal|distritofederalbrasilia)$",
              "distritofederal", key)

  m <- match(key, k_ascii)
  out[!is.na(m)] <- canon_names[m[!is.na(m)]]

  if (allow_acronym) {
    todo <- is.na(out) & !is.na(xx) & nchar(trimws(xx)) == 2
    m <- match(toupper(trimws(xx[todo])), canon)
    out[which(todo)[!is.na(m)]] <- canon_names[m[!is.na(m)]]
  }

  if (allow_drop) {
    unique_drop <- k_drop[!(duplicated(k_drop) | duplicated(k_drop, fromLast = TRUE))]
    todo <- is.na(out) & !is.na(xx)
    kd <- .key_drop(xx[todo])
    m <- match(kd, k_drop)
    ok <- !is.na(m) & nzchar(kd) & kd %in% unique_drop
    out[which(todo)[ok]] <- canon_names[m[ok]]
  }

  # typos (Gerias, Pernanbuco, Tocamtins, Garnde, ...): a single closest state
  # within one edit (two for names of 8+ letters); shorter names must match
  # exactly
  if (allow_fuzzy) {
    todo <- which(is.na(out) & !is.na(xx) & nchar(key) >= 6)
    if (length(todo) > 0) {
      d <- utils::adist(key[todo], k_ascii)
      best <- apply(d, 1, min)
      n_best <- apply(d, 1, function(r) sum(r == min(r)))
      pick <- apply(d, 1, which.min)
      ok <- best <= ifelse(nchar(key[todo]) >= 8, 2, 1) & n_best == 1
      out[todo[ok]] <- canon_names[pick[ok]]
    }
  }

  out
}

# Standardize the stateProvince of Brazilian records (countryCode "BR") to the
# canonical state names; other records are left untouched. The name of the
# country itself ("Brasil", "Brazil") is not a state and becomes NA.
.std_brazil_state <- function(state, is_brazil) {
  state <- as.character(state)
  idx <- which(is_brazil & !is.na(state))
  if (length(idx) == 0) {
    return(state)
  }
  u <- unique(state[idx])
  std <- .match_brazil_state(u)
  m <- match(state[idx], u)
  hit <- !is.na(std[m])
  state[idx[hit]] <- std[m][hit]
  not_a_state <- .norm_text(state[idx]) %in% c("brasil", "brazil", "bresil")
  state[idx[not_a_state]] <- NA_character_
  state
}


#_______________________________________________________________________________
# Controlled-vocabulary columns ####

# typeStatus in English (GBIF vocabulary, upper case), e.g. "Holotipo" and
# "HOLOTYPUS" become "HOLOTYPE".
.std_type_status <- function(x) {
  x <- as.character(x)
  key <- toupper(trimws(stringi::stri_trans_general(x, "Latin-ASCII")))
  key <- gsub("[.]+$", "", key)
  key <- gsub("TYPUS$|TIPO$|TYPE$", "TYPE", key)
  key <- gsub("^SIN(?=TYPE)", "SYN", key, perl = TRUE)
  key <- gsub("^ISOSIN(?=TYPE)", "ISOSYN", key, perl = TRUE)
  key[is.na(x)] <- NA_character_
  key
}

# identificationQualifier as "cf." / "aff."
.std_id_qualifier <- function(x) {
  x <- as.character(x)
  key <- tolower(trimws(x))
  x[!is.na(key) & key %in% c("cf", "cf.")] <- "cf."
  x[!is.na(key) & key %in% c("aff", "aff.")] <- "aff."
  x
}

# Conserved alternative family names replaced by the corresponding -aceae
# names used by the Flora e Funga do Brasil, so that filtering by taxon works
# regardless of which name each herbarium used.
.std_family_synonyms <- function(df) {

  if (!"family" %in% names(df)) {
    return(df)
  }

  syn <- c("labiatae" = "Lamiaceae", "umbelliferae" = "Apiaceae",
           "compositae" = "Asteraceae", "compositeae" = "Asteraceae",
           "cruciferae" = "Brassicaceae", "gramineae" = "Poaceae",
           "leguminosae" = "Fabaceae", "palmae" = "Arecaceae")

  m <- match(tolower(df$family), names(syn))
  tf <- !is.na(m)
  if (any(tf)) {
    df$family[tf] <- syn[m[tf]]
    if (all(c("taxonRank", "genus", "specificEpithet",
              "infraspecificEpithet") %in% names(df))) {
      df <- .fill_taxon_name(df)
    }
  }
  df
}


#_______________________________________________________________________________
# Dates, coordinates and elevation ####

.std_dates_coordinates <- function(df) {

  current_year <- as.integer(format(Sys.Date(), "%Y"))

  limit_int <- function(x, lower, upper) {
    x <- suppressWarnings(as.integer(x))
    x[!is.na(x) & (x < lower | x > upper)] <- NA_integer_
    x
  }
  if ("year" %in% names(df)) df$year <- limit_int(df$year, 1500L, current_year)
  if ("month" %in% names(df)) df$month <- limit_int(df$month, 1L, 12L)
  if ("day" %in% names(df)) df$day <- limit_int(df$day, 1L, 31L)

  if (all(c("decimalLatitude", "decimalLongitude") %in% names(df))) {
    lat <- suppressWarnings(as.numeric(df$decimalLatitude))
    lon <- suppressWarnings(as.numeric(df$decimalLongitude))
    bad <- (!is.na(lat) & abs(lat) > 90) | (!is.na(lon) & abs(lon) > 180)
    lat[bad] <- NA_real_
    lon[bad] <- NA_real_
    df$decimalLatitude <- lat
    df$decimalLongitude <- lon
  }

  for (cn in c("minimumElevationInMeters", "maximumElevationInMeters")) {
    if (cn %in% names(df)) {
      el <- suppressWarnings(as.numeric(df[[cn]]))
      el[!is.na(el) & (el < -500 | el > 9000)] <- NA_real_
      df[[cn]] <- el
    }
  }

  df
}

# Give every parsed herbarium the same column classes so that the elements
# returned by reflora_parse() can be row-bound without type conflicts.
.coerce_parsed_types <- function(df) {
  numeric_cols <- c("decimalLatitude", "decimalLongitude",
                    "minimumElevationInMeters", "maximumElevationInMeters")
  integer_cols <- c("year", "month", "day")
  for (cn in names(df)) {
    if (cn %in% numeric_cols) {
      df[[cn]] <- suppressWarnings(as.numeric(df[[cn]]))
    } else if (cn %in% integer_cols) {
      df[[cn]] <- suppressWarnings(as.integer(df[[cn]]))
    } else if (!is.character(df[[cn]])) {
      df[[cn]] <- as.character(df[[cn]])
    }
  }
  df
}


#_______________________________________________________________________________
# Wrapper applied to each parsed herbarium ####

.std_parsed_columns <- function(df) {

  df <- .std_whitespace(df)

  for (cn in c("country", "stateProvince", "municipality", "locality")) {
    if (cn %in% names(df)) {
      df[[cn]][.is_placeholder(df[[cn]])] <- NA_character_
    }
  }

  if (all(c("country", "countryCode") %in% names(df))) {
    ctry <- .std_country(df$country, df$countryCode)
    df$country <- ctry$country
    df$countryCode <- ctry$countryCode

    if ("stateProvince" %in% names(df)) {
      fill <- is.na(df$stateProvince) & !is.na(ctry$state)
      df$stateProvince[fill] <- ctry$state[fill]
      df$stateProvince <- .std_brazil_state(
        df$stateProvince,
        is_brazil = !is.na(df$countryCode) & df$countryCode == "BR"
      )
    }
  }

  if ("typeStatus" %in% names(df)) {
    df$typeStatus <- .std_type_status(df$typeStatus)
  }
  if ("identificationQualifier" %in% names(df)) {
    df$identificationQualifier <- .std_id_qualifier(df$identificationQualifier)
  }

  df <- .std_family_synonyms(df)
  df <- .std_dates_coordinates(df)
  df <- .coerce_parsed_types(df)

  df
}
