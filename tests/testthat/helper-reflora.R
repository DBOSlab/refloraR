skip_if_no_reflora <- function() {
  if (!identical(Sys.getenv("REFLORA_LIVE_TESTS"), "true")) {
    testthat::skip(
      "Live REFLORA integration test; set REFLORA_LIVE_TESTS=true to run."
    )
  }
}

# A small synthetic Reflora dcat catalog (same structure as the live
# https://ipt.jbrj.gov.br/reflora/dcat feed) with three datasets:
#  - heph: an ordinary, non-repatriated collection
#  - p_reflora: a repatriated collection ("Amostras Brasileiras Repatriadas")
#  - nyh: exercises the NYH -> NY collection-code correction
.fake_dcat_lines <- function() {
  c(
    '@prefix dct: <http://purl.org/dc/terms/> .',
    '@prefix dcat: <http://www.w3.org/ns/dcat#> .',
    '@prefix vcard: <http://www.w3.org/2006/vcard/ns#> .',
    '',
    '<https://ipt.jbrj.gov.br/reflora/resource?r=heph#Dataset>',
    'a dcat:Dataset ;',
    'dct:title "HEPH herbarium - Jardim Botânico de Brasília - Herbário Virtual REFLORA" ;',
    'dcat:keyword "Occurrence" , "Specimen" ;',
    'dcat:contactPoint [ a vcard:Individual ; vcard:fn "Roberta Chacon"; vcard:hasEmail <mailto:rgchacon@gmail.com> ] ;',
    'dct:modified "2026-09-15T01:08-03:00" ;',
    'dcat:landingPage <https://ipt.jbrj.gov.br/reflora/resource?r=heph> ;',
    'dct:identifier "https://www.gbif.org/dataset/1aedcdff-24f2-4925-8e55-23eeef236b0d" ;',
    'dcat:distribution <https://ipt.jbrj.gov.br/reflora/archive.do?r=heph> ;',
    'dct:language <http://id.loc.gov/vocabulary/iso639-1/en> .',
    '',
    '<https://ipt.jbrj.gov.br/reflora/archive.do?r=heph>',
    'a dcat:Distribution ;',
    'dct:title "Darwin Core Archive of HEPH herbarium" ;',
    'dct:format "dwc-a" ;',
    'dcat:mediaType "application/zip" ;',
    'dcat:downloadURL <https://ipt.jbrj.gov.br/reflora/archive.do?r=heph> ;',
    'dcat:accessURL <https://ipt.jbrj.gov.br/reflora/resource?r=heph> .',
    '',
    '<https://ipt.jbrj.gov.br/reflora/resource?r=p_reflora#Dataset>',
    'a dcat:Dataset ;',
    'dct:title "P herbarium - Muséum national d’Histoire naturelle - Amostras Brasileiras Repatriadas - Herbário Virtual REFLORA" ;',
    'dcat:keyword "Occurrence" , "Specimen" ;',
    'dcat:contactPoint [ a vcard:Individual ; vcard:fn "Vanessa Invernon"; vcard:hasEmail <mailto:vanessa.invernon@mnhn.fr> ] ;',
    'dct:modified "2026-07-01T09:04-03:00" ;',
    'dcat:landingPage <https://ipt.jbrj.gov.br/reflora/resource?r=p_reflora> ;',
    'dct:identifier "https://www.gbif.org/dataset/9118eee0-42d2-4a27-9ae0-78d49d163a5b" ;',
    'dcat:distribution <https://ipt.jbrj.gov.br/reflora/archive.do?r=p_reflora> ;',
    'dct:language <http://id.loc.gov/vocabulary/iso639-1/en> .',
    '',
    '<https://ipt.jbrj.gov.br/reflora/archive.do?r=p_reflora>',
    'a dcat:Distribution ;',
    'dct:title "Darwin Core Archive of P herbarium" ;',
    'dct:format "dwc-a" ;',
    'dcat:mediaType "application/zip" ;',
    'dcat:downloadURL <https://ipt.jbrj.gov.br/reflora/archive.do?r=p_reflora> ;',
    'dcat:accessURL <https://ipt.jbrj.gov.br/reflora/resource?r=p_reflora> .',
    '',
    '<https://ipt.jbrj.gov.br/reflora/resource?r=nyh#Dataset>',
    'a dcat:Dataset ;',
    'dct:title "NY herbarium - The New York Botanical Garden - Amostras Brasileiras Repatriadas - Herbário Virtual REFLORA" ;',
    'dcat:keyword "Occurrence" , "Specimen" ;',
    'dcat:contactPoint [ a vcard:Individual ; vcard:fn "No Contact"; vcard:hasEmail <mailto:none@example.com> ] ;',
    'dct:modified "2026-01-10T08:00-03:00" ;',
    'dcat:landingPage <https://ipt.jbrj.gov.br/reflora/resource?r=nyh> ;',
    'dct:identifier "https://www.gbif.org/dataset/00000000-0000-0000-0000-000000000000" ;',
    'dcat:distribution <https://ipt.jbrj.gov.br/reflora/archive.do?r=nyh> ;',
    'dct:language <http://id.loc.gov/vocabulary/iso639-1/en> .',
    '',
    '<https://ipt.jbrj.gov.br/reflora/archive.do?r=nyh>',
    'a dcat:Distribution ;',
    'dct:title "Darwin Core Archive of NY herbarium" ;',
    'dct:format "dwc-a" ;',
    'dcat:mediaType "application/zip" ;',
    'dcat:downloadURL <https://ipt.jbrj.gov.br/reflora/archive.do?r=nyh> ;',
    'dcat:accessURL <https://ipt.jbrj.gov.br/reflora/resource?r=nyh> .'
  )
}

# A synthetic IPT resource page, structured like the real
# https://ipt.jbrj.gov.br/reflora/resource?r=<code> HTML: the "latestVersion"
# table row (version, publish date, record count) is at a configurable line
# offset, so tests can exercise both the bounded (n = 900) read and the
# full-page fallback when that offset is pushed past it.
# A minimal synthetic occurrence.txt data.frame, with just enough columns to
# survive .merge_occur_txt()/.filter_occur_df()/.reorder_df() unmodified, for
# mocking reflora_parse()'s return value in reflora_records()/reflora_indets()
# tests without needing real Darwin Core Archive files or finch::dwca_read().
.fake_dwca_files <- function() {
  occurrence <- data.frame(
    occurrenceID = c("HEPH:1", "HEPH:2"),
    collectionCode = c("HEPH", "HEPH"),
    catalogNumber = c("1", "2"),
    taxonRank = c("SPECIES", "FAMILY"),
    family = c("Fabaceae", "Fabaceae"),
    genus = c("Inga", NA_character_),
    specificEpithet = c("edulis", NA_character_),
    species = c("Inga edulis", NA_character_),
    infraspecificEpithet = c(NA_character_, NA_character_),
    taxonName = c("Inga edulis", NA_character_),
    scientificNameAuthorship = c(NA_character_, NA_character_),
    scientificName = c("Inga edulis Mart.", NA_character_),
    recordedBy = c("J. Silva", "J. Silva"),
    recordNumber = c("101", "102"),
    eventDate = c("2020-01-05", "2020-02-10"),
    year = c("2020", "2020"),
    month = c("01", "02"),
    day = c("05", "10"),
    country = c("Brazil", "Brazil"),
    stateProvince = c("Bahia", "Bahia"),
    municipality = c("Salvador", "Salvador"),
    decimalLatitude = c(-12.97, -12.98),
    decimalLongitude = c(-38.5, -38.51),
    verbatimLatitude = c("-12.97", "-12.98"),
    verbatimLongitude = c("-38.5", "-38.51"),
    minimumElevationInMeters = c("10", "12"),
    maximumElevationInMeters = c("10", "12"),
    basisOfRecord = c("PRESERVED_SPECIMEN", "PRESERVED_SPECIMEN"),
    stringsAsFactors = FALSE
  )

  list(HEPH = list(data = list("occurrence.txt" = occurrence)))
}

# A minimal synthetic finch::dwca_read()-shaped object: raw (pre-
# standardization) occurrence.txt columns, for mocking reflora_parse()'s own
# call to finch::dwca_read() without needing a real Darwin Core Archive.
.fake_raw_dwca_object <- function(folder_name = "dwca_heph_v1") {
  occurrence <- data.frame(
    occurrenceID = c("HEPH:1", "HEPH:2"),
    catalogNumber = c("1", "2"),
    collectionCode = c("HEPH", "HEPH"),
    taxonRank = c("SPECIES", "FAMILY"),
    family = c("Fabaceae", "Fabaceae"),
    genus = c("Inga", NA_character_),
    specificEpithet = c("edulis", NA_character_),
    infraspecificEpithet = c(NA_character_, NA_character_),
    scientificNameAuthorship = c("Mart.", NA_character_),
    recordedBy = c("J. Silva", "J. Silva"),
    recordNumber = c("101", "102"),
    fieldNumber = c(NA_character_, NA_character_),
    eventDate = c("2020-01-05", "2020-02-10"),
    year = c("2020", "2020"),
    month = c("01", "02"),
    day = c("05", "10"),
    fieldNotes = c(NA_character_, NA_character_),
    occurrenceRemarks = c(NA_character_, NA_character_),
    eventRemarks = c(NA_character_, NA_character_),
    country = c("Brazil", "Brazil"),
    countryCode = c("BR", "BR"),
    stateProvince = c("Bahia", "Bahia"),
    municipality = c("Salvador", "Salvador"),
    locality = c(NA_character_, NA_character_),
    minimumElevationInMeters = c("10", "12"),
    maximumElevationInMeters = c("10", "12"),
    decimalLatitude = c(-12.97, -12.98),
    decimalLongitude = c(-38.5, -38.51),
    verbatimLatitude = c("-12.97", "-12.98"),
    verbatimLongitude = c("-38.5", "-38.51"),
    identificationQualifier = c(NA_character_, NA_character_),
    typeStatus = c(NA_character_, NA_character_),
    identifiedBy = c(NA_character_, NA_character_),
    dateIdentified = c(NA_character_, NA_character_),
    identificationRemarks = c(NA_character_, NA_character_),
    basisOfRecord = c("PreservedSpecimen", "PreservedSpecimen"),
    # shaped like real Reflora raw values: scheme-less JBRJ deep-zoom (DZI)
    # tile-server references, sometimes multiple per record separated by "|"
    associatedMedia = c(
      "jbrj-public.s3-sa-east-1.amazonaws.com/fsi/server?type=image&source=DZI/heph/heph/0/0/1/1/heph00000001.dzi",
      paste0(
        "jbrj-public.s3-sa-east-1.amazonaws.com/fsi/server?type=image&source=DZI/heph/heph/0/0/2/2/heph00000002.dzi|",
        "jbrj-public.s3-sa-east-1.amazonaws.com/fsi/server?type=image&source=DZI/heph/heph/0/0/2/2/heph00000002_1.dzi"
      )
    ),
    stringsAsFactors = FALSE
  )

  list(
    data = list("occurrence.txt" = occurrence),
    files = list(xml_files = paste0(folder_name, "/eml.xml"))
  )
}

.fake_resource_page <- function(version = "1.223",
                                published = "2026-09-15 01:08:54",
                                records = "28,697",
                                offset = 20) {
  c(
    rep("<!-- padding line to simulate page header/sidebar/description -->", offset),
    '        var aDataSet = [',
    sprintf(
      "            ['<img class=\"latestVersion\" src=\"forward_enabled_hover.png\"/>%s',",
      version
    ),
    sprintf("                '%s',", published),
    sprintf("                '%s',", records),
    '                "None provided&nbsp;",',
    "                '',",
    '                ""],',
    rep("<!-- padding line to simulate historical version rows -->", 20)
  )
}
