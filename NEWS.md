# refloraR (development version)

## `reflora_parse()` now standardizes herbarium metadata

Inspecting 40 Reflora herbaria (390,889 records) showed that the metadata supplied by each herbarium was far from consistent (e.g., six spellings of Brazil in `country`, 72k records without `countryCode`, state names with typos or corrupted accents, `typeStatus` in Latin, Portuguese and Spanish, month values such as `1774`). `reflora_parse()` now returns:

- `country` in English (e.g., `Brazil`, `Germany`, `United States`) with the matching ISO 3166-1 alpha-2 `countryCode` filled in, resolving names in Portuguese, Spanish, French, German and Italian, bracketed values and common typos. Placeholders (`?`, `s.loc.`, `Indeterminado`) and non-country text become `NA`.
- `stateProvince` of Brazilian records with the official state names (e.g., `Goias`, `GO`, `Goyaz` and values corrupted at the source such as `Goi  s` all become `Goiás`), so filtering with `reflora_records(state = ...)` no longer misses spelling variants.
- `typeStatus` in English upper case (e.g., `Holotipo` becomes `HOLOTYPE`) and `identificationQualifier` as `cf.`/`aff.`.
- Conserved family names replaced by their `-aceae` equivalents (`Labiatae` → `Lamiaceae`, `Umbelliferae` → `Apiaceae`, `Compositae` → `Asteraceae`, `Cruciferae` → `Brassicaceae`, `Gramineae` → `Poaceae`, `Leguminosae` → `Fabaceae`, `Palmae` → `Arecaceae`).
- The Darwin Core Archive core identifier `id` is now kept as the first column (as character) of every parsed herbarium, and therefore of the output of `reflora_records()` and `reflora_indets()`; it was previously dropped.
- Impossible `year`, `month`, `day`, coordinate and elevation values set to `NA`; leading/trailing/repeated blanks (including non-breaking spaces) removed from all text columns.

## Bug fixes

- `reflora_parse()` dropped, for all herbaria processed after it, any column missing in one herbarium (the column list was reassigned inside the loop). Every herbarium is now returned with the same columns, in the same order and with the same classes, so the parsed tables can be row-bound directly (previously `dplyr::bind_rows()` failed on, e.g., `occurrenceID` being integer in some herbaria and character in others).
- `reflora_parse()` failed on records with `associatedMedia` because the helper that cleans media URLs did not exist; these are now normalized to valid `https://` URLs.

`countrycode` was added to `Imports`.

# refloraR 1.0.0

## Initial Release

The first official release of the `refloraR` R package, designed to streamline access to plant specimen data from the REFLORA Virtual Herbarium hosted by the Rio de Janeiro Botanical Garden.

### Features

- `reflora_summary()`: Retrieve metadata and summary info from all or specific REFLORA herbaria.
- `reflora_download()`: Download original specimen data in Darwin Core Archive (DwC-A) format.
- `reflora_records()`: Parse, filter, and organize REFLORA records based on taxon, herbarium, region, and year.
- `reflora_indets()`: Retrieve indeterminate specimens (e.g., identified only to family or genus rank).
- Optional filters by `taxon`, `herbarium`, `state`, `recordYear`, and `level`.
- Supports integration with tidyverse workflows for downstream analyses.
- Repatriated collection filtering (`repatriated = TRUE` by default).
- Test coverage >95%, continuous integration via GitHub Actions.

### Infrastructure

- MIT license.
- GitHub Actions: R-CMD-check and test coverage.
- Hosted documentation: [refloraR-website](https://dboslab.github.io/refloraR-website/)

### Feedback

Please report bugs or issues at:  
<https://github.com/DBOSlab/refloraR/issues>
