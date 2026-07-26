# Source Inventory

## Coverage

- Coverage level: strong for CNPV 2002/2010 aggregates, detailed XCNPV 2022 analysis and ENHOGAR 2024; relationship links cover 2010/2022, while official labor-force time series are not yet stored in PostgreSQL.
- Sources checked: live PostgreSQL 18 cluster, official ONE downloads and codebooks, archived ONE unified file, reconstruction SQL, source manifests and validation queries.
- Missing high-value lanes: a maintained detailed P45 field-of-study label dictionary and a live ENCFT microdata warehouse.
- Rejected or lower-confidence candidates: `PHOGAR` as a national household key; unpublished joins based only on geography and interview date; drafts with hard-coded values.

## Sources

| Source | Type | Locator | Tool | Permission Status | Last Checked | Supports | Gaps Or Caveats | Automation Eligible | Update Boundary |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Recovered PostgreSQL cluster | Live database | `localhost:5433` | PostgreSQL CLI | Available locally | 2026-07-17 | Censuses, ENHOGAR, catalogs, analytical views | Requires local service and user authentication | Yes | May refresh documented counts and table availability after passing validation |
| XCNPV 2022 unified database | Official microdata | `D:\datos_one_censos\2022\BD_FINAL_VIVIENDA_HOGAR_PERSONA_XCNPV_PUB.csv` | Local file | Recovered from Internet Archive | 2026-07-17 | Household-person relationships and housing attributes | Current ONE download returns 404; derived `hogar_id` depends on preserved row order | Yes | Report source/hash changes; do not replace automatically |
| ONE data portal | Official catalog | `https://www.one.gob.do/datos-y-estadisticas/` | Web | Public | 2026-07-17 | Download inventory, dates and codebooks | Some large-file links are broken | Yes | May update availability notes; do not download or replace microdata silently |
| Reconstruction and analytical SQL | Transformation code | `research/mercado-laboral-dominicano/postgresql-recovery/` | Local repository | Available | 2026-07-17 | Grains, joins, derivations, QA and restore process | Must stay synchronized with live objects | Yes | May draft changes; validate before updating semantic rules |
| XCNPV persons codebook | Official documentation | `https://www.one.gob.do/catalogo-datos/CENSO_POBLACION_VIVIENDA/BD_XCNPV/Libro_de_c%C3%B3digos_de_base_de_datos_de_Persona_XCNPV.htm` | Web | Public | 2026-07-17 | P28 relationship, education, labor and marital-status meanings | P45 only labels nonresponse codes, not every field | Yes | May update labels when official documentation changes |
