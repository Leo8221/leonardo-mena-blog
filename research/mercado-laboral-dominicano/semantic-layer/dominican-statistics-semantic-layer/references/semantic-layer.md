# Dominican Statistics Semantic Layer

## Quick Reference

- Area: Dominican Republic population censuses and household surveys.
- Intended users: research, journalism, Quarto articles and reproducible exploratory analysis.
- Coverage level: strong for 2002, 2010, 2022 censuses and ENHOGAR 2024; relationship links are available only for 2010 and 2022.
- Source inventory: `references/source-inventory.md`.
- Last synthesized: 2026-07-17.
- Freshness expectations: census microdata are static; availability, codebooks and survey additions should be checked before new projects.
- Default time zone: `America/Santo_Domingo` for operational timestamps; census reference years are survey periods, not event timestamps.

## Entity Clarification

| Entity | Means | Does Not Mean | Primary IDs | Grain Notes | Sources |
| --- | --- | --- | --- | --- | --- |
| XCNPV 2022 person | One censused person | A survey-weighted observation | `fila_origen` or derived `id_persona` | 10,773,983 rows; one source row duplicates a within-block P25 order | `censo_2022.analitica.xcnpv_unificada` |
| XCNPV 2022 co-residence block | A contiguous roster reconstructed from the official unified file | The raw `PHOGAR` value | `hogar_id` | 3,736,217 blocks: 3,726,936 private households plus 9,281 collective blocks | Unified loader and QA |
| Identifiable couple | One head plus exactly one declared spouse/partner in the same household | Every romantic partnership or every married person | `hogar_id` | 1,735,238 rows in 2022; 1,443,521 in 2010 | Pair tables and QA |
| Field of study | ISCED-F training/career code from P45 | Current occupation or industry | `p45_code` | Applicable mainly to people with higher education | XCNPV codebook and analytical functions |
| Occupation | Main current or last occupation from P60 | Degree field or employer industry | `p60_code` | CNO hierarchy; nonresponse codes 9998/9999 | CNO dictionary |
| Industry | Activity of employer/business from P62 | Person's occupation | `p62_code` | CNAE hierarchy | CNAE dictionary |
| ENHOGAR person | One sampled survey person | One person in the national population without weighting | Household keys plus person line | Use expansion factors for estimates | `enhogar_2024.public.personas_analiticas` |

## Key Metrics

| Metric | Definition | Numerator | Denominator | Time Grain | Canonical Source | Caveats |
| --- | --- | --- | --- | --- | --- | --- |
| Identifiable co-resident couples | Households with exactly one head and one declared spouse/partner | Rows in pair table | None | 2010, 2022 | `analitica.parejas_jefatura_2022`, central historical view | Excludes non-co-resident and secondary couples; 2002 has no link key |
| Same broad field of study | Both partners share broad ISCED-F field | `mismo_campo_estudio_amplio=true` | Non-null same-field flag | 2022 | Pair table | Always report field coverage |
| Same major occupation group | Both partners share major CNO group | `mismo_gran_grupo_ocupacional=true` | Non-null occupation flag | 2022 | Pair table | P60 may refer to last occupation |
| Census unemployment rate | Non-employed people seeking work divided by census labor force | Derived unemployed | Employed + derived unemployed | 2022 | `public.vw_mercado_laboral_censo` | Structural census measure, not official ENCFT rate |
| ENHOGAR population estimate | Weighted sum using official expansion factor | Sum of factor for qualifying records | Depends on metric | 2024 | Analytical ENHOGAR views | Do not present raw sample rows as population totals |

## Standard Filters And Dimensions

| Filter Or Dimension | Default Logic | Override When | Applies To | Sources |
| --- | --- | --- | --- | --- |
| Valid age | Treat 999 as missing | Use original code only for QA | XCNPV people and couples | Persons codebook |
| Valid field of study | Broad field is non-null; exclude 9998/9999 | Codebook-specific nonresponse analysis | Education/profession analyses | P45 and ISCED-F mapping |
| Valid occupation | Exclude 9998/9999 and require mapped CNO group | Nonresponse analysis | Occupation analyses | P60 and CNO |
| Couple plausibility | Use `edades_plausibles=true` for substantive age comparisons | Data-quality reporting | Couples | Pair view |
| Territory | Prefer official numeric codes | Labels are required for presentation | All sources | ONE catalogs |
| Survey estimate | Use the documented expansion factor | Pure sample-quality analysis | ENHOGAR | ENHOGAR metadata |

## Key Tables

| Table | When To Use | Grain | Join Keys | Freshness | Caveats | Sources |
| --- | --- | --- | --- | --- | --- | --- |
| `censo_2022.analitica.xcnpv_unificada` | Detailed 2022 person-household analysis | Person | `fila_origen`; group with `hogar_id` | Static 2022 | Synthetic block sequence tied to exact official file; P25 is not globally unique | Unified loader |
| `censo_2022.analitica.personas_relaciones_2022` | Readable labels and common derived attributes | Person | `hogar_id`, `id_persona` | Static 2022 | Convenience view; verify codebook for unusual codes | Analytical SQL |
| `censo_2022.analitica.parejas_jefatura_2022` | Couple characteristics, assortative mating and profession pairing | Couple | `hogar_id` | Static 2022 | Only head-partner couples | Analytical SQL |
| `censo_2022.analitica.parejas_profesiones_2022` | “Who partners with whom” from either person's perspective | Person within pair | `hogar_id`, role | Static 2022 | Two rows per couple | Analytical SQL |
| `censos_linea_tiempo.analitica.parejas_historicas` | Compare head-partner characteristics over time | Couple | `anio`, `id_hogar` | Static 2010/2022 | No 2002 links because its public person file lacks IDs | Central FDW |
| `censos_linea_tiempo.blog.personas_todos` | Cross-census demographic and education aggregates | Person | `anio`, source-specific IDs | Static censuses | Check comparability metadata before trends | Central FDW |
| `censos_linea_tiempo.catalogo.fuentes` | Source selection and provenance | Source | `fuente_id` | Refresh on source changes | Not microdata | Central catalog |
| `enhogar_2024.public.personas_analiticas` | Weighted survey analysis | Sampled person | Survey household/person keys | 2024 | Expansion factor required | ENHOGAR loader |

## Query Patterns

- Couple characteristics:
  - Use `analitica.parejas_jefatura_2022`.
  - Filter `edades_plausibles` when interpreting age gaps.
  - Segment by sex composition, territory, education, household assets or employment.
- Field-of-study pairing:
  - Use `matriz_campos_estudio_parejas_2022` for broad fields.
  - Use `parejas_profesiones_2022` when detailed codes or custom filters are needed.
  - Report coverage where both fields are observed.
- Occupation pairing:
  - Use `matriz_ocupaciones_parejas_2022` for major groups.
  - Exclude 9998/9999 and label P60 as current or last occupation.
- Cross-census trends:
  - Start from `censos_linea_tiempo.blog.*` views.
  - Read `blog.comparabilidad` and keep census year in every grouping.
- Survey estimates:
  - Use ENHOGAR analytical views and weighted sums.
  - Keep sample row counts separate from estimated totals.

## Gotchas

- `PHOGAR` is not a national household key in the public XCNPV 2022 CSV. Using it alone creates false joins across millions of households.
- XCNPV 2022 state of marriage/union is P63. P66 is surviving children; older transformation code incorrectly treated P66 as marital status.
- CNO codes must match their published textual level. Padding every P60 code to four digits and forcing level 4 loses valid mappings such as code 815.
- A field of study is not an occupation. A lawyer by education can work in another occupation or industry.
- Census labor indicators are not substitutes for official ENCFT quarterly rates.
- Cross-year occupation and education codes are not automatically comparable; use broad groups and explicit comparability notes.
- The pair table describes co-resident head-partner links and should not be generalized to all partnerships.
- The loaded 2002 public person CSV has no household/person identifiers. Use it for individual or aggregate analysis, never for partner links.

## Related Docs

| Source | Use It For | Caveats |
| --- | --- | --- |
| `research/mercado-laboral-dominicano/postgresql-recovery/README.md` | Connection, loading, backup and recovery details | Local repository path |
| ONE persons codebook | Official P-variable labels and values | Large-file download links may be stale |
| `censos_linea_tiempo.catalogo.metricas` | Canonical metric definitions | Requires live local PostgreSQL |

## Open Questions

- Detailed human-readable labels for every P45 field code should be sourced from a maintained official ISCED-F mapping.
- Future ENHOGAR years should be loaded into separate year-specific schemas or databases and harmonized only after their weights and questionnaires are reviewed.
- A future ENCFT layer should retain quarter, weights and official labor-status definitions rather than reusing census derivations.
