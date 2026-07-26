# Evidence Register

| Fact Or Claim | Source Type | Source Link Or Path | Observed | Confidence | Notes |
| --- | --- | --- | --- | --- | --- |
| XCNPV 2022 has 10,773,983 person rows | Live count and loader assertion | `censo_2022.analitica.xcnpv_unificada` | 2026-07-17 | High | Also matches the recovered persons table |
| P28=1 is household head and P28=2 is spouse/partner | Official codebook | ONE persons codebook URL in source inventory | 2026-07-17 | High | Exact official labels reviewed |
| P63 is marital status and P66 is surviving children | Official codebook | ONE persons codebook URL in source inventory | 2026-07-17 | High | Corrects the older 2022 harmonization |
| `PHOGAR` alone is non-unique | Live profile | `censo_2022.public.personas_limpia` | 2026-07-17 | High | Only 15 values across 10.8 million rows |
| Unified file preserves co-residence blocks ordered by P25 | Official file plus hard validation | `load-unificada-xcnpv.sql` | 2026-07-17 | High after validation | Loader verifies every block start and explicitly checks the single preserved source anomaly |
| The unified file has one duplicated within-block order value | Official file plus hard validation | Source row 8,141,679 in `analitica.xcnpv_unificada` | 2026-07-17 | High | It repeats `P25_ORDEN=2` in block 2,843,047; person identity uses `fila_origen` instead |
| 2022 has 1,735,238 identifiable head-partner couples | Live validated count | `censo_2022.analitica.parejas_jefatura_2022` | 2026-07-17 | High | Exactly one head and one declared spouse/partner per household |
| 2010 has 1,443,521 identifiable head-partner couples | Live validated count | `censo_2010.analitica.parejas_jefatura_historica` | 2026-07-17 | High | Uses the published composite household key |
| 2002 partner links are unavailable in the loaded public CSV | Schema and harmonization review | `censo_2002.armonizado.personas` | 2026-07-17 | High | Household, dwelling and person IDs are absent; no links are fabricated |
| Cluster has no detected physical corruption | `pg_amcheck` | Local PostgreSQL 18 cluster on port 5433 | 2026-07-17 | High | 1,973 relations and 1,832,686 pages checked; exit 0 |
| Page checksums and durability settings are enabled | PostgreSQL control and settings | Live cluster | 2026-07-17 | High | checksums, fsync, full_page_writes and synchronous_commit enabled |
