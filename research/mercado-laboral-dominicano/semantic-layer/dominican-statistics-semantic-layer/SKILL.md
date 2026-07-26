---
name: dominican-statistics-semantic-layer
description: Use when answering data questions with the recovered Dominican Republic censuses and household surveys, including source choice, grains, joins, labor metrics, household relationships, couples, education, occupations, weights, and known caveats.
---

# Dominican Statistics Semantic Layer

Use this skill for analyses based on the recovered PostgreSQL censuses and household surveys.

## Start Here

1. Read `references/semantic-layer.md`.
2. Select the canonical source for the requested unit of analysis.
3. Preserve grains, weights, filters, code meanings and comparability caveats.
4. Check `meta.controles_calidad_analitica` before relationship or couple analysis.
5. Verify time-sensitive availability against the source inventory.

## References

- `references/semantic-layer.md`: tables, metrics, joins, filters, query patterns and gotchas.
- `references/source-inventory.md`: sources checked, coverage and update boundaries.
- `references/evidence.md`: provenance for the key rules.

## Answering Rules

- Treat this layer as source-selection guidance, not as a substitute for live reads.
- Never use `PHOGAR` alone to join XCNPV 2022 people.
- Distinguish field of study, occupation and industry; do not call all three “profession”.
- Distinguish census counts from survey-weighted estimates and official ENCFT labor rates.
- Label inferred, partial, non-comparable or coverage-limited results.
- Show source SQL only when the user explicitly asks for SQL or methodology.

