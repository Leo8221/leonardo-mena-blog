# ENHOGAR 2022

Microdatos públicos descargados desde el portal oficial de la Oficina Nacional de Estadística (ONE) el 25 de julio de 2026.

## Archivos

- Personas_ENH22.csv: personas, parentesco, educación, unión, trabajo, identificadores de hogar y factores de expansión.
- Elegidos_ENH22.csv: módulo de personas elegidas.
- Libro_de_codigos_ENHOGAR2022_Personas.htm: diccionario oficial de personas.
- Libro_de_codigos_ENHOGAR2022_Elegidos.htm: diccionario oficial de personas elegidas.

## Uso en homogamia educativa

La base de personas permite reconstruir parejas convivientes cuando existe exactamente una persona jefa (P205 = 1) y una esposa o compañera/o (P205 = 2) dentro de UPM + HVIVIEN + HHOGAR.

El análisis usa F_expansión para estimar proporciones ponderadas. No se restringe a parejas donde ambas personas trabajan. La depuración principal se hace por comparación observado/esperado y por ajuste de edad, composición sexual, unión y región.

## Fuente

<https://www.one.gob.do/datos-y-estadisticas/>

Descargas directas:

- <https://www.one.gob.do/catalogo-datos/ENHOGAR/ENHOGAR_2022_BD_SPSS/Personas_ENH22.csv>
- <https://www.one.gob.do/catalogo-datos/ENHOGAR/ENHOGAR_2022_BD_SPSS/Elegidos_ENH22.csv>

Los hashes SHA-256 de los archivos descargados quedan registrados en la bitácora de trabajo y deben conservarse al regenerar resultados.
