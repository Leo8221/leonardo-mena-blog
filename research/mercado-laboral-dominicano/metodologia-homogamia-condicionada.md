# Homogamia condicionada: método y controles

## Pregunta y universo

La figura 12 estima cuánto coinciden las parejas en seis rasgos más de lo que se esperaría bajo emparejamiento independiente. La unidad es una pareja formada por la persona declarada como jefatura y la persona declarada como cónyuge o compañero/a en el mismo hogar.

El universo canónico del Censo 2022 contiene 1.731.814 parejas con ambos miembros de 16 años o más y una diferencia de edad absoluta no mayor de 45 años. Esta definición no representa a todas las relaciones de pareja: excluye parejas que no conviven, hogares sin una jefatura y un cónyuge identificables y uniones no declaradas mediante esos parentescos.

## Estimador

Para cada rasgo se calcula la proporción observada de parejas en la misma categoría, `P_o`. El esperado nacional bajo independencia es:

`P_e = Σ_c P(jefatura = c) × P(cónyuge = c)`.

El contrafactual condicionado repite ese cálculo dentro de cada estrato `s` y luego pondera los esperados por el tamaño del estrato:

`P_e|S = Σ_s (N_s / N) × Σ_c P(jefatura = c | s) × P(cónyuge = c | s)`.

La medida publicada es el kappa de Cohen:

`κ = (P_o - P_e) / (1 - P_e)`.

Un valor de cero indica que la coincidencia no supera la esperada por los márgenes usados; un valor positivo indica coincidencia adicional. No se usan p-valores para el Censo: se trabaja con el universo censal definido y, con más de 1,7 millones de parejas, pruebas convencionales convertirían diferencias triviales en “significativas”. Se priorizan magnitud, sensibilidad y replicación.

## Estratos y sensibilidad

Cuando la edad es el resultado, el contrafactual conserva territorio, composición por sexo de la pareja y combinación de estados conyugales. No se condiciona por edad porque eso predeterminaría el resultado.

Para educación, autoidentificación, dificultad funcional, categoría ocupacional y campo de estudio se conservan además las bandas de edad de diez años de ambos miembros.

La figura muestra dos granularidades territoriales:

- región, como sensibilidad más parsimoniosa;
- provincia, como especificación principal.

La corta línea azul es el rango entre ambas. La estabilidad de ese rango permite distinguir resultados robustos de los que dependen de una geografía particular.

## Cobertura y categorías

Cada rasgo usa solamente parejas con dato válido en ambos miembros. Edad tiene cobertura completa; educación 88,8%; autoidentificación 98,3%; dificultad funcional 100,0%; categoría ocupacional 55,3%; y campo de estudio 10,0%.

Campo de estudio y categoría ocupacional se muestran como subuniversos y no como resultados directamente generalizables a todas las parejas. En la especificación provincial, 3,29% de los casos válidos de campo de estudio queda en estratos con menos de diez parejas; para los otros rasgos el máximo es 0,59%.

## Replicación

### Censo 2010

La réplica usa 1.429.898 parejas plausibles y la misma lógica de enlaces y estratos. Para comparar educación, 2010 y 2022 se armonizan a cuatro niveles: preprimaria, primaria, secundaria y superior. Universidad, maestría y doctorado de 2022 se agrupan como superior.

### ENHOGAR 2024

La validación externa usa hogares completos con exactamente una jefatura y un cónyuge, 4.872 parejas plausibles y el factor de expansión final de la encuesta. Para edad se condiciona por región, composición por sexo y unión. Para educación se conservan composición por sexo, unión y edades de ambos, pero se omite región para evitar fragmentar una muestra de 4.405 parejas con educación válida. Con esos estratos reducidos, solo 7,45% de los casos queda en celdas menores de diez.

La codificación se verificó contra el [diccionario oficial de personas de ENHOGAR 2024](https://www.one.gob.do/catalogo-datos/ENHOGAR/ENHOGAR_2024_BD_PUB/Libro_de_c%C3%B3digo_ENHOGAR_2024_Personas.htm): P205 identifica parentesco, P208 el estado conyugal actual, P203 la edad, P303 el nivel educativo y `FEXPANSION` el factor final. P209 no se usa como estado conyugal porque pregunta si la persona alguna vez estuvo casada o unida.

Los intervalos de 95% se obtienen con 399 réplicas Rao–Wu: dentro de cada uno de los 21 estratos de diseño se remuestrean `m-1` UPM con reemplazo y se reescala por `m/(m-1)`. Las 399 réplicas convergieron para ambos rasgos. ENHOGAR valida dirección y orden de magnitud; no es una réplica idéntica del Censo.

## Resultados de robustez

| Rasgo | κ nacional 2022 | κ condicionado por provincia |
|---|---:|---:|
| Grupo de edad | 0,354 | 0,346 |
| Nivel educativo | 0,353 | 0,284 |
| Autoidentificación etnorracial | 0,296 | 0,276 |
| Dificultad funcional | 0,342 | 0,250 |
| Categoría ocupacional | 0,192 | 0,197 |
| Campo de estudio superior | 0,135 | 0,119 |

La coincidencia por edad casi no cambia. Parte importante de la coincidencia en educación y dificultad funcional sí se explica por estructura etaria y territorial, pero ambas permanecen positivas. Categoría ocupacional es estable alrededor de 0,20. Campo de estudio conserva una señal pequeña y su baja cobertura exige cautela.

En la comparación externa, la edad pasa de κ condicionado 0,305 en 2010 a 0,346 en 2022; ENHOGAR 2024 estima 0,364, con intervalo 0,337–0,386. Educación armonizada es 0,285 en 2010, 0,292 en 2022 y 0,298 en ENHOGAR 2024, con intervalo 0,262–0,317.

## Límites de interpretación

- El análisis es transversal y descriptivo: no separa selección inicial de convergencia durante la convivencia.
- Jefatura y cónyuge son roles censales asimétricos, no dos muestras intercambiables de toda la población.
- Condicionar reduce confusión observable, pero no elimina variables omitidas como duración de la unión, origen social, religión o institución educativa, que no están disponibles de forma comparable.
- Los resultados describen parejas corresidentes identificables, no el mercado completo de relaciones.
- La advertencia activa de calidad en PostgreSQL corresponde a una fila oficial con `P25_ORDEN` repetido; la identidad analítica usa `fila_origen`, por lo que no altera los enlaces de pareja ni los conteos publicados.

## Reproducción

Desde la raíz del repositorio:

```powershell
$env:PG_RECOVERY_PASSWORD = '<clave>'
powershell -ExecutionPolicy Bypass -File research/mercado-laboral-dominicano/render-grafico-homogamia-condicionada.ps1
Remove-Item Env:PG_RECOVERY_PASSWORD
```

Los resultados exactos quedan en `data/procesados/12_homogamia_condicionada.csv` y `data/procesados/12_homogamia_validacion.csv`; los controles quedan en `qa-homogamia-condicionada.csv`.
