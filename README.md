# Prueba Técnica COBOL – Modernización Tecnológica y Migración de Clientes Bancarios

## 1. Descripción general

Este proyecto implementa un proceso batch en COBOL para la migración de clientes y productos financieros desde archivos legados hacia una estructura estándar, simulando un escenario de modernización tecnológica en un entorno bancario.

La solución responde al reto técnico **“Modernización Tecnológica y Migración de Clientes Bancarios”**, cuyo objetivo es validar, consolidar y transformar información de clientes y productos financieros para generar archivos listos para plataformas modernas.

## 2. Objetivo de la solución

El proceso permite:

- Leer archivos de entrada de clientes, cuentas y CDT.
- Validar reglas de negocio.
- Consolidar clientes con sus productos financieros.
- Generar salidas de:
  - registros migrables,
  - registros rechazados,
  - reporte de ejecución.

 ## 3. Tecnologías utilizadas

**GnuCOBOL** (compilación y ejecución batch)
**Archivos planos '.txt'**
**SYNCSORT (simulado en JCL)** para ordenamiento de datasets
**COBOL batch (migracion.cbl y estructura.cbl)**
**JCL documental (simulación de entorno mainframe)**
**VS Code** como entorno de desarrollo

## 4. Estructura del proyecto

├─ data/
│ ├─ clientes.txt
│ ├─ cuentas.txt
│ └─ cdt.txt
│
├─ output/
│ ├─ clioutput.txt
│ ├─ cuenoutput.txt
│ ├─ cdtoutput.txt
│ ├─ clientesort.txt
│ ├─ cuentasort.txt
│ ├─ cdtsort.txt
│ ├─ CLIENTES_MIGRADOS.txt
│ ├─ CLIENTES_RECHAZADOS.txt
│ └─ REPORTE_MIGRACION.txt
│
├─ migracion.cbl
├─ estructura.cbl
├─ migrajob.jcl
└─ README.md

## 5. Archivos de entrada
-clientes.txt
-cuentas.txt
-cdt.txt

## 6. Proceso de transformación de datos
### 6.1 Programa 'estructura.cbl' (preparación)
Este programa:
- Lee archivos originales.
- Asigna formato fijo a cada registro.
- Genera archivos intermedios:
  - clioutput.txt
  - cuenoutput.txt
  - cdtoutput.txt
  
### 6.2 Ordenamiento (JCL-SYNCSORT)
Los archivos estructurados son ordenados por llave de negocio (NUMDOC cliente), generando:
- clientesort.txt
- cuentasort.txt
- cdtsort.txt
Estos archivos ordenados son la **entrada del proceso COBOL principal**.

### 6.3 Programa 'migracion' (validacion y transformacion de datos)
procesa registros, aplicando reglas de negocio, formatenado archivos para integrar migracion final. 

## 7. Archivos de salida

### 7.1 CLIENTES_MIGRADOS.txt
Registros válidos consolidados 
Clientes con productos ccumpliendo las reglas de negocio.


### 7.2 CLIENTES_RECHAZADOS.txt
Registros rechazados con motivo
Clientes sin productos asociados, datos incompletos acorde a reglas de negocio.

### 7.3 REPORTE_MIGRACION.txt
- Clientes procesados
- Clientes migrados
- Productos migrados
- Registros rechazados
- Porcentaje de éxito


  ## 8. Reglas de negocio

### Clientes
- Documento obligatorio
- No duplicados
- Nombre obligatorio
- Email válido

### Cuentas
- Cliente existente
- Saldo > 0
- Tipo de producto válido
- Sin duplicados

### CDT
- Cliente existente
- Monto > 0
- Fecha vencimiento > apertura

## 9. Flujo del proceso batch

DATA → ESTRUCTURA → SYNCSORT → COBOL BATCH (migracion) → OUTPUT

## 10. Compilación y ejecución

```bash
cobc -x -free estructura.cbl
./estructura

ejecucion de JCL (ordanamiento)

cobc -x -free migracion.cbl
./migracion

## 11. Consideraciones

- Diseño tipo mainframe real
- Separación por etapas
- Ordenamiento previo obligatorio
- Datos consistentes antes del COBOL principal

## 12. Entregables

- estructura.cbl
- migracion.cbl
- migrajob.jcl
- data/*.txt
- output/*.txt
- README.md

## 13. Autor

**Eder Alberto Niño Mora**

