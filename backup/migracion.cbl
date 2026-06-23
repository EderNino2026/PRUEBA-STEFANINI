       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIGRACION.
       AUTHOR. EDER NINO MORA.
       DATE-WRITTEN. 22/06/2026.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT CLIENTES-INPUT ASSIGN TO "output/clientesort.txt"
       ORGANIZATION IS LINE SEQUENTIAL
       FILE STATUS IS ESTADO-01.

       SELECT CUENTAS-INPUT ASSIGN TO "output/cuentasort.txt"
       ORGANIZATION IS LINE SEQUENTIAL
       FILE STATUS IS ESTADO-02.

       SELECT CDT-INPUT ASSIGN TO "output/cdtsort.txt"
       ORGANIZATION IS LINE SEQUENTIAL
       FILE STATUS IS ESTADO-03.

       SELECT MIGRADOS-OUTPUT ASSIGN TO "output/CLIENTES_MIGRADOS.txt"
       ORGANIZATION IS LINE SEQUENTIAL
       FILE STATUS IS ESTADO-04.

       SELECT RECHAZO-OUTPUT ASSIGN TO "output/CLIENTES_RECHAZADOS.txt"
       ORGANIZATION IS LINE SEQUENTIAL
       FILE STATUS IS ESTADO-05.

       SELECT REPORTE-OUTPUT ASSIGN TO "output/REPORTE_MIGRACION.txt"
       ORGANIZATION IS LINE SEQUENTIAL
       FILE STATUS IS ESTADO-06.

       DATA DIVISION.
       FILE SECTION.

       FD CLIENTES-INPUT.
       01 CLIENTE-REG.
           05 CLI-TIPODOC   PIC X(3).
           05 CLI-NUMDOC    PIC X(15).
           05 CLI-NOMBRE    PIC X(50).
           05 CLI-EMAIL     PIC X(50).

       FD CUENTAS-INPUT.
       01 CUENTAS-REG.
           05 CUEN-NUMPRODUCTO   PIC X(5).
           05 CUEN-TIPOPRODUCTO  PIC X(3).
           05 CUEN-SALDO         PIC X(20).
           05 CUEN-CLIENTE       PIC X(15).

       FD CDT-INPUT.
       01 CDT-REG.
           05 CDT-NUMCDT       PIC X(5).
           05 CDT-MONTO        PIC X(20).
           05 CDT-FECHA-APE    PIC X(8).
           05 CDT-FECHA-VEC    PIC X(8).
           05 CDT-CLIENTE-CDT  PIC X(15).

       FD MIGRADOS-OUTPUT.
       01 MIGRADOS-REG PIC X(200).

       FD RECHAZO-OUTPUT.
       01 RECHAZO-REG PIC X(300).

       FD REPORTE-OUTPUT.
       01 REPORTE-REG PIC X(200).

       WORKING-STORAGE SECTION.

       01 SALIDA-RECHAZO.
           05 SAL1-REG-RECH     PIC X(200).
           05 SAL1-MOTIVO       PIC X(50).
           05 SAL1-ORIGEN       PIC X(50).

       01 SALIDA-MIGRADOS.
           05 SAL2-CLIENTE      PIC X(15).
           05 SAL2-PRODUCTO     PIC X(5).
           05 SAL2-TIPO         PIC X(9).
           05 SAL2-VALOR        PIC X(20).

       01 SALIDA-REPORTE.
           05 SAL3-CLI-PROCESADOS   PIC 9(5).
           05 SAL3-CLI-MIGRADOS     PIC 9(5).
           05 SAL3-PRO-MIGRADOS     PIC 9(5).
           05 SAL3-RECHAZADOS       PIC 9(5).
           05 SAL3-PORC-EXITO       PIC 9(3)V9(2).
           05 SAL3-PORC-EDIT        PIC ZZ9.99.
           05 SAL3-PORC             PIC x(7).

      *---------------------------------------------------------------*
      * VARIABLES DE TRABAJO                     *
      *---------------------------------------------------------------*
       01 WS-VARIABLES.
           05 WS-PRODUCTO PIC X(5).
           05 WS-TIPO PIC X(9).
           05 WS-VALOR PIC X(20).
           05 WS-CUEN-ACTUAL.
               10 ACT-CLI-CUEN PIC X(15).
               10 ACT-NPROD-CUEN PIC X(5).
               10 ACT-TPROD-CUEN PIC X(3).
           05 WS-CUEN-ANT.
               10 ANT-CLI-CUEN PIC X(15).
               10 ANT-NPROD-CUEN PIC X(5).
               10 ANT-TPROD-CUEN PIC X(3).
           05 WS-CDT-ACTUAL.
               10 ACT-CLIENTE-CDT PIC X(15).
               10 ACT-NUM-CDT PIC X(5).
           05 WS-CDT-ANT.
              10 ANT-CLIENTE-CDT PIC X(15).
              10 ANT-NUM-CDT PIC X(5).
           05 WS-SALDO-CUEN PIC X(20).
           05 WS-SALDO-NUM-CUEN REDEFINES WS-SALDO-CUEN PIC 9(20).
           05 WS-SALDO-CDT PIC X(20).
           05 WS-SALDO-NUM-CDT REDEFINES WS-SALDO-CDT PIC 9(20).
      *---------------------------------------------------------------*
      * VARIABLES CONTROL DE DUPLICADOS                               *
      *---------------------------------------------------------------*
       01 WS-CONTROL-DUPLICADOS.
           05 WS-CLIENTE-ACTUAL-REG.
               10 WS-CLI-TIPODOC-ACT   PIC X(3).
               10 WS-CLI-NUMDOC-ACT    PIC X(15).
               10 WS-CLI-NOMBRE-ACT    PIC X(50).
               10 WS-CLI-EMAIL-ACT     PIC X(50).
           05 SW-DUPLICADO         PIC X(1) VALUE 'N'.
               88 CLI-ES-DUPLICADO VALUE 'Y'.
               88 CLI-NO-DUPLICADO VALUE 'N'.

       01 WS-CABECERA-MIGRADOS.
           05 FILLER PIC X(200) VALUE 'CLIENTE|PRODUCTO|TIPO|VALOR'.   

      *---------------------------------------------------------------*
      * SWITCHES                         *
      *---------------------------------------------------------------*
       01 WS-SWITCHES.
           05 SW-CLIENTES PIC X(1)    VALUE 'N'.
               88 SI-FIN-CLIENTES     VALUE 'Y'.
               88 NO-FIN-CLIENTES     VALUE 'N'.
           05 SW-CUENTAS PIC X(1)     VALUE 'N'.
               88 SI-FIN-CUENTAS      VALUE 'Y'.
               88 NO-FIN-CUENTAS      VALUE 'N'.
           05 SW-CDT PIC X(1)         VALUE 'N'.
               88 SI-FIN-CDT          VALUE 'Y'.
               88 NO-FIN-CDT          VALUE 'N'.
           05 SW-ESTADO PIC X(1)      VALUE 'N'.
               88 SI-CORRECTO         VALUE 'Y'.
               88 NO-CORRECTO         VALUE 'N'.
           05 SW-CDT-ASO PIC X(1)    VALUE 'N'.
               88 SI-CDT              VALUE 'Y'.
               88 NO-CDT              VALUE 'N'.
           05 SW-CUENTAS-ASO PIC X(1) VALUE 'N'.
               88 SI-CUENTAS          VALUE 'Y'.
               88 NO-CUENTAS          VALUE 'N'.
           05 SW-RECHAZO PIC X(1)     VALUE 'N'.
               88 SI-RECHAZADO        VALUE 'Y'.
               88 NO-RECHAZADO        VALUE 'N'.
           05 SW-PRODUCTOS PIC X(3)   VALUE SPACES.
               88 PROD-VALIDOS        VALUE 'AHO' 'CTE'.

      *---------------------------------------------------------------*
      * ESTADO DE ARCHIVOS                        *
      *---------------------------------------------------------------*
       77 ESTADO-01 PIC X(2) VALUE '00'.
       77 ESTADO-02 PIC X(2) VALUE '00'.
       77 ESTADO-03 PIC X(2) VALUE '00'.
       77 ESTADO-04 PIC X(2) VALUE '00'.
       77 ESTADO-05 PIC X(2) VALUE '00'.
       77 ESTADO-06 PIC X(2) VALUE '00'.

      *---------------------------------------------------------------*
      * CONTADORES                         *
      *---------------------------------------------------------------*
       77 WS-LEIDOS-CLIENTES   PIC 9(5) VALUE ZEROS.
       77 WS-LEIDOS-CUENTAS    PIC 9(5) VALUE ZEROS.
       77 WS-LEIDOS-CDT        PIC 9(5) VALUE ZEROS.
       77 WS-GRABADOS-MIGRADOS PIC 9(5) VALUE ZEROS.
       77 WS-GRABADOS-RECHAZOS PIC 9(5) VALUE ZEROS.
       77 WS-GRABADOS-REPORTE  PIC 9(5) VALUE ZEROS.
       77 WS-CLI-MIGRADOS      PIC 9(5) VALUE ZEROS.
       77 WS-PROD-MIGRADOS     PIC 9(5) VALUE ZEROS.
       77 WS-ARROBA-COUNT      PIC 9(2) VALUE ZEROS.

      *---------------------------------------------------------------*
      * CONSTANTES                         *
      *---------------------------------------------------------------*
       77 WSC-AHORROS         PIC X(9)  VALUE 'AHORROS'.
       77 WSC-CTE             PIC X(9)  VALUE 'CORRIENTE'.
       77 WSC-CDT             PIC X(3)  VALUE 'CDT'.

       PROCEDURE DIVISION.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO UNTIL SI-FIN-CLIENTES 
           PERFORM 2800-LEER-RESTO 
           PERFORM 3000-FINAL.

      *---------------------------------------------------------------*
      * 1000-INICIO                              *
      *---------------------------------------------------------------*
       1000-INICIO.
           INITIALIZE WS-VARIABLES
           SET SI-CORRECTO TO TRUE
           PERFORM 1100-ABRIR-ARCHIVOS
           PERFORM 1300-LEER-ARCHIVOS
           PERFORM 1800-GRABAR-CABECERA-MIGRADOS
           .

      *---------------------------------------------------------------*
      * 1100-ABRIR-ARCHIVOS                      *
      *---------------------------------------------------------------*
       1100-ABRIR-ARCHIVOS.
           OPEN INPUT CLIENTES-INPUT
           IF ESTADO-01 = '00' OR '97'
               CONTINUE
           ELSE
           DISPLAY 'ERROR ABRIENDO ARCHIVO DE CLIENTES    ' ESTADO-01
           MOVE 12                 TO RETURN-CODE
           PERFORM 3000-FINAL
           END-IF

           OPEN INPUT CUENTAS-INPUT
           IF ESTADO-02 = '00' OR '97'
               CONTINUE
           ELSE
           DISPLAY 'ERROR ABRIENDO ARCHIVO DE CUENTAS    ' ESTADO-02
           MOVE 12                 TO RETURN-CODE
           CLOSE CLIENTES-INPUT
           PERFORM 3000-FINAL
           END-IF

           OPEN INPUT CDT-INPUT
           IF ESTADO-03 = '00' OR '97'
               CONTINUE
           ELSE
           DISPLAY 'ERROR ABRIENDO ARCHIVO DE CDT    ' ESTADO-03
           MOVE 12                 TO RETURN-CODE
           CLOSE CLIENTES-INPUT CUENTAS-INPUT
           PERFORM 3000-FINAL
           END-IF

           OPEN OUTPUT MIGRADOS-OUTPUT
           IF ESTADO-04 = '00' OR '97'
               CONTINUE
           ELSE
           DISPLAY 'ERROR ABRIENDO ARCHIVO DE MIGRADOS    ' ESTADO-04
           MOVE 12                 TO RETURN-CODE
           CLOSE CLIENTES-INPUT CUENTAS-INPUT CDT-INPUT
           PERFORM 3000-FINAL
           END-IF

           OPEN OUTPUT RECHAZO-OUTPUT
           IF ESTADO-05 = '00' OR '97'
               CONTINUE
           ELSE
           DISPLAY 'ERROR ABRIENDO ARCHIVO DE RECHAZOS    ' ESTADO-05
           MOVE 12                 TO RETURN-CODE
           CLOSE CLIENTES-INPUT CUENTAS-INPUT CDT-INPUT MIGRADOS-OUTPUT
           PERFORM 3000-FINAL
           END-IF

           OPEN OUTPUT REPORTE-OUTPUT
           IF ESTADO-06 = '00' OR '97'
               CONTINUE
           ELSE
           DISPLAY 'ERROR ABRIENDO ARCHIVO DE REPORTE    ' ESTADO-06
           MOVE 12                 TO RETURN-CODE
           CLOSE CLIENTES-INPUT CUENTAS-INPUT CDT-INPUT MIGRADOS-OUTPUT
                 RECHAZO-OUTPUT
           PERFORM 3000-FINAL
           END-IF
           .

      *---------------------------------------------------------------*
      *                    1300-LEER-ARCHIVOS                         *
      *---------------------------------------------------------------*
       1300-LEER-ARCHIVOS.
           PERFORM 1310-LEER-CLIENTES
           PERFORM 1320-LEER-CUENTAS
           PERFORM 1330-LEER-CDT
           .

      *---------------------------------------------------------------*
      *                    1310-LEER-CLIENTES                         *
      *---------------------------------------------------------------*
       1310-LEER-CLIENTES.
           READ CLIENTES-INPUT NEXT RECORD
            AT END
               SET SI-FIN-CLIENTES       TO TRUE
               MOVE HIGH-VALUES          TO CLIENTE-REG
            NOT AT END
               ADD  1                    TO WS-LEIDOS-CLIENTES
           END-READ
           .

      *---------------------------------------------------------------*
      *                    1320-LEER-CUENTAS                          *
      *---------------------------------------------------------------*
       1320-LEER-CUENTAS.
           MOVE WS-CUEN-ACTUAL     TO WS-CUEN-ANT
           READ CUENTAS-INPUT NEXT RECORD
            AT END
               SET SI-FIN-CUENTAS        TO TRUE
               MOVE HIGH-VALUES          TO CUENTAS-REG
            NOT AT END
      *     PERFORM 1350-VALIDAR-CUENTAS
                MOVE CUEN-NUMPRODUCTO   TO ACT-NPROD-CUEN
                MOVE CUEN-TIPOPRODUCTO  TO ACT-TPROD-CUEN
                MOVE CUEN-CLIENTE       TO ACT-CLI-CUEN
                ADD  1                  TO WS-LEIDOS-CUENTAS
           END-READ
           .

      *---------------------------------------------------------------*
      *                    1330-LEER-CDT                              *
      *---------------------------------------------------------------*
       1330-LEER-CDT.
           MOVE WS-CDT-ACTUAL     TO WS-CDT-ANT
           READ CDT-INPUT NEXT RECORD
            AT END
               SET SI-FIN-CDT       TO TRUE
               MOVE HIGH-VALUES          TO CDT-REG
            NOT AT END
                MOVE CDT-CLIENTE-CDT   TO ACT-CLIENTE-CDT
                MOVE CDT-NUMCDT        TO ACT-NUM-CDT
                ADD  1                 TO WS-LEIDOS-CDT
           END-READ
           .

      *---------------------------------------------------------------*
      *                    1350-VALIDAR-CUENTAS                       *
      *---------------------------------------------------------------*
       1350-VALIDAR-CUENTAS.
           INITIALIZE WS-SALDO-CUEN
           IF CUEN-SALDO EQUAL SPACES OR LOW-VALUES
               SET NO-CORRECTO TO TRUE
               MOVE  CUEN-CLIENTE                      TO SAL1-REG-RECH
               MOVE 'SALDO NO INFORMADO'               TO SAL1-MOTIVO
               MOVE 'ARCHIVO ENTRADA CUENTAS'          TO SAL1-ORIGEN
               PERFORM 2300-REGISTRAR-RECHAZO-CLI
           ELSE
               MOVE CUEN-SALDO                TO WS-SALDO-CUEN
               IF WS-SALDO-NUM-CUEN <= 0
                   SET NO-CORRECTO TO TRUE
                   MOVE  CUEN-CLIENTE                  TO SAL1-REG-RECH
                   MOVE 'SALDO INVALIDO'               TO SAL1-MOTIVO
                   MOVE 'ARCHIVO ENTRADA CUENTAS'     TO SAL1-ORIGEN
                   PERFORM 2300-REGISTRAR-RECHAZO-CLI
               END-IF
           END-IF
           IF CUEN-CLIENTE EQUAL SPACES OR LOW-VALUES
               SET NO-CORRECTO TO TRUE
               MOVE  CUEN-CLIENTE                     TO SAL1-REG-RECH
               MOVE 'CLIENTE NO INFORMADO'            TO SAL1-MOTIVO
               MOVE 'ARCHIVO ENTRADA CUENTAS'         TO SAL1-ORIGEN
               PERFORM 2300-REGISTRAR-RECHAZO-CLI
           END-IF
           IF CUEN-NUMPRODUCTO EQUAL SPACES OR LOW-VALUES
               SET NO-CORRECTO TO TRUE
               MOVE  CUEN-CLIENTE                      TO SAL1-REG-RECH
               MOVE 'PRODUCTO NO INFORMADO'            TO SAL1-MOTIVO
               MOVE 'ARCHIVO ENTRADA CUENTAS'         TO SAL1-ORIGEN
               PERFORM 2300-REGISTRAR-RECHAZO-CLI
           END-IF
           IF CUEN-TIPOPRODUCTO EQUAL SPACES OR LOW-VALUES
               SET NO-CORRECTO TO TRUE
               MOVE  CUEN-CLIENTE                      TO SAL1-REG-RECH
               MOVE 'TIPO DE PRODUCTO NO INFORMADO'    TO SAL1-MOTIVO
               MOVE 'ARCHIVO ENTRADA CUENTAS'         TO SAL1-ORIGEN
               PERFORM 2300-REGISTRAR-RECHAZO-CLI
           ELSE
               MOVE CUEN-TIPOPRODUCTO                 TO SW-PRODUCTOS
               IF PROD-VALIDOS
                   CONTINUE
               ELSE
                   SET NO-CORRECTO TO TRUE
                   MOVE  CUEN-CLIENTE                 TO SAL1-REG-RECH
                   MOVE 'PRODUCTO INVALIDO'            TO SAL1-MOTIVO
                   MOVE 'ARCHIVO ENTRADA CUENTAS'      TO SAL1-ORIGEN
                   PERFORM 2300-REGISTRAR-RECHAZO-CLI
               END-IF
           END-IF
           .

      *---------------------------------------------------------------*
      *                        1360-VALIDAR-CDT                       *
      *---------------------------------------------------------------*
       1360-VALIDAR-CDT.
           INITIALIZE WS-SALDO-CDT
           IF CDT-NUMCDT EQUAL SPACES OR LOW-VALUES
               SET NO-CORRECTO TO TRUE
               MOVE  CDT-CLIENTE-CDT              TO SAL1-REG-RECH
               MOVE 'CDT NO INFORMADO'            TO SAL1-MOTIVO
               MOVE 'ARCHIVO ENTRADA CDT'         TO SAL1-ORIGEN
               PERFORM 2300-REGISTRAR-RECHAZO-CLI
           END-IF
           IF CDT-FECHA-APE EQUAL SPACES OR LOW-VALUES
               SET NO-CORRECTO TO TRUE
               MOVE  CDT-CLIENTE-CDT                  TO SAL1-REG-RECH
               MOVE 'FECHA APERTURA NO INFORMADA'     TO SAL1-MOTIVO
               MOVE 'ARCHIVO ENTRADA CDT'             TO SAL1-ORIGEN
               PERFORM 2300-REGISTRAR-RECHAZO-CLI
           END-IF
           IF CDT-FECHA-VEC EQUAL SPACES OR LOW-VALUES
               SET NO-CORRECTO TO TRUE
               MOVE  CDT-CLIENTE-CDT                  TO SAL1-REG-RECH
               MOVE 'FECHA VENCIMIENTO NO INFORMADA'  TO SAL1-MOTIVO
               MOVE 'ARCHIVO ENTRADA CDT'             TO SAL1-ORIGEN
               PERFORM 2300-REGISTRAR-RECHAZO-CLI
           END-IF
           IF CDT-MONTO EQUAL SPACES OR LOW-VALUES
               SET NO-CORRECTO TO TRUE
               MOVE  CDT-CLIENTE-CDT               TO SAL1-REG-RECH
               MOVE 'CLIENTE NO INFORMADO'         TO SAL1-MOTIVO
               MOVE 'ARCHIVO ENTRADA CDT'          TO SAL1-ORIGEN
               PERFORM 2300-REGISTRAR-RECHAZO-CLI
           ELSE
              MOVE CDT-MONTO                     TO WS-SALDO-CDT
               IF WS-SALDO-NUM-CDT <= 0
                   SET NO-CORRECTO TO TRUE
                   MOVE  CDT-CLIENTE-CDT          TO SAL1-REG-RECH
                   MOVE 'MONTO INVALIDO'          TO SAL1-MOTIVO
                   MOVE 'ARCHIVO ENTRADA CDT'     TO SAL1-ORIGEN
                   PERFORM 2300-REGISTRAR-RECHAZO-CLI
               END-IF
           END-IF
           IF CDT-CLIENTE-CDT EQUAL SPACES OR LOW-VALUES
               SET NO-CORRECTO TO TRUE
               MOVE  CDT-CLIENTE-CDT              TO SAL1-REG-RECH
               MOVE 'CLIENTE NO INFORMADO'         TO SAL1-MOTIVO
               MOVE 'ARCHIVO ENTRADA CDT'          TO SAL1-ORIGEN
               PERFORM 2300-REGISTRAR-RECHAZO-CLI
           END-IF

           IF CDT-FECHA-VEC < CDT-FECHA-APE
               SET NO-CORRECTO TO TRUE
               MOVE  CDT-CLIENTE-CDT               TO SAL1-REG-RECH
               MOVE 'FECHA DE VENCIMIENTO MENOR A FECHA APERTURA'
                                                   TO SAL1-MOTIVO
               MOVE 'ARCHIVO ENTRADA CDT'          TO SAL1-ORIGEN
               PERFORM 2300-REGISTRAR-RECHAZO-CLI
           END-IF
           .

      *---------------------------------------------------------------*
      *                 1800-GRABAR-CABECERA-MIGRADOS                 *
      *---------------------------------------------------------------*
       1800-GRABAR-CABECERA-MIGRADOS.
           MOVE WS-CABECERA-MIGRADOS TO MIGRADOS-REG
           WRITE MIGRADOS-REG
           .

      *---------------------------------------------------------------*
      *                      2000-PROCESO                             *
      *---------------------------------------------------------------*
       2000-PROCESO.    
      * RESETEAR SWITCHES PARA CADA NUEVO REGISTRO -------------------*
            SET SI-CORRECTO TO TRUE
            SET NO-CUENTAS  TO TRUE
            SET NO-CDT      TO TRUE
            SET NO-RECHAZADO TO TRUE
            SET CLI-NO-DUPLICADO TO TRUE

      * GUARDAR EL REGISTRO ACTUAL PARA EVALUARLO 
           MOVE CLIENTE-REG TO WS-CLIENTE-ACTUAL-REG

      * LEER ADELANTADO PARA DETECTAR DUPLICADOS
           PERFORM 1310-LEER-CLIENTES

      * CICLO PARA SALTAR TODOS LOS REGISTROS CON LA MISMA CEDULA
            PERFORM UNTIL CLI-NUMDOC NOT = WS-CLI-NUMDOC-ACT 
            OR SI-FIN-CLIENTES
               SET CLI-ES-DUPLICADO TO TRUE
               PERFORM 1310-LEER-CLIENTES 
            END-PERFORM

            IF CLI-ES-DUPLICADO    
                SET SI-RECHAZADO TO TRUE  
                MOVE  WS-CLI-NUMDOC-ACT             TO SAL1-REG-RECH
                MOVE 'CLIENTE DUPLICADO'            TO SAL1-MOTIVO
                MOVE 'ARCHIVO ENTRADA CLIENTE'      TO SAL1-ORIGEN
                PERFORM 2300-REGISTRAR-RECHAZO-CLI
                PERFORM 2040-DESCARTAR-PRODUCTOS-DUPLICADO
            ELSE   
                PERFORM 2010-VALIDA-ENTRADA
                IF SI-CORRECTO
                    IF WS-CUEN-ACTUAL = WS-CUEN-ANT
                        MOVE WS-CLI-NUMDOC-ACT          TO SAL1-REG-RECH
                        MOVE 'CUENTA DUPLICADA'         TO SAL1-MOTIVO
                        MOVE 'ARCHIVO ENTRADA CUENTA'   TO SAL1-ORIGEN
                        PERFORM 2300-REGISTRAR-RECHAZO-CLI
                    ELSE
                        PERFORM 2020-CRUCE-CUENTAS
                    END-IF
        
                    IF WS-CDT-ACTUAL = WS-CDT-ANT
                        MOVE  WS-CLI-NUMDOC-ACT         TO SAL1-REG-RECH
                        MOVE 'CDT DUPLICADO'            TO SAL1-MOTIVO
                        MOVE 'ARCHIVO ENTRADA CDT'      TO SAL1-ORIGEN
                        PERFORM 2300-REGISTRAR-RECHAZO-CLI
                    ELSE
                        PERFORM 2030-CRUCE-CDT
                    END-IF
        
                    IF NO-CDT AND NO-CUENTAS
                        DISPLAY  'CLIENTES SIN PRODUCTO ASOCIADO' 
                        MOVE  WS-CLI-NUMDOC-ACT         
                                                        TO SAL1-REG-RECH
                        MOVE 'CLIENTES SIN PRODUCTO ASOCIADO' 
                                                        TO SAL1-MOTIVO
                        MOVE 'ARCHIVO ENTRADA CLIENTES'   
                                                        TO SAL1-ORIGEN
                        PERFORM 2300-REGISTRAR-RECHAZO-CLI
                    ELSE
                        ADD 1                        TO WS-CLI-MIGRADOS
                    END-IF
                END-IF
            END-IF
           .
      *---------------------------------------------------------------*
      *                   2010-VALIDA-ENTRADA                         *
      *---------------------------------------------------------------*
       2010-VALIDA-ENTRADA.
    
           IF WS-CLI-TIPODOC-ACT EQUAL SPACES OR LOW-VALUES
               SET NO-CORRECTO TO TRUE
               MOVE  WS-CLI-NUMDOC-ACT                 TO SAL1-REG-RECH
               MOVE 'TIPO DE DOCUMENTO NO INFORMADO'   TO SAL1-MOTIVO
               MOVE 'ARCHIVO ENTRADA CLIENTES'         TO SAL1-ORIGEN
               PERFORM 2300-REGISTRAR-RECHAZO-CLI
           END-IF

           IF WS-CLI-NUMDOC-ACT EQUAL SPACES OR LOW-VALUES
               SET NO-CORRECTO TO TRUE
               MOVE  WS-CLI-NUMDOC-ACT                 TO SAL1-REG-RECH
               MOVE 'NUMERO DE DOCUMENTO NO INFORMADO' TO SAL1-MOTIVO
               MOVE 'ARCHIVO ENTRADA CLIENTES'         TO SAL1-ORIGEN
               PERFORM 2300-REGISTRAR-RECHAZO-CLI
           END-IF

           IF WS-CLI-NOMBRE-ACT EQUAL SPACES OR LOW-VALUES
               SET NO-CORRECTO TO TRUE
               MOVE  WS-CLI-NUMDOC-ACT                 TO SAL1-REG-RECH
               MOVE 'NOMBRE DEL CLIENTE NO INFORMADO'  TO SAL1-MOTIVO
               MOVE 'ARCHIVO ENTRADA CLIENTES'         TO SAL1-ORIGEN
               PERFORM 2300-REGISTRAR-RECHAZO-CLI
           END-IF
           IF WS-CLI-EMAIL-ACT EQUAL SPACES OR LOW-VALUES
               SET NO-CORRECTO TO TRUE
               MOVE  WS-CLI-NUMDOC-ACT                 TO SAL1-REG-RECH
               MOVE 'CORREO NO INFORMADO'              TO SAL1-MOTIVO
               MOVE 'ARCHIVO ENTRADA CLIENTES'         TO SAL1-ORIGEN
               PERFORM 2300-REGISTRAR-RECHAZO-CLI
           ELSE
               MOVE ZEROS TO WS-ARROBA-COUNT
               INSPECT WS-CLI-EMAIL-ACT TALLYING WS-ARROBA-COUNT 
               FOR ALL "@"
               IF WS-ARROBA-COUNT = ZEROS OR WS-ARROBA-COUNT > 1
                   SET NO-CORRECTO TO TRUE
                   MOVE  WS-CLI-NUMDOC-ACT            TO SAL1-REG-RECH
                   MOVE 'CORREO INVALIDO'             TO SAL1-MOTIVO
                   MOVE 'ARCHIVO ENTRADA CLIENTES'    TO SAL1-ORIGEN
               PERFORM 2300-REGISTRAR-RECHAZO-CLI      
               END-IF
           END-IF
           .

      *---------------------------------------------------------------*
      * 2020-CRUCE-CUENTAS                       *
      *---------------------------------------------------------------*
       2020-CRUCE-CUENTAS.
           IF WS-CLI-NUMDOC-ACT = CUEN-CLIENTE
           PERFORM 1350-VALIDAR-CUENTAS
             IF SI-CORRECTO
               MOVE CUEN-NUMPRODUCTO TO WS-PRODUCTO
               MOVE CUEN-SALDO       TO WS-VALOR
               IF CUEN-TIPOPRODUCTO = 'AHO'
                  MOVE WSC-AHORROS    TO WS-TIPO
               ELSE
                    IF CUEN-TIPOPRODUCTO = 'CTE'
                       MOVE WSC-CTE  TO WS-TIPO
                    END-IF 
               END-IF
               ADD 1                 TO WS-PROD-MIGRADOS
               PERFORM 2500-REGISTRAR-MIGRADOS
             END-IF 
               SET SI-CUENTAS TO TRUE
               PERFORM 1320-LEER-CUENTAS
           ELSE
               IF WS-CLI-NUMDOC-ACT > CUEN-CLIENTE  
                 MOVE  CUEN-CLIENTE                 TO SAL1-REG-RECH
                 MOVE 'CUENTA SIN CLIENTE ASOCIADO' TO SAL1-MOTIVO
                 MOVE 'ARCHIVO ENTRADA CUENTAS'     TO SAL1-ORIGEN
                 PERFORM 2300-REGISTRAR-RECHAZO-CLI
                 PERFORM 1320-LEER-CUENTAS
               ELSE
                   SET NO-CUENTAS TO TRUE
               END-IF 
           END-IF
           .
      *---------------------------------------------------------------*
      * 2030-CRUCE-CDT                       *
      *---------------------------------------------------------------*
       2030-CRUCE-CDT.
           IF WS-CLI-NUMDOC-ACT = CDT-CLIENTE-CDT
            PERFORM 1360-VALIDAR-CDT
             IF SI-CORRECTO
                MOVE CDT-MONTO  TO WS-VALOR
                MOVE CDT-NUMCDT TO WS-PRODUCTO
                MOVE WSC-CDT     TO WS-TIPO
                ADD 1                 TO WS-PROD-MIGRADOS
                PERFORM 2500-REGISTRAR-MIGRADOS
                MOVE WS-CDT-ACTUAL TO WS-CDT-ANT
             END-IF
                SET SI-CDT TO TRUE
                PERFORM 1330-LEER-CDT
           ELSE
               IF WS-CLI-NUMDOC-ACT > CDT-CLIENTE-CDT
                 MOVE  CDT-CLIENTE-CDT              TO SAL1-REG-RECH
                 MOVE 'CDT SIN CLIENTE ASOCIADO'    TO SAL1-MOTIVO
                 MOVE 'ARCHIVO ENTRADA CDT'         TO SAL1-ORIGEN
                 PERFORM 2300-REGISTRAR-RECHAZO-CLI
                 MOVE CDT-CLIENTE-CDT   TO ANT-CLIENTE-CDT
                 MOVE CDT-NUMCDT        TO ANT-NUM-CDT
                 PERFORM 1330-LEER-CDT
               ELSE
                   SET NO-CDT TO TRUE
               END-IF
           END-IF
           .

      *---------------------------------------------------------------*
      * 2300-REGISTRAR-RECHAZO-CLI                  *
      *---------------------------------------------------------------*
       2300-REGISTRAR-RECHAZO-CLI.
           INITIALIZE RECHAZO-REG
      *SE ESCRIBE CLIENTE
           MOVE 'CLIENTE: '   TO RECHAZO-REG(1:9)
           MOVE SAL1-REG-RECH TO RECHAZO-REG(10:24)
           WRITE RECHAZO-REG
           IF ESTADO-05 = '00'
               ADD   1                      TO WS-GRABADOS-RECHAZOS
           ELSE
               DISPLAY 'ERROR DE ESCRITURA ARCHIVO RECHAZOS'
           END-IF
      *SE ESCRIBE MOTIVO
           MOVE 'MOTIVO: '    TO RECHAZO-REG(1:8)
           MOVE SAL1-MOTIVO   TO RECHAZO-REG(9:58)
           WRITE RECHAZO-REG
           IF ESTADO-05 = '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR DE ESCRITURA ARCHIVO RECHAZOS'
           END-IF  
      *SE ESCRIBE ORIGEN
           MOVE 'ORIGEN: '    TO RECHAZO-REG(1:8)
           MOVE SAL1-ORIGEN   TO RECHAZO-REG(9:58)
           WRITE RECHAZO-REG   
           IF ESTADO-05 = '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR DE ESCRITURA ARCHIVO RECHAZOS'
           END-IF
      *SEPARADOR ESPACIO   
           INITIALIZE RECHAZO-REG
           WRITE RECHAZO-REG 
           INITIALIZE SALIDA-RECHAZO
           .
      *---------------------------------------------------------------*
      * 2040-DESCARTAR-PRODUCTOS-DUPLICADO                            *
      *---------------------------------------------------------------*
       2040-DESCARTAR-PRODUCTOS-DUPLICADO.
           PERFORM 2045-DESCARTAR-CUENTAS 
               UNTIL SI-FIN-CUENTAS OR CUEN-CLIENTE > WS-CLI-NUMDOC-ACT
           PERFORM 2055-DESCARTAR-CDT 
               UNTIL SI-FIN-CDT OR CDT-CLIENTE-CDT > WS-CLI-NUMDOC-ACT
           .

       2045-DESCARTAR-CUENTAS.
           IF CUEN-CLIENTE = WS-CLI-NUMDOC-ACT
               PERFORM 1320-LEER-CUENTAS
           ELSE
               PERFORM 2850-RESTO-CUENTAS
           END-IF.

       2055-DESCARTAR-CDT.
           IF CDT-CLIENTE-CDT = WS-CLI-NUMDOC-ACT
               PERFORM 1330-LEER-CDT
           ELSE
               PERFORM 2880-RESTO-CDT
           END-IF.

      *---------------------------------------------------------------*
      * 2500-REGISTRAR-MIGRADOS                      *
      *---------------------------------------------------------------*
       2500-REGISTRAR-MIGRADOS.
           INITIALIZE MIGRADOS-REG  
           MOVE WS-CLI-NUMDOC-ACT                    TO SAL2-CLIENTE
           MOVE WS-PRODUCTO                          TO SAL2-PRODUCTO
           MOVE WS-TIPO                              TO SAL2-TIPO
           MOVE WS-VALOR                             TO SAL2-VALOR
           STRING
              FUNCTION TRIM(SAL2-CLIENTE)  
                             DELIMITED BY SIZE '|'
                             DELIMITED BY SIZE 
              FUNCTION TRIM(SAL2-PRODUCTO) 
                             DELIMITED BY SIZE '|'
                             DELIMITED BY SIZE
              FUNCTION TRIM(SAL2-TIPO) 
                             DELIMITED BY SIZE '|'
                             DELIMITED BY SIZE
              FUNCTION TRIM(SAL2-VALOR) 
                             DELIMITED BY SIZE
              INTO MIGRADOS-REG  
           END-STRING
           PERFORM 2510-GUARDAR-MIGRADOS
           .

      *---------------------------------------------------------------*
      * 2510-GUARDAR-MIGRADOS                       *
      *---------------------------------------------------------------*
       2510-GUARDAR-MIGRADOS.
           WRITE MIGRADOS-REG
           IF ESTADO-04 = '00'
               ADD   1                      TO WS-GRABADOS-MIGRADOS
           ELSE
               DISPLAY 'ERROR DE ESCRITURA ARCHIVO MIGRADOS'
           END-IF
           .

      *---------------------------------------------------------------*
      * 2800-LEER-RESTO                          *
      *---------------------------------------------------------------*
       2800-LEER-RESTO.
           IF SI-FIN-CLIENTES
             IF NO-FIN-CUENTAS
                PERFORM 2850-RESTO-CUENTAS UNTIL SI-FIN-CUENTAS
             END-IF
             IF NO-FIN-CDT
                PERFORM 2880-RESTO-CDT UNTIL SI-FIN-CDT
             END-IF
           END-IF 
           .

      *---------------------------------------------------------------*
      * 2850-RESTO-CUENTAS                       *
      *---------------------------------------------------------------*
       2850-RESTO-CUENTAS.
           IF NO-FIN-CUENTAS
               MOVE  CUEN-CLIENTE                  TO SAL1-REG-RECH
               MOVE 'CUENTA SIN CLIENTE ASOCIADO'  TO SAL1-MOTIVO
               MOVE 'ARCHIVO ENTRADA CUENTAS'      TO SAL1-ORIGEN
               PERFORM 2300-REGISTRAR-RECHAZO-CLI
           END-IF
           PERFORM 1320-LEER-CUENTAS
           .

      *---------------------------------------------------------------*
      * 2880-RESTO-CDT                           *
      *---------------------------------------------------------------*
       2880-RESTO-CDT.
           IF NO-FIN-CDT
               MOVE  CDT-CLIENTE-CDT              TO SAL1-REG-RECH
               MOVE 'CDT SIN CLIENTE ASOCIADO'    TO SAL1-MOTIVO
               MOVE 'ARCHIVO ENTRADA CDT'         TO SAL1-ORIGEN
               PERFORM 2300-REGISTRAR-RECHAZO-CLI          
           END-IF  
           PERFORM 1330-LEER-CDT
           .

      *---------------------------------------------------------------*
      * 2999-REPORTE                        *
      *---------------------------------------------------------------*
       2999-REPORTE.
           INITIALIZE REPORTE-REG
      *SE ESCRIBE CLIENTES PROCESADOS
           MOVE 'CLIENTES PROCESADOS: ' TO REPORTE-REG(1:22)
           MOVE WS-LEIDOS-CLIENTES      TO REPORTE-REG(23:27)
           WRITE REPORTE-REG
      *SE ESCRIBE CLIENTES MIGRADOS
           MOVE 'CLIENTES MIGRADOS: '   TO REPORTE-REG(1:22)
           MOVE WS-CLI-MIGRADOS          TO REPORTE-REG(23:27)
           WRITE REPORTE-REG
      *SE ESCRIBE PRODUCTOS MIGRADOS
           MOVE 'PRODUCTOS MIGRADOS: '  TO REPORTE-REG(1:22)
           MOVE WS-PROD-MIGRADOS        TO REPORTE-REG(23:27)
           WRITE REPORTE-REG
      *SE ESCRIBE RECHAZADOS
           MOVE 'RECHAZADOS: '            TO REPORTE-REG(1:22)
           MOVE WS-GRABADOS-RECHAZOS     TO REPORTE-REG(23:27)
           WRITE REPORTE-REG
      *SE ESCRIBE PORCENTAJE DE EXITO
           MOVE 'PORCENTAJE DE EXITO: '    TO REPORTE-REG(1:22)
           COMPUTE SAL3-PORC-EXITO = 
           (WS-CLI-MIGRADOS / WS-GRABADOS-RECHAZOS) * 100
           MOVE  SAL3-PORC-EXITO         TO SAL3-PORC-EDIT 
           STRING
               SAL3-PORC-EDIT
               DELIMITED BY SIZE
               '%'
               DELIMITED BY SIZE
               INTO SAL3-PORC
           END-STRING
           MOVE SAL3-PORC TO REPORTE-REG(23:27)
           WRITE REPORTE-REG
           ADD   1                      TO WS-GRABADOS-REPORTE
           .

      *---------------------------------------------------------------*
      * 3000-FINAL                               *
      *---------------------------------------------------------------*
       3000-FINAL.
           PERFORM 2999-REPORTE
           CLOSE CLIENTES-INPUT CUENTAS-INPUT CDT-INPUT MIGRADOS-OUTPUT
                 RECHAZO-OUTPUT REPORTE-OUTPUT
           STOP RUN
           .