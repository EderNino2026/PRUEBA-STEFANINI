       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESTRUCTURA.
       AUTHOR. EDER NINO MORA.
       DATE-WRITTEN. 22/06/2026.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT CLIENTES-INPUT ASSIGN TO "data/clientes.txt"
       ORGANIZATION IS LINE SEQUENTIAL
       FILE STATUS IS ESTADO-01.

       SELECT CUENTAS-INPUT ASSIGN TO "data/cuentas.txt"
       ORGANIZATION IS LINE SEQUENTIAL
       FILE STATUS IS ESTADO-02.

       SELECT CDT-INPUT ASSIGN TO "data/cdt.txt"
       ORGANIZATION IS LINE SEQUENTIAL
       FILE STATUS IS ESTADO-03.

       SELECT CLI-OUTPUT ASSIGN TO "output/clioutput.txt"
       ORGANIZATION IS LINE SEQUENTIAL
       FILE STATUS IS ESTADO-04.

       SELECT CUEN-OUTPUT ASSIGN TO "output/cuenoutput.txt"
       ORGANIZATION IS LINE SEQUENTIAL
       FILE STATUS IS ESTADO-05.

       SELECT CDT-OUTPUT ASSIGN TO "output/cdtoutput.txt"
       ORGANIZATION IS LINE SEQUENTIAL
       FILE STATUS IS ESTADO-06.
       
       DATA DIVISION.
       FILE SECTION.

       FD CLIENTES-INPUT.
       01 CLIENTE-REG PIC X(200).

       FD CUENTAS-INPUT.
       01 CUENTAS-REG PIC X(200).

       FD CDT-INPUT.
       01 CDTS-REG PIC X(200).

       FD CLI-OUTPUT.
       01 CLI-REG.
           05 CLI-TIPODOC  PIC X(3).
           05 CLI-NUMDOC   PIC X(15).
           05 CLI-NOMBRE   PIC X(50).
           05 CLI-EMAIL    PIC X(50).

       FD CUEN-OUTPUT.
       01 CUEN-REG.
           05 CUEN-NUMPRODUCTO   PIC X(5).
           05 CUEN-TIPOPRODUCTO  PIC X(3).
           05 CUEN-SALDO         PIC X(20).
           05 CUEN-CLIENTE       PIC X(15).

       FD CDT-OUTPUT.
       01 CDT-REG.
           05 CDT-NUMCDT       PIC X(5).
           05 CDT-MONTO        PIC X(20).
           05 CDT-FECHA-APE    PIC X(8).
           05 CDT-FECHA-VEC    PIC X(8).
           05 CDT-CLIENTE-CDT  PIC X(15).
      *---------------------------------------------------------------*
      *                   WORKING-STORAGE SECTION                     *
      *---------------------------------------------------------------*

       WORKING-STORAGE SECTION.

       01 ENTRADA-CLIENTES.
           05 ENT1-TIPODOC  PIC X(3).
           05 ENT1-NUMDOC   PIC X(15).
           05 ENT1-NOMBRE   PIC X(50).
           05 ENT1-EMAIL    PIC X(50).

       01 ENTRADA-CUENTAS.
           05 ENT2-NUMPRODUCTO   PIC X(5).
           05 ENT2-TIPOPRODUCTO  PIC X(3).
           05 ENT2-SALDO         PIC X(20).
           05 ENT2-CLIENTE       PIC X(15).

       01 ENTRADA-CDT.
           05 ENT3-NUMCDT       PIC X(5).
           05 ENT3-MONTO        PIC X(20).
           05 ENT3-FECHA-APE    PIC X(8).
           05 ENT3-FECHA-VEC    PIC X(8).
           05 ENT3-CLIENTE-CDT  PIC X(15).
      *---------------------------------------------------------------*
      *                              SWITCHES                         *
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
      *---------------------------------------------------------------*
      *                     ESTADO DE ARCHIVOS                        *
      *---------------------------------------------------------------*
       77 ESTADO-01 PIC X(2) VALUE '00'.
       77 ESTADO-02 PIC X(2) VALUE '00'.
       77 ESTADO-03 PIC X(2) VALUE '00'.
       77 ESTADO-04 PIC X(2) VALUE '00'.
       77 ESTADO-05 PIC X(2) VALUE '00'.
       77 ESTADO-06 PIC X(2) VALUE '00'.
      *---------------------------------------------------------------*
      *                            CONTADORES                         *
      *---------------------------------------------------------------*
       77 WS-LEIDOS-CLIENTES   PIC 9(5) VALUE ZEROS.
       77 WS-LEIDOS-CUENTAS    PIC 9(5) VALUE ZEROS.
       77 WS-LEIDOS-CDT        PIC 9(5) VALUE ZEROS.
       77 WS-GRABADOS-CLIENTES PIC 9(5) VALUE ZEROS.
       77 WS-GRABADOS-CUENTAS  PIC 9(5) VALUE ZEROS.
       77 WS-GRABADOS-CDT      PIC 9(5) VALUE ZEROS.
      *---------------------------------------------------------------*
      *                      PROCEDURE DIVISION                       *
      *---------------------------------------------------------------*
       PROCEDURE DIVISION.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO
           PERFORM 3000-FINAL.
      *---------------------------------------------------------------*
      *                      1000-INICIO                              *
      *---------------------------------------------------------------*
       1000-INICIO.
           PERFORM 1100-ABRIR-ARCHIVOS
           PERFORM 1200-SALTAR-CABECERAS
           .
      *---------------------------------------------------------------*
      *                      1100-ABRIR-ARCHIVOS                      *
      *---------------------------------------------------------------*
       1100-ABRIR-ARCHIVOS.
           OPEN INPUT CLIENTES-INPUT
           IF ESTADO-01 = '00' OR '97'
               CONTINUE
           ELSE
           DISPLAY 'ERROR ABRIENDO ARCHIVO DE CLIENTES    ' ESTADO-01
           MOVE 12                 TO RETURN-CODE
           STOP RUN
           END-IF

           OPEN INPUT CUENTAS-INPUT
           IF ESTADO-02 = '00' OR '97'
               CONTINUE
           ELSE
           DISPLAY 'ERROR ABRIENDO ARCHIVO DE CUENTAS    ' ESTADO-02
           MOVE 12                 TO RETURN-CODE
           CLOSE CLIENTES-INPUT
           STOP RUN
           END-IF

           OPEN INPUT CDT-INPUT
           IF ESTADO-03 = '00' OR '97'
               CONTINUE
           ELSE
           DISPLAY 'ERROR ABRIENDO ARCHIVO DE CDT    ' ESTADO-03
           MOVE 12                 TO RETURN-CODE
           CLOSE CLIENTES-INPUT CUENTAS-INPUT
           STOP RUN
           END-IF

           OPEN OUTPUT CLI-OUTPUT
           IF ESTADO-04 = '00' OR '97'
               CONTINUE
           ELSE
           DISPLAY 'ERROR ABRIENDO ARCHIVO SALIDA CLIENTES ' ESTADO-04
           MOVE 12                 TO RETURN-CODE
           CLOSE CLIENTES-INPUT CUENTAS-INPUT CDT-INPUT
           STOP RUN
           END-IF

           OPEN OUTPUT CUEN-OUTPUT
           IF ESTADO-05 = '00' OR '97'
               CONTINUE
           ELSE
           DISPLAY 'ERROR ABRIENDO ARCHIVO SALIDA CUENTAS ' ESTADO-05
           MOVE 12                 TO RETURN-CODE
           CLOSE CLIENTES-INPUT CUENTAS-INPUT CDT-INPUT CLI-OUTPUT
           STOP RUN
           END-IF

           OPEN OUTPUT CDT-OUTPUT
           IF ESTADO-06 = '00' OR '97'
               CONTINUE
           ELSE
           DISPLAY 'ERROR ABRIENDO ARCHIVO SALIDA CDT    ' ESTADO-06
           MOVE 12                 TO RETURN-CODE
           CLOSE CLIENTES-INPUT CUENTAS-INPUT CDT-INPUT CLI-OUTPUT
                 CUEN-OUTPUT
           STOP RUN
           END-IF
           .
      *---------------------------------------------------------------*
      *                    1200-SALTAR-CABECERAS                      *
      *---------------------------------------------------------------*
       1200-SALTAR-CABECERAS.
           READ CLIENTES-INPUT NEXT RECORD
           AT END
               SET SI-FIN-CLIENTES       TO TRUE
               MOVE HIGH-VALUES          TO CLIENTE-REG
           END-READ

           READ CUENTAS-INPUT NEXT RECORD
           AT END
               SET SI-FIN-CUENTAS       TO TRUE
               MOVE HIGH-VALUES         TO CUENTAS-REG
           END-READ

           READ CDT-INPUT NEXT RECORD
           AT END
               SET SI-FIN-CDT      TO TRUE
               MOVE HIGH-VALUES         TO CDTS-REG
           END-READ
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
               UNSTRING CLIENTE-REG DELIMITED BY '|'
                INTO ENT1-TIPODOC
                     ENT1-NUMDOC
                     ENT1-NOMBRE
                     ENT1-EMAIL
                END-UNSTRING
               PERFORM 2200-GRABAR-SALIDA1
               ADD  1                    TO WS-LEIDOS-CLIENTES
           .
      *---------------------------------------------------------------*
      *                    1320-LEER-CUENTAS                          *
      *---------------------------------------------------------------*
       1320-LEER-CUENTAS.
           READ CUENTAS-INPUT NEXT RECORD
            AT END
               SET SI-FIN-CUENTAS       TO TRUE
               MOVE HIGH-VALUES          TO CUENTAS-REG
            NOT AT END
                UNSTRING CUENTAS-REG DELIMITED BY '|'
                INTO ENT2-NUMPRODUCTO
                     ENT2-TIPOPRODUCTO
                     ENT2-SALDO
                     ENT2-CLIENTE
                END-UNSTRING
                PERFORM 2300-GRABAR-SALIDA2
                ADD  1              TO WS-LEIDOS-CUENTAS
           .
      *---------------------------------------------------------------*
      *                    1330-LEER-CDT                              *
      *---------------------------------------------------------------*
       1330-LEER-CDT.
           READ CDT-INPUT NEXT RECORD
            AT END
               SET SI-FIN-CDT       TO TRUE
               MOVE HIGH-VALUES          TO CDT-REG
            NOT AT END
               UNSTRING CDTS-REG DELIMITED BY '|'
                INTO ENT3-NUMCDT
                     ENT3-MONTO
                     ENT3-FECHA-APE
                     ENT3-FECHA-VEC
                     ENT3-CLIENTE-CDT
                END-UNSTRING
                PERFORM 2400-GRABAR-SALIDA3
                ADD  1              TO WS-LEIDOS-CDT
           .
      *---------------------------------------------------------------*
      *                      2000-PROCESO                               *
      *---------------------------------------------------------------*
       2000-PROCESO.
           PERFORM 2010-LEER-CLIENTES 
           .
      *---------------------------------------------------------------*
      *                    2010-LEER-CLIENTES                         *
      *---------------------------------------------------------------*
       2010-LEER-CLIENTES.
           PERFORM 1310-LEER-CLIENTES UNTIL SI-FIN-CLIENTES
           PERFORM 1320-LEER-CUENTAS  UNTIL SI-FIN-CUENTAS
           PERFORM 1330-LEER-CDT      UNTIL SI-FIN-CDT
           .
      *---------------------------------------------------------------*
      *                    2200-GRABAR-SALIDA1                        *
      *---------------------------------------------------------------*
       2200-GRABAR-SALIDA1.
           MOVE ENT1-NUMDOC  TO CLI-NUMDOC
           MOVE ENT1-TIPODOC TO CLI-TIPODOC
           MOVE ENT1-NOMBRE  TO CLI-NOMBRE
           MOVE ENT1-EMAIL   TO CLI-EMAIL
           WRITE CLI-REG
           IF ESTADO-04 = '00' OR '97'
               ADD 1 TO WS-GRABADOS-CLIENTES
           ELSE
               DISPLAY 'ERROR GRABANDO ARCHIVO DE CLIENTES ' ESTADO-04
               MOVE 12                 TO RETURN-CODE
               PERFORM 3000-FINAL
           END-IF
           .
      *---------------------------------------------------------------*
      *                    2300-GRABAR-SALIDA2                        *
      *---------------------------------------------------------------*
       2300-GRABAR-SALIDA2.
           MOVE ENT2-NUMPRODUCTO   TO CUEN-NUMPRODUCTO
           MOVE ENT2-TIPOPRODUCTO  TO CUEN-TIPOPRODUCTO
           MOVE ENT2-SALDO         TO CUEN-SALDO
           MOVE ENT2-CLIENTE       TO CUEN-CLIENTE
           WRITE CUEN-REG
           IF ESTADO-05 = '00' OR '97'
               ADD 1 TO WS-GRABADOS-CUENTAS
           ELSE
               DISPLAY 'ERROR GRABANDO ARCHIVO DE CUENTAS ' ESTADO-05
               MOVE 12                 TO RETURN-CODE
               PERFORM 3000-FINAL
           END-IF
           .
      *---------------------------------------------------------------*
      *                    2400-GRABAR-SALIDA3                        *
      *---------------------------------------------------------------*
       2400-GRABAR-SALIDA3.
           MOVE ENT3-NUMCDT       TO CDT-NUMCDT
           MOVE ENT3-MONTO        TO CDT-MONTO
           MOVE ENT3-FECHA-APE    TO CDT-FECHA-APE
           MOVE ENT3-FECHA-VEC    TO CDT-FECHA-VEC
           MOVE ENT3-CLIENTE-CDT  TO CDT-CLIENTE-CDT
           WRITE CDT-REG
           IF ESTADO-06 = '00' OR '97'
               ADD 1 TO WS-GRABADOS-CDT
           ELSE
               DISPLAY 'ERROR GRABANDO ARCHIVO DE CDT ' ESTADO-06
               MOVE 12                 TO RETURN-CODE
               PERFORM 3000-FINAL
           END-IF
           .
      *---------------------------------------------------------------*
      *                      3000-FINAL                               *
      *---------------------------------------------------------------*
       3000-FINAL.
           CLOSE CLIENTES-INPUT CUENTAS-INPUT CDT-INPUT CLI-OUTPUT 
           CUEN-OUTPUT CDT-OUTPUT
           DISPLAY '--------RESULTADO DEL PROCESO--------'
           DISPLAY 'ARCHIVOS CLIENTES LEIDOS:   ' WS-LEIDOS-CLIENTES
           DISPLAY 'ARCHIVOS CUENTAS LEIDOS:    ' WS-LEIDOS-CUENTAS
           DISPLAY 'ARCHIVOS CDT LEIDOS:        ' WS-LEIDOS-CDT
           DISPLAY 'ARCHIVOS CLIENTES GRABADOS: ' WS-GRABADOS-CLIENTES
           DISPLAY 'ARCHIVOS CUENTAS GRABADOS:  ' WS-GRABADOS-CUENTAS
           DISPLAY 'ARCHIVOS CDT GRABADOS:      ' WS-GRABADOS-CDT
           DISPLAY '-------------------------------------'
           STOP RUN
           .