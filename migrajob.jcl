//MIGRAJOB JOB (ACCT),'PRUEBA TECNICA',
//             CLASS=A,MSGCLASS=A,NOTIFY=&SYSUID
//* ----------------------------------------------------
//* EJECUCIÓN PROGRAMA CAMBIA ESTRUCTURA
//* ----------------------------------------------------
//STEP10 EXEC PGM=ESTRUCTURA
//CLIENTES-INPUT   DD DSN=data/clientes.txt,DISP=SHR
//CUENTAS-INPUT    DD DSN=data/cuentas.txt,DISP=SHR
//CDT-INPUT        DD DSN=data/cdt.txt,DISP=SHR
//CLI-OUTPUT       DD DSN=output/clioutput.txt,
//                    DISP=(NEW,CATLG,DELETE),
//                    SPACE=(CYL,(20,5),RLSE),
//                    DCB=(RECFM=FB,BLKSIZE=0)
//CUEN-OUTPUT      DD DSN=output/cuenoutput.txt,
//                    DISP=(NEW,CATLG,DELETE),
//                    SPACE=(CYL,(20,5),RLSE),
//                    DCB=(RECFM=FB,BLKSIZE=0)
//CDT-OUTPUT       DD DSN=output/cdtoutput.txt,
//                    DISP=(NEW,CATLG,DELETE),
//                    SPACE=(CYL,(20,5),RLSE),
//                    DCB=(RECFM=FB,BLKSIZE=0)
//SYSOUT           DD  SYSOUT=*
//SYSUDUMP         DD  SYSOUT=*
//* ----------------------------------------------------
//* ORDENAMIENTO DE CLIENTES POR DOCUMENTO
//* ----------------------------------------------------
//STEP07 EXEC PGM=SYNCSORT,COND=(4,LT)                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SORTIN   DD DSN=output/clioutput.txt,             
//            DISP=SHR                                                  
//SORTOUT  DD DSN=ouput/clientes_sort.txt,             
//            UNIT=SYSDA,                                               
//            SPACE=(CYL,(20,5),RLSE),                               
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0,DSORG=PS),              
//            DISP=(NEW,CATLG,DELETE)                                   
//SYSIN    DD *                                                         
   SORT FIELDS=(1,15,CH,A) 
//* ----------------------------------------------------
//* ORDENAMIENTO DE CUENTAS POR CLIENTE
//* ----------------------------------------------------
//STEP04 EXEC PGM=SYNCSORT,COND=(4,LT)                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SORTIN   DD DSN=output/cuenoutput.txt,             
//            DISP=SHR                                                  
//SORTOUT  DD DSN=ouput/cuentas_sort.txt,             
//            UNIT=SYSDA,                                               
//            SPACE=(CYL,(20,5),RLSE),                               
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0,DSORG=PS),              
//            DISP=(NEW,CATLG,DELETE)                                   
//SYSIN    DD *                                                         
   SORT FIELDS=(29,15,CH,A)
//* ----------------------------------------------------
//* ORDENAMIENTO DE CDT POR CLIENTE
//* ----------------------------------------------------
//STEP04 EXEC PGM=SYNCSORT,COND=(4,LT)                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SORTIN   DD DSN=output/cdtoutput.txt,             
//            DISP=SHR                                                  
//SORTOUT  DD DSN=ouput/cdt_sort.txt,             
//            UNIT=SYSDA,                                               
//            SPACE=(CYL,(20,5),RLSE),                               
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0,DSORG=PS),              
//            DISP=(NEW,CATLG,DELETE)                                   
//SYSIN    DD *                                                         
   SORT FIELDS=(41,15,CH,A) 
//* ----------------------------------------------------
//* EJECUCIÓN PROGRAMA MIGRACION
//* ----------------------------------------------------
//STEP02 EXEC PGM=MIGRACION
//CLIENTES-INPUT   DD DSN=ouput/clientesort.txt,DISP=SHR
//CUENTAS-INPUT    DD DSN=ouput/cuentasort.txt,DISP=SHR
//CDT-INPUT        DD DSN=ouput/cdtsort.txt,DISP=SHR
//MIGRADOS-OUTPUT  DD DSN=output/CLIENTES_MIGRADOS.txt,
//                    DISP=(NEW,CATLG,DELETE),
//                    SPACE=(CYL,(20,5),RLSE),
//                    DCB=(RECFM=FB,BLKSIZE=0)
//RECHAZOS-OUTPUT   DD DSN=output/CLIENTES_RECHAZADOS.txt,
//                    DISP=(NEW,CATLG,DELETE),
//                    SPACE=(CYL,(20,5),RLSE),
//                    DCB=(RECFM=FB,BLKSIZE=0)
//REPORTE-OUTPUT   DD DSN=output/REPORTE_MIGRACION.txt,
//                    DISP=(NEW,CATLG,DELETE),
//                    SPACE=(CYL,(20,5),RLSE),
//                    DCB=(RECFM=FB,BLKSIZE=0)
//SYSOUT           DD  SYSOUT=*
//SYSUDUMP         DD  SYSOUT=*