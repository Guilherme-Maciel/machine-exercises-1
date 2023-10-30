       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX08.
       AUTHOR. Guilherme, Nirley, Renato.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 05/09/2023.
       DATE-COMPILED.
       SECURITY. NAO TEM

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. DELL.
       OBJECT-COMPUTER. DELL.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADFUN ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADSAI ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CADFUN
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADFUN.DAT".

       01 REG-ENT.
           02 COD-ENT PIC 9(05).
           02 NOME-ENT PIC X(20).
           02 BRUTO-ENT PIC 9(5)V99.

       FD CADSAI
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADSAI.DAT".

       01 REG-SAI.
           02 COD-SAI PIC 9(05).
           02 NOME-SAI PIC X(20).
           02 REAJUS-SAI PIC 9(5)V99.

       WORKING-STORAGE SECTION.
           77 FIM-ARQ PIC X(03) VALUE "NAO".
           77 PERCENTUAL-REAJUS PIC 9(2)V99 VALUE 0.
		   77 REAJUSTE PIC 9(5)V99 VALUE 0.

       PROCEDURE DIVISION.

       PGM-EX08.
           PERFORM INICIO.
           PERFORM PRINCIPAL 
               UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM TERMINO.
           STOP RUN.

       INICIO.
           OPEN INPUT CADFUN
           OUTPUT CADSAI.
           PERFORM LEITURA.
       
       LEITURA.
           READ CADFUN
               AT END 
               MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM GRAVACAO.
           PERFORM LEITURA.
               
       GRAVACAO.
           MOVE COD-ENT TO COD-SAI
           MOVE NOME-ENT TO NOME-SAI

           IF BRUTO-ENT > 0 AND BRUTO-ENT NOT > 1000
               MOVE 00,12 TO PERCENTUAL-REAJUS
           ELSE IF BRUTO-ENT > 1000 AND BRUTO-ENT NOT > 2000
               MOVE 00,11 TO PERCENTUAL-REAJUS
           ELSE 
               MOVE 00,10 TO PERCENTUAL-REAJUS
           END-IF.

           COMPUTE REAJUSTE = BRUTO-ENT + BRUTO-ENT * PERCENTUAL-REAJUS.
		   MOVE REAJUSTE TO REAJUS-SAI.

           WRITE REG-SAI.

       TERMINO.
           CLOSE CADFUN
                 CADSAI.