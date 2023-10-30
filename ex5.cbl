       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX05.
       AUTHOR. Guilherme, Renato, Nirley.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 15/08/2023.
       DATE-COMPILED.
       SECURITY. NAO TEM

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. DELL.
       OBJECT-COMPUTER. DELL.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADALU ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADAPR ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CADALU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADALU.DAT".
       
       01 REG-ENT.
           02 COD-ENT PIC 9(05).
           02 NOME-ENT PIC X(20).
           02 NOTA1-ENT PIC 9(2)V99.
           02 NOTA2-ENT PIC 9(2)V99.
           02 FALTAS-ENT PIC 9(2).
       FD CADAPR
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADAPR.DAT".

       01 REG-SAI.
           02 COD-SAI PIC 9(05).
           02 NOME-SAI PIC X(20).
           02 MEDIA-SAI PIC 9(02)V99.


       WORKING-STORAGE SECTION.
           77 FIM-ARQ PIC X(03) VALUE "NAO".
           77 MEDIA PIC 9(02)V99 VALUE 0.


       PROCEDURE DIVISION.

       PGM-EX01.
           PERFORM INICIO.
           PERFORM PRINCIPAL 
               UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM TERMINO.
           STOP RUN.

       INICIO.
           OPEN INPUT CADALU
           OUTPUT CADAPR.
           PERFORM LEITURA.
       
       LEITURA.
           READ CADALU
               AT END 
               MOVE "SIM" TO FIM-ARQ.
               
       PRINCIPAL.
           PERFORM GRAVACAO.
           PERFORM LEITURA.

       GRAVACAO.
           COMPUTE MEDIA = (NOTA1-ENT + NOTA2-ENT) / 2.
           IF MEDIA NOT< 7 AND FALTAS-ENT NOT> 18
               MOVE COD-ENT TO COD-SAI
               MOVE NOME-ENT TO NOME-SAI
               MOVE MEDIA TO MEDIA-SAI
               WRITE REG-SAI.
           
       TERMINO.
           CLOSE CADALU
                 CADAPR.
