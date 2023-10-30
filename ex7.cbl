       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX07.
       AUTHOR. Guilherme, Renato, Nirley
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 17/10/2023.
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
           SELECT CADATU ASSIGN TO DISK
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
           02 NOTA3-ENT PIC 9(2)V99.
           02 NOTA4-ENT PIC 9(2)V99.
           02 SEXO-ENT PIC X(1).
       FD CADATU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADATU.DAT".

       01 REG-SAI.
           02 COD-SAI PIC 9(05).
           02 NOME-SAI PIC X(20).
           02 MEDIA-SAI PIC 9(02)V99.
           02 SEXO-SAI PIC X(01).

       WORKING-STORAGE SECTION.
           77 FIM-ARQ PIC X(03) VALUE "NAO".
           77 MEDIA PIC 9(02)V99 VALUE 0.


       PROCEDURE DIVISION.

       PGM-EX07.
           PERFORM INICIO.
           PERFORM PRINCIPAL 
               UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM TERMINO.
           STOP RUN.

       INICIO.
           OPEN INPUT CADALU
           OUTPUT CADATU.
           PERFORM LEITURA.
       
       LEITURA.
           READ CADALU
               AT END 
               MOVE "SIM" TO FIM-ARQ.
               
       PRINCIPAL.
           PERFORM GRAVACAO.
           PERFORM LEITURA.

       GRAVACAO.
           IF SEXO-ENT = "F" OR "f"
               ADD NOTA1-ENT, NOTA2-ENT, NOTA3-ENT, NOTA4-ENT TO MEDIA
               DIVIDE 4 INTO MEDIA
               MOVE COD-ENT TO COD-SAI
               MOVE NOME-ENT TO NOME-SAI
               MOVE MEDIA TO MEDIA-SAI
               MOVE SEXO-ENT TO SEXO-SAI
               WRITE REG-SAI.
           
       TERMINO.
           CLOSE CADALU
                 CADATU.
