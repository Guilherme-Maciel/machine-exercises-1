       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX02.
       AUTHOR. Guilherme, Nirley, Renato.
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
           SELECT CADATU ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CADALU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADALU.DAT".
       
       01 REG-ENT.
           02 NUMERO-LU PIC 9(05).
           02 NOME-LU PIC X(20).
           02 SEXO-LU PIC X(01).
           02 DATA-NASCIMENTO-LU.
               03 DD-LU PIC 9(2).
               03 MM-LU PIC 99.
               03 AAAA-LU PIC 9(04).
       FD CADATU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADATU.DAT".

       01 REG-SAI.
           02 NUMERO-TU PIC 9(05).
           02 NOME-TU PIC X(20).
           02 DATA-NASCIMENTO-TU.
               03 DD-TU PIC 9(2).
               03 MM-TU PIC 99.
               03 AAAA-TU PIC 9(04).

       WORKING-STORAGE SECTION.
           77 FIM-ARQ PIC X(03) VALUE "NAO".

       PROCEDURE DIVISION.

       PGM-EX02.
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
           MOVE NUMERO-LU TO NUMERO-TU.
           MOVE NOME-LU TO NOME-TU.
           MOVE DATA-NASCIMENTO-LU TO DATA-NASCIMENTO-TU
           MOVE DD-LU TO DD-TU.
           MOVE MM-LU TO MM-TU.
           MOVE AAAA-LU TO AAAA-TU.
           WRITE REG-SAI.
           
       TERMINO.
           CLOSE CADALU
                 CADATU.
