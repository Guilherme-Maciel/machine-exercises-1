       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX04.
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
      *Entrar arquivo em disco chamado CAD-ENT gerando um CAD-SAI
           SELECT CADCLI1 ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADCLI2 ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CADCLI1
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADCLI1.DAT".
       
       01 REG-ENT.
           02 COD-ENT PIC 9(05).
           02 NOME-ENT PIC X(20).
           02 SEXO-ENT PIC X(01).

       FD CADCLI2
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADCLI2.DAT".

       01 REG-SAI.
           02 COD-SAI PIC 9(05).
           02 NOME-SAI PIC X(20).

       WORKING-STORAGE SECTION.
           77 FIM-ARQ PIC X(03) VALUE "NAO".

       PROCEDURE DIVISION.

       PGM-EX04.
           PERFORM INICIO.
           PERFORM PRINCIPAL 
               UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM TERMINO.
           STOP RUN.

       INICIO.
           OPEN INPUT CADCLI1
           OUTPUT CADCLI2.
           PERFORM LEITURA.
       
       LEITURA.
           READ CADCLI1
               AT END 
               MOVE "SIM" TO FIM-ARQ.
               
       PRINCIPAL.
           PERFORM GRAVACAO.
           PERFORM LEITURA.

       GRAVACAO.
           IF SEXO-ENT = "M" OR "m"
               MOVE COD-ENT TO COD-SAI
               MOVE NOME-ENT TO NOME-SAI
               WRITE REG-SAI.
           
       TERMINO.
           CLOSE CADCLI1
                 CADCLI2.
