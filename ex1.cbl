       IDENTIFICATION DIVISION.
       PROGRAM-ID. EX01.
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
      *disco = standard relatório = ommited
      *cad-ent = cad-ent.dat = arquivo
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADCLI1.DAT".
       
       01 REG-ENT.
           02 COD-ENT PIC 9(05).
           02 NOME-ENT PIC X(20).

       FD CADCLI2
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADCLI2.DAT".

       01 REG-SAI.
           02 NOME-SAI PIC X(20).
           02 COD-SAI PIC 9(05).

       WORKING-STORAGE SECTION.
           77 FIM-ARQ PIC X(03) VALUE "NAO".

       PROCEDURE DIVISION.

       PGM-EX01.
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
           MOVE COD-ENT TO COD-SAI.
           MOVE NOME-ENT TO NOME-SAI.
           WRITE REG-SAI.
           
       TERMINO.
           CLOSE CADCLI1
                 CADCLI2.
