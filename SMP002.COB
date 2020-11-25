       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP002.
      *AUTHOR. Roberto da Silva Mitsunari.
      ******************************************
      *    MANUTENCAO DO CADASTRO DE DOENÇAS   *
      ******************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCID ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS DENOMINACAO WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCID
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCID.DAT". 
       01 REGCID.
          03 CODIGO            PIC 9(04).
          03 DENOMINACAO       PIC X(30).
          03 SINTOMAS          PIC X(50).
          03 TRATAMENTO        PIC X(50).

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       77 LIMPA         PIC X(50) VALUE SPACES. 
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 W-SEL         PIC 9(01) VALUE ZEROS.
       01 TXTTPAMIGO    PIC X(10) VALUE SPACES.
       01 IND           PIC 9(02) VALUE ZEROS.
       01 TEXSEXO       PIC X(12) VALUE SPACES.

       

      *-----------------------------------------------------------------
       SCREEN SECTION.
   
       01  CIDTELA.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "-----------------------------CADASTRO DE".
           05  LINE 02  COLUMN 41 
               VALUE  " DOENCAS--------------------------------".
           05  LINE 05  COLUMN 01 
               VALUE  "     Codigo Da Doenca:".
           05  LINE 07  COLUMN 01 
               VALUE  "     Deniminacao:".         
           05  LINE 09  COLUMN 01 
               VALUE  "     Sintomas:".
           05  LINE 11 COLUMN 01 
               VALUE  "     Tratamento:".

           05  TCOD
               LINE 05  COLUMN 24  PIC 9(04)
               USING  CODIGO
               HIGHLIGHT.
           05  TDEN
               LINE 07  COLUMN 19  PIC X(30)
               USING  DENOMINACAO
               HIGHLIGHT.
           05  TSIN
               LINE 09  COLUMN 16  PIC X(50)
               USING  SINTOMAS
               HIGHLIGHT.
           05  TTRAT
               LINE 11  COLUMN 18  PIC X(50)
               USING  TRATAMENTO
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       
       R0.
           OPEN I-O CADCID
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCID
                 CLOSE CADCID
                 MOVE "*** ARQUIVO CADCID FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADCID" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.


       R1.    
           MOVE SPACES TO DENOMINACAO SINTOMAS TRATAMENTO
           MOVE ZEROS TO CODIGO
           DISPLAY CIDTELA.

       R2.
           
           MOVE ZEROS TO CODIGO
           
           ACCEPT TCOD
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.

           IF CODIGO = ZEROS
                MOVE "CODIGO INVALIDO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R2.

        
      *---------------VERIFICA SE JÁ EXISTE NO ARQUIVO-----------------*

       LER-CADCID.
           READ CADCID
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                DISPLAY CIDTELA
                GO TO ACE-001
                
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADCID" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.

      *----------------------------------------------------------------*

       R3.
           MOVE SPACES TO DENOMINACAO
           
           ACCEPT TDEN
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2. 

           IF DENOMINACAO = SPACES 
              MOVE "DIGITE A DESCRICAO DA DOENCA" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.  

       R4.
           MOVE SPACES TO SINTOMAS
           
           ACCEPT TSIN
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3. 

           IF SINTOMAS = SPACES 
              MOVE "DIGITE OS SISTOMAS DA DOENCA" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.

       R5.
           MOVE SPACES TO TRATAMENTO
           
           ACCEPT TTRAT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4. 

           IF TRATAMENTO = SPACES 
              MOVE "DIGITE OS SISTOMAS DA DOENCA" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R5.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCID
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  
                  GO TO ALT-RW1
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.  


      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01
                   GO TO R1.
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R1  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3
                   ELSE
                      GO TO EXC-OPC.
      *          
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADCID RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R3.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCID
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA ALTERACAO DO REGISTRO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       ROT-FIM.
           CLOSE CADCID.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (23, 12) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

      *    FILE STATUS
      *    00 = OPERA��O REALIZADO COM SUCESSO
      *    22 = REGISTRO J� CADASTRADO
      *    23 = REGISTRO N�O ENCONTRADO
      *    30 = ARQUIVO N�O ENCONTRADO
           