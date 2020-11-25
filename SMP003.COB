       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP003.
      *AUTHOR. ROBERTO DA SILVA MITSUNARI.
      *******************************************
      *    MANUTENCAO DO CADASTRO DO CONVENIO   *
      *******************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME WITH DUPLICATES
                    ALTERNATE RECORD KEY IS PLANO WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT". 
       01 REGCONV.
          03 CODIGO       PIC 9(04).
          03 NOME         PIC X(30).
          03 PLANO        PIC 9(02).

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

       01 TABCONVX.
          03 FILLER     PIC X(30) VALUE "ENFERMARIA REGINAL".
          03 FILLER     PIC X(30) VALUE "ENFERMARIA NACIONAL".
          03 FILLER     PIC X(30) VALUE "ENFERMARIA INTERNACIONACAO".
          03 FILLER     PIC X(30) VALUE "APTO PADRAO REGINAL".
          03 FILLER     PIC X(30) VALUE "APTO PADRAO NACIONAL".
          03 FILLER     PIC X(30) VALUE "APTO PADRAO INTERNACIONAL".
          03 FILLER     PIC X(30) VALUE "EMERGENCIA REGIONAL".
          03 FILLER     PIC X(30) VALUE "EMERGENCIA NACIONAL".
          03 FILLER     PIC X(30) VALUE "EMERCENCIA INTERNACIONAL".
          03 FILLER     PIC X(30) VALUE "PLANO GLOBAL".
          03 FILLER     PIC X(30) VALUE "PLANO GLOBAL OURO".
          03 FILLER     PIC X(30) VALUE "PLANO GLOBAL PLATINIUM".

       01 TABCONV REDEFINES TABCONVX.
          03 TBCONV   PIC X(30) OCCURS 12 TIMES.

       01 PLANOTXT    PIC X(30) VALUE SPACES.

      *-----------------------------------------------------------------
       SCREEN SECTION.
   
       01  TELACONV.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                            CADASTRO DE".
           05  LINE 02  COLUMN 41 
               VALUE  " CONVENIO                                   ".
           05  LINE 05  COLUMN 01 
               VALUE  "     CODIGO:".
           05  LINE 07  COLUMN 01 
               VALUE  "     NOME:".
           05  LINE 10  COLUMN 01 
               VALUE  "     PLANO:".
           05  TCOD
               LINE 05  COLUMN 14  PIC 9(04)
               USING  CODIGO
               HIGHLIGHT.
           05  TNOME
               LINE 07  COLUMN 12  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TPLANO
               LINE 10  COLUMN 13  PIC 9(02)
               USING  PLANO
               HIGHLIGHT.
           05  TPLANOTXT
               LINE 11  COLUMN 17  PIC X(30)
               USING  PLANOTXT
               HIGHLIGHT.

       01  SMP003TELA.
           05  LINE 13  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 13  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 15  COLUMN 01 
               VALUE  "   1 - ENFERMARIA REGIONAL".
           05  LINE 15  COLUMN 41 
               VALUE  "       7 - EMERGENCIA REGIONAL".
           05  LINE 16  COLUMN 01 
               VALUE  "   2 - ENFERMARIA NACIONAL".
           05  LINE 16  COLUMN 41 
               VALUE  "       8 - EMERGENCIA NACIONAL".
           05  LINE 17  COLUMN 01 
               VALUE  "   3 - ENFERMARIA INTERNACIONACAO".
           05  LINE 17  COLUMN 41 
               VALUE  "       9 - EMERGENCIA INTERNACIONAL".
           05  LINE 18  COLUMN 01 
               VALUE  "   4 - APTO PADRAO REGIONAL".
           05  LINE 18  COLUMN 41 
               VALUE  "      10 - PLANO GLOBAL".
           05  LINE 19  COLUMN 01 
               VALUE  "   5 - APTO PADRAO NACIONAL".
           05  LINE 19  COLUMN 41 
               VALUE  "      11 - PLANO GLOBAL OURO".
           05  LINE 20  COLUMN 01 
               VALUE  "   6 - APTO PADRAO INTERNACIONAL".
           05  LINE 20  COLUMN 41 
               VALUE  "      12 - PLANO GLOBAL PLATINIUM".
         
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       
       R0.
           OPEN I-O CADCONV
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCONV
                 CLOSE CADCONV
                 MOVE "*** ARQUIVO CADAMIGO FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADMIGO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.


       R1.    
           MOVE SPACES TO NOME PLANOTXT
           MOVE ZEROS TO CODIGO PLANO
           DISPLAY TELACONV.

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

       LER-CADCID.
           READ CADCONV
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                PERFORM R5
                DISPLAY TELACONV
                GO TO ACE-001
                
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADCONV" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.

       R3.
       
           MOVE SPACES TO NOME
           
           ACCEPT TNOME
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2. 

           IF NOME = SPACES 
              MOVE "DIGITE A DESCRICAO DA DOENCA" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.  
       
       R4.
           MOVE ZEROS TO PLANO
           MOVE SPACES TO PLANOTXT
           DISPLAY SMP003TELA.
           
           ACCEPT TPLANO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3. 

           IF PLANO = ZEROS
              MOVE "DIGITE UM PLANO VALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.    

       R5.
           IF PLANO < 13
              MOVE TBCONV(PLANO) TO PLANOTXT
              DISPLAY TPLANOTXT
           ELSE
              GO TO R4.
   

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R4.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCONV
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
                DELETE CADCONV RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO CARRO EXCLUIDO ***" TO MENS
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
                IF W-ACT = 01 GO TO R4.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCONV
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA ALTERACAO DO REGISTRO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       ROT-FIM.
           CLOSE CADCONV.
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
           