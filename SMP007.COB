       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP007.
      *AUTHOR. Roberto da Silva Mitsunari.
      **************************************************
      * CONSULTA DE CONVENIO *
      **************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
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
      *
       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT". 
       01 REGCONV.
          03 CODIGO       PIC 9(04).
          03 NOME         PIC X(30).
          03 PLANO        PIC 9(02).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-SEL            PIC 9(01) VALUE ZEROS.
       01 W-CONT           PIC 9(04) VALUE ZEROS.
       01 W-OPCAO          PIC X(01) VALUE SPACES.
       01 ST-ERRO          PIC X(02) VALUE "00".
       01 W-ACT            PIC 9(02) VALUE ZEROS.
       01 MENS             PIC X(50) VALUE SPACES.
       01 LIMPA            PIC X(55) VALUE SPACES.
       01 SOLIC            PIC X(20) VALUE SPACES.
       01 CONLIN           PIC 9(03) VALUE 001.
       01 CEPENTR          PIC 9(09) VALUE ZEROS.
       01 LOGRENTR         PIC X(30) VALUE SPACES.
       01 IND              PIC 9(05) VALUE ZEROS.
       01 TABCEP.
          03 TBCOD         PIC 9(08) OCCURS 1000 TIMES.

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
      *
      ******************
      * ROTINA DE TELA *
      ******************
      *
       SCREEN SECTION.
       01  TELACONV.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "--------------------------->CADASTRO DE".
           05  LINE 02  COLUMN 41 
               VALUE  " CONVENIO<------------------------------".
           05  LINE 05  COLUMN 01 
               VALUE  "     Codigo:".
           05  LINE 08  COLUMN 01 
               VALUE  "     Nome:".
           05  LINE 11  COLUMN 01 
               VALUE  "     Plano:".
           05  LINE 21  COLUMN 01 
               VALUE  "              OPCAO :   (E=ENCERRA     P".
           05  LINE 21  COLUMN 41 
               VALUE  "=PROXIMO     A=ANTERIOR )".
           05  LINE 22  COLUMN 41 
               VALUE  "   ==>            <==".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TCOD
               LINE 05  COLUMN 14  PIC 9(04)
               USING  CODIGO
               HIGHLIGHT.
           05  TNOME
               LINE 08  COLUMN 12  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TPLANO
               LINE 11  COLUMN 13  PIC 9(02)
               USING  PLANO
               HIGHLIGHT.
           05  TPLANOTXT
               LINE 11  COLUMN 17  PIC X(30)
               USING  PLANOTXT
               HIGHLIGHT.
           05  TW-OPCAO
               LINE 21  COLUMN 23  PIC X(01)
               USING  W-OPCAO
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT  CADCONV
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADCONV NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO CADCONV"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.
           MOVE 1 TO IND
           MOVE ZEROS TO CODIGO.
           START CADCONV KEY IS NOT LESS CODIGO INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM.

       LER-CEP.
           
           READ CADCONV NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADCONV"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              MOVE CODIGO TO TBCOD(IND)
              ADD 1 TO IND
              IF IND > 1000
                 MOVE "*** TABELA ESTOURADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001
              ELSE
                 GO TO LER-CEP.

      *
       INC-001.
           MOVE ZEROS TO CODIGO, PLANO
           MOVE SPACES TO NOME 
           DISPLAY  TELACONV.
       INC-001A.
           ACCEPT TCOD
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE CADCONV
                      GO TO ROT-FIM.
           IF W-ACT > 02
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM   
                      GO TO INC-001A.
           MOVE 1 TO IND.
       R111.
           IF TBCOD(IND) < CODIGO
              ADD 1 TO IND
              IF IND >1000
                 MOVE "*** CEP NAO ENCONTRADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001A
              ELSE
                 GO TO R111.
       R112.

           MOVE TBCOD(IND) TO CODIGO
           MOVE TBCONV(PLANO) TO PLANOTXT
           DISPLAY TPLANOTXT.

       INC-RD2.
           READ CADCONV
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
                 MOVE "*** REGISTRO NAO ENCONTRADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADCONV"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
       ROT-MONTAR.
              
              DISPLAY TELACONV.
              MOVE TBCONV(PLANO) TO PLANOTXT
              DISPLAY TPLANOTXT.
       ROT-SOL.

           ACCEPT  TW-OPCAO  
           IF W-OPCAO = "E"
                  GO TO ROT-FIM 
           ELSE
             IF W-OPCAO = "P"
                   ADD 1 TO IND
                   GO TO R112
             ELSE
                IF W-OPCAO = "A"
                   ADD -1 TO IND
                   GO TO R112
                ELSE
                
                     MOVE "*** OPCAO NAO DISPONIVEL ***" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO ROT-SOL.





       ROT-PLANO.

           
           



       ROT-FIM-PLANO.

      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.

           CLOSE CADCONV.
       ROT-FIM2.
           EXIT PROGRAM.
       ROT-FIM3.
           STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23, 13) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 1000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-MENS.
      *

      *---------------------*** FIM DE PROGRAMA ***--------------------*