       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP011.
       AUTHOR. Roberto da Silva Mitsunari
      ************************
      * RELATORIO DE CONV'S   *
      ************************
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
           SELECT CADCONVTX ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS  IS ST-ERRO.
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
      *
       FD CADCONVTX
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONVPRL.DOC".
       01 REGCONVTX    PIC X(100).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL      PIC 9(01) VALUE ZEROS.
       77 W-CONT     PIC 9(06) VALUE ZEROS.
       77 W-OPCAO    PIC X(01) VALUE SPACES.
       77 ST-ERRO    PIC X(02) VALUE "00". 
       77 W-ACT      PIC 9(02) VALUE ZEROS.
       77 MENS       PIC X(50) VALUE SPACES.
       77 LIMPA      PIC X(50) VALUE SPACES.

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
      *
       01 CONVTX.
                03 CODIGOTX            PIC 9(04).
                03 FILLER              PIC X(03) VALUE "   ".
                03 NOMETX              PIC X(30).
                03 FILLER              PIC X(01) VALUE " ".
                03 PLANOTX             PIC 9(02).
                03 FILLER              PIC X(04) VALUE "    ".
                03 PLANOTXTTX          PIC X(30).
       01 CAB0.
                03 FILLER      PIC X(20) VALUE "FATEC ZL".
                03 FILLER      PIC X(30) VALUE
                        "   *** RELACAO DE CONVENIO ***".

       01 CAB1.
                03 CAB-CODIGO         PIC X(06) VALUE "Codigo".
                03 FILLER             PIC X(01) VALUE " ".
                03 CAB-NOME           PIC X(30) VALUE "Nome".
                03 FILLER             PIC X(01) VALUE " ".
                03 CAB-PLANO          PIC X(05) VALUE "Plano".
                03 FILLER             PIC X(01) VALUE " ".
                03 CAB-PLANOTXTTX     PIC X(30) VALUE "Nome Do Plano".

       01 CAB2.
                03 CAB-CODIGO        PIC X(06) VALUE "------".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-NOME          PIC X(30) VALUE "----------------".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-PLANO         PIC X(05) VALUE "-----".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-PLANOTXTTX    PIC X(30) VALUE "--------------".


      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  SMP011.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                *** CONVERSAO DE ARQUIVO".
           05  LINE 02  COLUMN 41 
               VALUE  " DE CONV PARA TXT ***".
           05  LINE 12  COLUMN 01 
               VALUE  "                         CONFIRMA IMPRES".
           05  LINE 12  COLUMN 41 
               VALUE  "SAO (S ou N ) :".
           05  LINE 24  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TW-OPCAO
               LINE 12  COLUMN 57  PIC X(01)
               USING  W-OPCAO
               HIGHLIGHT.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO. 
      *
       INC-001.
                MOVE SPACES TO NOME PLANOTXT.
                MOVE ZEROS TO CODIGO PLANO. 
                DISPLAY SMP011.
       INC-OPC.
                ACCEPT TW-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
      *
       INC-OP0.
           OPEN INPUT CADCONV
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      MOVE "* ARQUIVO CONV NAO EXISTE *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CADCONV" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
      *
           OPEN OUTPUT CADCONVTX
           IF ST-ERRO NOT = "00"
                      MOVE "ERRO ABERTURA DO ARQUIVO CADCONVTX" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
       ROT-CABEC.
           WRITE REGCONVTX FROM CAB0.
           MOVE SPACES TO REGCONVTX
           WRITE REGCONVTX.
           WRITE REGCONVTX FROM CAB1.
           WRITE REGCONVTX FROM CAB2.
      *
       LER-CEP01.
                READ CADCONV NEXT
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "10"
                      MOVE "*** FIM DO CADCONV ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA LEITURA CADCONV"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
           MOVE CODIGO TO CODIGOTX.
           MOVE NOME  TO NOMETX.
           MOVE PLANO TO PLANOTX.
           PERFORM R5
           MOVE PLANOTXT TO PLANOTXTTX.
           MOVE CONVTX  TO REGCONVTX.

       INC-WR1.
                WRITE REGCONVTX
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO LER-CEP01
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO CADCONVTX"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.


       R5.
           MOVE TBCONV(PLANO) TO PLANOTXT.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                CLOSE CADCONV CADCONVTX.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
