       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP010.
       AUTHOR. Roberto da Silva Mitsunari
      ************************
      * RELATORIO DE CID'S   *
      ************************
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
           SELECT CADCIDTX ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS  IS ST-ERRO.
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
      *
       FD CADCIDTX
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCIDPRL.DOC".
       01 REGCIDTX    PIC X(100).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL      PIC 9(01) VALUE ZEROS.
       77 W-CONT     PIC 9(06) VALUE ZEROS.
       77 W-OPCAO    PIC X(01) VALUE SPACES.
       77 ST-ERRO    PIC X(02) VALUE "00". 
       77 W-ACT      PIC 9(02) VALUE ZEROS.
       77 MENS       PIC X(50) VALUE SPACES.
       77 LIMPA      PIC X(50) VALUE SPACES.
      *
       01 CIDTX.
                03 CODIGOTX            PIC 9(04).
                03 FILLER              PIC X(03) VALUE "   ".
                03 DENOMINACAOTX       PIC X(30).
                03 FILLER              PIC X(01) VALUE " ".
                03 SINTOMASTX          PIC X(50).
                03 FILLER              PIC X(01) VALUE " ".
                03 TRATAMENTOTX        PIC X(50).
       01 CAB0.
                03 FILLER      PIC X(20) VALUE "FATEC ZL".
                03 FILLER      PIC X(30) VALUE
                        "   *** RELACAO DE DOENCA ***".

       01 CAB1.
                03 CAB-CODIGO         PIC X(06) VALUE "Codigo".
                03 FILLER             PIC X(01) VALUE " ".
                03 CAB-DENOMINA       PIC X(30) VALUE "Denominacao".
                03 FILLER             PIC X(01) VALUE " ".
                03 CAB-SINTOMAS       PIC X(50) VALUE "Sintomas".
                03 FILLER             PIC X(01) VALUE " ".
                03 CAB-TRATAMENTO     PIC X(50) VALUE "Tratamento".

       01 CAB2.
                03 CAB-CODIGO        PIC X(04) VALUE "----".
                03 FILLER            PIC X(03) VALUE "   ".
                03 CAB-DENOMINA      PIC X(30) VALUE "----------------".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-SINTOMAS      PIC X(50) VALUE "----------------".
                03 FILLER            PIC X(01) VALUE " ".
                03 CAB-TRATAMENTO    PIC X(50) VALUE "--------------".


      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  SMP010.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                *** CONVERSAO DE ARQUIVO".
           05  LINE 02  COLUMN 41 
               VALUE  " DE CID PARA TXT ***".
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
                MOVE SPACES TO DENOMINACAO SINTOMAS TRATAMENTO.
                MOVE ZEROS TO CODIGO. 
                DISPLAY SMP010.
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
           OPEN INPUT CADCID
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      MOVE "* ARQUIVO CID NAO EXISTE *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CID" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
      *
           OPEN OUTPUT CADCIDTX
           IF ST-ERRO NOT = "00"
                      MOVE "ERRO ABERTURA DO ARQUIVO CADCIDTX" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
       ROT-CABEC.
           WRITE REGCIDTX FROM CAB0.
           MOVE SPACES TO REGCIDTX
           WRITE REGCIDTX.
           WRITE REGCIDTX FROM CAB1.
           WRITE REGCIDTX FROM CAB2.
      *
       LER-CEP01.
                READ CADCID NEXT
                IF ST-ERRO NOT = "00"
                   IF ST-ERRO = "10"
                      MOVE "*** FIM DO CADCID ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA LEITURA CADCID"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
           MOVE CODIGO TO CODIGOTX.
           MOVE DENOMINACAO  TO DENOMINACAOTX.
           MOVE SINTOMAS TO SINTOMASTX.
           MOVE TRATAMENTO TO TRATAMENTOTX.
           MOVE CIDTX      TO REGCIDTX.

       INC-WR1.
                WRITE REGCIDTX
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO LER-CEP01
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO CADCIDTX"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                CLOSE CADCID CADCIDTX.
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
