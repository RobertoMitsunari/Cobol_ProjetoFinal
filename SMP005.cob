       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRCEP.
      *AUTHOR. ROBERTO DA SILVA MITSUNARI.
      **************************************
      * MANUTENCAO DO CADASTRO DE CEP   *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS ENDERECO WITH DUPLICATES.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCEP       
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT". 
       01 REGCEP.
          03 CEP        PIC 9(08).
          03 ENDERECO         PIC X(30).
          03 BAIRRO           PIC X(20).
          03 CIDADE           PIC X(20).
          03 UF           PIC X(02).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       77 LIMPA         PIC X(50) VALUE SPACES. 
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 W-SEL         PIC 9(01) VALUE ZEROS.
       01 IND           PIC 9(02) VALUE ZEROS.
       01 ESTADCOMPL    PIC X(20) VALUE SPACES.


       01 TABUFFULL.
          03 FILLER     PIC X(22) VALUE "ACACRE".
          03 FILLER     PIC X(22) VALUE "ALALAGOAS".
          03 FILLER     PIC X(22) VALUE "APAMAPA".
          03 FILLER     PIC X(22) VALUE "AMAMAZONAS".
          03 FILLER     PIC X(22) VALUE "BABAHIA".
          03 FILLER     PIC X(22) VALUE "CECEARA".
          03 FILLER     PIC X(22) VALUE "DFDISTRITO FEDERAL".
          03 FILLER     PIC X(22) VALUE "ESESPIRITO SANTO".
          03 FILLER     PIC X(22) VALUE "GOGOIAS".
          03 FILLER     PIC X(22) VALUE "MAMARANHAO".
          03 FILLER     PIC X(22) VALUE "MTMATO GROSSO".
          03 FILLER     PIC X(22) VALUE "MSMATO GROSSO DO SUL".
          03 FILLER     PIC X(22) VALUE "MGMINAS GERAIS".
          03 FILLER     PIC X(22) VALUE "PAPARA".
          03 FILLER     PIC X(22) VALUE "PBPARAIBA".
          03 FILLER     PIC X(22) VALUE "PRPARANA".
          03 FILLER     PIC X(22) VALUE "PEPERNAMBUCO".
          03 FILLER     PIC X(22) VALUE "PIPIAUI".
          03 FILLER     PIC X(22) VALUE "RJRIO DE JANEIRO".
          03 FILLER     PIC X(22) VALUE "RNRIO GRANDE DO NORTE".
          03 FILLER     PIC X(22) VALUE "RSRIO GRANDE DO SUL".
          03 FILLER     PIC X(22) VALUE "RORONDONIA".
          03 FILLER     PIC X(22) VALUE "RRRORAIMA".
          03 FILLER     PIC X(22) VALUE "SCSANTA CATARINA".
          03 FILLER     PIC X(22) VALUE "SPSAO PAULO".
          03 FILLER     PIC X(22) VALUE "SESERGIPE".
          03 FILLER     PIC X(22) VALUE "TOTOCANTINS".         
       01 TABUF REDEFINES TABUFFULL.
          03 TBUF   PIC X(22) OCCURS 27 TIMES.
       01 TXTUF.
          03 TXTUFSIGLA PIC X(02) VALUE SPACES.
          03 TXTUFTEXTO PIC X(20) VALUE SPACES.

      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELACEP.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "                               REGISTRO".
           05  LINE 01  COLUMN 41 
               VALUE  "DE CEP".
           05  LINE 03  COLUMN 01 
               VALUE  "Codigo:".
           05  LINE 05  COLUMN 01 
               VALUE  "Endereco:".
           05  LINE 07  COLUMN 01 
               VALUE  "Bairro:".
           05  LINE 09  COLUMN 01 
               VALUE  "Cidade:".
           05  LINE 11  COLUMN 01 
               VALUE  "UF:".
           05  TCEP
               LINE 03  COLUMN 08  PIC 9(08)
               USING  CEP
               HIGHLIGHT.
           05  TENDERECO
               LINE 05  COLUMN 10  PIC X(30)
               USING  ENDERECO
               HIGHLIGHT.
           05  TBAIRRO
               LINE 07  COLUMN 08  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 09  COLUMN 08  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TSIGLAUF
               LINE 11  COLUMN 08  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TESTADCOMPL
               LINE 11  COLUMN 11  PIC X(20)
               USING  TXTUF
               HIGHLIGHT.

       01  TELAUF.
           05  LINE 07  COLUMN 51 
               VALUE  "         AC - ACRE".
           05  LINE 08  COLUMN 51 
               VALUE  "         AL - ALAGOAS".
           05  LINE 09  COLUMN 51 
               VALUE  "         AP - AMAPA". 
           05  LINE 10  COLUMN 51 
               VALUE  "         AM - AMAZONAS".      
           05  LINE 11  COLUMN 51 
               VALUE  "         BA - BAHIA".
           05  LINE 12  COLUMN 51 
               VALUE  "         CE - CEARA".
           05  LINE 13  COLUMN 51 
               VALUE  "         DF - DISTRITO FEDERAL". 
           05  LINE 14  COLUMN 51 
               VALUE  "         ES - ESPIRITO SANTO".
           05  LINE 15  COLUMN 51 
               VALUE  "         GO - GOIAS". 
           05  LINE 16  COLUMN 51 
               VALUE  "         MA - MARANHAO".
           05  LINE 17  COLUMN 51 
               VALUE  "         MT - MATO GROSSO".
           05  LINE 07  COLUMN 25 
               VALUE  "         MS - MATO GROSSO DO SUL". 
           05  LINE 08  COLUMN 25  
               VALUE  "         MG - MINAS GERAIS".      
           05  LINE 09  COLUMN 25  
               VALUE  "         PA - PARA".
           05  LINE 10  COLUMN 25  
               VALUE  "         PB - PARAIBA".
           05  LINE 11  COLUMN 25  
               VALUE  "         PR - PARANA". 
           05  LINE 12  COLUMN 25  
               VALUE  "         PE - PERNAMBUCO".
           05  LINE 13  COLUMN 25  
               VALUE  "         PI - PIAUI".         
           05  LINE 14  COLUMN 25  
               VALUE  "         RJ - RIO DE JANEIRO".
           05  LINE 15  COLUMN 25  
               VALUE  "         RN - RIO GRANDE DO NORTE".
           05  LINE 16  COLUMN 25  
               VALUE  "         RS - RIO GRANDE DO SUL". 
           05  LINE 17  COLUMN 25  
               VALUE  "         RO - RONDONIA".      
           05  LINE 18  COLUMN 25  
               VALUE  "         RR - RORAIMA".
           05  LINE 19  COLUMN 25  
               VALUE  "         SC - SANTA CATARINA".
           05  LINE 20  COLUMN 25  
               VALUE  "         SP - SAO PAULO". 
           05  LINE 21  COLUMN 25  
               VALUE  "         SE - SERGIPE".
           05  LINE 22  COLUMN 25  
               VALUE  "         TO - TOCANTINS".     
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.

       R0.
           OPEN I-O CADCEP
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCEP
                 CLOSE CADCEP
                 MOVE "*** ARQUIVO CADCEP FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADCEP" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.


       R1.
           MOVE SPACES TO ENDERECO BAIRRO CIDADE UF TXTUF ESTADCOMPL
           MOVE ZEROS TO CEP
           DISPLAY TELACEP.

       R2.
           ACCEPT TCEP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           IF CEP = 0
              MOVE "*** DIGITE UM CODIGO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2.

       LER-CADCEP.
           READ CADCEP
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                PERFORM R6A
                DISPLAY TELACEP
                GO TO ACE-001
                
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADCEP" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.

       R3. 
           DISPLAY TELACEP       
           ACCEPT TENDERECO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2. 
           IF ENDERECO = SPACES 
              MOVE "DIGITE O ENDERECO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.
       R4.   
           DISPLAY TELACEP       
           ACCEPT TBAIRRO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3. 
           IF BAIRRO = SPACES 
              MOVE "DIGITE O BAIRRO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.
           
       R5.    
           DISPLAY TELACEP    
           ACCEPT TCIDADE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4. 
           IF CIDADE = SPACES 
              MOVE "*** DIGITE A CIDADE ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5. 
       
       R6.     
           DISPLAY TELAUF     
           MOVE 1 TO IND 

           ACCEPT TSIGLAUF
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5. 
           IF UF = SPACES 
              MOVE "*** ESCOLHA UM UF ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6. 
       R6A.    
           MOVE TBUF(IND) TO TXTUF
           IF TXTUFSIGLA NOT = UF
              ADD 1 TO IND
              IF IND < 28
                 GO TO R6A
              ELSE
                 MOVE "*** CODIGO UF INCORRETO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R6 
           ELSE
                MOVE TXTUFTEXTO TO TXTUF 
                DISPLAY TESTADCOMPL.

      *------------------------- SALVAR --------------------------------

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R6.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  
                  GO TO ALT-RW1
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DO CARRO"
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
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R1  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3.
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
                DELETE CADCEP RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO CARRO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R6.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA ALTERACAO DO REGISTRO CARRO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.


       ROT-FIM.
           CLOSE CADCEP.
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