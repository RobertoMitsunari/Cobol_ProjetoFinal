       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP004.
      *AUTHOR. Roberto da Silva Mitsunari.
      *******************************************
      *    MANUTENCAO DO CADASTRO DE PACIENTE   *
      *******************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADPACI ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CPF
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME WITH DUPLICATES.
       
           SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS ENDERECO
                                   WITH DUPLICATES.

           SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CNOME WITH DUPLICATES
                    ALTERNATE RECORD KEY IS PLANO WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADPACI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADPACI.DAT". 
       01 REGPACI.
          03 CPF            PIC 9(11).
          03 NOME           PIC X(30).
          03 NASCIMENTO.
             05 DIA         PIC 9(02).
             05 MES         PIC 9(02).
             05 ANO         PIC 9(04).
          03 SEXO           PIC X(01).
          03 GENERO         PIC X(01).  
          03 TELEFONE.       
             05 DDD         PIC 9(02).
             05 NUM         PIC 9(09).
          03 EMAIL          PIC X(40).
          03 CEPPACI        PIC 9(08).
          03 NUMLOGRA       PIC 9(04).
          03 COMLOGRA       PIC X(12).
          03 CONVENIO       PIC 9(04).


       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
          03 CEP           PIC 9(08).
          03 ENDERECO      PIC X(30).
          03 BAIRRO        PIC X(20).
          03 CIDADE        PIC X(20).
          03 UF            PIC X(02).

       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT". 
       01 REGCONV.
          03 CODIGO       PIC 9(04).
          03 CNOME        PIC X(30).
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
       01 BI            PIC 9(01) VALUE ZEROS.

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

       01 TABUFX.
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
       01 TABUF REDEFINES TABUFX.
          03 TBUF   PIC X(22) OCCURS 27 TIMES.
       01 TXTUF.
          03 TXTUFCOD PIC X(02) VALUE SPACES.
          03 TXTUFTEXTO PIC X(20) VALUE SPACES.   

       01 TABGENEROX.
          03 FILLER     PIC X(15) VALUE "NNAO DECLARADO".
          03 FILLER     PIC X(15) VALUE "HHETEROSEXUAL".
          03 FILLER     PIC X(15) VALUE "BBISEXUAL".
          03 FILLER     PIC X(15) VALUE "TRANSEXUAL".
          03 FILLER     PIC X(15) VALUE "OOUTROS".
       01 TABGENERO REDEFINES TABGENEROX.
          03 TBGENERO   PIC X(15) OCCURS 5 TIMES.
       01 TXTGENERO.
          03 TXTCOD PIC X(01) VALUE SPACES.
          03 TXTTEXT PIC X(14) VALUE SPACES. 

       01 LIXO PIC 9(03) VALUE ZEROS.

      *-----------------------------------------------------------------
       SCREEN SECTION.
    
       01  TELAPACIENTE.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "-------------------------->CADASTRO DE P".
           05  LINE 01  COLUMN 41 
               VALUE  "ACIENTES<-------------------------------".
           05  LINE 03  COLUMN 01 
               VALUE  " Cpf:             Nome:".
           05  LINE 03  COLUMN 41 
               VALUE  "                  Nascimento:".
           05  LINE 05  COLUMN 01 
               VALUE  " Sexo:               Genero:".
           05  LINE 05  COLUMN 51 
               VALUE  "        Telefone: (  ) ".
           05  LINE 07  COLUMN 01 
               VALUE  " Email:".
           05  LINE 09  COLUMN 01 
               VALUE  "------------------------------->ENDERECO".
           05  LINE 09  COLUMN 41 
               VALUE  "<---------------------------------------".
           05  LINE 11  COLUMN 01 
               VALUE  " Cep:            Logradouro:".
           05  LINE 11  COLUMN 70 
               VALUE  " Num:".   
           05  LINE 13  COLUMN 01 
               VALUE  " Bairro:                      Cidade:".
           05  LINE 15  COLUMN 01 
               VALUE  " Unidade Da Federacao:".
           05  LINE 15  COLUMN 50 
               VALUE  " Complemento:".
           05  LINE 17  COLUMN 01 
               VALUE  "------------------------------->CONVENIO".
           05  LINE 17  COLUMN 41 
               VALUE  "<---------------------------------------".
           05  LINE 19  COLUMN 01 
               VALUE  " Codigo:       Nome:".
           05  LINE 21  COLUMN 01 
               VALUE  " Plano:".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM:".
           05  TCPF
               LINE 03  COLUMN 07  PIC 9(11)
               USING  CPF
               HIGHLIGHT.
           05  TNOME
               LINE 03  COLUMN 25  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TNASCIMENTO
               LINE 03  COLUMN 71  PIC 99/99/9999
               USING  NASCIMENTO
               HIGHLIGHT.
           05  TSEXO
               LINE 05  COLUMN 08  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  TSEXOTXT
               LINE 05  COLUMN 10  PIC X(10)
               USING  TEXSEXO
               HIGHLIGHT.
           05  TGENERO
               LINE 05  COLUMN 30  PIC X(01)
               USING  GENERO
               HIGHLIGHT.
           05  TTXTGENERO
               LINE 05  COLUMN 32  PIC X(14)
               USING  TXTGENERO
               HIGHLIGHT.
           05  TDDD
               LINE 05  COLUMN 68  PIC 99
               USING  DDD
               HIGHLIGHT.
           05  TNUM
               LINE 05  COLUMN 72  PIC 9(09)
               USING  NUM
               HIGHLIGHT.
           05  TEMAIL
               LINE 07  COLUMN 09  PIC X(40)
               USING  EMAIL
               HIGHLIGHT.
           05  TCEP
               LINE 11  COLUMN 07  PIC 9(08)
               USING  CEP
               HIGHLIGHT.
           05  TLOGRA
               LINE 11  COLUMN 30  PIC X(40)
               USING  ENDERECO
               HIGHLIGHT.
           05  TNUMLOGRA
               LINE 11  COLUMN 76  PIC 9(04)
               USING  NUMLOGRA
               HIGHLIGHT. 
           05  TBAIRRO
               LINE 13  COLUMN 10  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 13  COLUMN 39  PIC X(30)
               USING  CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 15  COLUMN 24  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TCOMPLEMENTO
               LINE 15  COLUMN 64  PIC X(12)
               USING  COMLOGRA
               HIGHLIGHT.
           05  TTUF
               LINE 15  COLUMN 27  PIC X(20)
               USING  TXTUF
               HIGHLIGHT.
           05  TCODIGO
               LINE 19  COLUMN 10  PIC 9(04)
               USING  CODIGO
               HIGHLIGHT.
           05  TCNOME
               LINE 19  COLUMN 22  PIC X(30)
               USING  CNOME
               HIGHLIGHT.
           05  TPLANO
               LINE 21  COLUMN 09  PIC 9(02)
               USING  PLANO
               HIGHLIGHT.
           05  TXTPLANO
               LINE 21  COLUMN 12  PIC X(30)
               USING  PLANOTXT
               HIGHLIGHT.

       
         
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       
       R0.
           OPEN I-O CADPACI
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADPACI
                 CLOSE CADPACI
                 MOVE "*** ARQUIVO CADPACI FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADMIGO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      * 
       R0A.
           OPEN INPUT CADCEP
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CEP NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CEP " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       R0B.
           OPEN INPUT CADCONV
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CADCONV NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CEP " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.


       R1.    
           MOVE SPACES TO NOME SEXO GENERO EMAIL TXTGENERO TEXSEXO
           MOVE ZEROS TO  NASCIMENTO CPF NUM DDD MES ANO DIA
           MOVE SPACES TO ENDERECO BAIRRO CIDADE UF TXTUF COMLOGRA
           MOVE ZEROS TO CEP CPF NUMLOGRA IND
           MOVE SPACES TO CNOME PLANOTXT TXTUFCOD TXTUFTEXTO COMLOGRA
           MOVE ZEROS TO CODIGO PLANO CEPPACI
           DISPLAY TELAPACIENTE. 


       R2.
           
           MOVE ZEROS TO CPF
           
           ACCEPT TCPF
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.

           IF CPF = ZEROS
                MOVE "CPF INVALIDO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R2.

        
      *---------------VERIFICA SE JÁ EXISTE NO ARQUIVO-----------------*

       LER-CADPACI.
           READ CADPACI
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                MOVE CONVENIO TO CODIGO
                MOVE CEPPACI TO CEP
                PERFORM R5A
                PERFORM R6A
                PERFORM R9A
                PERFORM R12A
                PERFORM R9B
                PERFORM R9C
                PERFORM R13
                DISPLAY TELAPACIENTE
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADPACI" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.

      *----------------------------------------------------------------*

       R3.
           MOVE SPACES TO NOME
           
           ACCEPT TNOME
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2. 

           IF NOME = SPACES 
              MOVE "DIGITE UM NOME VALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.  
       
       R4.
           MOVE ZEROS TO NASCIMENTO
           
           ACCEPT TNASCIMENTO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.  

           IF NASCIMENTO = SPACES 
              MOVE "DIGITE UM NASCIMENTO VALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.

           IF ANO < 1 OR ANO > 2020
              MOVE "DIGITE UM NASCIMENTO VALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.

           IF MES < 1 OR MES > 12
              MOVE "DIGITE UM NASCIMENTO VALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.
           

           IF MES = 04 OR MES = 06 OR MES = 09 OR MES = 11

                IF DIA > 30 OR DIA < 1
                   MOVE "DIGITE UM NASCIMENTO VALIDO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R4.

               
           IF MES = 02

                 DIVIDE ANO BY 4 GIVING LIXO REMAINDER BI
                 
                 IF BI = 0

                    PERFORM R4A
                    
                 ELSE 

                    PERFORM R4B

           
           IF DIA > 31 OR DIA < 1
                MOVE "DIGITE UM NASCIMENTO VALIDO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R4.  

       R5.
           ACCEPT TSEXO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
       R5A. 

           IF SEXO = "F" 
              MOVE "FEMINIMO" TO TEXSEXO
           ELSE
              IF SEXO = "M"
                 MOVE "MASCULINO" TO TEXSEXO
              ELSE
                 IF SEXO = "I"
                    MOVE "INDEFINIDO" TO TEXSEXO
                 ELSE
                    MOVE "*DIGITE M - MASCULINO E F - FEMININO*" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO R5.

           DISPLAY TELAPACIENTE.
        R6.      
           MOVE 1 TO IND
           ACCEPT TGENERO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
       R6A.
           MOVE TBGENERO(IND) TO TXTGENERO
           IF TXTCOD NOT = GENERO
              ADD 1 TO IND
              IF IND < 10
                 GO TO R6A
              ELSE
                 MOVE "*** TIPO GENERO INCORRETO***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R6
           ELSE
               MOVE TXTTEXT TO TXTGENERO
               DISPLAY TTXTGENERO.

       
       R7A.
           MOVE ZEROS TO DDD
           
           ACCEPT TDDD
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6. 

           MOVE ZEROS TO NUM
           
           ACCEPT TNUM
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6. 

           IF NUM = ZEROS OR DDD = ZEROS 
              MOVE "DIGITE UM TELEFONE VALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R7A.

       R8.
           MOVE SPACES TO EMAIL
           
           ACCEPT TEMAIL
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R7A. 

           IF EMAIL = SPACES 
              MOVE "DIGITE UM EMAIL VALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R8.

       R9.
           MOVE ZEROS TO CEP

           ACCEPT TCEP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                 DISPLAY TELAPACIENTE
                 GO TO R8.

           IF CEP = 0 
                 MOVE "*** CEP NAO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R9.

       R9A.

           READ CADCEP
           IF ST-ERRO = "23"
                   MOVE "*** CEP NAO EXISTE ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R9
           
           IF ST-ERRO NOT = "00"
                  MOVE "ERRO NA LEITURA ARQUIVO CADASTRO DE CEP" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM.
 
       
       R9B.

           MOVE 1 TO IND.

       R9C. 

           MOVE TBUF(IND) TO TXTUF
           IF TXTUFCOD NOT = UF
              ADD 1 TO IND
              IF IND < 28
                 GO TO R9C
              ELSE
                 MOVE "*** CEP UF INCORRETO***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R9 
           ELSE
                MOVE TXTUFTEXTO TO TXTUF
                DISPLAY TTUF.
                DISPLAY TELAPACIENTE. 

           MOVE CEP TO CEPPACI.
           

       R10.

           MOVE ZEROS TO NUMLOGRA
           
           ACCEPT TNUMLOGRA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R9. 

           IF NUMLOGRA = ZEROS 
              MOVE "DIGITE UM NUMERO VALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R10.

       R11.

           MOVE SPACES TO COMLOGRA
           
           ACCEPT TCOMPLEMENTO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R10.    
           
           IF COMLOGRA = SPACES
              MOVE "DIGITE UM COMPLEMENTO VALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R11.

       R12.

           MOVE ZEROS TO CODIGO
           MOVE SPACES TO PLANOTXT

           ACCEPT TCODIGO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                 DISPLAY TELAPACIENTE
                 GO TO R11.

           IF CODIGO = ZEROS
                 MOVE "*** CODIGO NAO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R12.

       R12A.
           READ CADCONV
           IF ST-ERRO = "23"
                   MOVE "*** CODIGO NAO EXISTE ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R12.
           IF ST-ERRO NOT = "00"
                  MOVE "ERRO NA LEITURA ARQUIVO CADASTRO CONVENIOS" 
                      TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM.

           MOVE CODIGO TO CONVENIO.

       R13. 

           MOVE TBCONV(PLANO) TO PLANOTXT
           DISPLAY TELAPACIENTE.
   

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R12.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGPACI
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

      ******************************************************************

       R4A.

               IF DIA > 29 OR DIA < 1
                  MOVE "DIGITE UM NASCIMENTO VALIDO" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R4.


       R4A-FIM.

       R4B.

               IF DIA > 28 OR DIA < 1
                  MOVE "DIGITE UM NASCIMENTO VALIDO" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R4.


       R4B-FIM.

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
                DELETE CADPACI RECORD
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
                REWRITE REGPACI
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA ALTERACAO DO REGISTRO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       ROT-FIM.
           CLOSE CADPACI
           CLOSE CADCEP
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