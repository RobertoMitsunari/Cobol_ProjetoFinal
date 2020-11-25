       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP006.
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

           SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CNOME WITH DUPLICATES
                    ALTERNATE RECORD KEY IS PLANO WITH DUPLICATES.

           SELECT CADMED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CRM
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME WITH DUPLICATES.

           SELECT CADCID ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CID
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS DENOMINACAO WITH DUPLICATES.

           SELECT CADCONS ASSIGN TO DISK 
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS KEYPRINCIPAL
                    FILE STATUS  IS ST-ERRO.

      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCONS
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONS.DAT". 
       01 REGCONS.
          03 KEYPRINCIPAL.
             05 PACI-CPF              PIC 9(11).       
             05 DATACONSULTA.          
                07 DIACONS            PIC 9(02).
                07 MESCONS            PIC 9(02).
                07 ANOCONS            PIC 9(04).
          03 CODMEDICO                PIC 9(06).
          03 CODIGO-DOENCA            PIC 9(04).
          03 DESCRICAO1               PIC X(60).
          03 DESCRICAO2               PIC X(60).

       
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

       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT". 
       01 REGCONV.
          03 CODIGO       PIC 9(04).
          03 CNOME        PIC X(30).
          03 PLANO        PIC 9(02).


       FD CADMED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMED.DAT". 
       01 REGMED.
          03 CRM           PIC 9(06).
          03 NOMEMED          PIC X(30).
          03 ESPEC         PIC 9(02).
          03 SEXOMED          PIC X(01).
          03 DATANASC.
             05 DIANASC        PIC 99.
             05 MESNASC        PIC 99.
             05 ANONASC        PIC 9(04).
          03 EMAIL         PIC X(30).
          03 TELEFONE.
             05 DDD         PIC 9(02).
             05 NUM         PIC 9(09).  

       FD CADCID
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCID.DAT". 
       01 REGCID.
          03 CID               PIC 9(04).
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
       01 BI            PIC 9(01) VALUE ZEROS.
       01 TXTSEXOMED    PIC X(12) VALUE SPACES.
       01 ESPECTXT      PIC X(14) VALUE SPACES.

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

       01 TABESP.
          03 COLESP    PIC X(30) OCCURS 6 TIMES.

      *-----------------------------------------------------------------
       SCREEN SECTION.
    
       01  TELASMP006.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                            Cadastro De".
           05  LINE 03  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 03  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 02  COLUMN 41 
               VALUE  "Consultas".
           05  LINE 04  COLUMN 01 
               VALUE  " Paciente :                           Da".
           05  LINE 04  COLUMN 41 
               VALUE  "ta Consulta :".
           05  LINE 05  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 05  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 06  COLUMN 01 
               VALUE  " Nome :".
           05  LINE 07  COLUMN 01 
               VALUE  " Data Nascimento :   .  .       Sexo :".
           05  LINE 07  COLUMN 41 
               VALUE  "-             Genero :  -".
           05  LINE 09  COLUMN 01 
               VALUE  " Convenio :      -".
           05  LINE 10  COLUMN 01 
               VALUE  " Plano    :      -".
           05  LINE 11  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 11  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 12  COLUMN 01 
               VALUE  " Medico".
           05  LINE 13  COLUMN 01 
               VALUE  " CRM :          Nome :".
           05  LINE 13  COLUMN 41 
               VALUE  "                Sexo :  -".
           05  LINE 14  COLUMN 01 
               VALUE  " Especialidade :".
           05  LINE 15  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 15  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 16  COLUMN 01 
               VALUE  " Consulta".
           05  LINE 17  COLUMN 01 
               VALUE  " CID :         Denominacao :".
           05  LINE 18  COLUMN 01 
               VALUE  " Descricao 1 :".
           05  LINE 19  COLUMN 01 
               VALUE  " Descricao 2 :".
           05  LINE 21  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 21  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM:".
           05  TCPF
               LINE 04  COLUMN 13  PIC 999.999.999.99
               USING  PACI-CPF
               HIGHLIGHT.
           05  TDATACONSULTA 
               LINE 04  COLUMN 55  PIC 99.99.9999
               USING  DATACONSULTA 
               HIGHLIGHT.
           05  TNOME
               LINE 06  COLUMN 09  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TNASCIMENTO
               LINE 07  COLUMN 20  PIC 99/99/9999
               USING  NASCIMENTO
               HIGHLIGHT.
           05  TSEXO
               LINE 07  COLUMN 40  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  TSEXOTXT
               LINE 07  COLUMN 42  PIC X(10)
               USING  TEXSEXO
               HIGHLIGHT.
           05  TGENERO
               LINE 07  COLUMN 64  PIC X(01)
               USING  GENERO
               HIGHLIGHT.
           05  TTXTGENERO
               LINE 07  COLUMN 66  PIC X(14)
               USING  TXTGENERO
               HIGHLIGHT.
           05  TCONVENIO
               LINE 09  COLUMN 13  PIC 9(04)
               USING  CONVENIO
               HIGHLIGHT.
           05  TCNOME
               LINE 09  COLUMN 20  PIC X(30)
               USING  CNOME
               HIGHLIGHT.
           05  TPLANO
               LINE 10  COLUMN 15  PIC 9(02)
               USING  PLANO
               HIGHLIGHT.
           05  TXTPLANO
               LINE 10  COLUMN 20  PIC X(30)
               USING  PLANOTXT
               HIGHLIGHT.
           05  TCRM
               LINE 13  COLUMN 08  PIC 999.999
               USING  CODMEDICO
               HIGHLIGHT.
           05  TNOMEMED
               LINE 13  COLUMN 24  PIC X(30)
               USING  NOMEMED
               HIGHLIGHT.
           05  TSEXOMED
               LINE 13  COLUMN 64  PIC X(01)
               USING  SEXOMED
               HIGHLIGHT.
           05  TSEXOMEDTXT
               LINE 13  COLUMN 66  PIC X(10)
               USING  TXTSEXOMED
               HIGHLIGHT.
           05  TESPEC
               LINE 14  COLUMN 18  PIC 99
               USING  ESPEC
               HIGHLIGHT.
           05  TESPECTXT
               LINE 14  COLUMN 21  PIC X(20)
               USING  ESPECTXT
               HIGHLIGHT.
           05  TCID
               LINE 17  COLUMN 08  PIC 9(04)
               USING  CODIGO-DOENCA
               HIGHLIGHT.
           05  TDENOMINACAO
               LINE 17  COLUMN 30  PIC X(30)
               USING  DENOMINACAO
               HIGHLIGHT.
           05  TDESCRICAO1
               LINE 18  COLUMN 16  PIC X(60)
               USING  DESCRICAO1
               HIGHLIGHT.
           05  TDESCRICAO2
               LINE 19  COLUMN 16  PIC X(60)
               USING  DESCRICAO2
               HIGHLIGHT.

       
         
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE "CLINICA MEDICA" TO COLESP(1)
           MOVE "UROLOGIA" TO COLESP(2)
           MOVE "GINECOLOGIA" TO COLESP(3)
           MOVE "PEDIATRIA" TO COLESP(4)
           MOVE "CARDIOLOGIA" TO COLESP(5)
           MOVE "OFTALMOLOGIA" TO COLESP(6).
       
       R0.
           OPEN I-O CADCONS
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCONS
                 CLOSE CADCONS
                 MOVE "*** ARQUIVO CADCONS FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADCONS" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      * 
       R0A.
           OPEN INPUT CADMED
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO CADMED NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADMED " TO MENS
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

       R0C.
           OPEN INPUT CADPACI
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

       R0D.
           OPEN INPUT CADCID
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
           MOVE SPACES TO NOME SEXO GENERO COMLOGRA
           MOVE ZEROS TO PACI-CPF DIA MES ANO
           MOVE ZEROS TO CONVENIO
           MOVE SPACES TO CNOME 
           MOVE ZEROS TO CODIGO PLANO
           MOVE SPACES TO NOME SEXO TXTSEXOMED
           MOVE ZEROS TO CRM ESPEC 
           MOVE SPACES TO DENOMINACAO SINTOMAS TRATAMENTO
           MOVE ZEROS TO CID
           MOVE ZEROS TO CPF DATACONSULTA CODMEDICO 
           MOVE ZEROS TO CODIGO-DOENCA


           DISPLAY TELASMP006. 

       R2A.
           
           MOVE ZEROS TO PACI-CPF
           
           
           ACCEPT TCPF
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                GO TO ROT-FIM.

       R2B.

           MOVE ZEROS TO DATACONSULTA


           ACCEPT TDATACONSULTA
           
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                GO TO R2A.

           IF DATACONSULTA = SPACES 
              MOVE "DIGITE UM NASCIMENTO VALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2B.

           IF ANOCONS < 1 OR ANOCONS > 2020
              MOVE "DIGITE UM NASCIMENTO VALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2B.

           IF MESCONS < 1 OR MESCONS > 12
              MOVE "DIGITE UM NASCIMENTO VALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2B.
           

           IF MESCONS = 04 OR MESCONS = 06 OR MESCONS = 09 OR 
           MESCONS = 11

                IF DIACONS > 30 OR DIACONS < 1
                   MOVE "DIGITE UM NASCIMENTO VALIDO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R2B.

               
           IF MESCONS = 02

                 DIVIDE ANOCONS BY 4 GIVING LIXO REMAINDER BI
                 
                 IF BI = 0

                    PERFORM R20A
                    
                 ELSE 

                    PERFORM R20B

           
           IF DIACONS > 31 OR DIACONS < 1
                MOVE "DIGITE UM NASCIMENTO VALIDO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R2B.  
           
        
      *---------------VERIFICA SE JÁ EXISTE NO ARQUIVO-----------------*

       LER-CADCONS.
           READ CADCONS
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                PERFORM R3A
                PERFORM R4A
                PERFORM R5A
                PERFORM R6A
                PERFORM R7A
                PERFORM R9A
                PERFORM R11A
                PERFORM R14A
                DISPLAY TELASMP006
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADCONS" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.

      *----------------------------------------------------------------*
       R3A.
           MOVE PACI-CPF TO CPF

           READ CADPACI
           IF ST-ERRO = "23"
                   MOVE "*** CPF NAO EXISTE ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R2A.
           
           IF ST-ERRO NOT = "00"
                  MOVE "ERRO NA LEITURA ARQUIVO CADASTRO DE CEP" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM.

       R4A. 

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
                    GO TO R4A.

       R5A.
           MOVE TBGENERO(IND) TO TXTGENERO
           IF TXTCOD NOT = GENERO
              ADD 1 TO IND
              IF IND < 10
                 GO TO R5A
           ELSE
               MOVE TXTTEXT TO TXTGENERO.

           DISPLAY TELASMP006.
       R6A.

           MOVE CONVENIO TO CODIGO

           READ CADCONV
           IF ST-ERRO = "23"
                   MOVE "*** CONV NAO EXISTE ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R2A.
           
           IF ST-ERRO NOT = "00"
                  MOVE "ERRO NA LEITURA ARQUIVO CADCONV" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM.
           
       R7A. 

           MOVE TBCONV(PLANO) TO PLANOTXT
           DISPLAY TELASMP006.


       R8A.
           
           MOVE ZEROS TO CODMEDICO
           
           ACCEPT TCRM
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2A. 

           IF CODMEDICO = ZEROS
              MOVE "DIGITE UM CRM VALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R8A. 

           
       R9A. 

           MOVE CODMEDICO TO CRM

           READ CADMED
           IF ST-ERRO = "23"
                   MOVE "*** CODMEDICO NAO EXISTE ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R8A.
           
           IF ST-ERRO NOT = "00"
                  MOVE "ERRO NA LEITURA ARQUIVO CODMEDICO" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM.

       R10A.
           
           IF ESPEC > 0 AND ESPEC < 7
              MOVE COLESP(ESPEC) TO ESPECTXT
           ELSE
              MOVE "*VERIFIQUE A TABELA E DIGITE UM NUMERO VALIDO*" 
              TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2A.

       R11A.

           IF SEXOMED = "F" 
              MOVE "FEMINIMO" TO TXTSEXOMED
           ELSE
              IF SEXOMED = "M"
                 MOVE "MASCULINO" TO TXTSEXOMED
              ELSE
                 IF SEXOMED = "I"
                    MOVE "INDEFINIDO" TO TXTSEXOMED
                 ELSE
                    MOVE "*DIGITE M - MASCULINO E F - FEMININO*" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO R11A.

       R12A.

           DISPLAY TELASMP006.


       R13A.

           MOVE ZEROS TO CODIGO-DOENCA
           
           ACCEPT TCID
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R8A. 

           IF CODIGO-DOENCA = ZEROS
              MOVE "DIGITE UM CID VALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R13A. 

           

       R14A.

           MOVE CODIGO-DOENCA TO CID

           READ CADCID
           IF ST-ERRO = "23"
                   MOVE "*** CADCID NAO EXISTE ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R13A.
           
           IF ST-ERRO NOT = "00"
                  MOVE "ERRO NA LEITURA ARQUIVO CADCID" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM.
           
       R15A.

           DISPLAY TELASMP006.

       R16A. 
           
           MOVE SPACES TO DESCRICAO1
           
           ACCEPT TDESCRICAO1
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R13A. 

           IF DESCRICAO1 = SPACE
              MOVE "DIGITE UMA DESCRICAO VALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R16A. 

       R17A.

           MOVE SPACES TO DESCRICAO2
           
           ACCEPT TDESCRICAO2
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R16A. 

           IF DESCRICAO2 = SPACE
              MOVE "DIGITE UMA DESCRICAO VALIDO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R17A.


       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R17A.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R2A.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCONS
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

       R20A.

               IF DIA > 29 OR DIA < 1
                  MOVE "DIGITE UM NASCIMENTO VALIDO" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R2B.


       R20A-FIM.

       R20B.

               IF DIA > 28 OR DIA < 1
                  MOVE "DIGITE UM NASCIMENTO VALIDO" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R2B.


       R20B-FIM.

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
                      GO TO R8A
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
                DELETE CADCONS RECORD
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
                IF W-ACT = 01 GO TO R8A.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCONS
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA ALTERACAO DO REGISTRO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       ROT-FIM.
           CLOSE CADPACI
           CLOSE CADMED
           CLOSE CADCONV
           CLOSE CADCONS
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