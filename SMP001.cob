       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR92.
      *AUTHOR. ROBERTO DA SILVA MITSUNARI.
      **************************************
      * CADASTRO DE MEDICOS *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT REGMED ASSIGN TO DISK
              ORGANIZATION IS INDEXED
              ACCESS MODE  IS DYNAMIC
              RECORD KEY   IS CRM
              FILE STATUS  IS ST-ERRO
              ALTERNATE RECORD KEY IS NOME WITH DUPLICATES.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD REGMED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "REGMED.DAT".        
       01 CADMEDICO.
          03 CRM                   PIC 9(06).
          03 NOME                  PIC X(30).
          03 ESPECIALIDADE         PIC 9(02).
          03 SEXO                  PIC X(01).
          03 DATANASCIMENTO.
               05 DIANASC          PIC 9(02).
               05 MESNASC          PIC 9(02).
               05 ANONASC          PIC 9(04).
          03 EMAIL                 PIC X(30).
          03 TELEFONE.
               05 DDD              PIC 9(02).
               05 NUM              PIC 9(11).
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
       

      *----------------------------------------------------------------
       SCREEN SECTION.
       01  TELAMEDICO.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "                           CADASTRO DE M".
           05  LINE 01  COLUMN 41 
               VALUE  "EDICOS".
           05  LINE 03  COLUMN 01 
               VALUE  "CRM:".
           05  LINE 05  COLUMN 01 
               VALUE  "NOME:".
           05  LINE 07  COLUMN 01 
               VALUE  "ESPECIALIDADE:".
           05  LINE 09  COLUMN 01 
               VALUE  "SEXO:".
           05  LINE 11  COLUMN 01 
               VALUE  "DATA NASCIMENTO:  -  -".
           05  LINE 13  COLUMN 01 
               VALUE  "EMAIL:".
           05  LINE 15  COLUMN 01 
               VALUE  "TELEFONE:  -".
           05  TCRM
               LINE 03  COLUMN 05  PIC 9(06)
               USING  CRM
               HIGHLIGHT.
           05  TNOME
               LINE 05  COLUMN 06  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TESPECIAL
               LINE 07  COLUMN 15  PIC 9(02)
               USING  ESPECIALIDADE
               HIGHLIGHT.
           05  TSEXO
               LINE 09  COLUMN 06  PIC X(01)
               USING  SEXO
	           HIGHLIGHT.
           05  TDIA
               LINE 11  COLUMN 17  PIC 9(02)
               USING  DIANASC
               HIGHLIGHT.
           05  TMES
               LINE 11  COLUMN 20  PIC 9(02)
               USING  MESNASC
               HIGHLIGHT.
           05  TANO
               LINE 11  COLUMN 23  PIC 9(04)
               USING  ANONASC
               HIGHLIGHT.
           05  TEMAIL
               LINE 13  COLUMN 07  PIC X(30)
               USING  EMAIL
               HIGHLIGHT.
           05  TDDD
               LINE 15  COLUMN 10  PIC 9(02)
               USING  DDD
               HIGHLIGHT.
           05  TNUM
               LINE 15  COLUMN 13  PIC 9(11)
               USING  NUM
               HIGHLIGHT.

       01  TELAE.
           05  LINE 14  COLUMN 41 VALUE  "1-CLINICA MEDICA        ".
           05  LINE 15  COLUMN 41 VALUE  "2-UROLOGIA       ".
           05  LINE 16  COLUMN 41 VALUE  "3-GINICOLOGISTA         ".
           05  LINE 17  COLUMN 41 
               VALUE  "4-PEDIATRIA       ".
           05  LINE 18  COLUMN 41 
               VALUE  "5-CARDIOLOGISTA        ".
           05  LINE 19  COLUMN 41 
               VALUE  "6-NEUROLOGIA        ".
           05  LINE 20  COLUMN 41 
               VALUE  "7-DERMATOLOGIA        ".
           05  LINE 21  COLUMN 41 
               VALUE  "8-UROLOGIA        ".
      *-----------------------------------------------------------------         
       PROCEDURE DIVISION.
       R0.
           OPEN I-O REGMED
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT REGMED
                 CLOSE REGMED
                 MOVE "*** ARQUIVO CADMEDICO FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADMEDICO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       R1.
           MOVE SPACES TO NOME SEXO EMAIL
           MOVE ZEROS TO CRM ESPECIALIDADE DIANASC MESNASC ANONASC
           MOVE ZEROS TO DDD NUM
           DISPLAY TELAMEDICO.

       R2.
           ACCEPT TCRM
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
               GO TO ROT-FIM.
           IF CRM = 000000
               GO TO R2.

       LER-CADMED.
           
           READ REGMED
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                DISPLAY TELAMEDICO
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADCARRO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.  
       R3.       
           ACCEPT TNOME
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
           IF NOME = SPACES
                   GO TO R3.
        
        
       R33.
           DISPLAY TELAE.
           ACCEPT TESPECIAL
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
           IF ESPECIALIDADE < 1 OR ESPECIALIDADE > 8
                   MOVE "*** DIGITE APENAS DE 1 ATE 8 ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R33.

       R5.
           ACCEPT TSEXO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
               GO TO R33.
           IF SEXO NOT = "M" AND NOT = "F"
               MOVE "*** DIGITE APENAS M OU F ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R5.
       
       R6A.
           ACCEPT TDIA.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF DIANASC = 0 OR DIANASC >  31
                MOVE "DIA INVALIDO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R6A.
           IF W-ACT = 01
               GO TO R5.
       R6B.
           ACCEPT TMES.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF MESNASC = 0 OR MESNASC >  12
                MOVE "MES INVALIDO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R6B.
           IF W-ACT = 01
               GO TO R6A.
       R6C.
           ACCEPT TANO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF ANONASC < 1950 OR ANONASC >  2020
                MOVE "ANO INVALIDO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS2
                GO TO R6C.
           IF W-ACT = 01
                GO TO R6B.
       R7.       
           ACCEPT TEMAIL
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6C.
           IF EMAIL = SPACES
                   GO TO R7.

       R9.
           ACCEPT TDDD 
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R7.
           IF DDD = 000
                   GO TO R9.
       R9B.
           ACCEPT TNUM
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R7.
           IF NUM = 00000000000
                   GO TO R9B.
         

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R7.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE CADMEDICO
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
                DELETE REGMED RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO DO MEDICO EXCLUIDO ***" TO MENS
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
                IF W-ACT = 01 GO TO R7.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE CADMEDICO
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA ALTERACAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM. 
       
       ROT-FIM.
           CLOSE REGMED.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------   
       
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 2000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (23, 12) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

      *    FILE STATUS
      *    00 = OPERAÇÃO REALIZADO COM SUCESSO
      *    22 = REGISTRO JÁ CADASTRADO
      *    23 = REGISTRO NÃO ENCONTRADO
      *    30 = ARQUIVO NÃO ENCONTRADO