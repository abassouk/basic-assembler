REM
REM 20140514: Published - Check the LICENSE file.
REM 

DATA MOVE,TST,CMP,RTS,LEA,JMP,JSR,SUB,ADD,MOVEQ,ADDQ,PEA,SUBQ,EXG,CLR,SWAP,NOT
DATA RA,SR,HI,LS,CC,CS,NE,EQ,VC,VS,PL,MI,GE,LT,GT,LE
DATA D0,D1,D2,D3,D4,D5,D6,D7,A0,A1,A2,A3,A4,A5,A6,A7
IF FRE(0)<40000& THEN CLEAR,65000&
OPTION BASE 0:DEFLNG a-z
DIM b$(16),cc$(15),r$(15),abso(200,1)
DIM equ$(400),equ2$(400),lab$(200),lab(200,1),reg$(20,1)
FOR i=0 TO 16:READ b$(i):NEXT
FOR i=0 TO 15:READ cc$(i):NEXT
FOR i=0 TO 15:READ r$(i):NEXT
PRINT "Assembler V3.2"
INPUT"in";in$
INPUT"out";out$
OPEN in$ FOR INPUT AS #1
zeit0=TIMER
PRINT "0:";:i=0
WHILE NOT EOF(1)
  i=i+1
  LINE INPUT #1,z$:LOCATE,3:PRINT i;
  IF UCASE$(LEFT$(z$,9))="INCLUDE "+CHR$(34) THEN
    OPEN MID$(z$,10,INSTR(10,z$,CHR$(34))-10) FOR INPUT AS #2
    i=i-1:WHILE NOT EOF(2):LINE INPUT #2,z$:i=i+1:LOCATE,3:PRINT i;:WEND
    CLOSE 2
  END IF
WEND:CLOSE 1,2
DIM z$(i),z(i):OPEN in$ FOR INPUT AS #1:zei=i:i=0
PRINT :PRINT "1:";
WHILE NOT EOF(1)
  LINE INPUT #1,z$:z$(i)=z$:i=i+1:LOCATE,3:PRINT i;
  IF UCASE$(LEFT$(z$,9))="INCLUDE "+CHR$(34) THEN
    OPEN MID$(z$,10,INSTR(10,z$,CHR$(34))-10) FOR INPUT AS #2:i=i-1
    WHILE NOT EOF(2):LINE INPUT #2,z$(i):i=i+1:LOCATE,3:PRINT i;:WEND
    CLOSE 2
  END IF
WEND:CLOSE 1,2
IF i><zei THEN BEEP:END
PRINT :PRINT "2:";:instruct=0:ON ERROR GOTO erro
FOR i=0 TO zei-1
  LOCATE,3:PRINT i+1;
  z$=z$(i)
  IF INSTR(z$,";")>0 THEN z$=LEFT$(z$,INSTR(z$,";")-1)
  spaceout z$:IF (z$=""OR LEFT$(z$,1)="*")AND i<zei THEN GOSUB del
  spaceout2 z$
  FOR j=0 TO reg-1
1   k=INSTR(z$,reg$(j,0))
    IF k>0 THEN
      z$=LEFT$(z$,k-1)+reg$(j,1)+MID$(z$,k+LEN(reg$(j,0))+1)
      GOTO 1
    END IF
  NEXT
  IF INSTR(UCASE$(z$)," EQU ")>0 THEN
    k=INSTR(UCASE$(z$)," EQU ")
    equ$(equ)=UCASE$(LEFT$(z$,k-1)):equ=equ+1:spaceout2 equ$(equ-1):IF equ$(equ-1)=""THEN END
    z$=MID$(z$,k+5):spaceout z$
    equ2$(equ-1)=z$:GOSUB del
  ELSEIF INSTR(UCASE$(z$)," EQUR ")>0 THEN
    k=INSTR(UCASE$(z$)," EQUR ")
    reg$(reg,0)=LEFT$(z$,k-1):reg=reg+1:spaceout2 reg$(reg-1,0):IF reg$(reg-1,0)=""THEN END
    z$=MID$(z$,k+5):spaceout z$
    reg$(reg-1,1)=z$:GOSUB del
  ELSEIF INSTR(z$,"=")>0 THEN
    k=INSTR(z$,"=")
    equ$(equ)=UCASE$(LEFT$(z$,k-1)):equ=equ+1:spaceout2 equ$(equ-1):IF equ$(equ-1)=""THEN END
    z$=MID$(z$,k+1):spaceout z$:getval z$,f1:z$=STR$(f1):spaceout z$
    equ2$(equ-1)=z$:GOSUB del
  ELSEIF UCASE$(LEFT$(z$,10))="STRUCTURE " OR instruct THEN
    IF instruct=0 THEN
      instruct=1
      IF INSTR(z$,",")=0 THEN erro$="Bad header":ERROR 255
      a$=MID$(z$,INSTR(z$,",")+1)
      getval a$,offset
    ELSE
      bef$=UCASE$(LEFT$(z$,INSTR(z$," ")-1)):arg$=MID$(z$,INSTR(z$," ")+1):spaceout arg$
      IF bef$="" THEN END
      IF bef$="APTR" OR bef$="BPTR" OR bef$="LONG" OR bef$="ULONG" THEN
        anz=4:a$=arg$
      ELSEIF bef$="WORD" OR bef$="UWORD" THEN
        anz=2:a$=arg$
      ELSEIF bef$="BYTE" OR bef$="UBYTE" THEN
        anz=1:a$=arg$
      ELSEIF bef$="STRUCT" THEN
        a$=LEFT$(arg$,INSTR(arg$,",")-1):arg$=MID$(arg$,INSTR(arg$,",")+1)
        getval arg$,anz
      ELSEIF bef$="LABEL" THEN
        instruct=0
        anz=offset:a$=arg$
      ELSE
        erro$="Unknown type":ERROR 255
      END IF
      equ$(equ)=UCASE$(a$):equ2$(equ)=STR$(offset):equ=equ+1
      offset=offset+anz:IF instruct=0 THEN offset=0
    END IF
    GOSUB del
  END IF
  z$(i)=z$
  IF i>zei-1 THEN i=UBOUND(z$)+10
NEXT:j=0
FOR i=0 TO zei-1
  LOCATE ,3:PRINT i,j;
  IF z(i)=0 THEN
    IF j<i THEN z$(j)=z$(i)
    j=j+1
  END IF
NEXT:zei=j:ERASE z
PRINT:segments=-1
FOR times=0 TO 1
  OPEN out$ FOR OUTPUT AS #1
  abso=0:lenfile=0:maxseg=segments:segments=-1
  IF times=1 THEN
    PRINT #1,MKL$(&H3F3)+MKL$(0)+MKL$(maxseg+1)+MKL$(0)+MKL$(maxseg);
    FOR i=0 TO maxseg
      PRINT #1,MKL$(leng(i));
    NEXT
  END IF
  PRINT OCT$(times+3)+":";
  FOR i=0 TO zei-1 STEP 1
    start2:LOCATE,3:PRINT i+1;
    z$=z$(i)  
    IF times=0 THEN
      IF INSTR(z$,":")>0 THEN
        k=INSTR(z$,":")
        lab$(lab)=UCASE$(LEFT$(z$,k-1)):lab=lab+1:IF lab$(lab-1)=""THEN erro$="What label ?":ERROR 255
        z$=MID$(z$,k+1):spaceout z$:lab(lab-1,0)=lenfile:lab(lab-1,1)=segments
        IF z$="" OR LEFT$(z$,1)="*" THEN GOSUB del2:GOTO start2
      END IF
      z$(i)=z$
    END IF
    IF INSTR(z$," ")>0 THEN bef$=LEFT$(z$,INSTR(z$," ")-1):z$=MID$(z$,INSTR(z$," ")+1):spaceout z$ ELSE bef$=z$:b1$="":b2$=""
    IF INSTR(z$,",")>0 THEN
      b1$=LEFT$(z$,INSTR(z$,",")-1):b2$=MID$(z$,INSTR(z$,",")+1)
      spaceout b2$:spaceout2 b1$
    ELSEIF bef$<>z$ THEN
      b1$=UCASE$(z$):b2$=""
    ELSE
      b1$="":b2$=""
    END IF
    bef=-1:bef$=UCASE$(bef$)
    IF INSTR(bef$,".")=LEN(bef$)-1 THEN add$=RIGHT$(bef$,2):bef$=LEFT$(bef$,LEN(bef$)-2) ELSE add$=""
    m$="":m2=0:m1=0
    findestr b$(),bef$,bef
    IF bef>-1 THEN
      ON bef+1 GOSUB move,tst,cmp,rts,lea,jmp,jsr,suba,add,moveq,addq,pea,subq,exg,clr,swp,noti
    ELSEIF bef$="EVEN" THEN
      IF lenfile AND 1 THEN m$=CHR$(0) ELSE m$=""
    ELSEIF bef$="DC" THEN
      m$="":p=1
      WHILE p<=LEN(z$)
2       b$="":f1=0
        WHILE (MID$(z$,p,1)><"," OR f1) AND p<=LEN(z$)
          b$=b$+MID$(z$,p,1):p=p+1:f2=0
          IF f1=0 AND RIGHT$(b$,1)=CHR$(34) THEN f1=-1:f2=1
          IF f1=-1 AND RIGHT$(b$,1)=CHR$(34)AND f2=0 THEN f1=0
        WEND:spaceout b$:spaceout2 b$:p=p+1
        IF b$=""AND p<=LEN(z$) THEN 2
        IF add$=".B" THEN
          IF LEFT$(b$,1)=CHR$(34) AND RIGHT$(b$,1)=CHR$(34) THEN
            b$=LEFT$(b$,LEN(b$)-1):b$=RIGHT$(b$,LEN(b$)-1)
            m$=m$+b$
            GOTO 2
          END IF
        END IF
        getval b$,f1
        IF add$=".W" THEN m$=m$+RIGHT$(MKL$(f1),2) ELSE IF add$=".B"THEN m$=m$+RIGHT$(MKL$(f1),1) ELSE m$=m$+MKL$(f1)
      WEND
    ELSEIF bef$="BLK" OR bef$="DS" THEN
      IF b1$="" THEN erro$="Invalid Arguments":ERROR 255
      IF add$=".B" OR add$="" THEN f1=1
      IF add$=".W" THEN f1=2
      IF add$=".L" THEN f1=4
      getval b1$,anz:getval b2$,anz2
      m$=STRING$(anz*f1,anz2)
    ELSEIF LEFT$(bef$,1)="B" AND LEN(bef$)=3 AND bef$><"BSS" THEN
      IF b1$=""OR b2$<>""THEN erro$="Invalid Arguments":ERROR 255
      m$=MKI$(&H6000):m1=&H60:m2=0:cc=0
      FOR p=0 TO 15
        IF LEFT$(bef$,3)="B"+cc$(p) THEN cc=p:p=18
      NEXT
      IF p><19 THEN erro$="Invalid Condition Code":ERROR 255
      IF add$=".S" THEN f1=0 ELSE f1=1
      b$=b1$:GOSUB getx:IF mode>9 OR mode<8 THEN erro$="Invalid Arguments":ERROR 255
      IF absof=1 THEN absof=0:abso=abso-1:anz2=-lenfile-2+anz2
      IF f1=0 AND (anz2>127 OR anz2<-128) THEN f1=1
      m1=m1 OR cc:IF f1=0 THEN m2=m2 OR ASC(RIGHT$(MKI$(anz2),1))
      m$=CHR$(m1)+CHR$(m2):IF f1 THEN m$=m$+MKI$(anz2)
    ELSEIF LEFT$(bef$,2)="DB" AND LEN(bef$)=4 THEN
      IF b1$=""OR b2$="" THEN erro$="Invalid Arguments":ERROR 255
      m$=MKI$(&H50C8):m1=&H50:m2=&HC8:cc=0
      FOR p=0 TO 15
        IF LEFT$(bef$,4)="DB"+cc$(p) THEN cc=p:p=18
      NEXT:IF cc=0 THEN cc=1
      IF p=15 THEN erro$="Invalid Condition Code":ERROR 255
      b$=b1$:GOSUB getx
      IF mode><0 THEN erro$="Invalid Arguments":ERROR 255
      f2=anz
      b$=b2$:GOSUB getx
      IF mode<8 OR mode>9 THEN erro$="Invalid Arguments":ERROR 255
      IF absof=1 THEN absof=0:abso=abso-1:anz=-lenfile-2+anz2 ELSE anz=anz2
      m1=m1 OR cc:m2=m2 OR f2
      m$=CHR$(m1)+CHR$(m2)+MKI$(anz)
    ELSEIF bef$="SUBI" OR bef$="ADDI" OR bef$="ORI" OR bef$="ANDI" OR bef$="EORI" OR bef$="CMPI" THEN
      IF b1$="" OR b2$="" THEN erro$="Invalid arguments":ERROR 255
      IF bef$="ORI" THEN m1=0
      IF bef$="ANDI" THEN m1=2
      IF bef$="SUBI" THEN m1=4
      IF bef$="ADDI" THEN m1=6
      IF bef$="EORI" THEN m1=10
      IF bef$="CMPI" THEN m1=12
      IF add$=".B" THEN m2=0
      IF add$=".W" THEN m2=&H40
      IF add$=".L" OR add$="" THEN m2=&H80
      m$=CHR$(m1)+CHR$(m2)
      IF LEFT$(b1$,1)><"#" THEN erro$="Invalid arguments":ERROR 255
      getval MID$(b1$,2),anz
      IF m2=&H80 THEN m$=m$+MKL$(anz) ELSE m$=m$+RIGHT$(MKL$(anz),2)
      b$=b2$:GOSUB getx:GOSUB putx
    ELSEIF bef$="BSET" OR bef$="BCLR" OR bef$="BCHG" OR bef$="BTST" THEN
      IF b1$="" OR b2$="" OR add$><"" THEN erro$="Invalid arguments":ERROR 255
      m1=0:m2=0
      IF bef$="BCHG" THEN m2=&H40
      IF bef$="BCLR" THEN m2=&H80
      IF bef$="BSET" THEN m2=&HC0
      b$=b1$:GOSUB getx
      IF mode><0 AND mode><12 THEN erro$="Invalid arguments":ERROR 255
      IF mode=0 THEN m1=1+anz*2 ELSE m1=8
      m$=CHR$(m1)+CHR$(m2)
      IF mode=12 THEN m$=m$+MKI$(anz2)
      b$=b2$:GOSUB getx:GOSUB putx
    ELSEIF bef$="MOVEM" THEN
      IF add$><".W" AND add$><".L" THEN erro$="Invalid suffix":ERROR 255
      IF b1$="" OR b2$="" THEN erro$="Invalid arguments":ERROR 255
      m1=&H48:m2=&H80-&H40*(add$=".L")
      IF UCASE$(b1$)="(A7)+" THEN m1=&H4C
      m$=CHR$(m1)+CHR$(m2)
      IF m1=&H4C THEN SWAP b1$,b2$
      b$=UCASE$(b1$):anz=0
      WHILE b$>""
        c$=LEFT$(b$,2):b$=MID$(b$,3)
        IF LEFT$(c$,1)="A" THEN f1=8 ELSE f1=0
        f1=f1+VAL(RIGHT$(c$,1))
        c$=LEFT$(b$,1):b$=MID$(b$,2)
        IF c$="/"OR c$="" THEN
          IF m1=&H4C THEN anz=anz OR 2^f1 ELSE anz=anz OR 2^(15-f1)
        ELSEIF c$="-" THEN
          c$=LEFT$(b$,2):b$=MID$(b$,3)
          IF LEFT$(c$,1)="A" THEN f2=8 ELSE f2=0
          f2=f2+VAL(RIGHT$(c$,1)):IF f1>f2 THEN SWAP f1,f2
          FOR m=f1 TO f2
            IF m1=&H4C THEN anz=anz OR 2^m ELSE anz=anz OR 2^(15-m)
          NEXT
          b$=MID$(b$,2)
        END IF
      WEND
      m$=m$+RIGHT$(MKL$(anz),2)
      b$=b2$:GOSUB getx:GOSUB putx
    ELSEIF bef$="MULU" OR bef$="MULS" OR bef$="DIVU" OR bef$="DIVS" THEN
      IF b2$="" OR b1$="" OR add$><"" THEN erro$="Invalid arguments":ERROR 255
      IF LEFT$(bef$,1)="M" THEN m1=&HC0 ELSE m1=&H80
      IF RIGHT$(bef$,1)="S" THEN m1=m1 OR 1
      m2=&HC0
      b2$=UCASE$(b2$):IF LEFT$(b2$,1)><"D" THEN erro$="Invalid arguments":ERROR 255
      m1=m1 OR VAL(RIGHT$(b2$,1))*2
      m$=CHR$(m1)+CHR$(m2)
      b$=b1$:GOSUB getx:GOSUB putx
    ELSEIF (bef$="CODE" OR bef$="DATA"OR bef$="BSS"OR bef$="END") THEN
      IF segments>-1 THEN
        IF lastseg=0 AND lenfile AND 3 THEN PRINT #1,STRING$(4-(lenfile AND 3),0);
        IF lenfile AND 3 THEN lenfile=lenfile+(4-(lenfile AND 3))
        IF abso=0 THEN endeif
        PRINT #1,MKL$(&H3EC);
        FOR anz2=0 TO maxseg
          anz=0
          FOR m=0 TO abso-1
            IF abso(m,1)=anz2 THEN anz=anz+1
          NEXT
          IF anz>0 THEN
            PRINT #1,MKL$(anz)+MKL$(anz2);
            FOR m=0 TO abso-1
              IF abso(m,1)=anz2 THEN PRINT #1,MKL$(abso(m,0));
            NEXT
          END IF
        NEXT
        PRINT #1,MKL$(0);
        endeif:
        PRINT #1,MKL$(&H3F2);
        leng(segments)=lenfile/4
      END IF
      lastseg=0
      IF bef$="CODE" THEN PRINT #1,MKL$(&H3E9);
      IF bef$="DATA" THEN PRINT #1,MKL$(&H3EA);
      IF bef$="BSS" THEN PRINT #1,MKL$(&H3EB);:lastseg=3
      IF bef$><"END" THEN segments=segments+1
      IF times=1 AND bef$><"END" THEN PRINT #1,MKL$(leng(segments)); 'ELSE PRINT #1,MKL$(0);
      abso=0:lenfile=0:m$=""
    ELSEIF bef$="EXT" THEN
      IF b2$><"" OR b1$="" THEN erro$="Invalid arguments":ERROR 255
      IF  (add$><".L" AND add$><".W" AND add$><"") THEN erro$="Invalid suffix":ERROR 255
      m1=&H48:m2=&H80:IF add$=".L" OR add$="" THEN m2=&HC0
      IF UCASE$(LEFT$(b1$,1))><"D" OR LEN(b1$)><2 THEN erro$="Invalid arguments":ERROR 255
      m2=m2 OR VAL(RIGHT$(b1$,1))
      m$=CHR$(m1)+CHR$(m2)
    END IF
    endewend:
    IF i>zei-1 THEN i=UBOUND(z$)+10
    IF m$><""AND lastseg><3 THEN PRINT #1,m$;
    lenfile=lenfile+LEN(m$)
  NEXT
  CLOSE:PRINT
NEXT
PRINT :PRINT "TIME:"(TIMER-zeit0)"="(zei/(TIMER-zeit0))"lines/sec"
PRINT "EQU:";equ,"LABELS:";lab:BEEP
END
move:m2=0
  IF b1$="" OR b2$="" THEN erro$="Invalid arguments":ERROR 255
  IF add$=".B" THEN m1=&H10
  IF add$=".W" THEN m1=&H30
  IF add$=".L" OR add$="" THEN m1=&H20
  m$=CHR$(m1)+CHR$(m2)
  b$=b1$:GOSUB getx:GOSUB putx
  b$=b2$:GOSUB getx:GOSUB puty
RETURN
pea:
  m1=&H48:m2=&H40
 s:
  IF add$><"" OR b1$="" OR b2$><"" THEN erro$="Invalid arguments":ERROR 255
 s2:
  m$=CHR$(m1)+CHR$(m2)
  b$=b1$:GOSUB getx:GOSUB putx
RETURN
noti:m1=&H46:GOTO tst2
rts:m$="Nu":RETURN
jmp:m1=&H4E:m2=&HC0:GOTO s
jsr:m1=&H4E:m2=&H80:GOTO s
tst:m1=&H4A:m2=0
tst2:IF add$=".B" THEN m2=0
  IF add$=".W" THEN m2=&H40
  IF add$=".L" OR add$="" THEN m2=&H80
  GOTO s2
clr:m1=&H42:m2=0:GOTO tst2
lea:
  IF add$><"" OR b1$="" OR b2$="" THEN erro$="Invalid arguments":ERROR 255
  m1=&H41:m2=&HC0
  b$=b2$:GOSUB getx:IF mode><1 THEN erro$="Invalid arguments":ERROR 255
  m1=m1 OR (anz*2)
  GOTO s2
moveq:
  IF add$><"" OR b2$="" OR b1$="" THEN erro$="Invalid arguments":ERROR 255
  b$=b1$:GOSUB getx:IF mode><12 THEN erro$="Invalid arguments":ERROR 255
  IF anz2>255 OR anz2<0 THEN erro$="Invalid arguments":ERROR 255
  m1=&H70:m2=anz2
  b$=b2$:GOSUB getx:IF mode>0 THEN erro$="Invalid arguments":ERROR 255
  m1=m1 OR (anz*2)
  m$=CHR$(m1)+CHR$(m2 AND 255)
RETURN
addq:
subq:
  m1=&H50:m2=0:IF b1$="" OR b2$="" THEN erro$="Invalid arguments":ERROR 255
  IF bef$="SUBQ" THEN m1=&H51
  IF add$="" THEN add$=".L"
  IF add$=".B" THEN m2=0
  IF add$=".W" THEN m2=&H40
  IF add$=".L" THEN m2=&H80
  b$=b1$:GOSUB getx:IF mode><12 THEN erro$="Invalid arguments":ERROR 255
  IF anz2<1 OR anz2>8 THEN erro$="Invalid arguments":ERROR 255
  m1=m1 OR(anz2 AND 7)*2
  m$=CHR$(m1)+CHR$(m2)
  b$=b2$:GOSUB getx:GOSUB putx
RETURN
cmp:m=11:GOTO s3
suba:m=9:GOTO s3
ora:m=8:GOTO s3
add:m=&HD
s3:
  m1=m*16:m2=0
  IF b1$="" OR b2$="" THEN erro$="Invalid arguments":ERROR 255
  IF add$=".B" THEN m2=&H0 ELSE IF add$=".W" THEN m2=&H40 ELSE m2=&H80
  b$=b2$:GOSUB getx
  IF mode=0 OR mode=1 THEN
    f2=0:f3=mode
  ELSE
    b$=b1$:GOSUB getx
    IF mode>1 THEN erro$="Invalid arguments":ERROR 255
    f2=1:SWAP b1$,b2$:f3=mode
  END IF
  IF f3=0 THEN
    m1=m1 OR f2
    b$=b2$:GOSUB getx
    m1=m1 OR anz*2
  ELSE
    m2=&HC0
    IF add$=".B"THEN END ELSE IF add$=".L" THEN m1=m1 OR 1 ELSE m1=m1 AND -2
    b$=b2$:GOSUB getx
    m1=m1 OR anz*2 
  END IF
  m$=CHR$(m1)+CHR$(m2)
  b$=b1$:GOSUB getx:GOSUB putx
RETURN
exg:
  IF b1$=""OR b2$="" OR add$><"" THEN erro$="Invalid arguments":ERROR 255
  m1=&HC1:m2=0:b1$=UCASE$(b1$):b2$=UCASE$(b2$)
  IF LEN(b1$)><2 OR LEN(b2$)><2 THEN erro$="Invalid arguments":ERROR 255
  IF LEFT$(b1$,1)="A" AND LEFT$(b2$,1)="D" THEN SWAP b1$,b2$
  IF LEFT$(b1$,1)=LEFT$(b2$,1) THEN m2=&H40 ELSE m2=&H80
  IF LEFT$(b2$,1)="A" THEN m2=m2 OR 8
  m1=m1 OR VAL(RIGHT$(b1$,1))*2:m2=m2 OR VAL(RIGHT$(b2$,1))
  m$=CHR$(m1)+CHR$(m2)
RETURN
swp:m1=&H48:m2=&H40:GOTO s
del:
  z$(i)="":z$=""
  z(i)=1
RETURN
del2:
  FOR m=i TO  zei-2
    z$(m)=z$(m+1)
  NEXT
  z$(m)=""
  z$="":zei=zei-1
RETURN
erro:
  IF ERR><255 THEN ON ERROR GOTO 0
  PRINT  z$+"  :  "+erro$
  CLOSE
  END
getx:
  mode=-1:b$=UCASE$(b$)
  IF LEFT$(b$,1)="#" THEN mode=12:b$=RIGHT$(b$,LEN(b$)-1)
  IF mode=-1 AND LEN(b$)=2 THEN 
    FOR k=0 TO 15
      IF b$=r$(k) THEN mode=(k AND 8)/8:anz=k AND 7:k=18
    NEXT
  END IF
  IF mode=-1 AND LEFT$(b$,1)="(" AND RIGHT$(b$,1)=")" AND LEN(b$)=4 THEN
    FOR k=8 TO 15
      IF MID$(b$,2,2)=r$(k) THEN mode=2:anz=k AND 7:k=18
    NEXT
  END IF
  IF mode=-1 AND LEFT$(b$,1)="(" AND RIGHT$(b$,2)=")+" AND LEN(b$)=5 THEN
    FOR k=8 TO 15
      IF MID$(b$,2,2)=r$(k) THEN mode=3:anz=k AND 7:k=18
    NEXT    
  END IF
  IF mode=-1 AND LEFT$(b$,2)="-(" AND RIGHT$(b$,1)=")" AND LEN(b$)=5 THEN
    FOR k=8 TO 15
      IF MID$(b$,3,2)=r$(k) THEN mode=4:anz=k AND 7:k=18
    NEXT    
  END IF
  IF mode=-1 AND LEN(b$)>4 THEN
    IF  RIGHT$(b$,1)=")"AND MID$(b$,LEN(b$)-3,1)="(" THEN
    FOR k=8 TO 15
      IF LEFT$(RIGHT$(b$,3),2)=r$(k) THEN
        mode=5:anz=k AND 7
        k=INSTR(b$,"("):getval LEFT$(b$,k-1),anz2:k=18
      END IF
    NEXT
    END IF
  END IF
  IF mode=-1 AND LEN(b$)>7 AND RIGHT$(b$,1)=")" THEN
    FOR k=8 TO 15
      IF LEFT$(RIGHT$(b$,7),4)="("+r$(k)+"+" THEN mode=6:anz=k:k=18
    NEXT
    IF mode=6 THEN
      FOR k=0 TO 15
        IF MID$(b$,LEN(b$)-3,2)=r$(k) THEN anz3=k:k=18
      NEXT
      getval LEFT$(b$,INSTR(b$,"(")-1),anz2
    END IF
  END IF
  IF RIGHT$(b$,4)="(PC)"AND LEN(b$)>4 THEN
    mode=10:getval LEFT$(b$,INSTR(b$,"(")-1),anz2
    IF absof=1 THEN
      absof=0:abso=abso-1
      anz2=-abso(abso,0)+anz2
    END IF
  END IF
  IF mode><-1 AND mode<>12 THEN RETURN
  IF mode=-1 THEN mode=7
  IF mode=7 THEN IF UCASE$(RIGHT$(b$,2))=".W" THEN mode=8 ELSE mode=9
  getval b$,anz2
RETURN
putx:
  m1=ASC(m$):m2=ASC(MID$(m$,2,1))
  m2=m2 AND &HC0
  IF mode<7 THEN
    m2=m2 OR mode*8 OR anz
    IF mode=5 THEN 
      m$=m$+MKI$(anz2)
    ELSEIF mode=6 THEN
      m$=m$+CHR$(anz2*16)+CHR$(anz3)
    END IF
  ELSE
    IF mode=7 THEN IF (anz2 AND &HFFFF)=anz2 AND absof=0 THEN mode=8 ELSE mode=9
    IF mode><9 AND NOT(mode=12 AND add$=".L" ) THEN
      m$=m$+RIGHT$(MKL$(anz2),2)
    ELSE
      m$=m$+MKL$(anz2)
    END IF
    m2=m2 OR &H38 OR(mode AND 7)
  END IF
  MID$(m$,1,1)=CHR$(m1):MID$(m$,2,1)=CHR$(m2)
RETURN
puty:
  m1=ASC(m$):m2=ASC(MID$(m$,2,1))
  m1=m1 AND &HF0:m2=m2 AND &H3F
  IF mode<7 THEN
    m1=m1 OR anz*2 OR (mode AND 4)/4:m2=(m2 OR mode*&H40)AND &HFF
    IF mode=5 THEN
      m$=m$+MKI$(anz2)
    ELSEIF mode=6 THEN
      m$=m$+CHR$(anz2*16)+CHR$(anz3)
    END IF
  ELSE
    IF mode=7 THEN IF (anz2 AND &HFFFF)=anz2 AND absof=0 THEN mode=8 ELSE mode=9
    IF mode><9 THEN
      m$=m$+MKI$(anz2)
    ELSE
      m$=m$+MKL$(anz2)
    END IF
    m1=m1 OR 1:m2=m2 OR &HC0
    m1=m1 OR (mode AND 7)*2
  END IF
  MID$(m$,1,1)=CHR$(m1):MID$(m$,2,1)=CHR$(m2)
RETURN
SUB spaceout(a$)STATIC
  IF a$="" THEN EXIT SUB
  WHILE LEFT$(a$,1)=" " OR LEFT$(a$,1)=CHR$(9) AND a$><""
    a$=RIGHT$(a$,LEN(a$)-1)
  WEND
END SUB
SUB spaceout2(a$)STATIC
  IF a$="" THEN EXIT SUB
  WHILE RIGHT$(a$,1)=" " OR RIGHT$(a$,1)=CHR$(9) AND a$><""
    a$=LEFT$(a$,LEN(a$)-1)
  WEND
END SUB
SUB getval(a$,a&)STATIC
  SHARED abso,absof:absof=0
  b$=UCASE$(a$):a&=0:spaceout b$:spaceout2 b$
  IF LEFT$(b$,1)><"-" THEN b$="+"+b$
  WHILE b$>""
    d$=LEFT$(b$,1):b$=MID$(b$,2)
    p=1:c$=""
    WHILE p<=LEN(b$) AND MID$(b$,p,1)><"+" AND MID$(b$,p,1)><"-" AND MID$(b$,p,1)><"*" AND MID$(b$,p,1)><"/" AND MID$(b$,p,1)><"|" AND MID$(b$,p,1)><"&" AND MID$(b$,p,1)><"^"
      c$=c$+MID$(b$,p,1):p=p+1
    WEND
    b$=MID$(b$,p)
    spaceout c$:spaceout2 c$:t=absof
    getval2 c$,b&
    IF d$="+" THEN
      a&=a&+b&
    ELSEIF d$="-" THEN
      a&=a&-b&
      IF t=1 AND absof=1 THEN absof=0:abso=abso-1
    ELSEIF d$="*" THEN
      a&=a&*b&
    ELSEIF d$="/" THEN
      a&=FIX(a&/b&)
    ELSEIF d$="|" THEN
      a&=a& OR b&
    ELSEIF d$="&" THEN
      a&=a& AND b&
    ELSEIF d$="^" THEN
      a&=a& XOR b&
    ELSE
      BEEP:END
    END IF
  WEND
END SUB
SUB getval2(a$,a&)STATIC
  SHARED lab$(),lab(),m$,abso(),abso,absof,lenfile,equ$(),equ2$()
  b$=UCASE$(a$):absof=0:a&=0
ga:IF LEFT$(b$,1)="$" THEN
    b$=MID$(b$,2)
    FOR i=1 TO LEN(b$)
      a&=a&*16+VAL("&H"+MID$(b$,i,1))
    NEXT
  ELSEIF (LEFT$(b$,1)=CHR$(34) OR LEFT$(b$,1)="'") AND LEFT$(b$,1)=RIGHT$(b$,1) THEN
    b$=MID$(a$,2):b$=LEFT$(b$,LEN(b$)-1)
    b$=RIGHT$(MKL$(0)+b$,4)
    a&=CVL(b$)
  ELSEIF LEFT$(b$,1)="%" THEN
    b$=MID$(b$,2)
    FOR i=1 TO LEN(b$)
      a&=a&*2+-(MID$(b$,i,1)="1")
    NEXT
  ELSEIF VAL(b$)><0 OR LEFT$(b$,1)="0" THEN
    a&=VAL(b$)
  ELSE
    findestr lab$(),b$,j
    IF j>=0 THEN 
      a&=lab(j,0)
      abso(abso,0)=lenfile+LEN(m$)
      abso(abso,1)=lab(j,1)
      abso=abso+1:absof=1
      EXIT SUB
    END IF
    findestr equ$(),b$,a&
    IF a&>=0 THEN b$=UCASE$(equ2$(a&)):a&=0:GOTO ga
  END IF
END SUB
SUB findestr(a$(),s$,d) STATIC
  IF ftime=0 THEN ftime=1:GOSUB einlesen
  diff%=0:auf&=0:strv&=0:lstr%=0:fenr&=0:strf&=0:feld%=0
  auf&=VARPTR(a%(0)):lstr%=LEN(s$):diff%=UBOUND(a$)-LBOUND(a$)
  fenr&=VARPTR(feld%):strv&=SADD(s$):strf&=VARPTR(a$(0))
  CALL auf&(fenr&,lstr%,strv&,strf&,diff%)
  d=feld%
  EXIT SUB
einlesen:
  DIM a%(50):fenr&=0:RESTORE dat
  FOR i=0 TO 50
    READ a%(i):fenr&=fenr&+a%(i)
  NEXT
  IF fenr&><463967& THEN PRINT "Fehler in data's.":BEEP:LIST dat:END
  RETURN
dat:
  DATA 18663,32760,17029,17030,17031,9263,68,8815
  DATA 64,8303,60,8751,56,9839,52,5657
  DATA -7861,5657,14012,-1,-19901,26144,19009,26414
  DATA 17028,6169,-7860,6169,-7796,6169,9284,14849
  DATA 10312,-19188,26120,21317,26388,24822,22153,-16830
  DATA 26382,5657,-7861,5657,-8580,1,24778,13959
  DATA 19679,8190,20085
END SUB
