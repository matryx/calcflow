Questo è il file leggimi di windows.


File binari inclusi in questa distribuzione
--------------------------------------------

La versione Windows del pacchetto di Maxima include dei file
binari di software Open Source presenti anche su Sourceforge.

gcc: 

gcc.exe, cc1.exe, i file presenti nelle sottocartelle lib/gcc-lib 
e in include/ sono derivati dalla versione mingw di gcc.  
Quest'ultimo è disponibile su http://prdownloads.sf.net/mingw/
     

binutils:

as.exe deriva dal port mingw (http://www.mingw.org/) di binutils
disponibile su http://prdownloads.sf.net/mingw/


gnuplot:

I file wgnuplot.exe, wgnuplot.hlp e wgnuplot.mnu provengono dalla
distribuzione Windows di gnuplot dal sito http://gnuplot.sourceforge.net


wxMaxima:

I file presenti nella sottocartella wxMaxima provengono dalla 
distribuzione Windows di wxMaxima disponibile su 
http://wxmaxima.sourceforge.net


Le GUI di Maxima e i firewall
-----------------------------

Talvolta le GUI di Maxima (xmaxima o wxMaxima) non sono in grado di
eseguire Maxima o presentano problemi di timeout oppure non ottengono
risposta dai comandi inviati a Maxima.
Molto probabilmente il problema deriva dal firewall o dal software antivirus.
Le GUI di Maxima dialogano con il motore computazionale attraverso una 
connessione di rete (socket). Questa connessione viene vista dai software
di protezione come un pericolo (dato che alcuni programmi maligni aprono 
anch'essi delle connessioni, ma in questo caso per violare la nostra privacy) 
e la bloccano.

Per risolvere il problema:

1.  Cercare il pannello di controllo dell'antivirus e/o firewall. 

2.  Cercare la GUI di Maxima nell'elenco dei programmi bloccati e togliere il 
    blocco.  Il programma GUI potrebbe apparire come "Tcl/Tk" (il nome della 
    libreria GUI di xmaxima).


Spazio ambiente esaurito
------------------------

Quando si tenta di eseguire maxima su un sistema Windows 9x
si potrebbe presentare l'errore

    Out of environment space

oppure

    Spazio ambiente esaurito

La seguente descrizione presa da Microsoft Knowledge Base Article 230205
(http://support.microsoft.com/support/kb/articles/Q230/2/05.ASP)
potrebbe essere di aiuto.

CAUSA

Il problema deriva dal fatto che MS-DOS non ha abbastanza 
memoria per impostare una variabile ambiente.

SOLUZIONE

Per risolvere il problema, aumentare lo spazio ambiente disponibile 
predefinito per i programmi MS-DOS.
Per farlo basta eseguire uno dei seguenti metodi.

Incremento dello spazio ambiente predefinito per tutti i programmi
MS-DOS.

Per aumentare lo spazio ambiente predefinito per tutti i programmi
MS-DOS che girano sotto Windows, modificare il comando SHELL nel
file CONFIG.SYS . Per farlo eseguire i seguenti passi:

1.  Fare clic su Start o Avvio, e poi su Esegui. 

2.  Nella finestra di dialogo aperta, digitare sysedit, 
    e fare clic su OK. 

3.  Fare clic sulla finestra CONFIG.SYS. 

4.  All'inizio della riga Shell=, battere REM, e uno spazio per
    commentarla.

    Se la riga Shell= non esiste o è già stata commentata, procedere 
    con i prossimi passi. 

5.  Premere il tasto chiamato HOME o alle volte indicato con una freccia 
    laterale, presente nel tastierino funzionale (lo trovate vicino ai 
    tasti Canc, Ins, PgSu e PgGiù). 

6.  Battere la riga seguente per creare una nuova riga Shell=, e premere
    INVIO: 

      SHELL=C:\COMMAND.COM /E:4096 /P 

7.  Sul menu FILE, fare clic su Salva o Save. 

8.  Sul FILE menu, fare clic su Esci o Exit. 

9.  Riavviare il computer. 


Modificare lo spazio ambiente per un programma MS-DOS specifico.

Per aumentare lo spazio ambiente predefinito solo per maxima.bat, 
eseguire i seguenti passi:

1.  Fare clic destro sull'icona del programma maxima.bat, poi fare clic su Proprietà. 

2.  Fare clic sulla scheda Memoria.

3.  Nel riquadro Ambiente iniziale, battere il valore di kilobytes (KB)
    che il programma richiede, poi premere OK. 

NOTA: l'ammontare massimo di memoria che si può allocare è 4096 KB.

