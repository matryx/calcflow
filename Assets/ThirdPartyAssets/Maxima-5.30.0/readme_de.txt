Dies ist die Windows-LIESMICH-Datei.


In dieser Distribution enthaltende Binärdateien
-----------------------------------------------

Die Windows-Version des Maxima-Pakets enthält Binärdateien 
aus anderen Open-Source-Projekten, die ebenfalls auf Sourceforge 
gewartet werden.


gcc: 

gcc.exe, cc1.exe und die Dateien in den lib/gcc-lib und include/ 
Unterverzeichnissen stammen aus der mingw-Version des gcc.  Diese
ist erhältlich unter http://prdownloads.sf.net/mingw/
     

binutils:

as.exe stammt vom mingw (http://www.mingw.org/)-Port der binutils
erhältlich unter http://prdownloads.sf.net/mingw/


gnuplot:

Die Dateien wgnuplot.exe, wgnuplot.hlp und wgnuplot.mnu stammen 
aus der Windows-Distribution von gnuplot unter 
http://gnuplot.sourceforge.net


wxMaxima:

Die Dateien im wxMaxima-Unterverzeichnis stammen aus der Windows-
Distribution von wxMaxima erhältlich unter 
http://wxmaxima.sourceforge.net


Maxima-GUI und Firewall
-----------------------

Manchmal kann die Maxima-GUI (xmaxima oder wxMaxima) Maxima nicht 
starten, gibt Zeitüberschreitungsmeldungen aus oder erhält auf 
Maxima-Kommandos keine Antworten. Sehr wahrscheinlich ist dieses 
Problem dann durch die Firewall und/oder eine Antivirus-Software 
verursacht. Die Maxima-GUI kommuniziert mit der Berechnungseinheit 
über ein Socket. Antivirus- und/oder Firewall-Progamme erkennen 
das und versuchen eventuell, dies zu blocken 
(da einige schädliche Programme ebenfalls Sockets öffnen). 

Zur Lösung des Problems:

1.  Versuchen Sie, die Systemsteuerung der Antivirus- und/oder 
    Firewall-Software zu finden.

2.  Finden Sie die Maxima-GUI auf der Liste der geblockten Programme 
    und deaktivieren Sie die Blockierung der GUI. Das GUI-Programm 
    wird eventuell als "Tcl/Tk" aufgeführt 
    (der Name des GUI-Werkzeugsatzes für xmaxima).


Datenausführungsverhinderung (DEP)
----------------------------------

In einigen Fällen funktioniert weder Maxima GUI noch 
Kommandozeilen-Maxima (maxima.bat startet und beendet sich sofort wieder).
Sehr wahrscheinlich hängt das Problem mit Windwos DEP zusammen.
Einige Lisp-Implementierungen führen Code in Datenbereichen des 
Arbeitsspeichers aus. Windows DEP bemerkt und blockiert dies (weil
einige schädliche Programme ebenfalls Code in Datenbereichen ausführen).

Lösung:

   Fügen Sie den vollen Programmpfad von Maxima (z.B. 
   C:\Programme\Maxima-5.12.0\lib\maxima\5.12.0\binary-gcl\maxima.exe)
   in die Liste von DEP-Ausnahmen ein  
   (Systemsteuerung -> System ->  Erweitert -> Systemleistung "Einstellung" 
    -> Datenausführungsverhinderung)


Zu wenig Umgebungsspeicher
--------------------------

Wenn Sie versuchen, Maxima auf einer Windows 9x-Maschine laufen 
zu lassen, erhalten sie eventuell die Fehlermeldung 

    Zu wenig Umgebungsspeicher (Out of environment space)

Der folgende Hinweis des Artikels 230205 der Microsoft Knowledge Base 
(http://support.microsoft.com/support/kb/articles/Q230/2/05.ASP)
könnte Ihnen helfen.


URSACHE
 
Dieses Problem kann auftreten, wenn Sie nicht über ausreichend 
Arbeitsspeicher in der MS-DOS-Umgebung verfügen, um eine 
Umgebungsvariable festzulegen. 

LÖSUNG 

Erhöhen Sie den für MS-DOS-Programme verfügbaren vorgegebenen 
Umgebungsspeicherplatz, um dieses Problem zu beheben. Wenden Sie 
zu diesem Zweck eine der folgenden Methoden an. 


Ändern Sie für alle MS-DOS-Programme die Umgebung

Bearbeiten Sie den Shell-Befehl in der Datei Config.sys, um den 
vorgegebenen Umgebungsspeicherplatz für alle MS-DOS-Programme zu erhöhen, 
die in Windows ausgeführt werden. Gehen Sie hierzu folgendermaßen vor:

1. Klicken Sie auf Start und klicken Sie dann auf Ausführen.  

2. Geben Sie in dem Feld Öffnen Sysedit ein und klicken Sie dann auf OK.  

3. Klicken Sie auf das Fenster Config.sys.  

4. Geben Sie am Anfang der Shell= -Zeile REM ein und drücken anschließend 
   die LEERTASTE. 
   Sollte die Shell= -Zeile nicht vorhanden sein, fahren Sie mit dem 
   nächsten Schritt fort.  

5. Drücken Sie die Taste POS1.  

6. Um eine neue Shell= -Zeile zu erstellen, geben Sie die folgende Zeile 
   ein und drücken anschließend die EINGABETASTE: 

      SHELL=C:\COMMAND.COM /E:4096 /P 

7. Klicken Sie in dem Menü Datei auf Speichern.  

8. Klicken Sie in dem Menü Datei auf Beenden.  

9. Starten Sie den Computer erneut.  


Ändern Sie die Umgebung für ein bestimmtes MS-DOS-Programm 

Gehen Sie folgendermaßen vor, um den vorgegebenen Umgebungsspeicherplatz nur 
für maxima.bat zu erhöhen: 

1. Klicken Sie mit der rechten Maustaste auf das maxima.bat-Programmsymbol 
   und klicken Sie dann auf Eigenschaft.  

2. Klicken Sie auf die Registerkarte Speicher.  

3. Geben Sie in dem Feld Anfänglicher Umgebungsspeicher die Zahl von 
   Kilobytes (KB), die für das Programm benötigt werden, ein und 
   klicken Sie dann auf OK. 

Hinweis: 4096 KB ist die maximale Größe des Speichers, die Sie für ein 
MS-DOS-Programm reservieren können.  



