This is the windows README file.


Binary files included with this distribution
--------------------------------------------

The Windows version package of Maxima includes binary files 
from other Open Source projects also hosted on Sourceforge.

gcc: 

gcc.exe, cc1.exe and the files in lib/gcc-lib and include/ 
subdirectories are from the mingw version of gcc.  This is
available from http://prdownloads.sf.net/mingw/
     

binutils:

as.exe is from the mingw (http://www.mingw.org/) port of binutils
available from http://prdownloads.sf.net/mingw/


gnuplot:

The files wgnuplot.exe, wgnuplot.hlp and wgnuplot.mnu are from the
Windows distribution of gnuplot from http://gnuplot.sourceforge.net


wxMaxima:

The files in the wxMaxima subdirectory are from the Windows distribution
of wxMaxima available from http://wxmaxima.sourceforge.net


Maxima GUI and firewall
-----------------------

Sometimes Maxima GUI (xmaxima or wxMaxima) can't launch Maxima
or issues timeout message or gets no response for Maxima commands.  
Quite probably the problem is caused by firewall and/or antivirus software.  
The Maxima GUI talks to the computational engine through a socket.  
Antivirus and/or firewall programs see that and may try to block it 
(because some malicious programs open sockets too).  

To resolve the problem:

1.  Try to find the control panel for the antivirus and/or firewall. 

2.  Find the Maxima GUI on the list of blocked programs and disable 
    blocking for it.  The GUI program might appear as "Tcl/Tk" 
    (the name of the GUI toolkit for xmaxima).


Data Execution Prevention (DEP)
-------------------------------
Sometimes not only Maxima GUI but also command line Maxima
doesn't work (maxima.bat starts and immediately quits).
Quite probably the problem is caused by Windows DEP.
Some Lisp implementations execute code in data areas of memory. 
Windows DEP sees that and blocks it (because some malicious programs 
execute code in data areas too).

Solution:  

  Include the full program path of the Maxima executable, for example: 
  C:\Program Files\Maxima-5.12.0\lib\maxima\5.12.0\binary-gcl\maxima.exe 
  in the list of DEP exceptions 
  (Control Panel -> System -> Advanced -> Performance -> DEP)


Out of Environment Space
------------------------

When you attempt to run maxima on a Windows 9x machine
you may get the error 

    Out of environment space

The following advice from Microsoft Knowledge Base Article 230205
(http://support.microsoft.com/support/kb/articles/Q230/2/05.ASP)
may be of assistance.

CAUSE

This issue can occur if you do not have enough memory in the MS-DOS 
environment to set an environment variable. 

RESOLUTION

To resolve this issue, increase the default environment space available 
for MS-DOS programs. To do this, use one of the following methods. 

Modify the Environment for All MS-DOS Programs

To increase the default environment space for all MS-DOS programs 
running in Windows, edit the Shell command in the Config.sys file. 
To do this, follow these steps: 

1.  Click Start, and then click Run. 

2.  In the Open box, type sysedit, and then click OK. 

3.  Click the Config.sys window. 

4.  At the beginning of the Shell= line, type REM, and then press 
    the SPACEBAR. 

    If the Shell= line does not exist, proceed to the next step. 

5.  Press the HOME key. 

6.  Type the following line to create a new Shell= line, and then 
    press ENTER: 

      SHELL=C:\COMMAND.COM /E:4096 /P 

7.  On the File menu, click Save. 

8.  On the File menu, click Exit. 

9.  Restart the computer. 


Modify the Environment for a Specific MS-DOS Program

To increase the default environment space just for maxima.bat, follow 
these steps: 

1.  Right-click the maxima.bat program icon, and then click Properties. 

2.  Click the Memory tab. 

3.  In the Initial Environment box, type the number of kilobytes (KB) 
    that the program requires, and then click OK. 

NOTE: The maximum amount of memory that you can allocate is 4096 KB.

