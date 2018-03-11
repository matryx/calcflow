Fichero LEAME para Windows

Ficheros binarios incluidos en esta distribución
------------------------------------------------

La versión de Windows del paquete Maxima incluye ficheros
binarios procedentes de otros proyectos de Código Abierto,
también alojados en Sourceforge.


gcc: 

gcc.exe, cc1.exe y los ficheros de los subdirectorios lib/gcc-lib
y include/ proceden de la versión mingw de gcc, que pueden
conseguirse en http://prdownloads.sf.net/mingw/
     

binutils:

as.exe procede de la versión específica de binutils de mingw
(http://www.mingw.org/), la cual puede a su vez conseguirse
en http://prdownloads.sf.net/mingw/


gnuplot:

Los ficheros wgnuplot.exe, wgnuplot.hlp y wgnuplot.mnu proceden de
la distribución para Windows de gnuplot, http://gnuplot.sourceforge.net


wxMaxima:

Los ficheros del sudirectorio wxMaxima proceden de la distribución
para Windows de wxMaxima, alojado en http://wxmaxima.sourceforge.net


Entornos gráficos de Maxima y cortafuegos
-----------------------------------------

En ocasiones, los entornos gráficos de Maxima (xmaxima o wxMaxima)
no pueden iniciar una sesión de Maxima, o emiten mensajes relativos
al exceso de tiempo de espera o, simplemente, no obtienen respuesta
alguna de las instrucciones de Maxima. Es probable que la causa de
estos problemas se encuentre en los antivirus y/o cortafuegos que
haya instalados en el sistema. Los entornos gráficos conectan con
Maxima a través de sockets, los cuales pueden verse bloqueados por
los antivirus y/o cortafuegos; esto es así porque algunos programas
dañinos también utilizan sockets.

Para resolver el problema:

1.  Inténtese encontrar el panel de control del antivirus y/o cortafuegos.

2.  Búsquese el entorno gráfico de Maxima en la lista de programas
    bloqueados y desactívese el bloqueo. El entorno gráfico puede aparecer
    con el nombre "Tcl/Tk", que es el nombre de la herramienta gráfica de
    xmaxima.


Prevención en la ejecución de datos (DEP)
-----------------------------------------

Algunas veces no sólo la interfaz gráfica de Maxima sino también la
interfaz en línea de comandos pueden no trabajar  (maxima.bat inicia y
termina inmediatamente). Probablemente el problema es causado por el sistema
DEP de Windows. Algunas implementaciones de Lisp ejecutan código en áreas
de datos en memoria. El sistema DEP de Windows bloquea estos códigos
(debido a que algunos programas maliciiosos ejecutan código en éstas
áreas también)

Solución:

Incluya la ruta completa del ejecutable de Maxima, por ejemplo:
C:\Program Files\Maxima-5.12.0\lib\maxima\5.12.0\binary-gcl\maxima.exe
en la lista de excepciones del sistema DEP:
(Panel de Control -> Sistema -> Avanzado -> Desempeño -> DEP)


Mensaje de fuera de espacio de entorno
--------------------------------------

Cuando se intenta ejecutar Maxima en una plataforma Windows 9x,
es posible recibir el siguiente mensaje de error:

            Fuera de espacio de entorno 

El artículo 230205 de la Microsoft Knowledge Base
(http://support.microsoft.com/support/kb/articles/Q230/2/05.ASP)
puede ser de utilidad en estos casos.

CAUSA

Este problema puede ocurrir si no tiene suficiente memoria en el entorno
MS-DOS para establecer una variable de entorno.

SOLUCIÓN

Para resolver este problema, se debe aumentar el espacio predeterminado
de entorno disponible para programas de MS-DOS. Para ello, utilícese uno
de los métodos siguientes.


Modificación del entorno para todos los programas de MS-DOS.

Para aumentar el espacio predeterminado de entorno para todos los programas
de MS-DOS que se ejecutan en Windows, modifíquese el comando Shell en el 
archivo Config.sys; para ello deben seguirse los siguientes pasos:

1. Hágase clic en Inicio y a continuación otro clic en Ejecutar.

2. En el cuadro Abrir, escríbase sysedit y a continuación clic en Aceptar.

3. Hágase clic en la ventana Config.sys.

4. Al comienzo del renglón Shell=, escríbase REM y a continuación presiónese la 
   BARRA ESPACIADORA.

   Si no existe el renglón Shell=, continúese con el paso siguiente.

5. Púlsese la tecla INICIO.

6. Escríbase el siguiente código para crear un nuevo renglón Shell=
   y a continuación presiónese ENTRAR:

      SHELL=C:\COMMAND.COM /E:4096 /P

7. En el menú Archivo, clic en Guardar.

8. En el menú Archivo, clic en Salir.

9. Reiníciese el equipo.



Modificación del entorno para un programa específico de MS-DOS

A fin de aumentar el espacio predeterminado de entorno
para maxima.bat, deben seguirse los siguientes pasos:

1. Hágase clic con el botón secundario en el icono de maxima.bat y
   a continuación clic en Propiedades.

2. Hágase clic en la ficha Memoria.

3. En el cuadro Entorno inicial, escríbase el número de kilobytes (KB)
   que necesita el programa y a continuación clic en Aceptar. 


NOTA: La cantidad máxima de memoria que se puede asignar a
      un programa de MS-DOS es 4096 KB.
