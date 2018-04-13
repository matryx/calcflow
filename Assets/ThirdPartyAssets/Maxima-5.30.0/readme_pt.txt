Este é o ficheiro LEIA-ME para Windows.


Ficheiros binários incluídos nesta distribuição
-----------------------------------------------

A versão Windows do pacote do Maxima inclui ficheiros binários
provenientes de outros projectos de Software Livre também
alojados no Sourceforge.

gcc: 

gcc.exe, cc1.exe e os ficheiros nas directórios lib/gcc-lib e
include/ provêm da versão mingw do gcc. Essa versão encontra-se
disponível em http://prdownloads.sf.net/mingw/
     

binutils:

as.exe faz parte da versão de binutils em mingw
(http://www.mingw.org/), disponível em
http://prdownloads.sf.net/mingw/


gnuplot:

Os ficheiros wgnuplot.exe, wgnuplot.hlp e wgnuplot.mnu provêm da
distribuição de gnuplot para Windows, em
http://gnuplot.sourceforge.net


wxMaxima:

Os ficheiros no directório wxMaxima são da distribuição de
wxMaxima para Windows, disponível em http://wxmaxima.sourceforge.net


Interface gráfica do Maxima e firewalls
---------------------------------------

Por vezes, as interfaces gráficas do Maxima (xmaxima ou wxMaxima)
não conseguem arrancar o Maxima, ou enviam uma mensagem de fim
do tempo de espera ou não obtêm nenhuma resposta aso comandos
do Maxima. O mais provável é que esses problemas sejam causados
por um programa de firewall ou de antivírus. A interface gráfica
do Maxima comunica-se com o motor do Maxima através de uma porta
(socket). Os programas de antivírus e/ou firewall verão isso como
um risco de segurança e tentarão bloqueá-lo (porque alguns vírus
costumam também usar portas para causar estragos).

Para resolver esse problema:

1.  Tente encontrar o painel de controlo do antivírus e/ou firewall.

2.  Procure o nome da interface gráfica do Maxima entre a lista de
    programas bloqueados e desactive esse bloqueio. A interface
    gráfica poderá aparecer identificada como um programa "Tcl/Tk" 
    (nome da linguagem em que a interface xmaxima está feita).


Falta de Espaço para Ambiente
-----------------------------

Quando tentar executar o maxima numa máquina com Windows 9x,
poderá obter o seguinte erro:

    Espaço para ambiente esgotado (Out of environment space)

A seguinte dica, no Artigo 230205 da Base de Conhecimento da
Microsoft, poderá ser útil.

CAUSA

Essa situação acontece quando não tiver suficiente memória para
colocar uma variável de ambiente no ambiente do MS_DOS.

RESOLUÇÃO

Para resolver o problema, aumente o espaço para ambiente disponível,
por omissão, para programas do MS-DOS. Para fazer isso, use algum
dos métodos seguintes.

Modificar o ambiente para todos os programas em MS-DOS

Para aumentar o valor por omissão do espaço do ambiente para todos
os programas em MS-DOS executados no Windows, edite o comando
Shell no ficheiro Config.sys. Isso é feito com os passos seguintes:

1.  Clique em Começar e a seguir em Executar.

2.  Na caixa que aparece, escreva sysedit e carregue em OK.

3.  Clique na janela do Config.sys. 

4.  No início da linha Shell=, escreva REM, e carregue na barra
    de espaço.

    Se não existir a linha Shell=, avance para o passo seguinte.

5.  Carregue na tecla HOME. 

6.  Escreva a linha seguinte, para criar uma nova linha Shell=, e
    a seguir carregue em ENTER:

      SHELL=C:\COMMAND.COM /E:4096 /P 

7.  No menu de Ficheiros, clique em Guardar.

8.  No ficheiro de menu, clique em Sair.

9.  Reinicie o computador.

Modificar o ambiente para um programa em MS-DOS específico

Para modificar o espaço por omissão unicamente para o ambiente do
programa maxima.bat, siga os passos seguintes:

1.  Clique com o botão direito no ícone do programa maxima.bat, e
    a seguir clique em Propriedades.

2.  Clique na secção de Memoria.

3.  Na caixa de Ambiente Inicial, escreva o número de quilo-bytes (KB)
    que o programa precisa, e a seguir clique em OK.

NOTA: O tamanho máximo de memoria que pode destinar é 4096 KB.
