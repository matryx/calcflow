Esse  o arquivo README do Windows.

Arquivos binários inclusos nessa distribuição
--------------------------------------------

A versão do Maxima para Windows inclui arquivos binários 
de outros projetos de código aberto também hospedados no Sourceforge.

gcc: 

gcc.exe, cc1.exe e os arquivos nos subdiretórios lib/gcc-lib
e include/ são da versão mingw do gcc.  Essa versão está
disponível  em http://prdownloads.sf.net/mingw/
     

binutils:

as.exe é o port mingw (http://www.mingw.org/) do binutils
disponível em http://prdownloads.sf.net/mingw/


gnuplot:

Os arquivo wgnuplot.exe, wgnuplot.hlp e wgnuplot.mnu são da
distribuição para Windows do gnuplot localizado em http://gnuplot.sourceforge.net


wxMaxima:

Os arquivos no subdiretório wxMaxima são da distribuição para Windows
do wxMaxima disponível em http://wxmaxima.sourceforge.net


Interface Gráfica do Maxima e firewall
--------------------------------------

Algumas vezes a interface gráfica do Maxima (xmaxima ou wxMaxima) não consegue ativar o Maxima
ou mostra uma mensagem de tempo esgotado ou não recebe as respostas dos comandos do Maxima.  
Muito provavelmente o problema é causando pelo firewall e/ou software antivirus.  
A interface gráfica do Maxima conversa com o mecanismo computacional através de um socket.  
Antivirus e/ou programas de firewall vêem e tentam bloqueá-lo 
(porque alguns programas suspeitos abrem sockets também).  

Para resolver o problema:

1.  Tente encontrar o painel de controle para o antivirus e/ou firewall. 

2.  Encontre a interface gráfica do Maxima na lista de programas bloqueados e desabilite
    o bloqueio para o Maxima.  O programa de interface gráfica pode aparecer como "Tcl/Tk" 
    (o nome da coleção de ferramentas utilizadas na montagem da interface gráfica xmaxima).


Out of Environment Space
------------------------

Quando você tenta rodar Maxima em uma máquina Windows 9x
pode aparecer a seguinte mensagem de erro 

    Out of environment space

O seguinte boletim do Microsoft Knowledge Base Article 230205
(http://support.microsoft.com/support/kb/articles/Q230/2/05.ASP)
pode ajudar.

MOTIVO

Essa mensagem pode ocorrer se você não tiver memória livre no ambiente
MS-DOS para disponibilizar uma variável de ambiente. 

SOLUÇÃO

Para resolver essa mensagem, aumente o valor do espaço padrão para o ambiente 
para programas MS-DOS. Para fazer isso, use um dos seguintes métodos. 

Modifique o ambiente para todos os programas MS-DOS

Para aumentar o espaço padrão de ambiente para todos os programas MS-DOS 
rodando no Windows, edite o Shell de comandos no arquivo Config.sys. 
Para fazer isso, siga os passos abaixo: 

1.  Iniciar, e então em Executar. 

2.  Na caixa aberta,digite sysedit, e então clique em OK. 

3.  Clique na Janela do Config.sys. 

4.  No começo da linha contendo Shell=, digite REM, e então pressione
    a barrade espaço. 

    Se a linha contendo Shell= não existir, conforme os passos seguintes. 

5.  Pressione a tecla HOME. 

6.  Digite a seguinte linha para criar uma nova linha contendo Shell=, e então
    pressione ENTER: 

      SHELL=C:\COMMAND.COM /E:4096 /P 

7.  No menu Arquivo, clique em Salvar. 

8.  No menu Arquivo, clique Sair. 

9.  Reinicie o computador. 


Modificar o ambiente para um programa específico do MS-DOS

ara aumentar espaço de ambiente padrão apenas para maxima.bat, siga
esses passos: 

1.  Dê um clique com o botão direito do mouse sobre o ícone do programa maxima.bat, e então clique em Propriedades. 

2.  Clique na guia de Memória. 

3.  Na caixa inicial do Ambiente, digite o número de kilobytes (KB) 
    que o programa requer, e então clique em OK. 

NOTA: O máximo de meória que você pode disponibilizar é 4096 KB.

