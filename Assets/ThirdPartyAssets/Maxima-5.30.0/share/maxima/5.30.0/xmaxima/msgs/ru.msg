# Russian xmaxima translation by S. Semerikov and V. Zhytnikov


### Menu.tcl
mcset ru "Edit" "Редактировать"
mcset ru "Save to a file" "Сохранить в файле"
mcset ru "Error writing to file" "Ошибка записи в файл"
mcset ru "Batch File" "Выполнить пакетный файл"
mcset ru "Open a file to Batch" "Открыть пакетный файл"
mcset ru "Error writing to file:\n%s" "Ошибка записи в файл:\n%s"
mcset ru "File" "Файл"
mcset ru "Batch File Silently" "Выполнить пакетный файл без вывода"
mcset ru "Open a file to Batch Silently" "Открыть файл с помощью batchload"
mcset ru "Save Expressions to File" "Сохранить выражения в файле"
mcset ru "Interrupt" "Прервать"
mcset ru "Restart" "Перезапустить"
mcset ru "Exit" "Выход"
mcset ru "Edit" "Правка"
mcset ru "Cut" "Вырезать"
mcset ru "Copy" "Копировать"
mcset ru "Paste" "Вставить"
mcset ru "Kill" "Отменить"
mcset ru "Yank" "Повторить"
mcset ru "Previous Input" "Предыдущий ввод"
mcset ru "Next Input" "Следующий ввод"
mcset ru "Clear input" "Очистить ввод"
mcset ru "Save Console to File" "Сохранить консоль в файле"
mcset ru "Options" "Настройки"
mcset ru "Toggle Browser Visibility" "Переключить видимость браузера"
mcset ru "Plot Windows" "Графическое окно"
mcset ru "Embedded" "Встроенное"
mcset ru "Separate" "Отдельное"
mcset ru "Multiple" "Множественное"
mcset ru "Preferences" "Настройки"
mcset ru "Show Tcl Console" "Отобразить консоль Tcl"
mcset ru "Clear Memory" "Очистить память"
mcset ru "Input prompt" "Приглашение"
mcset ru "Kill All" "Очистить всё"
mcset ru "Run Tests" "Запуск тестов"
mcset ru "Help" "Справка"
mcset ru "Maxima Help" "Справка по Maxima"
mcset ru "Maxima Homepage" "Домашняя страница Maxima"
mcset ru "Maxima Manual" "Руководство по Maxima"
mcset ru "Maxima Manual (xmaxima browser)" "Руководство по Maxima (браузер xmaxima)"
mcset ru "Maxima Manual (web browser)" "Руководство по Maxima (веб-браузер)"
mcset ru "Xmaxima Manual" "Руководство по Xmaxima"
mcset ru "Xmaxima Manual (xmaxima browser)" "Руководство по Xmaxima (браузер xmaxima)"
mcset ru "Xmaxima Manual (web browser)" "Руководство по Xmaxima (веб-браузер)"
mcset ru "Project Page" "Страница проекта"
mcset ru "Bug Reports" "Для сообщений об ошибках"
mcset ru "About Maxima" "О Maxima"


### EHref.tcl
mcset ru "args to href.  Wanted 1, got:" "аргументов в href.  Ожидается 1, получено:"


### EMaxima.tcl
mcset ru "unable to call" "невозможно вызвать"


### EOpenplot.tcl
mcset ru "not allowed, only" "недопустим, только"


### Bindings.tcl
mcset ru "
Bindings:
<Return>    This sends the current expression (ie where the insert
            cursor is)  for evaluation.
<Linefeed>  (Control-j) This inserts a newline, and is useful
            for entering multiline input.
<Control-k> Kills the current line and puts it in kill ring.
            Successive control-k's append their output together.
<Control-y> Yank out the last kill, Meta-y cycles through previous
            kills.
<Control-g> Interrupts the current computation.
<Alt-p>     Gets the previous input in the history of inputs; if the
            first input is reached, it proceeds to the last input. If
            some characters are written before clicking on Alt-p, only
            history items containing those characters will be considered.
<Alt-n>     Gets the next input in the history of inputs; if the end is
            reached, it proceeds to the first input. If some characters
            are written before clicking on Alt-n, only history items
            containing those characters will be considered.
<Alt-s>     Print again the Maxima input prompt.
" "
Клавиатурные привязки:
<Return>    Вычислить текущее выражение, находящееся в позиции
            курсора.
<Linefeed>  (Control-j) Вставить новую строку; полезно при
            многострочном вводе.
<Control-k> Удалить строку, начиная с текущей позиции, добавляя
            ее в буфер удаленных строк. Последующие нажатия 
	    Control-k's добавляет строки с буферу удалённых строк.
<Control-y> Возвращает последнюю удаленную строку, Meta-y 
            циклически перебирает предшествующие удаленные строки.
<Control-g> Прерывать текущие вычисления.
<Alt-p>     Предыдущая введенная строка, при повторном нажатии выводит
            все предыдущие строки. При достижении первой строки в истстории
            переходит к последней. Если текущая строка ввода не пуста,
            выводит только те строки, которые содержат её как подстроку.
<Alt-n>     Последующая введенная строка, при повторном нажатии выводит
            все последующие строки. При достижении последней строки в истстории
            переходит к первой. Если текущая строка ввода не пуста,
            выводит только те строки, которые содержат её как подстроку.
<Alt-s>     Снова вывести приглашение Maxima.
"


### Browser.tcl
mcset ru "illegal command in rich text:" "Неверная команда в Rich Text:"
mcset ru "unable to evaluate command:" "Невозможно выполнить команду:"
mcset ru "History List" "История посещений"
mcset ru "This window may be dragged elsewhere by grabbing this title bar with the mouse.  Double clicking on a history item, moves to that page." \
         "Это окно может быть перемещено в любое место с помощью захвата мышью его заголовка.   Двойной щелчок на элементе истории приводит к переходу на заданную страницу."
mcset ru "Close" "Закрыть"
mcset ru "Remove the history list" "Удалить историю посещений"
mcset ru "unknown type:" "unknown неизвестный тип:"
mcset ru "error unsupported url type:" "error неподдерживаемый тип url:"
mcset ru "Error connecting to %s on %s\n%s" "Ошибка соединения с %s на порту %s\n%s"
mcset ru "not supported" "не поддерживается"
mcset ru "need version of tk with 'binary' command for images" \
         "Необходима версия Tk с командой 'binary' для изображений"
mcset ru "could not get image" "невозможно получить изображение"
mcset ru "unknown type of image" "неизвестный тип изображения"
mcset ru "cant remove tmp file" "невозможно удалить временный файл"
mcset ru "Unable to open an image for" "Невозможно открыть изображение для"
mcset ru "Opening" "Открываем"
mcset ru "unable to evaluate" "невозможно вычислить"
mcset ru "Font Settings\nThe proportional font is " "Установки шрифтов\nПропорциональный шрифт "
mcset ru "with a size adjustment of " " с корректировкой размера "
mcset ru "\nThe fixed font is " "\nМоноширинный шрифт "
mcset ru "Default nmtp servers  " "nmtp-сервер по умолчанию "
mcset ru "http Proxy host and port:" "http proxy-сервер и порт:"
mcset ru "\nIf you are behind a firewall enter the name of your http proxy host and port,\n eg: `foo.ma.utexas.edu 3128', otherwise leave this blank" \
         "\nЕсли Вы находитесь за брандмауэром, введите имя Вашего http proxy-сервера и порт,\n например: `foo.ma.utexas.edu 3128', в противном случае оставьте это поле пустым"
mcset ru "\nShould plot windows be " "\nГрафическое окно должно быть " 
mcset ru " Apply and Quit " " Применить и выйти "
mcset ru " Apply " " Применить "
mcset ru " Cancel " " Отменить "
mcset ru " Save Preferences " " Сохранить установки "


### Getdata1.tcl
mcset ru "error:" "ошибка:"
mcset ru "writing" "записывается"


### Myhtml.tcl
mcset ru "Internal font error:" "Внутренняя ошибка шрифта:"
mcset ru "bad window" "неверное окно"
mcset ru "not yet" "ещё не"
mcset ru "Click to follow link to" "Нажмите для перехода на ссылку"
mcset ru " Enter search keywords: " " Введите слова для поиска: "
mcset ru "cant decode hex" "невозможно декодировать шестнадцатиричное"


### OpenMath.tcl
mcset ru "Back" "Назад"
mcset ru "Forward" "Вперёд"
mcset ru "Edit" "Редактировать"
mcset ru "Bring down a menu with some edit options" "Выпадающее меню с настройками редактирования"
mcset ru "show edit bar" "показать панель редактирования"
mcset ru "Mark the currently selected region of the text window, so that clicking on the region will cause evaluation by a program specified below." \
         "Помечает текущую выделенную область в текстовом окне, чтобы при щелчке она был вычислена выбранной ниже программой."
mcset ru "mark for eval" "выделить для вычисления"
mcset ru "Set `%s' to be the current type used by 'mark for eval' for making the selected region sensitive to double clicking." \
         "Устанавливает `%s' в качестве текущего типа, используемого 'выделить для вычисления' для установки выделенного участка чувствительным к двойному щелчку."
mcset ru "Options" "Настройки"
mcset ru "File" "Файл"
mcset ru "Offer possible help options, including toggling \n	whether to show balloon help messages" \
         "Установки справки, в т.ч. -\n включение/выключение всплывающих подсказок"
mcset ru "Hide Balloon Help" "Спрятать всплывающие подсказки"
mcset ru "Show Balloon Help" "Показывать всплывающие подсказки"
mcset ru {Menu of file options, for saving and preferences} {Файловое меню - сохранение и настройки}
mcset ru "Reload" "Перезагрузить"
mcset ru "Interrupt" "Прервать"
mcset ru {Try to interrupt the current remote computations.} {Попытаться прервать текущие удаленные вычисления.}
mcset ru "Abort" "Отменить"
mcset ru {Try to abort the current remote computation.} {Попытаться отменить текущие удаленные вычисления.}
mcset ru "Stop" "Остановить"
mcset ru {Stop reading the current url or image.} {Остановить чтение текущего URL или изображения.}
mcset ru "Forget" "Забыть"
mcset ru {Move back one in the history, and remove the current one from the history, unless it was the first window.} \
         {Возврат на предыдущий элемент истории с удалением текущего, если он не первый.}
mcset ru "History" "История"
mcset ru {Display the history list, so that one may be selected by clicking.} \
         {Отображает историю с выбором элемента по нажатию.}
mcset ru "Base Program" "Главная программа"
mcset ru {Show and allow altering of the base program, which shows which is the default host for programs to run on.   May also be specified in <body baseprogram= ...> in the .html file.} \
         {Показывает и позволяет изменять главную программу, которая указывается узлом по умолчанию для работающих на нем программ. Также может быть указана тегом <body baseprogram= ...> в HTML-файле.}
mcset ru "Save to file" "Сохранить в файле"
mcset ru "Fonts" "Шрифты"
mcset ru {set the default font sizes and types} {Устанавливает типы и размеры шрифтов по умолчанию}
mcset ru "Exit" "Выход"
mcset ru {Exit this program} {Выход из программы}
mcset ru \
{Fetch the URL or FILE indicated in the entry box. \
				   A local file is something like file:/home/wfs/foo.om, and a URL \
				   begins with http.} \
{Выбирает URL или файл, указанный в строке ввода. \
				   Локальный файл задается как file:/home/wfs/foo.om, а URL \
				   начинается с http.}

mcset ru {Address of the current document.  You may modify it and type Enter, to fetch a new document.} \
         {Адрес текущего документа. Чтобы выбрать новый документ, введите адрес и нажмите Enter.}
mcset ru "Could not open file %s\n%s" "Невозможно открыть файл %s\n%s"
mcset ru "wrote" "записан"
mcset ru "Double clicking (with the left mouse button), in the marked region will cause evaluation. " \
         "Двойной щелчок левой кнопкой мыши в помеченной области выполнит вычисление. "
mcset ru "No result field with" "Отсутствует поле результата с"
mcset ru "Failed to eval region" "Ошибка вычисления области"
mcset ru "Reload the current URL displayed in the window." "Перезагрузка URL, отображенного в окне."
mcset ru " by %s." " %s"
mcset ru " The result will be inserted." " Результат будет вставлен."
mcset ru " The result field is named" " Наименование результирующего поля"


### Proxy.tcl
mcset ru "Could not connect" "Невозможно соединиться с"


### Wmenu.tcl
mcset ru "needs a window name arg" "аргументом требуется имя окна"
mcset ru "error" "ошибка"
mcset ru "must be one of:" "требуется одно из:"
mcset ru "This is a menu window" "Меню окна"


### Tryembed.tcl
mcset ru "unknown host:" "неизвестный хост:"
mcset ru "connection from %s and %s disallowed" "соединение с %s и %s запрещено"
mcset ru "bad port:" "неверный порт:"
mcset ru "wrong args: socket host port OR socket -server command port" \
	 "неверные аргументы: socket host port ИЛИ socket -server command port"
mcset ru "Cannot change" "Невозможно изменить"
mcset ru "unknown option" "неизвестная опция"


### Tkmaxima.tcl
mcset ru "Usage: xmaxima \[options\] \[url\]" "Использование: xmaxima \[опции\] \[url\]"
mcset ru "           If given, \[url\] will be opened in the help browser instead" "           Если задан \[url\], он будет открыт в окне браузера вместо"
mcset ru "           of the default starting page." "           начальной страницы по умолчанию."
mcset ru "options:" "опции:"
mcset ru "    --help: Display this usage message." "    --help: Отображает это сообщение об использовании."
mcset ru "    -l <lisp>, --lisp=<lisp>: Use lisp implementation <lisp>." "    -l <lisp>, --lisp=<lisp>: Использовать реализацию Lisp <lisp>."
mcset ru "    --use-version=<version>: Launch maxima version <version>." "    --use-version=<version>: Запустить Maxima версии <version>."
mcset ru "Error: arguments %s not understood." "Ошибка: неопознанные аргументы: %s"
mcset ru "Error sourcing %s\n%s" "Ошибка при чтении %s\n%s"
mcset ru "Error starting Maxima:" "Ошибка при запуске Maxima:"


### Send-some.tcl
mcset ru "connection closed" "соединение закрыто"
mcset ru "sending" "отсылка"
mcset ru "on" "на"
mcset ru "had error in" "обнаружена ошибка в"
mcset ru "connecting to" "соединение с"
mcset ru "connected to" "соединено с"
mcset ru "Sending scoket interrupt" "Отправка прерывания сокету"
mcset ru "connecting" "соединение с"
mcset ru "Can't connect to" "Невозможно установить соединение с"
mcset ru "You can try another host by altering Base Program under the \"File\" menu." \
         "Попытайтесь выбрать другой хост в пункте \"Главная программа\" меню \"Файл\"."
	 

### RunMaxima.tcl	 
mcset ru "Could not start Maxima - empty command" "Запуск Maxima невозможен - пустая команда"
mcset ru "Can't execute" "Невозможно выполнить"
mcset ru "Could not open a socket " "Невозможно открыть сокет "
mcset ru "Closed socket" "Закрытый сокет"
mcset ru "Killed process" "Завершенный процесс"
mcset ru "Starting Maxima" "Запуск Maxima"
mcset ru "Starting maxima timed out.  Wait longer?" "Истекло время ожидания запуска Maxima. Подождать еще?"
mcset ru "Starting Maxima timed out" "Истекло время ожидания запуска Maxima"
mcset ru "Failed to start Maxima" "Запуск Maxima невозможен"
mcset ru "Started Maxima" "Maxima запущена"
mcset ru "Error sending to Maxima:" "Ошибка пересылки в Maxima:"
mcset ru "You must Restart" "Необходимо Перезапустить"
mcset ru "You may need to Restart" "Возможно необходимо Перезапустить"
mcset ru "socket closed" "сокет закрыт"
mcset ru "timed out" "таймаут"
mcset ru "Sending signal %s to process %s" "Посылаем сигнал %s процессу %s"


# Russian omplotdata translation by S. Semerikov and V. Zhytnikov


##### Constants.tcl
mcset ru "A URL (universal resource locator) such as http://www.ma.utexas.edu/foo.om" \
         "URL (универсальный локатор ресурса), такой как http://www.ma.utexas.edu/foo.om"
mcset ru "A string to search for, to get an initial position" \
         "Строка поиска для получения начальной позиции"
mcset ru "A regexp to search for, to get an initial position" \
         "Регулярное выражение для поиска начальной позиции"


##### Parse.tcl
mcset ru "parser unrecognized:" "parser не распознал:"

mcset ru \
{
The syntax for the definition of functions is like C, except that it is \
permitted to write x^n instead of pow(x,n).
} \
{
Синтаксис похож на C, за исключением того, что вместо pow(x,n) записывается x^n.
}

mcset ru {
Functions:} {
Функции:}

mcset ru {

Operators:} {

Операторы:}

mcset ru "syntax error" "синтаксическая ошибка"



##### NPlot3d.tcl
mcset ru "from %s to %s requires at least 2 points" "от %s до %s требуется минимум 2 точки"
mcset ru "mismatch" "несоответствие"


##### Getopt.tcl
mcset ru "error" "ошибка"
mcset ru "does not take the key" "не принимает ключ"
mcset ru "is required option for" "требует опцию для"


##### Printops.tcl
mcset ru "Encapsulated PostScript File Options" "Настройки файла Encapsulated PostScript"
mcset ru "Save" "Сохранить"
mcset ru "Cancel" "Отменить"
mcset ru "Left margin (inches)" "Левое поле (дюймы)"
mcset ru "Top margin (inches)" "Верхнее поле (дюймы)"
mcset ru "Printer Setup" "Установки печати"
mcset ru "Save via ftp" "Сохранить по ftp"
mcset ru "Save as Postscript File" "Сохранить как Postscript-файл"
mcset ru "Print To Printer" "Печатать на принтер"
mcset ru "Center on Page" "Выровнять по центру"
mcset ru "Landscape Mode" "Альбомная ориентация"
mcset ru "do margin" "делать отступ"
mcset ru "Postscript filename" "Имя Postscript файла"


##### Paths.tcl
mcset ru "Maxima data directory not found in '%s'" "Каталог с данными (data) Maxima не найден в '%s'"
mcset ru "Documentation not found in '%s'" "Документация не найдена в '%s'"
mcset ru "Starting documentation not found in '%s'" "Введная документация не найдена в '%s'"
mcset ru "Error: Maxima executable not found\n%s\n\n Try setting the environment variable  XMAXIMA_MAXIMA." \
         "Ошибка: исполняемый файл Maxima не найден\n%s\n\n Попытайтесь установить переменную окружения XMAXIMA_MAXIMA."
mcset ru "Error: maxima executable not found." "Ошибка: Исполняемый файл maxima не найден."


##### Plotdf.tcl
mcset ru "A direction field plotter by William Schelter" "Построитель поля направлений Вильяма Шелтера"
mcset ru "Direction Fields" "Поля направлений"
mcset ru "Integrate" "Интегрировать"
mcset ru {Causes clicking on the  plot with the left mouse button at a point, to draw a trajectory passing through that point.   Under Config there is an entry box which allows entering exact x,y coordinates, and which also records the place of the last trajectory computed.} \
         {Нажатие на графике левой кнопкой мыши приводит к рисованию траектории, проходящую через данную точку. В меню Настройка есть поля ввода, позволяющие ввести точные координаты x, y, и указать местоположение последней вычисленной траектории.}
mcset ru "Plot Versus t" "График от t"
mcset ru "Plot the x and y values for the  last trajectory versus t." "Рисует траектории x(t) и y(t)"
mcset ru "X and Y versus t" "X и Y от t"
mcset ru "bad -ode argument:" "неверный параметр -ode :"
mcset ru "\nwant d(y,x)=f(x,y) \n   OR d(x,t)=f(x,y) d(y,t)=g(x,y)" "\nожидается d(y,x)=f(x,y) \n   или d(x,t)=f(x,y) d(y,t)=g(x,y)"
mcset ru \
{
SOLVER/PLOTTER FOR SYSTEMS OF DIFFERENTIAL EQUAITONS

To quit this help click anywhere on this text.

Clicking at a point computes the trajectory (x(t),y(t)) starting at that \
point, and satisfying the differential equations		
      dx/dt = dxdt       dy/dt = dydt

By clicking on Zoom, the mouse will allow you to zoom in on a region \
of the plot. Each click near a point magnifies the plot, keeping the center \
at the point you clicked. Depressing the SHIFT key while clicking \
zooms in the opposite direction. To resume computing trajectories click \
on Integrate.

Clicking on Config will open a menu where several settings can be changed, \
such as the differential equations being solved, the intial point for the \
trajectory to be computed, the direction of integration for that trajectory, \
the time step for each integration interval and the number of integration \
steps (nsteps). Replot is used to update the plot with the \
changes made in the Config menu.

Holding the right mouse button down while moving the mouse will drag \
(translate) the plot sideways or up and down.

The plot can be saved as a postscript file, by clicking on Save.
} {
ОКНО РЕШЕНИЯ И ПОСТРОЕНИЯ ГРАФИКОВ ДИФФЕРЕНЦИАЛЬНЫХ УРАВНЕНИЙ

Для выхода из подсказки шелкните мышкой в любом месте этого текста.

При шелчке мышкой в некоторой точке вычисляется траектория (x(t),y(t)), \
начинающаяся в данной точке и удовлетворяющая уравнениям
      dx/dt = dxdt       dy/dt = dydt

Кнопка Масштабировать позволяет масштабировать график.  Щелчок мышью увеличивает \
изображение, сохраняя центр в точке нажатия.  При удержании \
клавиши SHIFT в момент щелчка изображение умньшается.

При нажатии на Настройка, открывается окно, в котором можно \
изменить некоторые установки, такие как: решаемое уравнение, \
начальная точка и направление вычисляемой траектории, шаг интервала \
интегрирования, количество интегралов интегрирования (nsteps). \
После изменеия установок, для обновления изображения с новыми настройками \
следует нажать Перерисовать.
 
Удержание правой кнопки мыши позволит перемещать \
график вверх или вниз.
 
График может быть сохранен в PostScript файл при щелчке на Сохранить. 
}


##### Plotconf.tcl
mcset ru "Pointer Coordinates" "Координаты указателя"
mcset ru "Menu Here" "Меню"
mcset ru "Dismiss" "Закрыть"
mcset ru "Zoom" "Маштабировать"
mcset ru "Save" "Сохранить"
mcset ru "Mark" "Выделить"
mcset ru "Help" "Справка"
mcset ru "Replot" "Перерисовать"
mcset ru "Config" "Настройка"
mcset ru "Clicking the left mouse button on the plot menu \
	  (top left corner), will bring up a menu.  Holding down \
	  right mouse button and dragging will translate the plot." \
         "Щелчек мышью в левом верхнем уголн окна открывает меню. \
	  Перемещение графика выполняется при нажатой правой кнопке."
mcset ru "Click to Zoom\nShift+Click Unzoom" "Щелчок для увеличения\nShift+щелчок для уменьшения"
mcset ru "Ftp Dialog Box" "Диалоговое окно FTP"
mcset ru "host to write file on" "хост для записи файла "
mcset ru "host to write to via" "хост для записи через "
mcset ru "Your User ID on host" "Ваш UID "
mcset ru "Your password on host" "Ваш пароль "
mcset ru "remote subdirectory for output" "удалённый каталог для вывода"
mcset ru "filename " "имя файла "
mcset ru "chapter " "раздел "
mcset ru "section" "секция "
mcset ru "Problem number" "Номер задачи "
mcset ru "Send it" "Отослать"
mcset ru "Cancel" "Отменить"
mcset ru "Failed to specify" "Не задано"
mcset ru "The variable %s appeared in %s but was not in allowed variables: %s or in parameters: %s" \
         "Переменная %s присутствует в %s, но не является допустимой переменной: %s или параметром: %s"
mcset ru "The form %s may involve variables other than %s or the parameters %s, or the latter may have invalid expressions: %s" \
         "Форма %s может содержать переменные, отличные от %s или параметров %s, или последние могут содержать ошибочные выражения: %s"
mcset ru "only wanted one function:" "ожидается только одна функция:"
mcset ru "different variables" "различные переменные"
mcset ru "Plot Setup" "Установки Графика"
mcset ru "Print Options" "Настройки Печати"


##### Plot2d.tcl
mcset ru "Schelter's 2d Plot Window" "Окно 2D графики Вильяма Шелтера"
mcset ru "Number of mesh grids" "Число делений сетки"
mcset ru \
{

XMAXIMA'S PLOTTER FOR TWO-DIMENSIONAL GRAPHICS

To quit this help click anywhere on this text.

Clicking on Config will open a menu where several settings can be changed, \
such as the function being plotted, the line width, and the \
x and y centers and radii. Replot is used to update the plot with the \
changes made in the Config menu.

By clicking on Zoom, the mouse will allow you to zoom in on a region \
of the plot. Each click near a point magnifies the plot, keeping the center \
at the point you clicked. Depressing the SHIFT key while clicking \
zooms in the opposite direction.

Holding the right mouse button down while moving the mouse will drag \
(translate) the plot sideways or up and down.

The plot can be saved as a postscript file, by clicking on Save.
} \
{

ОКНО 2-МЕРНОЙ ГРАФИКИ XMAXIMA.

Для выхода из подсказки шелкните мышкой в любом месте этого текста.

При нажатии на Настройка, открывается окно, в котором можно \
изменить некоторые установки, такие как: изображаемая функция, \
толщина линий, центры и радиусы для x и y.  После изменеия установок, \
для обновления изображения с новыми настройками следует нажать Перерисовать.
 
Кнопка Масштабировать позволяет масштабировать график.  Щелчок мышью увеличивает \
изображение, сохраняя центр в точке нажатия.  При удержании \
клавиши SHIFT в момент щелчка изображение умньшается.
 
Удержание правой кнопки мыши позволит перемещать \
график вверх или вниз.
 
График может быть сохранен в PostScript файл при щелчке на Сохранить. 
}

mcset ru "Extra Plotting Information" "Дополнительная информация о графике"
mcset ru "Plot Function f(x)" "Рисуемая функция f(x)"
mcset ru "plot color" "Цвет графика"


##### Plot3d.tcl
mcset ru \
{

XMAXIMA'S PLOTTER FOR THREE-DIMENSIONAL GRAPHICS

To quit this help click anywhere on this text.

Clicking on Config will open a menu where several settings can be changed, \
such as the function being plotted, the azimuth and elevation angles, \
and the x and y centers and radii. Replot is used to update the plot with \
the changes made in the Config menu.

By clicking on Zoom, the mouse will allow you to zoom in on a region \
of the plot. Each click near a point magnifies the plot, keeping the center \
at the point you clicked. Depressing the SHIFT key while clicking \
zooms in the opposite direction.

Clicking on Rotate will return the mouse to its default behavior, namely, \
pressing the left mouse button while the mouse is moved will rotate the \
graph.

Holding the right mouse button down while moving the mouse will drag \
(translate) the plot sideways or up and down.

The plot can be saved as a postscript file, by clicking on Save.
} \
{

ОКНО 3-МЕРНОЙ ГРАФИКИ XMAXIMA.

Для выхода из подсказки шелкните мышкой в любом месте этого текста.

При нажатии на Настройка, открывается окно, в котором можно \
изменить некоторые установки, такие как: изображаемая функция, \
азимут и угол места, центры и радиусы для x и y.  После изменеия установок, \
для обновления изображения с новыми настройками следует нажать Перерисовать.
 
Кнопка Масштабировать позволяет масштабировать график.  Щелчок мышью увеличивает \
изображение, сохраняя центр в точке нажатия.  При удержании \
клавиши SHIFT в момент щелчка изображение умньшается.

Кнопка Вращение возвращает мыши поведение по умолчанию - \
движение мыши при удерживаемой правой левой кнопке вызывает \
вращение графика.
 
Удержание правой кнопки мыши позволит перемещать \
график вверх или вниз.
 
График может быть сохранен в PostScript файл при щелчке на Сохранить. 
}

mcset ru "Schelter's 3d Plot Window" "Окно 3D графики Вильяма Шелтера"
mcset ru "Rotate" "Вращение"
mcset ru {Dragging the mouse with the left button depressed will cause the object to rotate.  The rotation keeps the z axis displayed in an upright position (ie parallel to the sides of the screen), but changes the viewpoint.   Moving right and left changes the azimuth (rotation about the z axis), and up and down changes the elevation (inclination of z axis).   The red,blue and green sides of the bounding box are parallel to the X, Y and Z axes, and are on the smaller side.} \
         {Передвижение мыши с нажатой левой кнопкой вызовет вращение объекта. Вращение сохраняет прямое направление оси z (т.е. параллельно сторонам экрана), но изменяет положение наблюдателя. Перемещение вправо и влево меняет азимут (вращение вокруг оси z), вверх и вниз меняет склонение (отклонение от оси z).   Красная, синяя и зеленая стороны ограничивающего параллелепипеда параллельны осям на X, Y и Z, и находятся на меньшей стороне.}
mcset ru "azimuth" "азимут"
mcset ru "elevation" "склонение"


