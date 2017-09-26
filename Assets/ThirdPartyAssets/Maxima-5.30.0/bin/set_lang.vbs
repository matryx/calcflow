'******************************************************
'*                                                    *
'*  set_lang.vbs  -- sets LANG environment variable   *
'*                   according to current locale      *
'*                                                    *
'*  > cscript //nologo set_lang.vbs                   *          
'*                                                    *
'******************************************************

LCID = GetLocale

CC=""

Select Case LCID
  ' English
  Case 1033, 2057, 3081, 10249, 4105, 9225, 15369, 16393, 14345, _
       6153, 8201, 17417, 5129, 13321, 18441, 7177, 11273, 12297
    CC="en"

  ' Spanish
  Case 1034, 3082, 11274, 16394, 13322, 9226, 5130, 7178, 12298, _
       17418, 4106, 18442, 58378, 2058, 19466, 6154, 15370, 10250, _
       20490, 21514, 14346, 8202 
    CC="es"

  ' Portuguese
  Case 1046, 2070
    CC="pt"

  ' German
  Case 1031, 3079, 5127, 4103, 2055
    CC="de"

  ' Franch
  Case 1036, 2060, 11276, 3084, 9228, 12300, 15372, 5132, 13324, 6156, _
       14348, 58380, 8204, 10252, 4108, 7180, 1122
    CC="fr"

  ' Italian
  Case 1040, 2064
    CC="it"

  ' Russian
  Case 1049, 2073
    CC="ru"

End Select

'------------------------------------------------------------------------------------------------------------------
'
'  Types of environment:
'    
'    SYSTEM   - Applies to all users of the computer and is saved between logoffs and restarts
'                HKLM\System\CurrentControlSet\Control\Session Manager\Environment    
'
'    USER     - Applies to the user currently logged on to the computer and is saved between logoffs and restarts
'                HKCU\Environment
'
'    VOLATILE - Applies to current logon session and is not saved between logoffs and restarts
'                HKCU\VolatileEnvironment
'
'    PROCESS  - Applies to current process and might be passed to child processes
'                Not stored in the registry
'
'-----------------------------------------------------------------------------------------------------------------

If CC<>"" Then
  Set WS = CreateObject("WScript.Shell")
  Set WPE = WS.Environment("VOLATILE")
  WPE("LANG") = CC
End if

