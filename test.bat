"C:\Program Files\Unity 5.6.2\Editor\Unity.exe" -projectpath C:\Users\ExtraButter23\Documents\Calcflow -logFile .\batchlog.log -executeMethod UnitTestBegin.BeginTests -quit

type batchlog.log | FINDSTR /C:"Assertion failed" /C:"Expected:"