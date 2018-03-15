"C:\Program Files\Unity 5.6.2\Editor\Unity.exe" -projectpath C:\Users\ExtraButter23\Documents\Calcflow -logFile .\batchlog.log -nographics -batchmode -executeMethod UnitTestBegin.BeginTests

type batchlog.log | FINDSTR /C:"Assertion failed." /C:"Expected:" /C:"AssertMsg:"