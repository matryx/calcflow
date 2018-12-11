using System;
using System.Collections.Generic;
using System.IO;
using UnityEngine;

using Nanome.Core.Extension;

namespace Nanome.Core
{

    public class File
    {

        public class Path
        {

            static object locker = new object();

            static string dataPath = null;
            static string persistentDataPath = null;
            static string documentsDataPath = null;

            public static void init()
            {
                lock (locker)
                {
                    if (dataPath == null)
                    {
                        dataPath = Application.dataPath;
                    }
                    if (persistentDataPath == null)
                    {
                        persistentDataPath = Application.persistentDataPath;
                    }
                }
            }

            public static string extractFilename(string path)
            {
                var fileFullName = System.IO.Path.GetFileName(absolute(path));
                var parts = fileFullName.Split(".");
                return parts[0];
            }

            public static string extractExtension(string path)
            {
                var fileFullName = System.IO.Path.GetFileName(absolute(path));
                var parts = fileFullName.Split(".");
                return parts[parts.Length - 1];
            }

            public static string extractDirectory(string path)
            {
                return System.IO.Path.GetDirectoryName(absolute(path));
            }

            public static string absolute(string path)
            {
                return System.IO.Path.GetFullPath(path);
            }

            private static Dictionary<string, string> inDocumentsCache = new Dictionary<string, string>();
            public static string inDocuments(string path)
            {
                lock (locker)
                {
                    var cachedPath = (string)null;
                    if (inDocumentsCache.TryGetValue(path, out cachedPath))
                    {
                        return cachedPath;
                    }
                    if (documentsDataPath == null)
                    {
                        documentsDataPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments);
                    }

                    var result = absolute(documentsDataPath + "/" + path);
                    inDocumentsCache[path] = result;
                    return result;
                }
            }

            private static Dictionary<string, string> inProjectCache = new Dictionary<string, string>();
            public static string inProject(string path)
            {
                lock (locker)
                {
                    var cachedPath = (string)null;
                    if (inProjectCache.TryGetValue(path, out cachedPath))
                    {
                        return cachedPath;
                    }
                    if (dataPath == null)
                    {
                        dataPath = Application.dataPath;
                    }
                    var result = absolute(dataPath + "/../" + path);
                    inProjectCache[path] = result;
                    return result;
                }
            }

            private static Dictionary<string, string> inStorageCache = new Dictionary<string, string>();
            public static string inStorage(string path)
            {
                lock (locker)
                {
                    var cachedPath = (string)null;
                    if (inStorageCache.TryGetValue(path, out cachedPath))
                    {
                        return cachedPath;
                    }
                    if (persistentDataPath == null)
                    {
                        persistentDataPath = Application.persistentDataPath;
                    }
                    var result = absolute(persistentDataPath + "/" + path);
                    inStorageCache[path] = result;
                    return result;
                }
            }

            public static string newTemporaryFile()
            {
                return System.IO.Path.GetTempFileName();
            }

            public static List<string> listInDirectory(string dir, string pattern = "*")
            {
                var di = new DirectoryInfo(dir);
                var files = di.GetFiles(pattern);
                var list = new List<string>();
                foreach (FileInfo file in files)
                {
                    list.Add(absolute(file.FullName));
                }
                return list;
            }

            public static string specialFolder(string name)
            {
                return "";
                // TODO
            }

        }

        /**
         * Reading local files
         */
        static public string readFileContent(string path)
        {
            string result = null;
            using (StreamReader reader = new StreamReader(path))
            {
                result = reader.ReadToEnd();
            }
            return result;
        }

        static public byte[] readFileBytes(string path)
        {
            return System.IO.File.ReadAllBytes(path);
        }

        static public string[] readFileLines(string path)
        {
            var lines = new List<string>();
            using (StreamReader reader = new StreamReader(path))
            {
                var line = reader.ReadLine();
                while (line != null)
                {
                    lines.Add(line);
                    line = reader.ReadLine();
                }
            }
            return lines.ToArray();
        }

        static public void writeFileLines(string path, List<string> lines, string separator = null)
        {
            if (separator == null)
            {
                separator = Environment.NewLine;
            }
            writeFileOverride(path, String.Join(separator, lines.ToArray()));
        }

        /**
         * Writing local files
         */
        static public void writeFileOverride(string path, string content)
        {
            using (StreamWriter writer = new StreamWriter(path, false))
            {
                writer.Write(content);
            }
        }
        static public void writeFileAppend(string path, string content)
        {
            using (StreamWriter writer = new StreamWriter(path, true))
            {
                writer.Write(content);
            }
        }

        static public void writeFileOverride(string path, byte[] content)
        {
            System.IO.File.WriteAllBytes(path, content);
        }

        /**
         * Threaded file writing
         */
        public delegate void WriteFileDelegate(bool success);
        static public void writeFileOverrideInThread(string path, string content, WriteFileDelegate callback)
        {
            Async main = Async.runInThread(delegate (Async thread)
            {
                try
                {
                    File.writeFileOverride(path, content);
                    thread.pushEvent("WroteFileOverride", true);
                }
                catch
                {
                    thread.pushEvent("WroteFileOverride", false);
                }
            });
            main.onEvent("WroteFileOverride", delegate (object datas)
            {
                bool success = (bool)datas;
                callback(success);
            });
        }
        static public void writeFileAppendInThread(string path, string content, WriteFileDelegate callback)
        {
            Async main = Async.runInThread(delegate (Async thread)
            {
                try
                {
                    File.writeFileAppend(path, content);
                    thread.pushEvent("WroteFileAppend", true);
                }
                catch
                {
                    thread.pushEvent("WroteFileAppend", false);
                }
            });
            main.onEvent("WroteFileAppend", delegate (object datas)
            {
                bool success = (bool)datas;
                callback(success);
            });
        }

        static public void writeFileOverrideInThread(string path, byte[] content, WriteFileDelegate callback)
        {
            Async main = Async.runInThread(delegate (Async thread)
            {
                try
                {
                    File.writeFileOverride(path, content);
                    thread.pushEvent("WroteFileOverride", true);
                }
                catch
                {
                    thread.pushEvent("WroteFileOverride", false);
                }
            });
            main.onEvent("WroteFileOverride", delegate (object datas)
            {
                bool success = (bool)datas;
                callback(success);
            });
        }

        static public bool ensureFolderExists(string path)
        {
            try
            {
                if (!Directory.Exists(path))
                {
                    Directory.CreateDirectory(path);
                }
                return true;
            }
            catch (Exception e)
            {
                Logs.errorOnChannel("Nanome.Core", "Could not create directory", path, e);
                return false;
            }
        }

        /**
         * File checks
         */
        static public bool exists(string path)
        {
            return System.IO.File.Exists(Path.absolute(path));
        }

        static public void delete(string path)
        {
            System.IO.File.Delete(Path.absolute(path));
        }

        static public void move(string pathFrom, string pathTo)
        {
            System.IO.File.Move(Path.absolute(pathFrom), Path.absolute(pathTo));
        }

        static public DateTime modified(string path)
        {
            return System.IO.File.GetLastWriteTime(Path.absolute(path));
        }

        static public void tryDelete(string path)
        {
            try
            {
                Nanome.Core.File.delete(path);
            }
            catch (Exception e)
            {
                // We expected it to fail maybe
                Logs.debug("Nanome.Core", "Could not delete", path, e);
            }
        }

        static public DateTime created(string path)
        {
            return System.IO.File.GetCreationTime(path);
        }

    }

}
