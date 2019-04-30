using System;
using System.Collections.Generic;

using Nanome.Core.Extension;

namespace Nanome.Core.Importer
{
    public class Ini
    {

        /**
         * Parsed data structure
         */
        public class Values
        {

            private Dictionary<string, object> data;

            public Values()
            {
                data = new Dictionary<string, object>();
            }

            public Values(Values toCopy)
            {
                data = new Dictionary<string, object>(toCopy.data);
            }

            public bool has(string key)
            {
                return data.ContainsKey(key);
            }

            public void set(string key, object val)
            {
                data[key] = val;
            }

            public object get(string key, string def = null)
            {
                if (has(key))
                {
                    return data[key];
                }
                return def;
            }

            public List<string> keys()
            {
                return new List<string>(data.Keys);
            }

        }

        /**
         * Easiest way to do asynchronous loading
         */
        public delegate void ImportFileDelegate(Values result);
        public static void importFileInThread(string path, ImportFileDelegate callback)
        {
            Async main = Async.runInThread(delegate (Async thread)
            {
                Values result = Ini.importFile(path);
                thread.pushEvent("INILoaded", result);
            });
            main.onEvent("INILoaded", delegate (object datas)
            {
                Values result = (Values)datas;
                callback(result);
            });
        }

        // Simple entry point
        public static Values importFile(string path)
        {
            string content = File.readFileContent(path);
            if (content != null)
            {
                return importString(content);
            }
            return null;
        }

        // String entry point
        public static Values importString(string ini)
        {
            Values results = new Values();
            string[] lines = ini.Split(new string[] { "\r\n", "\n" }, StringSplitOptions.None);
            foreach (string line in lines)
            {
                // Remove comments chunks
                string[] parts = line.Split(";");
                // Read key+value
                string[] elems = parts[0].Split("=", true, 1);
                if (elems.Length >= 2)
                {
                    string key = elems[0].Trim();
                    string val = elems[1].Trim();
                    results.set(key, val);
                }
            }
            return results;
        }


        public delegate void ExportFileDelegate(bool success);
        public static void exportFileInThread(string path, Values values, ExportFileDelegate callback = null)
        {
            Async main = Async.runInThread(delegate (Async thread)
            {
                try
                {
                    Ini.exportFile(path, values);
                    thread.pushEvent("INIExported", true);
                }
                catch (Exception exc)
                {
                    Logs.warning("Could not save INI file", exc);
                    thread.pushEvent("INIExported", false);
                }
            });
            main.onEvent("INIExported", delegate (object datas)
            {
                bool result = (bool)datas;
                if (callback != null)
                {
                    callback(result);
                }
            });
        }

        public static void exportFile(string path, Values values)
        {
            var lines = new List<string>();
            foreach (var key in values.keys())
            {
                var value = values.get(key, "");
                lines.Add(key + " = " + value);
            }
            lines.Add("");
            File.writeFileLines(path, lines);
        }

    }

}
