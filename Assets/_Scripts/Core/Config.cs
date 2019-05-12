using Nanome.Core.Extension;
using Nanome.Core.Importer;
using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace Nanome.Core
{

    public class Config
    {

        static public bool getBool(string key, string def)
        {
            var vv = get(key, def);
            return ((string)vv) == "0" ? false : true;
        }

        static public float getFloat(string key, string def)
        {
            var vv = get(key, def);
            return (float)vv;
        }

        static public int getInt(string key, string def)
        {
            var vv = get(key, def);
            return (int)vv;
        }

        static public string getString(string key, string def)
        {
            var vv = get(key, def);
            return (string)vv;
        }

        static public byte[] getBytes(string key, string def)
        {
            var vv = get(key, def);
            return vv as byte[];
        }

        static public string getFormattedString(string key, Dictionary<string, string> variables, string def)
        {
            var vv = Nanome.Core.Config.getString(key, def);
            if (vv != null)
            {
                foreach (var replaceKey in variables.Keys)
                {
                    var replaceValue = variables[replaceKey];
                    if (replaceValue != null)
                    {
                        vv = vv.Replace("{{" + replaceKey + "}}", variables[replaceKey]);
                    }
                }
            }
            else
            {
                vv = "";
            }
            return vv;
        }

        static public void setBytes(string key, byte[] val, bool save = false, string where = null )
        {
            set(key, val, save);
        }

        static public void setString(string key, string val, bool save = false, string where = null)
        {
            set(key, val, save);
        }

        static public void setInt(string key, int val, bool save = false, string where = null)
        {
            set(key, val.toString(), save);
        }

        static public void setFloat(string key, float val, bool save = false, string where = null)
        {
            set(key, val.toString(), save);
        }

        static public void setBool(string key, bool val, bool save = false, string where = null)
        {
            set(key, val.toString(), save);
        }

        static public bool hasKey(string key)
        {
            var vv = get(key, null);
            return vv != null;
        }

        static private object locker = new object();
        static private Ini.Values values = null;
        static private Ini.Values session = null;


        static private string configPath(string storageType=null)
        {
            switch(storageType)
            {
                case "local":
                    return localConfigPath();
                case "documents":
                    return documentsConfigPath();
                case "storage":
                    return storageConfigPath();
                default:
                    return storageConfigPath();
            }
        }

        static private string localConfigPath()
        {
            return File.Path.inProject("calcflow-config.ini");
        }

        static private string documentsConfigPath()
        {
            return File.Path.inDocuments("Matryx/calcflow-config.ini");
        }

        static private string storageConfigPath()
        {
            return File.Path.inStorage("./Config/calcflow-config-saved.ini");
        }

        static private void loadIfNeeded()
        {
            lock (locker)
            {
                if (Config.values == null)
                {
                    string localConfig = localConfigPath();
                    try
                    {
                        Config.values = Ini.importFile(localConfig);
                    }
                    catch (Exception exc)
                    {
                        Logs.warningOnChannel("Nanome.Core", "Could not load local config file", localConfig, exc);
                        values = new Ini.Values();
                    }
                    string localConfigSaved = storageConfigPath();
                    try
                    {
                        var saveds = Ini.importFile(localConfigSaved);
                        foreach (var key in saveds.keys())
                        {
                            Config.values.set(key, saveds.get(key));
                        }
                        if (session == null)
                        {
                            Config.session = saveds;
                        }
                    }
                    catch (Exception exc)
                    {
                        Logs.debugOnChannel("Nanome.Core", "Could not load saved config file", localConfigSaved, exc);
                        if (session == null)
                        {
                            session = new Ini.Values();
                        }
                    }
                    string documentsConfigSaved = documentsConfigPath();
                    try
                    {
                        var saveds = Ini.importFile(documentsConfigSaved);
                        foreach(var key in saveds.keys())
                        {
                            Config.values.set(key, saveds.get(key));
                        }
                        session = saveds;
                    }
                    catch (Exception exc)
                    {
                        Logs.debugOnChannel("Nanome.Core", "Could not load saved config file", localConfigSaved, exc);
                        if (session == null)
                        {
                            session = new Ini.Values();
                        }
                    }
                }
            }
        }

        static private object get(string key, string def)
        {
            Config.loadIfNeeded();
            return Config.values.get(key, def);
        }

        static private void set(string key, object val, bool save = false, string where=null)
        {
            Config.loadIfNeeded();
            Config.values.set(key, val);
            if (save)
            {
                session.set(key, val);
                Config.save(where);
            }
        }

        static private void save(string where="storage")
        {
            Config.loadIfNeeded();
            var storagePath = configPath(where);
            var storageDirectory = File.Path.extractDirectory(storagePath);
            File.ensureFolderExists(storageDirectory);
            Ini.exportFile(storagePath, Config.session);
        }

    }
}