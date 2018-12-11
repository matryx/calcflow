
using UnityEngine;
using System;
using System.Collections;
using System.Collections.Generic;

namespace Nanome.Core.Extension
{

    public static class ObjectExtension
    {

        public static object GetValueAsDictionary(this object obj, string key)
        {
            return (object)((Dictionary<string, object>)obj)[key];
        }

        public static string PrettyPrint(this object obj)
        {
            if (obj == null)
            {
                return "NULL";
            }
            if (obj is IList)
            {
                var stringList = new List<string>();
                foreach (var child in (IList)obj)
                {
                    if (stringList.Count < 100)
                    {
                        stringList.Add(child.PrettyPrint());
                    }
                    else
                    {
                        break;
                    }
                }
                return String.Format("[{0}...]", stringList.Join(","));
            }
            if (obj is IDictionary)
            {
                var stringList = new List<string>();
                foreach (var item in (IDictionary)obj)
                {
                    if (stringList.Count < 100)
                    {
                        var entry = (DictionaryEntry)item;
                        stringList.Add(entry.Key.PrettyPrint() + ":" + entry.Value.PrettyPrint());
                    }
                    else
                    {
                        break;
                    }
                }
                return String.Format("{{{0}...}}", stringList.Join(","));
            }
            return obj.ToString();
        }

    }

}