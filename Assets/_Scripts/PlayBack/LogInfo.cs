using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using System;
//using VoxelBusters.Utility;
using Nanome.Core;

[Serializable]
public class LogInfo
{
    #region Properties

    public Type ObjectType
    {
        get;
        private set;
    }

    [SerializeField]
    internal List<string> keys = new List<string>();
    [SerializeField]
    internal List<LogInfoEntry> entries = new List<LogInfoEntry>();

    [SerializeField]
    public LogInfoEntry testEntry = new LogInfoEntry("testEntry", Quaternion.identity, Quaternion.identity.GetType());
    public int MemberCount
    {
        get
        {
            Debug.Log(keys.Count == entries.Count);
            return keys.Count;
        }
    }

    #endregion

    #region Constructors

    public LogInfo()
    {
    }


    #endregion

    #region Add Value Methods


    public void AddValue<T>(string _name, T _value)
    {
        AddValue(_name, _value, typeof(T));
        testEntry.Name = "testEntry1";
    }


    public void AddValue(string _name, object _value, Type _valueType)
    {
        LogInfoEntry _newEntry = new LogInfoEntry(_name, _value, _valueType);
        keys.Add(_name);
        entries.Add(_newEntry);
    }

    #endregion

    #region Get Value Methods

    public T GetValue<T>(string _name)
    {
        return (T)GetValue(_name, typeof(T));
    }

    public object GetValue(string _name, Type _type)
    {
        object _value;

        TryGetValue(_name, out _value, _type);

        return _value;
    }

    public bool TryGetValue<T>(string _name, out T _value)
    {
        object _serializedValue;
        bool _success = TryGetValue(_name, out _serializedValue, typeof(T));

        // Update reference value
        _value = (T)_serializedValue;

        return _success;
    }

    public bool TryGetValue(string _name, out object _value, Type _type)
    {
        // Fetch value associated with given name and check target values validity
        LogInfoEntry _entry = null;

        if (keys.Contains(_name))
        {
            for (int i = 0; i < keys.Count; i++)
            {
                if (keys[i] == _name)
                {
                    _entry = entries[i];
                    break;
                }
            }
            if (_entry.Value != null && _type.IsInstanceOfType(_entry.Value))
                _value = _entry.Value;
            else
                _value = _type.DefaultValue();

            return true;
        }

        // Requested value not found
        _value = _type.DefaultValue();

        return false;
    }

    public bool ContainsValue(string _name)
    {
        return keys.Contains(_name);
    }

    #endregion
}

[Serializable]
public class LogInfoEntry
{
    #region Properties

    [SerializeField]
    private string _name;
    [SerializeField]
    public int intValue;
    [SerializeField]
    public UnityEngine.Object objectValue;
    [SerializeField]
    public float floatValue;
    [SerializeField]
    public string stringValue;
    [SerializeField]
    public Vector3 vector3Value;
    [SerializeField]
    public Quaternion quaternionValue;

    [SerializeField]
    private string _type;


    public string Name
    {
        get { return _name; }
        internal set { _name = value; }
    }

    public Type Type
    {
        get
        {
            if (_type != null)
            {
                return Type.GetType(_type);
            }
            else
            {
                return null;
            }
        }
        private set { _type = value.FullName; }
    }

    public object Value
    {
        get
        {
            if (_type == "int")
            {
                return intValue;
            }
            else if (_type == "float")
            {
                return floatValue;
            }
            else if (_type == "string")
            {
                return stringValue;
            }
            else if (_type == "UnityEngine.Vector3")
            {
                return vector3Value;
            }
            else if (_type == "UnityEngine.Quaternion")
            {
                return quaternionValue;
            }
            else if (_type == "UnityEngine.Object")
            {
                return objectValue;
            }
            else
            {
                return null;
            }
        }

        private set { setValue(value); }
    }


    void setValue(object value)
    {
        if (_type == "int")
        {
            intValue = (int)value;
        }
        else if (_type == "float")
        {
            floatValue = (float)value;
        }
        else if (_type == "string")
        {
            stringValue = (string)value;
        }
        else if (_type == "UnityEngine.Vector3")
        {
            vector3Value = (Vector3)value;
        }
        else if (_type == "UnityEngine.Quaternion")
        {
            quaternionValue = (Quaternion)value;
        }
        else if (_type == "UnityEngine.Object")
        {
            objectValue = (UnityEngine.Object)value;
        }
        else
        {
            Debug.Log("unsupported type: " + _type);
        }
    }


    #endregion

    #region Constructors

    internal LogInfoEntry(string name, object value, Type type)
    {
        Name = name;
        Type = type;
        setValue(value);
    }

    #endregion
}



