using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using System;
//using VoxelBusters.Utility;
using Nanome.Core;

[Serializable]
public class LogInfo : ISerializationCallbackReceiver
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
    internal Dictionary<string, object> entryMap = new Dictionary<string, object>();

    public void OnBeforeSerialize() { }
    public void OnAfterSerialize() { }
    public void OnBeforeDeserialize() { }
    public void OnAfterDeserialize()
    {
        for (int i = 0; i < keys.Count; i++)
        {
            entryMap.Add(keys[i], entries[i].Value);
        }
    }

    public int MemberCount
    {
        get
        {
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
    }


    public void AddValue(string _name, object _value, Type _valueType)
    {
        LogInfoEntry _newEntry = new LogInfoEntry(_name, _value, _valueType);
        entryMap.Add(_name, _newEntry);
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
        object _entry = null;

        if (entryMap.ContainsKey(_name))
        {
            _entry = entryMap[_name];
            if (_entry != null && _type.IsInstanceOfType(_entry))
                _value = _entry;
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
        return entryMap.ContainsKey(_name);
    }

    #endregion
}

[Serializable]
public class LogInfoEntry : ISerializationCallbackReceiver
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

    object _value;

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
            return _value;
        }

        private set { setValue(value); }
    }


    void setValue(object value)
    {
        _value = value;
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

    void makeSerializable(object obj, string _type)
    {
        if (_type == "System.Int32")
        {
            intValue = (int)_value;
        }
        else if (_type == "System.Single")
        {
            floatValue = (float)_value;
        }
        else if (_type == "System.String")
        {
            stringValue = (string)_value;
        }
        else if (_type == "UnityEngine.Vector3")
        {
            vector3Value = (Vector3)_value;
        }
        else if (_type == "UnityEngine.Quaternion")
        {
            quaternionValue = (Quaternion)_value;
        }
        else if (_type == "UnityEngine.Object")
        {
            objectValue = (UnityEngine.Object)_value;
        }
        else
        {
            Debug.Log("unsupported type: " + _type);
        }
    }

    public void OnBeforeSerialize()
    {
        makeSerializable(_value, _type);
    }
    public void OnAfterSerialize() { }
    public void OnBeforeDeserialize() { }
    public void OnAfterDeserialize()
    {
        if (_type == "System.Int32")
        {
            _value = intValue;
        }
        else if (_type == "System.Single")
        {
            _value = floatValue;
        }
        else if (_type == "System.String")
        {
            _value = stringValue;
        }
        else if (_type == "UnityEngine.Vector3")
        {
            _value = vector3Value;
        }
        else if (_type == "UnityEngine.Quaternion")
        {
            _value = quaternionValue;
        }
        else if (_type == "UnityEngine.Object")
        {
            _value = objectValue;
        }
        else
        {
            _value = null;
        }
    }
}



