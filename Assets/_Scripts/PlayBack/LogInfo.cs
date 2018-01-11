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

    internal List<string> keys = new List<string>();
    internal List<LogInfoEntry> entries = new List<LogInfoEntry>();

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
            for(int i = 0; i<keys.Count; i++)
            {
                if (keys[i] == _name){
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

class LogInfoEntry
{
    #region Properties

    public string Name
    {
        get;
        private set;
    }

    public object Value
    {
        get;
        private set;
    }

    public Type Type
    {
        get;
        private set;
    }

    #endregion

    #region Constructors

    internal LogInfoEntry(string _name, object _value, Type _type)
    {
        Name = _name;
        Value = _value;
        Type = _type;
    }

    #endregion
}



