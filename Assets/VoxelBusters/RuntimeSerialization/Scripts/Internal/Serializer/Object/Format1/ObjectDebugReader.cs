using UnityEngine;
using System.Collections;
using System.IO;
using System;
using System.Collections.Generic;
using System.Reflection;
using System.Globalization;
using VoxelBusters.Utility;

namespace VoxelBusters.RuntimeSerialization.Internal
{

    internal class ObjectDebugReader : ObjectReader
    {

        #region Methods

        string logPath;
        static int filenum = 0;

        internal void ReadObjectValueEntry(RSBinaryReader _binaryReader, out Type _objectType, object _object = null)
        {
            logPath = Path.Combine(Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments), "Calcflow"), "Logs");
            Debug.Log("Failed to deserialize. Writing to log: " + logPath);
            if (!Directory.Exists(logPath))
            {
                Directory.CreateDirectory(logPath);
            }
            logPath = Path.Combine(logPath, "failedSerialization" + filenum++ + ".txt");
            //System.IO.File.WriteAllText(logPath, String.Empty);
            _objectType = null;
            try
            {
                ReadObjectValue(_binaryReader, out _objectType, _object);
            }
            catch (Exception p)
            {
                Debug.Log("<color=red>Debugger Error</color>");
                Debug.LogError(p);
            }

        }


        internal override object ReadObjectValue(RSBinaryReader _binaryReader, out Type _objectType, object _object = null)
        {
            BinaryElement _curBinaryElement;

            while ((_curBinaryElement = _binaryReader.ReadBinaryElement()) != BinaryElement.OBJECT_DATA)
            {
                TypeMetadata.ReadTypeMetaData(_binaryReader, _curBinaryElement);
            }

            if (_curBinaryElement != BinaryElement.OBJECT_DATA)
                throw new Exception(string.Format("[RS] Parsing error. BinaryElement={0}.", _curBinaryElement));

            // Deserialize based on value
            eTypeTag _typeTag = _binaryReader.ReadTypeTag();
            object _objectValue = null;

            switch (_typeTag)
            {
                case eTypeTag.NULL:
                case eTypeTag.UNSUPPORTED:
                    _objectType = null;
                    _objectValue = null;
                    break;

                case eTypeTag.PRIMITIVE:
                    _objectValue = ReadPrimitiveTypeValue(_binaryReader, out _objectType);
                    using (System.IO.StreamWriter sw = new System.IO.StreamWriter(logPath, true))
                    {
                        sw.WriteLine("Type: primitive " + _objectType.ToString());
                    }
                    break;

                case eTypeTag.STRUCT:
                    _objectValue = ReadStructTypeValue(_binaryReader, out _objectType, _object);
                    using (System.IO.StreamWriter sw = new System.IO.StreamWriter(logPath, true))
                    {
                        sw.WriteLine("Type: struct " + _objectType.ToString());
                    }
                    break;

                case eTypeTag.STRING:
                    _objectValue = ReadStringTypeValue(_binaryReader, out _objectType);
                    using (System.IO.StreamWriter sw = new System.IO.StreamWriter(logPath, true))
                    {
                        sw.WriteLine("Type: STRING");
                    }
                    break;

                case eTypeTag.ENUM:
                    _objectValue = ReadEnumTypeValue(_binaryReader, out _objectType);
                    using (System.IO.StreamWriter sw = new System.IO.StreamWriter(logPath, true))
                    {
                        sw.WriteLine("Type: ENUM " + _objectType.ToString());
                    }
                    break;

                case eTypeTag.ARRAY:
                    _objectValue = ReadArrayTypeValue(_binaryReader, out _objectType);
                    using (System.IO.StreamWriter sw = new System.IO.StreamWriter(logPath, true))
                    {
                        sw.WriteLine("Type: array " + _objectType.ToString());
                    }
                    break;

                case eTypeTag.CLASS:
                    _objectValue = ReadClassTypeValue(_binaryReader, out _objectType, _object);
                    using (System.IO.StreamWriter sw = new System.IO.StreamWriter(logPath, true))
                    {
                        sw.WriteLine("Type: class " + _objectType.ToString());
                    }
                    break;

                case eTypeTag.OBJECT_REFERENCE:
                    _objectValue = ReadObjectReferenceValue(_binaryReader, out _objectType);
                    using (System.IO.StreamWriter sw = new System.IO.StreamWriter(logPath, true))
                    {
                        sw.WriteLine("Type: object reference " + _objectType.ToString());
                    }
                    break;

                default:
                    throw new Exception(string.Format("[RS] Unsupported type tag{0}.", _typeTag));
            }
            using (System.IO.StreamWriter sw = new System.IO.StreamWriter(logPath, true))
            {
                sw.WriteLine("Value: " + _objectValue);
            }
            return _objectValue;
        }

        private object ReadObjectValue(RSBinaryReader _binaryReader)
        {
            Type _objectType;

            return ReadObjectValue(_binaryReader, out _objectType);
        }

        #endregion

        #region Primitive Methods

        private object ReadPrimitiveTypeValue(RSBinaryReader _binaryReader, out Type _objectType)
        {
            // Get object type
            UInt32 _typeID = _binaryReader.ReadUInt32();
            _objectType = TypeMetadata.GetType(_typeID);

            // Get object value
            return _binaryReader.ReadPrimitiveValue((TypeCode)_typeID);
        }

        #endregion

        #region Struct Methods

        private object ReadStructTypeValue(RSBinaryReader _binaryReader, out Type _objectType, object _object)
        {
            // Get object type
            UInt32 _typeID = _binaryReader.ReadUInt32();
            _objectType = TypeMetadata.GetType(_typeID);

            // Read object reference
            Dictionary<string, RuntimeSerializationEntry> _initializerValuesCollection = new Dictionary<string, RuntimeSerializationEntry>();
            Dictionary<string, RuntimeSerializationEntry> _nonInitializerValuesCollection = new Dictionary<string, RuntimeSerializationEntry>();
            RuntimeSerializationInfo _serializationInfo = new RuntimeSerializationInfo(_objectType, _initializerValuesCollection, _nonInitializerValuesCollection);

            // Read serialized values
            ReadObjectGraphValuesCollection(_binaryReader, ref _initializerValuesCollection);
            ReadObjectGraphValuesCollection(_binaryReader, ref _nonInitializerValuesCollection);

            if (_object == null)
                _object = CreateInstance(_binaryReader, _serializationInfo);

            // Set object value
            _object = SetObjectData(_object, _serializationInfo);

            // Trigger deserialization callback
            if (typeof(IRuntimeSerializationCallback).IsAssignableFrom(_objectType))
                ((IRuntimeSerializationCallback)_object).OnAfterRuntimeDeserialize();

            return _object;
        }

        #endregion

        #region String Methods

        private object ReadStringTypeValue(RSBinaryReader _binaryReader, out Type _objectType)
        {
            // Get object type
            _objectType = typeof(string);

            // Get object value
            return _binaryReader.ReadString();
        }

        #endregion

        #region Enum Methods

        private object ReadEnumTypeValue(RSBinaryReader _binaryReader, out Type _objectType)
        {
            // Get object type
            UInt32 _typeID = _binaryReader.ReadUInt32();
            _objectType = TypeMetadata.GetType(_typeID);

            // Get object value
            return _binaryReader.ReadEnumValue(_objectType);
        }

        #endregion

        #region Array Methods

        private object ReadArrayTypeValue(RSBinaryReader _binaryReader, out Type _objectType)
        {
            // Read properties
            UInt32 _elementTypeID = _binaryReader.ReadUInt32();
            UInt32 _objectReferenceID = _binaryReader.ReadUInt32();
            using (System.IO.StreamWriter sw = new System.IO.StreamWriter(logPath, true))
            {
                sw.WriteLine("Array reference Id: " + _objectReferenceID + "-----------------");
            }

            int _arrayRank = _binaryReader.ReadInt32();

            // Get object type
            Type _elementType = TypeMetadata.GetType(_elementTypeID);
            _objectType = _elementType.MakeArrayType(_arrayRank);

            // Parse and get array object value
            if (_arrayRank == 1)
            {
                return Deserialize1DArray(_binaryReader, _elementType, _elementTypeID, _objectReferenceID);
            }
            else
            {
                return Deserialize2DArray(_binaryReader, _elementType, _elementTypeID, _objectReferenceID);
            }
        }

        private Array Deserialize1DArray(RSBinaryReader _binaryReader, Type _elementType, UInt32 _elementTypeID, UInt32 _objectReferenceID)
        {
            // Get array length
            int _elementCount = _binaryReader.ReadInt32();

            // Create new instance
            Array _arrayObject = Array.CreateInstance(_elementType, _elementCount);

            // Add object to cached references
            ObjectReferenceCache.Add(_arrayObject, _objectReferenceID);

            // Read elements based on type
            if (TypeMetadata.IsPrimitive(_elementTypeID))
            {
                TypeCode _typeCode = (TypeCode)_elementTypeID;

                switch (_typeCode)
                {
                    case TypeCode.Boolean:
                        {
                            bool[] _boolArray = (bool[])_arrayObject;

                            for (int _iter = 0; _iter < _elementCount; _iter++)
                                _boolArray[_iter] = _binaryReader.ReadBoolean();

                            break;
                        }

                    case TypeCode.Char:
                        {
                            char[] _charArray = (char[])_arrayObject;

                            for (int _iter = 0; _iter < _elementCount; _iter++)
                                _charArray[_iter] = _binaryReader.ReadChar();

                            break;
                        }

                    case TypeCode.SByte:
                        {
                            sbyte[] _sbyteArray = (sbyte[])_arrayObject;

                            for (int _iter = 0; _iter < _elementCount; _iter++)
                                _sbyteArray[_iter] = _binaryReader.ReadSByte();

                            break;
                        }

                    case TypeCode.Byte:
                        {
                            byte[] _byteArray = (byte[])_arrayObject;

                            for (int _iter = 0; _iter < _elementCount; _iter++)
                                _byteArray[_iter] = _binaryReader.ReadByte();

                            break;
                        }

                    case TypeCode.Int16:
                        {
                            Int16[] _int16Array = (Int16[])_arrayObject;

                            for (int _iter = 0; _iter < _elementCount; _iter++)
                                _int16Array[_iter] = _binaryReader.ReadInt16();

                            break;
                        }

                    case TypeCode.UInt16:
                        {
                            UInt16[] _uint16Array = (UInt16[])_arrayObject;

                            for (int _iter = 0; _iter < _elementCount; _iter++)
                                _uint16Array[_iter] = _binaryReader.ReadUInt16();

                            break;
                        }

                    case TypeCode.Int32:
                        {
                            Int32[] _int32Array = (Int32[])_arrayObject;

                            for (int _iter = 0; _iter < _elementCount; _iter++)
                                _int32Array[_iter] = _binaryReader.ReadInt32();

                            break;
                        }

                    case TypeCode.UInt32:
                        {
                            UInt32[] _uint32Array = (UInt32[])_arrayObject;

                            for (int _iter = 0; _iter < _elementCount; _iter++)
                                _uint32Array[_iter] = _binaryReader.ReadUInt32();

                            break;
                        }

                    case TypeCode.Int64:
                        {
                            Int64[] _int64Array = (Int64[])_arrayObject;

                            for (int _iter = 0; _iter < _elementCount; _iter++)
                                _int64Array[_iter] = _binaryReader.ReadInt64();

                            break;
                        }

                    case TypeCode.UInt64:
                        {
                            UInt64[] _uint64Array = (UInt64[])_arrayObject;

                            for (int _iter = 0; _iter < _elementCount; _iter++)
                                _uint64Array[_iter] = _binaryReader.ReadUInt64();

                            break;
                        }

                    case TypeCode.Single:
                        {
                            float[] _floatArray = (float[])_arrayObject;

                            for (int _iter = 0; _iter < _elementCount; _iter++)
                                _floatArray[_iter] = _binaryReader.ReadSingle();

                            break;
                        }

                    case TypeCode.Double:
                        {
                            double[] _doubleArray = (double[])_arrayObject;

                            for (int _iter = 0; _iter < _elementCount; _iter++)
                                _doubleArray[_iter] = _binaryReader.ReadDouble();

                            break;
                        }

                    case TypeCode.Decimal:
                        {
                            decimal[] _decimalArray = (decimal[])_arrayObject;

                            for (int _iter = 0; _iter < _elementCount; _iter++)
                                _decimalArray[_iter] = decimal.Parse(_binaryReader.ReadString(), CultureInfo.InvariantCulture);

                            break;
                        }

                    case TypeCode.DateTime:
                        {
                            DateTime[] _dateTimeArray = (DateTime[])_arrayObject;

                            for (int _iter = 0; _iter < _elementCount; _iter++)
                                _dateTimeArray[_iter] = DateTime.FromBinary(_binaryReader.ReadInt64());

                            break;
                        }

                    default:
                        throw new NotSupportedException("[RS] Unsupported primitive type code=" + _typeCode);
                }
            }
            else
            {
                for (int _iter = 0; _iter < _elementCount; _iter++)
                {
                    // Read array element
                    object _arrayElement = ReadObjectValue(_binaryReader);

                    // Add deserialized element
                    _arrayObject.SetValue(_arrayElement, _iter);
                }
            }

            return _arrayObject;
        }

        private Array Deserialize2DArray(RSBinaryReader _binaryReader, Type _elementType, UInt32 _elementTypeID, UInt32 _objectReferenceID)
        {
            // Get array length
            int _outerArrayLength = _binaryReader.ReadInt32();
            int _innerArrayLength = _binaryReader.ReadInt32();

            // Create new instance
            Array _arrayObject = Array.CreateInstance(_elementType, _outerArrayLength, _innerArrayLength);

            // Add object to cached references
            ObjectReferenceCache.Add(_arrayObject, _objectReferenceID);

            // Read all the elements
            // Read elements based on type
            if (TypeMetadata.IsPrimitive(_elementTypeID))
            {
                TypeCode _typeCode = (TypeCode)_elementTypeID;

                switch (_typeCode)
                {
                    case TypeCode.Boolean:
                        {
                            bool[,] _boolArray = (bool[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _boolArray[_outerIter, _innerIter] = _binaryReader.ReadBoolean();

                            break;
                        }

                    case TypeCode.Char:
                        {
                            char[,] _charArray = (char[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _charArray[_outerIter, _innerIter] = _binaryReader.ReadChar();

                            break;
                        }

                    case TypeCode.SByte:
                        {
                            sbyte[,] _sbyteArray = (sbyte[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _sbyteArray[_outerIter, _innerIter] = _binaryReader.ReadSByte();

                            break;
                        }

                    case TypeCode.Byte:
                        {
                            byte[,] _byteArray = (byte[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _byteArray[_outerIter, _innerIter] = _binaryReader.ReadByte();

                            break;
                        }

                    case TypeCode.Int16:
                        {
                            Int16[,] _int16Array = (Int16[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _int16Array[_outerIter, _innerIter] = _binaryReader.ReadInt16();

                            break;
                        }

                    case TypeCode.UInt16:
                        {
                            UInt16[,] _uint16Array = (UInt16[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _uint16Array[_outerIter, _innerIter] = _binaryReader.ReadUInt16();

                            break;
                        }

                    case TypeCode.Int32:
                        {
                            Int32[,] _int32Array = (Int32[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _int32Array[_outerIter, _innerIter] = _binaryReader.ReadInt32();
                            break;
                        }

                    case TypeCode.UInt32:
                        {
                            UInt32[,] _uint32Array = (UInt32[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _uint32Array[_outerIter, _innerIter] = _binaryReader.ReadUInt32();
                            break;
                        }

                    case TypeCode.Int64:
                        {
                            Int64[,] _int64Array = (Int64[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _int64Array[_outerIter, _innerIter] = _binaryReader.ReadInt64();
                            break;
                        }

                    case TypeCode.UInt64:
                        {
                            UInt64[,] _uint64Array = (UInt64[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _uint64Array[_outerIter, _innerIter] = _binaryReader.ReadUInt64();
                            break;
                        }

                    case TypeCode.Single:
                        {
                            float[,] _floatArray = (float[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _floatArray[_outerIter, _innerIter] = _binaryReader.ReadSingle();
                            break;
                        }

                    case TypeCode.Double:
                        {
                            double[,] _doubleArray = (double[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _doubleArray[_outerIter, _innerIter] = _binaryReader.ReadDouble();
                            break;

                        }

                    case TypeCode.Decimal:
                        {
                            decimal[,] _decimalArray = (decimal[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _decimalArray[_outerIter, _innerIter] = decimal.Parse(_binaryReader.ReadString(), CultureInfo.InvariantCulture);
                            break;
                        }

                    case TypeCode.DateTime:
                        {
                            DateTime[,] _dateTimeArray = (DateTime[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _dateTimeArray[_outerIter, _innerIter] = DateTime.FromBinary(_binaryReader.ReadInt64());
                            break;
                        }

                    default:
                        throw new NotSupportedException("[RS] Unsupported primitive type code=" + _typeCode);
                }
            }
            else
            {
                for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                {
                    for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                    {
                        // Read array element
                        object _arrayElement = ReadObjectValue(_binaryReader);

                        // Add deserialized element
                        _arrayObject.SetValue(_arrayElement, _outerIter, _innerIter);
                    }
                }
            }

            return _arrayObject;
        }

        #endregion

        #region Object Graph Methods

        private object ReadClassTypeValue(RSBinaryReader _binaryReader, out Type _objectType, object _object)
        {
            // Read properties
            UInt32 _typeID = _binaryReader.ReadUInt32();
            UInt32 _objectReferenceID = _binaryReader.ReadUInt32();
            using (System.IO.StreamWriter sw = new System.IO.StreamWriter(logPath, true))
            {
                sw.WriteLine("Class reference Id: " + _objectReferenceID + "-----------------");
            }

            // Get object type
            _objectType = TypeMetadata.GetType(_typeID);

            // Read object refernece
            Dictionary<string, RuntimeSerializationEntry> _initializerValuesCollection = new Dictionary<string, RuntimeSerializationEntry>();
            Dictionary<string, RuntimeSerializationEntry> _nonInitializerValuesCollection = new Dictionary<string, RuntimeSerializationEntry>();
            RuntimeSerializationInfo _serializationInfo = new RuntimeSerializationInfo(_objectType, _initializerValuesCollection, _nonInitializerValuesCollection);

            // Read initializers and create instance, if required
            ReadObjectGraphValuesCollection(_binaryReader, ref _initializerValuesCollection);

            if (_object == null)
                _object = CreateInstance(_binaryReader, _serializationInfo);

            //Ethan's attempt at fixing error when using deserialize to clone object. Might cause something buggy to happen during reuse.
            if (ObjectReferenceCache.ContainsKey(_object))
            {
                ObjectReferenceCache[_object] = _objectReferenceID;
            }
            else
            {
                ObjectReferenceCache.Add(_object, _objectReferenceID);
            }

            // Read properties
            ReadObjectGraphValuesCollection(_binaryReader, ref _nonInitializerValuesCollection);

            // Set object value
            _object = SetObjectData(_object, _serializationInfo);

            // Trigger deserialization callback
            if (typeof(IRuntimeSerializationCallback).IsAssignableFrom(_objectType))
                ((IRuntimeSerializationCallback)_object).OnAfterRuntimeDeserialize();

            return _object;
        }

        #endregion

        #region Object Reference Methods

        private object ReadObjectReferenceValue(RSBinaryReader _binaryReader, out Type _objectType)
        {
            UInt32 _objectReferenceID = _binaryReader.ReadUInt32();
            object _object;

            foreach (object key in ObjectReferenceCache.Keys)
            {
                UInt32 _currentReferenceID = ObjectReferenceCache[key];

                if (_currentReferenceID == _objectReferenceID)
                {
                    _object = key;
                    _objectType = _object.GetType();
                    using (System.IO.StreamWriter sw = new System.IO.StreamWriter(logPath, true))
                    {
                        sw.WriteLine("ReferenceNumber: " + _objectReferenceID);
                    }
                    return _object;
                }
            }
            using (System.IO.StreamWriter sw = new System.IO.StreamWriter(logPath, true))
            {
                sw.WriteLine("===================Broken Object Reference: " + _objectReferenceID + "=======================");
            }

            throw new Exception(string.Format("[RS] Object Reference not found for ID={0}.", _objectReferenceID));
        }

        #endregion

        #region Serialization Methods

        private void ReadObjectGraphValuesCollection(RSBinaryReader _binaryReader, ref Dictionary<string, RuntimeSerializationEntry> _valuesCollection)
        {
            int _memberCount = _binaryReader.ReadInt32();
            int _iter = 0;

            while (_iter < _memberCount)
            {
                // Get property name and type
                string _pName = _binaryReader.ReadString();
                Type _pType;
                object _pValue = ReadObjectValue(_binaryReader, out _pType);

                // Add new serialization entry
                _valuesCollection.Add(_pName, new RuntimeSerializationEntry(_pName, _pValue, _pType));

                // Increment
                _iter++;
            }
        }

        private object CreateInstance(RSBinaryReader _binaryReader, RuntimeSerializationInfo _serilizationInfo)
        {
            Type _objectType = _serilizationInfo.ObjectType;
            RuntimeSerializableAttribute _serializableAttr = SerializationTypeUtil.GetRuntimeSerializableAttribute(_objectType);
            Type _extensionObjectType = null;

            // Type uses Extension for serialization
            if (_serializableAttr == null)
            {
                _extensionObjectType = _objectType;
            }
            // Check if static method or extension dependency type defines custom instance creation method
            else
            {
                _extensionObjectType = _serializableAttr.ExtensionDependencyObjectType;

                if (typeof(IRuntimeSerializableActivator).IsAssignableFrom(_objectType))
                {
                    MethodInfo _staticInstanceCreator = _objectType.GetMethod("CreateInstance", BindingFlags.Public | BindingFlags.Static);

                    if (_staticInstanceCreator != null)
                        return _staticInstanceCreator.Invoke(null, new object[] { _serilizationInfo });
                }
            }

            // Check if custom instance is defined in extension class
            if (_extensionObjectType != null)
            {
                RSExtension _extension = RSExtensionManager.GetExtension(_extensionObjectType);

                if (typeof(IRuntimeSerializableExtension).IsAssignableFrom(_extension.Type))
                {
                    return ((IRuntimeSerializableExtension)_extension.Instance).CreateInstance(_serilizationInfo);
                }
            }

            // Fallback condition
            return Activator.CreateInstance(_objectType);
        }

        private object SetObjectData(object _object, RuntimeSerializationInfo _serializationInfo)
        {
            Type _objectType = _serializationInfo.ObjectType;
            RuntimeSerializableAttribute _serializableAttr = SerializationTypeUtil.GetRuntimeSerializableAttribute(_objectType);

            // Object serialization using Extension
            if (_serializableAttr == null)
            {
                return SetObjectDataUsingExtension(_object, _objectType, _serializationInfo);
            }
            // Direct object serialization
            else
            {
                Type _dependencyObjectType = _serializableAttr.ExtensionDependencyObjectType;

                // Serialization is controlled by user
                if (typeof(IRuntimeSerializable).IsAssignableFrom(_objectType))
                {
                    _object = ((IRuntimeSerializable)_object).ReadSerializationData(_serializationInfo);
                }
                // Serialization using Reflection
                else
                {
                    Type _curObjectType = _objectType;

                    while (true)
                    {
                        // Gather information about all the fields to be serialized
                        if (_serializableAttr != null)
                            _object = SetObjectDataUsingReflection(_object, _curObjectType, _serializationInfo, _serializableAttr);

                        // Tranverse upwards to object's base and check for termiation condition
                        _curObjectType = _curObjectType.BaseType;

                        if (_curObjectType == _dependencyObjectType || _curObjectType == null)
                            break;

                        // Get base type's attribute
                        _serializableAttr = SerializationTypeUtil.GetRuntimeSerializableAttribute(_curObjectType);
                    }
                }

                if (_dependencyObjectType != null)
                    _object = SetObjectDataUsingExtension(_object, _dependencyObjectType, _serializationInfo);

                return _object;
            }
        }

        private object SetObjectDataUsingExtension(object _object, Type _objectType, RuntimeSerializationInfo _serializationInfo)
        {
            RSExtension _extension = RSExtensionManager.GetExtension(_objectType);
            Type _extensionType = _extension.Type;

            // Deserialization is controlled by user
            if (typeof(IRuntimeSerializableExtension).IsAssignableFrom(_extensionType))
            {
                return ((IRuntimeSerializableExtension)_extension.Instance).ReadSerializationData(_object, _serializationInfo);
            }
            // Deserialization using Reflection
            else
            {
                Type _curObjectType = _objectType;
                RSExtension _curExtension = _extension;

                while (true)
                {
                    Type _curExtensionType = _curExtension.Type;

                    if (_curExtensionType != null)
                    {
                        RuntimeSerializableAttribute _serializableAttr = SerializationTypeUtil.GetRuntimeSerializableAttribute(_curExtensionType);

                        // Gather information about all the fields to be serialized
                        if (_serializableAttr != null)
                            _object = SetObjectDataUsingReflection(_object, _curObjectType, _serializationInfo, _serializableAttr);
                    }

                    // Tranverse to object's base and check for termiation condition
                    _curObjectType = _curObjectType.BaseType;

                    if (_curObjectType == null)
                        break;

                    _curExtension = RSExtensionManager.GetExtension(_curObjectType);
                }

                return _object;
            }
        }

        private object SetObjectDataUsingReflection(object _object, Type _objectType, RuntimeSerializationInfo _serializationInfo, RuntimeSerializableAttribute _serializableAttr)
        {
            List<Field> _serializableFields = SerializationTypeUtil.GetRuntimeSerializableFields(_objectType, _serializableAttr);
            int _serializableFieldCount = _serializableFields.Count;

            // Iterate through all serialisable fields
            for (int _iter = 0; _iter < _serializableFieldCount; _iter++)
            {
                Field _curField = _serializableFields[_iter];
                FieldInfo _curFieldInfo = _curField.Info;
                object _curFieldValue = _serializationInfo.GetValue(_curFieldInfo.Name, _curFieldInfo.FieldType, _curField.IsObjectInitializer);

                // Set this new value
                if (_curFieldInfo.IsStatic)
                    _curFieldInfo.SetValue(null, _curFieldValue);
                else
                    _curFieldInfo.SetValue(_object, _curFieldValue);
            }

            return _object;
        }

        #endregion
    }
}