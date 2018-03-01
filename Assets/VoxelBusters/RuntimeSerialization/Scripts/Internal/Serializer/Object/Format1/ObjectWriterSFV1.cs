using UnityEngine;
using System.Collections;
using System;
using System.Collections.Generic;
using System.Reflection;
using System.Globalization;
using VoxelBusters.Utility;

namespace VoxelBusters.RuntimeSerialization.Internal
{
    internal class ObjectWriterSFV1 : ObjectWriter
    {
        #region Methods

        internal override void WriteObjectValue(RSBinaryWriter _binaryWriter, object _object)
        {
            if (_object == null || _object.Equals(null))
            {
                WriteUnsupportedTypeValue(_binaryWriter, eTypeTag.NULL);
                return;
            }

            // Handle serialization based on object type tag
            Type _objectType = _object.GetType();
            eTypeTag _typeTag = TypeMetadata.GetTypeTag(_objectType);

            switch (_typeTag)
            {
                case eTypeTag.UNSUPPORTED:
                    Debug.LogWarning(string.Format("[RS] Serialization isnt supported for type={0}.", _objectType));
                    WriteUnsupportedTypeValue(_binaryWriter, _typeTag);
                    break;

                case eTypeTag.PRIMITIVE:
                    WritePrimitiveTypeValue(_binaryWriter, _object, _objectType);
                    break;

                case eTypeTag.STRUCT:
                    WriteStructTypeValue(_binaryWriter, _object, _objectType);
                    break;

                case eTypeTag.STRING:
                    WriteStringTypeValue(_binaryWriter, _object, _objectType);
                    break;

                case eTypeTag.ENUM:
                    WriteEnumTypeValue(_binaryWriter, _object, _objectType);
                    break;

                case eTypeTag.ARRAY:
                case eTypeTag.CLASS:
                    WriteObjectReferenceTypeValue(_binaryWriter, _object, _objectType, _typeTag);
                    break;

                default:
                    Debug.LogWarning(string.Format("[RS] Unknown type={0}.", _typeTag));
                    break;
            }
        }

        #endregion

        #region Unsupported / Null Type Methods

        private void WriteUnsupportedTypeValue(RSBinaryWriter _binaryWriter, eTypeTag _typeTag)
        {
            _binaryWriter.WriteBinaryElement(BinaryElement.OBJECT_DATA);
            _binaryWriter.WriteTypeTag(_typeTag);
        }

        #endregion

        #region Primitive Methods

        private void WritePrimitiveTypeValue(RSBinaryWriter _binaryWriter, object _object, Type _objectType)
        {
            int _typeID = (int)Type.GetTypeCode(_objectType);

            // Write primitive object data
            _binaryWriter.WriteBinaryElement(BinaryElement.OBJECT_DATA);
            _binaryWriter.WriteTypeTag(eTypeTag.PRIMITIVE);
            _binaryWriter.Write(_typeID);
            _binaryWriter.WritePrimitiveValue(_object, (TypeCode)_typeID);
        }

        #endregion

        #region Struct Methods

        private void WriteStructTypeValue(RSBinaryWriter _binaryWriter, object _object, Type _objectType)
        {
            // Register object graph type
            UInt32 _objectTypeID;
            bool _newType;

            TypeMetadata.RegisterType(_objectType, out _objectTypeID, out _newType);

            if (_newType)
                TypeMetadata.WriteTypeMetadata(_binaryWriter, _objectType, _objectTypeID);

            // Write object properties
            _binaryWriter.WriteBinaryElement(BinaryElement.OBJECT_DATA);
            _binaryWriter.WriteTypeTag(eTypeTag.STRUCT);
            _binaryWriter.Write(_objectTypeID);

            // Write object graph
            WriteObjectGraph(_binaryWriter, _object, _objectType);
        }

        #endregion

        #region String Methods

        private void WriteStringTypeValue(RSBinaryWriter _binaryWriter, object _object, Type _objectType)
        {
            // Write string object data
            _binaryWriter.WriteBinaryElement(BinaryElement.OBJECT_DATA);
            _binaryWriter.WriteTypeTag(eTypeTag.STRING);
            _binaryWriter.Write(_object as string);
        }

        #endregion

        #region Enum Methods

        private void WriteEnumTypeValue(RSBinaryWriter _binaryWriter, object _object, Type _enumType)
        {
            // Register enum type
            UInt32 _enumTypeID;
            bool _newType;

            TypeMetadata.RegisterType(_enumType, out _enumTypeID, out _newType);

            if (_newType)
                TypeMetadata.WriteTypeMetadata(_binaryWriter, _enumType, _enumTypeID);

            // Write enum object data
            _binaryWriter.WriteBinaryElement(BinaryElement.OBJECT_DATA);
            _binaryWriter.WriteTypeTag(eTypeTag.ENUM);
            _binaryWriter.Write(_enumTypeID);
            _binaryWriter.WriteEnumValue(_object, _enumType);
        }

        #endregion

        #region Array Methods

        private void WriteArrayTypeValue(RSBinaryWriter _binaryWriter, object _object, Type _arrayType, UInt32 _objectReferenceID)
        {
            Array _arrayObject = _object as Array;
            int _arrayRank = _arrayType.GetArrayRank();

            // Register array element type
            Type _elementType = _arrayType.GetElementType();
            UInt32 _elementTypeID;
            bool _newType;

            TypeMetadata.RegisterType(_elementType, out _elementTypeID, out _newType);

            if (_newType)
                TypeMetadata.WriteTypeMetadata(_binaryWriter, _elementType, _elementTypeID);

            // Write array object properties
            _binaryWriter.WriteBinaryElement(BinaryElement.OBJECT_DATA);
            _binaryWriter.WriteTypeTag(eTypeTag.ARRAY);
            _binaryWriter.Write(_elementTypeID);
            _binaryWriter.Write(_objectReferenceID);
            _binaryWriter.Write(_arrayRank);

            // Write 1D Array properties and elements
            if (_arrayRank == 1)
            {
                Write1DArrayValue(_binaryWriter, _arrayObject, _elementTypeID);
            }
            // Write 2D Array properties and elements
            else
            {
                Write2DArrayValue(_binaryWriter, _arrayObject, _elementTypeID);
            }
        }

        int i = 0;

        private void Write1DArrayValue(RSBinaryWriter _binaryWriter, Array _arrayObject, UInt32 _elementTypeID)
        {
            int _arrayLength = _arrayObject.Length;

            // Write array length
            _binaryWriter.Write(_arrayLength);

            // Write array value
            if (TypeMetadata.IsPrimitive(_elementTypeID))
            {
                TypeCode _typeCode = (TypeCode)_elementTypeID;


                switch (_typeCode)
                {
                    case TypeCode.Boolean:
                        {
                            bool[] _boolArray = (bool[])_arrayObject;

                            for (int _iter = 0; _iter < _arrayLength; _iter++)
                                _binaryWriter.Write(_boolArray[_iter]);

                            break;
                        }

                    case TypeCode.Char:
                        {
                            char[] _charArray = (char[])_arrayObject;

                            for (int _iter = 0; _iter < _arrayLength; _iter++)
                                _binaryWriter.Write(_charArray[_iter]);

                            break;
                        }

                    case TypeCode.SByte:
                        {
                            sbyte[] _sbyteArray = (sbyte[])_arrayObject;

                            for (int _iter = 0; _iter < _arrayLength; _iter++)
                                _binaryWriter.Write(_sbyteArray[_iter]);

                            break;
                        }

                    case TypeCode.Byte:
                        {
                            byte[] _byteArray = (byte[])_arrayObject;

                            for (int _iter = 0; _iter < _arrayLength; _iter++)
                                _binaryWriter.Write(_byteArray[_iter]);

                            break;
                        }

                    case TypeCode.Int16:
                        {
                            Int16[] _int16Array = (Int16[])_arrayObject;

                            for (int _iter = 0; _iter < _arrayLength; _iter++)
                                _binaryWriter.Write(_int16Array[_iter]);

                            break;
                        }

                    case TypeCode.UInt16:
                        {
                            UInt16[] _uint16Array = (UInt16[])_arrayObject;

                            for (int _iter = 0; _iter < _arrayLength; _iter++)
                                _binaryWriter.Write(_uint16Array[_iter]);

                            break;
                        }

                    case TypeCode.Int32:
                        {
                            Int32[] _int32Array = (Int32[])_arrayObject;

                            for (int _iter = 0; _iter < _arrayLength; _iter++)
                                _binaryWriter.Write(_int32Array[_iter]);

                            break;
                        }

                    case TypeCode.UInt32:
                        {
                            UInt32[] _uint32Array = (UInt32[])_arrayObject;

                            for (int _iter = 0; _iter < _arrayLength; _iter++)
                                _binaryWriter.Write(_uint32Array[_iter]);

                            break;
                        }

                    case TypeCode.Int64:
                        {
                            Int64[] _int64Array = (Int64[])_arrayObject;

                            for (int _iter = 0; _iter < _arrayLength; _iter++)
                                _binaryWriter.Write(_int64Array[_iter]);

                            break;
                        }

                    case TypeCode.UInt64:
                        {
                            UInt64[] _uint64Array = (UInt64[])_arrayObject;

                            for (int _iter = 0; _iter < _arrayLength; _iter++)
                                _binaryWriter.Write(_uint64Array[_iter]);

                            break;
                        }

                    case TypeCode.Single:
                        {
                            float[] _floatArray = (float[])_arrayObject;

                            for (int _iter = 0; _iter < _arrayLength; _iter++)
                                _binaryWriter.Write(_floatArray[_iter]);

                            break;
                        }

                    case TypeCode.Double:
                        {
                            double[] _doubleArray = (double[])_arrayObject;

                            for (int _iter = 0; _iter < _arrayLength; _iter++)
                                _binaryWriter.Write(_doubleArray[_iter]);

                            break;
                        }

                    case TypeCode.Decimal:
                        {
                            decimal[] _decimalArray = (decimal[])_arrayObject;

                            for (int _iter = 0; _iter < _arrayLength; _iter++)
                                _binaryWriter.Write(_decimalArray[_iter].ToString(CultureInfo.InvariantCulture));

                            break;
                        }

                    case TypeCode.DateTime:
                        {
                            DateTime[] _dateTimeArray = (DateTime[])_arrayObject;

                            for (int _iter = 0; _iter < _arrayLength; _iter++)
                                _binaryWriter.Write(_dateTimeArray[_iter].ToBinary());

                            break;
                        }

                    default:
                        throw new NotSupportedException("[RS] Unsupported primitive type code=" + _typeCode);
                }
            }
            else
            {
                IEnumerator _enumerator = _arrayObject.GetEnumerator();

                while (_enumerator.MoveNext())
                    WriteObjectValue(_binaryWriter, _enumerator.Current);
            }
        }

        private void Write2DArrayValue(RSBinaryWriter _binaryWriter, Array _arrayObject, UInt32 _elementTypeID)
        {
            int _outerArrayLength = _arrayObject.GetLength(0);
            int _innerArrayLength = _arrayObject.GetLength(1);

            // Write array length
            _binaryWriter.Write(_outerArrayLength);
            _binaryWriter.Write(_innerArrayLength);

            // Write array value
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
                                    _binaryWriter.Write(_boolArray[_outerIter, _innerIter]);

                            break;
                        }

                    case TypeCode.Char:
                        {
                            char[,] _charArray = (char[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _binaryWriter.Write(_charArray[_outerIter, _innerIter]);

                            break;
                        }

                    case TypeCode.SByte:
                        {
                            sbyte[,] _sbyteArray = (sbyte[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _binaryWriter.Write(_sbyteArray[_outerIter, _innerIter]);

                            break;
                        }

                    case TypeCode.Byte:
                        {
                            byte[,] _byteArray = (byte[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _binaryWriter.Write(_byteArray[_outerIter, _innerIter]);

                            break;
                        }

                    case TypeCode.Int16:
                        {
                            Int16[,] _int16Array = (Int16[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _binaryWriter.Write(_int16Array[_outerIter, _innerIter]);

                            break;
                        }

                    case TypeCode.UInt16:
                        {
                            UInt16[,] _uint16Array = (UInt16[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _binaryWriter.Write(_uint16Array[_outerIter, _innerIter]);

                            break;
                        }

                    case TypeCode.Int32:
                        {
                            Int32[,] _int32Array = (Int32[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _binaryWriter.Write(_int32Array[_outerIter, _innerIter]);

                            break;
                        }

                    case TypeCode.UInt32:
                        {
                            UInt32[,] _uint32Array = (UInt32[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _binaryWriter.Write(_uint32Array[_outerIter, _innerIter]);

                            break;
                        }

                    case TypeCode.Int64:
                        {
                            Int64[,] _int64Array = (Int64[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _binaryWriter.Write(_int64Array[_outerIter, _innerIter]);

                            break;
                        }

                    case TypeCode.UInt64:
                        {
                            UInt64[,] _uint64Array = (UInt64[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _binaryWriter.Write(_uint64Array[_outerIter, _innerIter]);

                            break;
                        }

                    case TypeCode.Single:
                        {
                            float[,] _floatArray = (float[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _binaryWriter.Write(_floatArray[_outerIter, _innerIter]);

                            break;
                        }

                    case TypeCode.Double:
                        {
                            double[,] _doubleArray = (double[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _binaryWriter.Write(_doubleArray[_outerIter, _innerIter]);

                            break;
                        }

                    case TypeCode.Decimal:
                        {
                            decimal[,] _decimalArray = (decimal[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _binaryWriter.Write(_decimalArray[_outerIter, _innerIter].ToString(CultureInfo.InvariantCulture));

                            break;
                        }

                    case TypeCode.DateTime:
                        {
                            DateTime[,] _dateTimeArray = (DateTime[,])_arrayObject;

                            for (int _outerIter = 0; _outerIter < _outerArrayLength; _outerIter++)
                                for (int _innerIter = 0; _innerIter < _innerArrayLength; _innerIter++)
                                    _binaryWriter.Write(_dateTimeArray[_outerIter, _innerIter].ToBinary());

                            break;
                        }

                    default:
                        throw new NotSupportedException("[RS] Unsupported primitive type code=" + _typeCode);
                }
            }
            else
            {
                IEnumerator _enumerator = _arrayObject.GetEnumerator();

                while (_enumerator.MoveNext())
                    WriteObjectValue(_binaryWriter, _enumerator.Current);
            }
        }

        #endregion

        #region Object Graph Methods

        private void WriteClassTypeValue(RSBinaryWriter _binaryWriter, object _object, Type _objectType, UInt32 _objectReferenceID)
        {
            // Register object graph type
            UInt32 _objectTypeID;
            bool _newType;

            TypeMetadata.RegisterType(_objectType, out _objectTypeID, out _newType);

            if (_newType)
                TypeMetadata.WriteTypeMetadata(_binaryWriter, _objectType, _objectTypeID);

            // Write object properties
            _binaryWriter.WriteBinaryElement(BinaryElement.OBJECT_DATA);
            _binaryWriter.WriteTypeTag(eTypeTag.CLASS);
            _binaryWriter.Write(_objectTypeID);
            _binaryWriter.Write(_objectReferenceID);

            // Write object graph
            WriteObjectGraph(_binaryWriter, _object, _objectType);
        }

        private void WriteObjectGraph(RSBinaryWriter _binaryWriter, object _object, Type _objectType)
        {

            if (typeof(IRuntimeSerializationCallback).IsAssignableFrom(_objectType))
                ((IRuntimeSerializationCallback)_object).OnBeforeRuntimeSerialize();
            // Fetch properties that needs to be serialized
            RuntimeSerializationInfo _serializationInfo = new RuntimeSerializationInfo(_objectType);

            // Get serialization entries for this object
            GetObjectData(_object, ref _serializationInfo);

            // Write initializers, properties
            WriteObjectGraphValuesCollection(_binaryWriter, _serializationInfo.InitializerValuesCollection);
            WriteObjectGraphValuesCollection(_binaryWriter, _serializationInfo.NonInitializerValuesCollection);

            // Trigger serialization finished callback
            if (typeof(IRuntimeSerializationCallback).IsAssignableFrom(_objectType))
                ((IRuntimeSerializationCallback)_object).OnAfterRuntimeSerialize();
        }

        #endregion

        #region Object Reference Methods

        private void WriteObjectReferenceTypeValue(RSBinaryWriter _binaryWriter, object _object, Type _objectType, eTypeTag _typeTag)
        {
            // Check if this object exists in object reference cache
            bool _firstTime;
            UInt32 _objectReferenceID;

            RegisterObject(_object, out _objectReferenceID, out _firstTime);

            if (_firstTime)
            {
                if (_typeTag == eTypeTag.ARRAY)
                {
                    WriteArrayTypeValue(_binaryWriter, _object, _objectType, _objectReferenceID);
                    return;
                }
                else if (_typeTag == eTypeTag.CLASS)
                {
                    WriteClassTypeValue(_binaryWriter, _object, _objectType, _objectReferenceID);
                    return;
                }
            }
            else
            {
                _binaryWriter.WriteBinaryElement(BinaryElement.OBJECT_DATA);
                _binaryWriter.WriteTypeTag(eTypeTag.OBJECT_REFERENCE);
                _binaryWriter.Write(_objectReferenceID);
                return;
            }
        }

        private void RegisterObject(object _object, out UInt32 _referenceID, out bool _firstTime)
        {
            // Check if this object already exists
            if (ObjectReferenceCache.TryGetValue(_object, out _referenceID))
            {
                _firstTime = false;
                return;
            }

            // Cache this new object
            _referenceID = ObjectReferenceCounter++;
            _firstTime = true;
            ObjectReferenceCache.Add(_object, _referenceID);
        }

        #endregion

        #region Serialization Methods

        private void GetObjectData(object _object, ref RuntimeSerializationInfo _serializationInfo)
        {
            Type _objectType = _serializationInfo.ObjectType;
            RuntimeSerializableAttribute _serializableAttr = SerializationTypeUtil.GetRuntimeSerializableAttribute(_objectType);

            // Object serialization using Extension
            if (_serializableAttr == null)
            {
                GetObjectDataUsingExtension(_object, _objectType, ref _serializationInfo);
                return;
            }
            // Direct object serialization
            else
            {
                Type _dependencyObjectType = _serializableAttr.ExtensionDependencyObjectType;

                // Serialization is controlled by user
                if (typeof(IRuntimeSerializable).IsAssignableFrom(_objectType))
                {
                    ((IRuntimeSerializable)_object).WriteSerializationData(_serializationInfo);
                }
                // Serialization using Reflection
                else
                {
                    Type _curObjectType = _objectType;

                    while (true)
                    {
                        // Gather information about all the fields to be deserialized
                        if (_serializableAttr != null)
                            GetObjectDataUsingReflection(_object, _curObjectType, ref _serializationInfo, _serializableAttr);

                        // Tranverse to object's base and check for termiation condition
                        _curObjectType = _curObjectType.BaseType;

                        if (_curObjectType == _dependencyObjectType || _curObjectType == null)
                            break;

                        // Get base type's attribute
                        _serializableAttr = SerializationTypeUtil.GetRuntimeSerializableAttribute(_curObjectType);
                    }
                }

                if (_dependencyObjectType != null)
                    GetObjectDataUsingExtension(_object, _dependencyObjectType, ref _serializationInfo);

                return;
            }
        }

        private void GetObjectDataUsingExtension(object _object, Type _objectType, ref RuntimeSerializationInfo _serializationInfo)
        {
            RSExtension _extension = RSExtensionManager.GetExtension(_objectType);
            Type _extensionType = _extension.Type;

            // Deserialization is controlled by user
            if (typeof(IRuntimeSerializableExtension).IsAssignableFrom(_extensionType))
            {
                ((IRuntimeSerializableExtension)_extension.Instance).WriteSerializationData(_object, _serializationInfo);
                return;
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
                            GetObjectDataUsingReflection(_object, _curObjectType, ref _serializationInfo, _serializableAttr);
                    }

                    // Tranverse to object's base and check for termiation condition
                    _curObjectType = _curObjectType.BaseType;

                    if (_curObjectType == null)
                        break;

                    _curExtension = RSExtensionManager.GetExtension(_curObjectType);
                }
            }
        }

        private void GetObjectDataUsingReflection(object _object, Type _objectType, ref RuntimeSerializationInfo _serializationInfo, RuntimeSerializableAttribute _serializableAttr)
        {
            List<Field> _serializableFields = SerializationTypeUtil.GetRuntimeSerializableFields(_objectType, _serializableAttr);
            int _serializableFieldCount = _serializableFields.Count;

            // Iterate through all serialisable fields
            for (int _iter = 0; _iter < _serializableFieldCount; _iter++)
            {
                Field _curField = _serializableFields[_iter];
                FieldInfo _curFieldInfo = _curField.Info;
                object _curFieldValue;

                if (_curFieldInfo.IsStatic)
                    _curFieldValue = _curFieldInfo.GetValue(null);
                else
                    _curFieldValue = _curFieldInfo.GetValue(_object);

                // Add this field info
                _serializationInfo.AddValue(_curFieldInfo.Name, _curFieldValue, _curFieldInfo.FieldType, _curField.IsObjectInitializer);
            }
        }

        private void WriteObjectGraphValuesCollection(RSBinaryWriter _binaryWriter, Dictionary<string, RuntimeSerializationEntry> _valuesCollection)
        {
            Dictionary<string, RuntimeSerializationEntry>.Enumerator _enumerator = _valuesCollection.GetEnumerator();

            // Write count
            _binaryWriter.Write(_valuesCollection.Count);

            // Write entries
            while (_enumerator.MoveNext())
            {
                RuntimeSerializationEntry _curEntry = _enumerator.Current.Value;

                // Write serialization entry
                _binaryWriter.Write(_curEntry.Name);

                // Write child properties
                WriteObjectValue(_binaryWriter, _curEntry.Value);
            }
        }

        #endregion
    }
}